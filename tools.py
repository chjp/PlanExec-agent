'''Tool implementations for the Plan-and-Execute agent.
'''

from __future__ import annotations

import json
import os
import subprocess
from dataclasses import dataclass
from typing import Any, Dict, List, Optional

import requests
from duckduckgo_search import DDGS

MAX_TEXT_LENGTH = 4000


@dataclass
class ToolArgument:
    description: str
    type_hint: str = 'string'
    required: bool = True
    default: Any = None


class Tool:
    '''Base class for agent tools.'''

    def __init__(
        self,
        name: str,
        description: str,
        arguments: Optional[Dict[str, ToolArgument]] = None,
        example_json: Optional[str] = None,
    ) -> None:
        self.name = name
        self.description = description
        self.arguments = arguments or {}
        self.example_json = example_json

    def get_argument_prompt_spec(self) -> Dict[str, Dict[str, Any]]:
        '''Return a serialisable dictionary describing the tool arguments.'''

        spec: Dict[str, Dict[str, Any]] = {}
        for arg_name, arg in self.arguments.items():
            spec[arg_name] = {
                'description': arg.description,
                'type': arg.type_hint,
                'required': arg.required,
                'default': arg.default,
            }
        return spec

    def build_planning_hint(self) -> str:
        '''Return a human-readable hint for planning prompts.'''

        lines = [f'{self.name}: {self.description}']
        if self.arguments:
            lines.append('Arguments:')
            for arg_name, arg in self.arguments.items():
                requirement = 'required' if arg.required else 'optional'
                default_info = ''
                if arg.default is not None:
                    default_info = f', default={arg.default!r}'
                lines.append(
                    f'  - {arg_name} ({requirement}, type={arg.type_hint}{default_info}): {arg.description}'
                )
        if self.example_json:
            lines.append(f'Example JSON: {self.example_json}')
        return '\n'.join(lines)

    def format_arguments_for_logging(self, arguments: Dict[str, Any]) -> str:
        '''Pretty-print arguments for logging.'''

        return json.dumps(arguments, ensure_ascii=False, indent=2)

    def execute(self, arguments: Dict[str, Any]) -> str:
        '''Validate arguments then run the tool.'''

        validated = self._validate_arguments(arguments)
        return self._run(**validated)

    def _validate_arguments(self, arguments: Dict[str, Any]) -> Dict[str, Any]:
        if not isinstance(arguments, dict):
            raise ValueError('Tool arguments must be provided as a JSON object.')

        validated: Dict[str, Any] = {}
        missing: List[str] = []
        for arg_name, spec in self.arguments.items():
            if arg_name in arguments and arguments[arg_name] is not None:
                validated[arg_name] = arguments[arg_name]
            elif spec.required:
                missing.append(arg_name)
            else:
                validated[arg_name] = spec.default

        if missing:
            raise ValueError(f"Missing required arguments: {', '.join(missing)}")

        for arg_name, value in arguments.items():
            if arg_name not in validated:
                validated[arg_name] = value

        return validated

    def _run(self, **kwargs: Any) -> str:
        raise NotImplementedError('Tool subclasses must implement _run().')


class ProjectTool(Tool):
    '''Base class for tools that operate on the project directory.'''

    def __init__(
        self,
        project_dir: str,
        name: str,
        description: str,
        arguments: Optional[Dict[str, ToolArgument]] = None,
        example_json: Optional[str] = None,
    ) -> None:
        super().__init__(name, description, arguments, example_json)
        self.project_dir = os.path.abspath(project_dir)

    def _resolve_path(self, path: str) -> str:
        if not path:
            raise ValueError('File path must be provided.')

        candidate = os.path.abspath(
            path if os.path.isabs(path) else os.path.join(self.project_dir, path)
        )

        try:
            common = os.path.commonpath([self.project_dir, candidate])
        except ValueError as exc:
            raise ValueError('Invalid file path.') from exc

        if common != self.project_dir:
            raise ValueError('File path must be within the project directory.')

        return candidate


class ReadFileTool(ProjectTool):
    '''Read file contents from the project directory.'''

    def __init__(self, project_dir: str) -> None:
        super().__init__(
            project_dir,
            name='read_file',
            description='Read and return the contents of a project file.',
            arguments={
                'path': ToolArgument('Relative or absolute path to the file inside the project.'),
                'encoding': ToolArgument(
                    'Text encoding to use when reading.',
                    required=False,
                    default='utf-8',
                ),
            },
            example_json='{"path": "README.md"}',
        )

    def _run(self, path: str, encoding: str = 'utf-8') -> str:
        resolved = self._resolve_path(path)
        try:
            with open(resolved, 'r', encoding=encoding) as handle:
                content = handle.read()
        except OSError as exc:
            raise RuntimeError(f'Failed to read file {path}: {exc}') from exc

        if len(content) > MAX_TEXT_LENGTH:
            return content[:MAX_TEXT_LENGTH] + '\n[truncated]'

        return content


class WriteFileTool(ProjectTool):
    '''Write text content to a file within the project directory.'''

    def __init__(self, project_dir: str) -> None:
        super().__init__(
            project_dir,
            name='write_file',
            description='Write text content to a file within the project.',
            arguments={
                'path': ToolArgument('Relative or absolute path to the file inside the project.'),
                'content': ToolArgument('Text content to write to the file (use \\n for new lines).'),
                'encoding': ToolArgument(
                    'Text encoding to use when writing.',
                    required=False,
                    default='utf-8',
                ),
                'append': ToolArgument(
                    'Append to the file instead of overwriting when true.',
                    type_hint='boolean',
                    required=False,
                    default=False,
                ),
            },
            example_json='{"path": "notes/todo.txt", "content": "Line one\\\nLine two"}',
        )

    def _run(self, path: str, content: str, encoding: str = 'utf-8', append: bool = False) -> str:
        resolved = self._resolve_path(path)
        directory = os.path.dirname(resolved) or self.project_dir
        os.makedirs(directory, exist_ok=True)
        mode = 'a' if append else 'w'
        text = content.replace('\\n', '\n')
        try:
            with open(resolved, mode, encoding=encoding) as handle:
                handle.write(text)
        except OSError as exc:
            raise RuntimeError(f'Failed to write file {path}: {exc}') from exc

        return f'Write successful: {resolved}'


class RunCommandTool(ProjectTool):
    '''Execute shell commands from the project directory.'''

    def __init__(self, project_dir: str) -> None:
        super().__init__(
            project_dir,
            name='run_command',
            description='Execute a shell command in the project directory.',
            arguments={
                'command': ToolArgument('Shell command to execute from the project root.'),
                'timeout': ToolArgument(
                    'Maximum number of seconds to allow the command to run.',
                    type_hint='integer',
                    required=False,
                    default=120,
                ),
            },
            example_json='{"command": "ls -la"}',
        )

    def _run(self, command: str, timeout: int = 120) -> str:
        try:
            timeout_value = int(timeout)
        except (TypeError, ValueError) as exc:
            raise ValueError('Timeout must be an integer number of seconds.') from exc

        try:
            result = subprocess.run(
                command,
                shell=True,
                cwd=self.project_dir,
                capture_output=True,
                text=True,
                timeout=timeout_value,
            )
        except subprocess.TimeoutExpired as exc:
            raise RuntimeError(f'Command timed out after {timeout_value} seconds.') from exc

        stdout = (result.stdout or '').strip()
        stderr = (result.stderr or '').strip()
        exit_code = result.returncode

        parts = []
        if stdout:
            parts.append(f'stdout:\n{stdout}')
        if stderr:
            parts.append(f'stderr:\n{stderr}')
        parts.append(f'exit_code={exit_code}')

        combined = '\n'.join(parts) if parts else f'exit_code={exit_code} (no output)'
        if len(combined) > MAX_TEXT_LENGTH:
            combined = combined[:MAX_TEXT_LENGTH] + '\n[truncated]'

        return combined


class WebSearchTool(Tool):
    '''Search the web using DuckDuckGo.'''

    def __init__(self) -> None:
        super().__init__(
            name='web_search',
            description='Search the web via DuckDuckGo.',
            arguments={
                'query': ToolArgument('Search query to send to DuckDuckGo.'),
                'max_results': ToolArgument(
                    'Maximum number of results to return.',
                    type_hint='integer',
                    required=False,
                    default=5,
                ),
                'site': ToolArgument(
                    'Optional domain to scope the search (e.g. example.com).',
                    required=False,
                    default=None,
                ),
            },
            example_json='{"query": "python dataclasses", "max_results": 3}',
        )

    def _run(self, query: str, max_results: int = 5, site: Optional[str] = None) -> str:
        actual_query = query if not site else f'site:{site} {query}'
        results = []

        try:
            with DDGS() as ddgs:
                for item in ddgs.text(actual_query, max_results=int(max_results)):
                    results.append(
                        {
                            'title': item.get('title'),
                            'url': item.get('href'),
                            'snippet': item.get('body'),
                        }
                    )
        except Exception as exc:  # pragma: no cover - network errors vary
            raise RuntimeError(f'Search error: {exc}') from exc

        return json.dumps(results, ensure_ascii=False, indent=2)


class FetchUrlTool(Tool):
    '''Fetch the contents of a URL.'''

    def __init__(self) -> None:
        super().__init__(
            name='fetch_url',
            description='Fetch a URL and return metadata plus a text preview.',
            arguments={
                'url': ToolArgument('The URL to fetch.'),
                'timeout': ToolArgument(
                    'Request timeout in seconds.',
                    type_hint='integer',
                    required=False,
                    default=20,
                ),
            },
            example_json='{"url": "https://example.com"}',
        )

    def _run(self, url: str, timeout: int = 20) -> str:
        headers = {
            'User-Agent': 'PlanExec-Agent/0.1 (+https://github.com/yourusername/yourproject)'
        }
        try:
            response = requests.get(url, headers=headers, timeout=float(timeout))
        except Exception as exc:  # pragma: no cover - network errors vary
            raise RuntimeError(f'Fetch error: {exc}') from exc

        content_type = response.headers.get('content-type', '')
        text_preview = response.text or ''
        if len(text_preview) > MAX_TEXT_LENGTH:
            text_preview = text_preview[:MAX_TEXT_LENGTH] + '\n[truncated]'

        payload = {
            'status_code': response.status_code,
            'content_type': content_type,
            'text_preview': text_preview,
            'url': response.url,
        }

        return json.dumps(payload, ensure_ascii=False, indent=2)


def create_project_tools(project_dir: str) -> Dict[str, Tool]:
    '''Create the standard suite of tools bound to the provided project directory.'''

    project_dir = os.path.abspath(project_dir)

    tools = [
        ReadFileTool(project_dir),
        WriteFileTool(project_dir),
        RunCommandTool(project_dir),
        WebSearchTool(),
        FetchUrlTool(),
    ]

    return {tool.name: tool for tool in tools}


__all__ = [
    'Tool',
    'ToolArgument',
    'ProjectTool',
    'ReadFileTool',
    'WriteFileTool',
    'RunCommandTool',
    'WebSearchTool',
    'FetchUrlTool',
    'create_project_tools',
]
