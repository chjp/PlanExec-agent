'''Prompt builders for the Plan-and-Execute agent.
Each function returns a fully formatted prompt string with no unintended indentation.
'''

from __future__ import annotations

import json
from typing import Dict, Iterable, Optional


def _format_tool_hint_lines(tool_hint: str) -> str:
    '''Indent multi-line tool hints so they render cleanly in prompts.'''
    hint_lines = tool_hint.splitlines()
    if not hint_lines:
        return ''
    indented = [hint_lines[0]]
    indented.extend(f'  {line}' for line in hint_lines[1:])
    return '\n'.join(indented)


def planning_prompt(task: str, tool_hints: Optional[Iterable[str]] = None) -> str:
    '''Create the planning prompt, optionally including available tool hints.'''

    prompt = (
        f'You are a planning agent. Create a detailed step-by-step plan for the following task.\n\n'
        f'Task: {task}\n\n'
        'Requirements:\n'
        '1. Break down the task into clear, actionable steps\n'
        '2. Each step should be specific and achievable\n'
        '3. Format each step EXACTLY as: [STEP] <description>\n'
        '4. Include 3-7 steps depending on complexity\n'
        '5. Steps should be sequential and build on each other\n'
    )

    if tool_hints:
        formatted_hints = '\n'.join(
            f'- {_format_tool_hint_lines(hint)}' for hint in tool_hints if hint
        )
        if formatted_hints:
            prompt += (
                '\nAvailable tools (mention a tool name in the step description when it should be used):\n'
                f'{formatted_hints}\n'
                '\nInclude the tool name directly in the relevant step so the execution agent knows when to call it.\n'
            )

    prompt += (
        'Example format:\n'
        '[STEP] Research the main topic and gather relevant information\n'
        '[STEP] Analyze and organize the collected information\n'
        '[STEP] Create an outline based on the analysis\n'
        '[STEP] Develop the final deliverable\n'
        '\n'
        'Now create a plan for the given task:'
    )

    return prompt


def execution_prompt(step_description: str, context: str) -> str:
    return (
        'You are an execution agent. Execute the following step and provide a detailed result.\n\n'
        f'Current Step: {step_description}\n\n'
        'Context from previous steps:\n'
        f'{context}\n\n'
        'Instructions:\n'
        '1. Execute this step thoroughly\n'
        '2. Provide concrete, actionable results\n'
        '3. Be specific and detailed\n'
        '4. If this involves creation, actually create the content\n'
        '5. If this involves analysis, provide the analysis\n\n'
        'Execute the step now and provide the result:'
    )


def synthesis_prompt(task_description: str, step_results_block: str) -> str:
    return (
        'Synthesize the following step results into a comprehensive final answer.\n\n'
        f'Task that was completed: {task_description}\n\n'
        'Step Results:\n'
        f'{step_results_block}\n\n'
        'Instructions:\n'
        '1. Create a coherent, well-structured summary\n'
        '2. Highlight key achievements and findings\n'
        '3. Ensure all important information is included\n'
        '4. Make it clear and easy to understand\n\n'
        'Provide the final synthesized result:'
    )


def tool_invocation_prompt(
    tool_name: str,
    tool_description: str,
    step_description: str,
    argument_spec: Dict[str, Dict[str, object]],
    example_json: Optional[str] = None,
) -> str:
    '''Prompt the LLM to produce JSON arguments for a tool call.'''

    if argument_spec:
        spec_lines = []
        for arg_name, spec in argument_spec.items():
            requirement = 'required' if spec.get('required', True) else 'optional'
            type_hint = spec.get('type', 'string')
            default_value = spec.get('default')
            default_suffix = ''
            if default_value is not None:
                default_suffix = f', default={json.dumps(default_value, ensure_ascii=False)}'
            description = spec.get('description', 'No description provided.')
            spec_lines.append(
                f'- "{arg_name}" ({requirement}, type={type_hint}{default_suffix}): {description}'
            )
        spec_block = '\n'.join(spec_lines)
        keys_list = ', '.join(argument_spec.keys())
        instruction = (
            f'Return ONLY a valid JSON object containing the keys: {keys_list}.\n'
            'Do not wrap the JSON in quotes or backticks.'
        )
    else:
        spec_block = 'This tool does not require any arguments.'
        instruction = (
            'Return ONLY an empty JSON object ({}).\n'
            'Do not wrap the JSON in quotes or backticks.'
        )

    example_section = ''
    if example_json:
        example_section = f'\nExample JSON:\n{example_json}\n'

    prompt = (
        f'You are preparing structured input for the tool `{tool_name}`.\n\n'
        f'Tool purpose: {tool_description}\n\n'
        'Expected arguments:\n'
        f'{spec_block}\n'
        f'{example_section}'
        f'Step description: {step_description}\n\n'
        'Instructions:\n'
        '- Produce only JSON that adheres to the expected structure.\n'
        '- Use `\\n` escape sequences for newlines inside string values.\n'
        '- If a required value is missing from the step description, respond with a JSON object {"error": "<explanation>"}.\n'
        '- Do not include any extra commentary or formatting around the JSON.\n\n'
        f'{instruction}'
    )

    return prompt


def tool_interpretation_prompt(
    tool_name: str,
    tool_output: str,
    original_step: str,
    tool_arguments: Optional[Dict[str, object]] = None,
) -> str:
    arguments_block = 'None provided.'
    if tool_arguments is not None:
        arguments_block = json.dumps(tool_arguments, ensure_ascii=False, indent=2)

    return (
        'Given this tool execution result, provide a comprehensive response:\n\n'
        f'Tool: {tool_name}\n'
        f'Arguments Used: {arguments_block}\n'
        f'Tool Output: {tool_output}\n'
        f'Original Step: {original_step}\n\n'
        'Provide a detailed interpretation and expansion of this result:'
    )
