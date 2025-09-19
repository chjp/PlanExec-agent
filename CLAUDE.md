# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Setup and Development Commands

### Environment Setup
```bash
# Install uv if not available
curl -LsSf https://astral.sh/uv/install.sh | sh

# Create environment and install dependencies
uv venv
source .venv/bin/activate
uv sync

# Configure API key (copy from .env.example)
cp .env.example .env
# Edit .env and add your OpenRouter API key
```

### Running the Agent
```bash
# Interactive mode (prompts for task input)
uv run python agent.py

# Pass task via command line
uv run python agent.py "Create a brief guide about Python decorators"

# Update dependencies after changes
uv lock --upgrade
```

## Architecture Overview

This is a minimal Plan-and-Execute LLM agent that uses DeepSeek v3.1 through OpenRouter. The agent operates in three phases:

1. **Planning**: Breaks down user tasks into sequential steps
2. **Execution**: Executes each step with optional tool usage
3. **Synthesis**: Combines step results into a final output

### Core Components

- **agent.py**: Main `PlanAndExecuteAgent` class and `OpenRouterLLM` client
- **prompts.py**: All prompt templates (planning, execution, synthesis, tool invocation)
- **tools.py**: Tool framework with built-in tools (file I/O, commands, web search, HTTP fetch)
- **agentlog/**: Session logs with timestamped execution traces (`yyyymmddhhmm.log`)

### Tool System

The agent automatically detects tool usage when step descriptions contain tool names:
- `read_file`: Read project files
- `write_file`: Write/append to project files  
- `run_command`: Execute shell commands in project directory
- `web_search`: DuckDuckGo search
- `fetch_url`: HTTP requests with text preview

Tools are project-aware and automatically resolve paths relative to the project root.

## Configuration

- **API Key**: Set `OPENROUTER_API_KEY` in `.env` file (required)
- **Model**: Default is `deepseek/deepseek-chat-v3.1`
- **Logging**: All sessions automatically logged to `agentlog/` directory
- **Project Root**: Tools operate relative to the directory containing `agent.py`

## Development Philosophy

This is designed as a toy project following KISS principles:
- Minimal abstractions and step-by-step growth
- Plain text prompts (avoid JSON-wrapping)
- Explicit over clever code
- Manual testing via agentlog inspection
- No defensive try/catch unless it clarifies flow

## Module Organization

- Keep agent logic in `agent.py`
- Add new prompt templates to `prompts.py`
- Extend tools in `tools.py` and register in `create_project_tools()`
- Store supporting docs in `docs/`
- Project metadata stays in `pyproject.toml`