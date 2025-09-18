# Repository Guidelines

## Development Philosophy
Treat this as a toy project: favor KISS solutions, minimal abstractions, and step-by-step growth. Optimize for your own understanding, not production hardening. Skip defensive layers such as try/except blocks unless they clarify the flow. When unsure, trim complexity and document trade-offs in code comments or commit messages.

## Project Structure & Module Organization
The agent lives in agent.py, prompt templates in prompts.py, and default tools in tools.py. Execution traces are written as timestamped logs inside agentlog/. Diagrams and supporting docs reside under docs/ (architecture.drawio plus the exported .svg). Project metadata stays in pyproject.toml; there are no nested packages.

## Build, Test, and Development Commands
Set up the environment with `uv venv`, then `source .venv/bin/activate`. Install dependencies via `uv sync`. Run the agent interactively with `uv run python agent.py`, or inject a task such as `uv run python agent.py "Summarize today’s plan"`. Refresh dependency pins after edits using `uv lock --upgrade`.

## Coding Style & Naming Conventions
Target Python ≥3.9 and follow PEP 8: 4-space indentation, snake_case functions, CamelCase classes (PlanAndExecuteAgent). Keep prompt text plain; avoid JSON-wrapping messages. Place new tools in tools.py, give them concise docstrings, and register them in the exported tool list. Readability beats cleverness—choose explicit variables and short functions.

## Testing Guidelines
No automated suite exists yet. Manually exercise representative tasks and inspect the generated agentlog/*.log to confirm step ordering and tool usage. When adding tests, prefer pytest, store files in tests/, and name them test_<feature>.py. Keep logging deterministic so future assertions stay stable.

## Commit & Pull Request Guidelines
Follow the existing pattern of short, imperative commit summaries (e.g., "Switch project setup instructions to uv"). Reference issues in the body when relevant. Pull requests should explain the motivation, call out touched modules, list manual test commands, and provide log snippets or screenshots for behavior changes.

## Configuration & Secrets
Copy .env.example to .env and supply OPENROUTER_API_KEY; python-dotenv loads it automatically. Never commit .env or agentlog/ artifacts. Gate new integrations behind environment checks and fail fast when a required key is missing.
