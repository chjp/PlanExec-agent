To use this code:

Create and activate a Python virtual environment (recommended):

```bash
# Create a virtual environment in a .venv folder
python3 -m venv .venv

# Activate it (macOS/Linux)
source .venv/bin/activate
```

Install the required packages:

```bash
pip install -r requirements.txt
```

Get your OpenRouter API key from [OpenRouter](https://openrouter.ai/keys)
Create a `.env` file (or copy from `.env.example`) with your key:

```
OPENROUTER_API_KEY=your-api-key-here
```

Note: The app automatically loads environment variables from `.env` using `python-dotenv`.

Run the script:

```bash
python agent.py
```
The key components are:

- **OpenRouterLLM**: Handles API calls to DeepSeek v3 through OpenRouter
- **PlanAndExecuteAgent**: Core agent that creates plans and executes them step by step
- **AdvancedPlanExecuteAgent**: Extended version with tool support
- **Tool classes**: Extensible framework for adding capabilities like search, calculations, etc.

The agent will automatically break down any task into steps, execute each step using DeepSeek v3, and synthesize the results into a final output.

## Architecture

```mermaid
flowchart TD
    U[User Task Input]
    U --> RUN[PlanAndExecuteAgent.run]
    RUN --> PLAN[create_plan]
    PLAN --> P1[planning_prompt]
    P1 --> LLM1[OpenRouterLLM.generate]
    LLM1 --> PARSE[Parse into steps list]
    PARSE --> LOOP[For each Step]
    LOOP --> EXEC[execute_step]
    EXEC --> CTX[_get_context]
    EXEC --> P2[execution_prompt]
    P2 --> LLM2[OpenRouterLLM.generate]
    LLM2 --> UPDATE[Update step status/result]
    UPDATE --> LOOP
    LOOP --> SYN[_synthesize_results]
    SYN --> P3[synthesis_prompt]
    P3 --> LLM3[OpenRouterLLM.generate]
    LLM3 --> FINAL[Final Result]

    subgraph AdvancedPlanExecuteAgent
        EXEC --> TOOLQ{Step mentions tool?}
        TOOLQ -- Yes --> TEXEC[tool.execute]
        TEXEC --> P4[tool_interpretation_prompt]
        P4 --> LLM4[OpenRouterLLM.generate]
        LLM4 --> UPDATE
        TOOLQ -- No --> P2
    end
```

Notes:
- Prompts are constructed in `prompts.py` and consumed by `agent.py`.
- API key is read from `.env` via `python-dotenv` (`OPENROUTER_API_KEY`).
- `AdvancedPlanExecuteAgent` extends the base execution by invoking registered `Tool`s when referenced by step descriptions.
