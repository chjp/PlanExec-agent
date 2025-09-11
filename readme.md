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
    A[User Task Input] --> B[PlanAndExecuteAgent.run(task)]
    B --> C[create_plan(task)]
    C -->|build prompt| P1[planning_prompt(task)]
    P1 -->|send| LLM1[OpenRouterLLM.generate]
    LLM1 -->|plan text| D[Parse steps -> List<Step>]
    D --> E[Execute loop]
    E --> F[execute_step(step)]
    F -->|gather context| CTX[_get_context()]
    F -->|build prompt| P2[execution_prompt(step, context)]
    P2 -->|send| LLM2[OpenRouterLLM.generate]
    LLM2 -->|step result| H[Update step status/result]
    H -->|next step| E
    E -->|all steps processed| S[_synthesize_results()]
    S -->|build prompt| P3[synthesis_prompt(task_desc, results)]
    P3 -->|send| LLM3[OpenRouterLLM.generate]
    LLM3 --> R[Final Result]

    subgraph Optional Tool Path [AdvancedPlanExecuteAgent]
        F -->|if step mentions tool| TSEL{Tool found?}
        TSEL -- Yes --> TEXEC[tool.execute(step.description)]
        TEXEC --> P4[tool_interpretation_prompt(tool_name, output, step)]
        P4 -->|send| LLM4[OpenRouterLLM.generate]
        LLM4 --> H
        TSEL -- No --> P2
    end

    subgraph OpenRouter
        LLM1
        LLM2
        LLM3
        LLM4
    end
```

Notes:
- Prompts are constructed in `prompts.py` and consumed by `agent.py`.
- API key is read from `.env` via `python-dotenv` (`OPENROUTER_API_KEY`).
- `AdvancedPlanExecuteAgent` extends the base execution by invoking registered `Tool`s when referenced by step descriptions.
