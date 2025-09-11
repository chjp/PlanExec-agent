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
