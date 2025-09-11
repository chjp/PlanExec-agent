To use this code:

Install the required package:

bashpip install requests

Get your OpenRouter API key from https://openrouter.ai/keys
Set the environment variable:

bashexport OPENROUTER_API_KEY='your-api-key-here'

Run the script:

bashpython plan_execute_agent.py
The key components are:

OpenRouterLLM: Handles API calls to DeepSeek v3 through OpenRouter
PlanAndExecuteAgent: Core agent that creates plans and executes them step by step
AdvancedPlanExecuteAgent: Extended version with tool support
Tool classes: Extensible framework for adding capabilities like search, calculations, etc.

The agent will automatically break down any task into steps, execute each step using DeepSeek v3, and synthesize the results into a final output.Retry
