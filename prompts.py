"""
Prompt builders for the Plan-and-Execute agent.
Each function returns a fully formatted prompt string with no unintended indentation.
"""

from typing import Optional


def planning_prompt(task: str) -> str:
    return f"""You are a planning agent. Create a detailed step-by-step plan for the following task.

Task: {task}

Requirements:
1. Break down the task into clear, actionable steps
2. Each step should be specific and achievable
3. Format each step EXACTLY as: [STEP] <description>
4. Include 3-7 steps depending on complexity
5. Steps should be sequential and build on each other

Example format:
[STEP] Research the main topic and gather relevant information
[STEP] Analyze and organize the collected information
[STEP] Create an outline based on the analysis
[STEP] Develop the final deliverable

Now create a plan for the given task:"""


def execution_prompt(step_description: str, context: str) -> str:
    return f"""You are an execution agent. Execute the following step and provide a detailed result.

Current Step: {step_description}

Context from previous steps:
{context}

Instructions:
1. Execute this step thoroughly
2. Provide concrete, actionable results
3. Be specific and detailed
4. If this involves creation, actually create the content
5. If this involves analysis, provide the analysis

Execute the step now and provide the result:"""


def synthesis_prompt(task_description: str, step_results_block: str) -> str:
    return f"""Synthesize the following step results into a comprehensive final answer.

Task that was completed: {task_description}

Step Results:
{step_results_block}

Instructions:
1. Create a coherent, well-structured summary
2. Highlight key achievements and findings
3. Ensure all important information is included
4. Make it clear and easy to understand

Provide the final synthesized result:"""


def tool_interpretation_prompt(tool_name: str, tool_output: str, original_step: str) -> str:
    return f"""Given this tool execution result, provide a comprehensive response:

Tool: {tool_name}
Tool Output: {tool_output}
Original Step: {original_step}

Provide a detailed interpretation and expansion of this result:"""
