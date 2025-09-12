"""
Minimal Plan-and-Execute LLM Agent Implementation with OpenRouter
Uses DeepSeek v3 through OpenRouter API
"""

import json
import re
import os
from typing import List, Dict, Any, Optional
from dataclasses import dataclass
import requests
from dotenv import load_dotenv

# Prompt builders
from prompts import (
    planning_prompt,
    execution_prompt,
    synthesis_prompt,
    tool_interpretation_prompt,
)

# Load environment variables from a .env file if present
load_dotenv()


class OpenRouterLLM:
    """OpenRouter LLM client for DeepSeek v3"""
    
    def __init__(self, api_key: Optional[str] = None, model: str = "deepseek/deepseek-chat"):
        """
        Initialize OpenRouter client
        
        Args:
            api_key: OpenRouter API key (or set OPENROUTER_API_KEY env variable)
            model: Model to use (default: deepseek/deepseek-chat for DeepSeek v3)
        """
        self.api_key = api_key or os.getenv("OPENROUTER_API_KEY")
        if not self.api_key:
            raise ValueError(
                "OpenRouter API key required. Set OPENROUTER_API_KEY env variable or pass api_key parameter"
            )
        
        self.model = model
        self.base_url = "https://openrouter.ai/api/v1/chat/completions"
        self.headers = {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json",
            "HTTP-Referer": "https://github.com/yourusername/yourproject",  # Optional but recommended
            "X-Title": "Plan-Execute Agent"  # Optional but recommended
        }
    
    def generate(self, prompt: str, temperature: float = 0.7, max_tokens: int = 1000) -> str:
        """
        Generate response from DeepSeek v3 via OpenRouter
        
        Args:
            prompt: The prompt to send
            temperature: Sampling temperature (0-2)
            max_tokens: Maximum tokens in response
            
        Returns:
            Generated text response
        """
        payload = {
            "model": self.model,
            "messages": [
                {"role": "user", "content": prompt}
            ],
            "temperature": temperature,
            "max_tokens": max_tokens
        }
        
        try:
            response = requests.post(
                self.base_url,
                headers=self.headers,
                json=payload
            )
            response.raise_for_status()
            
            data = response.json()
            return data["choices"][0]["message"]["content"]
            
        except requests.exceptions.RequestException as e:
            print(f"Error calling OpenRouter API: {e}")
            if hasattr(e.response, 'text'):
                print(f"Response: {e.response.text}")
            raise
        except (KeyError, IndexError) as e:
            print(f"Error parsing response: {e}")
            print(f"Response data: {data if 'data' in locals() else 'No data'}")
            raise


@dataclass
class Step:
    """Represents a single step in the plan"""
    id: int
    description: str
    status: str = "pending"
    result: str = ""


class PlanAndExecuteAgent:
    """
    A Plan-and-Execute agent using DeepSeek v3 via OpenRouter that:
    1. Takes a task/goal
    2. Creates a plan by breaking it down into steps
    3. Executes each step sequentially
    4. Returns the final result
    """
    
    def __init__(self, api_key: Optional[str] = None, model: str = "deepseek/deepseek-chat", verbose: bool = True,
                 tools: Optional[Dict[str, "Tool"]] = None):
        """
        Initialize the agent
        
        Args:
            api_key: OpenRouter API key (or set OPENROUTER_API_KEY env variable)
            model: Model to use (default: deepseek/deepseek-chat)
            verbose: Whether to print progress
            tools: Optional dict of tools the agent can use (name -> Tool)
        """
        self.llm = OpenRouterLLM(api_key=api_key, model=model)
        self.verbose = verbose
        self.plan: List[Step] = []
        self.execution_history: List[Dict[str, Any]] = []
        self.tools: Dict[str, Tool] = tools or {}

    def register_tool(self, tool: "Tool"):
        """Register a tool for the agent to use"""
        self.tools[tool.name] = tool
        if self.verbose:
            print(f"🔧 Registered tool: {tool.name}")
    
    def create_plan(self, task: str) -> List[Step]:
        """Generate a plan for the given task"""
        
        prompt = planning_prompt(task)
        
        if self.verbose:
            print(f"\n🤔 Creating plan for: {task}")
        
        # Get plan from LLM
        plan_text = self.llm.generate(prompt, temperature=0.5)  # Lower temperature for planning
        
        if self.verbose:
            print(f"  📝 Raw plan response: {plan_text[:200]}...")
        
        # Parse steps from the response
        steps = []
        step_pattern = r'\[STEP\]\s*(.+?)(?=\[STEP\]|$)'
        matches = re.findall(step_pattern, plan_text, re.DOTALL)
        
        # If no steps found with [STEP] format, try numbered format
        if not matches:
            numbered_pattern = r'(?:^|\n)\s*\d+\.\s*(.+?)(?=\n\d+\.|$)'
            matches = re.findall(numbered_pattern, plan_text, re.MULTILINE | re.DOTALL)
        
        for i, step_desc in enumerate(matches, 1):
            # Clean up the description
            clean_desc = step_desc.strip().replace('\n', ' ').replace('  ', ' ')
            if clean_desc:
                step = Step(id=i, description=clean_desc)
                steps.append(step)
                if self.verbose:
                    print(f"  📋 Step {i}: {step.description}")
        
        if not steps:
            # Fallback: create a generic plan if parsing fails
            if self.verbose:
                print("  ⚠️  Could not parse steps, creating generic plan")
            steps = [
                Step(id=1, description=f"Analyze the requirements for: {task}"),
                Step(id=2, description="Develop a solution approach"),
                Step(id=3, description="Implement the solution"),
                Step(id=4, description="Review and finalize the output")
            ]
        
        self.plan = steps
        return steps
    
    def execute_step(self, step: Step) -> str:
        """Execute a single step of the plan (with optional tool usage)"""
        
        # Try tools first if any are registered and referenced by the step description
        for tool_name, tool in self.tools.items():
            if tool_name.lower() in step.description.lower():
                if self.verbose:
                    print(f"  🔧 Using tool: {tool_name}")
                try:
                    tool_result = tool.execute(step.description)
                    prompt = tool_interpretation_prompt(tool_name, tool_result, step.description)
                    enhanced_result = self.llm.generate(prompt, temperature=0.7)
                    step.result = f"{tool_result}\n\nAnalysis: {enhanced_result}"
                    step.status = "completed"
                    # Store in history
                    self.execution_history.append({
                        "step_id": step.id,
                        "description": step.description,
                        "result": step.result
                    })
                    if self.verbose:
                        print(f"  ✅ Result: {step.result[:150]}...")
                    return step.result
                except Exception as e:
                    if self.verbose:
                        print(f"  ⚠️ Tool execution failed: {str(e)}")
                    # Fall through to regular LLM execution
        
        # Regular LLM execution (fallback or when no tools apply)
        context = self._get_context()
        prompt = execution_prompt(step.description, context)
        
        if self.verbose:
            print(f"\n⚙️  Executing Step {step.id}: {step.description}")
        
        result = self.llm.generate(prompt, temperature=0.7, max_tokens=1500)
        
        step.status = "completed"
        step.result = result
        
        self.execution_history.append({
            "step_id": step.id,
            "description": step.description,
            "result": result
        })
        
        if self.verbose:
            print(f"  ✅ Result: {result[:150]}...")
        
        return result
    
    def _get_context(self) -> str:
        """Get context from previous step executions"""
        if not self.execution_history:
            return "No previous steps executed yet."
        
        context = "Previous steps completed:\n"
        for entry in self.execution_history[-3:]:  # Last 3 steps for context
            result_preview = entry['result'][:100] if len(entry['result']) > 100 else entry['result']
            context += f"\nStep {entry['step_id']}: {entry['description']}\nResult: {result_preview}...\n"
        
        return context
    
    def run(self, task: str) -> Dict[str, Any]:
        """Main execution loop: plan then execute"""
        
        if self.verbose:
            print("="*60)
            print("🚀 STARTING PLAN-AND-EXECUTE AGENT")
            print(f"📍 Model: {self.llm.model}")
            print("="*60)
        
        # Reset state for new task
        self.plan = []
        self.execution_history = []
        
        # Phase 1: Planning
        try:
            self.create_plan(task)
        except Exception as e:
            return {"error": f"Failed to create plan: {str(e)}"}
        
        if not self.plan:
            return {"error": "Failed to create plan - no steps generated"}
        
        # Phase 2: Execution
        if self.verbose:
            print("\n" + "="*60)
            print("🎯 EXECUTING PLAN")
            print("="*60)
        
        for step in self.plan:
            try:
                self.execute_step(step)
            except Exception as e:
                if self.verbose:
                    print(f"  ❌ Error executing step {step.id}: {str(e)}")
                step.status = "failed"
                step.result = f"Error: {str(e)}"
        
        # Phase 3: Synthesize results
        final_result = self._synthesize_results()
        
        return {
            "task": task,
            "model": self.llm.model,
            "plan": [{"id": s.id, "description": s.description, "status": s.status} for s in self.plan],
            "execution_history": self.execution_history,
            "final_result": final_result
        }
    
    def _synthesize_results(self) -> str:
        """Combine all step results into final output"""
        
        if self.verbose:
            print("\n" + "="*60)
            print("📝 SYNTHESIZING RESULTS")
            print("="*60)
        
        # Collect successful results
        successful_results = []
        for step in self.plan:
            if step.status == "completed" and step.result:
                successful_results.append(f"Step {step.id} ({step.description}):\n{step.result}\n")
        
        if not successful_results:
            return "No steps were successfully completed."
        
        all_results = "\n---\n".join(successful_results)
        
        prompt = synthesis_prompt(
            self.plan[0].description if self.plan else 'Unknown task',
            all_results[:3000]
        )
        
        try:
            final = self.llm.generate(prompt, temperature=0.5, max_tokens=2000)
        except Exception as e:
            final = f"Error synthesizing results: {str(e)}\n\nRaw results:\n{all_results[:1000]}"
        
        if self.verbose:
            print(f"\n✨ Final Result: {final[:200]}...")
        
        return final


# Example Tools (for more advanced implementation)
class Tool:
    """Base class for agent tools"""
    def __init__(self, name: str, description: str):
        self.name = name
        self.description = description
    
    def execute(self, *args, **kwargs):
        raise NotImplementedError


class SearchTool(Tool):
    """Mock search tool - replace with real search API"""
    def __init__(self):
        super().__init__("search", "Search for information online")
    
    def execute(self, query: str) -> str:
        # In production, use real search API (Google, Bing, DuckDuckGo, etc.)
        return f"Search results for '{query}': [Would return real search results in production]"


 


# Advanced Agent with Tools
# (Removed AdvancedPlanExecuteAgent; functionality merged into PlanAndExecuteAgent.)


def main():
    """Example usage"""
    
    # Check for API key
    api_key = os.getenv("OPENROUTER_API_KEY")
    if not api_key:
        print("⚠️  WARNING: OPENROUTER_API_KEY not set!")
        print("Please set your OpenRouter API key:")
        print("  export OPENROUTER_API_KEY='your-api-key-here'")
        print("\nGet your API key at: https://openrouter.ai/keys")
        print("\nFor now, running in demo mode with mock responses...\n")
        return
    
    # Unified agent (with optional tools)
    print("Unified Plan-and-Execute Agent (tool-enabled)")
    print("-" * 60)
    
    try:
        agent = PlanAndExecuteAgent(api_key=api_key, verbose=True)
        # Register example tools (optional)
        agent.register_tool(SearchTool())
        
        result = agent.run("Create a brief guide about Python decorators")
        
        print("\n" + "="*60)
        print("FINAL OUTPUT:")
        print(json.dumps(result, indent=2)[:1000] + "...")  # Truncate for display
        
    except Exception as e:
        print(f"Error running agent: {str(e)}")


if __name__ == "__main__":
    main()
