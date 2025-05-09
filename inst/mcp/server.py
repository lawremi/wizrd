# server.py

from fastmcp import FastMCP
from fastmcp.prompts.prompt import Message

# Create an MCP server
mcp = FastMCP("Demo", log_level="WARNING") # change to DEBUG for debugging


# Add an addition tool
@mcp.tool()
def add(a: int, b: int) -> int:
    """Add two numbers"""
    return a + b


# Add a dynamic greeting resource
@mcp.resource("greeting://{name}")
def get_greeting(name: str) -> str:
    """Get a personalized greeting"""
    return f"Hello, {name}!"

@mcp.prompt()
def ask_review(code_snippet: str, language: str = "python") -> str:
    """Generates a standard code review request."""
    return f"Please review the following code snippet for potential bugs and style issues:\n```{language}\n{code_snippet}\n```"

@mcp.prompt()
def debug_session_start(error_message: str) -> list[Message]:
    """Initiates a debugging help session."""
    return [
        Message(role="user", content=f"I encountered an error:\n{error_message}"),
        Message(role="assistant", content="Okay, I can help with that. Can you provide the full traceback and tell me what you were trying to do?")
    ]

if __name__ == "__main__":
    mcp.run()
