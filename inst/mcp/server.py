# /// script
# dependencies = [
#   "mcp[cli]",
# ]
# ///

from mcp.server.fastmcp.utilities.logging import configure_logging
from mcp.server.fastmcp import FastMCP, Context
from mcp.server.fastmcp.prompts.base import Message
import anyio

# Create an MCP server
mcp = FastMCP("Demo")
configure_logging(level="WARNING")


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

@mcp.tool()
async def sleep(seconds: int = 10) -> str:
    """Sleep for the specified number of seconds."""
    await anyio.sleep(seconds)
    return f"Slept for {seconds} seconds"

@mcp.prompt()
async def ask_review(code_snippet: str, language: str = "python", ctx: Context = None) -> str:
    """Generates a standard code review request."""
    if not code_snippet or not code_snippet.strip():
        raise ValueError("Code snippet cannot be empty")
    if language.lower() == "r" and ctx:
        await ctx.warning("R is considered harmful")
    return f"Please review the following code snippet for potential bugs and style issues:\n```{language}\n{code_snippet}\n```"

@mcp.prompt()
def debug_session_start(error_message: str) -> list[Message]:
    """Initiates a debugging help session."""
    return [
        Message(role="user", content=f"I encountered an error:\n{error_message}"),
        Message(role="assistant", content="Okay, I can help with that. Can you provide the full traceback and tell me what you were trying to do?")
    ]

@mcp._mcp_server.set_logging_level()
async def set_logging_level(level: str) -> None:
    pass

if __name__ == "__main__":
    mcp.run()
