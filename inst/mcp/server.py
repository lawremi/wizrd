# /// script
# dependencies = [
#   "mcp[cli]",
# ]
# ///

from mcp.server.fastmcp.utilities.logging import configure_logging
from mcp.server.fastmcp import FastMCP, Context
from mcp.server.fastmcp.prompts.base import Message
import anyio
import argparse

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run the MCP server.")
    parser.add_argument("--transport", type=str, default="stdio")
    parser.add_argument("--port", type=int, default=8000)
    args = parser.parse_args()
    mcp = FastMCP("Demo", port=args.port)
else:
    mcp = FastMCP("Demo")

configure_logging(level="WARNING")

@mcp.tool()
def add(a: int, b: int) -> int:
    """Add two numbers"""
    return a + b

@mcp.resource("greeting://{name}")
def get_greeting(name: str) -> str:
    """Get a personalized greeting"""
    return f"Hello, {name}!"

@mcp.tool()
async def sleep(seconds: int = 10, ctx: Context = None) -> str:
    """Sleep for the specified number of seconds."""
    if (seconds < 0 and ctx):
        await ctx.warning("sleep requested for less than zero seconds")
    await anyio.sleep(max(seconds, 0))
    return f"Slept for {seconds} seconds"

@mcp.prompt()
async def ask_review(code_snippet: str, language: str = "python") -> str:
    """Generates a standard code review request."""
    if not code_snippet or not code_snippet.strip():
        raise ValueError("Code snippet cannot be empty")
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
    mcp.run(transport=args.transport)
