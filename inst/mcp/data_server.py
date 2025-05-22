# data_server.py

from fastmcp import FastMCP, Context
from typing import List

# Create an MCP server
mcp = FastMCP("Data Analysis Server", log_level="WARNING")

@mcp.tool()
def get_mean(data: List[float]) -> float:
    """Calculate the mean of a list of numbers."""
    if not data:
        raise ValueError("Input list cannot be empty")
    return sum(data) / len(data)

if __name__ == "__main__":
    mcp.run() 
