---
name: uv-script
description: Create standalone executable Python scripts using uv with inline metadata and shebang support. Use when agent needs to write Python scripts that (1) have self-declared dependencies via inline TOML metadata, (2) should be executable without manual virtualenv management, (3) need reproducible dependency locking, or (4) should work across machines without setup.
---

# uv Script

Create standalone Python scripts with uv that manage their own dependencies.

## Quick Start

### Basic Script with Dependencies

```python
# /// script
# requires-python = ">=3.11"
# dependencies = ["requests", "rich"]
# ///

import requests
from rich.console import Console

console = Console()
resp = requests.get("https://api.github.com")
console.print(f"Status: {resp.status_code}")
```

Run with: `uv run script.py`

### Executable Script (Shebang)

Create a file `greet` (no .py extension):

```
#!/usr/bin/env -S uv run --script
# /// script
# dependencies = ["rich"]
# ///

from rich import print

print("[bold green]Hello, world![/bold green]")
```

Make executable: `chmod +x greet`

Run with: `./greet`

## Adding Dependencies

Use `uv add --script` to add dependencies:

```bash
uv add --script myscript.py 'requests<3' 'rich'
```

This updates the inline metadata automatically.

## Locking Dependencies

For reproducibility, create a lockfile:

```bash
uv lock --script myscript.py
```

This creates `myscript.py.lock`. Subsequent runs use locked versions.

## Python Version

Specify minimum Python version:

```python
# /// script
# requires-python = ">=3.12"
# dependencies = []
# ///
```

## Common Patterns

### HTTP Script with Error Handling

```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = ["httpx"]
# ///

import sys
import httpx

def main():
    try:
        resp = httpx.get("https://example.com", timeout=10)
        resp.raise_for_status()
        print(f"Success: {resp.status_code}")
    except httpx.HTTPError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
```

### Script with CLI Arguments

```python
#!/usr/bin/env -S uv run --script
# /// script
# dependencies = ["click"]
# ///

import click

@click.command()
@click.argument("name")
@click.option("--count", default=1, help="Number of greetings")
def main(name, count):
    for _ in range(count):
        print(f"Hello, {name}!")

main()
```

### GUI Script (.pyw)

On Windows, use `.pyw` extension to run with pythonw:

```python
# /// script
# dependencies = ["tkinter"]
# ///

# Note: tkinter is in stdlib, no need to declare
from tkinter import Tk, ttk

root = Tk()
root.title("My App")
ttk.Label(root, text="Hello").pack()
root.mainloop()
```

## Best Practices

1. **Always lock dependencies** in production scripts: `uv lock --script`
2. **Use shebang** for scripts on PATH or frequently run
3. **Specify Python version** for compatibility guarantees
4. **Use `exclude-newer`** for reproducible scripts:

```python
# /// script
# dependencies = ["requests"]
# [tool.uv]
# exclude-newer = "2024-01-01T00:00:00Z"
# ///
```

5. **Keep dependencies minimal** - only declare what's actually imported
