# Claude


## Claude Code

```
ln -sv ~/.config/claude/claude-code-settings.json ~/.claude/settings.json
```

### CLAUDE.md

Here is my user memory config for Claude Code that would be linked to `~/.claude/CLAUDE.md`.

It includes personal preferences for all projects	Code styling preferences, personal tooling shortcuts.

see Claude Code's doc on [memory](https://docs.anthropic.com/en/docs/claude-code/memory)

References:

- [Leaked Claude Code system prompts](https://gist.github.com/wong2/e0f34aac66caf890a332f7b6f9e2ba8f)
- [Official Claude Code Best practices](https://www.anthropic.com/engineering/claude-code-best-practices)

### User Commands

see Claude Code's doc on [Personal commands](https://docs.anthropic.com/en/docs/claude-code/slash-commands#personal-commands)

```
ln -sv ~/.config/claude/commands ~/.claude/commands
```

## desktop app config

```
vim "~/Library/Application Support/Claude/claude_desktop_config.json"
```

### Debugging

- MacOS

```
open ~/Library/Logs/Claude
```
