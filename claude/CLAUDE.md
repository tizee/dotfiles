# Claude Code: Best Practices for Effective Collaboration

This document outlines best practices for working with Claude Code to ensure efficient and successful software development tasks.

## Important Instruction

- Always use TodoWrite tool to track complex tasks with multiple steps
- Provide summary confirmations for completed tasks
- Your context window will be automatically compacted as it approaches its limit. Never stop tasks early due to token budget concerns. Always complete tasks fully, even if the end of your budget is approaching.

## Project Knowledge Base

### Skills

For specialized guidance on specific topics, refer to these skills:

- **Software Design**: [Software Design Principles Skill](~/.claude/skills/software-design-principles/SKILL.md)
- **Code Formatting**: [Code Formatting Skill](~/.claude/skills/code-formatting/SKILL.md)
- **Testing**: [Testing Guidelines Skill](~/.claude/skills/testing-guidelines/SKILL.md)
- **AST-Grep**: [AST-Grep Skill](~/.claude/skills/ast-grep/SKILL.md)

## Tool Preferences

**Preferred Tools** (all pre-installed):
- **sg** (ast-grep): Structural code search and syntax-aware analysis
- **fd**: File discovery
- **rg** (ripgrep): Plain-text content search
- **gh**: GitHub operations

**Tool Priority**:
1. ast-grep (sg) - for code structure patterns
2. fd - for file discovery
3. rg - for plain-text search
4. Agent - for semantic exploration when needed

## Commit Messages

When generating commit messages:
- Do NOT include `Co-Authored-By: Claude <noreply@anthropic.com>`
- Do NOT include `🤖 Generated with [Claude Code](https://claude.ai/code)`

## Communication Style

**Communicate naturally and collaboratively:**
- Use natural language and varied responses
- Acknowledge mistakes naturally
- Suggest improvements proactively
- Avoid emojis unless explicitly requested
- Be concise and focused in CLI environment

Good collaboration requires mutual respect and honest communication.
