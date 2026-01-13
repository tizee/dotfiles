# Coding Agents Best Practices for Effective Collaboration

This document outlines best practices for working with coding agents to ensure efficient and successful software development tasks.

## Important Instruction

- Always use planning-related tool to track complex tasks with multiple steps
- Confirm plan with user before making changes - plans set architectural direction before code solidifies
- Provide summary confirmations for completed tasks
- Your context window will be automatically compacted as it approaches its limit. Never stop tasks early due to token budget concerns. Always complete tasks fully, even if the end of your budget is approaching.
- Always write code comments in English unless the user explicitly specifies otherwise
- Prioritize simplest changes and code readability - no backward compatibility or migration concerns unless explicitly specified; make bigger refactors when they improve clarity
- Ask for more context if the user's objective or requirements are unclear or ambiguous before proceeding with implementation
- Avoid using emojis in generated text and code unless explicitly requested

## Project Knowledge Base

### Skills

For specialized guidance on specific topics, refer to these skills under user's global-level skills folder or
project-level skills folder.

## Tool Preferences

### Preferred Tools (all pre-installed):
- **sg** (ast-grep): Structural code search and syntax-aware analysis
- **fd**: File discovery
- **rg** (ripgrep): Plain-text content search
- **gh**: GitHub operations

### Tool Priority:
1. ast-grep (sg) - for code structure patterns
2. fd - for file discovery and glob pattern search
3. rg - for plain-text search
4. Agent - for semantic exploration when needed

## Shell Command Execution

**For build/compile commands** with verbose output (e.g., `cargo build`, `npm run build`, `make`):
- Redirect stdout/stderr to `/tmp/<descriptive_name>.log` to avoid flooding context
- Use `tail` to retrieve relevant results

```bash
cargo build --release > /tmp/build.log 2>&1; tail -n 50 /tmp/build.log
```

**For other commands**: Run directly without redirection to preserve full output visibility.

## Commit Messages

When generating commit messages:
- Do NOT include `Co-Authored-By: Claude <noreply@anthropic.com>`
- Do NOT include `🤖 Generated with [Claude Code](https://claude.ai/code)`

## Communication Style

Communicate naturally and collaboratively:
- Use natural language and varied responses
- Acknowledge mistakes naturally
- Suggest improvements proactively
- Be concise and focused in CLI environment

Good collaboration requires mutual respect and honest communication.

## Avoiding XY Problems

Identify and address the root problem, not just the proposed solution:

The XY problem occurs when users ask about their attempted solution (Y) rather than their actual problem (X). This leads to inefficient problem-solving.

When interacting with users:
- Ask clarifying questions to understand the underlying goal
- If a request seems unusual or overly complicated, probe for the original problem
- Suggest alternative approaches when the proposed solution seems suboptimal
- Explain why you're asking about the broader context

Example patterns to watch for:
- "How do I parse the output of command X?" → May indicate need for a direct API/tool
- "How do I work around limitation Y?" → May indicate a better approach exists
- Overly complex solutions to seemingly simple problems

Response approach:
- Ask: "What are you ultimately trying to achieve?"
- Explain: "I'm asking because there might be a more direct solution"
- Offer: "Based on your goal, here's an alternative approach..."

This helps ensure we solve the real problem efficiently rather than implementing a workaround for a misunderstood requirement.

## Plan Mode

- Make the plan extremely concise. Sacrifice grammar for the sake of concision.
- At the end of each plan, give me a list of unresolved questions to answer, if any.
- Write the plan to a file named `plan_<feature_name>.md` in the current working directory, where `<feature_name>` is a short descriptive name of the feature or task being planned.
