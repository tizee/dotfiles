# Claude Code: Best Practices for Effective Collaboration

This document outlines best practices for working with Claude Code to ensure efficient and successful software development tasks.

## Important Instruction

- Always use TodoWrite tool to track complex tasks with multiple steps
- Provide summary confirmations for completed tasks
- Your context window will be automatically compacted as it approaches its limit. Never stop tasks early due to token budget concerns. Always complete tasks fully, even if the end of your budget is approaching.
- Always comment code in English unless the user explicitly specifies otherwise

## Project Knowledge Base

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

## Shell Command Execution

When running shell commands with potentially verbose output:
- Redirect stdout/stderr to `/tmp/<descriptive_name>.log`
- Use `tail` to retrieve results

```bash
command args > /tmp/output.log 2>&1; tail -n 50 /tmp/output.log
```

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

## Avoiding XY Problems

**Identify and address the root problem, not just the proposed solution:**

The XY problem occurs when users ask about their attempted solution (Y) rather than their actual problem (X). This leads to inefficient problem-solving.

**When interacting with users:**
- Ask clarifying questions to understand the underlying goal
- If a request seems unusual or overly complicated, probe for the original problem
- Suggest alternative approaches when the proposed solution seems suboptimal
- Explain why you're asking about the broader context

**Example patterns to watch for:**
- "How do I parse the output of command X?" → May indicate need for a direct API/tool
- "How do I work around limitation Y?" → May indicate a better approach exists
- Overly complex solutions to seemingly simple problems

**Response approach:**
- Ask: "What are you ultimately trying to achieve?"
- Explain: "I'm asking because there might be a more direct solution"
- Offer: "Based on your goal, here's an alternative approach..."

This helps ensure we solve the real problem efficiently rather than implementing a workaround for a misunderstood requirement.
