# Makefile for Codex skills management
# Syncs skills from Claude Code to Codex

# Source and destination directories
CLAUDE_SKILLS_DIR := ../claude/skills
CODEX_SKILLS_DIR := skills

# Get list of skill directories from Claude
SKILL_DIRS := $(wildcard $(CLAUDE_SKILLS_DIR)/*)

.PHONY: all sync-skills clean list-skills diff help

# Default target
all: sync-skills

# Sync all skills from Claude to Codex
# Uses rsync for efficient incremental updates
sync-skills:
	@echo "Syncing skills from Claude to Codex..."
	@mkdir -p $(CODEX_SKILLS_DIR)
	@rsync -av --delete \
		--exclude='.git' \
		--exclude='.DS_Store' \
		$(CLAUDE_SKILLS_DIR)/ $(CODEX_SKILLS_DIR)/
	@echo "Skills synced successfully."
	@echo ""
	@$(MAKE) --no-print-directory list-skills

# Clean codex skills directory (remove all skills)
clean:
	@echo "Removing all skills from $(CODEX_SKILLS_DIR)..."
	@rm -rf $(CODEX_SKILLS_DIR)/*
	@echo "Skills directory cleaned."

# List all available skills
list-skills:
	@echo "Available skills in Codex:"
	@if [ -d "$(CODEX_SKILLS_DIR)" ]; then \
		for skill in $(CODEX_SKILLS_DIR)/*/; do \
			if [ -f "$$skill/SKILL.md" ]; then \
				name=$$(grep -m1 '^name:' "$$skill/SKILL.md" | sed 's/name:[[:space:]]*//'); \
				desc=$$(grep -m1 '^description:' "$$skill/SKILL.md" | sed 's/description:[[:space:]]*//'); \
				echo "  - $$name: $${desc:0:60}..."; \
			fi \
		done \
	else \
		echo "  (no skills directory)"; \
	fi

# Show differences between Claude and Codex skills
diff:
	@echo "Comparing Claude skills with Codex skills..."
	@diff -rq $(CLAUDE_SKILLS_DIR) $(CODEX_SKILLS_DIR) 2>/dev/null || true

# Help target
help:
	@echo "Codex Skills Makefile"
	@echo ""
	@echo "Usage: make [target]"
	@echo ""
	@echo "Targets:"
	@echo "  sync-skills  - Sync skills from Claude to Codex (default)"
	@echo "  clean        - Remove all skills from Codex"
	@echo "  list-skills  - List all available skills"
	@echo "  diff         - Show differences between Claude and Codex skills"
	@echo "  help         - Show this help message"
	@echo ""
	@echo "Source: $(CLAUDE_SKILLS_DIR)"
	@echo "Destination: $(CODEX_SKILLS_DIR)"
