# Makefile for Claude Code configuration linking
# Automates symlink creation for settings, commands, skills, and agents

SHELL := /bin/bash
CONFIG_DIR := $(HOME)/.config/claude
CLAUDE_DIR := $(HOME)/.claude

# Source files/directories
SRC_SETTINGS := $(CONFIG_DIR)/claude-code-settings.json
SRC_COMMANDS := $(CONFIG_DIR)/commands
SRC_SKILLS := $(CONFIG_DIR)/skills
SRC_AGENTS := $(CONFIG_DIR)/agents

# Target symlinks
TARGET_SETTINGS := $(CLAUDE_DIR)/settings.json
TARGET_COMMANDS := $(CLAUDE_DIR)/commands
TARGET_SKILLS := $(CLAUDE_DIR)/skills
TARGET_AGENTS := $(CLAUDE_DIR)/agents

# Colors for output
BLUE := \033[0;34m
GREEN := \033[0;32m
YELLOW := \033[0;33m
RED := \033[0;31m
NC := \033[0m # No Color

.PHONY: all install link-settings link-commands link-skills link-agents check clean uninstall help

# Default target
all: install

# Install all symlinks
install: link-settings link-commands link-skills link-agents
	@echo -e "$(GREEN)✓ All Claude Code configurations linked successfully!$(NC)"

# Link settings.json
link-settings:
	@echo -e "$(BLUE)→ Linking settings.json...$(NC)"
	@if [ -L "$(TARGET_SETTINGS)" ]; then \
		echo -e "$(YELLOW)⚠ Symlink already exists: $(TARGET_SETTINGS)$(NC)"; \
		echo -e "  Points to: $$(readlink $(TARGET_SETTINGS))"; \
	elif [ -e "$(TARGET_SETTINGS)" ]; then \
		echo -e "$(RED)✗ File already exists (not a symlink): $(TARGET_SETTINGS)$(NC)"; \
		echo -e "  Please backup and remove it manually if you want to replace it."; \
		exit 1; \
	else \
		mkdir -p "$(CLAUDE_DIR)"; \
		ln -sv "$(SRC_SETTINGS)" "$(TARGET_SETTINGS)"; \
		echo -e "$(GREEN)✓ Settings linked successfully$(NC)"; \
	fi

# Link commands directory
link-commands:
	@echo -e "$(BLUE)→ Linking commands directory...$(NC)"
	@if [ -L "$(TARGET_COMMANDS)" ]; then \
		echo -e "$(YELLOW)⚠ Symlink already exists: $(TARGET_COMMANDS)$(NC)"; \
		echo -e "  Points to: $$(readlink $(TARGET_COMMANDS))"; \
	elif [ -e "$(TARGET_COMMANDS)" ]; then \
		echo -e "$(RED)✗ Directory already exists (not a symlink): $(TARGET_COMMANDS)$(NC)"; \
		echo -e "  Please backup and remove it manually if you want to replace it."; \
		exit 1; \
	else \
		mkdir -p "$(CLAUDE_DIR)"; \
		ln -sv "$(SRC_COMMANDS)" "$(TARGET_COMMANDS)"; \
		echo -e "$(GREEN)✓ Commands linked successfully$(NC)"; \
	fi

# Link skills directory
link-skills:
	@echo -e "$(BLUE)→ Linking skills directory...$(NC)"
	@if [ -L "$(TARGET_SKILLS)" ]; then \
		echo -e "$(YELLOW)⚠ Symlink already exists: $(TARGET_SKILLS)$(NC)"; \
		echo -e "  Points to: $$(readlink $(TARGET_SKILLS))"; \
	elif [ -e "$(TARGET_SKILLS)" ]; then \
		echo -e "$(RED)✗ Directory already exists (not a symlink): $(TARGET_SKILLS)$(NC)"; \
		echo -e "  Please backup and remove it manually if you want to replace it."; \
		exit 1; \
	else \
		mkdir -p "$(CLAUDE_DIR)"; \
		ln -sv "$(SRC_SKILLS)" "$(TARGET_SKILLS)"; \
		echo -e "$(GREEN)✓ Skills linked successfully$(NC)"; \
	fi

# Link agents directory
link-agents:
	@echo -e "$(BLUE)→ Linking agents directory...$(NC)"
	@if [ -L "$(TARGET_AGENTS)" ]; then \
		echo -e "$(YELLOW)⚠ Symlink already exists: $(TARGET_AGENTS)$(NC)"; \
		echo -e "  Points to: $$(readlink $(TARGET_AGENTS))"; \
	elif [ -e "$(TARGET_AGENTS)" ]; then \
		echo -e "$(RED)✗ Directory already exists (not a symlink): $(TARGET_AGENTS)$(NC)"; \
		echo -e "  Please backup and remove it manually if you want to replace it."; \
		exit 1; \
	else \
		mkdir -p "$(CLAUDE_DIR)"; \
		ln -sv "$(SRC_AGENTS)" "$(TARGET_AGENTS)"; \
		echo -e "$(GREEN)✓ Agents linked successfully$(NC)"; \
	fi

# Check status of all symlinks
check:
	@echo -e "$(BLUE)Checking Claude Code configuration status...$(NC)\n"
	@echo -e "$(BLUE)Settings:$(NC)"
	@if [ -L "$(TARGET_SETTINGS)" ]; then \
		echo -e "  $(GREEN)✓ Linked$(NC): $(TARGET_SETTINGS) → $$(readlink $(TARGET_SETTINGS))"; \
	elif [ -e "$(TARGET_SETTINGS)" ]; then \
		echo -e "  $(YELLOW)⚠ Exists but not a symlink$(NC): $(TARGET_SETTINGS)"; \
	else \
		echo -e "  $(RED)✗ Not linked$(NC): $(TARGET_SETTINGS)"; \
	fi
	@echo -e "\n$(BLUE)Commands:$(NC)"
	@if [ -L "$(TARGET_COMMANDS)" ]; then \
		echo -e "  $(GREEN)✓ Linked$(NC): $(TARGET_COMMANDS) → $$(readlink $(TARGET_COMMANDS))"; \
	elif [ -e "$(TARGET_COMMANDS)" ]; then \
		echo -e "  $(YELLOW)⚠ Exists but not a symlink$(NC): $(TARGET_COMMANDS)"; \
	else \
		echo -e "  $(RED)✗ Not linked$(NC): $(TARGET_COMMANDS)"; \
	fi
	@echo -e "\n$(BLUE)Skills:$(NC)"
	@if [ -L "$(TARGET_SKILLS)" ]; then \
		echo -e "  $(GREEN)✓ Linked$(NC): $(TARGET_SKILLS) → $$(readlink $(TARGET_SKILLS))"; \
	elif [ -e "$(TARGET_SKILLS)" ]; then \
		echo -e "  $(YELLOW)⚠ Exists but not a symlink$(NC): $(TARGET_SKILLS)"; \
	else \
		echo -e "  $(RED)✗ Not linked$(NC): $(TARGET_SKILLS)"; \
	fi
	@echo -e "\n$(BLUE)Agents:$(NC)"
	@if [ -L "$(TARGET_AGENTS)" ]; then \
		echo -e "  $(GREEN)✓ Linked$(NC): $(TARGET_AGENTS) → $$(readlink $(TARGET_AGENTS))"; \
	elif [ -e "$(TARGET_AGENTS)" ]; then \
		echo -e "  $(YELLOW)⚠ Exists but not a symlink$(NC): $(TARGET_AGENTS)"; \
	else \
		echo -e "  $(RED)✗ Not linked$(NC): $(TARGET_AGENTS)"; \
	fi

# Remove all symlinks (only removes if they are symlinks)
clean uninstall:
	@echo -e "$(BLUE)→ Removing Claude Code configuration symlinks...$(NC)"
	@if [ -L "$(TARGET_SETTINGS)" ]; then \
		rm -v "$(TARGET_SETTINGS)"; \
		echo -e "$(GREEN)✓ Settings symlink removed$(NC)"; \
	else \
		echo -e "$(YELLOW)⚠ No settings symlink to remove$(NC)"; \
	fi
	@if [ -L "$(TARGET_COMMANDS)" ]; then \
		rm -v "$(TARGET_COMMANDS)"; \
		echo -e "$(GREEN)✓ Commands symlink removed$(NC)"; \
	else \
		echo -e "$(YELLOW)⚠ No commands symlink to remove$(NC)"; \
	fi
	@if [ -L "$(TARGET_SKILLS)" ]; then \
		rm -v "$(TARGET_SKILLS)"; \
		echo -e "$(GREEN)✓ Skills symlink removed$(NC)"; \
	else \
		echo -e "$(YELLOW)⚠ No skills symlink to remove$(NC)"; \
	fi
	@if [ -L "$(TARGET_AGENTS)" ]; then \
		rm -v "$(TARGET_AGENTS)"; \
		echo -e "$(GREEN)✓ Agents symlink removed$(NC)"; \
	else \
		echo -e "$(YELLOW)⚠ No agents symlink to remove$(NC)"; \
	fi
	@echo -e "$(GREEN)✓ Cleanup complete$(NC)"

# Display help
help:
	@echo -e "$(BLUE)Claude Code Configuration Makefile$(NC)\n"
	@echo -e "$(GREEN)Available targets:$(NC)"
	@echo -e "  $(YELLOW)make install$(NC) (or $(YELLOW)make all$(NC))  - Link all configurations"
	@echo -e "  $(YELLOW)make link-settings$(NC)          - Link settings.json only"
	@echo -e "  $(YELLOW)make link-commands$(NC)          - Link commands directory only"
	@echo -e "  $(YELLOW)make link-skills$(NC)            - Link skills directory only"
	@echo -e "  $(YELLOW)make link-agents$(NC)            - Link agents directory only"
	@echo -e "  $(YELLOW)make check$(NC)                  - Check status of all symlinks"
	@echo -e "  $(YELLOW)make clean$(NC) (or $(YELLOW)uninstall$(NC))  - Remove all symlinks"
	@echo -e "  $(YELLOW)make help$(NC)                   - Display this help message"
	@echo -e "\n$(GREEN)Source locations:$(NC)"
	@echo -e "  Settings: $(SRC_SETTINGS)"
	@echo -e "  Commands: $(SRC_COMMANDS)"
	@echo -e "  Skills:   $(SRC_SKILLS)"
	@echo -e "  Agents:   $(SRC_AGENTS)"
	@echo -e "\n$(GREEN)Target locations:$(NC)"
	@echo -e "  Settings: $(TARGET_SETTINGS)"
	@echo -e "  Commands: $(TARGET_COMMANDS)"
	@echo -e "  Skills:   $(TARGET_SKILLS)"
	@echo -e "  Agents:   $(TARGET_AGENTS)"
