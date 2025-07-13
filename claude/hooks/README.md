# Claude Code Enhanced Hooks

## 概述

这套增强的hook系统为你的Claude Code会话提供了深度分析和性能跟踪功能。

## 新增功能

### Stop Hook 增强功能
- **会话统计**: 计算总工具使用次数、bash命令数、文件操作数
- **成功率分析**: 自动计算操作成功率
- **Todo状态检查**: 检测是否还有未完成的todo项
- **会话摘要**: 自动生成会话性能摘要报表

### SubagentStop Hook 增强功能
- **任务完成质量分析**: 分析任务完成、进行中、失败的状态
- **工具使用密度统计**: 分析每个子任务中工具的密集程度
- **性能指标存储**: 将每次子任务运行数据存储到SQLite数据库
- **效率评估**: 估算整体任务处理效率

### PreCompact Hook 增强功能
- **压缩影响分析**: 预估压缩前后的大小变化
- **关键洞察保留**: 识别重要关键词进行优先保留
- **压缩历史跟踪**: 记录每次压缩的详细信息和推荐
- **文件清理推荐**: 为交流管理提供智能清理建议

## 轻量级JSON架构设计

### 存储格式对比
- **SQLite**: ❌ 臃肿，需要数据库引擎
- **JSON**: ✅ 轻量，直接可读，Python原生支持
- **CSV**: ❌ 需要额外解析库

### JSON存储结构

#### subagent-metrics.json
```json
[
  {
    "timestamp": "2024-07-13T10:30:00.000000",
    "session_id": "abc123",
    "stats": {
      "transcript_lines": 1523,
      "total_tools": 45,
      "success_count": 42,
      "error_count": 3,
      "bash_commands": 12,
      "file_operations": 15
    }
  }
]
```

#### cleanup-recommendations.json
```json
[
  {
    "timestamp": "2024-07-13T10:30:00.000000",
    "trigger_type": "manual",
    "insights_to_preserve": ["error", "success", "warning"],
    "file_metrics": {
      "lines": 1523,
      "bytes": 48392,
      "estimated_reduction": 8500
    }
  }
]
```

## 安装和配置

### 1. 权限设置
确保所有脚本有可执行权限：
```bash
chmod +x hooks/*.py
```

### 2. 配置验证
JSON文件大小控制：
- 每个metrics最多保存100个记录
- 自动丢弃最早数据，回避文件膨胀
- 纯JSON架构，无需外部依赖

### 3. 更新Claude Code配置

```json
{
  "hooks": {
    "Stop": [
      {
        "matcher": "",
        "hooks": [
          {
            "type": "command",
            "command": "/path/to/your/stop-handler.py"
          }
        ]
      }
    ],
    "SubagentStop": [
      {
        "matcher": "",
        "hooks": [
          {
            "type": "command",
            "command": "/path/to/your/subagent-stop-handler.py"
          }
        ]
      }
    ],
    "PreCompact": [
      {
        "matcher": "",
        "hooks": [
          {
            "type": "command",
            "command": "/path/to/your/pre-compact-handler.py"
          }
        ]
      }
    ]
  }
}
```

## 日志文件结构

所有日志文件会存储在：`~/.claude/logs/` 目录下

- `session-summary.log` - 每次会话的总体摘要
- `task-analysis.log` - 子任务详细分析
- `compaction-tracking.log` - 压缩操作的详细记录
- `cleanup-recommendations.json` - 智能清理推荐
- `subagent-metrics.json` - 轻量级性能指标JSON

## 使用案例

### 案例1: 会话结束后检查完成度
会话结束时自动检查：
- ✅ 本次会话工具使用统计
- ✅ **TodoWrite未完成事项提醒** (独立于Task)
- ✅ 操作成功率和错误统计

**运行示例**:
```
[2024-07-13T15:45:12.123456] Session Summary:
  Total Tools: 23
  Bash Commands: 8
  File Operations: 12
  Success Rate: 95.7%
  Warnings: 2
  Errors: 1
  Warning: 2 todo items are still incomplete
```

### 案例2: 轻量级性能趋势
查看历史会话指标：
```bash
# 查看原始JSON数据
cat ~/.claude/logs/subagent-metrics.json | jq -r '.[-5:][] | {timestamp, stats}'

# 分析工具使用趋势
jq '.[] | [.timestamp, .stats.total_tools, .stats.success_count]' ~/.claude/logs/subagent-metrics.json
```

### 案例3: TodoWrite状态监控
检查当前会话中的TodoWrite状态：
```bash
# 查看最近的未完成TodoWrite
jq '.[0] | check_todo_status()}' <(jot /path/to/transcript.json)
```

### 案例4: 压缩前智能分析
在压缩前获取清理建议：
```bash
# 查看压缩推荐
jq '.[-1]' ~/.claude/logs/cleanup-recommendations.json
```

## 故障排除指南

### 日常检查命令
```bash
# 验证脚本权限
ls -la ~/.claude/hooks/*.py
chmod +x ~/.claude/hooks/*.py

# 查看实时日志
tail -f ~/.claude/logs/session-summary.log

# 查看性能统计
jq length ~/.claude/logs/subagent-metrics.json
```

### 常见问题

#### Q: Stop hook报错的调试方法
```bash
# 手动运行hook测试
echo '{"transcript_path": "/tmp/test.json", "session_id": "test"}' | python3 ~/.claude/hooks/stop-handler.py
```

#### Q: JSON文件过大怎么办？
- ✅ 自动限制100条记录（已内置）
- ✅ 可以直接删除历史文件
- ✅ 不会导致系统性能问题

#### Q: TodoWrite统计不准确？
- ✅ TodoWrite是独立工具，与Task功能无关
- ✅ 通过transcript文件直接读取，无需Task依赖
- ✅ 可以同时跟踪pending/in_progress/completed所有状态

## 数据API参考

### 会话统计字段
```json
{
  "total_tools": "使用的总工具数",
  "bash_commands": "执行的bash命令数", 
  "file_operations": "文件操作次数(R/W/E/ME)",
  "success_count": "成功操作数",
  "error_count": "错误操作数",
  "todo_incomplete": "未完成的TodoWrite项"
}
```

### 快速命令工具箱
```bash
# 一键查看当前会话摘要
alias claude-stats="tail -n 10 ~/.claude/logs/session-summary.log"

# 查看todo状态
alias todo-watch="grep -c 'pending\|in_progress' ~/.claude/logs/session-summary.log"

# 生成今日使用报告
function claude-report() {
  echo "=== Claude Code Daily Report ==="
  echo "Sessions completed:" $(wc -l < ~/.claude/logs/session-summary.log)
  echo "Total tools used:" $(jq '.[] | .stats.total_tools' ~/.claude/logs/subagent-metrics.json | jq -s add)
}
```

## 高级用法

### 集成到您的shell提示符
```bash
# 显示当前会话的工具计数统计
function claude_session_info() {
  local stats_file="$HOME/.claude/logs/session-summary-simple.log"
  if [[ -f "$stats_file" ]]; then
    tail -1 "$stats_file" | grep -o 'Session Summary.*$' || echo "Ready for session"
  fi
}
```

### 创建TodoWrite完成度报告
```bash
#!/bin/bash
# 生成每日TodoWrite完成度报告
logfile="~/.claude/logs/session-summary.log"
if [[ -f $logfile ]]; then
    echo "=== TodoWrite Daily Report ==="
    grep "todo items" "$logfile" | tail -5
fi
```

## 安全考虑

- ✅ **零系统权限**: 仅操作用主目录
- ✅ **可安全删除**: JSON文件可直接删除重置
- ✅ **只读操作**: 除日志外不会修改其他系统文件
- ✅ **容量控制**: 智能限制文件大小防止膨胀

所有hook都是读取和写入当前用户环境的日志文件，并不会执行破坏性操作。所有文件路径都经过验证和权限检查。

## 故障排除

如果hook不工作：
1. 检查JSON配置是否正确
2. 确认所有脚本有执行权限
3. 查看 `~/.claude/logs/` 下的错误日志
4. 使用 `/hooks` 命令验证配置