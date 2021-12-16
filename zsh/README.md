# tz-zshconfig

Almost done. Startup costs 0.18s.

## Why not use `oh-my-zsh`?

It prevent me from customizing the Zsh the way I want, i.e., setup my own zsh configuration from bottom up.

Another drawback for me is that each time after updating oh-my-zsh I could find lots of useless files has been modified.

For pursuing a minimal and clean config, I decide to setup my own config. It's not easy but really fun.

By setting up your own config, you will learn:

1. Zsh prompt configuration

2. Zsh prompt variables

3. bash-compatible zsh script

## Benchmark in seconds

Use script `expect-run` from [zsh-framework-benchmark](https://github.com/zimfw/zsh-framework-benchmark) to startup 100 times with only `$HOME/.zshrc`.

| mean     | stddev     | min      | max      |
| -------- | ---------- | -------- | -------- |
| 0.035888 | 0.00523845 | 0.031587 | 0.067983 |
