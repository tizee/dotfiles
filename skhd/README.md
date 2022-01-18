# skhd

A simple hotkey daemon for macOS.

more on https://github.com/koekeishiya/skhd

```
-V | --verbose: Output debug information
    skhd -V

-P | --profile: Output profiling information
    skhd -P

-v | --version: Print version number to stdout
    skhd -v

-c | --config: Specify location of config file
    skhd -c ~/.skhdrc

-o | --observe: Output keycode and modifiers of event. Ctrl+C to quit
    skhd -o

-r | --reload: Signal a running instance of skhd to reload its config file
    skhd -r

-h | --no-hotload: Disable system for hotloading config file
    skhd -h

-k | --key: Synthesize a keypress (same syntax as when defining a hotkey)
    skhd -k "shift + alt - 7"

-t | --text: Synthesize a line of text
    skhd -t "hello, worldシ"

```
