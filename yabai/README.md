# yabai

## Homebrew installed yabai

```
Copy the example configuration into your home directory:
  cp /opt/homebrew/opt/yabai/share/yabai/examples/yabairc ~/.yabairc
  cp /opt/homebrew/opt/yabai/share/yabai/examples/skhdrc ~/.skhdrc

If you want yabai to be managed by launchd (start automatically upon login):
  yabai --start-service

When running as a launchd service logs will be found in:
  /tmp/yabai_<user>.[out|err].log

If you are using the scripting-addition; remember to update your sudoers file:
  sudo visudo -f /private/etc/sudoers.d/yabai

Build the configuration row by running:
  echo "$(whoami) ALL=(root) NOPASSWD: sha256:$(shasum -a 256 $(which yabai) | cut -d " " -f 1) $(which yabai) --load-sa"

README: https://github.com/koekeishiya/yabai/wiki/Installing-yabai-(latest-release)#configure-scripting-addition

To start tizee/personal/yabai now and restart at login:
  brew services start tizee/personal/yabai
Or, if you don't want/need a background service you can just run:
  /opt/homebrew/opt/yabai/bin/yabai
```

## Update to latest yabai

```
brew upgrade --fetch-HEAD tizee/personal/yabai
```
