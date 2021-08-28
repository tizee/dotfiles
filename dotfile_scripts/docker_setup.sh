#!/usr/bin/env sh

case $(uname -s) in
  Darwin) echo 'not support in macOS'
    ;;
  Linux) 
      echo  "add new user group: docker"
      sudo groupadd docker
      echo  "add current user $USER to docker"
      sudo usermod -aG docker $USER
      newgrp docker
    ;;
esac
