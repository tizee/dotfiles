#!/usr/bin/env sh

case $(uname -s) in
  Darwin) echo 'not support in macOS'
    ;;
  Linux) 
          # curl -fsSL https://get.docker.com -o get-docker.sh
          # DRY_RUN=1 sh ./get-docker.sh
          # Ubuntu
          distribution_version=$(lsb_release -is)
          if [[ $distribution_version = Ubuntu ]]; then
            sudo apt-get update
            # allow https request
            sudo apt-get install \
              apt-transport-https \
              ca-certificates \
              curl \
              gnupg \
              lsb-release
            # add docker's gpg key
            # could also use `apt-key add`
            curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
            # add docker's repo
            # could also use `add-apt-repository` but the following code is more general
            echo \
              "deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
              $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
             sudo apt-get update
             # install
             # sudo apt-get install docker-ce docker-ce-cli containerd.io
             # could just list all available programs here
             apt-cache madison docker-ce
          fi
    ;;
esac

