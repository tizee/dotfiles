#!/bin/bash

# failed on error
set -e

# git >= 1.8
git submodule update --remote --merge
# git submodule foreach git pull origin master
