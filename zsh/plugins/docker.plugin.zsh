#!/usr/bin/env zsh
# oooooooooo.                       oooo                           
# `888'   `Y8b                      `888                           
#  888      888  .ooooo.   .ooooo.   888  oooo   .ooooo.  oooo d8b 
#  888      888 d88' `88b d88' `"Y8  888 .8P'   d88' `88b `888""8P 
#  888      888 888   888 888        888888.    888ooo888  888     
#  888     d88' 888   888 888   .o8  888 `88b.  888    .o  888     
# o888bood8P'   `Y8bod8P' `Y8bod8P' o888o o888o `Y8bod8P' d888b    

# docker container ls / docker container ps
function dk::container::ps(){
  docker container ls
}

function dk::image::ps() {
  docker image ls
}

function dk::help(){
  print "dkcps - docker container ls"
  print "dkips - docker image ls"
}

alias dkcps='dk::container::ps'
alias dkips='dk::image::ps'
alias dkhelp='dk::help'
