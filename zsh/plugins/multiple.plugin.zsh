#!/usr/bin/env zsh

function mload(){
 if [[ -e /tmp/mcopy.items ]]; then
   export MULTIPLE_COPY_ITEMS=($(</tmp/mcopy.items))
 fi 
 if [[ -e /tmp/mmove.items ]]; then
   export MULTIPLE_COPY_ITEMS=($(</tmp/mmove.items))
 fi 
}

function mreset(){
  export MULTIPLE_MOVE_ITEMS=()
  export MULTIPLE_COPY_ITEMS=()
}

function mprint() {
  echo -n "copy items: "
  echo $MULTIPLE_COPY_ITEMS  
  echo -n "move items: "
  echo $MULTIPLE_MOVE_ITEMS  
}

# append to copy items
function mcopy(){
  unset MULTIPLE_COPY_ITEMS
  export MULTIPLE_COPY_ITEMS=($(realpath -e $@) ${MULTIPLE_COPY_ITEMS[@]})
  # cross session
  echo $MULTIPLE_COPY_ITEMS > /tmp/mcopy.items
  echo "Copied $@" 
}

function mmove(){
  unset MULTIPLE_MOVE_ITEMS
  export MULTIPLE_MOVE_ITEMS=($(realpath -e $@) ${MULTIPLE_COPY_ITEMS[@]})
  # cross session
  echo $MULTIPLE_MOVE_ITEMS > /tmp/mmove.items
  echo "Moved $@"
}

function mpaste(){
  if [[ $# -eq 1 ]]; then
    if [[ ! -z $MULTIPLE_MOVE_ITEMS ]] && [[ -d $1 ]]; then
      mv -v ${MULTIPLE_MOVE_ITEMS[@]} $1
      unset MULTIPLE_MOVE_ITEMS
      # rm if presents
      if [[ -e /tmp/mmove.items ]]; then
        rm -v /tmp/mmove.items
      fi
    elif [[ ! -z $MULTIPLE_COPY_ITEMS ]] && [[ -d $1 ]]; then
      cp -vr ${MULTIPLE_COPY_ITEMS[@]} $1
    elif [[ ! -d $1 ]]; then
      >&2 echo "$1 not a directory"
    else
      >&2 echo "Empty Clipboard"
    fi
  else
    >&2 echo "mpaste [dist]\n"
  fi
}


