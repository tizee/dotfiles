#!/usr/bin/env zsh

# realpath based multiple copy/paste
# gnu's reaplath with -e
# macOS's reaplath with only -q

function mhelp() {
  print "mload:  load /tmp/mcopy.items or /tmp/mmove.items"
  print "mcopy:  write paths of items to /tmp/mcopy.items"
  print "mmove:  write paths of items to /tmp/mmove.items"
  print "mpaste: move or copy items to given path"
}

function mload(){
 if [[ -e /tmp/mcopy.items ]]; then
   export MULTIPLE_COPY_ITEMS=($(</tmp/mcopy.items))
 fi
 if [[ -e /tmp/mmove.items ]]; then
   export MULTIPLE_MOVE_ITEMS=($(</tmp/mmove.items))
 fi
}

function mreset(){
  export MULTIPLE_MOVE_ITEMS=()
  if [[ -e /tmp/mmove.items ]]; then
    rm -v /tmp/mmove.items
  fi
  export MULTIPLE_COPY_ITEMS=()
  if [[ -e /tmp/mcopy.items ]]; then
    rm -v /tmp/mcopy.items
  fi
}

function mclear(){
  mreset
}


function mprint() {
  echo "to copy: "
  echo ${(@F)MULTIPLE_COPY_ITEMS}
  echo "to move: "
  echo ${(@F)MULTIPLE_MOVE_ITEMS}
}

# append to copy items
function mcopy(){
  unset MULTIPLE_COPY_ITEMS
  export MULTIPLE_COPY_ITEMS=($(realpath $@) ${MULTIPLE_COPY_ITEMS[@]})
  # cross session
  echo $MULTIPLE_COPY_ITEMS > /tmp/mcopy.items
  echo "Copied: "
  echo "${(@F)@}"
}

function mmove(){
  unset MULTIPLE_MOVE_ITEMS
  export MULTIPLE_MOVE_ITEMS=($(realpath $@) ${MULTIPLE_COPY_ITEMS[@]})
  # cross session
  echo $MULTIPLE_MOVE_ITEMS > /tmp/mmove.items
  echo "Moved: "
  echo "${(@F)@}"
}

function mpaste(){
  if [[ $# -eq 1 ]]; then
    if [[ ! -z $MULTIPLE_MOVE_ITEMS ]] && [[ -d $1 ]]; then
      mv -v ${MULTIPLE_MOVE_ITEMS[@]} $1
      unset MULTIPLE_MOVE_ITEMS
      # remove temp file if presents
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
