#!/usr/bin/env zsh

# realpath based multiple copy/paste
# gnu's reaplath with -e
# macOS's reaplath with only -q

function mhelp() {
  print "mload:  load /tmp/mcopy.items or /tmp/mmove.items"
  print "mcp:    write paths of items to /tmp/mcopy.items"
  print "mmv:    write paths of items to /tmp/mmove.items"
  print "mpaste: move or copy items to given path"
  print "mclear: clear related env"
}

function mload(){
 if [[ -e /tmp/mcopy.items ]]; then
   export MULTIPLE_COPY_ITEMS=($(</tmp/mcopy.items))
   echo "load copy items to MULTIPLE_COPY_ITEMS" $MULTIPLE_COPY_ITEMS
 fi
 if [[ -e /tmp/mmove.items ]]; then
   export MULTIPLE_MOVE_ITEMS=($(</tmp/mmove.items))
   echo "load items to MULTIPLE_MOVE_ITEMS" $MULTIPLE_COPY_ITEMS
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
function mcp(){
  local missing_files=()
  local valid_files=()
  
  for file in "$@"; do
    if [[ ! -e "$file" ]]; then
      missing_files+=("$file")
    else
      valid_files+=("$(realpath "$file")")
    fi
  done
  
  if [[ ${#missing_files[@]} -gt 0 ]]; then
    echo "Error: The following files do not exist:"
    printf '  %s\n' "${missing_files[@]}"
    return 1
  fi
  
  if [[ ${#valid_files[@]} -eq 0 ]]; then
    echo "Error: No valid files provided"
    return 1
  fi
  
  unset MULTIPLE_COPY_ITEMS
  export MULTIPLE_COPY_ITEMS=(${valid_files[@]} ${MULTIPLE_COPY_ITEMS[@]})
  # cross session
  echo $MULTIPLE_COPY_ITEMS > /tmp/mcopy.items
  echo "Copied: "
  echo "${(@F)@}"
}

function mmv(){
  local missing_files=()
  local valid_files=()
  
  for file in "$@"; do
    if [[ ! -e "$file" ]]; then
      missing_files+=("$file")
    else
      valid_files+=("$(realpath "$file")")
    fi
  done
  
  if [[ ${#missing_files[@]} -gt 0 ]]; then
    echo "Error: The following files do not exist:"
    printf '  %s\n' "${missing_files[@]}"
    return 1
  fi
  
  if [[ ${#valid_files[@]} -eq 0 ]]; then
    echo "Error: No valid files provided"
    return 1
  fi
  
  unset MULTIPLE_MOVE_ITEMS
  export MULTIPLE_MOVE_ITEMS=(${valid_files[@]} ${MULTIPLE_COPY_ITEMS[@]})
  # cross session
  echo $MULTIPLE_MOVE_ITEMS > /tmp/mmove.items
  echo "Moved: "
  echo "${(@F)@}"
}

function mpaste(){
  if [[ $# -eq 1 ]]; then
    if [[ ! -d $1 ]]; then
      echo "Error: '$1' is not a directory or does not exist"
      return 1
    fi
    
    if [[ ! -z $MULTIPLE_MOVE_ITEMS ]]; then
      local missing_files=()
      for file in ${MULTIPLE_MOVE_ITEMS[@]}; do
        [[ ! -e "$file" ]] && missing_files+=("$file")
      done
      
      if [[ ${#missing_files[@]} -gt 0 ]]; then
        echo "Error: The following files to move no longer exist:"
        printf '  %s\n' "${missing_files[@]}"
        return 1
      fi
      
      mv -v ${MULTIPLE_MOVE_ITEMS[@]} "$1"
      unset MULTIPLE_MOVE_ITEMS
      # remove temp file if presents
      if [[ -e /tmp/mmove.items ]]; then
        rm -v /tmp/mmove.items
      fi
    elif [[ ! -z $MULTIPLE_COPY_ITEMS ]]; then
      local missing_files=()
      for file in ${MULTIPLE_COPY_ITEMS[@]}; do
        [[ ! -e "$file" ]] && missing_files+=("$file")
      done
      
      if [[ ${#missing_files[@]} -gt 0 ]]; then
        echo "Error: The following files to copy no longer exist:"
        printf '  %s\n' "${missing_files[@]}"
        return 1
      fi
      
      cp -vr ${MULTIPLE_COPY_ITEMS[@]} "$1"
    else
      echo "Error: No items in clipboard to paste"
      return 1
    fi
  else
    echo "Usage: mpaste [directory]"
    return 1
  fi
}
