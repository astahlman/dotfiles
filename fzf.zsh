# Setup fzf
# ---------
if [[ ! "$PATH" == */Users/andrewstahlman/tools/fzf/bin* ]]; then
  export PATH="$PATH:/Users/andrewstahlman/tools/fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/Users/andrewstahlman/tools/fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/Users/andrewstahlman/tools/fzf/shell/key-bindings.zsh"

# fd - fuzzy cd to selected directory
fd() {
  local dir
  dir=$(find ${1:-.} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir"
}

# fag - fuzzay ag (with an unfortunate mnemonic)
fag() {
  local search_string
  search_string=$(printf ",%s" "$@");
  echo "${search_string:1}"
  #ag --nobreak --nonumbers --noheading "$*" | fzf
}
