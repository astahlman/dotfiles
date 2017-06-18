# Setup fzf
# ---------
FZF_ROOT="$HOME/tools/fzf"
FZF_BIN="$HOME/tools/fzf/bin/"
if [[ -d "$FZF_BIN" ]]; then
    if [[ ! "$PATH" == *$FZF_BIN* ]]; then
      export PATH="$PATH:$FZF_BIN"
    fi
else
  echo "Could not find fzf on the \$PATH. Is it installed?"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "${FZF_ROOT}/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "${FZF_ROOT}/shell/key-bindings.zsh"

# fd - fuzzy cd to selected directory
fd() {
  local dir
  dir=$(find ${1:-.} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir"
}

# fag - fuzzy ag (with an unfortunate mnemonic)
fag() {
  local search_string
  search_string=$(printf ",%s" "$@");
  ag --nobreak --nonumbers --noheading "$*" | fzf
}
