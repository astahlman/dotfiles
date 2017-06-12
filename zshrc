source "$HOME/dotfiles/zshrc_local"
function exists {
  type -f $1 >/dev/null
  return $?
}

if exists local-pre-load-hook; then
    #echo "Pre-hook - sourcing local zshell overrides"
    local-pre-load-hook
fi

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable command auto-correction.
# DISABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

### For scrapy ###
#export PATH=$PATH:/usr/local/share/python

# shortcuts to common commands
alias u="cd ..; ls"

# always use mvim within the terminal
#alias vi="mvim -v"
#alias vim="mvim -v"
#alias emacs="open /Applications/Emacs.app"

# jump to marks
export MARKPATH=$HOME/.marks
function jump { 
    cd -P "$MARKPATH/$1" 2>/dev/null || echo "No such mark: $1"
}
function mark { 
    mkdir -p "$MARKPATH"; ln -s "$(pwd)" "$MARKPATH/$1"
}
function unmark { 
    rm -i "$MARKPATH/$1"
}
function marks {
    \ls -l "$MARKPATH" | tail -n +2 | sed 's/  / /g' | cut -d' ' -f9- | awk -F ' -> ' '{printf "%-10s -> %s\n", $1, $2}'
}

# Use vi keybindings
set -o vi

# <C-r> or <C-s> to search backwards or forwards in shell history
bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward

# Use <C-r> <C-s> to cycle through history while searching
bindkey -M isearch '^R' history-incremental-search-backward
bindkey -M isearch '^S' history-incremental-search-forward

case "$TERM" in
"dumb")
    PS1="> "
    ;;
xterm*|rxvt*|eterm*|screen*)
    # PS1="my fancy multi-line prompt > "
    ;;
*)
    PS1="> "
    ;;
esac

# Specific to OS X
function ppjson {
    cat $1 | python -m json.tool 
}

autoload -U compinit
compinit

#allow tab completion in the middle of a word
setopt COMPLETE_IN_WORD

# expand aliases in non-interactive terminals
setopt aliases

# Generate a random 32-character string
alias make-password="openssl rand -base64 32 | head -c 32"


# Cycle through autocompletions with Ctrl-A and Ctrl-B
# Stolen from @zhout
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down

# Randomly sample a file
function sample {
    if  [[ $# -eq 2 ]]; then
        cmd="awk 'BEGIN {srand()} !/^\$/ { if (rand() <= $1) print \$0}' $2"
    elif [[ $# -eq 1 ]]; then
        cmd="awk 'BEGIN {srand()} !/^\$/ { if (rand() <= $1) print \$0}'"
    else
        echo Usage: sample $ratio [ $filename ]
    fi
    eval "$cmd"
}

function pop-alert {
    command -v terminal-notifier >/dev/null 2>&1 || { echo >&2 "terminal-notifier is not installed. Aborting..."; exit 1; };
    terminal-notifier -title $1 -message $2
}

function alert-when-finished {
    start=$(date +%s)
    if eval "$@"; then
        title="Command Finished"
    else
        title="Command Failed"
    fi
    msg="Elapsed: $(($(date +%s) - $start)) seconds"
    pop-alert $title $msg
}

alias awf="alert-when-finished"

# TODO: Is this actually needed?
# Make TRAMP play well with zsh: https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
#[ $TERM = "dumb" ] && unsetopt zle && PS1="$ "

if exists local-post-load-hook; then
    #echo "Post-hook - sourcing local zshell overrides"
    local-post-load-hook
fi

# Don't store commands prefixed with SPACE in history
setopt HIST_IGNORE_SPACE

set -o vi

# GPG
function gpg-agent-restart {
    killall gpg-agent
    gpg-agent --daemon --enable-ssh-support --write-env-file
    gpg-agent-reload-info
}

function gpg-agent-reload-info {
    source ~/.gpg-agent-info
    export GPG_AGENT_INFO
    export SSH_AUTH_SOCK
    export SSH_AGENT_PID
}

function gpg-agent-restart {
    if test -f ~/.gpg-agent-info && \
            kill -0 `grep GPG_AGENT_INFO $HOME/.gpg-agent-info | cut -d: -f2` 2>/dev/null; then
        gpg-agent-reload-info
    else
        eval `gpg-agent --daemon --write-env-file`
    fi
}

gpg-agent-restart
GPG_TTY=$(tty)
export GPG_TTY

# Use 'e' to open a file in an existing emacs session
function e() {
         emacsclient --no-wait $*
}

function ff() {
         find $(pwd) -name "*${1}*" | tee >(pbcopy)
}

# fuzzyy-finding via fzf
[[ -f "$HOME/.fzf.zsh" ]] && source "$HOME/.fzf.zsh" || echo "[WARNING] Couldn't find ~/.fzf.zsh. Is fzf installed? (https://github.com/junegunn/fzf)" >&2

# Setup Marker to templatize common commands
# Invoke the menu with Ctrl-Space
# Bookmark a command with Ctrl-k
# Jump to next placeholder with Ctrl-t
if [[ -s "$HOME/.local/share/marker/marker.sh" ]]; then
   source "$HOME/.local/share/marker/marker.sh"
else
   echo "[WARNING] Couldn't find marker configuration. Is it installed? (https://github.com/pindexis/marker)" >&2
fi

#############################################################################
# Marker fzf integration                                                    #
#############################################################################
# marker template select
FZF_MARKER_CONF_DIR="$HOME/.local/share/marker"
_fzf_marker_main_widget() {
  if echo "$BUFFER" | grep -q -P "{{"; then
    _fzf_marker_placeholder
  else
    local selected
    if selected=$(cat ${FZF_MARKER_CONF_DIR:-~/.config/marker}/*.txt |
      sed -e "s/\(^[a-zA-Z0-9_-]\+\)\s/${FZF_MARKER_COMMAND_COLOR:-\x1b[38;5;255m}\1\x1b[0m /" \
          -e "s/\s*\(#\+\)\(.*\)/${FZF_MARKER_COMMENT_COLOR:-\x1b[38;5;8m}  \1\2\x1b[0m/" |
      fzf --bind 'tab:down,btab:up' --height=80% --ansi -q "$LBUFFER"); then
      LBUFFER=$(echo $selected | sed 's/\s*#.*//')
    fi
    zle redisplay
  fi
}

_fzf_marker_placeholder() {
  local strp pos placeholder
  strp=$(echo $BUFFER | grep -Z -P -b -o "\{\{[\w]+\}\}")
  strp=$(echo "$strp" | head -1)
  pos=$(echo $strp | cut -d ":" -f1)
  placeholder=$(echo $strp | cut -d ":" -f2)
  if [[ -n "$1" ]]; then
    BUFFER=$(echo $BUFFER | sed -e "s/{{//" -e "s/}}//")
    CURSOR=$(($pos + ${#placeholder} - 4))
  else
    BUFFER=$(echo $BUFFER | sed "s/$placeholder//")
    CURSOR=pos
  fi
}

_fzf_marker_placeholder_widget() { _fzf_marker_placeholder "defval" }

zle -N _fzf_marker_main_widget
zle -N _fzf_marker_placeholder_widget
bindkey "${FZF_MARKER_MAIN_KEY:-\C-@}" _fzf_marker_main_widget
bindkey "${FZF_MARKER_PLACEHOLDER_KEY:-\C-v}" _fzf_marker_placeholder_widget
###########################################################################

# Add every dir under ~/scripts to $PATH (e.g., ~/scripts/lyft_local)
SCRIPT_DIRS=$(find -d $HOME/dotfiles/scripts -type d | tr '\n' ':')
PATH="${PATH}:${SCRIPT_DIRS}"