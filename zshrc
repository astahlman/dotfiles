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

export PATH="/usr/local/bin:/usr/local/heroku/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin:/usr/local/git/bin:/usr/local/heroku/bin:/Library/Frameworks/Python.framework/Versions/3.3/bin:/opt/local/bin:/Users/astahlman/Documents/Programming/Tools/adt-bundle-mac-x86_64-20130219/sdk/platform-tools:/Users/astahlman/Documents/Programming/Tools/adt-bundle-mac-x86_64-20130219/sdk/platform-tools"
# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

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
export PATH=$PATH:~/Documents/Programming/Tools/adt-bundle-mac-x86_64-20130219/sdk/platform-tools
set -o vi

# Move to local
export CLOJURESCRIPT_HOME=/Users/astahlman/Documents/Programming/Web/clojurescript

# Move to local
### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# Move to local
### For scrapy ###
export PATH=$PATH:/usr/local/share/python

# Move to local
### For tex ###
export PATH=$PATH:/usr/local/texlive/2014/bin/universal-darwin

# shortcuts to common commands
alias u="cd ..; ls"

# always use mvim within the terminal
#alias vi="mvim -v"
#alias vim="mvim -v"
#alias emacs="open /Applications/Emacs.app"
alias vlc="/Applications/VLC.app/Contents/MacOS/VLC"

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

export PATH=/usr/local/bin:$PATH

bindkey "^R" history-incremental-search-backward

# no idea what this does...
bindkey -e

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

source "$HOME/dotfiles/zshrc_local"

