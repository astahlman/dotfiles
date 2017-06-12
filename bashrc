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

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
