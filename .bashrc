#!/bin/bash

if [ "$(uname)" == "Darwin" ]; then
    OS=darwin
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    OS=linux
fi

if [ -f "$HOME/git/bash-git-prompt/gitprompt.sh" ]; then
    source $HOME/git/bash-git-prompt/gitprompt.sh
fi

alias em=/usr/local/Cellar/emacs/24.5/Emacs.app/Contents/MacOS/Emacs

PS1="\[$GREEN\]\t\[$RED\]-\[$BLUE\]\u\[$YELLOW\]\[$YELLOW\]\w\[\033[m\]\[$MAGENTA\]\$(__git_ps1)\[$WHITE\]\$ "

if [ OS == "darwin" ]; then
    export JAVA_HOME="$(/usr/libexec/java_home -v 1.8)"
fi

export PATH=~/git/rebar/:$PATH
export PATH=~/git/rebar/:$PATH

export WITH_OTP_DEBUG_TOOLS=t
export ERL_LIBS=/Users/mjunggr/git/recon
export USE_SSL_DIR=/usr/local/opt/openssl
export ERL_COMPILER_OPTIONS='[debug_info]'
