#!/bin/bash

if [ "$(uname)" == "Darwin" ]; then
    OS=darwin
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    OS=linux
fi

if [ -f "$HOME/git/bash-git-prompt/gitprompt.sh" ]; then
    source $HOME/git/bash-git-prompt/gitprompt.sh
    PS1="\[$GREEN\]\t\[$RED\]-\[$BLUE\]\u\[$YELLOW\]\[$YELLOW\]\w\[\033[m\]\[$MAGENTA\]\$(__git_ps1)\[$WHITE\]\$ "
fi



if [ $OS == "darwin" ]; then
    export JAVA_HOME="$(/usr/libexec/java_home -v 1.8)"
    alias em=/usr/local/Cellar/emacs/24.5/Emacs.app/Contents/MacOS/Emacs
    export USE_SSL_DIR=/usr/local/opt/openssl
    export ERL_LIBS=/Users/mjunggr/git/recon
fi

export PATH=~/git/rebar/:$PATH

export WITH_OTP_DEBUG_TOOLS=t
export ERL_COMPILER_OPTIONS='[debug_info]'
export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx

alias d='dirs -v'
export WITH_OTP_DEBUG_TOOLS=true


export GIT_MERGE_AUTOEDIT=no
export PATH=${PATH}:~/git/git-bootstrap/commands/
if [ -f ~/.git-completion.bash ]; then
    #curl https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash -o ~/.git-completion.bash
  . ~/.git-completion.bash
fi
