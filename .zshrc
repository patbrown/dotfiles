#!/bin/zsh
export GPGUSER="Patrick Brown"
export EMAIL="pat@drilling.net"

export ZDOTDIR=$HOME/.config/zsh
source $ZDOTDIR/.zshrc
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory
setopt SHARE_HISTORY
ZSH_DISABLE_COMPFIX=true
export EDITOR="ec"

# Adding some random options (man zshoptions)
setopt autocd extendedglob nomatch menucomplete
setopt interactive_comments
stty stop undef		# Disable ctrl-s to freeze terminal.
zle_highlight=('paste:none')
unsetopt BEEP
#alias ls='ls -lsG'

# Turing on completions
autoload -Uz compinit
zstyle ':completion:*' menu select
# zstyle ':completion::complete:lsof:*' menu yes select
zmodload zsh/complist
# compinit
_comp_options+=(globdots)		# Include hidden files.

# Autoload
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
autoload -Uz colors && colors
export CLICOLOR=1
export LSCOLORS=ExGxBxDxCxEgEdxbxgxcxd
alias l="ls --color=auto"
alias la="l -la"
alias ll="l -l"
# Edit line in emacs terminal with ctrl-e:
autoload edit-command-line; zle -N edit-command-line


# Plugins from GIT
# Plugins
source "$ZDOTDIR/bootstrap"

compinit


# Bootstrapping Functions
source "$ZDOTDIR/bootstrap"
source "$ZDOTDIR/kbd"
source "$ZDOTDIR/alias"
source "$ZDOTDIR/functions"
source "$ZDOTDIR/prompt"
source "$ZDOTDIR/plugins/zsh-autopair/autopair.plugin.zsh"
#source "$ZDOTDIR/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh"
source "$ZDOTDIR/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh"
#source "$ZDOTDIR/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
source "$ZDOTDIR/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh"

case `uname` in
  Darwin)
    source "$ZDOTDIR/macos"
  ;;
  Linux)
    source "$ZDOTDIR/linux"
  ;;
esac

export EA_EDITOR='emacsclient -a "" -c'

export FIREBIRD_HOME=/Library/Frameworks/Firebird.framework/Resources
export DEV_BOX_FS=~/nd/resources/devops/dev/

export PATH=/usr/local/bin:$PATH
export PATH="$HOME/.amplify/bin:$PATH"
export PATH="$FIREBIRD_HOME/bin:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="$HOME/bin/graalvm-ce-java11-22.3.1:$PATH"
alias mmdc="npx -p @mermaid-js/mermaid-cli mmdc -h"
export GRAALVM_HOME=$HOME/bin/graalvm-ce-java11-22.3.1
_bb_tasks() {
    local matches=(`bb tasks |tail -n +3 |cut -f1 -d ' '`)
    compadd -a matches
    _files # autocomplete filenames as well
}
compdef _bb_tasks bb

# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix
export PATH="$HOME/.local/bin:$PATH"
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

function title {
    echo -ne "\033]0;"$*"\007"
}
export FZF_DEFAULT_COMMAND='fd --type f --hidden'
export FZF_DEFAULT_OPTS=

export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS="--header 'ENTER to edit' --reverse --preview='pistol {}' --bind 'enter:execute(emacsclient -t {})' --margin=3%"

export FZF_ALT_C_COMMAND='fd --type d . --color=never --hidden'
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -50'"
source ~/.config/zsh/key-bindings.zsh
