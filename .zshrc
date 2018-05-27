# Completion scripts written by the zsh community. Installed through brew
fpath=(/usr/local/share/zsh-completions $fpath)

plugins=(
  # Plugins from oh-my-zsh
  git
  npm
  docker
  hub
  zsh-syntax-highlighting
  zsh_reload
  # My own plugins
  project
  aws-switch
  git-pr
  # colorful-tabs
  theme
  git-fzf
  hello
  reload-function
  danger-danger
)

source $ZSH/oh-my-zsh.sh

# Nice small convenience key-bindings.
# CTRL+T to select a file in the current directory and paste it.
# CTRL+R to get a nicer backwards history search
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

### Tools

# AWS completions
source /usr/local/opt/awscli/libexec/bin/aws_zsh_completer.sh

# OPAM configuration
if which opam > /dev/null; then
  eval `opam config env`
  . /Users/hartmann/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
fi
