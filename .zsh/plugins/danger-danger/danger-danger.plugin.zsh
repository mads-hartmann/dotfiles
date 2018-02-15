# Dynamically switch profile in zsh if you're using iTerm
#
# Currently it switches if the kubectl context is named anything like
# %production%
#
# It expects you to have two iTerm profiles (Default and Danger)
#
# Inspired by this gist https://gist.github.com/mrfoto/c6072e4fede3a6fe0f6b

if [[ -n "$ITERM_SESSION_ID" ]]; then
  function change-tab-color() {
    echo -ne "\033]6;1;bg;red;brightness;$1\a"
    echo -ne "\033]6;1;bg;green;brightness;$2\a"
    echo -ne "\033]6;1;bg;blue;brightness;$3\a"
  }

  function change-profile() {
    echo -ne "\033]50;SetProfile=$1\a"
  }

  function precmd-hook () {
    # Note: If this turns out to be too slow I can cache the context for a bit.
    context=$(kubectl config current-context)
    if [[ "$context" =~ "production" ]]; then
        change-profile Danger
        change-tab-color 255 0 0
    else
        echo -ne "\033]6;1;bg;*;Default\a"
        change-profile Default
    fi
  }

  autoload -U add-zsh-hook
  add-zsh-hook precmd precmd-hook
fi
