# https://coderwall.com/p/s-2_nw/change-iterm2-color-profile-from-the-cli

function theme() {
    echo -e "\033]50;SetProfile=$1\a"
}
