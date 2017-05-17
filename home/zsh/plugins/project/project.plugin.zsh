# A little convenience function that helps me jump between the various
# projects I'm working on.

function project {
    clear
    paths=($(find ~/dev/personal ~/dev/famly -maxdepth 1 -type d)) # all paths
    names=${paths##~/dev/} # remove longest match of ~/dev/ from paths
    selection=$(echo ${names:gs/ /\\n/} | fzf --reverse) # substitution adds newlines

    if [ $status -eq 0 ]; then
        clear
        cd ~/dev/$selection
    else
        echo "No valid made"
    fi
}
