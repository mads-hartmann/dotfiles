parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

# Print the directory listing with folders before files. Only list the
# names of the files, no other additional information.
function pretty-ls() {
    ls -gGh \
       --color \
       --group-directories-first \
    | awk 'NR > 1 {print $7 }'
}
