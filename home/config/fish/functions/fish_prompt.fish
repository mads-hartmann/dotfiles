function fish_prompt --description 'My preffered prompt'
        # TODO: This only works in the root dir.
        if [ -d .git ]
                set -l git_branch (git rev-parse --abbrev-ref HEAD)
                set git_info " [$git_branch]"
        end

        set path (echo $PWD | sed -e "s|^$HOME|~|")

        set_color purple
        printf "%s" $path
        set_color normal
        printf "%s" $git_info
        printf " \$ "
end
