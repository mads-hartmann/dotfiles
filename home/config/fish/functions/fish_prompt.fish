function fish_prompt --description 'My preffered prompt'

        git rev-parse --is-inside-work-tree > /dev/null ^ /dev/null

        if [ $status != 0 ]
                set git_info ""
        else
                set -l git_branch (git rev-parse --abbrev-ref HEAD)
                set git_info " $git_branch"
        end

        set path (echo $PWD | sed -e "s|^$HOME|~|")

        set_color cyan
        printf "%s" $path
        set_color green
        printf "%s" $git_info
        set_color normal
        printf " "
end
