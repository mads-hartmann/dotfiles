# My ZSH theme.
# It's mainly bits and pieces I've stolen and modified from all over the web.

# I love colors.
# ohmyzsh has a function `spectrum_ls` which prints the color-codes of a wide
# varity of colors. They can be accessed using $FG[COLOR_CODE]

# I really don't want to re-implement the nice git prompt features so I'm
# simply using the functions provided by
# https://github.com/robbyrussell/oh-my-zsh/blob/master/lib/git.zsh
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$FG[051]%}❯%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_ADDED="%{$FG[046]%}❯%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_MODIFIED="%{$FG[226]%}❯%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_RENAMED="%{$FG[123]%}❯%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DELETED="%{$FG[196]%}❯%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_STASHED=''
ZSH_THEME_GIT_PROMPT_UNMERGED=''
ZSH_THEME_GIT_PROMPT_AHEAD=''
ZSH_THEME_GIT_PROMPT_BEHIND=''
ZSH_THEME_GIT_PROMPT_DIVERGED=''

function is_in_project {
  git rev-parse --is-inside-work-tree &> /dev/null
  return $status
}

function project_path {
  git rev-parse --show-toplevel
}

function project_name {
  echo ${$(project_path)##*/}
}

function path_part {
  local part
  if is_in_project
  then
    part="%{$FG[248]%}$(project_name)%{$reset_color%}"
    if [[ ! -z "$(parse_git_dirty)" ]]
    then
      part="${part} "
    else
      part="${part} %{$FG[046]%}✓%{$reset_color%}"
    fi
    part="${part}%{$fg[white]%}${$(pwd)##$(project_path)}%{$reset_color%}"
    echo ${part}
  else echo '%~'
  fi
}

function aws_part {
  local profile="${AWS_PROFILE:-${AWS_DEFAULT_PROFILE}}"

  if [[ -z "${profile}" ]]
  then echo ""
  else echo "aws:%{$fg[green]%}${profile}%{$reset_color%} "
  fi
}

function git_branch_part {
  local git_branch
  if is_in_project; then
    git_branch=$(git_current_branch)
    if [[ ${#git_branch} -gt 30 ]]
    then
      # TODO: I'd love to cut it at feature/FAMLY-XXXX or hotfix/FAMLY-XXXX
      git_branch="${git_branch:0:30}…"
    fi
    echo " %{$fg[green]%}${git_branch}%{$reset_color%}"
  fi
}

function docker_part {
  if [[ -z ${DOCKER_MACHINE_NAME} ]]
  then echo ""
  else echo "docker:%{$fg[green]%}${DOCKER_MACHINE_NAME}%{$reset_color%} "
  fi
}

function prompt_symbol {
  # Nice and quiet.
  echo " "
}

PROMPT='$(path_part)$(git_prompt_status)$(prompt_symbol)'
RPROMPT='$(docker_part)$(aws_part)$(git_branch_part)'
