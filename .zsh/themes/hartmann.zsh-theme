# My ZSH theme.
# It's mainly bits and pieces I've stolen and modified from all over the web.

# I love colors.
# ohmyzsh has a function `spectrum_ls` which prints the color-codes of a wide
# varity of colors. They can be accessed using $FG[COLOR_CODE]

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
  # TODO: I'd love to cut it at feature/FAMLY-XXXX or hotfix/FAMLY-XXXX
  local git_branch
  if is_in_project; then
    git_branch=$(git_current_branch)
    if [[ ${#git_branch} -gt 30 ]]
    then
      git_branch="${git_branch:0:30}…"
    fi
    echo " %{$fg[green]%}${git_branch}%{$reset_color%}"
  fi
}

function git_status_part() {
  # All of the checks are completely stolen from. I've just changed the output
  # https://github.com/robbyrussell/oh-my-zsh/blob/master/lib/git.zsh
  #
  # This shows colorful arrows indicating the state of my current git
  # working tree. It shows four arrows (❯❯❯) that can either have a color or be
  # white. The color depends on what the arrow represents.
  #
  # - the first arrow is green if you've staged additions
  # - the second arrow is yellow if you have unstaged changes
  # - The third arrow is red if you've staged deletions
  #
  local INDEX STATUS
  INDEX=$(command git status --porcelain -b 2> /dev/null)
  STATUS=""

  ZSH_THEME_GIT_PROMPT_ADDED="%{$FG[046]%}❯%{$reset_color%}"
  ZSH_THEME_GIT_PROMPT_MODIFIED="%{$FG[226]%}❯%{$reset_color%}"
  ZSH_THEME_GIT_PROMPT_RENAMED="%{$FG[123]%}❯%{$reset_color%}"
  ZSH_THEME_GIT_PROMPT_DELETED="%{$FG[196]%}❯%{$reset_color%}"

  if [[ -z $(git_current_branch) ]] ; then
    exit 0
  fi

  # Added
  if $(echo "$INDEX" | grep '^A  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_ADDED$STATUS"
  elif $(echo "$INDEX" | grep '^M  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_ADDED$STATUS"
  else
    STATUS="❯$STATUS"
  fi

  # Modified
  if $(echo "$INDEX" | grep '^ M ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  elif $(echo "$INDEX" | grep '^AM ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  elif $(echo "$INDEX" | grep '^ T ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  else
    STATUS="❯$STATUS"
  fi

  # Renamed
  if $(echo "$INDEX" | grep '^R  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_RENAMED$STATUS"
  fi

  # Deleted
  if $(echo "$INDEX" | grep '^ D ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$STATUS"
  elif $(echo "$INDEX" | grep '^D  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$STATUS"
  elif $(echo "$INDEX" | grep '^AD ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$STATUS"
  else
    STATUS="❯$STATUS"
  fi
  echo $STATUS
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

if [[ $TERM = "dumb" ]]
then
    PROMPT="$ "
    RPROMPT=""
else
    PROMPT='$(path_part) $(git_status_part)$(prompt_symbol)'
    RPROMPT='$(docker_part)$(aws_part)$(git_branch_part)'
fi
