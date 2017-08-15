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
  if is_in_project
  then echo "%{$fg[red]%}$(project_name)%{$reset_color%}:%{$fg[white]%}${$(pwd)##$(project_path)}%{$reset_color%}"
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

function git_part {
  local git_branch

  if is_in_project; then
    git_branch=$(git rev-parse --abbrev-ref HEAD)
    if [[ ${#git_branch} -gt 30 ]]
    then
      git_branch="${git_branch:0:30}…"
    fi
    echo "git:%{$fg[green]%}${git_branch}%{$reset_color%}"
  fi
}

function docker_part {
  if [[ -z ${DOCKER_MACHINE_NAME} ]]
  then echo ""
  else echo "docker:%{$fg[green]%}${DOCKER_MACHINE_NAME}%{$reset_color%} "
  fi
}

function prompt_symbol {
  echo " %{$fg[yellow]%}%(!.#.λ)%{$reset_color%} "
}

PROMPT='$(path_part)$(prompt_symbol)'
RPROMPT='$(docker_part)$(aws_part)$(git_part)'
