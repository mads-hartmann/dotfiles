# My ZSH theme.
# It's mainly bits and pieces I've stolen and modified from all over the web.

# I love colors.
# ohmyzsh has a function `spectrum_ls` which prints the color-codes of a wide
# varity of colors. They can be accessed using $FG[COLOR_CODE]

ENABLE_K8s_PART=0

function is_in_project {
  git rev-parse --is-inside-work-tree &> /dev/null
  return $status
}

function project_path {
  git rev-parse --show-toplevel
}

function project_name {
  echo $(git config --get remote.origin.url | cut -c 16-)
}

function path_part {
  local part
  if is_in_project
  then echo "%{$fg[blue]%}$(project_name)%{$reset_color%}"
  else echo '%~'
  fi
}

function aws_part {
  local profile="${AWS_PROFILE:-${AWS_DEFAULT_PROFILE}}"

  if [[ -z "${profile}" ]]
  then echo ""
  else echo "%{$FG[248]%}aws%{$reset_color%}:${profile} "
  fi
}

function docker_part {
  if [[ -z ${DOCKER_MACHINE_NAME} ]]
  then echo ""
  else echo "docker:%{$fg[green]%}${DOCKER_MACHINE_NAME}%{$reset_color%} "
  fi
}

function kube_part {
    if [[ ${ENABLE_K8s_PART} == 1 ]]; then
        local context
        context="$(kubectl config current-context)"
        if [[ -z "${context}" ]]
        then echo ""
        else echo "k8s:${context} "
        fi
    fi
}

function prompt_part {
  if is_in_project
  then
    local relative_path="${$(pwd)##$(project_path)}"
    local prompt="%{$fg_bold[blue]%}>%{$reset_color%} "
    if [[ -z "${relative_path}" ]]
    then echo "\n${prompt}"
    else echo "\n${relative_path} ${prompt}"
    fi
  else
    echo ' '
  fi
}

GIT_CB=""
ZSH_THEME_SCM_PROMPT_PREFIX=":%{$fg[green]%}"
ZSH_THEME_GIT_PROMPT_PREFIX=$ZSH_THEME_SCM_PROMPT_PREFIX$GIT_CB
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}*%{$fg[green]%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""

if [[ $TERM = "dumb" ]]
then
    PROMPT="$ "
    RPROMPT=""
else
    PROMPT='$(path_part)$(git_prompt_info)$(docker_part)$(kube_part)$(aws_part)$(prompt_part)'
fi
