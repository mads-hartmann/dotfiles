# Simple script to open the Github compare with the current branch
# against another.
#
# There are two ways to specify which branch to compare against.
#
#   1. Set DEFAULT_PR_BRANCH
#   2. Pass it as an argument to git-pr, e.g. `git pr staging`
#
# This will most likely break unless you have very simple branch
# names.

function git-pr() {
  DEFAULT_PR_BRANCH=${1:-${DEFAULT_PR_BRANCH:-master}}

  # Make sure we're inside a git repo.
  git rev-parse --is-inside-work-tree &>/dev/null

  if [[ $? != 0 ]]; then
    echo "Not a git repository." 1>&2
    exit 1
  fi

  # get the branch
  branch=$(git symbolic-ref -q --short HEAD)

  # get the repo name

  remote_url="remote.origin.url"

  giturl=$(git config --get "$remote_url")
  if [ -z "$giturl" ]; then
    echo "$remote_url not set." 1>&2
    exit 1
  fi

  giturl=${giturl/git\@github\.com\:/https://github.com/}
  giturl=${giturl%\.git}

  # Open the compare view.
  open "$giturl/compare/$DEFAULT_PR_BRANCH...$branch?expand=1"
}
