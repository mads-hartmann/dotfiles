### Aliases.
#
#    various commands that I'm too lazy to type out all the time.

alias reload=src
alias f=famlydev
alias e=edit
alias k=kubectl
alias epoch="date +%s | tr -d '\n'"

alias gti=git # DWIM version of git.

alias dps="docker ps --format 'table {{.ID}}\t{{.Names}}\t{{.Ports}}\t{{.Status}}'"
alias dc="docker-compose"
alias dm="docker-machine"

alias metap="coursier launch org.scalameta:metap_2.12:3.7.2 -r sonatype:snapshots --"
alias metac='coursier launch org.scalameta:metac_2.12:3.7.2 -r sonatype:snapshots -- -classpath $(coursier fetch org.scala-lang:scala-library:2.12.4 -p)'
alias metacp='coursier launch org.scalameta:metacp_2.12:3.7.2 -r sonatype:snapshots --'

METAC_CURRENT="${HOME}/dev/other/scalameta/semanticdb/metac/target/scala-2.12/metac-assembly-3.7.4-72-afbd949b.jar"
METAP_CURRENT="${HOME}/dev/other/scalameta/semanticdb/metap/.jvm/target/scala-2.12/metap-assembly-3.7.4-72-afbd949b.jar"

alias metac-local="java -jar ${METAC_CURRENT} -usejavacp"
alias metap-local="java -jar ${METAP_CURRENT}"
