# User created file containing aliases I want.

# alias for cmus so it is detachable
# tmux version
#alias cmus='tmux new-session -A -D -s cmus "$(which cmus)"'
#alias cmus='tmux attach-session -t cmus || tmux new-session -A -D -s cmus "$(which cmus)"'
# in cmus: :bind -f common q shell tmux detach-client -s cmus
# abduco version, uses ctrl-z to detach (can change by replacing ^z below)
alias cmus='abduco -A -e ^z cmus "$(which cmus)"'

# alias batcat to bat for ease of use
alias bat='batcat'

# alias for gnirehtet since it is a hard command to remember
alias phone='gnirehtet autorun'

# alias for bpytop, related to bashtop
alias btop='bpytop'

# add protection against clobering files
alias mv='mv -i'
alias cp='cp -i'

# aliases for running the factor listener
alias factor-listener="factorcode -run=listener"
alias factor-ui="factorcode -run=ui.tools"

