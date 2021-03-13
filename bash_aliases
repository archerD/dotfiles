# User created file containing aliases I want.

# alias for cmus so it is detachable
alias cmus='screen -q -r cmus || screen -S cmus $(which cmus)'

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

