cl () {
    if [ $# -le 1 ]
    then
        cd $1
        pwd
        lsd
    else
        echo "too many arguments"
        return 1
    fi
}