if [ $# -le 1 ]
then
    cd $1
    if [ "$1" != "-" ]; then
        pwd
    fi
    lsd
else
    echo "too many arguments"
    return 1
fi
