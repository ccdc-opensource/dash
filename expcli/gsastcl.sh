#!/bin/sh
# Run GSAS commands

pause()
{
    echo Press Enter key to continue . . .
    read
}

match()
{
    [ $# -gt 1 ] && eval case \"$1\" in $2 \) return 0 \;\; esac
    return 1
}

run()
{
    needpause=1
    if [ $# -gt 1 ]; then
        if match "$1" "*genles"; then
            cp -f $2.EXP $2.EXP.BAK
            $*
            [ -e $2.EXP.BAK ] || return
            echo
            echo -n "Keep $2.EXP file of this run? ( <Y>/N ) >"
            read REPLY
            match "$REPLY" '*n|*N' && cp -f $2.EXP.BAK $2.EXP
            needpause=0
            return
        fi
    fi
    $*
}

IFS=\;
set $*
unset IFS
needpause=1
while [ $# -gt 0 ]; do
    run $1
    shift
done

[ $needpause -ne 0 ] && pause

[ -e expgui.lck ] && rm expgui.lck
exit 0
