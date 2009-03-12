#!/bin/sh 

# Run RIETAN 2000/FP and display the resulting *.itx if any

usage()
{
    echo Usage: `basename $0` Path_to_RIETAN.exe ins_File \[ task-code \]
    echo
    echo "  task-code: 0 rietan; 1 lst2cif; 2 rietan+orffe; others rietan+lst2cif"
    echo
}

pause()
{
    echo Press Enter key to continue . . .
    read
}

quit()
{
    [ $# -gt 0 ] && usage
    pause
    exit 1
}

match()
{
    [ $# -gt 1 ] && eval case \"$1\" in $2 \) return 0 \;\; esac
    return 1
}

[ $# -lt 2 ] && quit show_usage

RIETANEXE=$1; shift
SAMPLE=$1; shift
[ -e $RIETANEXE -a -e $SAMPLE ] || quit show_usage

RIETAN=`dirname $RIETANEXE`
export RIETAN=$RIETAN

cd `dirname $SAMPLE`
#SAMPLE=`basename ${SAMPLE/%.???/}`
SAMPLE=`basename $SAMPLE | sed -e 's/\.[^.]*$//' `

REFINE=1
LST2CIF=1
ORFFE=
if [ $# -gt 0 ]; then
    case $1 in
        2) ORFFE=1; LST2CIF=;;
        1) REFINE=;;
        0) LST2CIF=;;
    esac
fi

if [ ! -z "$REFINE" ]; then
    [ -e "$SAMPLE.itx" ] && mv -f "$SAMPLE.itx" "$SAMPLE.sav"
    [ ! -z "$ORFFE" -a -e "$SAMPLE.xyz" ] && mv -f "$SAMPLE.xyz" "$SAMPLE.xyz.BAK"
    
    echo RIETAN is now running ...
    #  Input *.ins: Standard input (record length of 80).
    #        *.int: X-ray/neutron diffraction data.
    #        *.bkg: Background intensities.
    #        *.ffe: Input data created by ORFFE for imposing constraints on interatomic distances and/or bond angles.
    #        *.fba: Data created by PRIMA for MEM-based whole-pattern fitting.
    #        *.ffi: Initial integrated intensities for Le Bail refinement.
    # Output *.itx: Data for plotting Rietveld-refinement patterns or a simulated pattern.
    #        *.hkl: Data for Fourier/D synthesis by FOUSYN.
    #        *.xyz: Data for calculating interatomic distances and bond angles by ORFFE.
    #        *.fos: Data for MEM analysis by PRIMA.
    #        *.ffo: Integrated intensities resulting from Le Bail refinement.
    #        *.vcs: VICS (VIsualization of Crystal Structures) text file.
    #        *.lst: Standard output.
    cp -f "$SAMPLE.ins" "$SAMPLE.ins.BAK"
    "$RIETANEXE" "$SAMPLE.ins" "$SAMPLE.int" "$SAMPLE.bkg" "$SAMPLE.itx" "$SAMPLE.hkl" "$SAMPLE.xyz" "$SAMPLE.fos" "$SAMPLE.ffe" "$SAMPLE.fba" "$SAMPLE.ffi" "$SAMPLE.ffo" "$SAMPLE.vcs" | tee "$SAMPLE.lst" || quit
fi

if [ ! -z "$LST2CIF" -a -e "$SAMPLE.lst" ]; then
    	[ -e "$SAMPLE.cif" ] && mv -f "$SAMPLE.cif" "$SAMPLE.cif.BAK"
    	LST2CIF=$RIETAN
        "$RIETAN/lst2cif" "$SAMPLE.lst" || quit
fi

if [ ! -z "$ORFFE" ]; then
    echo ORFFE is now running ...
    #  Input *.xyz: ORFFE instructions.
    # Output *.dst: Standard output.
    #        *.ffe: Data for imposing restraints on interatomic distances and/or bond angles (a copy of *.dst).
    if [ -e  "$SAMPLE.xyz" ]; then
        [ -e "$SAMPLE.ffe" ] && mv -f "$SAMPLE.ffe" "$SAMPLE.ffe.BAK"
        # "$RIETAN/orffe" "$SAMPLE.xyz" | "%RIETAN%tee" "$SAMPLE.dst"
        "$RIETAN/orffe" "$SAMPLE.xyz" > "$SAMPLE.dst" || quit
        echo ORFFE Done
    else
    	echo Cannot find "$SAMPLE.xyz", make sure NDA set correctly
    fi
fi

if [ ! -z "$REFINE" ]; then
    if [ -e "$SAMPLE.itx" ]; then
        if [ -e "$RIETAN/gnuplot/bin/gnuplot" ]; then
             [ -e "$SAMPLE.plt" ] && "%RIETAN%gnuplot/bin/gnuplot" /noend "$SAMPLE.plt"
        else
        	echo
        	echo Cannot run gnuplot!
        	echo No such file: "$RIETAN/gnuplot/bin/gnuplot"
        fi
    fi
    rm -f "$SAMPLE.sav"
    echo 
    echo -n "Keep ins file of this run? ( <Y>/N ) >"
    read REPLY
    match "$REPLY" 'n*|N*' && cp -f "$SAMPLE.ins.BAK" "$SAMPLE.ins"
    exit 0
fi

pause

exit 0

