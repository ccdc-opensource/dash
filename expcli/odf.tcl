# $Id: odf.tcl,v 1.7 2004/10/04 16:11:47 toby Exp toby $

# Convert a Laue code as used in SPACEGRP to a number, as used in odfchk
proc LaueCode2number {laueaxis} {
    switch -exact $laueaxis {
	1bar {return 1}
	2/ma -
	2/mb -
	2/mc {return 2}
	mmm  {return 3}
	4/m  {return 4}
	4/mmm {return 5}
	3barR     {return 6}
	"3bar mR" {return 7}
	3bar    {return 8} 
	3barm1 {return 9}
	3bar1m  {return 10}
	6/m    {return 11}
	6/mmm  {return 12}
	"m 3"  {return 13}
	m3m    {return 14}
	default {return ""}
    }
}

# computes a list of ODF (l,m,n) terms for a given spherical harmonic order,
#   sample symmetry and Laue symmetry
proc ComputeODFterms {order ISAMSYM laueaxis} {

    set laue [LaueCode2number $laueaxis]

    set odflist {}
    set ITOT 0
    for {set I 2} {$I <= $order} {incr I 2} {
	for {set M -$I} {$M <= $I} {incr M 1} {
            if {[odfchk $ISAMSYM $I $M]} {
		for {set N -$I} {$N <= $I} {incr N 1} {
		    if {[odfchk $laue $I $N]} {
			incr ITOT
			lappend odflist [list $I $M $N]
		    }
		}
            }
	}
    }
    return $odflist
}

#PURPOSE: To determine if spherical harmonic term C(l,m) is allowed 
#   in LAUE group 
# based on GSAS FUNCTION ODFCHK(LAUE,L,M)
proc odfchk {laue l m} {

    set ODFCHK 0
    if { $l % 2 == 0 && abs($m) <= $l } {
        if { $laue == 0 } {
	    #Cylindricaly symmetric
	    if { $m == 0 } {set ODFCHK 1}
        } elseif { $laue == 1 } {
	    #1-bar
	    set ODFCHK 1
        } elseif { $laue == 2 } {
	    #2/m
	    if { abs($m) % 2 == 0 } {set ODFCHK 1}
        } elseif { $laue == 3 } {
	    #mmm
	    if { abs($m) % 2 == 0 && $m >= 0 } {set ODFCHK 1}
        } elseif { $laue == 4 } {
	    #4/m
	    if { abs($m) % 4 == 0 } {set ODFCHK 1}
        } elseif { $laue == 5 } {
	    #4/mmm
	    if { abs($m) % 4 == 0 && $m >= 0 } {set ODFCHK 1}
        } elseif { $laue == 6 } {
	    #R-3 R
	    if { abs($m) % 3 == 0 } {set ODFCHK 1}
        } elseif { $laue == 7 } {
	    #R-3m R
	    if { abs($m) % 3 == 0 && $m >= 0 } {set ODFCHK 1}
        } elseif { $laue == 8 } {
	    #-3
	    if { abs($m) % 3 == 0 } {set ODFCHK 1}
        } elseif { $laue == 9 } {
	    #-3m1
	    if { abs($m) % 3 == 0 && $m >= 0 } {set ODFCHK 1}
        } elseif { $laue == 10 } {
	    #-31m
	    if { abs($m) % 3 == 0 && $m >= 0 } {set ODFCHK 1}
        } elseif { $laue == 11 } {
	    #6/m
	    if { abs($m) % 6 == 0 } {set ODFCHK 1}
        } elseif { $laue == 12 } {
	    #6/mmm
	    if { abs($m) % 6 == 0 && $m >= 0 } {set ODFCHK 1}
        } elseif { $laue == 13 } {
	    #m3
	    if { $m > 0 } {
		if { $l % 12 == 2 } {
		    if {$m <= ($l/12) } {set ODFCHK 1}
		} else {
		    if {$m <= ($l/12+1) } {set ODFCHK 1}
		}
	    }
        } elseif { $laue == 14 } {
	    #m3m
	    if { $m > 0 } {
		if { $l % 12 == 2 } {
		    if {$m <= ($l/12) } {set ODFCHK 1}
		} else {
		    if {$m <= ($l/12+1) } {set ODFCHK 1}
		}
	    }
	}
    }
    return $ODFCHK
}

# called once to make the ODF (spherical harmonics) pane 
# this gets done the first time the pane is selected
proc MakeODFPane {} {
    global expgui entryvar entrycmd entrybox
    pack [TitleFrame $expgui(odfFrame).f1 -bd 4 \
	      -text "Spherical Harmonic (ODF) Preferential Orientation" \
	      -relief groove] -side top -expand yes -fill x -anchor n
    set expgui(odfFrameTop) [$expgui(odfFrame).f1 getframe]

    grid [frame  $expgui(odfFrameTop).ps] -column 0 -row 0 -sticky w
    # this is where the buttons will go
    pack [label $expgui(odfFrameTop).ps.0 -text "No Phases"] -side left
    
    grid [label $expgui(odfFrameTop).lA -text " title:" \
	    -fg blue ] -column 1 -row 0 -sticky e
    grid columnconfig $expgui(odfFrameTop) 1 -weight 1
    grid [entry $expgui(odfFrameTop).lB -textvariable entryvar(phasename) \
	    -fg blue -width 45] -column 2 -columnspan 10 -row 0 -sticky e
    grid columnconfigure $expgui(odfFrameTop) 1 -weight 1

    set row 1
    set angframe [frame $expgui(odfFrameTop).ang]
    grid $angframe -row 2 -column 0 -columnspan 10
    grid [label $angframe.l -text "Setting\nangles: "] \
	    -column 0 -row $row
    foreach col {1 4 7} var {omega chi phi} lbl {w c f} {
	grid [label $angframe.l$var -text $lbl] \
		-column $col -row $row -padx 5 -sticky e
	set font [$angframe.l$var cget -font]
	$angframe.l$var config -font "Symbol [lrange $font 1 end]"
	incr col
	grid [checkbutton $angframe.r$var \
		-variable entryvar(ODF${var}Ref)] \
		-column $col -row $row 
	incr col
	grid [entry $angframe.e$var \
		-textvariable entryvar(ODF$var) -width 10] \
		-column $col -row $row -padx 5
        set entrybox(ODF$var) $angframe.e$var
    }
    grid [label $angframe.lDamp -text "Damping  "] \
	    -column [incr col] -row $row
    tk_optionMenu $angframe.om entryvar(ODFdampA) 0 1 2 3 4 5 6 7 8 9
    grid $angframe.om -column [incr col] -row $row

    #
    set ordframe [frame $expgui(odfFrameTop).ord]
    grid $ordframe -row 1 -column 0 -columnspan 10
    set col -1
    grid [label $ordframe.lo -text "Spherical\nHarmonic Order: "] \
	    -column [incr col] -row $row
    set ordmenu [tk_optionMenu $ordframe.ord expgui(ODForder) 0]
    $ordmenu delete 0 end
    for {set i 0} {$i <= 34} {incr i 2} {
	$ordmenu insert end radiobutton -variable expgui(ODForder) \
		-label $i -value $i -command SetODFTerms
    }
    $ordframe.ord config -width 3
    grid $ordframe.ord -column [incr col] -row $row

    grid [label $ordframe.ls -text "Sample\nsymmetry: "] \
	    -column [incr col] -row $row
    set expgui(ODFsym) {}
    set expgui(symmenu) [tk_optionMenu $ordframe.sym expgui(ODFsymLbl) \
	    Cylindrical None "Shear (2/m)" "Rolling (mmm)"]
    grid $ordframe.sym -column [incr col] -row $row
    for {set i 0} {$i <= [$expgui(symmenu) index end]} {incr i} {
	$expgui(symmenu) entryconfigure $i -command "SetODFSym $i"
    }
    $ordframe.sym config -width 12
    grid [label $ordframe.lr -text "Refine ODF\ncoefficients"] \
		-column [incr col] -row $row -padx 5 -sticky e
    grid [checkbutton $ordframe.r \
		-variable entryvar(ODFRefcoef)] \
		-column [incr col] -row $row 
    grid [label $ordframe.lDamp -text "Damping  "] \
	    -column [incr col] -row $row
    tk_optionMenu $ordframe.om entryvar(ODFdampC) 0 1 2 3 4 5 6 7 8 9
    grid $ordframe.om -column [incr col] -row $row
    pack [TitleFrame $expgui(odfFrame).f2 -bd 4 \
	      -text "Spherical Harmonic Terms: (l,m,n) & coeff's" \
	      -relief groove] -side top -fill both -expand yes -anchor n
    set canvasfr [$expgui(odfFrame).f2 getframe]

    set expgui(odfFrameCanvas) $canvasfr.canvas
    set expgui(odfFrameScroll) $canvasfr.scroll
    grid [canvas $expgui(odfFrameCanvas) \
	    -scrollregion {0 0 5000 500} -width 0 -height 250 \
	    -yscrollcommand "$expgui(odfFrameScroll) set"] \
	    -row 3 -column 0 -sticky ns
    grid rowconfigure $expgui(odfFrameTop) 3 -weight 1
    scrollbar $expgui(odfFrameScroll) \
	    -command "$expgui(odfFrameCanvas) yview"
    frame $expgui(odfFrameCanvas).fr
    $expgui(odfFrameCanvas) create window 0 0 -anchor nw -window $expgui(odfFrameCanvas).fr
}

proc SetODFSym {i} {
    global expgui
    if {$expgui(ODFsym) == $i} return 
    set expgui(ODFsym) $i
    set expgui(ODForder) 0
    SetODFTerms
}

proc SetODFTerms {} {
    global expgui
    if {$expgui(curPhase) == ""} return
    set curterms [phaseinfo $expgui(curPhase) ODFterms]
    set laueaxis [GetLaue [phaseinfo $expgui(curPhase) spacegroup]]
    set newterms [ComputeODFterms $expgui(ODForder) $expgui(ODFsym) $laueaxis]
    phaseinfo $expgui(curPhase) ODFterms set $newterms
    phaseinfo $expgui(curPhase) ODForder set $expgui(ODForder)
    phaseinfo $expgui(curPhase) ODFsym set $expgui(ODFsym)
    # zero out the new terms
    for {set i [expr [llength $curterms]+1]} \
	    {$i <= [llength $newterms]} {incr i} {
	phaseinfo $expgui(curPhase) ODFcoef$i set 0.
   }
    incr expgui(changed)
    SelectODFPhase $expgui(curPhase)
}

proc DisplayODFPane {} {
    global expgui expmap
    eval destroy [winfo children $expgui(odfFrameTop).ps]
    pack [label $expgui(odfFrameTop).ps.0 -text Phase:] -side left
    foreach num $expmap(phaselist) type $expmap(phasetype) {
	pack [button $expgui(odfFrameTop).ps.$num -text $num \
		-command "SelectODFPhase $num" -padx 1.5m] -side left
	if {$type > 3} {
	    $expgui(odfFrameTop).ps.$num config -state disabled
	}
    }
    # select the current phase
    SelectODFPhase $expgui(curPhase)
}

# select a phase to display & display the ODF terms
# called when pane is displayed (DisplayODFPane), a phase is selected using the
# phase buttons or when the number of terms gets changed (SetODFTerms)
# problem: this seems to be called multiple times -- for reasons that 
#          are unresolved -- but at least it's quick
proc SelectODFPhase {num} {
    global entryvar entrycmd entrybox expmap expgui
    set crsPhase {}
    # if no phase is selected, select the first phase
    if {$num == ""} {set num [lindex $expmap(phaselist) 0]}
    foreach n $expmap(phaselist) type $expmap(phasetype) {
	if {$n == $num && $type <= 3} {
	    set crsPhase $num
	    catch {$expgui(odfFrameTop).ps.$num config -relief sunken}
	} else { 
	    catch {$expgui(odfFrameTop).ps.$n config -relief raised}
	}
    }

    # disable traces on entryvar until we are ready
    set entrycmd(trace) 0

    eval destroy [winfo children $expgui(odfFrameCanvas).fr]

    if {$crsPhase == "" || [llength $expmap(phaselist)] == 0} {
	# blank out the page
	set expgui(ODFsymLbl) {}
	set expgui(ODForder) {}
	foreach var {omega chi phi omegaRef chiRef phiRef \
		dampC dampA Refcoef} {
	    set entrycmd(ODF$var) {}
	    set entryvar(ODF$var) {}
	}
	set entryvar(phasename) {}
	set entryvar(phasename) {}
	grid forget $expgui(odfFrameScroll) 
	set entrycmd(trace) 1
	return
    }
    # phase name
    set entrycmd(phasename) "phaseinfo $crsPhase name"
    set entryvar(phasename) [phaseinfo $crsPhase name]
    # ODFsym   -- sample symmetry (0-3) (*)
    # prevent SetODFTerms from being run
    set expgui(curPhase) {}
    catch {$expgui(symmenu) invoke 0}
    catch {$expgui(symmenu) invoke [phaseinfo $crsPhase ODFsym]}
    set expgui(curPhase) $crsPhase
    # ODForder -- spherical harmonic order (*)
    set expgui(ODForder) [phaseinfo $expgui(curPhase) ODForder]
    # ODFomega -- omega oriention angle (*)
    # ODFchi -- chi oriention angle (*)
    # ODFphi -- phi oriention angle (*)
    # ODFomegaRef -- refinement flag for omega (*)
    # ODFchiRef -- refinement flag for chi (*)
    # ODFphiRef -- refinement flag for phi (*)
    # ODFdampA -- damping for angles (*)
    # ODFdampC -- damping for coefficients (*)
    # ODFRefcoef -- refinement flag for ODF terms (*)
    foreach var {omega chi phi omegaRef chiRef phiRef dampC dampA Refcoef} {
	set entrycmd(ODF$var) "phaseinfo $expgui(curPhase) ODF$var"
	set entryvar(ODF$var) [eval $entrycmd(ODF$var)]
	# reset to black
	catch {$entrybox(ODF$var) config -fg black}
    }
    #
    set row 0
    set term 0
    set col 99
    #     ODFterms -- a list of the {l m n} values for each ODF term (*)
    #     ODFcoefXXX -- the ODF coefficient for for ODF term XXX (*)
    set textureindex 1.0
    foreach lmn [phaseinfo $expgui(curPhase) ODFterms] {
	# make sure that numbers are separated by spaces
	regsub -all -- "-" $lmn " -" lmn
	incr term
	if {$col > 5} {
	    incr row 2
	    grid rowconfig $expgui(odfFrameCanvas).fr $row \
		    -minsize 2 -pad 10
	    set col 0
	}
	set lbl [eval format (%d,%d,%d) $lmn]
	grid [label $expgui(odfFrameCanvas).fr.l$term -text $lbl] \
		-column [incr col] -row $row -sticky s
#	grid columnconfig $expgui(odfFrameCanvas).fr $col -pad 4
	grid [entry $expgui(odfFrameCanvas).fr.e$term \
		-width 10 -textvariable entryvar(ODFcoef$term)] \
		-column $col -row [expr $row+1]
	set entrycmd(ODFcoef$term) "phaseinfo $expgui(curPhase) ODFcoef$term"
	set entryvar(ODFcoef$term) [eval $entrycmd(ODFcoef$term)]
        set entrybox(ODFcoef$term) $expgui(odfFrameCanvas).fr.e$term
	grid columnconfig $expgui(odfFrameCanvas).fr $col -pad 12
	set textureindex [expr {$textureindex + \
		($entryvar(ODFcoef$term) * $entryvar(ODFcoef$term)) \
		/ ((2. * [lindex $lmn 0]) + 1.)}]
    }

    if {$term == 0} {
	grid [label $expgui(odfFrameCanvas).fr.no -text "no terms" \
		-anchor center] \
		-column 0 -row 0 -sticky nsew
    } else {
	incr row 2
	grid [label $expgui(odfFrameCanvas).fr.last \
		-bd 2 -relief sunken -anchor center \
		-text "Texture index = [format %.4f $textureindex]" ] \
		-column 1 -columnspan 6 -row $row -sticky ew
	grid rowconfig $expgui(odfFrameCanvas).fr $row -pad 12
    }
    # resize
    update 
    set sizes [grid bbox $expgui(odfFrameCanvas).fr]
    set maxhgt 220
    # use the scroll for BIG atom lists
    if {[lindex $sizes 3] > $maxhgt} {
	grid $expgui(odfFrameScroll) -sticky ns -column 1 -row 3
	set height $maxhgt
    } else {
	grid forget $expgui(odfFrameScroll) 
	set height [lindex $sizes 3]
    }
    $expgui(odfFrameCanvas) config -scrollregion $sizes \
	    -width [lindex $sizes 2] -height $height
    set entrycmd(trace) 1
}

#debug code
#set expgui(odfFrame) .test
#catch {destroy $expgui(odfFrame)}
#toplevel $expgui(odfFrame)
#MakeODFPane
#DisplayODFPane
