# $Id: addcmds.tcl,v 1.40 2006/03/29 03:47:44 toby Exp toby $

#----------- Add Phase routines ----------------------------------------

proc MakeAddPhaseBox {} {
    global expmap expgui

    set expgui(coordList) {}
    set nextphase ""
    foreach p {1 2 3 4 5 6 7 8 9} {
	if {[lsearch $expmap(phaselist) $p] == -1} {
	    set nextphase $p
	    break
	}
    }

    # no more room
    if {$nextphase == ""} {
	MyMessageBox -parent . -title "Add Phase Error" \
		-message "There are already 9 phases. You cannot add more." \
		-icon error
	return
    }

    set np .newphase
    catch {destroy $np}
    toplevel $np
    bind $np <Key-F1> "MakeWWWHelp expgui2.html addphase"

    grid [label $np.l1 -text "Adding phase #$nextphase"] \
	    -column 0 -row 0 -sticky w
    grid [label $np.l2 -text "Phase title:"] -column 0 -row 1 
    grid [entry $np.t1 -width 68] -column 1 -row 1 -columnspan 8
    grid [label $np.l3 -text "Space Group:"] -column 0 -row 2 
    grid [entry $np.t2 -width 12] -column 1 -row 2 
    grid [frame $np.f -bd 4 -relief groove] -column 3 -row 2 -columnspan 8
    set col -1
    foreach i {a b c} {
	grid [label $np.f.l1$i -text " $i "] -column [incr col] -row 1
	grid [entry $np.f.e1$i -width 12] -column [incr col]  -row 1
    }
    set col -1
    foreach i {a b g} {
	grid [label $np.f.l2$i -text $i] -column [incr col] -row 2
	set font [$np.f.l2$i cget -font]
	$np.f.l2$i config -font "Symbol [lrange $font 1 end]"
	grid [entry $np.f.e2$i -width 12] -column [incr col]  -row 2
	$np.f.e2$i insert 0 90.
    }   
    
    grid [frame $np.bf] -row 3 -column 0 -columnspan 10 -sticky ew
    grid [button $np.bf.b1 -text Add \
	    -command "addphase $np"] -column 2 -row 3
    bind $np <Return> "addphase $np"
    grid [button $np.bf.b2 -text Cancel \
	    -command "destroy $np"] -column 3 -row 3
    grid columnconfig $np.bf 4 -weight 1
    grid [button $np.bf.help -text Help -bg yellow \
	    -command "MakeWWWHelp expgui2.html addphase"] \
	    -column 4 -row 3

    # get the input formats if not already defined
    GetImportFormats
    if {[llength $expgui(importFormatList)] > 0} {
	grid [frame $np.bf.fr -bd 4 -relief groove] -column 5 -row 3
	grid [button $np.bf.fr.b3 -text "Import phase from: " \
		-command "ImportPhase \$expgui(importFormat) $np"] \
		-column 0 -row 0 -sticky e
	set menu [eval tk_optionMenu $np.bf.fr.b4 expgui(importFormat) \
		$expgui(importFormatList)]
	for {set i 0} {$i <= [$menu index end]} {incr i} {
	    $menu entryconfig $i -command "ImportPhase \$expgui(importFormat) $np"
	}
	grid $np.bf.fr.b4 -column 1 -row 0 -sticky w
	grid rowconfig $np.bf.fr 0 -pad 10
	grid columnconfig $np.bf.fr 0 -pad 10
	grid columnconfig $np.bf.fr 1 -pad 10
    }
    wm title $np "add new phase"

    # set grab, etc.
    putontop $np
    
    tkwait window $np
    
    # fix grab...
    afterputontop
}

proc addphase {np {title {}} {input {}} {confirm 1}} {
    global expgui expmap
    # validate the input
    set err {}
    if {$title == ""} {
      set guimode 1
      set title [$np.t1 get]
    } else {
      set guimode 0
      set np .tmpphasewin
      if ![winfo exists $np] {label $np}
    }
    if {[string trim $title] == ""} {
	append err "  Title cannot be blank\n"
    }
    set spg [expr {$guimode? [$np.t2 get] : [lindex $input 0]}]
    if {[string trim $spg] == ""} {
	append err "  Space group cannot be blank\n"
    }
    foreach i {a b c} j {0 1 2} {
	set cell($i) [expr {$guimode? [$np.f.e1$i get] : [lindex [lindex $input 1] $j]}]
	if {[string trim $cell($i)] == ""} {
	    append err "  $i cannot be blank\n"
	} elseif {[catch {expr $cell($i)}]} {
	    append err "  $i is not valid\n"
	}
    }

    foreach i {a b g} lbl {alpha beta gamma} j {3 4 5} {
	set cell($lbl) [expr {$guimode? [$np.f.e2$i get] : [lindex [lindex $input 1] $j]}]
	if {[string trim $cell($lbl)] == ""} {
	    append err "  $lbl cannot be blank\n"
	} elseif {[catch {expr $cell($lbl)}]} {
	    append err "  $lbl is not valid\n"
	}
    }

    if {$err != ""} {
	MyMessageBox -parent . -title "Add Phase Error" \
		-message "The following error(s) were found in your input:\n$err" \
		-icon error
	set expgui(oldphaselist) -1
	return 1
    }

    # check the space group
    set fp [open spg.in w]
    puts $fp "N"
    puts $fp "N"
    puts $fp $spg
    puts $fp "Q"
    close $fp
    global tcl_platform
    catch {
	if {$tcl_platform(platform) == "windows"} {
	    exec [file join $expgui(gsasexe) spcgroup.exe] < spg.in >& spg.out
	} else {
	    exec [file join $expgui(gsasexe) spcgroup] < spg.in >& spg.out
	}
    }
    set fp [open spg.out r]
    set out [read $fp]
    close $fp
    # attempt to parse out the output (fix up if parse did not work)
    if {[regexp "space group symbol.*>(.*)Enter a new space group symbol" \
	    $out a b ] != 1} {set b $out}
    if {[string first Error $b] != -1} {
	# got an error, show it
	ShowBigMessage \
		 $np.error \
		 "Error processing space group\nReview error message below" \
		 $b OK "" 1
	set expgui(oldphaselist) -1
	return 1
    } elseif {$confirm} {
	# show the result and confirm
	set opt [ShowBigMessage \
		$np.check \
		"Check the symmetry operators in the output below" \
		$b \
		{Continue Redo} ]
	if {$opt > 1} {
	    set expgui(oldphaselist) -1
	    return 1
	}
    }
    file delete spg.in spg.out
    
    # ok do it!
    set fp [open exptool.in w]
    puts $fp "P"
    puts $fp $title
    puts $fp $spg
    puts $fp "$cell(a) $cell(b) $cell(c) $cell(alpha) $cell(beta) $cell(gamma)"
    puts $fp "/"
    close $fp
    global tcl_platform
    # Save the current exp file
    savearchiveexp
    # disable the file changed monitor
    set expgui(expModifiedLast) 0
    set expnam [file root [file tail $expgui(expfile)]]
    # save the previous phase list
    set expgui(oldphaselist) $expmap(phaselist)
    catch {
	if {$tcl_platform(platform) == "windows"} {
	    exec [file join $expgui(gsasexe) exptool.exe] $expnam \
		    < exptool.in >& exptool.out
	} else {
	    exec [file join $expgui(gsasexe) exptool] $expnam \
		    < exptool.in >& exptool.out
	}
    } errmsg
    # load the revised exp file
    set oldphaselist $expmap(phaselist)
    loadexp $expgui(expfile)
    set fp [open exptool.out r]
    set out [read $fp]
    close $fp
    destroy $np
    set err 0
    if {[llength $oldphaselist] == [llength $expmap(phaselist)]} {set err 1}
    if {$errmsg != ""} {
	set err 1
	append errmsg "\n" $out
    } else {
	set errmsg $out
    }
    if {$expgui(showexptool) || $err} {
	set msg "Please review the result from adding the phase" 
	if {$err} {append msg "\nIt appears an error occurred!"}
	ShowBigMessage $np $msg $errmsg OK "" $err
    }
    file delete exptool.in exptool.out
    # set the powpref warning (2 = required)
    set expgui(needpowpref) 2
    set msg "A phase was added" 
    if {[string first $msg $expgui(needpowpref_why)] == -1} {
	append expgui(needpowpref_why) "\t$msg\n"
    }
    # now select the new phase
    if $guimode {
    	SelectOnePhase [lindex $expmap(phaselist) end]
    }
		return 0
}

#----------- Add Histogram routines --------------------------------------
proc LabelInstParm {args} {
    global newhist
    switch $newhist(insttype) {
	TOF {
	    set newhist(instfiletext) "Neutron Time of Flight"
	    catch {
		set b $newhist(setnum)
		append newhist(instfiletext) ", 2theta = $newhist(inst${b}Angle)"
	    }
	}
	ED {set newhist(instfiletext) "X-ray Energy Dispersive"}
	"CW X" {set newhist(instfiletext) "CW X-ray"}
	"CW N" {set newhist(instfiletext) "CW Neutron"}
    }
}
trace variable newhist(setnum) w LabelInstParm
trace variable newhist(LimitMode) w ClearHistLimit
set newhist(LimitMode_boxes) {}

proc ClearHistLimit {args} {
    global newhist
    if {$newhist(LimitMode) == 1} {return}
    foreach box $newhist(LimitMode_boxes) {
	catch {$box delete 0 end}
    }
}

proc MakeAddHistBox {} {
    global expmap newhist

    # --> should check here if room for another histogram, but only texture
    # folks will ever need that

    set np .newhist
    catch {destroy $np}
    toplevel $np
    bind $np <Key-F1> "MakeWWWHelp expgui3.html AddHist"

    grid [label $np.l0 -text "Adding a new histogram"] \
	    -column 0 -row 0 -sticky ew -columnspan 7
    grid [checkbutton $np.d0 -text "Dummy Histogram" -variable newhist(dummy) \
	    -command "PostDummyOpts $np" \
	    ] -column 2 -row 0 -columnspan 99 -sticky e
    grid [label $np.l1 -text "Data file:"] -column 0 -row 2
    grid [label $np.t1 -textvariable newhist(rawfile) -bd 2 -relief ridge] \
	    -column 1 -row 2 -columnspan 3 -sticky ew
    grid [button $np.b1 -text "Select File" \
	    -command "getrawfile $np" \
	    ] -column 4 -row 2

    grid [label $np.lbank -text "Select bank" -anchor w] -column 1 -row 3 -sticky w
    grid [frame $np.bank]  -column 2 -row 3 -columnspan 7 -sticky ew

    grid [label $np.l2 -text "Instrument\nParameter file:"] -column 0 -row 5
    grid [label $np.t2 -textvariable newhist(instfile) -bd 2 -relief ridge] \
	    -column 1 -row 5 -columnspan 3 -sticky ew
    grid [button $np.b2 -text "Select File" \
	    -command "getinstfile $np" \
	    ] -column 4 -row 5
    grid [button $np.edit -text "Edit file" \
	    -command {EditInstFile $newhist(instfile)}] \
	    -column 5 -row 5

    grid [label $np.lset -text "Select set" -anchor w] -column 1 -row 6 -sticky w
    grid [frame $np.set]  -column 2 -row 6 -columnspan 7 -sticky ew
    grid [label $np.t2a -textvariable newhist(instfiletext) \
	    -justify center -anchor center -fg blue] \
	    -column 0 -row 8 -columnspan 99 -sticky ew

    grid [button $np.f6a -text "Run\nRAWPLOT" -command "RunRawplot $np"] \
	    -column 4 -row 18 -rowspan 2
    grid [label $np.l3 -text "Usable data limit:"] -column 0 -row 18 -rowspan 3 
    grid [entry $np.e3 -width 12 -textvariable newhist(2tLimit) \
	    ] -column 1 -row 18 -rowspan 3
    grid [radiobutton $np.cb3 -text "d-min" -variable newhist(LimitMode) \
	    -value 0] -column 2 -row 18 -sticky w
    grid [radiobutton $np.cb4 -textvariable newhist(limitLbl)  \
	    -variable newhist(LimitMode) -anchor w -justify l \
	    -value 1] -column 2 -row 20 -sticky w
    set newhist(LimitMode_boxes) $np.e3
    grid [radiobutton $np.cb5 -text "Q-max" -variable newhist(LimitMode) \
	    -value 2] -column 2 -row 19 -sticky w
    set newhist(limitLbl) "TOF-min\n2-Theta Max"
    # spacers
    grid [frame $np.sp0 -bg white] \
	    -columnspan 20 -column 0 -row 1 -sticky nsew -ipady 2
    grid [frame $np.sp1 -bg white] \
	    -columnspan 20 -column 0 -row 4 -sticky nsew -ipady 2
    grid [frame $np.sp2 -bg white] \
	    -columnspan 20 -column 0 -row 17 -sticky nsew -ipady 2
    grid [frame $np.sp3 -bg white] \
	    -columnspan 20 -column 0 -row 98 -sticky nsew -ipady 2
    grid [frame $np.f6] -column 0 -row 99 -columnspan 5 -sticky ew 
    grid [button $np.f6.b6a -text Add \
	    -command "addhist $np"] -column 0 -row 0
    bind $np <Return> "addhist $np"
    grid [button $np.f6.b6b -text Cancel \
	    -command "destroy $np"] -column 1 -row 0
    grid [button $np.f6.b6c -text "Add multiple banks" \
	    -command "addMultiplehist $np" -state disabled] -column 2 -row 0
    grid [button $np.f6.help -text Help -bg yellow \
	    -command "MakeWWWHelp expgui3.html AddHist"] \
	    -column 2 -row 0 -sticky e
    grid columnconfigure $np.f6 2 -weight 1
    grid columnconfigure $np 3 -weight 1

    # dummy histogram stuff
    frame $np.d1
    grid [label $np.d1.l1 -text min] -column 1 -row 1
    grid [label $np.d1.l2 -text max] -column 2 -row 1
    grid [label $np.d1.l3 -text step] -column 3 -row 1
    grid [label $np.d1.lu -text ""] -column 4 -row 1 -rowspan 2
    grid [entry $np.d1.e1 -width 10 -textvariable newhist(tmin)] -column 1 -row 2
    grid [entry $np.d1.e2 -width 10 -textvariable newhist(tmax)] -column 2 -row 2
    grid [entry $np.d1.e3 -width 10 -textvariable newhist(tstep)] -column 3 -row 2
    grid [label $np.d1.m1 -anchor w] -column 1 -row 3 -sticky ew
    grid [label $np.d1.m2 -anchor w] -column 2 -row 3 -sticky ew
    label $np.dl1 -text "Data range:"
    label $np.dl2 -text "Allowed"
    label $np.dl3 -text "\n" -justify left -fg blue
    wm title $np "add new histogram"

    set newhist(banknum) {}
    set newhist(setnum) {}
    if {[string trim $newhist(rawfile)] != {}} {
	validaterawfile $np $newhist(rawfile)
    }
    if {[string trim $newhist(instfile)] != {}} {
	validateinstfile $np $newhist(instfile)
    }

    PostDummyOpts $np
    # set grab, etc.
    putontop $np

    tkwait window $np

    # fix grab...
    afterputontop
}

# convert a file to Win-95 direct access
proc WinCvt {file win} {
    global expgui
    if ![file exists $file] {
	MyMessageBox -parent $win -title "Convert Error" \
		-message "File $file does not exist" -icon error
	return
    }

    set tmpname "[file join [file dirname $file] tempfile.xxx]"
    set oldname "[file rootname $file].org"
    if [file exists $oldname] {
	set ans [MyMessageBox -parent $win -title "OK to overwrite?" \
		-message "File [file tail $oldname] exists in [file dirname $oldname]. OK to overwrite?" \
		-icon question -type yesno -default yes]
	if {$ans == "no"} return
	catch {file delete $oldname}
    }

    if [catch {
	set in [open $file r]
	# needed to test under UNIX
	set out [open $tmpname w]
	fconfigure $out -translation crlf
	set len [gets $in line]
	if {$len > 160} {
	    # this is an old-style UNIX file. Hope there are no control characters
	    set i 0
	    set j 79
	    while {$j < $len} {
		puts $out [string range $line $i $j]
		incr i 80
		incr j 80
	    }
	} else {
	    while {$len >= 0} {
		append line "                                        "
		append line "                                        "
		set line [string range $line 0 79]
		puts $out $line
		set len [gets $in line]
	    }
	}
	close $in
	close $out
	file rename $file $oldname
	file rename $tmpname $file
    } errmsg] {
	MyMessageBox -parent $win -title Notify \
		-message "Error in conversion:\n$errmsg" -icon warning
    }
    return $file
}

proc getrawfile {np} {
    global newhist tcl_platform
    if {$tcl_platform(platform) == "windows"} {
	set inp [
	tk_getOpenFile -parent $np -initialfile $newhist(rawfile) -filetypes {
	    {"Data files" .GSAS} {"Data files" .GSA} 
	    {"Data files" .RAW}  {"All files" *}
	}
	]
    } else {
	set inp [
	tk_getOpenFile -parent $np -initialfile $newhist(rawfile) -filetypes {
	    {"Data files" .GSA*} {"Data files" .RAW}  
	    {"Data files" .gsa*} {"Data files" .raw}  
	    {"All files" *}
	} 
	]
    }
    validaterawfile $np $inp
}

proc validaterawfile {np inp} {
    global expgui newhist
    if {$inp == ""} return
    if {$np != ""} {
      set guimode 1
    } else {
      set guimode 0
      set np .tmprawwin
      if ![winfo exists $np] {label $np}
    }
    if [catch {set in [open $inp r]}] {
	MyMessageBox -parent $np -title "Open error" \
		-message "Unable to open file $inp" -icon error
	return 1
    }
    set newhist(banklist) {}
    if $guimode {foreach child [winfo children $np.bank] {destroy $child}}
    # is this a properly formatted file?
    # -- are lines the correct length & terminated with a CR-LF?    
    fconfigure $in -translation lf
    set i 0
    while {[set len [gets $in line]] > 0} {
	incr i
	if {$len != 81 || [string range $line end end] != "\r"} {
	    set ans [MyMessageBox -parent $np -title "Convert?" \
		    -message "File $inp is not in the correct format for GSAS.\nOK to convert?" \
		    -icon warning -type {OK Quit} -default OK]
	    if {$ans == "ok"} {
		# convert and reopen the file
		close $in
		WinCvt $inp $np
		set i 0
		set in [open $inp r]
		fconfigure $in -translation lf
		set line {}
	    } else {
		return 1
	    }
	}
	# scan for BANK lines
	if {[string first BANK $line] == 0} {
	    scan $line "BANK%d" num
	    lappend newhist(banklist) $num
	    # compute last point
	    set tmin 0
	    set tmax 0
	    catch {
		scan $line "BANK%d%d%d%s%f%f" num nchan nrec rest start step
		set tmin [expr $start/100.]
		set tmax [expr ($start + $step*($nchan-1))/100.]
	    }
	    set newhist(tmin$num) $tmin
	    set newhist(tmax$num) $tmax
	}
	# check for "Instrument parameter file" line
	if {$i == 2 && [string first "Instrument parameter" $line] == 0} {
      set insfile [string trim [string range $line 26 end]]
      if {[file dirname $inp] != "."} {
        set insfile [file join [file dirname $inp] $insfile]
      }
      validateinstfile [expr {$guimode? $np : {}}] $insfile
	}
    }
    # were banks found?
    if {$newhist(banklist) == ""} {
	MyMessageBox -parent $np -title "Read error" \
		-message "File $inp has no BANK lines.\nThis is not a valid GSAS data file." \
		-icon warning
	return 1
    }
    # don't use a full path unless needed
    if {[pwd] == [file dirname $inp]} {
	set newhist(rawfile) [file tail $inp]
    } else {
	set newhist(rawfile) $inp
    }
    set row 0
    set col -1
    set flag 0
    foreach i $newhist(banklist) {
	if {$col > 8} {
	    set col -1
	    incr row
	}
	if $guimode {
  grid [radiobutton $np.bank.$i -text $i -command SetTmax \
		-variable newhist(banknum) -value $i] \
		-column [incr col] -row $row -sticky w
	}
# only 1 choice, so set it
	if {[llength $newhist(banklist)] == 1} {
	    set newhist(banknum) $i
	    SetTmax
	} else {
	    set flag 1
	}
    }
    if {$flag} {
	set newhist(2tLimit) {}
	set newhist(LimitMode) {}
    }
  if $guimode {SetMultipleAdd $np}
	return 0
}

proc SetTmax {} {
    global newhist
    set num $newhist(banknum)
    if {$newhist(insttype) == "TOF"} {
	set newhist(2tLimit) [expr {$newhist(tmin$num) / 10.}]
	if {[llength $newhist(banklist)] == $newhist(instbanks)} {
	    set newhist(setnum) $newhist(banknum)
	}
    } else {
	set newhist(2tLimit) $newhist(tmax$num)
    }
    set newhist(LimitMode) 1

}

proc getinstfile {np} {
    global newhist tcl_platform
    if {$tcl_platform(platform) == "windows"} {
	set inp [
	tk_getOpenFile -parent $np -initialfile $newhist(instfile) -filetypes {
	    {"Inst files" .INST} {"Inst files" .INS} 
	    {"Inst files" .PRM} {"All files" *}
	}
	]
    } else {
	set inp [
	tk_getOpenFile -parent $np -initialfile $newhist(instfile) -filetypes {
	    {"Inst files" .INS*} {"Inst files" .ins*} 
	    {"Inst files" .PRM}  {"Inst files" .prm} 
	    {"All files" *}
	}
	]
    }
    set newhist(setnum) {}
    validateinstfile $np $inp
}

proc validateinstfile {np inp} {
    global expgui newhist
    if {$np != ""} {
      set guimode 1
    } else {
      set guimode 0
      set np .tmpinstwin
      if ![winfo exists $np] {label $np}
    }
    if {$inp == ""} return
    if [catch {set in [open $inp r]}] {
	MyMessageBox -parent $np -title "Open error" \
		-message "Unable to open file $inp" -icon error
	return 1
    }
    set newhist(instbanks) {}
    if $guimode {foreach child [winfo children $np.set] {destroy $child}}
    # is this a properly formatted file?
    # -- are lines the correct length & terminated with a CR-LF?    
    fconfigure $in -translation lf
    while {[set len [gets $in line]] > 0} {
	if {$len != 81 || [string range $line end end] != "\r"} {
	    set ans [MyMessageBox -parent $np -title "Convert?" \
		    -message "File $inp is not in the correct format for GSAS.\nOK to convert?" \
		    -icon warning -type {OK Quit} -default OK]
	    if {$ans == "ok"} {
		# convert and reopen the file
		close $in
		WinCvt $inp $np
		set in [open $inp r]
		fconfigure $in -translation lf
		set line {}
	    } else {
		return 1
	    }
	}
	# scan for the INS   BANK line
	if {[string first "INS   BANK" $line] == 0} {
	    set newhist(instbanks) \
		    [string trim [string range $line 12 end]]
	}
	# scan for the INS   HTYPE line
	if {[string first "INS   HTYPE" $line] == 0} {
	    if {[string index [lindex $line 2] 2] == "T"} {
		set newhist(insttype) TOF
	    } elseif {[string index [lindex $line 2] 2] == "E"} {
		set newhist(insttype) ED
	    } elseif {[string index [lindex $line 2] 1] == "X"} {
		set newhist(insttype) "CW X"
	    } else {
		set newhist(insttype) "CW N"
	    }
	}
	# scan for the instrument constants
	if {[regexp {INS ([ 1-9][0-9]) ICONS(.*)} $line a b c]} {
	    set b [string trim $b]
	    set newhist(inst${b}ICONS) [string trim $c]
	}
	if {[regexp {INS ([ 1-9][0-9])I ITYP(.*)} $line a b c]} {
	    set b [string trim $b]
	    set newhist(inst${b}ITYP) [string trim $c]
	}
	if {[regexp {INS ([ 1-9][0-9])BNKPAR(.*)} $line a b c]} {
	    set b [string trim $b]
	    set newhist(inst${b}Angle) [string trim [lindex $c 1]]
	}
    }
    # were banks found?
    if {$newhist(instbanks) == ""} {
	MyMessageBox -parent $np -title "Read error" -message \
		"File $inp has no \"INS   BANK\" line.\nThis is not a valid GSAS Instrument Parameter file." \
		-icon warning
	return 1
    }
    # don't use a full path unless needed
    if {[pwd] == [file dirname $inp]} {
	set newhist(instfile) [file tail $inp]
    } else {
	set newhist(instfile) $inp
    }
    set col -1
    set row 0
    for {set i 1} {$i <= $newhist(instbanks)} {incr i} {
	if {$col > 8} {
	    set col -1
	    incr row
	}
  if $guimode {
  grid [radiobutton $np.set.$i -text $i \
		-command "PostDummyOpts $np; ValidateDummyHist $np" \
		-variable newhist(setnum) -value $i] \
		-column [incr col] -row $row -sticky w
  }
	if {$newhist(instbanks) == 1} {set newhist(setnum) $i}
    }
    if {$newhist(dummy)} {PostDummyOpts $np; ValidateDummyHist $np}
    if $guimode {
    LabelInstParm
    SetMultipleAdd $np
    }
	return 0
}

proc addhist {"np {}"} {
    global expgui newhist tcl_platform expmap
    if {$np != ""} {
      set guimode 1
    } else {
      set guimode 0
      set np .tmphistwin
      if ![winfo exists $np] {label $np}
    }
if {$newhist(dummy)} {
	AddDummyHist $np
	return 0
    }
    # validate the input
    set err {}
    if {[string trim $newhist(rawfile)] == ""} {
	append err "  No data file specified\n"
    }
    if {[string trim $newhist(instfile)] == ""} {
	append err "  No instrument parameter file specified\n"
    }
    if {[string trim $newhist(banknum)] == ""} {
	    append err "  Bank number must be specified\n"
    } elseif {[catch {expr $newhist(banknum)}]} {
	    append err "  Bank number is not valid\n"
    }
    if {[string trim $newhist(setnum)] == ""} {
	append err "  Parameter set number must be specified\n"
    } elseif {[catch {expr $newhist(setnum)}]} {
	append err "  Parameter set number is not valid\n"
    }
    if {[string trim $newhist(2tLimit)] == ""} {
	append err "  2Theta/d-space limit must be specified\n"
    } elseif {[catch {expr $newhist(2tLimit)}]} {
	append err "  The 2Theta/d-space limit is not valid\n"
    } elseif {$newhist(2tLimit) <= 0} {
	append err "  The 2Theta/d-space limit is not valid\n"
    }
    if {[string trim $newhist(LimitMode)] == ""} {
	append err "  Please choose between either a 2Theta, Q or d-space limit\n"
    }

    if {$err != ""} {
	MyMessageBox -parent $np -title  "Add Histogram Error" \
		-message "The following error(s) were found in your input:\n$err" \
		-icon error -type ok -default ok \
		-helplink "expgui3.html AddHistErr"
	return 1
    }

    # ok do it!
    set fp [open exptool.in w]
    puts $fp "H"
    if {$tcl_platform(platform) == "windows"} {
	puts $fp [file attributes $newhist(rawfile) -shortname]
	puts $fp [file attributes $newhist(instfile) -shortname]
    } else {
	puts $fp $newhist(rawfile)
	puts $fp $newhist(instfile)
    }
    puts $fp $newhist(banknum)
    puts $fp $newhist(setnum)
    if {$newhist(LimitMode) == 1} {
	puts $fp "T"
	puts $fp "$newhist(2tLimit)"
    } elseif {$newhist(LimitMode) == 2} {
	puts $fp "D"
	set Q 100
	catch {set Q [expr {4*acos(0)/$newhist(2tLimit)}]}
	puts $fp "$Q"
    } else {
	puts $fp "D"
	puts $fp "$newhist(2tLimit)"
    }
    puts $fp "/"
    puts $fp "X"
    puts $fp "X"
    close $fp
    global tcl_platform
    # Save the current exp file
    savearchiveexp
    # disable the file changed monitor
    set expgui(expModifiedLast) 0
    set expnam [file root [file tail $expgui(expfile)]]
    catch {
	if {$tcl_platform(platform) == "windows"} {
	    exec [file join $expgui(gsasexe) exptool.exe] $expnam \
		    < exptool.in >& exptool.out
	} else {
	    exec [file join $expgui(gsasexe) exptool] $expnam \
		    < exptool.in >& exptool.out
	}
    } errmsg
    # load the revised exp file
    set oldpowderlist $expmap(powderlist)
    loadexp $expgui(expfile)
    set fp [open exptool.out r]
    set out [read $fp]
    close $fp
    destroy $np
    set err 0
    if {[llength $oldpowderlist] == [llength $expmap(powderlist)]} {set err 1}
    if {$errmsg != ""} {
	append errmsg "\n" $out
	set err 1
    } else {
	set errmsg $out
    }
    if {$expgui(showexptool) || $err} {
	set msg "Please review the result from adding the histogram" 
	if {$err} {append msg "\nIt appears an error occurred!"}
	ShowBigMessage $np $msg $errmsg OK "" $err
    }
    file delete exptool.in exptool.out
    # set the powpref warning (2 = required)
    set expgui(needpowpref) 2
    set msg "A histogram was added" 
    if {[string first $msg $expgui(needpowpref_why)] == -1} {
	append expgui(needpowpref_why) "\t$msg\n"
    }
    # select the most recently added histogram
    if {$guimode && !$err} {
	set i [llength $expmap(histlistboxcontents)]
	if {$i > 0} {
	    incr i -1
	    set expgui(curhist) $i
	    sethistlist
	}
    }
return 0
}

proc RunRawplot {parent} {
    global newhist tcl_platform
    set f1 $newhist(rawfile)
    set f2 $newhist(instfile)
    # for Windows put a message on top, in case file names must be shortened
    if {$tcl_platform(platform) == "windows"} {
	catch {set f1 [file nativename \
		    [file attributes $newhist(rawfile) -shortname]]}
	catch {set f2 [file nativename \
		[file attributes $newhist(instfile) -shortname]]}
    }
    if {$f1 != "" || $f2 != ""} {
	#set msg "Note: input to RAWPLOT\n"
	#if {$f1 != ""} {append msg "data file: $f1\n"}
	#if {$f2 != ""} {append msg "instrument file: $f2"}
	catch {toplevel $parent.msg}
	catch {eval destroy [winfo children $parent.msg]}
	wm title $parent.msg "File names"
	grid [label $parent.msg.1 \
		-text "File names to be input to RAWPLOT" \
		-justify center -anchor center] \
		-column 0 -row 0 -columnspan 2
	if {$f1 != ""} {
	    grid [label $parent.msg.2a \
		    -text "Raw histogram: $f1" \
		    -justify center -anchor e] \
		    -column 0 -row 1
	    grid [button $parent.msg.2b \
		    -command "clipboard clear; clipboard append $f1" \
		    -text "put name\nin clipboard"] \
		    -column 1 -row 1
	}	    
	if {$f2 != ""} {
	    grid [label $parent.msg.3a \
		    -text "Raw histogram: $f2" \
		    -justify center -anchor e] \
		    -column 0 -row 2
	    grid [button $parent.msg.3b \
		    -command "clipboard clear; clipboard append $f2" \
		    -text "put name\nin clipboard"] \
		    -column 1 -row 2
	}	    
	grid [button $parent.msg.4 \
		-command "destroy $parent.msg" \
		-text "Close"] \
		-column 0 -columnspan 2 -row 9
    }
    # start RAWPLOT
    runGSASprog rawplot 1
    if {[winfo exists $parent.msg]} {raise $parent.msg}
    update
}
#--- Dummy histogram stuff
proc PostDummyOpts {np} {
    global newhist
    if {$newhist(dummy)} {
	trace variable newhist(tmin) w "ValidateDummyHist $np"
	trace variable newhist(tmax) w "ValidateDummyHist $np"
	trace variable newhist(tstep) w "ValidateDummyHist $np"
	foreach w {l1 t1 lbank} {
	    $np.$w config -fg grey
	}
	$np.d1.m1 config -text {}
	$np.d1.m2 config -text {}
	$np.b1 config -state disabled
	grid forget $np.l3 $np.e3 $np.cb3 $np.cb4 $np.cb5 $np.bank $np.f6a
	grid $np.dl1 -column 0 -row 18
	grid $np.d1 -column 1 -row 18 -rowspan 2 -columnspan 4 -sticky e
	grid $np.dl3 -column 0 -columnspan 99 -row 20 -sticky ew
	if {$newhist(insttype) == "TOF"} {
	    $np.dl1 config -text "Data range:\n(TOF)"
	    $np.d1.lu config -text millisec
	    grid $np.dl2 -column 0 -row 19
	    catch {
		set s $newhist(setnum)
		foreach {x tmin tmax x} $newhist(inst${s}ITYP) {}
		$np.d1.m1 config -text $tmin
		$np.d1.m2 config -text $tmax
	    }
	} elseif {[lindex $newhist(insttype) 0] == "CW"} {
	    $np.dl1 config -text "Data range:\n(2Theta)"
	    $np.d1.lu config -text degrees
	    #grid forget $np.dl2
	    $np.d1.m1 config -text >0.
	    $np.d1.m2 config -text <180.
	} elseif {$newhist(insttype) == "ED"} {
	    $np.dl1 config -text "Data range:\n(Energy)"
	    $np.d1.lu config -text KeV
	    $np.d1.m1 config -text 1.
	    $np.d1.m2 config -text 200.
	    grid $np.dl2 -column 0 -row 19
	} else {
	    $np.dl1 config -text "No file\nselected"
	    $np.d1.lu config -text {}
	}
    } else {
	foreach var {tmin tmax tstep} {
	    foreach v [ trace vinfo newhist($var)] {
		eval trace vdelete newhist($var) $v
	    }
	}
	grid forget $np.dl1 $np.d1 $np.dl2 $np.dl3
	foreach w {l1 t1 lbank} {
	    $np.$w config -fg black
	}
	$np.b1 config -state normal
	grid $np.bank -column 2 -row 3 -columnspan 7 -sticky ew
	grid $np.f6a -column 4 -row 18 -rowspan 3
	grid $np.l3 -column 0 -row 18 -rowspan 3
	grid $np.e3 -column 1 -row 18 -rowspan 3 
	grid $np.cb3 -column 2 -row 18 -sticky w
	grid $np.cb4 -column 2 -row 20 -sticky w
	grid $np.cb5 -column 2 -row 19 -sticky w
     }
}

proc ValidateDummyHist {np args} {
    # validate input
    global newhist
    set msg {}
    $np.dl3 config -text "\n"
    foreach e {e1 e2 e3} v {tmin tmax tstep} {
	if [catch {expr $newhist($v)}] {
	    $np.d1.$e config -fg red
	    append msg "Value of $newhist($v) is invalid for $v\n"
	} else {
	    $np.d1.$e config -fg black
	}
    }
    if {[catch {expr $newhist(setnum)}]} {
	append msg "An instrument file bank number must be selected\n"
    } elseif {$newhist(setnum) <= 0 || \
	    $newhist(setnum) > $newhist(instbanks)} {
	append msg "An invalid instrument file bank has been selected\n"
    }

    if {$msg != ""} {return $msg}

    if {$newhist(tmax) <= $newhist(tmin)} {
	$np.d1.e1 config -fg red
	$np.d1.e2 config -fg red
	return "Tmax <= Tmin\n"
    }


    set dmin -1
    set dmax -1
    if {$newhist(insttype) == "TOF"} {
	catch {
	    set s $newhist(setnum)
	    foreach {x tmin tmax x} $newhist(inst${s}ITYP) {}
	    if {$newhist(tmin) <$tmin } {
		$np.d1.e1 config -fg red
		append msg "Min value of $newhist(tmin) msec is invalid.\n"
	    }
	    if {$newhist(tmax) >$tmax } {
		$np.d1.e2 config -fg red
		append msg "Max value of $newhist(tmax) msec is invalid.\n"
	    }
	    set s $newhist(setnum)
	    set dmin [expr {1000. * $newhist(tmin) / \
		    [lindex $newhist(inst${s}ICONS) 0]}]
	    set dmax [expr {1000. * $newhist(tmax) / \
		    [lindex $newhist(inst${s}ICONS) 0]}]
	}
    } elseif {[lindex $newhist(insttype) 0] == "CW"} {
	if {$newhist(tmin) <= 0 } {
	    $np.d1.e1 config -fg red
	    append msg "Min value of $newhist(tmin) degrees is invalid.\n"
	}
	if {$newhist(tmax) >=180 } {
	    $np.d1.e2 config -fg red
	    append msg "Max value of $newhist(tmax) degrees is invalid.\n"
	}
	catch {
	    set s $newhist(setnum)
	    set dmin [expr {[lindex $newhist(inst${s}ICONS) 0]\
		    * 0.5 / sin(acos(0.)*$newhist(tmax)/180.)}]
	    set dmax [expr {[lindex $newhist(inst${s}ICONS) 0]\
		    * 0.5 / sin(acos(0.)*$newhist(tmin)/180.)}]
	}
    } else {
	if {$newhist(tmin) <1 } {
	    $np.d1.e1 config -fg red
	    append msg "Min value of $newhist(tmin) KeV is invalid.\n"
	}
	if {$newhist(tmax) >200 } {
	    $np.d1.e2 config -fg red
	    append msg "Max value of $newhist(tmax) KeV is invalid.\n"
	}
	catch {
	    set s $newhist(setnum)
	    set ang [lindex $newhist(inst${s}ICONS) 0]
	    set dmin [expr {12.398/ (2.0*sin($ang*acos(0.)/180) * \
		    $newhist(tmax))}]
	    set dmax [expr {12.398/ (2.0*sin($ang*acos(0.)/180) * \
		    $newhist(tmin))}]
	}
    }
    if {$msg != ""} {return $msg}
    set pnts -1
    catch {
	set pnts [expr {1+int(($newhist(tmax) - $newhist(tmin))/$newhist(tstep))}]
	set qmin [expr {4.*acos(0)/$dmax}]
	set qmax [expr {4.*acos(0)/$dmin}]
    }
    if {$pnts <= 0} {
	$np.d1.e3 config -fg red
	append msg "Step value of $newhist(tstep) is invalid.\n"
    }
    if {$pnts >20000} {
	$np.d1.e3 config -fg red
	append msg "Step value of $newhist(tstep) is too small (>20000 points).\n"
    }
    if {$msg != ""} {return $msg}
    if {$dmin > 0 && $dmax > 0} {
	catch {
	    set msg [format \
		    {  %d points.%s  D-space range: %.2f-%.2f A,  Q: %.2f-%.2f/A} \
		    $pnts "\n" $dmin $dmax $qmin $qmax]
	    $np.dl3 config -text $msg
	}
    }
    if {$msg != ""} {return ""}
    $np.dl3 config -text [format {  %d points.%s  Range: ?} $pnts "\n"]
    return "Invalid data range -- something is wrong!"
}

proc AddDummyHist {np} {
    global newhist expgui expmap
    global tcl_platform
    set msg [ValidateDummyHist $np]
    if {$msg != ""} {
	MyMessageBox -parent $np -title  "Add Histogram Error" \
		-message "The following error(s) were found in your input:\n$msg" \
		-icon error -type ok -default ok \
		-helplink "expgui3.html AddHistErr"
	return
    }
    set fp [open exptool.in w]
    puts $fp "D"
    puts $fp $newhist(instfile)
    puts $fp $newhist(setnum)
    if {$newhist(insttype) == "TOF"} {
	puts $fp "C"
    }
    puts $fp $newhist(tmin)
    puts $fp $newhist(tmax)
    puts $fp $newhist(tstep)
    puts $fp "X"
    puts $fp "0"
    close $fp
    # Save the current exp file
    savearchiveexp
    # disable the file changed monitor
    set expgui(expModifiedLast) 0
    set expnam [file root [file tail $expgui(expfile)]]
    set err [catch {
	if {$tcl_platform(platform) == "windows"} {
	    exec [file join $expgui(gsasexe) exptool.exe] $expnam \
		    < exptool.in >& exptool.out
	} else {
	    exec [file join $expgui(gsasexe) exptool] $expnam \
		    < exptool.in >& exptool.out
	}
    } errmsg ]
    # load the revised exp file
    set oldpowderlist $expmap(powderlist)
    loadexp $expgui(expfile)
    set fp [open exptool.out r]
    set out [read $fp]
    close $fp
    if {[llength $oldpowderlist] == [llength $expmap(powderlist)]} {set err 1}
    if {$errmsg != ""} {
	append errmsg "\n" $out
    } else {
	set errmsg $out
    }
    if {[regexp {\(P,H,A\)} $out]} {
	set msg {You must upgrade the EXPTOOL program.}
	append msg { This version cannot add dummy histograms.}
	MyMessageBox -icon error -title "Old EXPTOOL program" \
		-message $msg -parent $np \
		-helplink "expguierr.html OldEXPTOOL"
	# update the documentation & link
	destroy $np
    } elseif {$expgui(showexptool) || $err} {
	set msg "Please review the result from adding the dummy histogram" 
	if {$err} {append msg "\nIt appears an error occurred!"}
	ShowBigMessage $np $msg $errmsg OK "" $err
    } else {
	destroy $np
    }
    file delete exptool.in exptool.out
    # set the powpref warning (2 = required)
    set expgui(needpowpref) 2
    set msg "A histogram was added" 
    if {[string first $msg $expgui(needpowpref_why)] == -1} {
	append expgui(needpowpref_why) "\t$msg\n"
    }
}

#--- multiple histogram stuff
proc SetMultipleAdd {np} {
    global newhist
    $np.f6.b6c configure -state disabled
    catch {
	if {$newhist(instbanks) == [llength $newhist(banklist)] \
		&& $newhist(instbanks) > 1} {
	    $np.f6.b6c configure -state normal
	}
    }
}

proc addMultiplehist {np} {
    global newhist
    # should not happen, but just in case
    if {$newhist(instbanks) != [llength $newhist(banklist)]} {
	$np.f6.b6c configure -state disable
	return
    }
    catch {destroy [set top $np.addMult]}
    toplevel $top
    grid [canvas $top.canvas \
	    -scrollregion {0 0 5000 500} -width 0 -height 250 \
	    -yscrollcommand "$top.scroll set"] \
	    -column 0 -columnspan 2 -row 2 -sticky ns
    grid columnconfigure $top 0 -weight 1
    grid rowconfigure $top 2 -weight 1
    scrollbar $top.scroll \
	    -command "$top.canvas yview"
    frame [set cfr $top.canvas.fr]
    $top.canvas create window 0 0 -anchor nw -window $cfr
    grid [label $top.top -text "Select banks to add" -bg beige] \
	    -column 0 -columnspan 3 -row 0 -sticky ew
    grid [frame $top.vartyp -bd 2 -relief groove] \
	    -column 0 -columnspan 3 -row 1 -sticky ew
    grid [label $top.vartyp.top -text "Data limit units:"] -column 0 -row 0 -columnspan 3 -sticky w
    grid [radiobutton $top.vartyp.cb3 -text "d-min" -variable newhist(LimitMode) \
	    -value 0] -column 0 -row 1 -sticky w
    grid [radiobutton $top.vartyp.cb4 -textvariable newhist(datalimlbl)  \
	    -variable newhist(LimitMode) -anchor w -justify l \
	    -value 1] -column 1 -row 1 -sticky w
    grid [radiobutton $top.vartyp.cb5 -text "Q-max" -variable newhist(LimitMode) \
	    -value 2] -column 2 -row 1 -sticky w
    set newhist(LimitMode) 1
    
    grid [button $top.add -text Add -command "destroy $np"] -column 0 -row 3
    grid [button $top.cancel -text Cancel -command "destroy $top"] \
	    -column 1 -row 3 -columnspan 2
    set row 1
    grid [label $cfr.t1 -text "Bank\n#"] -column 0 -row 0
    switch $newhist(insttype) {
	TOF {set newhist(datalimlbl) "T-min\n(ms)"}
	ED  {set newhist(datalimlbl) "E-max\n(KeV)"}
	default {set newhist(datalimlbl) "2theta\nmax"}
    }
    grid [label $cfr.t2 -textvariable newhist(datalimlbl)] -column 1 -row 0
    foreach i $newhist(banklist) {
	grid [checkbutton $cfr.c$i -text $i \
		-variable newhist(usebank$i)] \
		-column 0 -row [incr row] -sticky w
	set newhist(usebank$i) 1
	grid [entry $cfr.e$i -width 8 -textvariable newhist(tlimit$i)] \
	    -column 1 -row $row -sticky w
	lappend newhist(LimitMode_boxes) $cfr.e$i
	if {$newhist(insttype) == "TOF"} {
	    set newhist(tlimit$i) $newhist(tmin$i)
	} else {
	    set newhist(tlimit$i) $newhist(tmax$i)
	}
    }
    # resize the list
    update
    set sizes [grid bbox $top.canvas.fr]
    $top.canvas config -scrollregion $sizes -width [lindex $sizes 2]
    # use the scroll for BIG lists
    if {[lindex $sizes 3] > [winfo height $top.canvas]} {
	grid $top.scroll -sticky ns -column 3 -row 2
    } else {
	grid forget $top.scroll 
    }
    update
    putontop $top
    tkwait window $top
    afterputontop

    if {[winfo exists $np]} return

    # validate the input
    set err {}
    if {[string trim $newhist(rawfile)] == ""} {
	append err "  No data file specified\n"
    }
    if {[string trim $newhist(instfile)] == ""} {
	append err "  No instrument parameter file specified\n"
    }
    foreach i $newhist(banklist) {
	if {$newhist(usebank$i)} {
	    if {[catch {expr $newhist(tlimit$i)}]} {
		append err "  The Max/Min limit is not valid for bank $i\n"
	    } elseif {$newhist(tlimit$i) <= 0} {
		append err "  The Max/Min limit is not valid for bank $i\n"
	    }
	}
    }
    if {$err != ""} {
	MyMessageBox -parent $np -title  "Add Histogram Error" \
		-message "The following error(s) were found in your input:\n$err" \
		-icon error -type ok -default ok \
		-helplink "expgui3.html AddHistErr"
	return
    }

    # ok do it!
    global tcl_platform expmap expgui
    # Save the current exp file
    savearchiveexp
    set oldpowderlist $expmap(powderlist)
    # disable the file changed monitor
    set expgui(expModifiedLast) 0
    set expnam [file root [file tail $expgui(expfile)]]
    if {$tcl_platform(platform) == "windows"} {
	set rfile [file attributes $newhist(rawfile) -shortname]
	set ifile [file attributes $newhist(instfile) -shortname]
	set exe [file join $expgui(gsasexe) exptool.exe]
    } else {
	set rfile $newhist(rawfile)
	set ifile $newhist(instfile)
	set exe [file join $expgui(gsasexe) exptool]
    }
    set k 0
    set added 0
    set outlog {}
    set err 0
    pleasewait "adding histograms" expgui(temp)
    foreach i $newhist(banklist) {
	incr k
	if {$newhist(usebank$i)} {
	    incr added
	    set expgui(temp) "adding bank $i"
	    update
	    set fp [open exptool.in w]
	    puts $fp "H"
	    puts $fp $rfile
	    puts $fp $ifile
	    puts $fp $i
	    puts $fp $k
	    if {$newhist(LimitMode) == 1} {
		puts $fp "T"
		puts $fp "$newhist(tlimit$i)"
	    } elseif {$newhist(LimitMode) == 2} {
		puts $fp "D"
		set Q 100
		catch {set Q [expr {4*acos(0)/$newhist(tlimit$i)}]}
		puts $fp "$Q"
	    } else {
		puts $fp "D"
		puts $fp "$newhist(tlimit$i)"
	    }
	    puts $fp "/"
	    puts $fp "X"
	    puts $fp "X"
	    close $fp
	    catch {
		exec $exe $expnam < exptool.in >& exptool.out
	    } errmsg
	    set fp [open exptool.out r]
	    set out [read $fp]
	    close $fp
	    if {$errmsg != ""} {
		append outlog "\n\n\nNOTE ERROR:\n" $errmsg $out
		set err 1
	    } else {
		append outlog $out
	    }
	}
    }
    # load the revised exp file
    loadexp $expgui(expfile)
    if {[llength $oldpowderlist]+$added != [llength $expmap(powderlist)]} {
	set err 1
    }
    # set the powpref warning (2 = required)
    set expgui(needpowpref) 2
    set msg "A histogram was added" 
    if {[string first $msg $expgui(needpowpref_why)] == -1} {
	append expgui(needpowpref_why) "\t$msg\n"
    }
    file delete exptool.in exptool.out
    donewait 
    if {$expgui(showexptool) || $err} {
	set msg "Please review the result from adding the histogram" 
	if {$err} {append msg "\nIt appears an error occurred!"}
	ShowBigMessage $np $msg $outlog OK "" $err
    }
    # select the most recently added histogram
    if {!$err} {
	set i [llength $expmap(histlistboxcontents)]
	if {$i > 0} {
	    incr i -1
	    set expgui(curhist) $i
	    sethistlist
	}
    }
}

#----------- Add Atoms routines ----------------------------------------
proc MakeAddAtomsBox {phase "atomlist {}"} {
    global expmap expgui

    # is there room for more atoms? Well, we will check this someday
    if {$phase == ""} return
    if {[llength $phase] != 1} return

    set top .newatoms
    catch {destroy $top}
    toplevel $top
    bind $top <Key-F1> "MakeWWWHelp expgui2.html addatoms"

    grid [label $top.l1 -relief groove -bd 4 -anchor center\
	    -text "Adding atoms to phase #$phase"] \
	    -column 0 -row 0 \
	    -sticky we -columnspan 10
    
    grid [canvas $top.canvas \
	    -scrollregion {0 0 5000 500} -width 0 -height 250 \
	    -yscrollcommand "$top.scroll set"] \
	    -column 0 -row 2 -columnspan 4 -sticky nsew
    grid columnconfigure $top 3 -weight 1
    grid rowconfigure $top 2 -weight 1
    grid rowconfigure $top 1 -pad 5
    scrollbar $top.scroll \
	    -command "$top.canvas yview"
    frame $top.canvas.fr
    $top.canvas create window 0 0 -anchor nw -window $top.canvas.fr

    set np $top.canvas.fr
    set row 0
    set col 0
    grid [label $np.l_${row}0 -text "  #  "] -column $col -row $row
    foreach i {Atom\ntype Name x y z Occ Uiso} \
	    var {type name x y z occ uiso} {
	grid [button $np.l_${row}$i -text $i -padx 0 -pady 0 \
		-command "sortAddAtoms $phase $top $var"] \
		-column [incr col] -row $row -sticky nsew
    }
    grid [label $np.l_${row}Use -text Use\nFlag] -column [incr col] -row $row

    set expgui(SetAddAtomsScroll) 0
    set i [llength $atomlist]
    if {$i == 0} {incr i}
    for {set j 0} {$j < $i} {incr j} {
	MakeAddAtomsRow $top
    }
    set row 0
    foreach item $atomlist {
	incr row
	foreach val $item w {n x y z t o u} {
	    if {$val != ""} {
		$np.e${row}$w delete 0 end
		$np.e${row}$w insert end $val
	    }
	}
    }
    bind $top <Configure> "SetAddAtomsScroll $top"
    grid rowconfigure $top 3 -min 10
    grid [button $top.b1 -text "Add Atoms"\
	    -command "addatom $phase $top"] -column 0 -row 5 -sticky w
    bind $top <Return> "addatom $phase $top"
    grid [button $top.b2 -text Cancel \
	    -command "destroy $top"] -column 1 -row 5 -sticky w
    grid [button $top.help -text Help -bg yellow \
	    -command "MakeWWWHelp expgui2.html addatoms"] \
	    -column 0 -columnspan 2 -row 4

    # get the input formats if not already defined
    GetImportFormats
    if {[llength $expgui(importFormatList)] > 0} {
	grid [frame $top.fr -bd 4 -relief groove] \
		-column 3 -row 5 -columnspan 2 -sticky e
	grid [button $top.fr.b3 -text "Import atoms from: " \
		-command "ImportAtoms \$expgui(importFormat) $top $phase"] \
		-column 0 -row 0 -sticky e
	set menu [eval tk_optionMenu $top.fr.b4 expgui(importFormat) \
		$expgui(importFormatList)]
	for {set i 0} {$i <= [$menu index end]} {incr i} {
	    $menu entryconfig $i -command "ImportAtoms \$expgui(importFormat) $top $phase"
	}
	grid $top.fr.b4 -column 1 -row 0 -sticky w
	grid rowconfig $top.fr 0 -pad 10
	grid columnconfig $top.fr 0 -pad 10
	grid columnconfig $top.fr 1 -pad 10
    }

    grid [button $top.b3 -text  "More atom boxes" \
	    -command "MakeAddAtomsRow $top"] -column 3 \
	    -columnspan 2 -row 4 -sticky e
    
    wm title $top "add new atom"

    # set grab, etc.
    putontop $top

    tkwait window $top

    # fix grab...
    afterputontop
}

proc MakeAddAtomsRow {top} {
    set np $top.canvas.fr
    set col -1
    set row 1
    # find an empty row
    while {![catch {grid info $np.e${row}t}]} {incr row}
    grid [label $np.e${row}num -text $row] -column [incr col]  -row $row
    grid [entry $np.e${row}t -width 5] -column [incr col]  -row $row
    grid [entry $np.e${row}n -width 8] -column [incr col]  -row $row
    foreach i {x y z o u} {
	grid [entry $np.e${row}$i -width 9] -column [incr col] -row $row
    }
    grid [checkbutton $np.e${row}use -variable expgui(UseAtom$row)] \
	    -column [incr col] -row $row
    # default occupancy
    $np.e${row}o delete 0 end
    $np.e${row}o insert end 1.0
    # default Uiso
    $np.e${row}u delete 0 end
    $np.e${row}u insert end 0.025
    # default label
    $np.e${row}n delete 0 end
    $np.e${row}n insert end (default)
    # use by default
    $np.e${row}use select

    SetAddAtomsScroll $top
    return $row
}

proc SetAddAtomsScroll {top} {
    global expgui
    if $expgui(SetAddAtomsScroll) return
    # prevent reentrance
    set expgui(SetAddAtomsScroll) 1
    update
    set sizes [grid bbox $top.canvas.fr]
    $top.canvas config -scrollregion $sizes -width [lindex $sizes 2]
    # use the scroll for BIG atom lists
    if {[lindex $sizes 3] > [winfo height $top.canvas]} {
	grid $top.scroll -sticky ns -column 4 -row 2
    } else {
	grid forget $top.scroll 
    }
    update
    set expgui(SetAddAtomsScroll) 0
}

# Validate the atoms in the atoms add/phase replace box 
# returns a null string on error or a list of atoms
proc ValidateAtomsBox {top np} {
    global expgui
    set row 0
    # loop over the defined rows
    set err {}
    set atomlist {}
    set validatmtypes {
	H H-1 H_1 H_2 D H_3 HE HE_3 HE_4 LI LI+1 LI_6 LI_7 BE BE+2 B B_10
	B_11 C CV C_12 C_13 N N_14 N_15 O O-1 O-2 O_16 O_17 O_18 F F-1 F_19 NE
	NE_20 NE_21 NE_22 NA NA+1 NA_23 MG MG+2 MG_24 MG_25 MG_26 AL AL+3
	AL_27 SI SI+4 SIV SI_28 SI_29 SI_30 P P_31 S S_32 S_33 S_34 CL CL-1
	CL_35 CL_37 AR AR_36 AR_40 K K+1 K_39 K_41 CA CA+2 CA_40 CA_44 SC SC+3
	SC_45 TI TI+2 TI+3 TI+4 TI_46 TI_47 TI_48 TI_49 TI_50 V V+2 V+3 V+5
	V_51 CR CR+2 CR+3 CR_50 CR_52 CR_53 CR_54 MN MN+2 MN+3 MN+4 MN_55 FE
	FE+2 FE+3 FE_54 FE_56 FE_57 FE_58 CO CO+2 CO+3 CO_59 NI NI+2 NI+3
	NI_58 NI_60 NI_61 NI_62 NI_64 CU CU+1 CU+2 CU_63 CU_65 ZN ZN+2 ZN_64
	ZN_66 ZN_67 ZN_68 GA GA+3 GE GE+4 AS AS_75 SE BR BR-1 BR_79 BR_81 KR
	RB RB+1 SR SR+2 Y Y+3 Y_89 ZR ZR+4 NB NB+3 NB+5 NB_93 MO MO+3 MO+5
	MO+6 TC TC_98 RU RU+3 RU+4 RH RH+3 RH+4 RH_103 PD PD+2 PD+4 AG AG+1
	AG+2 CD CD+2 CD_112 CD_113 CD_114 CD_116 IN IN+3 IN_113 IN_115 SN SN+2
	SN+4 SB SB+3 SB+5 TE I I-1 I_127 XE CS CS+1 CS_133 BA BA+2 LA LA+3 CE
	CE+3 CE+4 PR PR+3 PR+4 PR_141 ND ND+3 PM PM+3 PM_147 SM SM+3 SM_152
	SM_154 EU EU+2 EU+3 EU_153 GD GD+3 GD_160 TB TB+3 TB_159 DY DY+3 HO
	HO+3 HO_165 ER ER+3 TM TM+3 TM_169 YB YB+2 YB+3 LU LU+3 HF HF+4 TA
	TA+5 TA_181 W W+6 RE OS OS+4 IR IR+3 IR+4 PT PT+2 PT+4 AU AU+1 AU+3
	AU_197 HG HG+1 HG+2 TL TL+1 TL+3 PB PB+2 PB+4 BI BI+3 BI+5 BI_209 PO
	PO_210 AT AT_210 RN RN_222 FR FR_223 RA RA+2 RA_226 AC AC+3 AC_227 TH
	TH+4 TH_232 PA PA_231 U U+3 U+4 U+6 U_235 U_238 NP NP+3 NP+4 NP+6
	NP_237 PU PU+3 PU+4 PU+6 PU_239 PU_240 PU_242 AM AM_243 CM CM_244 BK
	BK_247 CF CF_249
    }
    # loop over the defined rows
    while {![catch {grid info $np.e[incr row]t}]} {
	if !{$expgui(UseAtom$row)} continue
	# ignore blank entries
	set line {}
	foreach i {t x y z} {
	    append line [string trim [$np.e${row}$i get]]
	}
	if {$line == ""} continue

	# validate the input
	if {[set type [string trim [$np.e${row}t get]]] == ""} {
	    append err "  line $row: No atom type specified\n"
	}
	if {[lsearch $validatmtypes [string toupper $type]] == -1} {
	    append err "  line $row: Atom type $type is invalid for GSAS\n"
	}
	set name [string trim [$np.e${row}n get]]
	if {$name == "(default)"} {set name "/"}
	if {$name == ""} {set name "/"}
	foreach i {x y z o u} n {x y z Occ Uiso} {
	    if {[set $i [string trim [$np.e${row}$i get]]] == ""} {
		append err "  line $row: No value specified for $n\n"
	    } elseif {[catch {expr [set $i]}]} {
		append err "  line $row: The value for $n is invalid\n"
	    }
	}
	lappend atomlist "$type $x $y $z $o $name I $u"
    }
    if {$err != ""} {
	MyMessageBox -icon warning -message "Note Errors:\n$err" -parent $top
	return {}
    }
    if {[llength $atomlist] == 0} {
	MyMessageBox -icon warning -message "No atoms to load!" -parent $top
	return {}
    }
    return $atomlist
}

proc addatom {phase top "atomlist {}"} {
    global expgui env expmap
    if {$top != ""} {
      set guimode 1
      set np $top.canvas.fr
      set atomlist [ValidateAtomsBox $top $np]
    } else {
      set guimode 0
      set np .tmpatomtwin
      if ![winfo exists $np] {label $np}
    }

   	# validate the atoms info
    if {$atomlist == ""} {
      return 0
    }
    # ok add the atoms!
    set fp [open exptool.in w]
    puts $fp "A"
    puts $fp $phase
    # number of atoms
    puts $fp [llength $atomlist]
    foreach atomline $atomlist {
	puts $fp $atomline
    }
    close $fp
    # needed in UNIX
    set env(ATOMDATA) [file join $expgui(gsasdir) data atmdata.dat]
    set env(gsas) [file nativename $expgui(gsasdir)]
    # needed in Windows
    set env(GSAS) [file nativename $expgui(gsasdir)]

    global tcl_platform
    # Save the current exp file
    savearchiveexp
    # disable the file changed monitor
    set expgui(expModifiedLast) 0
    set expnam [file root [file tail $expgui(expfile)]]
    catch {
	if {$tcl_platform(platform) == "windows"} {
	    exec [file join $expgui(gsasexe) exptool.exe] $expnam \
		    < exptool.in >& exptool.out
	} else {
	    exec [file join $expgui(gsasexe) exptool] $expnam \
		    < exptool.in >& exptool.out
	}
    } errmsg
    # load the revised exp file
    set oldatomlist $expmap(atomlist_$phase)
    loadexp $expgui(expfile)
    set fp [open exptool.out r]
    set out [read $fp]
    close $fp
    destroy $top
    set err 0
    if {[llength $oldatomlist] == [llength $expmap(atomlist_$phase))]} {
	set err 1
    }
    if {$errmsg != ""} {
	append errmsg "\n" $out
	set err 1
    } else {
	set errmsg $out
    }
    if {$expgui(showexptool) || $err} {
	set msg "Please review the result from adding the atom(s)" 
	if {$err} {append msg "\nIt appears an error occurred!"}
	ShowBigMessage $top $msg $errmsg OK "" $err
    }
    file delete exptool.in exptool.out
    return 0
}

#---------------------------------------------------------------------------
# commands to modify a group of selected atoms 
#---------------------------------------------------------------------------

# make the dialog to choose an action
proc MakeXformAtomsBox {phase} {
    global expgui expmap
    set numberList {}
    set p $expgui(curPhase)
    foreach AtomIndex $expgui(selectedatomlist) {
	# get atom number & phase
	set tuple [lindex $expmap(atomlistboxcontents) $AtomIndex]
	lappend numberList [lindex $tuple 0]
    }
    if {$numberList == ""} return
    if {[llength $numberList] > 1} {
	set suffix s
	set suffixy "ies"
    } else {
	set suffix ""
	set suffixy "y"
    }
    set w .global
    catch {destroy $w}
    toplevel $w
    wm title $w "Edit Atomic Parameter -- phase #$phase"
    bind $w <Key-F1> "MakeWWWHelp expgui2.html xform"
    # this needs to track by phase
    grid [label $w.0 \
	    -text "Modifying atom${suffix} [CompressList $numberList] Phase $phase" \
	    -bg yellow -anchor center] -row 0 -column 0 -columnspan 10 \
	    -sticky nsew
    grid rowconfigure $w 0 -pad 5
    grid rowconfigure $w 1 -minsize 2

    grid [TitleFrame $w.1 -bd 6 -relief groove -text "Modify coordinates"] \
	    -row 2 -column 0 -columnspan 10 -sticky news
    set w1 [$w.1 getframe]
    set row 0
    foreach v {x y z} {
	incr row
	set col -1
	grid [label $w1.l$v -text "new $v   =   "] -column [incr col] -row $row
	foreach o {x y z} {
	    grid [entry $w1.e${v}${o} -width 6] -column [incr col] -row $row
	    $w1.e${v}${o} delete 0 end
	    if {$v == $o} {
		$w1.e${v}${o} insert end "1.0"
	    } else {
		$w1.e${v}${o} insert end "0."
	    }
	    grid [label $w1.p${v}${o} -text " $o  +  "] \
		    -column [incr col] -row $row
	}
	grid [entry $w1.e${v} -width 6] -column [incr col] -row $row
	$w1.e${v} delete 0 end
	$w1.e${v} insert end "0."
    }
    grid [button $w1.do -text "Transform Coordinates" \
	    -command "XformAtomsCoord $phase [list $numberList] $w1" \
	    ] -row [incr row] -column 0 -columnspan 10

    set shift [GetOrigin1Shift $phase]
    grid [button $w1.d1 -text "Xform Origin 1 to Origin 2" \
	      -command "XformAtoms2Origin2 $phase [list $numberList] $w1 [list $shift]" \
	     ] -row [incr row] -column 3 -columnspan 10 -sticky e
    if {$shift == ""} {$w1.d1 config -state disabled}

    grid [button $w1.d4 -text "Reset Multiplicities" \
		-command "ResetMultiplicities $phase $w" \
		] -row $row -column 0 -columnspan 3 -sticky w


    grid rowconfigure $w 3 -minsize 5
    grid [TitleFrame $w.4 -bd 6 -relief groove -text "Modify occupanc${suffixy}"] \
	    -row 4 -column 0 -columnspan 10 -sticky news
    set w2 [$w.4 getframe]
    grid [label $w2.1 -text "Occupancy: "] -row 1 -column 0
    grid [entry $w2.e -width 10] -column 1 -row 1
    $w2.e delete 0 end
    $w2.e insert end 1.0
    grid columnconfigure $w2 2 -weight 1
    grid [button $w2.do -text "Set Occupanc${suffixy}" \
	    -command "XformAtomsOcc $phase [list $numberList] $w2" \
	    ] -row 2 -column 0 -columnspan 10

    grid rowconfigure $w 5 -minsize 5
    grid [TitleFrame $w.6 -bd 6 -relief groove \
	    -text "Modify Displacement Parameter$suffix"] \
	    -row 6 -column 0 -columnspan 10 -sticky news
    set w2 [$w.6 getframe]
    grid [entry $w2.e -width 10] -column 1 -row 1
    $w2.e delete 0 end
    $w2.e insert end 0.025
    grid columnconfigure $w2 2 -weight 1
    grid [button $w2.do -text "Set U" \
	    -command "XformAtomsU $phase [list $numberList] $w2" \
	    ] -row 2 -column 0 -columnspan 10
    grid [frame $w2.f] -row 3 -column 0 -columnspan 10

    if {[lindex $expmap(phasetype) [expr {$p - 1}]] != 4} {
	grid [label $w2.1 -text "Uiso or Uequiv: "] -row 1 -column 0
	grid [button $w2.f.iso -text "Set Isotropic" \
		-command "XformAtomsU $phase [list $numberList] iso" \
		] -row 0 -column 0
	grid [button $w2.f.aniso -text "Set Anisotropic" \
		-command "XformAtomsU $phase [list $numberList] aniso" \
		] -row 0 -column 1
    } else {
	grid [label $w2.1 -text "Uiso: "] -row 1 -column 0
    }

    grid rowconfigure $w 7 -minsize 5
    if {[lindex $expmap(phasetype) [expr {$p - 1}]] != 4} {
	grid [TitleFrame $w.8 -bd 6 -relief groove \
		-text "Erase Atom$suffix"] \
		-row 8 -column 0 -columnspan 10 -sticky news
	set w2 [$w.8 getframe]
	grid [button $w2.do -text "Erase Atom${suffix}" \
		-command "EraseAtoms $phase [list $numberList] $w" \
		] -row 2 -column 0 -columnspan 10
    }

    grid rowconfigure $w 11 -minsize 5
    grid [frame $w.b] -row 12 -column 0 -columnspan 10 -sticky ew
    pack [button $w.b.3 -text Close -command "destroy $w"] -side left \
	    -padx 5 -pady 5
    pack [button $w.b.help -text Help -bg yellow \
	    -command "MakeWWWHelp expgui2.html xform"] -side right \
	    -padx 5 -pady 5
    bind $w <Return> "destroy $w"

    # force the window to stay on top
    putontop $w
    focus $w.b.3
    tkwait window $w
    afterputontop
    # if there are selected atoms, reset their display
    if {[llength $expgui(selectedatomlist)] != 0} editRecord
}

# transform the coordinates
proc XformAtomsCoord {phase numberList w1} {
    global expgui expmap
    if {[lindex $expmap(phasetype) [expr {$phase - 1}]] == 4} {
	set cmd mmatominfo
    } else {
	set cmd atominfo
    }
    # get the matrix
    foreach v {x y z} {
	foreach o {x y z} {
	    set matrix(${v}${o}) [$w1.e${v}${o} get]
	}
	set matrix(${v}) [$w1.e${v} get]
    }
    foreach atom $numberList {
	foreach v {x y z} {
	    set $v [$cmd $phase $atom $v]
	}
	foreach v {x y z} {
	    set new$v $matrix(${v})
	    foreach o {x y z} {
		set new$v [expr [set new$v] + $matrix(${v}${o})*[set $o]]
	    }
	    $cmd $phase $atom $v set [set new$v]
	}
	incr expgui(changed)
    }
    # update multiplicities for the phase
    set parent [winfo toplevel $w1]
    ResetMultiplicities $phase $parent
    SelectOnePhase $phase
    MyMessageBox -parent $parent -type OK -default ok -title "Transform applied" \
	-message "The coordinates of atoms [CompressList $numberList] have been transformed"
#    UpdateAtomLine $numberList $phase
    destroy $parent
}

# set the occupancies to a single value
proc XformAtomsOcc {phase numberList w2} {
    global expgui expmap
    if {[lindex $expmap(phasetype) [expr {$phase - 1}]] == 4} {
	set cmd mmatominfo
    } else {
	set cmd atominfo
    }
    # get the value
    set val [$w2.e get]
    foreach atom $numberList {
	$cmd $phase $atom frac set $val
	incr expgui(changed)
    }
    UpdateAtomLine $numberList $phase
}

# transform Uiso or Uij; if anisotropic set Uequiv to Uij
proc XformAtomsU {phase numberList w2} {
    global expgui
    if {$w2 == "iso"} {
	foreach atom $numberList {
	    if {[atominfo $phase $atom temptype] != "I"} {
		atominfo $phase $atom temptype set I
		incr expgui(changed)
	    }
	}
    } elseif {$w2 == "aniso"} {
	foreach atom $numberList {
	    if {[atominfo $phase $atom temptype] == "I"} {
		atominfo $phase $atom temptype set A
		incr expgui(changed)
	    }
	}
    } else {
	# get the value
	set val [$w2.e get]
	foreach atom $numberList {
	    global expmap
	    if {[lindex $expmap(phasetype) [expr {$phase - 1}]] == 4} {
		mmatominfo $phase $atom Uiso set $val
	    } elseif {[atominfo $phase $atom temptype] == "I"} {
		atominfo $phase $atom Uiso set $val
	    } else {
		atominfo $phase $atom U11 set $val
		atominfo $phase $atom U22 set $val
		atominfo $phase $atom U33 set $val
		atominfo $phase $atom U12 set 0.0
		atominfo $phase $atom U13 set 0.0
		atominfo $phase $atom U23 set 0.0
	    }
	    incr expgui(changed)
	}
    }
    UpdateAtomLine $numberList $phase
}

# confirm and erase atoms
proc EraseAtoms {phase numberList w2} {
    global expgui
    if {[llength $numberList] <= 0} return
    # make a list of atoms
    foreach atom $numberList {
	append atomlist "\n\t$atom  [atominfo $phase $atom label]"
    }
    set msg "OK to remove the following [llength $numberList] atoms from phase $phase:$atomlist"
    set val [MyMessageBox -parent $w2 -type okcancel -icon warning \
	    -default cancel -title "Confirm Erase" -message $msg]
    if {$val == "ok"} {
	foreach atom $numberList {
	    EraseAtom $atom $phase
	    incr expgui(changed)
	}
	mapexp
	DisplayAllAtoms $phase
	destroy $w2
    }
}

#----------- more Add Phase routines (import) -------------------------------
proc ImportPhase {format np} {
    global expgui
    foreach item $expgui(extensions_$format) {
	lappend typelist [list $format $item]
    }
    lappend typelist [list "All files" *]
    set file [tk_getOpenFile -parent $np -filetypes $typelist]
    if {![file exists $file]} return
    # read in the file
    set input [$expgui(proc_$format) $file]
    catch {
	$np.bf.b1 config -text "Continue" -command "addphase $np; AddAtomsList"
	bind $np <Return> "addphase $np; AddAtomsList"
    }
    catch {
	$np.t1 delete 0 end
	$np.t1 insert end "from $file"
    }
    $np.t2 delete 0 end
    $np.t2 insert end [lindex $input 0]
    foreach i {.e1a .e1b .e1c .e2a .e2b .e2g} val [lindex $input 1] {
	$np.f$i delete 0 end
	$np.f$i insert end $val
    }
    set expgui(coordList) [lindex $input 2]
    set msg [lindex $input 3]
    if {$msg != ""} {
	catch {destroy $np.msg}
	grid [label $np.msg -text $msg -fg red -anchor center -bd 4 -relief raised] \
		-column 0 -columnspan 99 -row 20 -sticky ew
    }
}

proc ImportAtoms {format top phase} {
    global expgui
    foreach item $expgui(extensions_$format) {
	lappend typelist [list $format $item]
    }
    lappend typelist [list "All files" *]
    set file [tk_getOpenFile -parent $top -filetypes $typelist]
    if {![file exists $file]} return
    # disable during read
    catch {
	foreach b "$top.b1 $top.b2 $top.fr.b3" {
	    $b config -state disabled
	}
    }
    # read in the file
    set input [$expgui(proc_$format) $file]
    # add atoms to table
    foreach item [lindex $input 2] {
	set row [MakeAddAtomsRow $top]
	set np $top.canvas.fr
	foreach val $item w {n x y z t o u} {
	    if {$val != ""} {
		$np.e${row}$w delete 0 end
		$np.e${row}$w insert end $val
	    }
	}
    }
    # sort the atoms by number, so that empty entries are at the bottom
    sortAddAtoms $phase $top number
    # reenable
    catch {
	foreach b "$top.b1 $top.b2 $top.fr.b3" {
	    $b config -state normal
	}
    }
}

proc AddAtomsList {} {
    global expgui expmap
    # skip if we aborted out of addphase
    if {$expgui(oldphaselist) == -1} return
    # find the new phase
    set phase {}
    foreach p $expmap(phaselist) {
	if {[lsearch $expgui(oldphaselist) $p] == -1} {
	    set phase $p
	    break
	}
    }
    if {$phase == ""} return
    MakeAddAtomsBox $phase $expgui(coordList)
}

# get the input formats by sourcing files named import_*.tcl
proc GetImportFormats {} {
    global expgui tcl_platform
    # only needs to be done once
    if [catch {set expgui(importFormatList)}] {
	set filelist [glob -nocomplain [file join $expgui(scriptdir) import_*.tcl]]
	foreach file $filelist {
	    set description ""
	    source $file
	    if {$description != ""} {
		lappend expgui(importFormatList) $description
		if {$tcl_platform(platform) == "unix"} {
		    set extensions "[string tolower $extensions] [string toupper $extensions]"
		}
		set expgui(extensions_$description) $extensions
		set expgui(proc_$description) $procname
	    }
	}
    }
}

proc MakeReplacePhaseBox {} {
    global expmap expgui tcl_platform

    set expgui(coordList) {}
    # ignore the command if no phase is selected
    foreach p {1 2 3 4 5 6 7 8 9} {
	if {[lsearch $expmap(phaselist) $expgui(curPhase)] == -1} {
	    return
	}
    }

    set top .newphase
    catch {destroy $top}
    toplevel $top
    bind $top <Key-F1> "MakeWWWHelp expgui2.html replacephase"

    grid [label $top.l1 -text "Replacing phase #$expgui(curPhase)" \
	    -bg yellow -anchor center] -column 0 -columnspan 8 -row 0 -sticky ew
    grid [label $top.l3a -text "Current Space Group: "] \
	    -column 0 -row 2 -columnspan 2 -sticky e
    grid [label $top.l3b -text [phaseinfo $expgui(curPhase) spacegroup]\
	    -bd 4 -relief groove] \
	    -column 2 -row 2  -sticky ew
    grid [label $top.l4 -text "New Space Group: "] \
	    -column 0 -row 3 -columnspan 2 -sticky e
    grid [entry $top.t2 -width 12] -column 2 -row 3 -sticky w
    grid [radiobutton $top.r1 -text "Reenter current atoms"\
	    -variable expgui(DeleteAllAtoms) -value 0] \
	    -column 1 -row 4 -columnspan 8 -sticky w
    grid [radiobutton $top.r2 -text "Delete current atoms" \
	    -variable expgui(DeleteAllAtoms) -value 1] \
	    -column 1 -row 5 -columnspan 8 -sticky w
    
    grid [frame $top.f -bd 4 -relief groove] \
	    -column 3 -row 2 -columnspan 3 -rowspan 4
    set col -1
    foreach i {a b c} {
	grid [label $top.f.l1$i -text " $i "] -column [incr col] -row 1
	grid [entry $top.f.e1$i -width 12] -column [incr col]  -row 1
	$top.f.e1$i delete 0 end
	$top.f.e1$i insert 0 [phaseinfo $expgui(curPhase) $i]
    }
    set col -1
    foreach i {a b g} var {alpha beta gamma} {
	grid [label $top.f.l2$i -text $i] -column [incr col] -row 2
	set font [$top.f.l2$i cget -font]
	$top.f.l2$i config -font "Symbol [lrange $font 1 end]"
	grid [entry $top.f.e2$i -width 12] -column [incr col]  -row 2
	$top.f.e2$i delete 0 end
	$top.f.e2$i insert 0 [phaseinfo $expgui(curPhase) $var]
    } 

    grid [button $top.b1 -text Continue \
	    -command "replacephase1 $top $expgui(curPhase)"] \
	    -column 0 -row 6 -sticky w
    bind $top <Return> "replacephase1 $top $expgui(curPhase)"
    grid [button $top.b2 -text Cancel \
	    -command "destroy $top"] -column 1 -row 6 -sticky w
    grid [button $top.help -text Help -bg yellow \
	    -command "MakeWWWHelp expgui2.html replacephase"] \
	    -column 2 -row 6

    # get the input formats if not already defined
    GetImportFormats
    if {[llength $expgui(importFormatList)] > 0} {
	grid [frame $top.fr -bd 4 -relief groove] \
		-column 2 -row 6 -columnspan 8 -sticky e
	grid [button $top.fr.b3 -text "Import phase from: " \
		-command "ImportPhase \$expgui(importFormat) $top"] \
		-column 0 -row 0 -sticky e
	set menu [eval tk_optionMenu $top.fr.b4 expgui(importFormat) \
		$expgui(importFormatList)]
	for {set i 0} {$i <= [$menu index end]} {incr i} {
	    $menu entryconfig $i -command "ImportPhase \$expgui(importFormat) $top"
	}
	grid $top.fr.b4 -column 1 -row 0 -sticky w
	grid rowconfig $top.fr 0 -pad 10
	grid columnconfig $top.fr 0 -pad 10
	grid columnconfig $top.fr 1 -pad 10
#	grid columnconfig $top 4 -weight 1
	grid columnconfig $top 2 -weight 1
    }
    
    wm title $top "Replace phase $expgui(curPhase)"

    # set grab, etc.
    putontop $top

    tkwait window $top

    # fix grab...
    afterputontop
}

proc replacephase1 {top phase} {
    # validate cell & space group & save to pass
    global expgui expmap
    set expgui(SetAddAtomsScroll) 0
    # validate the input
    set err {}
    set spg [$top.t2 get]
    if {[string trim $spg] == ""} {
	append err "  Space group cannot be blank\n"
    }
    set cell {}
    foreach i {a b c a b g} lbl {a b c alpha beta gamma} n {1 1 1 2 2 2} {
	set $lbl [$top.f.e${n}$i get]
	if {[string trim [set $lbl]] == ""} {
	    append err "  $lbl cannot be blank\n"
	} elseif {[catch {expr [set $lbl]}]} {
	    append err "  [set $lbl] is not valid for $lbl\n"
	}
	lappend cell [set $lbl]
    }

    if {$err != ""} {
	MyMessageBox -parent $top -title "Replace Phase Error" -icon warning \
		-message "The following error(s) were found in your input:\n$err" 
	return
    }

    # check the space group
    set fp [open spg.in w]
    puts $fp "N"
    puts $fp "N"
    puts $fp $spg
    puts $fp "Q"
    close $fp
    global tcl_platform
    catch {
	if {$tcl_platform(platform) == "windows"} {
	    exec [file join $expgui(gsasexe) spcgroup.exe] < spg.in >& spg.out
	} else {
	    exec [file join $expgui(gsasexe) spcgroup] < spg.in >& spg.out
	}
    }
    set fp [open spg.out r]
    set out [read $fp]
    close $fp
    # attempt to parse out the output (fix up if parse did not work)
    if {[regexp "space group symbol.*>(.*)Enter a new space group symbol" \
	    $out a b ] != 1} {set b $out}
    if {[string first Error $b] != -1} {
	# got an error, show it
	ShowBigMessage \
		 $top.error \
		 "Error processing space group\nReview error message below" \
		 $b OK "" 1
	return
    } else {
	# show the result and confirm
	set opt [ShowBigMessage \
		$top.check \
		"Check the symmetry operators in the output below" \
		$b \
		{Continue Redo} ]
	if {$opt > 1} return
    }
    file delete spg.in spg.out
    # draw coordinates box
    eval destroy [winfo children $top]
    grid [label $top.l1 -relief groove -bd 4 -anchor center\
	    -text "Atom list for phase #$phase"] \
	    -column 0 -row 0 \
	    -sticky we -columnspan 10
    grid [canvas $top.canvas \
	    -scrollregion {0 0 5000 500} -width 0 -height 250 \
	    -yscrollcommand "$top.scroll set"] \
	    -column 0 -row 2 -columnspan 4 -sticky nsew
    grid columnconfigure $top 3 -weight 1
    grid rowconfigure $top 2 -weight 1
    grid rowconfigure $top 1 -pad 5
    scrollbar $top.scroll \
	    -command "$top.canvas yview"
    frame $top.canvas.fr
    $top.canvas create window 0 0 -anchor nw -window $top.canvas.fr

    set np $top.canvas.fr
    set row 0
    set col 0
    grid [label $np.l_${row}0 -text "  #  "] -column $col -row $row
    foreach i {Atom\ntype Name x y z Occ Uiso} \
	    var {type name x y z occ uiso} {
	grid [button $np.l_${row}$i -text $i -padx 0 -pady 0 \
		-command "sortAddAtoms $phase $top $var"] \
		-column [incr col] -row $row -sticky nsew
    }
    grid [label $np.l_${row}Use -text Use\nFlag] -column [incr col] -row $row

    # add the old atoms, if appropriate
    if {!$expgui(DeleteAllAtoms)} {
	# loop over all atoms
	foreach atom $expmap(atomlist_$phase) {
	    set row [MakeAddAtomsRow $top]
	    # add all atoms in the current phase to the list
	    foreach w {n x y z t o} var {label x y z type frac} {
		$np.e${row}$w delete 0 end
		$np.e${row}$w insert end [atominfo $phase $atom $var]
	    }
	    $np.e${row}u delete 0 end
	    if {[atominfo $phase $atom temptype] == "I"} {
		$np.e${row}u insert end [atominfo $phase $atom Uiso]
	    } else {
		$np.e${row}u insert end [expr ( \
			[atominfo $phase $atom U11] + \
			[atominfo $phase $atom U22] + \
			[atominfo $phase $atom U33]) / 3.]
	    }
	}
    }

    # add coordinates that have been read in, if any
    foreach item $expgui(coordList) {
	set row [MakeAddAtomsRow $top]
	foreach val $item w {n x y z t o u} {
	    if {$val != ""} {
		$np.e${row}$w delete 0 end
		$np.e${row}$w insert end $val
	    }
	}
    }
    # a blank spot in the table
    MakeAddAtomsRow $top

    bind $top <Configure> "SetAddAtomsScroll $top"
    grid rowconfigure $top 3 -min 10
    grid [button $top.b1 -text "Continue"\
	    -command "replacephase2 $phase $top [list $spg] [list $cell]"] \
	    -column 0 -row 5 -sticky w
    bind $top <Return> "replacephase2 $phase $top [list $spg] [list $cell]"
    grid [button $top.b2 -text Cancel \
	    -command "destroy $top"] -column 1 -row 5 -sticky w
    if {[llength $expgui(importFormatList)] > 0} {
	grid [frame $top.fr -bd 4 -relief groove] \
		-column 3 -row 5 -columnspan 2 -sticky e
	grid [button $top.fr.b3 -text "Import atoms from: " \
		-command "ImportAtoms \$expgui(importFormat) $top $phase"] \
		-column 0 -row 0 -sticky e
	set menu [eval tk_optionMenu $top.fr.b4 expgui(importFormat) \
		$expgui(importFormatList)]
	for {set i 0} {$i <= [$menu index end]} {incr i} {
	    $menu entryconfig $i -command "ImportAtoms \$expgui(importFormat) $top $phase"
	}
	grid $top.fr.b4 -column 1 -row 0 -sticky w
	grid rowconfig $top.fr 0 -pad 10
	grid columnconfig $top.fr 0 -pad 10
	grid columnconfig $top.fr 1 -pad 10
    }

    grid [button $top.b3 -text  "More atom boxes" \
	    -command "MakeAddAtomsRow $top"] -column 3 \
	    -columnspan 2 -row 4 -sticky e
    
    wm title $top "Replacing phase: Enter atoms"
    SetAddAtomsScroll $top

    # fix grab for old window
    afterputontop
    # set grab, etc.
    putontop $top
}

proc replacephase2 {phase top spg cell} {
    global expgui expmap env
    # validate coordinates
    set np $top.canvas.fr
    # validate the atoms info
    set atomlist [ValidateAtomsBox $top $np]
    if {$atomlist == ""} return

    pleasewait "updating phase"
    # replace spacegroup and cell
    phaseinfo $phase spacegroup set $spg
    foreach val $cell var {a b c alpha beta gamma} {
	phaseinfo $phase $var set $val
    }
    incr expgui(changed) 
    # delete all atoms
    foreach i $expmap(atomlist_$phase) {
	EraseAtom $i $phase
	incr expgui(changed) 
    }
    # write new atoms from table as input to exptool
    set fp [open exptool.in w]
    puts $fp "A"
    puts $fp $phase
    # number of atoms
    puts $fp [llength $atomlist]
    foreach atomline $atomlist {
	puts $fp $atomline
	incr expgui(changed) 
    }
    close $fp
    # needed in UNIX
    set env(ATOMDATA) [file join $expgui(gsasdir) data atmdata.dat]
    set env(gsas) [file nativename $expgui(gsasdir)]
    # needed in Windows
    set env(GSAS) [file nativename $expgui(gsasdir)]

    global tcl_platform
    # Save the current exp file
    savearchiveexp
    # disable the file changed monitor
    set expgui(expModifiedLast) 0
    set expnam [file root [file tail $expgui(expfile)]]
    catch {
	if {$tcl_platform(platform) == "windows"} {
	    exec [file join $expgui(gsasexe) exptool.exe] $expnam \
		    < exptool.in >& exptool.out
	} else {
	    exec [file join $expgui(gsasexe) exptool] $expnam \
		    < exptool.in >& exptool.out
	}
    } errmsg
    # load the revised exp file
    loadexp $expgui(expfile)
    set fp [open exptool.out r]
    set out [read $fp]
    close $fp
    set err 0
    if {[llength $atomlist] != [llength $expmap(atomlist_$phase))]} {
	set err 1
    }
    if {$errmsg != ""} {
	append errmsg "\n" $out
	set err 1
    } else {
	set errmsg $out
    }
    donewait 
    if {$expgui(showexptool) || $err} {
	set msg "Please review the result from adding the atom(s)" 
	if {$err} {append msg "\nIt appears an error occurred!"}
	ShowBigMessage $top $msg $errmsg OK "" $err
    }
    file delete exptool.in exptool.out
    # set the powpref warning (2 = required)
    set expgui(needpowpref) 2
    set msg "A phase was replaced"
    if {[string first $msg $expgui(needpowpref_why)] == -1} {
	append expgui(needpowpref_why) "\t$msg\n"
    }
    destroy $top
}

proc sortAddAtoms {phase top sortvar} {
    global expgui
    set np $top.canvas.fr
    set validlist {}
    set invalidlist {}
    set row 0
    while {![catch {grid info $np.e[incr row]t}]} {
	set valid 1
	set line $row
	if !{$expgui(UseAtom$row)} {set valid 0}
	lappend line $expgui(UseAtom$row)
	if {[set type [string trim [$np.e${row}t get]]] == ""} {set valid 0}
	lappend line [string trim [$np.e${row}t get]]
	lappend line [string trim [$np.e${row}n get]]
	foreach i {x y z o u} {
	    set tmp [string trim [$np.e${row}$i get]]
	    lappend line $tmp
	    if {$tmp == "" || [catch {expr $tmp}]} {set valid 0}
	}
	if {$valid} {
	    lappend validlist $line
	} else {
	    lappend invalidlist $line
	}
    }
    switch $sortvar {
	type {set sortlist [lsort -index 2 -dictionary $validlist]}
	name {set sortlist [lsort -index 3 -dictionary $validlist]}
	x {set sortlist [lsort -index 4 -real $validlist]}
	y {set sortlist [lsort -index 5 -real $validlist]}
	z {set sortlist [lsort -index 6 -real $validlist]}
	occ {set sortlist [lsort -index 7 -real $validlist]}
	uiso  {set sortlist [lsort -index 8 -real $validlist]}
	default {set sortlist $validlist}
    }

    if {[llength $invalidlist] > 0} {append sortlist " $invalidlist"}
    set row 0
    foreach line $sortlist {
	incr row
	set expgui(UseAtom$row) [lindex $line 1]
	foreach item [lrange $line 2 end] \
		var {t n x y z o u} {
	    $np.e${row}$var delete 0 end
	    $np.e${row}$var insert end $item
	}
    }
}

proc EditInstFile {"filename {}"} {
    global expgui
    # on the first call, load the commands
    if {[catch {
	if {[info procs instMakeWindow] == ""} {
	    source [file join $expgui(scriptdir) instedit.tcl]
	}
    } errmsg]} {
	MyMessageBox -parent . -title "Load error" \
		-message "Unexpected error while sourcing file instedit.tcl: $errmsg" \
		-icon error
    }
    instMakeWindow $filename
}

# load a list of Origin 1/2 space groups
proc GetOrigin12List {} {
    # don't need to read the file twice
    if {[array names ::Origin1list] != ""} return
    set line {}
    set fp1 [open [file join $::expgui(scriptdir) spacegrp.ref] r]
    while {[lindex $line 1] != 230} {
	if {[gets $fp1 line] < 0} break
    }
    while {[gets $fp1 line] >= 0} {
	set key [string tolower [lindex $line 8]]
	regsub -all " " $key "" key
	regsub -- "-3" $key "3" key
	if {$key != ""} {
#	puts "$key -- [lindex $line 1] [lindex $line 8] [lindex $line 9]"
	    set ::Origin1list($key) [lindex $line 9]
	}
    }
    close $fp1
}

# get the shift to be added to origin 1 coordinates to obtain origin 2 settings
proc GetOrigin1Shift {phase} {
    GetOrigin12List
    set spg [string tolower [phaseinfo $phase spacegroup]]
    regsub -all " " $spg "" spg
    regsub -- "-3" $spg "3" spg
    if {[catch {set shift $::Origin1list($spg)}]} {
	return ""
    } else {
	return $shift
    }
}

proc XformAtoms2Origin2 {phase numberList w1 shift} {
    global expgui expmap
    set parent [winfo toplevel $w1]
    if {[llength $numberList] != [llength $expmap(atomlist_$phase)]} {
	# not all atoms were selected in phase -- do a sanity check
	set msg {You have selected only some atoms to be shifted. Do you want to shift all atoms or only the selected atoms?}
	set val [MyMessageBox -parent $parent -icon warning \
		     -type "{Use all} {Use Selection}" -default "use all" \
		     -title "Shift all" -message $msg]
#	puts "$phase $numberList $w1 $shift"
	if {$val == "use all"} {set numberList $expmap(atomlist_$phase)}
    }
    if {[lindex $expmap(phasetype) [expr {$phase - 1}]] == 4} {
	set cmd mmatominfo
    } else {
	set cmd atominfo
    }
    foreach atom $numberList {
	foreach v {x y z} vs $shift {
	    set c [$cmd $phase $atom $v]
	    $cmd $phase $atom $v set [expr {$c + $vs}]
	}
	incr expgui(changed)
    }

    ResetMultiplicities $phase $parent
    SelectOnePhase $phase
    MyMessageBox -parent $parent -type OK -default ok -title "Shift applied" \
	-message "A shift of \"$shift\" has been added to coordinates of atoms [CompressList $numberList]"
#    UpdateAtomLine $numberList $phase
    destroy $parent
}

# reset the site multiplicities using the EXPEDT program
proc ResetMultiplicities {phase parent} {
    global expgui tcl_platform
    set input [open resetmult.inp w]
    puts $input "Y"
    puts $input "l a p $phase"
    puts $input "l"
    puts $input "x x x"
    puts $input "x"
    close $input
    # Save the current exp file
    savearchiveexp
    # disable the file changed monitor
    set expgui(expModifiedLast) 0
    set expnam [file root [file tail $expgui(expfile)]]
    set err [catch {
	if {$tcl_platform(platform) == "windows"} {
	    exec [file join $expgui(gsasexe) expedt.exe] $expnam < resetmult.inp >& resetmult.out
	} else {
	    exec [file join $expgui(gsasexe) expedt] $expnam < resetmult.inp >& resetmult.out
	}
    } errmsg]
    loadexp $expgui(expfile)
    catch {file delete resetmult.inp}
    if {$expgui(showexptool) || $err} {
	set fp [open resetmult.out r]
	set out [read $fp]
	close $fp
	if {$errmsg != ""} {
	    append errmsg "\n" $out
	} else {
	    set errmsg $out
	}
	set msg "Please review the result from listing the phase." 
	if {$err} {append msg "\nIt appears an error occurred!"}
	ShowBigMessage $parent.msg $msg $errmsg OK "" $err
    }
}

# default values
set newhist(insttype) {}
set newhist(dummy) 0
set newhist(instfiletext) {}
