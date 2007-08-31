# $Id: gsascmds.tcl,v 1.63 2006/03/29 03:52:11 toby Exp toby $
#------------------------------------------------------------------------------
# display routines
#------------------------------------------------------------------------------
#	Message box code that centers the message box over the parent.
#          or along the edge, if too close, 
#          but leave a border along +x & +y for reasons I don't remember
#       It also allows the button names to be defined using 
#            -type $list  -- where $list has a list of button names
#       larger messages are placed in a scrolled text widget
#       capitalization is now ignored for -default
#       The command returns the name button in all lower case letters
#       otherwise see  tk_messageBox for a description
#
#       This is a modification of tkMessageBox (msgbox.tcl v1.5)
#
proc MyMessageBox {args} {
    global tkPriv tcl_platform

    set w tkPrivMsgBox
    upvar #0 $w data

    #
    # The default value of the title is space (" ") not the empty string
    # because for some window managers, a 
    #		wm title .foo ""
    # causes the window title to be "foo" instead of the empty string.
    #
    set specs {
	{-default "" "" ""}
        {-icon "" "" "info"}
        {-message "" "" ""}
        {-parent "" "" .}
        {-title "" "" " "}
        {-type "" "" "ok"}
        {-helplink "" "" ""}
    }

    tclParseConfigSpec $w $specs "" $args

    if {[lsearch {info warning error question} $data(-icon)] == -1} {
	error "bad -icon value \"$data(-icon)\": must be error, info, question, or warning"
    }
    if {![string compare $tcl_platform(platform) "macintosh"]} {
      switch -- $data(-icon) {
          "error"     {set data(-icon) "stop"}
          "warning"   {set data(-icon) "caution"}
          "info"      {set data(-icon) "note"}
	}
    }

    if {![winfo exists $data(-parent)]} {
	error "bad window path name \"$data(-parent)\""
    }

    switch -- $data(-type) {
	abortretryignore {
	    set buttons {
		{abort  -width 6 -text Abort -under 0}
		{retry  -width 6 -text Retry -under 0}
		{ignore -width 6 -text Ignore -under 0}
	    }
	}
	ok {
	    set buttons {
		{ok -width 6 -text OK -under 0}
	    }
          if {![string compare $data(-default) ""]} {
		set data(-default) "ok"
	    }
	}
	okcancel {
	    set buttons {
		{ok     -width 6 -text OK     -under 0}
		{cancel -width 6 -text Cancel -under 0}
	    }
	}
	retrycancel {
	    set buttons {
		{retry  -width 6 -text Retry  -under 0}
		{cancel -width 6 -text Cancel -under 0}
	    }
	}
	yesno {
	    set buttons {
		{yes    -width 6 -text Yes -under 0}
		{no     -width 6 -text No  -under 0}
	    }
	}
	yesnocancel {
	    set buttons {
		{yes    -width 6 -text Yes -under 0}
		{no     -width 6 -text No  -under 0}
		{cancel -width 6 -text Cancel -under 0}
	    }
	}
	default {
#	    error "bad -type value \"$data(-type)\": must be abortretryignore, ok, okcancel, retrycancel, yesno, or yesnocancel"
	    foreach item $data(-type) {
		lappend buttons [list [string tolower $item] -text $item -under 0]
	    }
	}
    }

    if {[string compare $data(-default) ""]} {
	set valid 0
	foreach btn $buttons {
	    if {![string compare [lindex $btn 0] [string tolower $data(-default)]]} {
		set valid 1
		break
	    }
	}
	if {!$valid} {
	    error "invalid default button \"$data(-default)\""
	}
    }

    # 2. Set the dialog to be a child window of $parent
    #
    #
    if {[string compare $data(-parent) .]} {
	set w $data(-parent).__tk__messagebox
    } else {
	set w .__tk__messagebox
    }

    # 3. Create the top-level window and divide it into top
    # and bottom parts.

    catch {destroy $w}
    toplevel $w -class Dialog
    wm title $w $data(-title)
    wm iconname $w Dialog
    wm protocol $w WM_DELETE_WINDOW { }
    # Make the message box transient if the parent is viewable.
    if {[winfo viewable [winfo toplevel $data(-parent)]] } {
        wm transient $w $data(-parent)
    } 
   
    catch {
	if {[string equal [tk windowingsystem] "classic"]
	|| [string equal [tk windowingsystem] "aqua"]} {
	    unsupported::MacWindowStyle style $w dBoxProc
	}
    }

    frame $w.bot
    pack $w.bot -side bottom -fill both
    frame $w.top
    pack $w.top -side top -fill both -expand 1
    if {$data(-helplink) != ""} {
#	frame $w.help
#	pack $w.help -side top -fill both
	pack [button $w.top.1 -text Help -bg yellow \
		-command "MakeWWWHelp $data(-helplink)"] \
		-side right -anchor ne
	bind $w <Key-F1> "MakeWWWHelp $data(-helplink)"
    }
    if {[string compare $tcl_platform(platform) "macintosh"]} {
	$w.bot configure -relief raised -bd 1
	$w.top configure -relief raised -bd 1
    }

    # 4. Fill the top part with bitmap and message (use the option
    # database for -wraplength and -font so that they can be
    # overridden by the caller).

    option add *Dialog.msg.wrapLength 6i widgetDefault

    if {[string length $data(-message)] > 300} {
	if {![string compare $tcl_platform(platform) "macintosh"]} {
	    option add *Dialog.msg.t.font system widgetDefault
	} else {
	    option add *Dialog.msg.t.font {Times 18} widgetDefault
	}
	frame $w.msg
	grid [text  $w.msg.t  \
		-height 20 -width 55 -relief flat -wrap word \
		-yscrollcommand "$w.msg.rscr set" \
		] -row 1 -column 0 -sticky news
	grid [scrollbar $w.msg.rscr  -command "$w.msg.t yview" \
		] -row 1 -column 1 -sticky ns
	# give extra space to the text box
	grid columnconfigure $w.msg 0 -weight 1
	grid rowconfigure $w.msg 1 -weight 1
	$w.msg.t insert end $data(-message)
    } else {
	if {![string compare $tcl_platform(platform) "macintosh"]} {
	    option add *Dialog.msg.font system widgetDefault
	} else {
	    option add *Dialog.msg.font {Times 18} widgetDefault
	}
	label $w.msg -justify left -text $data(-message)
    }
    pack $w.msg -in $w.top -side right -expand 1 -fill both -padx 3m -pady 3m
    if {[string compare $data(-icon) ""]} {
	label $w.bitmap -bitmap $data(-icon)
	pack $w.bitmap -in $w.top -side left -padx 3m -pady 3m
    }

    # 5. Create a row of buttons at the bottom of the dialog.

    set i 0
    foreach but $buttons {
	set name [lindex $but 0]
	set opts [lrange $but 1 end]
      if {![llength $opts]} {
	    # Capitalize the first letter of $name
          set capName [string toupper \
		    [string index $name 0]][string range $name 1 end]
	    set opts [list -text $capName]
	}

      eval button [list $w.$name] $opts [list -command [list set tkPriv(button) $name]]

	if {![string compare $name [string tolower $data(-default)]]} {
	    $w.$name configure -default active
	}
      pack $w.$name -in $w.bot -side left -expand 1 -padx 3m -pady 2m

	# create the binding for the key accelerator, based on the underline
	#
	set underIdx [$w.$name cget -under]
	if {$underIdx >= 0} {
	    set key [string index [$w.$name cget -text] $underIdx]
          bind $w <Alt-[string tolower $key]>  [list $w.$name invoke]
          bind $w <Alt-[string toupper $key]>  [list $w.$name invoke]
	}
	incr i
    }

    # 6. Create a binding for <Return> on the dialog if there is a
    # default button.

    if {[string compare $data(-default) ""]} {
      bind $w <Return> [list $w.[string tolower $data(-default)] invoke]
    }

    # 7. Withdraw the window, then update all the geometry information
    # so we know how big it wants to be, then center the window in the
    # display and de-iconify it.

    wm withdraw $w
    update idletasks
    set wp $data(-parent)
    # center the new window in the middle of the parent
    set x [expr [winfo x $wp] + [winfo width $wp]/2 - \
	    [winfo reqwidth $w]/2 - [winfo vrootx $wp]]
    set y [expr [winfo y $wp] + [winfo height $wp]/2 - \
	    [winfo reqheight $w]/2 - [winfo vrooty $wp]]
    # make sure that we can see the entire window
    set xborder 10
    set yborder 25
    if {$x < 0} {set x 0}
    if {$x+[winfo reqwidth $w] +$xborder > [winfo screenwidth $w]} {
	incr x [expr \
		[winfo screenwidth $w] - ($x+[winfo reqwidth $w] + $xborder)]
    }
    if {$y < 0} {set y 0}
    if {$y+[winfo reqheight $w] +$yborder > [winfo screenheight $w]} {
	incr y [expr \
		[winfo screenheight $w] - ($y+[winfo reqheight $w] + $yborder)]
    }
    wm geom $w +$x+$y
    wm deiconify $w

    # 8. Set a grab and claim the focus too.

    catch {set oldFocus [focus]}
    catch {set oldGrab [grab current $w]}
    catch {
	grab $w
	if {[string compare $data(-default) ""]} {
	    focus $w.[string tolower $data(-default)]
	} else {
	    focus $w
	}
    }

    # 9. Wait for the user to respond, then restore the focus and
    # return the index of the selected button.  Restore the focus
    # before deleting the window, since otherwise the window manager
    # may take the focus away so we can't redirect it.  Finally,
    # restore any grab that was in effect.

    tkwait variable tkPriv(button)
    catch {focus $oldFocus}
    destroy $w
    catch {grab $oldGrab}
    return $tkPriv(button)
}

# tell'em what is happening
#    message    is a text message to display
#    statusvar  is a variable name containing a message that gets updated
#    parent     is the name of the parent window
#    button     defines a button for the window. Element 0 in $button is the
#               text for the button and Element 1 is the command to execute.
proc pleasewait {{message {}} {statusvar {}} {parent .} {button ""}} {
    catch {destroy .msg}
    toplevel .msg
    wm transient .msg [winfo toplevel .]
    pack [frame .msg.f -bd 4 -relief groove] -padx 5 -pady 5
    pack [message .msg.f.m -text "Please wait $message"] -side top
    if {$statusvar != ""} {
	pack [label .msg.f.status -textvariable $statusvar] -side top
    }
    if {$button != ""} {
	pack [button .msg.f.button -text [lindex $button 0] \
		-command [lindex $button 1]] -side top
    }
    wm withdraw .msg
    update idletasks
    # place the message on top of the parent window
    set x [expr [winfo x $parent] + [winfo width $parent]/2 - \
	    [winfo reqwidth .msg]/2 - [winfo vrootx $parent]]
    if {$x < 0} {set x 0}
    set y [expr [winfo y $parent] + [winfo height $parent]/2 - \
	    [winfo reqheight .msg]/2 - [winfo vrooty $parent]]
    if {$y < 0} {set y 0}
    wm geom .msg +$x+$y
    wm deiconify .msg
    global makenew
    set makenew(OldGrab) ""
    set makenew(OldFocus) ""
    # save focus & grab
    catch {set makenew(OldFocus) [focus]}
    catch {set makenew(OldGrab) [grab current .msg]}
    catch {grab .msg}
    update
}

# clear the message
proc donewait {} {
    global makenew
    catch {destroy .msg}
    # reset focus & grab
    catch {
	if {$makenew(OldFocus) != ""} {
	    focus $makenew(OldFocus)
	}
    }
    catch {
	if {$makenew(OldGrab) != ""} {
	    grab $makenew(OldGrab)
	}
    }
}

proc putontop {w "center 0"} {
    # center window $w above its parent and make it stay on top
    set wpt [winfo toplevel [set wp [winfo parent $w]]]
    if {[winfo viewable $wpt]} {
	wm transient $w $wpt
    }
    wm withdraw $w
    update idletasks
    if {$center} {
	set x [expr {[winfo screenwidth $w]/2 - [winfo reqwidth $w]/2 \
		- [winfo vrootx $wpt]}]
	set y [expr {[winfo screenheight $w]/2 - [winfo reqheight $w]/2 \
		- [winfo vrooty $wpt]}]
    } else {
	# center the new window in the middle of the parent
	set x [expr [winfo x $wpt] + [winfo width $wpt]/2 - \
		[winfo reqwidth $w]/2 - [winfo vrootx $wpt]]
	if {$x < 0} {set x 0}
	set xborder 10
	if {$x+[winfo reqwidth $w] +$xborder > [winfo screenwidth $w]} {
	    incr x [expr [winfo screenwidth $w] - \
		    ($x+[winfo reqwidth $w] + $xborder)]
	}
	set y [expr [winfo y $wpt] + [winfo height $wpt]/2 - \
		[winfo reqheight $w]/2 - [winfo vrooty $wpt]]
	if {$y < 0} {set y 0}
	set yborder 25
	if {$y+[winfo reqheight $w] +$yborder > [winfo screenheight $w]} {
	    incr y [expr [winfo screenheight $w] - \
		    ($y+[winfo reqheight $w] + $yborder)]
	}
    }
    wm geometry $w +$x+$y
    wm deiconify $w

    global makenew
    # set grab & focus; use new approach for 8.3 & later
    if {[info proc ::tk::SetFocusGrab] == ""} {
	set makenew(OldGrab) ""
	set makenew(OldFocus) ""
	catch {set makenew(OldFocus) [focus]}
	catch {set makenew(OldGrab) [grab current $w]}
	catch {grab $w}
    } else {
	set makenew(OldGrab) $w
	set makenew(OldFocus) $w
	::tk::SetFocusGrab $w $w
    }
}

# restore focus after putontop has completed
proc afterputontop {} {
    global makenew
    # reset focus & grab; use new approach for 8.3 & later
    if {[info proc ::tk::SetFocusGrab] == ""} {
	if {$makenew(OldFocus) != ""} {
	    catch {focus $makenew(OldFocus)}
	}
	if {$makenew(OldGrab) != ""} {
	    catch {grab $makenew(OldGrab)}
	}
    } else {
	catch {::tk::RestoreFocusGrab $makenew(OldGrab) $makenew(OldFocus)}
    }
}

proc ShowBigMessage {win labeltext msg "optionlist OK" "link {}" "err 0"} {
    catch {destroy $win}
    toplevel $win

    pack [label $win.l1 -text $labeltext] -side top
    if {$err} {$win.l1 config -fg red}
    pack [frame $win.f1] -side top -expand yes -fill both
    grid [text  $win.f1.t  \
	    -height 20 -width 55  -wrap none -font Courier \
	    -xscrollcommand "$win.f1.bscr set" \
	    -yscrollcommand "$win.f1.rscr set" \
	    ] -row 1 -column 0 -sticky news
    grid [scrollbar $win.f1.bscr -orient horizontal \
	    -command "$win.f1.t xview" \
	    ] -row 2 -column 0 -sticky ew
    grid [scrollbar $win.f1.rscr  -command "$win.f1.t yview" \
	    ] -row 1 -column 1 -sticky ns
    # give extra space to the text box
    grid columnconfigure $win.f1 0 -weight 1
    grid rowconfigure $win.f1 1 -weight 1
    $win.f1.t insert end $msg

    global makenew
    set makenew(result) 0
    bind $win <Return> "destroy $win"
    bind $win <KeyPress-Prior> "$win.f1.t yview scroll -1 page"
    bind $win <KeyPress-Next> "$win.f1.t yview scroll 1 page"
    bind $win <KeyPress-Right> "$win.f1.t xview scroll 1 unit"
    bind $win <KeyPress-Left> "$win.f1.t xview scroll -1 unit"
    bind $win <KeyPress-Up> "$win.f1.t yview scroll -1 unit"
    bind $win <KeyPress-Down> "$win.f1.t yview scroll 1 unit"
    bind $win <KeyPress-Home> "$win.f1.t yview 0"
    bind $win <KeyPress-End> "$win.f1.t yview end"
    set i 0
    foreach item $optionlist {
	pack [button $win.q[incr i] \
		-command "set makenew(result) $i; destroy $win" -text $item] -side left
    }
    if {$link != ""} {
	pack [button $win.help -text Help -bg yellow \
	    -command "MakeWWWHelp $link"] \
	    -side right
	bind $win <Key-F1> "MakeWWWHelp $link"
    }
    putontop $win
    tkwait window $win

    # fix grab...
    afterputontop
    return $makenew(result)
}

# get a value in a modal dialog
proc getstring {what "chars 40" "quit 1" "initvalue {}"} {
    global expgui expmap
    set w .global
    catch {destroy $w}
    toplevel $w -bg beige
    bind $w <Key-F1> "MakeWWWHelp expguierr.html Input[lindex $what 0]"
    wm title $w "Input $what"
    set expgui(temp) {}
    pack [frame $w.0 -bd 6 -relief groove -bg beige] \
	    -side top -expand yes -fill both
    grid [label $w.0.a -text "Input a value for the $what" \
	    -bg beige] \
	    -row 0 -column 0 -columnspan 10
    grid [entry $w.0.b -textvariable expgui(temp) -width $chars] \
	    -row 1 -column 0 

    set expgui(temp) $initvalue
    pack [frame $w.b -bg beige] -side top -fill x -expand yes
    pack [button $w.b.2 -text Set -command "destroy $w"] -side left
    if $quit {
	pack [button $w.b.3 -text Quit \
		-command "set expgui(temp) {}; destroy $w"] -side left
    }
    bind $w <Return> "destroy $w"
    pack [button $w.b.help -text Help -bg yellow \
	    -command "MakeWWWHelp expguierr.html Input[lindex $what 0]"] \
	    -side right

    # force the window to stay on top
    putontop $w

    focus $w.b.2
    tkwait window $w
    afterputontop

    return $expgui(temp)
}

#------------------------------------------------------------------------------
# profile/symmetry routines
#------------------------------------------------------------------------------
# profile terms
array set expgui {
    prof-T-names {"Von Dreele-Jorgensen-Windsor" \
		      "David-Ikeda-Carpenter" "Exponential pseudo-Voigt" \
		      "Exponential p-V+Stephens aniso strain" \
		      "Exponential p-V+macro strain"
    }
    prof-T-1 {alp-0 alp-1 bet-0 bet-1 sig-0 sig-1 sig-2 rstr rsta \
	    rsca s1ec s2ec }
    prof-T-2 {alp-0 alp-1 beta switch sig-0 sig-1 sig-2 gam-0 gam-1 \
	    gam-2 ptec stec difc difa zero }
    prof-T-3 {alp bet-0 bet-1 sig-0 sig-1 sig-2 gam-0 gam-1 \
	    gam-2 gsf g1ec g2ec rstr rsta rsca L11 L22 L33 L12 L13 L23 }
    prof-T-4 {alp bet-0 bet-1 sig-1 sig-2 gam-2 g2ec gsf \
	    rstr rsta rsca eta}
    prof-T-5 {alp bet-0 bet-1 sig-0 sig-1 sig-2 gam-0 gam-1 \
	    gam-2 gsf g1ec g2ec rstr rsta rsca D1 D2 D3 D4 D5 D6 }
    prof-C-names {"Gaussian only" "Pseudo-Voigt" \
		      "pseudo-Voigt/FCJ Asym" "p-V/FCJ+Stephens aniso strain" \
		      "p-V/FCJ+macro strain"
    }
    prof-C-1 {GU GV GW asym F1 F2 }
    prof-C-2 {GU GV GW LX LY trns asym shft GP stec ptec sfec \
	    L11 L22 L33 L12 L13 L23 }
    prof-C-3 {GU GV GW GP LX LY S/L H/L trns shft stec ptec sfec \
	    L11 L22 L33 L12 L13 L23 }
    prof-C-4 {GU GV GW GP LX ptec trns shft sfec S/L H/L eta} 
    prof-C-5 {GU GV GW GP LX LY S/L H/L trns shft stec ptec sfec \
            D1 D2 D3 D4 D5 D6 }
    prof-E-names {Gaussian "Otto pseudo-Voigt"}
    prof-E-1 {A B C ds cds}
    prof-E-2 {A B C ds cds LX LY ptec stec}
}

# number of profile terms depends on the histogram type
# the LAUE symmetry and the profile number
proc GetProfileTerms {phase hist ptype} {
    global expmap expgui
    if {$hist == "C" || $hist == "T" || $hist == "E"} {
	set htype $hist
    } else {
	set htype [string range $expmap(htype_$hist) 2 2]
    }
    # get the cached copy of the profile term labels, when possible
    set lbls {}
    catch {
	set lbls $expmap(ProfileTerms${phase}_${ptype}_${htype})
    }
    if {$lbls != ""} {return $lbls}

    catch {set lbls $expgui(prof-$htype-$ptype)}
    if {$lbls == ""} {return}
    # add terms based on the Laue symmetry
    if {($htype == "C" || $htype == "T") && $ptype == 4} {
	set laueaxis [GetLaue [phaseinfo $phase spacegroup]]
	eval lappend lbls [Profile4Terms $laueaxis]
    }
    set expmap(ProfileTerms${phase}_${ptype}_${htype}) $lbls
    return $lbls
}

proc Profile4Terms {laueaxis} {
# GSAS Laue classes by number vs spacegrp labeling
#   1    2    3    4     5      6     7       8     9      10     11     12   13  14
# 1bar, 2/m, mmm, 4/m, 4/mmm, 3bar, 3bar m, 3bar, 3barm1, 3bar1m, 6/m, 6/mmm, m 3, m3m
#                              R      R      H      H       H
# (R=Rhombohedral setting; H=Hexagonal setting)
    switch -exact $laueaxis {
	1bar {return \
		"S400 S040 S004 S220 S202 S022 S310 S103 S031 \
		S130 S301 S013 S211 S121 S112"}
	2/ma {return "S400 S040 S004 S220 S202 S022 S013 S031 S211"}
	2/mb {return "S400 S040 S004 S220 S202 S022 S301 S103 S121"}
	2/mc {return "S400 S040 S004 S220 S202 S022 S130 S310 S112"}
	mmm  {return "S400 S040 S004 S220 S202 S022"}
	4/m  {return "S400 S004 S220 S202 S310"}
	4/mmm {return "S400 S004 S220 S202"}
	3barR     {return "S400 S220 S310 S301 S211"}
	"3bar mR" {return "S400 S220 S310 S211"}
	3bar    {return "S400 S004 S202 S310 S211"}
	3barm1 {return "S400 S004 S202 S301"}
	3bar1m  {return "S400 S004 S202 S211"}
	6/m    {return "S400 S004 S202"}
	6/mmm  {return "S400 S004 S202"}
	"m 3"  {return "S400 S220"}
	m3m    {return "S400 S220"}
	default {return ""}
    }
}

proc GetLaue {spg} {
    global tcl_platform expgui
    # check the space group
    set fp [open spg.in w]
    puts $fp "N"
    puts $fp "N"
    puts $fp $spg
    puts $fp "Q"
    close $fp
    catch {
	if {$tcl_platform(platform) == "windows"} {
	    exec [file join $expgui(gsasexe) spcgroup.exe] < spg.in >& spg.out
	} else {
	    exec [file join $expgui(gsasexe) spcgroup] < spg.in >& spg.out
	}
    }
    set fp [open spg.out r]
    set laue {}
    set uniqueaxis {}
    while {[gets $fp line] >= 0} {
	regexp {Laue symmetry (.*)} $line junk laue
	regexp {The unique axis is (.*)} $line junk uniqueaxis
    }
    close $fp
    catch {file delete -force spg.in spg.out}
    set laue [string trim $laue]
    # add a R suffix for rhombohedral settings
    if {[string range [string trim $spg] end end] == "R"} {
	return "${laue}${uniqueaxis}R"
    }
    return "${laue}$uniqueaxis"
}

# set up to change the profile type for a series of histogram/phase entries
# (histlist & phaselist should be lists of the same length)
#
proc ChangeProfileType {histlist phaselist} {
    global expgui expmap 
    set w .profile
    catch {destroy $w}
    toplevel $w -bg beige
    wm title $w "Change Profile Function"
    
    # all histogram/phases better be the same type, so we can just use the 1st
    set hist [lindex $histlist 0]
    set phase [lindex $phaselist 0]
    set ptype [string trim [hapinfo $hist $phase proftype]]

    # get list of allowed profile terms for the current histogram type
    set i 1
    while {[set lbls [GetProfileTerms $phase $hist $i]] != ""} {
	lappend lbllist $lbls
	incr i
    }
    # labels for the current type
    set i $ptype
    set oldlbls [lindex $lbllist [incr i -1]]
    
    if {[llength $histlist] == 1} {
	pack [label $w.a -bg beige \
		-text "Change profile function for Histogram #$hist Phase #$phase" \
		] -side top
    } else {
	# make a list of histograms by phase
	foreach h $histlist p $phaselist {
	    lappend phlist($p) $h
	}
	set num 0
	pack [frame $w.a -bg beige] -side top 
	pack [label $w.a.$num -bg beige \
		-text "Change profile function for:" \
		] -side top -anchor w
	foreach i [lsort [array names phlist]] {
	    incr num
	    pack [label $w.a.$num -bg beige -text \
		    "\tPhase #$i, Histograms [CompressList $phlist($i)]" \
		    ] -side top -anchor w
	}
    }
    pack [label $w.e1 \
	    -text "Current function is type $ptype." \
	    -bg beige] -side top -anchor w
    pack [frame $w.e -bg beige] -side top -expand yes -fill both
    pack [label $w.e.1 \
	    -text "Set function to type" \
	    -bg beige] -side left 
    set menu [tk_optionMenu $w.e.2 expgui(newpeaktype) junk]
    pack $w.e.2 -side left -anchor w

    pack [radiobutton $w.e.4 -bg beige -variable expgui(DefaultPeakType) \
	    -command "set expgui(newpeaktype) $ptype; \
	    FillChangeProfileType $w.c $hist $phase $ptype [list $oldlbls] [list $oldlbls]" \
	    -value 1 -text "Current value overrides"] -side right
    pack [radiobutton $w.e.3 -bg beige -variable expgui(DefaultPeakType) \
	    -command \
	    "set expgui(newpeaktype) $ptype; \
	    FillChangeProfileType $w.c $hist $phase $ptype [list $oldlbls] [list $oldlbls]" \
	    -value 0 -text "Default value overrides"] -side right

    $w.e.2 config -bg beige
    pack [frame $w.c -bg beige] -side top -expand yes -fill both
    pack [frame $w.d -bg beige] -side top -expand yes -fill both
    pack [button $w.d.2 -text Set  \
	      -command "SaveChangeProfileType $w.c [list $histlist] [list $phaselist]; destroy $w"\
	    ] -side left
    pack [button $w.d.3 -text Quit \
	    -command "destroy $w"] -side left
    pack [button $w.d.help -text Help -bg yellow \
	    -command "MakeWWWHelp expgui5.html ChangeType"] \
	    -side right
    bind $w <Key-F1> "MakeWWWHelp expgui5.html ChangeType"
    bind $w <Return> "destroy $w"

    $menu delete 0 end
    set i 0
    foreach lbls $lbllist {
	incr i
	$menu add command -label $i -command \
		"set expgui(newpeaktype) $i; \
		FillChangeProfileType $w.c $hist $phase $i [list $lbls] [list $oldlbls]"
    }
    set expgui(newpeaktype) $ptype
    FillChangeProfileType $w.c $hist $phase $ptype $oldlbls $oldlbls

    # force the window to stay on top
    putontop $w
    focus $w.e.2
    tkwait window $w
    afterputontop
    sethistlist
}

# save the changes to the profile
proc SaveChangeProfileType {w histlist phaselist} {
    global expgui
    foreach phase $phaselist hist $histlist {
	hapinfo $hist $phase proftype set $expgui(newpeaktype)
	hapinfo $hist $phase profterms set $expgui(newProfileTerms)
	for {set i 1} {$i <=  $expgui(newProfileTerms)} {incr i} {
	    hapinfo $hist $phase pterm$i set [$w.ent${i} get]
	    hapinfo $hist $phase pref$i set $expgui(ProfRef$i)
	}
	set i [expr 1+$expgui(newProfileTerms)]
	hapinfo $hist $phase pcut set [$w.ent$i get]
	incr expgui(changed) [expr 3 + $expgui(newProfileTerms)]
    }
}

# file the contents of the "Change Profile Type" Menu
proc FillChangeProfileType {w hist phase newtype lbls oldlbls} {
    global expgui expmap 
    set ptype [string trim [hapinfo $hist $phase proftype]]
    catch {unset oldval}
    # loop through the old terms and set up an array of starting values
    set num 0
    foreach term $oldlbls {
	incr num
	set oldval($term) [hapinfo $hist $phase pterm$num]
    }
    set oldval(Peak\nCutoff) [hapinfo $hist $phase pcut]

    # is the new type the same as the current?
    if {$ptype == $newtype} {
	set nterms [hapinfo $hist $phase profterms]
    } else {
	set nterms [llength $lbls]
    }
    set expgui(newProfileTerms) $nterms
    set expgui(CurrentProfileTerms) $nterms
    # which default profile set matches the new type
    set setnum {}
    foreach j {" " 1 2 3 4 5 6 7 8 9} {
	set i [profdefinfo $hist $j proftype]
	if {$i == ""} continue
	if {$i == $newtype} {
	    set setnum $j
	    break
	}
    }

    eval destroy [winfo children $w]

    set colstr 0
    set row 2
    set maxrow [expr $row + $nterms/2]
    for { set num 1 } { $num <= $nterms + 1} { incr num } {
	# get the default value (originally from the in .INS file)
	set val {}
	if {$setnum != ""} {
	    set val 0.0
	    catch {
		set val [profdefinfo $hist $setnum pterm$num]
		# pretty up the number
		if {$val == 0.0} {
		    set val 0.0
		} elseif {abs($val) < 1e-2 || abs($val) > 1e6} {
		    set val [format %.3e $val]
		} elseif {abs($val) > 1e-2 && abs($val) < 10} {
		    set val [format %.5f $val]
		} elseif {abs($val) < 9999} {
		    set val [format %.2f $val]
		} elseif {abs($val) < 1e6} {
		    set val [format %.0f $val]
		}
	    }
	}
	# heading
	if {$row == 2} {
	    set col $colstr
	    grid [label $w.h0${num} -text "lbl" -bg beige] \
		-row $row -column $col
	    grid [label $w.h2${num} -text "ref" -bg beige] \
		-row $row -column [incr col]
	    grid [label $w.h3${num} -text "next value" -bg beige] \
		-row $row -column [incr col]
	    grid [label $w.h4${num} -text "default" -bg beige] \
		-row $row -column [incr col]
	    grid [label $w.h5${num} -text "current" -bg beige] \
		-row $row -column [incr col]
	}
	set col $colstr
	incr row
	set term {}
	catch {set term [lindex $lbls [expr $num-1]]}
	if {$term == ""} {set term $num}
	if {$num == $nterms + 1} {
	    set term "Peak\nCutoff"
	    set val {}
	    if {$setnum != ""} {
		set val 0.0
		catch {set val [profdefinfo $hist $setnum pcut]}
	    }
	}

	grid [label $w.l${num} -text "$term" -bg beige] \
		-row $row -column $col
	grid [checkbutton $w.chk${num} -variable expgui(ProfRef$num) \
		-bg beige -activebackground beige] -row $row -column [incr col]
	grid [entry $w.ent${num} \
		-width 12] -row $row -column [incr col]
	if {$val != ""} {
	    grid [button $w.def${num} -text $val -command \
		    "$w.ent${num} delete 0 end; $w.ent${num} insert end $val" \
		    ] -row $row -column [incr col] -sticky ew
	} else {
	    grid [label $w.def${num} -text (none) \
		    ] -row $row -column [incr col]
	}
	set curval {}
	catch {
	    set curval [expr $oldval($term)]
	    # pretty up the number
	    if {$curval == 0.0} {
		set curval 0.0
	    } elseif {abs($curval) < 1e-2 || abs($curval) > 1e6} {
		set curval [format %.3e $curval]
	    } elseif {abs($curval) > 1e-2 && abs($curval) < 10} {
		set curval [format %.5f $curval]
	    } elseif {abs($curval) < 9999} {
		set curval [format %.2f $curval]
	    } elseif {abs($curval) < 1e6} {
		set curval [format %.0f $curval]
	    }
	    grid [button $w.cur${num} -text $curval -command  \
		    "$w.ent${num} delete 0 end; $w.ent${num} insert end $curval" \
		    ] -row $row -column [incr col] -sticky ew
	}
	# set default values for flag and value
	set ref 0
	if {$setnum != ""} {
	    catch {
		if {[profdefinfo $hist $setnum pref$num] == "Y"} {set ref 1}
	    }
	}
	set expgui(ProfRef$num) $ref
	
	$w.ent${num} delete 0 end
	if {!$expgui(DefaultPeakType) && $val != ""} {
	    $w.ent${num} insert end $val
	} elseif {$curval != ""} {
	    $w.ent${num} insert end $curval
	} elseif {$val != ""} {
	    $w.ent${num} insert end $val
	} else {
	    $w.ent${num} insert end 0.0
	}
	if {$row > $maxrow} {
	    set row 2
	    incr colstr 5
	}
    }
    if {$::tcl_platform(os) == "Darwin"} {
	# on OS X force a window resize
	wm geometry [winfo toplevel $w] {}
    }
}

#------------------------------------------------------------------------------
# WWW/help routines
#------------------------------------------------------------------------------
# browse a WWW page with URL. The URL may contain a #anchor
# On UNIX assume netscape or mozilla is in the path or env(BROWSER) is loaded. 
# On Windows search the registry for a browser. Mac branch not tested.
# This is taken from http://mini.net/cgi-bin/wikit/557.html with many thanks
# to the contributers
proc urlOpen {url} {
    global env tcl_platform
    if {$tcl_platform(os) == "Darwin"} {
	# if this is an external URL or does not contain an anchor, take the 
	# easy approach
 	if {[string range $url 0 4] == "http:" || \
		[string first "#" $url] == -1} {
	    if {![catch {exec open $url}]} {
		return
	    }
	}
	# so sorry, have to use Internet Explorer
	set url [file nativename $url]; # replace ~/ if present
	if {[file pathtype $url] == "relative"} {
	    set url [file join [pwd] $url]
	}
	exec osascript -e "tell application \"Internet Explorer\"\rGetURL \"file://$url\"\rend tell"
    } elseif {$tcl_platform(platform) == "unix"} {
	set browserlist {}
	if {[info exists env(BROWSER)]} {
	    set browserlist $env(BROWSER)
	}
	lappend browserlist netscape mozilla 
	foreach p $browserlist {
	    set progs [auto_execok $p]
	    if {[llength $progs]} {
		if {[catch {exec $progs -remote openURL($url)}]} {
		    # perhaps browser doesn't understand -remote flag
		    if {[catch {exec $env(BROWSER) $url &} emsg]} {
			error "Error displaying $url in browser\n$emsg"
		    }
		}
		return
	    }
	}
	MyMessageBox -parent . -title "No Browser" \
	    -message "Could not find a browser. Netscape & Mozilla not found. Define environment variable BROWSER to be full path name of browser." \
	    -icon warning
    } elseif {$tcl_platform(platform) == "windows"} {
	package require registry
	# Look for the application under
	# HKEY_CLASSES_ROOT
	set root HKEY_CLASSES_ROOT
	
	# Get the application key for HTML files
	set appKey [registry get $root\\.html ""]
	
	# Get the command for opening HTML files
	set appCmd [registry get \
			$root\\$appKey\\shell\\open\\command ""]

	# Substitute the HTML filename into the command for %1
	# or stick it on the end
	if {[string first %1 $appCmd] != -1} {
	    regsub %1 $appCmd $url appCmd
	} else {
	    append appCmd " " $url
	}
	
	# Double up the backslashes for eval (below)
	regsub -all {\\} $appCmd  {\\\\} appCmd
	
	# Invoke the command
	eval exec $appCmd &
    } elseif {$tcl_platform(platform) == "macintosh"} {
	# preOSX -- this is not used
	if {0 == [info exists env(BROWSER)]} {
	    set env(BROWSER) "Browse the Internet"
	}
	if {[catch {
	    AppleScript execute\
		"tell application \"$env(BROWSER)\"
                         open url \"$url\"
                     end tell
                "} emsg]
	} then {
	    error "Error displaying $url in browser\n$emsg"
	}
    }
}

proc NetHelp {file anchor localloc netloc} {
    # use the file on-line, if it exists
    if {[file exists [file join $localloc $file]]} {
	set url "[file join $localloc $file]"
    } else {
	set url "http://$netloc/$file"
    }
    catch {
	pleasewait "Starting web browser..."
	after 2000 donewait
    }
    if {$anchor != ""} {
	append url # $anchor
    }
    urlOpen $url
}

proc MakeWWWHelp {"topic {}" "anchor {}"} {
    global expgui
    if {$topic == ""} {
	foreach item $expgui(notebookpagelist) {
	    if {[lindex $item 0] == $expgui(pagenow)} {
		NetHelp [lindex $item 5] [lindex $item 6] $expgui(docdir) $expgui(website)
		return
	    }
	}
	# this should not happen
	NetHelp expgui.html "" $expgui(docdir) $expgui(website)
    } elseif {$topic == "menu"} {
	NetHelp expguic.html "" $expgui(docdir) $expgui(website)
    } else {
	NetHelp $topic $anchor $expgui(docdir) $expgui(website)
    }
}

# show help information
proc showhelp {} {
    global expgui_helplist helpmsg
    set helpmsg {}
    set frm .help
    catch {destroy $frm}
    toplevel $frm
    wm title $frm "Help Summary"
    grid [label $frm.0 -text \
	    "Click on an entry below to see information on the EXPGUI/GSAS topic" ] \
	-column 0 -columnspan 4 -row 0
#    grid [message $frm.help -textvariable helpmsg -relief groove] \
#	   -column 0 -columnspan 4 -row 2 -sticky nsew
    grid [text $frm.help -relief groove -bg beige -width 0\
	    -height 0 -wrap word -yscrollcommand "$frm.escroll set"] \
	   -column 0 -columnspan 3 -row 2 -sticky nsew
    grid [scrollbar $frm.escroll -command "$frm.help yview"] \
	    -column 4 -row 2 -sticky nsew
    grid rowconfig $frm 1 -weight 1 -minsize 50
    grid rowconfig $frm 2 -weight 2 -pad 20 -minsize 150
    grid columnconfig $frm 0 -weight 1
    grid columnconfig $frm 2 -weight 1
    set lst [array names expgui_helplist]
    grid [listbox $frm.cmds -relief raised -bd 2 \
	    -yscrollcommand "$frm.scroll set" \
	    -height 8 -width 0 -exportselection 0 ] \
	    -column 0 -row 1 -sticky nse 
    grid [scrollbar $frm.scroll -command "$frm.cmds yview"] \
	    -column 1 -row 1 -sticky nsew
    foreach item [lsort -dictionary $lst] {
	$frm.cmds insert end $item 
    }
    if {[$frm.cmds curselection] == ""} {$frm.cmds selection set 0}
    grid [button $frm.done -text Done -command "destroy $frm"] \
	    -column 2 -row 1
#    bind $frm.cmds <ButtonRelease-1> \
#	    "+set helpmsg \$expgui_helplist(\[$frm.cmds get \[$frm.cmds curselection\]\])"
    bind $frm.cmds <ButtonRelease-1> \
	    "+$frm.help config -state normal; $frm.help delete 0.0 end; \
	     $frm.help insert end \$expgui_helplist(\[$frm.cmds get \[$frm.cmds curselection\]\]); \
	     $frm.help config -state disabled"

    # get the size of the window and expand the message boxes to match
#    update
#    $frm.help config -width [winfo width $frm.help ]
}


#------------------------------------------------------------------------------
# utilities
#------------------------------------------------------------------------------
# run liveplot
proc liveplot {{dir {}}} {
    global expgui liveplot wishshell expmap
    set expnam [file root [file tail $expgui(expfile)]]
    # which histograms are ready for use?
    set validlist {}
    foreach ihist $expmap(powderlist) {
	if {[string trim [string range $expmap(htype_$ihist) 3 3]] == "" || \
		[string range $expmap(htype_$ihist) 3 3] == "D"} {
	    lappend validlist $ihist
	}
    }
    if {[llength $validlist] == 0} {
	MyMessageBox -parent . -title "No Valid Histograms" \
		-message "No histograms are ready to plot. Run GENLES and try again" \
		-icon warning -helplink "expguierr.html NoValidHist"
	return
    }
    if {$dir == ""} {
    	set dir $expgui(scriptdir)
    }
    # use $liveplot(hst) if valid, the 1st entry otherwise
    if {[lsearch $validlist $liveplot(hst)] != -1} {
	exec $wishshell [file join $dir liveplot] \
		$expnam $liveplot(hst) $liveplot(legend) &
    } else {
	exec $wishshell [file join $dir liveplot] \
		$expnam [lindex $validlist 0] $liveplot(legend) &
    }
}

# run lstview
proc lstview {} {
    global expgui wishshell
    set expnam [file root [file tail $expgui(expfile)]]
    exec $wishshell [file join $expgui(scriptdir) lstview] $expnam &
}

# run widplt
proc widplt {"prog widplt"} {
    global expgui wishshell
    exec $wishshell [file join $expgui(scriptdir) $prog] \
	    $expgui(expfile) &
}

# run bkgedit
proc bkgedit {"hst {}"} {
    global expgui liveplot wishshell expmap
    set expnam [file root [file tail $expgui(expfile)]]
    if {$hst == ""} {
	# which histograms are ready for use?
	set validlist {}
	foreach ihist $expmap(powderlist) {
	    if {[string trim [string range $expmap(htype_$ihist) 3 3]] == "" || \
		    [string range $expmap(htype_$ihist) 3 3] == "*"} {
		lappend validlist $ihist
	    }
	}
	if {[llength $validlist] == 0} {
	    MyMessageBox -parent . -title "No Valid Histograms" \
		    -message "No histograms are ready to plot. Run POWPREF and try again" \
		    -icon warning -helplink "expguierr.html NoValidHist"
	    return
	}
	# use $liveplot(hst) if valid, the 1st entry otherwise
	if {[lsearch $validlist $liveplot(hst)] != -1} {
	    set hst $liveplot(hst)
	} else {
	    set hst [lindex $validlist 0]
	}
    }
    # Save the current exp file
    savearchiveexp
    # disable the file change monitor if we will reload the .EXP file automatically
    if {$expgui(autoexpload)} {set expgui(expModifiedLast) 0}
    if {$expgui(autoiconify)} {wm iconify .}
    exec $wishshell [file join $expgui(scriptdir) bkgedit] \
	    $expnam $hst $liveplot(legend)
    if {$expgui(autoiconify)} {wm deiconify .}
    # load the changed .EXP file automatically?
    if {$expgui(autoexpload)} {
	# load the revised exp file
	loadexp $expgui(expfile)
    } else {
	# check for changes in the .EXP file immediately
	whenidle
    }
}

# run excledt
proc excledit {} {
    global expgui liveplot wishshell expmap
    set expnam [file root [file tail $expgui(expfile)]]
    # which histograms are ready for use?
    set validlist {}
    foreach ihist $expmap(powderlist) {
	if {[string trim [string range $expmap(htype_$ihist) 3 3]] == "" || \
		[string range $expmap(htype_$ihist) 3 3] == "*" || \
		[string range $expmap(htype_$ihist) 3 3] == "D"} {
	    lappend validlist $ihist
	}
    }
    if {[llength $validlist] == 0} {
	MyMessageBox -parent . -title "No Valid Histograms" \
		-message "No histograms are ready to plot. Run POWPREF and try again" \
		-icon warning -helplink "expguierr.html NoValidHist"
	return
    }
    #if {$expgui(autoiconify)} {wm iconify .}
    StartExcl 
    #if {$expgui(autoiconify)} {wm deiconify .}
}

# compute the composition for each phase and display in a dialog
proc composition {} {
    global expmap expgui
    set Z 1
    foreach phase $expmap(phaselist) type $expmap(phasetype) {
	if {$type == 4} continue
	ResetMultiplicities $phase {}
	catch {unset total}
	foreach atom $expmap(atomlist_$phase) {
	    set type [atominfo $phase $atom type]
	    set mult [atominfo $phase $atom mult]
	    if [catch {set total($type)}] {
		set total($type) [expr \
			$mult * [atominfo $phase $atom frac]]
	    } else {
		set total($type) [expr $total($type) + \
			$mult * [atominfo $phase $atom frac]]
	    }
	    if {$mult > $Z} {set Z $mult}
	}
	append text "\nPhase $phase\n"
	append text "  Unit cell contents\n"
	foreach type [lsort [array names total]] {
	    append text "   $type[format %8.3f $total($type)]"
	}
	append text "\n\n"
	
	append text "  Asymmetric Unit contents (Z=$Z)\n"
	foreach type [lsort [array names total]] {
	    append text "   $type[format %8.3f [expr $total($type)/$Z]]"
	}
	append text "\n"
    }
    
    catch {destroy .comp}
    toplevel .comp -class MonoSpc
    bind .comp <Key-F1> "MakeWWWHelp expgui.html Composition"
    wm title .comp Composition
    pack [label .comp.results -text $text \
	    -justify left] -side top
    pack [frame .comp.box]  -side top -expand y -fill x
    pack [button .comp.box.1 -text Close -command "destroy .comp"] -side left

    set lstnam [string toupper [file tail [file rootname $expgui(expfile)].LST]]
    pack [button .comp.box.2 -text "Save to $lstnam file" \
	    -command "writelst [list $text] ; destroy .comp"] -side left
    pack [button .comp.box.help -text Help -bg yellow \
	    -command "MakeWWWHelp expgui.html Composition"] \
	    -side right
}

# Delete History Records
proc DeleteHistoryRecords {{msg ""}} {
    global expgui
    set frm .history
    catch {destroy $frm}
    toplevel $frm
    bind $frm <Key-F1> "MakeWWWHelp expgui.html DeleteHistoryRecords"
    if {[string trim $msg] == ""} {
	set msg "There are [CountHistory] history records"
    }
    pack [frame $frm.1 -bd 2 -relief groove] -padx 3 -pady 3 -side left
    pack [label $frm.1.0 -text $msg] -side top
    pack [frame $frm.1.1] -side top
    pack [label $frm.1.1.1 -text "Number of entries to keep"] -side left
    pack [entry $frm.1.1.2 -width 3 -textvariable expgui(historyKeep)\
	    ] -side left
    set expgui(historyKeep) 10
    pack [checkbutton $frm.1.2 -text renumber -variable expgui(renumber)] -side top
    set expgui(renumber) 1
    pack [frame $frm.2] -padx 3 -pady 3 -side left -fill both -expand yes
    pack [button $frm.2.help -text Help -bg yellow \
	    -command "MakeWWWHelp expgui.html DeleteHistoryRecords"] -side top
    pack [button $frm.2.4 -text Quit \
	    -command {destroy .history}] -side bottom
    pack [button $frm.2.3 -text OK \
	    -command { 
	if ![catch {expr $expgui(historyKeep)}] {
	    DeleteHistory $expgui(historyKeep) $expgui(renumber)
	    set expgui(changed) 1
	    destroy .history
	}
    }] -side bottom
    bind $frm <Return> "$frm.2.3 invoke"
    
    # force the window to stay on top
    putontop $frm 
    focus $frm.2.3
    tkwait window $frm
    afterputontop
}

proc archiveexp {} {
    global expgui tcl_platform
    # is there a file to archive?
    if {![file exists $expgui(expfile)]} return
    set expnam [file rootname $expgui(expfile)]
    # get the last archived version
    set lastf [lindex [lsort [glob -nocomplain $expnam.{O\[0-9A-F\]\[0-9A-F\]}]] end]
    if {$lastf == ""} {
	set num 01
    } else {
	regexp {.*\.O([0-9A-F][0-9A-F])$} $lastf a num
	scan $num %x num
	if {$num >= 255} {
	    set num FF
	} else {
	    set num [string toupper [format %.2x [incr num]]]
	}
    }
    catch {
	set file $expnam.O$num
	file copy -force $expgui(expfile) $file
	set fp [open $expnam.LST a+]
	puts $fp "\n----------------------------------------------"
	puts $fp "     Archiving [file tail $expnam.EXP] as [file tail $file]"
	puts $fp "----------------------------------------------\n"
	close $fp
    } errmsg
    if {$errmsg != ""} {
	tk_dialog .warn Confirm "Error archiving the current .EXP file: $errmsg" warning 0 OK
    }
}

# save and optionally archive the expfile
proc savearchiveexp {} {
    global expgui expmap
    if {$expgui(expfile) == ""} {
	if {[catch {expr !$expgui(guiless)} rst] || $rst} SaveAsFile
	return
    }
    if !$expgui(changed) return
    if {$expgui(archive)} archiveexp
    # add a history record
    exphistory add " EXPGUI [lindex $expgui(Revision) 1] [lindex $expmap(Revision) 1] ($expgui(changed) changes) -- [clock format [clock seconds] -format {%D %T}]"
    # now save the file
    expwrite $expgui(expfile)
    # change the icon and assign an app to this .EXP file
    global tcl_platform
    if {$tcl_platform(os) == "Darwin" && $expgui(MacAssignApp)} {
	MacSetResourceFork $expgui(expfile)
    }
    set expgui(changed) 0
    set expgui(expModifiedLast) [file mtime $expgui(expfile)]
    set expgui(last_History) [string range [string trim [lindex [exphistory last] 1]] 0 50 ]
    wm title . $expgui(expfile)
    set expgui(titleunchanged) 1
    # set convergence criterion
    if {[catch {expr !$expgui(guiless)} rst] || $rst} InitLSvars
}

#------------------------------------------------------------------------------
# GSAS interface routines
#------------------------------------------------------------------------------
# run a GSAS program that does not require an experiment file
proc runGSASprog {proglist "concurrent 1"} {
    # if concurrent is 0, EXPGUI runs the GSAS program in background
    # -- this is not currently needed anywhere where the .EXP file is not.
    global expgui tcl_platform
    set cmd {}
    foreach prog $proglist {
	StartGRWND $prog
	if {$tcl_platform(platform) == "windows"} {
	    if {$tcl_platform(os) == "Windows 95"} {
		    append cmd " \"$expgui(gsasexe)/${prog}.exe \" "
	    } else {
		    lappend cmd [list $expgui(gsasexe)/${prog}.exe]
	    }
	} else {
	    if {$cmd != ""} {append cmd "\;"}
	    append cmd "[file join $expgui(gsasexe) $prog]"
	}
    }
    forknewterm $prog $cmd [expr !$concurrent] 1
}

# dummy routine, overridden if needed
proc StartGRWND {prog} {
}

# run a GSAS program that requires an experiment file for input/output
proc runGSASwEXP {proglist "concurrent 0"} {
    # most programs that require the .EXP file change it and 
    # cannot be run concurrently
    global expgui tcl_platform
    # Save the current exp file
    savearchiveexp
    # load the changed .EXP file automatically?
    if {$expgui(autoexpload)} {
	# disable the file changed monitor
	set expgui(expModifiedLast) 0
    }
    set cmd {}
    set expnam [file root [file tail $expgui(expfile)]]
    foreach prog $proglist {
	if {$prog == "powpref"} {
	    set expgui(needpowpref) 0
	    set expgui(needpowpref_why) ""
	} elseif {$prog == "genles" && $expgui(needpowpref) != 0} {
	    set msg "You are attempting to run GENLES, after making changes that require POWPREF:\n\n$expgui(needpowpref_why) \nRun POWPREF first?"
	    set ans [MyMessageBox -parent . -title "Run POWPREF" \
		    -message $msg -icon warning -type "Yes No" -default yes \
		    -helplink "expguierr.html RunPowpref"]
	    if {$ans == "yes"} {
		set expgui(needpowpref) 0
		set expgui(needpowpref_why) ""
		if {$tcl_platform(platform) == "windows"} {
		    if {$tcl_platform(os) == "Windows 95"} {
			    append cmd " \"$expgui(gsasexe)/powpref.exe $expnam \" "
		    } else {
			    lappend cmd [list $expgui(gsasexe)/powpref.exe $expnam]
		    }
		} else {
		    if {$cmd != ""} {append cmd "\;"}
		    append cmd "[file join $expgui(gsasexe) powpref] $expnam"
		}
	    }
	}
	StartGRWND $prog
	if {$tcl_platform(platform) == "windows"} {
	    if {$tcl_platform(os) == "Windows 95"} {
		    append cmd " \"$expgui(gsasexe)/${prog}.exe $expnam \" "
	    } else {
		    lappend cmd [list $expgui(gsasexe)/${prog}.exe $expnam]
	    }
	} else {
	    if {$cmd != ""} {append cmd "\;"}
	    append cmd "[file join $expgui(gsasexe) $prog] $expnam"
	}
    }
    forknewterm "$prog -- $expnam" $cmd [expr !$concurrent] 1
    # load the changed .EXP file automatically?
    if {$expgui(autoexpload)} {
	# load the revised exp file
	loadexp $expgui(expfile)
    }
}

# write text to the .LST file
proc writelst {text} {
    global expgui
    set lstnam [file rootname $expgui(expfile)].LST 
    set fp [open $lstnam a]
    puts $fp "\n-----------------------------------------------------------------"
    puts $fp $text
    puts $fp "-----------------------------------------------------------------\n"
    close $fp
}


# rename file current to suggested, 
#   delete window if supplied
#   use parent, if supplied or .
proc RenameAsFile {current suggested "window {}" "parent {}"} {
    if {$parent == "" && $window != ""} {set parent $window}
    if {$parent == ""} {set parent .}
    set newfile [tk_getSaveFile -initialfile $suggested -parent $parent]
    if {$newfile == ""} return
    if {[catch {
	file rename -force $current $newfile
    }]} {
	file copy -force $current $newfile
	file delete -force $current
    }
    if {$window != ""} {destroy $window}
}

# optionally run disagl as a windowless process, w/results in a separate window
proc rundisagl {} {
    global expgui txtvw tcl_version tcl_platform
    if {$expgui(disaglSeparateBox)} {
        set root [file root $expgui(expfile)] 
	catch {file delete -force $root.tmp}
        if {[catch {file rename -force $root.LST $root.OLS}]} {
	    file copy -force $root.LST $root.OLS
	    file delete -force $root.OLS
	}
	# PSW reports this does not happen right away on windows
	set i 0
	while {$i < 10 && [file exists $root.LST]} {
	    # debug code
	    #catch {console show}
	    #puts "try $i"
	    # end debug code
	    after 100
	    incr i
	}
	if {[file exists $root.LST]} {
	    # it was not possible to rename the file
	    MyMessageBox -parent . -title "Rename Problem" \
		-message "Unable to rename $root.LST. Please close LSTVIEW and try again" \
		-icon warning -helplink "expguierr.html NoRename"
	    return
	}

	#run the program
	pleasewait "Running DISAGL"	
	# create an empty input file
	close [open disagl.inp w]
	catch {exec [file join $expgui(gsasexe) disagl] \
		[file tail $root] < disagl.inp > disagl.out}
        if {[catch {file rename -force $root.LST $root.tmp}]} {
	    file copy -force $root.LST $root.tmp
	    file delete -force $root.LST
	}
	catch {file delete -force disagl.inp disagl.out}
        if {[catch {file rename -force $root.OLS $root.LST}]} {
	    file copy -force $root.OLS $root.LST
	    file delete -force $root.OLS
	}
	donewait
        # open a new window
        catch {toplevel .disagl}
        catch {eval grid forget [grid slaves .disagl]}
        text .disagl.txt -width 100 -wrap none \
                -yscrollcommand ".disagl.yscroll set" \
                -xscrollcommand ".disagl.xscroll set" 
        scrollbar .disagl.yscroll -command ".disagl.txt yview"
        scrollbar .disagl.xscroll -command ".disagl.txt xview" -orient horizontal
        grid .disagl.xscroll -column 0 -row 2 -sticky ew
        grid .disagl.txt -column 0 -row 1 -sticky nsew
        grid .disagl.yscroll -column 1 -row 1 -sticky ns
	grid [frame .disagl.f] -column 0 -columnspan 2 -row 3 -sticky ew
	grid columnconfig .disagl.f 2 -weight 1
	grid [button .disagl.f.close -text "Close & Delete" \
		-command "destroy .disagl; file delete $root.tmp"] \
		-column 3 -row 0 -sticky e
	grid [button .disagl.f.rename \
		-command "RenameAsFile $root.tmp $root.DIS .disagl" \
		-text "Close & Save as..."] \
		-column 4 -row 0 -sticky e
	# allow font changes on the fly
        if {$tcl_version >= 8.0} {
	    .disagl.txt config -font $txtvw(font)
	    set fontbut [tk_optionMenu .disagl.f.font txtvw(font) ""]
	    grid .disagl.f.font -column 1 -row 0 -sticky w
	    grid [label .disagl.f.t -text font:] -column 0 -row 0 -sticky w
	    $fontbut delete 0 end
	    foreach f {5 6 7 8 9 10 11 12 13 14 15 16} {
		$fontbut add command -label "Courier $f" -font "Courier $f"\
			-command "set txtvw(font) \"Courier $f\"; \
			.disagl.txt config -font \$txtvw(font)"
	    }
	}
	
        grid columnconfigure .disagl 0 -weight 1
        grid rowconfigure .disagl 1 -weight 1
        wm title .disagl "DISAGL results $expgui(expfile)"
        wm iconname .disagl "DISAGL $root"
        set in [open $root.tmp r]
        .disagl.txt insert end [read $in]
        close $in
        bind all  {destroy .disagl}
        bind .disagl  ".disagl.txt yview scroll -1 page"
        bind .disagl  ".disagl.txt yview scroll 1 page"
        bind .disagl  ".disagl.txt xview scroll 1 unit"
        bind .disagl  ".disagl.txt xview scroll -1 unit"
        bind .disagl  ".disagl.txt yview scroll -1 unit"
        bind .disagl  ".disagl.txt yview scroll 1 unit"
        bind .disagl  ".disagl.txt yview 0"
        bind .disagl  ".disagl.txt yview end"
        # don't disable in Win as this prevents the highlighting of selected text
        if {$tcl_platform(platform) != "windows"} {
            .disagl.txt config -state disabled
        }
    } else {
        runGSASwEXP disagl
    }
}

#------------------------------------------------------------------------------
# file conversions
#------------------------------------------------------------------------------
proc convfile {} {
    global expgui
    set frm .file
    catch {destroy $frm}
    toplevel $frm
    wm title $frm "Convert File"
    bind $frm <Key-F1> "MakeWWWHelp expgui.html ConvertWin"
    pack [frame [set frmA $frm.1] -bd 2 -relief groove] -padx 3 -pady 3 -side left
    pack [frame [set frmC $frm.3] ] -padx 3 -pady 3 \
	    -side left -fill y -expand yes
    pack [button $frmC.help -text Help -bg yellow \
	    -command "MakeWWWHelp expgui.html ConvertWin"] -side top
    pack [button $frmC.q -text Quit -command "destroy $frm"] -side bottom
    pack [button $frmC.b -text Convert -command "ValidWinCnv $frm"] \
	    -side bottom
    pack [label $frmA.0 -text "Select a file to convert"] -side top -anchor center
    winfilebox $frm
    bind $frm <Return> "ValidWinCnv $frm"

    # force the window to stay on top
    putontop $frm
    focus $frmC.q 
    tkwait window $frm
    afterputontop
}

# validate the files and make the conversion
proc ValidWinCnv {frm} {
    global expgui
    # change backslashes to something sensible
    regsub -all {\\} $expgui(FileMenuCnvName) / expgui(FileMenuCnvName)
    # allow entry of D: for D:/ and D:TEST for d:/TEST
    if {[string first : $expgui(FileMenuCnvName)] != -1 && \
	    [string first :/ $expgui(FileMenuCnvName)] == -1} {
	regsub : $expgui(FileMenuCnvName) :/ expgui(FileMenuCnvName)
    }
    if {$expgui(FileMenuCnvName) == "<Parent>"} {
	set expgui(FileMenuDir) [file dirname [set expgui(FileMenuDir)] ]
	ChooseWinCnv $frm
	return
    } elseif [file isdirectory \
	    [file join $expgui(FileMenuDir) $expgui(FileMenuCnvName)]] {
	if {$expgui(FileMenuCnvName) != "."} {
	    set expgui(FileMenuDir) \
		[file join $expgui(FileMenuDir) $expgui(FileMenuCnvName)]
	}
	ChooseWinCnv $frm
	return
    }
 
    set file [file join $expgui(FileMenuDir) $expgui(FileMenuCnvName)]
    if ![file exists $file] {
	MyMessageBox -parent $frm -title "Convert Error" \
		-message "File $file does not exist" -icon error
	return
    }

    set tmpname "[file join [file dirname $file] tempfile.xxx]"
    set oldname "[file rootname $file].org"
    if [file exists $oldname] {
	set ans [MyMessageBox -parent . -title "Overwrite?" \
		-message "File [file tail $oldname] exists in [file dirname $oldname]. OK to overwrite?" \
		-icon warning -type {Overwrite Cancel} -default Overwrite \
		-helplink "expguierr.html OverwriteCnv"]
	if {[string tolower $ans] == "cancel"} return
	catch {file delete $oldname}
    }

    if [catch {
	set in [open $file r]
	set out [open $tmpname w]
	fconfigure $out -translation crlf -encoding ascii
	set len [gets $in line]
	if {$len > 160} {
	    # this is a UNIX file. Hope there are no control characters
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
	file rename -force $file $oldname
	file rename -force $tmpname $file
    } errmsg] {
	MyMessageBox -parent $frm -title "Conversion error" \
		-message "Error in conversion:\n$errmsg" -icon warning
    } else {
	set ans [MyMessageBox -parent $frm -title "More?" \
		-message "File [file tail $file] converted.\n(Original saved as [file tail $oldname]).\n\n Convert more files?" \
		-type yesno -default no]
	if {$ans == "no"} {destroy $frm}
    }
}

# create a file box
proc winfilebox {frm} {
    global expgui
    set bx $frm.1
    pack [frame $bx.top] -side top 
    pack [label $bx.top.a -text "Directory" ] -side left 
    set expgui(FileDirButtonMenu) [tk_optionMenu $bx.top.d expgui(FileMenuDir) [pwd] ]
    pack $bx.top.d -side left
    set expgui(FileMenuDir) [pwd]
    # the icon below is from tk8.0/tkfbox.tcl
    set upfolder [image create bitmap -data {
#define updir_width 28
#define updir_height 16
static char updir_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x80, 0x1f, 0x00, 0x00, 0x40, 0x20, 0x00, 0x00,
   0x20, 0x40, 0x00, 0x00, 0xf0, 0xff, 0xff, 0x01, 0x10, 0x00, 0x00, 0x01,
   0x10, 0x02, 0x00, 0x01, 0x10, 0x07, 0x00, 0x01, 0x90, 0x0f, 0x00, 0x01,
   0x10, 0x02, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01,
   0x10, 0xfe, 0x07, 0x01, 0x10, 0x00, 0x00, 0x01, 0x10, 0x00, 0x00, 0x01,
   0xf0, 0xff, 0xff, 0x01};}]

    pack [button $bx.top.b -image $upfolder \
	    -command "updir; ChooseWinCnv $frm" ]
    pack [frame $bx.a -width 200 -height 75] -side top -expand yes -fill both
    listbox $bx.a.files -relief raised -bd 2 \
	    -yscrollcommand "sync2boxesY $bx.a.files $bx.a.dates $bx.a.scroll" \
	    -height 15 -width 0 -exportselection 0 
    listbox $bx.a.dates -relief raised -bd 2 \
	    -yscrollcommand "sync2boxesY $bx.a.dates $bx.a.files $bx.a.scroll" \
	    -height 15 -width 0 -takefocus 0 -exportselection 0 
    scrollbar $bx.a.scroll -command "move2boxesY \" $bx.a.files $bx.a.dates \" "
    ChooseWinCnv $frm
    bind $bx.a.files <ButtonRelease-1> "ReleaseWinCnv $frm"
    bind $bx.a.dates <ButtonRelease-1> "ReleaseWinCnv $frm"
    bind $bx.a.files <Double-1> "SelectWinCnv $frm"
    bind $bx.a.dates <Double-1> "SelectWinCnv $frm"
    pack $bx.a.scroll -side left -fill y 
    pack $bx.a.files $bx.a.dates -side left -fill both -expand yes
    pack [entry $bx.c -textvariable expgui(FileMenuCnvName)] -side top
}

# set the box or file in the selection window
proc ReleaseWinCnv {frm} {
    global expgui
    set files $frm.1.a.files
    set dates $frm.1.a.dates
    set select [$files curselection]
    if {$select == ""} {
	set select [$dates curselection]
    }
    if {$select == ""} {
	set expgui(FileMenuCnvName) ""
    } else {
	set expgui(FileMenuCnvName) [string trim [$files get $select]]
    }
    if {$expgui(FileMenuCnvName) == "<Parent>"} {
	set expgui(FileMenuDir) [file dirname $expgui(FileMenuDir)]
	ChooseWinCnv $frm
    } elseif [file isdirectory \
	    [file join [set expgui(FileMenuDir)] $expgui(FileMenuCnvName)]] {
	if {$expgui(FileMenuCnvName) != "."} {
	    set expgui(FileMenuDir) [file join $expgui(FileMenuDir) $expgui(FileMenuCnvName)]
	    ChooseWinCnv $frm
	}
    }
    return
}

# select a file or directory -- called on double click
proc SelectWinCnv {frm} {
    global expgui
    set files $frm.1.a.files
    set dates $frm.1.a.dates
    set select [$files curselection]
    if {$select == ""} {
	set select [$dates curselection]
    }
    if {$select == ""} {
	set file .
    } else {
	set file [string trim [$files get $select]]
    }
    if {$file == "<Parent>"} {
	set expgui(FileMenuDir) [file dirname [set expgui(FileMenuDir)] ]
	ChooseWinCnv $frm
    } elseif [file isdirectory [file join [set expgui(FileMenuDir)] $file]] {
	if {$file != "."} {
	    set expgui(FileMenuDir) [file join [set expgui(FileMenuDir)] $file]
	    ChooseWinCnv $frm
	}
    } else {
	set expgui(FileMenuCnvName) [file tail $file]
	ValidWinCnv $frm
    }
}

# fill the files & dates & Directory selection box with current directory,
# also called when box is created to fill it
proc ChooseWinCnv {frm} {
    global expgui
    set files $frm.1.a.files
    set dates $frm.1.a.dates
    set expgui(FileMenuCnvName) {}
    $files delete 0 end
    $dates delete 0 end
    $files insert end {<Parent>}
    $dates insert end {(Directory)}
    set filelist [glob -nocomplain \
	    [file join [set expgui(FileMenuDir)] *] ]
    foreach file [lsort -dictionary $filelist] {
	if {[file isdirectory $file]} {
	    $files insert end [file tail $file]
	    $dates insert end {(Directory)}
	}
    }
    foreach file [lsort -dictionary $filelist] {
	if {![file isdirectory $file]} {
	    set modified [clock format [file mtime $file] -format "%T %D"]
	    $files insert end [file tail $file]
	    $dates insert end $modified
	}
    }
    $expgui(FileDirButtonMenu)	delete 0 end
    set list ""
    global tcl_version
    if {$tcl_version > 8.0} {
	catch {set list [string tolower [file volume]]}
    }
    set dir ""
    foreach subdir [file split [set expgui(FileMenuDir)]] {
	set dir [string tolower [file join $dir $subdir]]
	if {[lsearch $list $dir] == -1} {lappend list $dir}
    }
    foreach path $list {
	$expgui(FileDirButtonMenu) add command -label $path \
		-command "[list set expgui(FileMenuDir) $path]; \
		ChooseWinCnv $frm"
    }
    return
}

#------------------------------------------------------------------------------
# set options for liveplot
proc liveplotopt {} {
    global liveplot expmap
    set frm .file
    catch {destroy $frm}
    toplevel $frm
    pack [frame [set frmA $frm.1] -bd 2 -relief groove] -padx 3 -pady 3 -side left
    set last [lindex [lsort -integer $expmap(powderlist)] end]
    if {$last == ""} {set last 1}
    pack [scale  $frmA.1 -label "Histogram number" -from 1 -to $last \
	    -length  150 -orient horizontal -variable liveplot(hst)] -side top
    pack [checkbutton $frmA.2 -text {include plot legend}\
	    -variable liveplot(legend)] -side top
    pack [button $frm.2 -text OK \
	    -command {if ![catch {expr $liveplot(hst)}] "destroy .file"} \
	    ] -side top
    bind $frm <Return> {if ![catch {expr $liveplot(hst)}] "destroy .file"}
    
    # force the window to stay on top
    putontop $frm 
    focus $frm.2
    tkwait window $frm
    afterputontop
}

#------------------------------------------------------------------------------
# get an experiment file name
#------------------------------------------------------------------------------
proc getExpFileName {mode} {
    global expgui tcl_platform
    set frm .file
    catch {destroy $frm}
    toplevel $frm
    wm title $frm "Experiment file"
    bind $frm <Key-F1> "MakeWWWHelp expguierr.html open"
    pack [frame [set frmA $frm.1] -bd 2 -relief groove] -padx 3 -pady 3 -side left
    pack [frame [set frmC $frm.3] ] -padx 3 -pady 3 -side left \
	    -fill y -expand yes
    pack [button $frmC.help -text Help -bg yellow \
	    -command "MakeWWWHelp expguierr.html open"] \
	    -side top -anchor e
    pack [label $frmC.2 -text "Sort .EXP files by" ] -side top
    pack [radiobutton $frmC.1 -text "File Name" -value 1 \
	    -variable expgui(filesort) -command "ChooseExpFil $frmA"] -side top
    pack [radiobutton $frmC.0 -text "Mod. Date" -value 0 \
	    -variable expgui(filesort) -command "ChooseExpFil $frmA"] -side top

    set expgui(includearchived) 0
    set expgui(FileInfoBox) $frmC.info
    if {$mode == "old"} {
	pack [checkbutton $frmC.ar -text "Include Archived Files" \
		-variable expgui(includearchived) \
		-command "ChooseExpFil $frmA"] -side top -pady 10
	pack [frame $expgui(FileInfoBox) -bd 4 -relief groove \
		-class SmallFont] \
		-side top -fill both -expand yes -pady 5
    } elseif {$mode != "new"} {
	# for initial read, don't access archived files
	pack [frame $expgui(FileInfoBox) -bd 4 -relief groove \
		-class SmallFont] \
		-side top -fill both -expand yes -pady 5
	set mode "old"
    }
    pack [button $frmC.b -text Read \
	    -command "valid_exp_file $frmA $mode"] -side bottom
    if {$mode == "new"} {
	$frmC.b config -text Save
    }
    pack [button $frmC.q -text Quit \
	    -command "set expgui(FileMenuEXPNAM) {}; destroy $frm"] -side bottom
    bind $frm <Return> "$frmC.b invoke"

    if {$mode == "new"} {
	pack [label $frmA.0 -text "Enter an experiment file to create"] \
		-side top -anchor center
    } else {
	pack [label $frmA.0 -text "Select an experiment file to read"] \
		-side top -anchor center
    }
    expfilebox $frmA $mode
    # force the window to stay on top
    putontop $frm
    focus $frmC.b
    tkwait window $frm
    afterputontop
    if {$expgui(FileMenuEXPNAM) == ""} return
    # is there a space in the EXP name?
    if {[string first " " [file tail $expgui(FileMenuEXPNAM)]] != -1} {
	update
	MyMessageBox -parent . -title "File Name Error" \
	    -message "File name \"$expgui(FileMenuEXPNAM)\" is invalid -- EXPGUI cannot process experiment files with spaces in the name" \
	    -icon warning -type Continue -default continue
#		-helplink "expguierr.html OpenErr"
	return
    }
    if {[string first " " $expgui(FileMenuDir)] != -1} {
	update
	MyMessageBox -parent . -title "Good luck..." \
	    -message "You are using a directory with a space in the name ([file dirname $expgui(FileMenuDir)]) -- You may encounter bugs in EXPGUI. Please e-mail them to Brian.Toby@NIST.gov so they can be fixed." \
	    -icon warning -type Continue -default continue
#		-helplink "expguierr.html OpenErr"
    }
    return [file join $expgui(FileMenuDir) $expgui(FileMenuEXPNAM)]
}

# validation routine
proc valid_exp_file {frm mode} {
    global expgui tcl_platform
    # windows fixes
    if {$tcl_platform(platform) == "windows"} {
	# change backslashes to something sensible
	regsub -all {\\} $expgui(FileMenuEXPNAM) / expgui(FileMenuEXPNAM)
	# allow entry of D: for D:/ and D:TEST for d:/TEST
	if {[string first : $expgui(FileMenuEXPNAM)] != -1 && \
		[string first :/ $expgui(FileMenuEXPNAM)] == -1} {
	    regsub : $expgui(FileMenuEXPNAM) :/ expgui(FileMenuEXPNAM)
	}
    }
    if {$expgui(FileMenuEXPNAM) == "<Parent>"} {
	set expgui(FileMenuDir) [file dirname [set expgui(FileMenuDir)] ]
	ChooseExpFil $frm
	return
    } elseif [file isdirectory \
	    [file join $expgui(FileMenuDir) $expgui(FileMenuEXPNAM)]] {
	if {$expgui(FileMenuEXPNAM) != "."} {
	    set expgui(FileMenuDir) \
		[file join $expgui(FileMenuDir) $expgui(FileMenuEXPNAM)]
	}
	ChooseExpFil $frm
	return
    }
    # append a .EXP if not present
    if {[file extension $expgui(FileMenuEXPNAM)] == ""} {
	append expgui(FileMenuEXPNAM) ".EXP"
    }
    # is there a space in the name?
    if {[string first " " $expgui(FileMenuEXPNAM)] != -1} {
	MyMessageBox -parent . -title "File Name Error" \
		-message "File name $expgui(FileMenuEXPNAM) is invalid -- EXPGUI cannot process experiment files with spaces in the name" \
		-icon warning -type Continue -default continue
#		-helplink "expguierr.html OpenErr"
	return
    }
    # check for archive files
    if {[string match {*.O[0-9A-F][0-9A-F]} $expgui(FileMenuEXPNAM)] && \
	    $mode == "old" && [file exists $expgui(FileMenuEXPNAM)]} {
	destroy .file
	return
    } elseif {[string toupper [file extension $expgui(FileMenuEXPNAM)]] != ".EXP"} {
	# check for files that end in something other than .EXP .exp or .Exp...
	MyMessageBox -parent . -title "File Open Error" \
		-message "File [file tail $expgui(FileMenuEXPNAM)] is not a valid name. Experiment files must end in \".EXP\"" \
		-icon error
	return
    }
    # check on the file status
    set file [file join $expgui(FileMenuDir) $expgui(FileMenuEXPNAM)]
    if {$mode == "new" && [file exists $file]} {
	set ans [
	MyMessageBox -parent . -title "File Open Error" \
		-message "File [file tail $file] already exists in [file dirname $file]. OK to overwrite?" \
		-icon question -type {"Select other" "Overwrite"} -default "select other" \
		-helplink "expguierr.html OverwriteErr"
	]
	if {[string tolower $ans] == "overwrite"} {destroy .file}
	return
    }
    # if file does not exist in case provided, set the name to all
    # upper case letters, since that is the best choice.
    # if it does exist, read from it as is. For UNIX we will force uppercase later.
    if {![file exists $file]} {
	set expgui(FileMenuEXPNAM) [string toupper $expgui(FileMenuEXPNAM)]
	set file [file join $expgui(FileMenuDir) $expgui(FileMenuEXPNAM)]
    }
    if {$mode == "old" && ![file exists $file]} {
	set ans [
	MyMessageBox -parent . -title "File Open Error" \
		-message "File [file tail $file] does not exist in [file dirname $file]. OK to create?" \
		-icon question -type {"Select other" "Create"} -default "select other" \
		-helplink "expguierr.html OpenErr"
	]
	if {[string tolower $ans] == "create"} {destroy .file}
	return
    }
    destroy .file
}

proc updir {} {
    global expgui
    set expgui(FileMenuDir) [file dirname [set expgui(FileMenuDir)]]
}

# create a file box
proc expfilebox {bx mode} {
    global expgui
    pack [frame $bx.top] -side top 
    pack [label $bx.top.a -text "Directory" ] -side left 
    set expgui(FileDirButtonMenu) [tk_optionMenu $bx.top.d expgui(FileMenuDir) [pwd] ]
    pack $bx.top.d -side left
    set expgui(FileMenuDir) [pwd]
    # the icon below is from tk8.0/tkfbox.tcl
    set upfolder [image create bitmap -data {
#define updir_width 28
#define updir_height 16
static char updir_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x80, 0x1f, 0x00, 0x00, 0x40, 0x20, 0x00, 0x00,
   0x20, 0x40, 0x00, 0x00, 0xf0, 0xff, 0xff, 0x01, 0x10, 0x00, 0x00, 0x01,
   0x10, 0x02, 0x00, 0x01, 0x10, 0x07, 0x00, 0x01, 0x90, 0x0f, 0x00, 0x01,
   0x10, 0x02, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01,
   0x10, 0xfe, 0x07, 0x01, 0x10, 0x00, 0x00, 0x01, 0x10, 0x00, 0x00, 0x01,
   0xf0, 0xff, 0xff, 0x01};}]

    pack [button $bx.top.b -image $upfolder \
	    -command "updir; ChooseExpFil $bx" ]
    pack [frame $bx.a -width 200 -height 75] -side top -expand yes -fill both
    listbox $bx.a.files -relief raised -bd 2 \
	    -yscrollcommand "sync2boxesY $bx.a.files $bx.a.dates $bx.a.scroll" \
	    -height 15 -width 0 -exportselection 0 
    listbox $bx.a.dates -relief raised -bd 2 \
	    -yscrollcommand "sync2boxesY $bx.a.dates $bx.a.files $bx.a.scroll" \
	    -height 15 -width 0 -takefocus 0 -exportselection 0 
    scrollbar $bx.a.scroll -command "move2boxesY \" $bx.a.files $bx.a.dates \" "
    ChooseExpFil $bx
    bind $bx.a.files <ButtonRelease-1> "ReleaseExpFil $bx"
    bind $bx.a.dates <ButtonRelease-1> "ReleaseExpFil $bx"
    bind $bx.a.files <Double-1> "SelectExpFil $bx $mode"
    bind $bx.a.dates <Double-1> "SelectExpFil $bx $mode"
    pack $bx.a.scroll -side left -fill y 
    pack $bx.a.files $bx.a.dates -side left -fill both -expand yes
    pack [entry $bx.c -textvariable expgui(FileMenuEXPNAM)] -side top
}
proc sync2boxesX {master slave scroll args} {
    $slave xview moveto [lindex [$master xview] 0]
    eval $scroll set $args
}
proc move2boxesX {boxlist args} {
    foreach listbox $boxlist { 
	eval $listbox xview $args
    }
}
proc sync2boxesY {master slave scroll args} {
    $slave yview moveto [lindex [$master yview] 0]
    eval $scroll set $args
}
proc move2boxesY {boxlist args} {
    foreach listbox $boxlist { 
	eval $listbox yview $args
    }
}

# creates a table that is scrollable in both x and y, use ResizeScrollTable
# to set sizes after gridding the boxes
proc MakeScrollTable {box} {
    grid [label $box.0] -column 0 -row 0
    grid [set tbox [canvas $box.top \
	    -scrollregion {0 0 10 10} \
	    -xscrollcommand "sync2boxesX $box.top $box.can $box.scroll" \
	    -width 10 -height 10]] \
	    -sticky sew -row 0 -column 1
    grid [set sbox [canvas $box.side \
	    -scrollregion {0 0 10 10} \
	    -yscrollcommand "sync2boxesY $box.side $box.can $box.yscroll" \
	    -width 10 -height 10]] \
	    -sticky nes -row 1 -column 0
    grid [set bbox [canvas $box.can \
	    -scrollregion {0 0 10 10} \
	    -yscrollcommand "sync2boxesY $box.can $box.side $box.yscroll" \
	    -xscrollcommand "sync2boxesX $box.can $box.top $box.scroll" \
	    -width 200 -height 200 -bg lightgrey]] \
	    -sticky news -row 1 -column 1
    grid [set sxbox [scrollbar $box.scroll -orient horizontal \
	    -command "move2boxesX \" $box.can $box.top \" "]] \
	    -sticky ew -row 2 -column 1
    grid [set sybox [scrollbar $box.yscroll \
	    -command "move2boxesY \" $box.can $box.side \" "]] \
	    -sticky ns -row 1 -column 2
    frame $tbox.f -bd 0
    $tbox create window 0 0 -anchor nw  -window $tbox.f
    frame $bbox.f -bd 2
    $bbox create window 0 0 -anchor nw  -window $bbox.f
    frame $sbox.f -bd 2 -relief raised
    $sbox create window 0 0 -anchor nw  -window $sbox.f
    grid columnconfig $box 1 -weight 1
    grid rowconfig $box 1 -weight 1
    return [list  $tbox.f  $bbox.f $sbox.f $box.0]
}

proc ResizeScrollTable {box} {
    update idletasks
    for {set i 0} {$i < [lindex [grid size $box.can.f] 0]} {incr i} {
	set x1 [lindex [grid bbox $box.can.f $i 0] 2]
	set x2 [lindex [grid bbox $box.top.f $i 0] 2]
	if {$x2 > $x1} {set x1 $x2}
	grid columnconfigure $box.top.f $i -minsize $x1
	grid columnconfigure $box.can.f $i -minsize $x1
    }
    for {set i 0} {$i < [lindex [grid size $box.can.f] 1]} {incr i} {
	set x1 [lindex [grid bbox $box.can.f 0 $i] 3]
	set x2 [lindex [grid bbox $box.side.f 0 $i] 3]
	if {$x2 > $x1} {set x1 $x2}
	grid rowconfigure $box.can.f $i -minsize $x1
	grid rowconfigure $box.side.f $i -minsize $x1
    }
    update idletasks
    set sizes [grid bbox $box.can.f]
    $box.can config -scrollregion $sizes
    $box.side config -scrollregion $sizes
    $box.top config -scrollregion $sizes
    $box.top config -height [lindex [grid bbox $box.top.f] 3]
    $box.side config -width [lindex [grid bbox $box.side.f] 2]
}
proc ExpandScrollTable {box} {
    # set height & width of central box
    $box.can config -width \
	    [expr [winfo width [winfo toplevel $box]] \
	    - [winfo width $box.side] - [winfo width $box.yscroll]-20]
    $box.can config -height \
	    [expr [winfo height [winfo toplevel $box]] \
	    - [winfo height $box.top] - [winfo height $box.scroll]-25]
}


# support routine for SetHistUseFlags
proc InitHistUseFlags {} {
    global expmap expgui
    for {set i 1} {$i <= $expmap(nhst)} {incr i} {
#	if {[string range $expmap(htype_$i) 0 0] == "P"} {
	    set expgui(useflag_$i) [histinfo $i use]
#	}
    }
}

# show all Powder histograms; set use/do not use flags
proc SetHistUseFlags {} {
    set box .test
    catch {toplevel $box}
    eval destroy [winfo children $box]
    grid [label $box.0 -text "Set histogram \"Use/Do Not Use\" flags" -bg white] -row 0 -column 0 -columnspan 2
    grid [frame $box.a] -row 1 -column 0 -columnspan 2
    grid [button $box.b -text Save -command "destroy $box"] -row 2 -column 0 -sticky e
    grid [button $box.c -text Cancel -command "InitHistUseFlags;destroy $box"] -row 2 -column 1 -sticky w
    grid columnconfig $box 0 -weight 1
    grid columnconfig $box 1 -weight 1
    foreach a [MakeScrollTable $box.a] b {tbox bbox sbox cbox} {set $b $a}
    $cbox config -text "Use\nFlag"
    [winfo parent $bbox] config -height 250 -width 400
    global expmap expgui
    set px 5
    set row -1
    for {set i 1} {$i <= $expmap(nhst)} {incr i} {
	if {[string range $expmap(htype_$i) 2 2] == "T"} {
	    set det [format %8.2f [histinfo $i tofangle]]
	} elseif {[string range $expmap(htype_$i) 2 2] == "C"} {
	    set det [format %8.5f [histinfo $i lam1]]
	} elseif {[string range $expmap(htype_$i) 2 2] == "E"} {
	    set det [format %8.2f [histinfo $i lam1]]
	} else {
	    set det {}
	}
	incr row
#	if {[string range $expmap(htype_$i) 0 0] == "P"} {
	    grid [checkbutton $sbox.$i -text $i -variable expgui(useflag_$i)] -row $row -column 0 
	    set expgui(useflag_$i) [histinfo $i use]
#	}
	grid [label $bbox.0$i \
		-text [string range $expmap(htype_$i) 0 3] \
		] -row $row -column 0 -padx $px
	grid [label $bbox.1$i -text [histinfo $i bank] \
		] -row $row -column 1 -padx $px
	grid [label $bbox.2$i -text $det] -row $row -column 2 -padx $px
	grid [label $bbox.3$i -text [string range [histinfo $i title] 0 66] \
		] -row $row -column 3 -padx $px -sticky ew
    }
    grid [label $tbox.0 -text type -bd 2 -relief raised] -row 0 -column 0 -padx $px
    grid [label $tbox.1 -text bank -bd 2 -relief raised] -row 0 -column 1 -padx $px
    grid [label $tbox.2 -text "ang/wave" -bd 2 -relief raised] -row 0 -column 2 -padx $px
    grid [label $tbox.3 -text "histogram title" -bd 2 -relief raised] -row 0 -column 3 -sticky w -padx $px
    ResizeScrollTable $box.a
    InitHistUseFlags
    putontop $box
    tkwait window $box
    afterputontop
    set prevchages $expgui(changed)
    for {set i 1} {$i <= $expmap(nhst)} {incr i} {
#	if {[string range $expmap(htype_$i) 0 0] == "P"} {
	    if {$expgui(useflag_$i) != [histinfo $i use]} {
		histinfo $i use set $expgui(useflag_$i)
		incr expgui(changed)
	    }
#	}
    }
    if {$prevchages != $expgui(changed)} {
	set msg "You have changed [expr $expgui(changed)-$prevchages] "
	append msg "histogram flag(s). You must run POWPREF "
	append msg "to include/remove these histograms. Do you want to "
	append msg "run POWPREF?"
	set ans [MyMessageBox -parent . -message $msg \
		-title "Process changes?"\
		-helplink "expguierr.html ProcessUse" \
		-default {Run POWPREF} \
		-type {{Run POWPREF} Skip}]
	
	if {$ans == "skip"} {
	    # save and reload the experiment file
	    savearchiveexp
	    loadexp $expgui(expfile)
	} else {
	    # run powpref and force a reload
	    set saveautoload $expgui(autoexpload)
	    set expgui(autoexpload) 1
	    runGSASwEXP powpref
	    set expgui(autoexpload) $saveautoload
	}
    }
}

# set the box or file in the selection window
proc ReleaseExpFil {frm} {
    global expgui
    set files $frm.a.files
    set dates $frm.a.dates
    set select [$files curselection]
    if {$select == ""} {
	set select [$dates curselection]
    }
    if {$select == ""} {
	set expgui(FileMenuEXPNAM) ""
    } else {
	set expgui(FileMenuEXPNAM) [string trim [$files get $select]]
	after idle UpdateInfoBox
    }
    if {$expgui(FileMenuEXPNAM) == "<Parent>"} {
	set expgui(FileMenuDir) [file dirname $expgui(FileMenuDir)]
	ChooseExpFil $frm
    } elseif [file isdirectory \
	    [file join [set expgui(FileMenuDir)] $expgui(FileMenuEXPNAM)]] {
	if {$expgui(FileMenuEXPNAM) != "."} {
	    set expgui(FileMenuDir) [file join $expgui(FileMenuDir) $expgui(FileMenuEXPNAM)]
	    ChooseExpFil $frm
	}
    }
    return
}
proc UpdateInfoBox {} {
    global expgui
    if {![winfo exists $expgui(FileInfoBox)]} return
    eval destroy [winfo children $expgui(FileInfoBox)]
    set file [file join [set expgui(FileMenuDir)] $expgui(FileMenuEXPNAM)]
    if [file isdirectory $file] return
    if [file exists $file] {
	pack [label $expgui(FileInfoBox).1 -text $expgui(FileMenuEXPNAM)] \
		-side top
	catch {
	    set fp [open $file r]
	    global testline
	    set testline [read $fp]
	    close $fp
	    update
	    regexp {GNLS  RUN on (.*) +Total.*run *([0-9]+) } \
		    $testline a last cycles
	    pack [label $expgui(FileInfoBox).2 -justify left \
		    -text "last GENLES run:\n  $last\n  total cycles: $cycles"] \
		-side top -anchor w
	    regexp {REFN GDNFT.*= *([0-9]*\.[0-9]*) +for *([0-9]+) variables} \
		    $testline a chi2 vars
	    pack [frame $expgui(FileInfoBox).3 -class SmallFont] \
		    -side top -anchor w
	    pack [label $expgui(FileInfoBox).3.a -justify left \
		    -text "c" -font symbol] \
		    -side left -anchor w
	    pack [label $expgui(FileInfoBox).3.b -justify left \
		    -text "2: $chi2, $vars vars"] \
		    -side top -anchor w
	    # check first 9 histograms
	    set lbl "h  Rwp     R(F2)"
	    set n 0
	    foreach k {1 2 3 4 5 6 7 8 9} {
		set key "HST  $k"
		append key { RPOWD +([0-9]*\.[0-9]*) }
		set i [regexp $key $testline a Rwp]
		set key "HST  $k"
		append key { R-FAC +[0-9]+ +([0-9]*\.[0-9]*) }
		set j [regexp $key $testline a Rb]
		if {$i || $j} {
		    incr n
		    append lbl "\n$k  "
		    if {$i} {
			append lbl [string range $Rwp 0 5]
		    } else {
			append lbl "    "
		    }
		}
		if {$j} {
		    append lbl " [string range $Rb 0 5]"
		}
		# stick 1st 3 entries in box
		if {$n >= 3} break
	    }
	    pack [label $expgui(FileInfoBox).4 -justify left \
		    -text $lbl] \
		    -side top -anchor w	    
	}
    }
}

# select a file or directory -- called on double click
proc SelectExpFil {frm mode} {
    global expgui
    set files $frm.a.files
    set dates $frm.a.dates
    set select [$files curselection]
    if {$select == ""} {
	set select [$dates curselection]
    }
    if {$select == ""} {
	set file .
    } else {
	set file [string trim [$files get $select]]
    }
    if {$file == "<Parent>"} {
	set expgui(FileMenuDir) [file dirname [set expgui(FileMenuDir)] ]
	ChooseExpFil $frm
    } elseif [file isdirectory [file join [set expgui(FileMenuDir)] $file]] {
	if {$file != "."} {
	    set expgui(FileMenuDir) [file join [set expgui(FileMenuDir)] $file]
	    ChooseExpFil $frm
	}
    } else {
	set expgui(FileMenuEXPNAM) [file tail $file]
	valid_exp_file $frm $mode
    }
}

# fill the files & dates & Directory selection box with current directory,
# also called when box is created to fill it
proc ChooseExpFil {frm} {
    global expgui
    set files $frm.a.files
    set dates $frm.a.dates
    set expgui(FileMenuEXPNAM) {}
    $files delete 0 end
    $dates delete 0 end
    $files insert end {<Parent>}
    $dates insert end {(Directory)}
    set filelist [glob -nocomplain \
	    [file join [set expgui(FileMenuDir)] *] ]
    foreach file [lsort -dictionary $filelist] {
	if {[file isdirectory $file]} {
	    $files insert end [file tail $file]
	    $dates insert end {(Directory)}
	}
    }
    set pairlist {}
    foreach file [lsort -dictionary $filelist] {
	if {![file isdirectory $file]  && \
		[string toupper [file extension $file]] == ".EXP"} {
	    set modified [file mtime $file]
	    lappend pairlist [list $file $modified]
	} elseif {![file isdirectory $file] && $expgui(includearchived) && \
		[string match {*.O[0-9A-F][0-9A-F]} $file]} {
	    set modified [file mtime $file]
	    lappend pairlist [list $file $modified]
	}
    }
    if {$expgui(filesort) == 0} {
	foreach pair [lsort -index 1 -integer -decreasing $pairlist] {
	    set file [lindex $pair 0]
	    set modified [clock format [lindex $pair 1] -format "%T %D"]
	    $files insert end [file tail $file]
	    $dates insert end $modified
	}
    } else {
	foreach pair [lsort -dictionary -index 0 $pairlist] {
	    set file [lindex $pair 0]
	    set modified [clock format [lindex $pair 1] -format "%T %D"]
	    $files insert end [file tail $file]
	    $dates insert end $modified
	}
    }
    $expgui(FileDirButtonMenu)	delete 0 end
    set list ""
    global tcl_platform tcl_version
    if {$tcl_platform(platform) == "windows" && $tcl_version > 8.0} {
	catch {set list [string tolower [file volume]]}
    }
    set dir ""
    foreach subdir [file split [set expgui(FileMenuDir)]] {
	set dir [file join $dir $subdir]
	if {$tcl_platform(platform) == "windows"} {
	    set dir [string tolower $dir]
	    if {[lsearch $list $dir] == -1} {lappend list $dir}
	} else {
	    lappend list $dir
	}
    }
    foreach path $list {
	$expgui(FileDirButtonMenu) add command -label $path \
		-command "[list set expgui(FileMenuDir) $path]; \
		ChooseExpFil $frm"
    }
    # highlight the current experiment -- if present
    for {set i 0} {$i < [$files size]} {incr i} {
	set file [$files get $i]
	if {$expgui(expfile) == [file join $expgui(FileMenuDir) $file]} {
	    $files selection set $i
	}
    }
    return
}


#------------------------------------------------------------------------------
# platform-specific definitions
if {$tcl_platform(platform) == "windows" && $tcl_platform(os) == "Windows 95"} {
    # windows-95, -98 and presumably -me do not allow Tcl/Tk to run the
    # DOS box synchronously, so we create a "lock" file that is deleted
    # at the end of the DOS run so we can tell when the run is done.
    # We create a window to force the deleting of the file so that if
    # the DOS process crashes, the user can continue anyway.
    #
    # procedure to check if the lock file is still there (Win-9x/me only)
    proc checklockfile {file window} {
	if [file exists $file] {
	    after 500 checklockfile $file $window
	} else {
	    catch {destroy $window}
	}
    }
    # this procedure starts the GRWND program, if needed for program $prog
    proc StartGRWND {prog} {
	global expgui
	if {!$expgui(autoGRWND)} return
	# at some point we might want to have a real list
	if {$prog != "genles" && $prog != "powpref"} {
	    # get a list of running jobs
 	    exec [file join $expgui(scriptdir) win9xbin tlist.exe] > tlist.tlist
	    set fp [open tlist.tlist r]
	    set text [read $fp]
	    close $fp
	    file delete -force tlist.tlist
	    # if GRWND.EXE is not currently running, start it
	    if {[lsearch [string toupper $text] GRWND.EXE] == -1} {
		exec [file join $expgui(gsasexe) grwnd.exe] &
		# give grwnd a 1 second head start
		after 1000
	    }
 	}
    }
    # this creates a DOS box to run a program in
    proc forknewterm {title command "wait 1" "scrollbar 1"} {
	global env expgui
	# Windows environment variables
	set env(GSAS) [file nativename $expgui(gsasdir)]
	# PGPLOT_FONT is needed by PGPLOT
	set env(PGPLOT_FONT) [file nativename [file join $expgui(gsasdir) pgl grfont.dat]]
	# this is the number of lines/page in the .LST (etc.) file
	set env(LENPAGE) 60
	set pwd [file nativename [pwd]]
	
	# check the .EXP path -- can DOS use it?
	if {[string first // [pwd]] != -1} {
	    MyMessageBox -parent . -title "Invalid Path" \
		    -message {Error -- Use "Map network drive" to access this directory with a letter (e.g. F:) GSAS can't directly access a network drive} \
		    -icon error -type ok -default ok \
		    -helplink "expgui_Win_readme.html NetPath"
	    return
	}
	if {[info command winutils::shell] == "" && \
		[info command winexec] == ""} {
	    MyMessageBox -parent . -title "Setup error" \
		-message {Error -- Use "Neither WINEXEC not WINTILS were found. Can't do anything!"} \
		-icon error -type darn -default darn \
		-helplink "expgui_Win_readme.html Winexec"
	    return
	}
	# N. B. pause is now hard coded in the .BAT file
	#
	# loop over multiple commands
	foreach cmd $command {
	    # simulate the wait with a lock file
	    if {$wait} {
		if {$expgui(autoiconify)} {wm iconify .}
		# create a blank lock file and a message window
		close [open expgui.lck w]
		toplevel .lock
		grid [button .lock.0 -text Help -bg yellow \
			-command "MakeWWWHelp expguierr.html lock"] \
			-column 1 -row 0
		grid [label .lock.1 \
			-text "Please wait while the GSAS program finishes."] \
			-column 0 -row 0
		grid [label .lock.2 -text \
			"In case a problem occurs, close the DOS box"] \
			-column 0 -columnspan 2 -row 1
		grid [label .lock.3 -text \
			"and press the \"Continue\" button (below)"] \
			-column 0 -columnspan 2 -row 2
		grid [button .lock.b -text "Continue" \
			-command "destroy .lock; wm deiconify ."] \
			-column 0 -columnspan 2 -row 3
		putontop .lock
		update
		checklockfile expgui.lck .lock
	    }
	    # replace the forward slashes with backward
	    regsub -all / $cmd \\ cmd
	    if {[info command winutils::shell] != ""} {
		winutils::shell [file join $expgui(scriptdir) gsastcl.bat] $cmd
	    } else {
		winexec -d [file nativename [pwd]] \
		    [file join $expgui(scriptdir) gsastcl.bat] $cmd
	    }
	    if {$wait} {
		tkwait window .lock
		file delete -force expgui.lck
	    }
	}
	if {$expgui(autoiconify) && $wait} {wm deiconify .}
	# check for changes in the .EXP file immediately
	whenidle
    }
} elseif {$tcl_platform(platform) == "windows"} {
    # now for Windows-NT, where we can run synchronously
    #
    # if path contains space, return its shortname
    proc checkpath {path} {
    	if [regexp \s $path] {
    		return [file nativename [file attrib $path -shortname]]
    	}
    	return [file nativename $path]
    }
    
    # this creates a DOS box to run a program in
    proc forknewterm {title command  "wait 1" "scrollbar 1"} {
	global env expgui
	# Windows environment variables
	set env(GSAS) [file nativename $expgui(gsasdir)]
	# PGPLOT_FONT is needed by PGPLOT
	set env(PGPLOT_FONT) [file nativename [file join $expgui(gsasdir) pgl grfont.dat]]
	# this is the number of lines/page in the .LST (etc.) file
	set env(LENPAGE) 60
	set pwd [file nativename [pwd]]
	# check the path -- can DOS use it?
	if {[string first // [pwd]] != -1} {
	    MyMessageBox -parent . -title "Invalid Path" \
		    -message {Error -- Use "Map network drive" to access this directory with a letter (e.g. F:) GSAS can't directly access a network drive} \
		    -icon error -type ok -default ok \
		    -helplink "expgui_Win_readme.html NetPath"
	    return
	}
	# pause is hard coded in the .BAT file

	if {$wait} {
	    if {$expgui(autoiconify)} {wm iconify .}
	    # create a blank lock file (keep liveplot from running)
	    close [open expgui.lck w]
	    # loop over commands
	    foreach cmd $command {
		# replace the forward slashes with backward
#		regsub -all / $cmd \\ cmd
		if [catch {
			exec $env(COMSPEC) /c "start \
			[checkpath [file join $expgui(scriptdir) gsastcl.bat]] \
			[checkpath [lindex $cmd 0]] [lrange $cmd 1 end]"
		} err_mes] {
			set ans [MyMessageBox -parent . -title "exec error" \
				-message "$err_mes occurred while running:\ngsastcl.bat $cmd" \
				-icon error -type okcancel -default cancel]
			if {$ans == "cancel"} return
		}
	    } 
	    file delete -force expgui.lck
	    if {$expgui(autoiconify)} {wm deiconify .}
	    # check for changes in the .EXP file immediately
	    whenidle
	} else {
	    # loop over commands
	    foreach cmd $command {
		# replace the forward slashes with backward
#		regsub -all / $cmd \\ cmd
		# run in background
		if [catch {
			exec $env(COMSPEC) /c "start \
			[checkpath [file join $expgui(scriptdir) gsastcl.bat]] \
			[checkpath [lindex $cmd 0]] [lrange $cmd 1 end]" &
		} err_mes] {
			set ans [MyMessageBox -parent . -title "exec error" \
				-message "$err_mes occurred while running:\ngsastcl.bat $cmd" \
				-icon error -type okcancel -default cancel]
			if {$ans == "cancel"} return
		}
		 
	    }
	}
    }
} else {
    # this creates a xterm window to run a program in
    proc forknewterm {title command "wait 1" "scrollbar 1"} {
	global env expgui
	# UNIX environment variables
	set env(GSAS) [file nativename $expgui(gsasdir)]
	set env(gsas) [file nativename $expgui(gsasdir)]
	set env(GSASEXE) $expgui(gsasexe)
	set env(ATOMDATA) [file join $expgui(gsasdir) data atmdata.dat]
	set env(ATMXSECT) [file join $expgui(gsasdir) data atmxsect.dat]
	# PGPLOT_DIR is needed by PGPLOT
	set env(PGPLOT_DIR) [file join $expgui(gsasdir) pgl]
	# this is the number of lines/page in the .LST (etc.) file
	set env(LENPAGE) 60
	set termopts {}
	if $env(GSASBACKSPACE) {
	    append termopts \
		    {-xrm "xterm*VT100.Translations: #override\\n <KeyPress>BackSpace: string(\\177)"}
	}
	if $scrollbar {
	    append termopts " -sb"
	} else {
	    append termopts " +sb"
	}
	if {$wait} {
	    set suffix {}
	} else {
	    set suffix {&}
	}
	#
	#if $wait {
	    append command "\; echo -n Press Enter to continue \; read x"
	#}
	if {$wait && $expgui(autoiconify)} {wm iconify .}
	catch {eval exec xterm $termopts -title [list $title] \
		-e /bin/sh -c [list $command] $suffix} errmsg
	if $expgui(debug) {puts "xterm result = $errmsg"}
	if {$wait} {
	    if {$expgui(autoiconify)} {wm deiconify .}
	    # check for changes in the .EXP file immediately
	    whenidle
	}
    }
}

# modify resource fork info for a .EXP file on the Mac
proc MacSetResourceFork {expfile} {
    global expgui tcl_platform
    if {$tcl_platform(os) != "Darwin"} {return}
    set expnative [file nativename $expfile]
    #
    # assign an app to the data file, if the app and the 
    # required tool (Rez) are installed
    set app [file join $expgui(gsasdir) expgui.app]
    if {[file exists /Developer/Tools/Rez]} {
	set RezApp /Developer/Tools/Rez
    } elseif {[file exists [file join $expgui(gsasdir) Rez]]} {
	set RezApp [file join $expgui(gsasdir) Rez]
    } else {
	set RezApp {}
    }
    if {[file exists /Developer/Tools/SetFile]} {
	set SetFileApp /Developer/Tools/SetFile
    } elseif {[file exists [file join $expgui(gsasdir) SetFile]]} {
	set SetFileApp [file join $expgui(gsasdir) SetFile]
    } else {
	set SetFileApp {}
    }
    if {[file exists $app] && $RezApp != ""} {
	# make a resource file
	set l [string length $app]; incr l
	set str "data 'usro' (0) {\n"
	append str {  $"}
	append str [format %.8X $l]
	foreach char [split $app {}] {
           append str [format %.2X [scan $char %c]]    
        }
        append str {00"}
	append str " \t/* ....$app. */\n};"
	set fp [open setapp.r w]
	puts $fp $str
	close $fp
	exec $RezApp setapp.r -o $expnative -a
	file delete -force setapp.r
    }

    # assign an icon to the data file, if it and the required tools exist
    set icon [file join $expgui(gsasdir) gsasicon.r]
    if {[file exists $icon] && $RezApp != "" && $SetFileApp != ""} {
	exec $RezApp [file nativename $icon] -o $expnative -a
	exec $SetFileApp -a C $expnative
    }
}
