# $Id: export_cif.tcl,v 1.1 2004/09/08 14:08:22 toby Exp toby $
# set local variables that define the proc to execute and the menu label
set label "coords-only CIF"
set action exp2cif
# write coordinates in an XML for FOX
proc exp2cif {} {
    global expmap expgui
    # don't bother if there are no phases to write
    if {[llength $expmap(phaselist)] == 0} {
	MyMessageBox -parent . -title "No phases" \
		-message "Sorry, no phases are present to write" \
		-icon warning
	return
    }
    MakeExportBox .export "Export coordinates (only) in CIF" \
	    "MakeWWWHelp expgui.html ExportCIF"
    # force the window to stay on top
    putontop .export
    # Wait for the Write or Quit button to be pressed
    tkwait window .export
    afterputontop
    # test for Quit
    if {$expgui(export_phase) == 0} {return}
    # 
    set phase $expgui(export_phase)
    #------------------------------------------------------------------
    if [catch {
	set filnam [file rootname $expgui(expfile)]_${phase}.cif
	set fp [open $filnam w]
	puts $fp "\# from $expgui(expfile) "
	puts $fp "_audit_creation_date                [clock format [clock seconds] -format "%Y-%m-%dT%T"]"

	set spacegroup [phaseinfo $phase spacegroup]
	# remove final R from rhombohedral space groups
	if {[string toupper [string range $spacegroup end end]] == "R"} {
	    set spacegroup [string range $spacegroup 0 \
				[expr [string length $spacegroup]-2]] 
	}
	puts $fp "_symmetry_space_group_name_H-M      '${spacegroup}'"
	foreach var {a b c} {
	    puts $fp "_cell_length_$var      [phaseinfo $phase $var]"
	}
	foreach var {alpha beta gamma} {
	    puts $fp "_cell_angle_$var      [phaseinfo $phase $var]"
	}
	    
	if {[lindex $expmap(phasetype) [expr {$phase - 1}]] == 4} {
	    set cmd mmatominfo
	} else {
	    set cmd atominfo
	}

	puts $fp "loop_\n  _atom_site_label\n _atom_site_type_symbol"
	puts $fp " _atom_site_fract_x\n _atom_site_fract_y\n _atom_site_fract_z"
	puts $fp " _atom_site_B_iso_or_equiv\n _atom_site_occupancy"

	foreach atom $expmap(atomlist_$phase) {
	    set label [$cmd $phase $atom label]
	    set elem [$cmd $phase $atom type]
	    # remove spaces
	    regsub -all " " $label "" label
	    regsub -all " " $elem "" elem
	    set Biso [expr 8 * 3.14159 * 3.14159 * [$cmd $phase $atom Uiso]]
	    foreach var {x y z frac} {
		set $var  [format %.5f [$cmd $phase $atom $var]]
	    }
	    puts $fp "$label $elem $x $y $z $Biso $frac"
	}
	close $fp
    } errmsg] {
	MyMessageBox -parent . -title "Export error" \
		-message "Export error: $errmsg" -icon warning
    } else {
	MyMessageBox -parent . -title "Done" \
	    -message "File [file tail $filnam] was written"
    }
}

