# $Id: readexp.tcl,v 1.46 2005/09/13 17:44:33 toby Exp toby $
# Routines to deal with the .EXP "data structure"
set expmap(Revision) {$Revision: 1.46 $ $Date: 2005/09/13 17:44:33 $}

#  The GSAS data is read from an EXP file.
#   ... reading an EXP file into an array
# returns -1 on error
# returns 0 if the file is old-style UNIX format (no CR/LF)
# returns 1 if the file is 80 char/line + cr/lf
# returns 2 if the file is sequential but not fixed-record length
proc expload {expfile} {
    global exparray tcl_platform
    # $expfile is the path to the data file. 

    if [catch {set fil [open "$expfile" r]}] {
	tk_dialog .expFileErrorMsg "File Open Error" \
		"Unable to open file $expfile" error 0 "Exit" 
	return -1
    }
    fconfigure $fil -translation lf
    set len [gets $fil line]
    if {[string length $line] != $len} {
	tk_dialog .expConvErrorMsg "old tcl" \
		"You are using an old version of Tcl/Tk and your .EXP file has binary characters; run convstod or upgrade" \
		error 0 "Exit"
	return -1
    }
    catch {
	unset exparray
    }
    if {$len > 160} {
	set fmt 0
	# a UNIX-type file
	set i1 0
	set i2 79
	while {$i2 < $len} {
	    set nline [string range $line $i1 $i2]
	    incr i1 80
	    incr i2 80
	    set key [string range $nline 0 11]
	    set exparray($key) [string range $nline 12 end]
	}
    } else {
	set fmt 1
	while {$len > 0} {
	    set key [string range $line 0 11]
	    set exparray($key) [string range $line 12 79]
	    if {$len != 81 || [string range $line end end] != "\r"} {set fmt 2}
	    set len [gets $fil line]
	}
    }
    close $fil
    return $fmt
}

proc createexp {expfile title} {
    global exparray expmap
    catch {unset exparray}
    foreach key   {"     VERSION" "      DESCR" "ZZZZZZZZZZZZ" " EXPR NPHAS"} \
	    value {"   6"         ""            "  Last EXP file record" ""} {
	# truncate long keys & pad short ones
	set key [string range "$key        " 0 11]
	set exparray($key) $value
    }
    expinfo title set $title
    exphistory add " created readexp.tcl [lindex $expmap(Revision) 1] [clock format [clock seconds] -format %Y-%m-%dT%T]"
    expwrite $expfile
}

# get information out from an EXP file
#   creates the following entries in global array expmap
#     expmap(phaselist)     gives a list of defined phases
#     expmap(phasetype)     gives the phase type for each defined phase
#                           =1 nuclear; 2 mag+nuc; 3 mag; 4 macro
#     expmap(atomlist_$p)   gives a list of defined atoms in phase $p
#     expmap(htype_$n)      gives the GSAS histogram type for histogram (all)
#     expmap(powderlist)    gives a list of powder histograms in use
#     expmap(phaselist_$n)  gives a list of phases used in histogram $n
#     expmap(nhst)          the number of GSAS histograms
#
proc mapexp {} {
    global expmap exparray
    # clear out the old array
    set expmap_Revision $expmap(Revision)
    unset expmap
    set expmap(Revision) $expmap_Revision
    # apply any updates to the .EXP file
    updateexpfile
    # get the defined phases
    set line [readexp " EXPR NPHAS"]
#    if {$line == ""} {
#	set msg "No EXPR NPHAS entry. This is an invalid .EXP file"
#	tk_dialog .badexp "Error in EXP" $msg error 0 Exit 
#	destroy .
#    }
    set expmap(phaselist) {}
    set expmap(phasetype) {}
    # loop over phases
    foreach iph {1 2 3 4 5 6 7 8 9} {
	set i5s [expr {($iph - 1)*5}]
	set i5e [expr {$i5s + 4}]
	set flag [string trim [string range $line $i5s $i5e]]
	if {$flag == ""} {set flag 0}
	if $flag {
	    lappend expmap(phaselist) $iph
	    lappend expmap(phasetype) $flag
	}
    }
    # get the list of defined atoms for each phase
    foreach iph $expmap(phaselist) {
	set expmap(atomlist_$iph) {}
	if {[lindex $expmap(phasetype) [expr {$iph - 1}]] != 4} {
	    foreach key [array names exparray "CRS$iph  AT*A"] {
		regexp { AT *([0-9]+)A} $key a num
		lappend expmap(atomlist_$iph) $num
	    }
	} else {
	    foreach key [array names exparray "CRS$iph  AT*"] {
		scan [string range $key 8 11] %x atm
		lappend expmap(atomlist_$iph) $atm
	    }
	}
	# note that sometimes an .EXP file contains more atoms than are actually defined
	# drop the extra ones
	set expmap(atomlist_$iph) [lsort -integer $expmap(atomlist_$iph)]
	set natom [phaseinfo $iph natoms]
	if {$natom != [llength $expmap(atomlist_$iph)]} {
	    set expmap(atomlist_$iph) [lrange $expmap(atomlist_$iph) 0 [expr {$natom-1}]]
	}
    }
    # now get the histogram types
    set expmap(nhst) [string trim [readexp { EXPR  NHST }]]
    set n 0
    set expmap(powderlist) {}
    for {set i 0} {$i < $expmap(nhst)} {incr i} {
	set ihist [expr {$i + 1}]
	if {[expr {$i % 12}] == 0} {
	    incr n
	    set line [readexp " EXPR  HTYP$n"]
	    if {$line == ""} {
		set msg "No HTYP$n entry for Histogram $ihist. This is an invalid .EXP file"
		tk_dialog .badexp "Error in readexp" $msg error 0 Exit 
	    }
	    set j 0
	} else {
	    incr j
	}
	set expmap(htype_$ihist) [string range $line [expr 2+5*$j] [expr 5*($j+1)]]
	# is this a dummy histogram?
	if {$ihist <=9} {
	    set key "HST  $ihist DUMMY"
	} else {
	    set key "HST $ihist DUMMY"
	}
	# at least for now, ignore non-powder histograms
	if {[string range $expmap(htype_$ihist) 0 0] == "P" && \
		[string range $expmap(htype_$ihist) 3 3] != "*"} {
	    if {[existsexp $key]} {
		set expmap(htype_$ihist) \
			[string range $expmap(htype_$ihist) 0 2]D
	    }
	    lappend expmap(powderlist) $ihist
	}
    }

    # now process powder histograms
    foreach ihist $expmap(powderlist) {
	# make a 2 digit key -- hh
	if {$ihist < 10} {
	    set hh " $ihist"
	} else {
	    set hh $ihist
	}
	set line [readexp "HST $hh NPHAS"]
	if {$line == ""} {
	    set msg "No NPHAS entry for Histogram $ihist. This is an invalid .EXP file"
	    tk_dialog .badexp "Error in readexp" $msg error 0 Exit 
	}
	set expmap(phaselist_$ihist) {}
	# loop over phases
	foreach iph {1 2 3 4 5 6 7 8 9} {
	    set i5s [expr {($iph - 1)*5}]
	    set i5e [expr {$i5s + 4}]
	    set flag [string trim [string range $line $i5s $i5e]]
	    if {$flag == ""} {set flag 0}
	    if $flag {lappend expmap(phaselist_$ihist) $iph}
	}
    }
}

# this routine is called to update changes in the .EXP file 
proc updateexpfile {} {
    global exparray
    # change "CRSx  ODFxxx" records to "CRSx  OD xxx" records
    # needed by the June 8, 2005 GSAS release
    set ODFlist [array names exparray "CRS?  ODF*"]
    if {[llength $ODFlist] > 0} {
	catch {incr ::expgui(changed)}
	foreach key $ODFlist {
	    regsub "ODF" $key "OD " newkey
	    set exparray($newkey) $exparray($key)
	    unset exparray($key)
	}
    }
}


# return the value for a ISAM key
proc readexp {key} {
    global exparray
    # truncate long keys & pad short ones
    set key [string range "$key        " 0 11]
    if [catch {set val $exparray($key)}] {
	global expgui
	if $expgui(debug) {puts "Error accessing record $key"}
	return ""
    }
    return $val
}

# return the number of records matching ISAM key (may contain wildcards)
proc existsexp {key} {
    global exparray
    # key can contain wild cards so don't pad
    return [llength [array names exparray  $key]]
}


# replace a section of the exparray with $value 
#   replace $char characters starting at character $start (numbered from 1)
proc setexp {key value start chars} {
    global exparray
    # truncate long keys & pad short ones
    set key [string range "$key        " 0 11]
    if [catch {set exparray($key)}] {
	global expgui
	if $expgui(debug) {puts "Error accessing record $key"}
	return ""
    }

    # pad value to $chars 
    set l0 [expr {$chars - 1}]
    set value [string range "$value                                           " 0 $l0]

    if {$start == 1} {
	set ret {}
	set l1 $chars
    } else {
	set l0 [expr {$start - 2}]
	set l1 [expr {$start + $chars - 1}]
	set ret [string range $exparray($key) 0 $l0]
    }
    append ret $value [string range $exparray($key) $l1 end]
    set exparray($key) $ret
}

proc makeexprec {key} {
    global exparray
    # truncate long keys & pad short ones
    set key [string range "$key        " 0 11]
    if [catch {set exparray($key)}] {
	# set to 68 blanks
	set exparray($key) [format %68s " "]
    }
}

# delete an exp record
# returns 1 if OK; 0 if not found
proc delexp {key} {
    global exparray
    # truncate long keys & pad short ones
    set key [string range "$key        " 0 11]
    if [catch {unset exparray($key)}] {
	return 0
    }
    return 1
}

# test an argument if it is a valid number; reform the number to fit
proc validreal {val length decimal} {
    upvar $val value
    # is this a number?
    if [catch {expr {$value}}] {return 0}
    if [catch {
	# how many digits are needed to the left of the decimal?
	set sign 0
	if {$value > 0} {
	    set digits [expr {1 + int(log10($value))}]
	} elseif {$value < 0} {
	    set digits [expr {1 + int(log10(-$value))}]
	    set sign 1
	} else {
	    set digits 1
	}
	if {$digits + $sign >= $length} {
	    # the number is much too big -- use exponential notation
	    set decimal [expr {$length - 6 - $sign}]
	    # drop more decimal places, as needed
	    set tmp [format "%${length}.${decimal}E" $value]
	    while {[string length $tmp] > $length && $decimal >= 0} {
		incr decimal -1
		set tmp [format "%${length}.${decimal}E" $value]
	    }
	} elseif {$digits + $sign >= $length - $decimal} {
	    # we will have to trim the number of decimal digits
	    set decimal [expr {$length - $digits - $sign - 1}]
	    set tmp [format "%#.${decimal}f" $value]
	} elseif {abs($value) < pow(10,2-$decimal) && $length > 6} {
	    # for small values, switch to exponential notation (2-$decimal -> three sig figs.)
	    set decimal [expr {$length - 6 - $sign}]
	    # drop more decimal places, as needed
	    set tmp [format "%${length}.${decimal}E" $value]
	    while {[string length $tmp] > $length && $decimal >= 0} {
		incr decimal -1
		set tmp [format "%${length}.${decimal}E" $value]
	    }
	} else {
	    # use format as specified
	    set tmp [format "%${length}.${decimal}f" $value]
	}
	set value $tmp
    } errmsg] {return 0}
    return 1
}

# test an argument if it is a valid integer; reform the number into
# an integer, if appropriate -- be sure to pass the name of the variable not the value
proc validint {val length} {
    upvar $val value
    # FORTRAN type assumption: blank is 0
    if {$value == ""} {set value 0}
    if [catch {
	set tmp [expr {round($value)}]
	if {$tmp != $value} {return 0}
	set value [format "%${length}d" $tmp]
    }] {return 0}
    return 1
}

# process history information
#    action == last 
#       returns number and value of last record
#    action == add
#
proc exphistory {action "value 0"} {
    global exparray
    if {$action == "last"} {
	set key [lindex [lsort -decreasing [array names exparray *HSTRY*]] 0]
	if {$key == ""} {return ""}
	return [list [string trim [string range $key 9 end]] $exparray($key)]
    } elseif {$action == "add"} {
	set key [lindex [lsort -decreasing [array names exparray *HSTRY*]] 0]
	if {$key == ""} {
	    set index 1
	} else {
	    set index [string trim [string range $key 9 end]]
	    if {$index != "***"} {
		if {$index < 999} {incr index}
		set key [format "    HSTRY%3d" $index]
		set exparray($key) $value
	    }
	}
	set key [format "    HSTRY%3d" $index]
	set exparray($key) $value
    }
}
# get overall info 
#   parm:
#     print     -- GENLES print option (*)
#     cycles    -- number of GENLES cycles (*)
#     title     -- the overall title (*)
#     convg     -- convergence criterion: -200 to 200 (*)
#     marq      -- Marquardt damping factor: 1.0 to 9.99 (*)
#     mbw       -- LS matrix bandwidth; =0 for full matrix (*)
proc expinfo {parm "action get" "value {}"} {
    switch ${parm}-$action {
	title-get {
	    return [string trim [readexp "      DESCR"]]
	}
	title-set {
	    setexp "      DESCR" "  $value" 2 68
	}
	cycles-get {
	    return [string trim [cdatget MXCY]]
	}
	cycles-set {
	    if ![validint value 1] {return 0}
	    cdatset MXCY [format %4d $value]
	}
	cyclesrun-get {
	    set cycle -1
	    regexp {.*cycles run *([0-9]*) } [readexp "  GNLS  RUN"] x cycle
	    return $cycle
	}
	print-get {
	    set print [string trim [cdatget PRNT]]
	    if {$print != ""} {return $print}
	    return 0
	}
	print-set {
	    if ![validint value 1] {return 0}
	    cdatset PRNT [format %4d $value]
	}
	convg-get {
	    set cvg [string trim [cdatget CVRG]]
	    if {$cvg == ""} {return -200}
	    if [catch {expr {$cvg}}] {return -200}
	    return $cvg
	}
	convg-set {
	    if ![validint value 1] {return 0}
	    set value [expr {-200>$value?-200:$value}]
	    set value [expr {200<$value?200:$value}]
	    cdatset CVRG [format %4d $value]
	}
	marq-get {
	    set mar [string trim [cdatget MARQ]]
	    if {$mar == ""} {return 1.0}
	    if [catch {expr $mar}] {return 1.}
	    return $mar
	}
	marq-set {
	    if [catch {
		set value [expr {1.0>$value?1.0:$value}]
		set value [expr {9.99<$value?9.99:$value}]
	    }] {return 0}
	    if ![validreal value 4 2] {return 0}
	    cdatset MARQ $value
	}
	mbw-get {
	    set mbw [string trim [cdatget MBW]]
	    if {$mbw == ""} {return 0}
	    if [catch {expr $mbw}] {return 0}
	    return $mbw
	}
	mbw-set {
	    if ![validint value 1] {return 0}
	    if {$value < 0} {return 0}
	    cdatset MBW [format %5d $value]
	}
	default {
	    set msg "Unsupported expinfo access: parm=$parm action=$action"
	    tk_dialog .badexp "Error in readexp" $msg error 0 Exit 
	}
    }
    return 1
}

proc cdatget {key} {
    foreach i {1 2 3 4 5 6 7 8 9} {
	if {[existsexp "  GNLS CDAT$i"] == 0} break
	set line [readexp "  GNLS CDAT$i"]
	if {$line == {}} break
	foreach i1 {2 10 18 26 34 42 50 58 66} \
		i2 {9 17 25 33 41 49 57 65 73} {
	    set item [string range $line $i1 $i2]
	    if {[string trim $item] == {}} continue
	    if [regexp "${key}(.*)" $item a b] {return $b}
	}
    }
    return {}
}

proc cdatset {key value} {
    # round 1 see if we can find the string
    foreach i {1 2 3 4 5 6 7 8 9} {
	set line [readexp "  GNLS CDAT$i"]
	if {$line == {}} break
	foreach i1 {2 10 18 26 34 42 50 58 66} \
		i2 {9 17 25 33 41 49 57 65 73} {
	    set item [string range $line $i1 $i2]
	    if {[string trim $item] == {}} continue
	    if [regexp "${key}(.*)" $item a b] {
		# found it now replace it
		incr i1
		setexp "  GNLS CDAT$i" "${key}${value}" $i1 8
		return
	    }
	}
    }
    # not found, take the 1st blank space, creating a card if needed
    foreach i {1 2 3 4 5 6 7 8 9} {
	set line [readexp "  GNLS CDAT$i"]
	if {$line == {}} {makeexprec "  GNLS CDAT$i"}
	foreach i1 {2 10 18 26 34 42 50 58 66} \
		i2 {9 17 25 33 41 49 57 65 73} {
	    set item [string range $line $i1 $i2]
	    if {[string trim $item] == {}} {
		# found a blank space: now replace it
		incr i1
		setexp "  GNLS CDAT$i" "${key}${value}" $i1 8
		return
	    }
	}
    }
    return {}
}

# get phase information: phaseinfo phase parm action value
#   phase: 1 to 9 (as defined)
#   parm:
#     name -- phase name
#     natoms -- number of atoms (*)
#     a b c alpha beta gamma -- cell parameters (*)
#     cellref -- refinement flag for the unit cell(*)
#     celldamp  -- damping for the unit cell refinement (*)
#     spacegroup -- space group symbol (*)
#     ODForder -- spherical harmonic order (*)
#     ODFsym   -- sample symmetry (0-3) (*)
#     ODFdampA -- damping for angles (*)
#     ODFdampC -- damping for coefficients (*)
#     ODFomega -- omega oriention angle (*)
#     ODFchi -- chi oriention angle (*)
#     ODFphi -- phi oriention angle (*)
#     ODFomegaRef -- refinement flag for omega (*)
#     ODFchiRef -- refinement flag for chi (*)
#     ODFphiRef -- refinement flag for phi (*)
#     ODFterms -- a list of the {l m n} values for each ODF term (*)
#     ODFcoefXXX -- the ODF coefficient for for ODF term XXX (*)
#     ODFRefcoef -- refinement flag for ODF terms (*)
#  action: get (default) or set
#  value: used only with set
#  * =>  read+write supported
proc phaseinfo {phase parm "action get" "value {}"} {
    switch -glob ${parm}-$action {

	name-get {
	    return [string trim [readexp "CRS$phase    PNAM"]]
	}

	name-set {
	    setexp "CRS$phase    PNAM" " $value" 2 68
	}

	spacegroup-get {
	    return [string trim [readexp "CRS$phase  SG SYM"]]
	}

	spacegroup-set {
	    setexp "CRS$phase  SG SYM" " $value" 2 68
	}

	natoms-get {
	    return [string trim [readexp "CRS$phase   NATOM"]]	    
	}

	natoms-set {
	    if ![validint value 5] {return 0}
	    setexp "CRS$phase   NATOM" $value 1 5
	}

	a-get {
	   return [string trim [string range [readexp "CRS$phase  ABC"] 0 9]]
	}
	b-get {
	   return [string trim [string range [readexp "CRS$phase  ABC"] 10 19]]
	}
	c-get {
	   return [string trim [string range [readexp "CRS$phase  ABC"] 20 29]]
	}
	alpha-get {
	   return [string trim [string range [readexp "CRS$phase  ANGLES"] 0 9]]
	}
	beta-get {
	   return [string trim [string range [readexp "CRS$phase  ANGLES"] 10 19]]
	}
	gamma-get {
	   return [string trim [string range [readexp "CRS$phase  ANGLES"] 20 29]]
	}

	a-set {
	    if ![validreal value 10 6] {return 0}
	    setexp "CRS$phase  ABC" $value 1 10 	    
	}
	b-set {
	    if ![validreal value 10 6] {return 0}
	    setexp "CRS$phase  ABC" $value 11 10 	    
	}
	c-set {
	    if ![validreal value 10 6] {return 0}
	    setexp "CRS$phase  ABC" $value 21 10 	    
	}
	alpha-set {
	    if ![validreal value 10 4] {return 0}
	    setexp "CRS$phase  ANGLES" $value 1 10 	    
	}
	beta-set {
	    if ![validreal value 10 4] {return 0}
	    setexp "CRS$phase  ANGLES" $value 11 10 	    
	}
	gamma-set {
	    if ![validreal value 10 4] {return 0}
	    setexp "CRS$phase  ANGLES" $value 21 10 	    
	}
	cellref-get {
	    if {[string toupper [string range [readexp "CRS$phase  ABC"] 34 34]] == "Y"} {
		return 1
	    }
	    return 0
	}
	cellref-set {
	    if $value {
		setexp "CRS$phase  ABC" "Y" 35 1
	    } else {
		setexp "CRS$phase  ABC" "N" 35 1
	    } 	    
	}
	celldamp-get {
	    set val [string range [readexp "CRS$phase  ABC"] 39 39]
	    if {$val == " "} {return 0}
	    return $val
	}
	celldamp-set {
	    setexp "CRS$phase  ABC" $value 40 1
	}

	ODForder-get {
	    set val [string trim [string range [readexp "CRS$phase  OD "] 0 4]]
	    if {$val == ""} {return 0}
	    return $val
	}
	ODForder-set {
	    if ![validint value 5] {return 0}
	    setexp "CRS$phase  OD " $value 1 5
	}
	ODFsym-get {
	    set val [string trim [string range [readexp "CRS$phase  OD "] 10 14]]
	    if {$val == " "} {return 0}
	    return $val
	}
	ODFsym-set {
	    if ![validint value 5] {return 0}
	    setexp "CRS$phase  OD " $value 11 5
	}
	ODFdampA-get {
	    set val [string range [readexp "CRS$phase  OD "] 24 24]
	    if {$val == " "} {return 0}
	    return $val
	}
	ODFdampA-set {
	    setexp "CRS$phase  OD " $value 25 1
	}
	ODFdampC-get {
	    set val [string range [readexp "CRS$phase  OD "] 29 29]
	    if {$val == " "} {return 0}
	    return $val
	}
	ODFdampC-set {
	    setexp "CRS$phase  OD " $value 30 1
	}
	ODFomegaRef-get {
	    if {[string toupper [string range [readexp "CRS$phase  OD "] 16 16]] == "Y"} {
		return 1
	    }
	    return 0
	}
	ODFomegaRef-set {
	    if $value {
		setexp "CRS$phase  OD " "Y" 17 1
	    } else {
		setexp "CRS$phase  OD " "N" 17 1
	    } 	    
	}
	ODFchiRef-get {
	    if {[string toupper [string range [readexp "CRS$phase  OD "] 17 17]] == "Y"} {
		return 1
	    }
	    return 0
	}
	ODFchiRef-set {
	    if $value {
		setexp "CRS$phase  OD " "Y" 18 1
	    } else {
		setexp "CRS$phase  OD " "N" 18 1
	    } 	    
	}
	ODFphiRef-get {
	    if {[string toupper [string range [readexp "CRS$phase  OD "] 18 18]] == "Y"} {
		return 1
	    }
	    return 0
	}
	ODFphiRef-set {
	    if $value {
		setexp "CRS$phase  OD " "Y" 19 1
	    } else {
		setexp "CRS$phase  OD " "N" 19 1
	    } 	    
	}
	ODFcoef*-get {
	    regsub ODFcoef $parm {} term
	    set k [expr {($term+5)/6}]
	    if {$k <= 9} {
		set k "  $k"
	    } elseif {$k <= 99} {
		set k " $k"
	    }
	    set j [expr {(($term-1) % 6)+1}]
	    set lineB [readexp "CRS$phase  OD${k}B"]
	    set j0 [expr { ($j-1) *10}]
	    set j1 [expr {$j0 + 9}]
	    set val [string trim [string range $lineB $j0 $j1]]
	    if {$val == ""} {return 0.0}
	    return $val
	}
	ODFcoef*-set {
	    regsub ODFcoef $parm {} term
	    if ![validreal value 10 3] {return 0}
	    set k [expr {($term+5)/6}]
	    if {$k <= 9} {
		set k "  $k"
	    } elseif {$k <= 99} {
		set k " $k"
	    }
	    set j [expr {(($term-1) % 6)+1}]
	    set col [expr { ($j-1)*10 + 1}]
	    setexp "CRS$phase  OD${k}B" $value $col 10
	}
	ODFRefcoef-get {
	    if {[string toupper [string range [readexp "CRS$phase  OD "] 19 19]] == "Y"} {
		return 1
	    }
	    return 0
	}
	ODFRefcoef-set {
	    if $value {
		setexp "CRS$phase  OD " "Y" 20 1
	    } else {
		setexp "CRS$phase  OD " "N" 20 1
	    } 	    
	}
	ODFomega-get {
	   return [string trim [string range [readexp "CRS$phase  OD "] 30 39]]
	}
	ODFchi-get {
	   return [string trim [string range [readexp "CRS$phase  OD "] 40 49]]
	}
	ODFphi-get {
	   return [string trim [string range [readexp "CRS$phase  OD "] 50 59]]
	}
	ODFomega-set {
	    if ![validreal value 10 4] {return 0}
	    setexp "CRS$phase  OD " $value 31 10
	}
	ODFchi-set {
	    if ![validreal value 10 4] {return 0}
	    setexp "CRS$phase  OD " $value 41 10
	}
	ODFphi-set {
	    if ![validreal value 10 4] {return 0}
	    setexp "CRS$phase  OD " $value 51 10
	}

	ODFterms-get {
	    set vallist {}
	    set val [string trim [string range [readexp "CRS$phase  OD "] 5 9]]
	    for {set i 1} {$i <= $val} {incr i 6} {
		set k [expr {1+($i-1)/6}]
		if {$k <= 9} {
		    set k "  $k"
		} elseif {$k <= 99} {
		    set k " $k"
		}
		set lineA [readexp "CRS$phase  OD${k}A"]
		set k 0
		for {set j $i} {$j <= $val && $j < $i+6} {incr j} {
		    set j0 [expr {($k)*10}]
		    set j1 [expr {$j0 + 9}]
		    lappend vallist [string trim [string range $lineA $j0 $j1]]
		    incr k
		}
	    }
	    return $vallist
	}
	ODFterms-set {
	    set key "CRS$phase  OD    "
	    if {![existsexp $key]} {
		makeexprec $key
		set oldlen 0
	    } else {
		set oldlen [string trim [string range [readexp $key] 5 9]]
	    }
	    set len [llength $value]
	    if ![validint len 5] {return 0}
	    setexp $key $len 6 5
	    set j 0
	    set k 0
	    foreach item $value {
		incr j
		if {$j % 6 == 1} {
		    incr k
		    if {$k <= 9} {
			set k "  $k"
		    } elseif {$k <= 99} {
			set k " $k"
		    }
		    set col 1
		    set keyA "CRS$phase  OD${k}A"
		    set keyB "CRS$phase  OD${k}B"
		    if {![existsexp $keyA]} {
			makeexprec $keyA
			makeexprec $keyB
		    }
		}
		set col1 [expr {$col + 1}]
		foreach n [lrange $item 0 2] {
		    if ![validint n 3] {return 0}
		    setexp $keyA $n $col1 3
		    incr col1 3
		}
		incr col 10
	    }
	    for {incr j} {$j <= $oldlen} {incr j} {
		if {$j % 6 == 1} {
		    incr k
		    if {$k <= 9} {
			set k "  $k"
		    } elseif {$k <= 99} {
			set k " $k"
		    }
		    set col 1
		    set keyA "CRS$phase  OD${k}A"
		    set keyB "CRS$phase  OD${k}B"
		    delexp $keyA
		    delexp $keyB
		}
		if {[existsexp $keyA]} {
		    setexp $keyA "          " $col 10
		    setexp $keyB "          " $col 10
		}
		incr col 10
	    }
	}

	default {
	    set msg "Unsupported phaseinfo access: parm=$parm action=$action"
	    tk_dialog .badexp "Error in readexp" $msg error 0 Exit 
	}
    }
    return 1
}


# get atom information: atominfo phase atom parm action value
#   phase: 1 to 9 (as defined)
#   atom: a valid atom number [see expmap(atomlist_$phase)]
#      Note that atom and phase can be paired lists, but if there are extra
#      entries in the atoms list, the last phase will be repeated.
#      so that atominfo 1 {1 2 3} xset 1 
#               will set the xflag for atoms 1-3 in phase 1
#      but atominfo {1 2 3} {1 1 1} xset 1 
#               will set the xflag for atoms 1 in phase 1-3
#   parm:
#     type -- element code
#     mult -- atom multiplicity
#     label -- atom label (*)
#     x y z -- coordinates (*)
#     frac --  occupancy (*)
#     temptype -- I or A for Isotropic/Anisotropic (*)
#     Uiso  -- Isotropic temperature factor (*)
#     U11  -- Anisotropic temperature factor (*)
#     U22  -- Anisotropic temperature factor (*)
#     U33  -- Anisotropic temperature factor (*)
#     U12  -- Anisotropic temperature factor (*)
#     U13  -- Anisotropic temperature factor (*)
#     U23  -- Anisotropic temperature factor (*)
#     xref/xdamp -- refinement flag/damping value for the coordinates (*)
#     uref/udamp -- refinement flag/damping value for the temperature factor(s)  (*)
#     fref/fdamp -- refinement flag/damping value for the occupancy (*)
#  action: get (default) or set
#  value: used only with set
#  * =>  read+write supported
proc atominfo {phaselist atomlist parm "action get" "value {}"} {
    foreach phase $phaselist atom $atomlist {
	if {$phase == ""} {set phase [lindex $phaselist end]}
	if {$atom < 10} {
	    set key "CRS$phase  AT  $atom"
	} elseif {$atom < 100} {
	    set key "CRS$phase  AT $atom"
	} else {
	    set key "CRS$phase  AT$atom"
	}
	switch -glob ${parm}-$action {
	    type-get {
		return [string trim [string range [readexp ${key}A] 2 9] ]
	    }
	    mult-get {
		return [string trim [string range [readexp ${key}A] 58 61] ]
	    }
	    label-get {
		return [string trim [string range [readexp ${key}A] 50 57] ]
	    }
	    label-set {
		setexp ${key}A $value 51 8
	    }
	    temptype-get {
		return [string trim [string range [readexp ${key}B] 62 62] ]
	    }
	    temptype-set {
		if {$value == "A"} {
		    setexp ${key}B A 63 1
		    # copy the Uiso to the diagonal terms
		    set Uiso [string range [readexp ${key}B] 0 9]
		    foreach value [CalcAniso $phase $Uiso] \
			    col {1 11 21 31 41 51} {
			validreal value 10 6
			setexp ${key}B $value $col 10
		    }
		} else {
		    setexp ${key}B I 63 1
		    set value 0.0
		    catch {
			# get the trace
			set value [expr {( \
				[string range [readexp ${key}B] 0 9] + \
				[string range [readexp ${key}B] 10 19] + \
				[string range [readexp ${key}B] 20 29])/3.}]
		    }
		    validreal value 10 6
		    setexp ${key}B $value 1 10
		    # blank out the remaining terms
		    set value " "
		    setexp ${key}B $value 11 10
		    setexp ${key}B $value 21 10
		    setexp ${key}B $value 31 10
		    setexp ${key}B $value 41 10
		    setexp ${key}B $value 51 10
		}
	    }
	    x-get {
		return [string trim [string range [readexp ${key}A] 10 19] ]
	    }
	    x-set {
		if ![validreal value 10 6] {return 0}
		setexp ${key}A $value 11 10
	    }
	    y-get {
		return [string trim [string range [readexp ${key}A] 20 29] ]
	    }
	    y-set {
		if ![validreal value 10 6] {return 0}
		setexp ${key}A $value 21 10
	    }
	    z-get {
		return [string trim [string range [readexp ${key}A] 30 39] ]
	    }
	    z-set {
		if ![validreal value 10 6] {return 0}
		setexp ${key}A $value 31 10
	    }
	    frac-get {
		return [string trim [string range [readexp ${key}A] 40 49] ]
	    }
	    frac-set {
		if ![validreal value 10 6] {return 0}
		setexp ${key}A $value 41 10
	    }
	    U*-get {
		regsub U $parm {} type
		if {$type == "iso" || $type == "11"} {
		    return [string trim [string range [readexp ${key}B] 0 9] ]
		} elseif {$type == "22"} {
		    return [string trim [string range [readexp ${key}B] 10 19] ]
		} elseif {$type == "33"} {
		    return [string trim [string range [readexp ${key}B] 20 29] ]
		} elseif {$type == "12"} {
		    return [string trim [string range [readexp ${key}B] 30 39] ]
		} elseif {$type == "13"} {
		    return [string trim [string range [readexp ${key}B] 40 49] ]
		} elseif {$type == "23"} {
		    return [string trim [string range [readexp ${key}B] 50 59] ]
		}
	    }
	    U*-set {
		if ![validreal value 10 6] {return 0}
		regsub U $parm {} type
		if {$type == "iso" || $type == "11"} {
		    setexp ${key}B $value 1 10
		} elseif {$type == "22"} {
		    setexp ${key}B $value 11 10
		} elseif {$type == "33"} {
		    setexp ${key}B $value 21 10
		} elseif {$type == "12"} {
		    setexp ${key}B $value 31 10
		} elseif {$type == "13"} {
		    setexp ${key}B $value 41 10
		} elseif {$type == "23"} {
		    setexp ${key}B $value 51 10
		}
	    }
	    xref-get {
		if {[string toupper [string range [readexp ${key}B] 64 64]] == "X"} {
		    return 1
		}
		return 0
	    }
	    xref-set {
		if $value {
		    setexp ${key}B "X" 65 1
		} else {
		    setexp ${key}B " " 65 1
		} 	    
	    }
	    xdamp-get {
		set val [string range [readexp ${key}A] 64 64]
		if {$val == " "} {return 0}
		return $val
	    }
	    xdamp-set {
		setexp ${key}A $value 65 1
	    }
	    fref-get {
		if {[string toupper [string range [readexp ${key}B] 63 63]] == "F"} {
		    return 1
		}
		return 0
	    }
	    fref-set {
		if $value {
		    setexp ${key}B "F" 64 1
		} else {
		    setexp ${key}B " " 64 1
		} 	    
	    }
	    fdamp-get {
		set val [string range [readexp ${key}A] 63 63]
		if {$val == " "} {return 0}
		return $val
	    }
	    fdamp-set {
		setexp ${key}A $value 64 1
	    }

	    uref-get {
		if {[string toupper [string range [readexp ${key}B] 65 65]] == "U"} {
		    return 1
		}
		return 0
	    }
	    uref-set {
		if $value {
		    setexp ${key}B "U" 66 1
		} else {
		    setexp ${key}B " " 66 1
		} 	    
	    }
	    udamp-get {
		set val [string range [readexp ${key}A] 65 65]
		if {$val == " "} {return 0}
		return $val
	    }
	    udamp-set {
		setexp ${key}A $value 66 1
	    }
	    default {
		set msg "Unsupported atominfo access: parm=$parm action=$action"
		tk_dialog .badexp "Error in readexp" $msg error 0 Exit 
	    }
	}
    }
    return 1
}

# get macromolecular atom information: mmatominfo phase atom parm action value
#   phase: 1 (at present only one mm phase can be defined)
#   atom: a valid atom number [see expmap(atomlist_$phase)]
#      Note that atoms can be lists
#      so that mmatominfo 1 {1 2 3} xset 1 
#               will set the xflag for atoms 1-3 in phase 1
#   parm:
#     type -- element code
#     frac --  occupancy (*)
#     x y z -- coordinates (*)
#     Uiso  -- Isotropic temperature factor (*)
#     label -- atom label (*)
#     residue -- residue label (*)
#     group -- group label (*)
#     resnum -- residue number (*)
#     xref/xdamp -- refinement flag/damping value for the coordinates (*)
#     uref/udamp -- refinement flag/damping value for the temperature factor(s)  (*)
#     fref/fdamp -- refinement flag/damping value for the occupancy (*)
#  action: get (default) or set
#  value: used only with set
#  * =>  read+write supported
proc mmatominfo {phaselist atomlist parm "action get" "value {}"} {
    foreach phase $phaselist atom $atomlist {
	if {$phase == ""} {set phase [lindex $phaselist end]}
	set num [string toupper [format %.4x $atom]]
	set key "CRS$phase  AT$num"
	switch -glob ${parm}-$action {
	    type-get {
		return [string trim [string range [readexp ${key}] 2 9] ]
	    }
	    frac-get {
		return [string trim [string range [readexp ${key}] 10 15] ]
	    }
	    frac-set {
		if ![validreal value 6 4] {return 0}
		setexp ${key} $value 11 6
	    }
	    x-get {
		return [string trim [string range [readexp ${key}] 16 23] ]
	    }
	    x-set {
		if ![validreal value 8 5] {return 0}
		setexp ${key} $value 17 8
	    }
	    y-get {
		return [string trim [string range [readexp ${key}] 24 31] ]
	    }
	    y-set {
		if ![validreal value 8 5] {return 0}
		setexp ${key} $value 25 8
	    }
	    z-get {
		return [string trim [string range [readexp ${key}] 32 39] ]
	    }
	    z-set {
		if ![validreal value 8 5] {return 0}
		setexp ${key} $value 33 8
	    }
	    Uiso-get {
		return [string trim [string range [readexp ${key}] 40 45] ]
	    }
	    Uiso-set {
		if ![validreal value 6 4] {return 0}
		setexp ${key} $value 41 6
	    }
	    label-get {
		return [string trim [string range [readexp ${key}] 46 50] ]
	    }
	    label-set {
		setexp ${key} $value 47 5
	    }
	    residue-get {
		return [string range [readexp ${key}] 51 53]
	    }
	    residue-set {
		setexp ${key} $value 52 3
	    }
	    group-get {
		return [string range [readexp ${key}] 54 55]
	    }
	    group-set {
		setexp ${key} $value 55 2
	    }
	    resnum-get {
		return [string trim [string range [readexp ${key}] 56 59] ]
	    }
	    resnum-set {
		if ![validint value 4] {return 0}
		setexp "${key} EPHAS" $value 57 4
	    }
	    fref-get {
		if {[string toupper [string range [readexp $key] 60 60]] == "F"} {
		    return 1
		}
		return 0
	    }
	    fref-set {
		if $value {
		    setexp $key "F" 61 1
		} else {
		    setexp $key " " 61 1
		} 	    
	    }
	    xref-get {
		if {[string toupper [string range [readexp $key] 61 61]] == "X"} {
		    return 1
		}
		return 0
	    }
	    xref-set {
		if $value {
		    setexp $key "X" 62 1
		} else {
		    setexp ${key}B " " 62 1
		} 	    
	    }
	    uref-get {
		if {[string toupper [string range [readexp $key] 62 62]] == "U"} {
		    return 1
		}
		return 0
	    }
	    uref-set {
		if $value {
		    setexp $key "U" 63 1
		} else {
		    setexp $key " " 63 1
		} 	    
	    }

	    fdamp-get {
		set val [string range [readexp ${key}] 63 63]
		if {$val == " "} {return 0}
		return $val
	    }
	    fdamp-set {
		setexp ${key} $value 64 1
	    }
	    xdamp-get {
		set val [string range [readexp ${key}] 64 64]
		if {$val == " "} {return 0}
		return $val
	    }
	    xdamp-set {
		setexp ${key} $value 65 1
	    }

	    udamp-get {
		set val [string range [readexp ${key}] 65 65]
		if {$val == " "} {return 0}
		return $val
	    }
	    udamp-set {
		setexp ${key} $value 66 1
	    }
	    default {
		set msg "Unsupported mmatominfo access: parm=$parm action=$action"
		tk_dialog .badexp "Error in readexp" $msg error 0 Exit 
	    }
	}
    }
    return 1
}



# get histogram information: histinfo histlist parm action value
# histlist is a list of histogram numbers
# parm:
#     title
#     file  -- file name of raw data for histogram (*)
#     scale (*)
#     sref/sdamp -- refinement flag/damping value for the scale factor (*)
#     lam1, lam2 (*)
#     ttref refinement flag for the 2theta (ED Xray) (*)
#     wref refinement flag for the wavelength (*)
#     ratref refinement flag for the wavelength ratio (*)
#     difc, difa -- TOF calibration constants (*)
#     dcref,daref -- refinement flag for difc, difa (*)
#     zero (*)
#     zref refinement flag for the zero correction (*)
#     ipola (*)
#     pola (*)
#     pref refinement flag for the polarization (*)
#     kratio (*)
#     ddamp -- damping value for the diffractometer constants (*)
#     backtype -- background function number *
#     backterms -- number of background terms *
#     bref/bdamp -- refinement flag/damping value for the background (*)
#     bterm$n -- background term #n (*)
#     bank -- Bank number
#     tofangle -- detector angle (TOF only)
#     foextract  -- Fobs extraction flag (*)
#     LBdamp  -- LeBail damping value (*)
#     tmin/tmax -- minimum & maximum usable 2theta/TOF/energy
#     excl -- excluded regions (*)
#     dmin -- minimum d-space for reflection generation (*)
#     use  -- use flag; 1 = use; 0 = do not use (*)
#     dstart -- dummy histogram starting tmin/emin/2theta (*)
#     dstep -- dummy histogram step size tmin/emin/2theta (*)
#     dpoints -- dummy histogram number of points (*)
#     dtype   -- dummy histogram type (CONST or SLOG)
#     abscor1 -- 1st absorption correction (*)
#     abscor2 -- 2nd absorption correction (*)
#     abstype -- absorption correction type (*)
#     absdamp -- damping for absorption refinement (*)
#     absref -- refinement damping for absorption refinement (*)
#   parameters transferred from the instrument parameter file:
#     ITYP    -- returns the contents of the ITYP record
proc histinfo {histlist parm "action get" "value {}"} {
    global expgui
    foreach hist $histlist {
	if {$hist < 10} {
	    set key "HST  $hist"
	} else {
	    set key "HST $hist"
	}
	switch -glob ${parm}-$action {
	    foextract-get {
		set line [readexp "${key} EPHAS"]
		# add a EPHAS if not exists
		if {$line == {}} {
		    makeexprec "${key} EPHAS"
		    # expedt defaults this to "F", but I think "T" is better
		    setexp "${key} EPHAS" "Y" 50 1
		    if $expgui(debug) {puts "Warning: creating a ${key} EPHAS record"}
		}
		if {[string toupper [string range $line 49 49]] == "T"} {
		    return 1
		}
		# the flag has changed to "Y/N" in the latest versions
		# of GSAS
		if {[string toupper [string range $line 49 49]] == "Y"} {
		    return 1
		}
		return 0
	    }
	    foextract-set {
		# the flag has changed to "Y/N" in the latest versions
		# of GSAS
		if $value {
		    setexp "${key} EPHAS" "Y" 50 1
		} else {
		    setexp "${key} EPHAS" "N" 50 1
		}
	    }
	    LBdamp-get {
		set v [string trim [string range [readexp "${key} EPHAS"] 54 54]]
		if {$v == ""} {return 0}
		return $v
	    }
	    LBdamp-set {
		if ![validint value 5] {return 0}
		setexp "${key} EPHAS" $value 51 5
	    }
	    title-get {
		return [string trim [readexp "${key}  HNAM"] ]
	    }
	    scale-get {
		return [string trim [string range [readexp ${key}HSCALE] 0 14]]
	    }
	    scale-set {
		if ![validreal value 15 6] {return 0}
		setexp ${key}HSCALE $value 1 15
	    }
	    sref-get {
		if {[string toupper [string range [readexp ${key}HSCALE] 19 19]] == "Y"} {
		    return 1
		}
		return 0
	    }
	    sref-set {
		if $value {
		    setexp ${key}HSCALE "Y" 20 1
		} else {
		    setexp ${key}HSCALE "N" 20 1
		} 	    
	    }
	    sdamp-get {
		set val [string range [readexp ${key}HSCALE] 24 24]
		if {$val == " "} {return 0}
		return $val
	    }
	    sdamp-set {
		setexp ${key}HSCALE $value 25 1
	    }

	    difc-get -
	    lam1-get {
		return [string trim [string range [readexp "${key} ICONS"] 0 9]]
	    }
	    difc-set -
	    lam1-set {
		if ![validreal value 10 7] {return 0}
		setexp "${key} ICONS" $value 1 10
		# set the powpref warning (1 = suggested)
		catch {
		    global expgui
		    if {$expgui(needpowpref) == 0} {set expgui(needpowpref) 1}
		    set msg "Diffractometer constants" 
		    if {[string first $msg $expgui(needpowpref_why)] == -1} {
			append expgui(needpowpref_why) "\t$msg were changed\n"
		    }
		}
	    }
	    difa-get -
	    lam2-get {
		return [string trim [string range [readexp "${key} ICONS"] 10 19]]
	    }
	    difa-set -
	    lam2-set {
		if ![validreal value 10 7] {return 0}
		setexp "${key} ICONS" $value 11 10
		# set the powpref warning (1 = suggested)
		catch {
		    global expgui
		    if {$expgui(needpowpref) == 0} {set expgui(needpowpref) 1}
		    set msg "Diffractometer constants" 
		    if {[string first $msg $expgui(needpowpref_why)] == -1} {
			append expgui(needpowpref_why) "\t$msg were changed\n"
		    }
		}
	    }
	    zero-get {
		return [string trim [string range [readexp "${key} ICONS"] 20 29]]
	    }
	    zero-set {
		if ![validreal value 10 5] {return 0}
		setexp "${key} ICONS" $value 21 10
		# set the powpref warning (1 = suggested)
		catch {
		    global expgui
		    if {$expgui(needpowpref) == 0} {set expgui(needpowpref) 1}
		    set msg "Diffractometer constants" 
		    if {[string first $msg $expgui(needpowpref_why)] == -1} {
			append expgui(needpowpref_why) "\t$msg were changed\n"
		    }
		}
	    }
	    ipola-get {
		return [string trim [string range [readexp "${key} ICONS"] 54 54]]
	    }
	    ipola-set {
		if ![validint value 1] {return 0}
		setexp "${key} ICONS" $value 55 1
	    }
	    pola-get {
		return [string trim [string range [readexp "${key} ICONS"] 40 49]]
	    }
	    pola-set {
		if ![validreal value 10 5] {return 0}
		setexp "${key} ICONS" $value 41 10
	    }
	    kratio-get {
		set val [string trim [string range [readexp "${key} ICONS"] 55 64]]
		if {$val == ""} {set val 0}
		# N.B. this code is used w/CW, where Kratio may not be 0.0
		set lam2 [string trim [string range [readexp "${key} ICONS"] 10 19]]
		if {$lam2 == ""} {set lam2 0}
		# Change kratio & flag the change (this is rather kludged)
		if {$val == 0 && $lam2 != 0} {
		    set val 0.5
		    validreal val 10 5
		    setexp "${key} ICONS" $val 56 10
		    catch {incr ::expgui(changed)}
		}
		return $val
	    }
	    kratio-set {
		if ![validreal value 10 5] {return 0}
		setexp "${key} ICONS" $value 56 10
	    }

	    wref-get {
	    #------------------------------------------------------
	    # col 33: refine flag for lambda, difc, ratio and theta
	    #------------------------------------------------------
		if {[string toupper [string range \
			[readexp "${key} ICONS"] 32 32]] == "L"} {
		    return 1
		}
		return 0
	    }
	    wref-set {
		if $value {
		    setexp "${key} ICONS" "L" 33 1
		} else {
		    setexp "${key} ICONS" " " 33 1
		} 	    
	    }
	    ratref-get {
		if {[string toupper [string range \
			[readexp "${key} ICONS"] 32 32]] == "R"} {
		    return 1
		}
		return 0
	    }
	    ratref-set {
		if $value {
		    setexp "${key} ICONS" "R" 33 1
		} else {
		    setexp "${key} ICONS" " " 33 1
		} 	    
	    }
	    dcref-get {
		if {[string toupper [string range \
			[readexp "${key} ICONS"] 32 32]] == "C"} {
		    return 1
		}
		return 0
	    }
	    dcref-set {
		if $value {
		    setexp "${key} ICONS" "C" 33 1
		} else {
		    setexp "${key} ICONS" " " 33 1
		} 	    
	    }
	    ttref-get {
		if {[string toupper [string range \
			[readexp "${key} ICONS"] 32 32]] == "T"} {
		    return 1
		}
		return 0
	    }
	    ttref-set {
		if $value {
		    setexp "${key} ICONS" "T" 33 1
		} else {
		    setexp "${key} ICONS" " " 33 1
		} 	    
	    }


	    pref-get {
	    #------------------------------------------------------
	    # col 34: refine flag for POLA & DIFA
	    #------------------------------------------------------
		if {[string toupper [string range \
			[readexp "${key} ICONS"] 33 33]] == "P"} {
		    return 1
		}
		return 0
	    }
	    pref-set {
		if $value {
		    setexp "${key} ICONS" "P" 34 1
		} else {
		    setexp "${key} ICONS" " " 34 1
		} 	    
	    }
	    daref-get {
		if {[string toupper [string range \
			[readexp "${key} ICONS"] 33 33]] == "A"} {
		    return 1
		}
		return 0
	    }
	    daref-set {
		if $value {
		    setexp "${key} ICONS" "A" 34 1
		} else {
		    setexp "${key} ICONS" " " 34 1
		} 	    
	    }

	    zref-get {
	    #------------------------------------------------------
	    # col 34: refine flag for zero correction
	    #------------------------------------------------------
		if {[string toupper [string range [readexp "${key} ICONS"] 34 34]] == "Z"} {
		    return 1
		}
		return 0
	    }
	    zref-set {
		if $value {
		    setexp "${key} ICONS" "Z" 35 1
		} else {
		    setexp "${key} ICONS" " " 35 1
		} 	    
	    }

	    ddamp-get {
		set val [string range [readexp "${key} ICONS"] 39 39]
		if {$val == " "} {return 0}
		return $val
	    }
	    ddamp-set {
		setexp "${key} ICONS" $value 40 1
	    }

	    backtype-get {
		set val [string trim [string range [readexp "${key}BAKGD "] 0 4]]
		if {$val == " "} {return 0}
		return $val
	    }
	    backtype-set {
		if ![validint value 5] {return 0}
		setexp "${key}BAKGD " $value 1 5
	    }
	    backterms-get {
		set val [string trim [string range [readexp "${key}BAKGD "] 5 9]]
		if {$val == " "} {return 0}
		return $val
	    }
	    backterms-set {
		# this takes a bit of work -- if terms are added, add lines as needed to the .EXP
		set oldval [string trim [string range [readexp "${key}BAKGD "] 5 9]]
		if ![validint value 5] {return 0}
		if {$oldval < $value} {
		    set line1  [expr {2 + ($oldval - 1) / 4}]
		    set line2  [expr {1 + ($value - 1) / 4}]
		    for {set i $line1} {$i <= $line2} {incr i} {
			# create a blank entry if needed
			makeexprec ${key}BAKGD$i
		    }
		    incr oldval
		    for {set num $oldval} {$num <= $value} {incr num} {
			set f1 [expr {15*(($num - 1) % 4)}]
			set f2 [expr {15*(1 + ($num - 1) % 4)-1}]
			set line  [expr {1 + ($num - 1) / 4}]
			if {[string trim [string range [readexp ${key}BAKGD$line] $f1 $f2]] == ""} {
			    set f1 [expr {15*(($num - 1) % 4)+1}]
			    setexp ${key}BAKGD$line 0.0 $f1 15			
			}
		    }
		}
		setexp "${key}BAKGD " $value 6 5

	    }
	    bref-get {
		if {[string toupper [string range [readexp "${key}BAKGD"] 14 14]] == "Y"} {
		    return 1
		}
		return 0
	    }
	    bref-set {
		if $value {
		    setexp "${key}BAKGD "  "Y" 15 1
		} else {
		    setexp "${key}BAKGD "  "N" 15 1
		}
	    }
	    bdamp-get {
		set val [string range [readexp "${key}BAKGD "] 19 19]
		if {$val == " "} {return 0}
		return $val
	    }
	    bdamp-set {
		setexp "${key}BAKGD " $value 20 1
	    }
	    bterm*-get {
		regsub bterm $parm {} num
		set f1 [expr {15*(($num - 1) % 4)}]
		set f2 [expr {15*(1 + ($num - 1) % 4)-1}]
		set line  [expr {1 + ($num - 1) / 4}]
		return [string trim [string range [readexp ${key}BAKGD$line] $f1 $f2] ]
	    }
	    bterm*-set {
		regsub bterm $parm {} num
		if ![validreal value 15 6] {return 0}
		set f1 [expr {15*(($num - 1) % 4)+1}]
		set line  [expr {1 + ($num - 1) / 4}]
		setexp ${key}BAKGD$line $value $f1 15
	    }
	    bank-get {
		return [string trim [string range [readexp "${key} BANK"] 0 4]]
	    }
	    tofangle-get {
		return [string trim [string range [readexp "${key}BNKPAR"] 10 19]]
	    }
	    tmin-get {
		return [string trim [string range [readexp "${key} TRNGE"] 0 9]]
	    }
	    tmax-get {
		return [string trim [string range [readexp "${key} TRNGE"] 10 19]]
	    }
	    excl-get {
		set n [string trim [string range [readexp "${key} NEXC"] 0 4]]
		set exlist {}
		for {set i 1} {$i <= $n} {incr i} {
		    set line [readexp [format "${key}EXC%3d" $i]]
		    lappend exlist [list \
			    [string trim [string range $line  0  9]] \
			    [string trim [string range $line 10 19]]]
		}
		return $exlist
	    }
	    excl-set {
		set n [llength $value]
		if ![validint n 5] {return 0}
		setexp "${key} NEXC" $n 1 5
		set i 0
		foreach p $value {
		    incr i
		    foreach {r1 r2} $p {}
		    validreal r1 10 3
		    validreal r2 10 3
		    set k [format "${key}EXC%3d" $i]
		    if {![existsexp $k]} {
			makeexprec $k
		    }
		    setexp $k ${r1}${r2} 1 20
		}
		# set the powpref warning (2 = required)
		catch {
		    global expgui
		    set expgui(needpowpref) 2
		    set msg "Excluded regions" 
		    if {[string first $msg $expgui(needpowpref_why)] == -1} {
			append expgui(needpowpref_why) "\t$msg were changed\n"
		    }
		}
	    }
	    file-get {
		return [string trim [readexp "${key}  HFIL"] ]
	    }
	    file-set {
		setexp "${key}  HFIL" $value 3 65
	    }
	    bank-get {
		return [string trim [string range [readexp "${key} BANK"] 0 4]]
	    }
	    dmin-get {
		return [string trim [string range [readexp "${key} NREF"] 5 14]]
	    }
	    dmin-set {
		if ![validreal value 10 4] {return 0}
		setexp "${key} NREF" $value 6 10
		# set the powpref warning (2 = required)
		catch {
		    global expgui
		    set expgui(needpowpref) 2
		    set msg "Dmin (reflection range)" 
		    if {[string first $msg $expgui(needpowpref_why)] == -1} {
			append expgui(needpowpref_why) "\t$msg was changed\n"
		    }
		}
	    }
	    use-get {
		set k [expr {($hist+11)/12}]
		set line [readexp " EXPR  HTYP$k"]
		set j [expr {((($hist-1) % 12)+1)*5}]
		if {[string range $line $j $j] == "*"} {return 0}
		return 1
	    }
	    use-set {
		set k [expr {($hist+11)/12}]
		set line [readexp " EXPR  HTYP$k"]
		set j [expr {((($hist-1) % 12)+1)*5+1}]
		if {$value} {
		    setexp " EXPR  HTYP$k" " " $j 1
		} else {
		    setexp " EXPR  HTYP$k" "*" $j 1
		}
		# set the powpref warning (2 = required)
		catch {
		    global expgui
		    set expgui(needpowpref) 2
		    set msg "Histogram use flags" 
		    if {[string first $msg $expgui(needpowpref_why)] == -1} {
			append expgui(needpowpref_why) "\t$msg were changed\n"
		    }
		}
	    }
	    dstart-get {
		return [string trim [string range [readexp "${key} DUMMY"] 20 29]]
	    }
	    dstart-set {
		if ![validreal value 10 3] {return 0}
		setexp "${key} DUMMY" $value 21 10
		# set the powpref warning (1 = suggested)
		catch {
		    global expgui
		    if {$expgui(needpowpref) == 0} {set expgui(needpowpref) 1}
		    set msg "Dummy histogram parameters" 
		    if {[string first $msg $expgui(needpowpref_why)] == -1} {
			append expgui(needpowpref_why) "\t$msg were changed\n"
		    }
		}
	    }
	    dstep-get {
		return [string trim [string range [readexp "${key} DUMMY"] 30 39]]
	    }
	    dstep-set {
		if ![validreal value 10 3] {return 0}
		setexp "${key} DUMMY" $value 31 10
		catch {
		    global expgui
		    if {$expgui(needpowpref) == 0} {set expgui(needpowpref) 1}
		    set msg "Dummy histogram parameters" 
		    if {[string first $msg $expgui(needpowpref_why)] == -1} {
			append expgui(needpowpref_why) "\t$msg were changed\n"
		    }
		}
	    }
	    dpoints-get {
		return [string trim [string range [readexp "${key} DUMMY"] 0 9]]
	    }
	    dpoints-set {
		if ![validint value 10] {return 0}
		setexp "${key} DUMMY" $value 1 10
		catch {
		    global expgui
		    if {$expgui(needpowpref) == 0} {set expgui(needpowpref) 1}
		    set msg "Dummy histogram parameters" 
		    if {[string first $msg $expgui(needpowpref_why)] == -1} {
			append expgui(needpowpref_why) "\t$msg were changed\n"
		    }
		}
	    }
	    dtype-get {
		return [string trim [string range [readexp "${key} DUMMY"] 10 19]]
	    }
	    abscor1-get {
		return [string trim [string range [readexp "${key}ABSCOR"] 0 14]]
	    }
	    abscor1-set {
		if ![validreal value 15 7] {return 0}
		setexp "${key}ABSCOR" $value 1 15
	    }
	    abscor2-get {
		return [string trim [string range [readexp "${key}ABSCOR"] 15 29]]
	    }
	    abscor2-set {
		# can't use validreal as the decimal must be in col 20
		if {[catch {
		    if {abs($value) < 99.99 && abs($value) > 1.e-4} {
			set tmp [format "%15.10f" $value]
			# make a final check of decimal
			if {[string range $tmp 4 4] != "."} {
			    set tmp [format "%15.6E" $value]
			}
		    } else {
			set tmp [format "%15.6E" $value]
		    }
		}]} {return 0}
		setexp "${key}ABSCOR" $tmp 16 15
	    }
	    abstype-get {
		set val [string trim [string range [readexp "${key}ABSCOR"] 40 44]]
		if {$val == ""} {set val 0}
		return $val
	    }
	    abstype-set {
		if ![validint value 5] {return 0}
		setexp "${key}ABSCOR" $value 41 5
	    }
	    absdamp-get {
		set val [string range [readexp "${key}ABSCOR"] 39 39]
		if {$val == " "} {return 0}
		return $val
	    }
	    absdamp-set {
		if ![validint value 5] {return 0}
		setexp "${key}ABSCOR" $value 36 5
	    }
	    absref-get {
		if {[string toupper \
			[string range [readexp "${key}ABSCOR"] 34 34]] == "Y"} {
		    return 1
		}
		return 0
	    }
	    absref-set {
		if $value {
		    setexp "${key}ABSCOR" "    Y" 31 5
		} else {
		    setexp "${key}ABSCOR" "    N" 31 5
		}
	    }
	    ITYP-get {
		return [string trim [readexp "${key}I ITYP"]]
	    }
	    default {
		set msg "Unsupported histinfo access: parm=$parm action=$action"
		tk_dialog .badexp "Error in readexp" $msg error 0 Exit 
	    }
	}
    }
    return 1
}

# read the information that differs by both histogram and phase (profile & phase fraction)
# use: hapinfo hist phase parm action value
#  
#     frac -- phase fraction (*)
#     frref/frdamp -- refinement flag/damping value for the phase fraction (*)
#     proftype -- profile function number (*)
#     profterms -- number of profile terms (*)
#     pdamp -- damping value for the profile (*)
#     pcut -- cutoff value for the profile (*)
#     pterm$n -- profile term #n (*)
#     pref$n -- refinement flag value for profile term #n (*)
#     extmeth -- Fobs extraction method (*)
#     POnaxis -- number of defined M-D preferred axes
proc hapinfo {histlist phaselist parm "action get" "value {}"} {
    foreach phase $phaselist hist $histlist {
	if {$phase == ""} {set phase [lindex $phaselist end]}
	if {$hist == ""} {set hist [lindex $histlist end]}
	if {$hist < 10} {
	    set hist " $hist"
	}
	set key "HAP${phase}${hist}"
	switch -glob ${parm}-$action {
	    extmeth-get {
		set i1 [expr {($phase - 1)*5}]
		set i2 [expr {$i1 + 4}]
		return [string trim [string range [readexp "HST $hist EPHAS"] $i1 $i2]]
	    }
	    extmeth-set {
		set i1 [expr {($phase - 1)*5 + 1}]
		if ![validint value 5] {return 0}
		setexp "HST $hist EPHAS" $value $i1 5
	    }
	    frac-get {
		return [string trim [string range [readexp ${key}PHSFR] 0 14]]
	    }
	    frac-set {
		if ![validreal value 15 6] {return 0}
		setexp ${key}PHSFR $value 1 15
	    }
	    frref-get {
		if {[string toupper [string range [readexp ${key}PHSFR] 19 19]] == "Y"} {
		    return 1
		}
		return 0
	    }
	    frref-set {
		if $value {
		    setexp ${key}PHSFR "Y" 20 1
		} else {
		    setexp ${key}PHSFR "N" 20 1
		} 	    
	    }
	    frdamp-get {
		set val [string range [readexp ${key}PHSFR] 24 24]
		if {$val == " "} {return 0}
		return $val
	    }
	    frdamp-set {
		setexp ${key}PHSFR $value 25 1
	    }
	    proftype-get {
		set val [string range [readexp "${key}PRCF "] 0 4]
		if {$val == " "} {return 0}
		return $val
	    }
	    proftype-set {
		if ![validint value 5] {return 0}
		setexp "${key}PRCF " $value 1 5
		# set the powpref warning (1 = suggested)
		catch {
		    global expgui
		    if {$expgui(needpowpref) == 0} {set expgui(needpowpref) 1}
		    set msg "Profile parameters" 
		    if {[string first $msg $expgui(needpowpref_why)] == -1} {
			append expgui(needpowpref_why) "\t$msg were changed\n"
		    }
		}
	    }
	    profterms-get {
		set val [string range [readexp "${key}PRCF "] 5 9]
		if {$val == " "} {return 0}
		return $val
	    }
	    profterms-set {
		if ![validint value 5] {return 0}
		setexp "${key}PRCF " $value 6 5
		# now check that all needed entries exist
		set lines [expr {1 + ($value - 1) / 4}]
		for {set i 1} {$i <= $lines} {incr i} {
		    makeexprec "${key}PRCF $i"
		}
		# set the powpref warning (1 = suggested)
		catch {
		    global expgui
		    if {$expgui(needpowpref) == 0} {set expgui(needpowpref) 1}
		    set msg "Profile parameters" 
		    if {[string first $msg $expgui(needpowpref_why)] == -1} {
			append expgui(needpowpref_why) "\t$msg were changed\n"
		    }
		}
	    }
	    pcut-get {
		return [string trim [string range [readexp "${key}PRCF "] 10 19]]
	    }
	    pcut-set {
		if ![validreal value 10 5] {return 0}
		setexp "${key}PRCF " $value 11 10
		# set the powpref warning (1 = suggested)
		catch {
		    global expgui
		    if {$expgui(needpowpref) == 0} {set expgui(needpowpref) 1}
		    set msg "Profile parameters" 
		    if {[string first $msg $expgui(needpowpref_why)] == -1} {
			append expgui(needpowpref_why) "\t$msg were changed\n"
		    }
		}
	    }
	    pdamp-get {
		set val [string range [readexp "${key}PRCF "] 24 24]
		if {$val == " "} {return 0}
		return $val
	    }
	    pdamp-set {
		setexp "${key}PRCF   " $value 25 1
	    }
	    pterm*-get {
		regsub pterm $parm {} num
		set f1 [expr {15*(($num - 1) % 4)}]
		set f2 [expr {15*(1 + ($num - 1) % 4)-1}]
		set line  [expr {1 + ($num - 1) / 4}]
		return [string trim [string range [readexp "${key}PRCF $line"] $f1 $f2] ]
	    }
	    pterm*-set {
		if ![validreal value 15 6] {return 0}
		regsub pterm $parm {} num
		set f1 [expr {1+ 15*(($num - 1) % 4)}]
		set line  [expr {1 + ($num - 1) / 4}]
		setexp "${key}PRCF $line" $value $f1 15
		# set the powpref warning (1 = suggested)
		catch {
		    global expgui
		    if {$expgui(needpowpref) == 0} {set expgui(needpowpref) 1}
		    set msg "Profile parameters" 
		    if {[string first $msg $expgui(needpowpref_why)] == -1} {
			append expgui(needpowpref_why) "\t$msg were changed\n"
		    }
		}
	    }
	    pref*-get {
		regsub pref $parm {} num
		set f [expr {24+$num}]
		if {[string toupper [string range [readexp "${key}PRCF  "] $f $f]] == "Y"} {
		    return 1
		}
		return 0
	    }
	    pref*-set {
		regsub pref $parm {} num
		set f [expr {25+$num}]
		if $value {
		    setexp ${key}PRCF "Y" $f 1
		} else {
		    setexp ${key}PRCF "N" $f 1
		} 	    
	    }
	    POnaxis-get {
		set val [string trim \
			[string range [readexp "${key}NAXIS"] 0 4]]
		if {$val == ""} {return 0}
		return $val
	    }
	    POnaxis-set {
		if ![validint value 5] {return 0}
		# there should be a NAXIS record, but if not make one
		if {![existsexp "${key}NAXIS"]} {
		    makeexprec "${key}NAXIS"
		}
		setexp "${key}NAXIS  " $value 1 5
	    }
	    default {
		set msg "Unsupported hapinfo access: parm=$parm action=$action"
		tk_dialog .badexp "Error in readexp" $msg error 0 Exit 
	    }
	}
    }
    return 1
}

#  get a logical constraint
#
#  type action
#  -----------
#  atom get  number        returns a list of constraints.
#   "   set  number value  replaces a list of constraints 
#                          (value is a list of constraints)
#   "   add  number value  inserts a new list of constraints 
#                          (number is ignored)
#   "   delete number      deletes a set of constraint entries
# Each item in the list of constraints is composed of 4 items: 
#              phase, atom, variable, multiplier
# If variable=UISO atom can be ALL, otherwise atom is a number
# legal variable names: FRAC, X, Y, Z, UISO, U11, U22, U33, U12, U23, U13,
#                       MX, MY, MZ
#
#  type action
#  -----------
#  profileXX get number         returns a list of constraints for term XX=1-36
#                               use number=0 to get # of defined
#                                  constraints for term XX
#   "        set number value   replaces a list of constraints 
#                               (value is a list of constraints)
#   "        add number value   inserts a new list of constraints 
#                               (number is ignored)
#   "        delete number      deletes a set of constraint entries
# Each item in the list of constraints is composed of 3 items: 
#              phase-list, histogram-list, multiplier
# Note that phase-list and/or histogram-list can be ALL

proc constrinfo {type action number "value {}"} {
    global expmap
    if {[lindex $expmap(phasetype) 0] == 4} {
	set mm 1
    } else {
	set mm 0
    }
    switch -glob ${type}-$action {
	atom-get {
	    # does this constraint exist?
	    set key [format "LNCN%4d%4d" $number 1]
	    if {![existsexp $key]} {return -1}
	    set clist {}
	    for {set i 1} {$i < 999} {incr i} {
		set key [format "LNCN%4d%4d" $number $i]
		if {![existsexp $key]} break
		set line [readexp $key]
		set j1 2
		set j2 17
		set seg [string range $line $j1 $j2]
		while {[string trim $seg] != ""} {
		    set p [string range $seg 0 0]
		    if {$p == 1 && $mm} {
			set atom [string trim [string range $seg 1 4]]
			set var [string trim [string range $seg 5 7]]
			if {$atom == "ALL"} {
			    set var UIS
			} else {
			    scan $atom %x atom
			}
			lappend clist [list $p $atom $var \
				[string trim [string range $seg 8 end]]]
		    } else {
			lappend clist [list $p \
				[string trim [string range $seg 1 3]] \
				[string trim [string range $seg 4 7]] \
				[string trim [string range $seg 8 end]]]
		    }
		    incr j1 16
		    incr j2 16
		    set seg [string range $line $j1 $j2]
		}
	    }
	    return $clist
	}
	atom-set {
	    # delete records for current constraint
	    for {set i 1} {$i < 999} {incr i} {
		set key [format "LNCN%4d%4d" $number $i]
		if {![existsexp $key]} break
		delexp $key
	    }
	    set line {}
	    set i 1
	    foreach tuple $value {
		set p [lindex $tuple 0]
 		if {$p == 1 && $mm && \
			[string toupper [lindex $tuple 1]] == "ALL"} {
		    set seg [format %1dALL UIS%8.4f \
			    [lindex $tuple 0] \
			    [lindex $tuple 3]]
		} elseif {$p == 1 && $mm} {
		    set seg [eval format %1d%.4X%-3s%8.4f $tuple]
		} elseif {[string toupper [lindex $tuple 1]] == "ALL"} {
		    set seg [format %1dALL%-4s%8.4f \
			    [lindex $tuple 0] \
			    [lindex $tuple 2] \
			    [lindex $tuple 3]]
		} else {
		    set seg [eval format %1d%3d%-4s%8.4f $tuple]
		}
		append line $seg
		if {[string length $line] > 50} {
		    set key  [format "LNCN%4d%4d" $number $i]
		    makeexprec $key
		    setexp $key $line 3 68
		    set line {}
		    incr i
		}
	    }
	    if {$line != ""} {
		set key  [format "LNCN%4d%4d" $number $i]
		makeexprec $key
		setexp $key $line 3 68
	    }
	    return
	}
	atom-add {
	    # loop over defined constraints
	    for {set j 1} {$j < 9999} {incr j} {
		set key [format "LNCN%4d%4d" $j 1]
		if {![existsexp $key]} break
	    }
	    set number $j
	    # save the constraint 
	    set line {}
	    set i 1
	    foreach tuple $value {
		set p [lindex $tuple 0]
 		if {$p == 1 && $mm && \
			[string toupper [lindex $tuple 1]] == "ALL"} {
		    set seg [format %1dALL UIS%8.4f \
			    [lindex $tuple 0] \
			    [lindex $tuple 3]]
		} elseif {$p == 1 && $mm} {
		    set seg [eval format %1d%.4X%-3s%8.4f $tuple]
		} elseif {[string toupper [lindex $tuple 1]] == "ALL"} {
		    set seg [format %1dALL%-4s%8.4f \
			    [lindex $tuple 0] \
			    [lindex $tuple 2] \
			    [lindex $tuple 3]]
		} else {
		    set seg [eval format %1d%3d%-4s%8.4f $tuple]
		}
		append line $seg
		if {[string length $line] > 50} {
		    set key  [format "LNCN%4d%4d" $number $i]
		    makeexprec $key
		    setexp $key $line 3 68
		    set line {}
		    incr i
		}
	    }
	    if {$line != ""} {
		set key  [format "LNCN%4d%4d" $number $i]
		makeexprec $key
		setexp $key $line 3 68
	    }
	    return
	}
	atom-delete {
	    for {set j $number} {$j < 9999} {incr j} {
		# delete records for current constraint
		for {set i 1} {$i < 999} {incr i} {
		    set key [format "LNCN%4d%4d" $j $i]
		    if {![existsexp $key]} break
		    delexp $key
		}
		# now copy records, from the next entry, if any
		set j1 $j
		incr j1
		set key1 [format "LNCN%4d%4d" $j1 1]
		# if there is no record, there is nothing to copy -- done
		if {![existsexp $key1]} return
		for {set i 1} {$i < 999} {incr i} {
		    set key1 [format "LNCN%4d%4d" $j1 $i]
		    if {![existsexp $key1]} break
		    set key  [format "LNCN%4d%4d" $j  $i]
		    makeexprec $key
		    setexp $key [readexp $key1] 1 68
		}
	    }
	}
	profile*-delete {
	    regsub profile $type {} term
	    if {$term < 10} {
		set term " $term"
	    }
	    set key "LEQV PF$term   "
	    # return nothing if no term exists
	    if {![existsexp $key]} {return 0}

	    # number of constraint terms
	    set nterms [string trim [string range [readexp ${key}] 0 4] ]
	    # don't delete a non-existing entry
	    if {$number > $nterms} {return 0}
	    set val [expr {$nterms - 1}]
	    validint val 5
	    setexp $key $val 1 5
	    for {set i1 $number} {$i1 < $nterms} {incr i1} {
		set i2 [expr {1 + $i1}]
		# move the contents of constraint #i2 -> i1
		if {$i1 > 9} {
		    set k1 [expr {($i1+1)/10}]
		    set l1 $i1
		} else {
		    set k1 " "
		    set l1 " $i1"
		}
		set key1 "LEQV PF$term  $k1"
		# number of constraint lines for #i1
		set n1 [string trim [string range [readexp ${key1}] \
			[expr {($i1%10)*5}] [expr {4+(($i1%10)*5)}]] ]
		if {$i2 > 9} {
		    set k2 [expr {($i2+1)/10}]
		    set l2 $i2
		} else {
		    set k2 " "
		    set l2 " $i2"
		}
		set key2 "LEQV PF$term  $k2"
		# number of constraint lines for #i2
		set n2 [string trim [string range [readexp ${key2}] \
			[expr {($i2%10)*5}] [expr {4+(($i2%10)*5)}]] ]
		set val $n2
		validint val 5
		# move the # of terms
		setexp $key1 $val [expr {1+(($i1%10)*5)}] 5
		# move the terms
		for {set j 1} {$j <= $n2} {incr j 1} {
		    set key "LEQV PF${term}${l1}$j"
		    makeexprec $key
		    setexp $key [readexp "LEQV PF${term}${l2}$j"] 1 68
		}
		# delete any remaining lines
		for {set j [expr {$n2+1}]} {$j <= $n1} {incr j 1} {
		    delexp "LEQV PF${term}${l1}$j"
		}
	    }

	    # clear the last term
	    if {$nterms > 9} {
		set i [expr {($nterms+1)/10}]
	    } else {
		set i " "
	    }
	    set key "LEQV PF$term  $i"
	    set cb [expr {($nterms%10)*5}]
	    set ce [expr {4+(($nterms%10)*5)}]
	    set n2 [string trim [string range [readexp ${key}] $cb $ce] ]
	    incr cb
	    setexp $key "     " $cb 5
	    # delete any remaining lines
	    for {set j 1} {$j <= $n2} {incr j 1} {
		delexp "LEQV PF${term}${nterms}$j"
	    }
	}
	profile*-set {
	    regsub profile $type {} term
	    if {$term < 10} {
		set term " $term"
	    }
	    set key "LEQV PF$term   "
	    # get number of constraint terms
	    set nterms [string trim [string range [readexp ${key}] 0 4] ]
	    # don't change a non-existing entry
	    if {$number > $nterms} {return 0}
	    if {$number > 9} {
		set k1 [expr {($number+1)/10}]
		set l1 $number
	    } else {
		set k1 " "
		set l1 " $number"
	    }
	    set key1 "LEQV PF$term  $k1"
	    # old number of constraint lines
	    set n1 [string trim [string range [readexp ${key1}] \
		    [expr {($number%10)*5}] [expr {4+(($number%10)*5)}]] ]
	    # number of new constraints
	    set j2 [llength $value]
	    # number of new constraint lines
	    set val [set n2 [expr {($j2 + 2)/3}]]
	    # store the new # of lines
	    validint val 5
	    setexp $key1 $val [expr {1+(($number%10)*5)}] 5

	    # loop over the # of lines in the old or new, whichever is greater
	    set v0 0
	    for {set j 1} {$j <= [expr {($n1 > $n2) ? $n1 : $n2}]} {incr j 1} {
		set key "LEQV PF${term}${l1}$j"
		# were there more lines in the old?
		if {$j > $n2} {
		    # this line is not needed
		    if {$j % 3 == 1} {
			delexp %key
		    }
		    continue
		}
		# are we adding new lines?
		if {$j > $n1} {
		    makeexprec $key
		}
		# add the three constraints to the line
		foreach s {3 23 43} \
			item [lrange $value $v0 [expr {2+$v0}]] {
		    if {$item != ""} {
			set val [format %-10s%9.3f \
				[lindex $item 0],[lindex $item 1] \
				[lindex $item 2]]
			setexp $key $val $s 19
		    } else {
			setexp $key " " $s 19
		    }
		}
		incr v0 3
	    }
	}
	profile*-add {
	    regsub profile $type {} term
	    if {$term < 10} {
		set term " $term"
	    }
	    set key "LEQV PF$term   "
	    if {![existsexp $key]} {makeexprec $key}
	    set nterms [string trim [string range [readexp ${key}] 0 4] ]
	    if {$nterms == ""} {
		set nterms 1
	    } elseif {$nterms >= 99} {
		return 0
	    } else {
		incr nterms
	    }
	    # store the new # of constraints
	    set val $nterms
	    validint val 5
	    setexp $key $val 1 5

	    if {$nterms > 9} {
		set k1 [expr {($nterms+1)/10}]
		set l1 $nterms
	    } else {
		set k1 " "
		set l1 " $nterms"
	    }
	    set key1 "LEQV PF$term  $k1"

	    # number of new constraints
	    set j2 [llength $value]
	    # number of new constraint lines
	    set val [set n2 [expr {($j2 + 2)/3}]]
	    # store the new # of lines
	    validint val 5
	    setexp $key1 $val [expr {1+(($nterms%10)*5)}] 5

	    # loop over the # of lines to be added
	    set v0 0
	    for {set j 1} {$j <= $n2} {incr j 1} {
		set key "LEQV PF${term}${l1}$j"
		makeexprec $key
		# add the three constraints to the line
		foreach s {3 23 43} \
			item [lrange $value $v0 [expr {2+$v0}]] {
		    if {$item != ""} {
			set val [format %-10s%9.3f \
				[lindex $item 0],[lindex $item 1] \
				[lindex $item 2]]
			setexp $key $val $s 19
		    } else {
			setexp $key " " $s 19
		    }
		}
		incr v0 3
	    }
	}
	profile*-get {
	    regsub profile $type {} term
	    if {$term < 10} {
		set term " $term"
	    }
	    if {$number > 9} {
		set i [expr {($number+1)/10}]
	    } else {
		set i " "
	    }
	    set key "LEQV PF$term  $i"
	    # return nothing if no term exists
	    if {![existsexp $key]} {return 0}
	    # number of constraint lines
	    
	    set numline [string trim [string range [readexp ${key}] \
		    [expr {($number%10)*5}] [expr {4+(($number%10)*5)}]] ]
	    if {$number == 0} {return $numline}
	    set clist {}
	    if {$number < 10} {
		set number " $number"
	    }
	    for {set i 1} {$i <= $numline} {incr i} {
		set key "LEQV PF${term}${number}$i"
		set line [readexp ${key}]
		foreach s {1 21 41} e {20 40 60} {
		    set seg [string range $line $s $e]
		    if {[string trim $seg] == ""} continue
		    # parse the string segment
		    set parse [regexp { *([0-9AL]+),([0-9AL]+) +([0-9.]+)} \
			    $seg junk phase hist mult]
		    # was parse successful
		    if {!$parse} {continue}
		    lappend clist [list $phase $hist $mult]
		}
	    }
	    return $clist
	}
	default {
	    set msg "Unsupported constrinfo access: type=$type action=$action"
	    tk_dialog .badexp "Error in readexp access" $msg error 0 OK
	}

    }
}

# read the default profile information for a histogram 
# use: profdefinfo hist set# parm action
#  
#     proftype -- profile function number
#     profterms -- number of profile terms
#     pdamp -- damping value for the profile (*)
#     pcut -- cutoff value for the profile (*)
#     pterm$n -- profile term #n
#     pref$n -- refinement flag value for profile term #n (*)

proc profdefinfo {hist set parm "action get"} {
    global expgui
    if {$hist < 10} {
	set key "HST  $hist"
    } else {
	set key "HST $hist"
    }
    switch -glob ${parm}-$action {
	proftype-get {
	    set val [string range [readexp "${key}PRCF$set"] 0 4]
	    if {$val == " "} {return 0}
	    return $val
	}
	profterms-get {
	    set val [string range [readexp "${key}PRCF$set"] 5 9]
	    if {$val == " "} {return 0}
	    return $val
	}
	pcut-get {
	    return [string trim [string range [readexp "${key}PRCF$set"] 10 19]]
	}
	pdamp-get {
		set val [string range [readexp "${key}PRCF$set"] 24 24]
	    if {$val == " "} {return 0}
	    return $val
	}
	pterm*-get {
	    regsub pterm $parm {} num
	    set f1 [expr {15*(($num - 1) % 4)}]
	    set f2 [expr {15*(1 + ($num - 1) % 4)-1}]
	    set line  [expr {1 + ($num - 1) / 4}]
	    return [string trim [string range [\
			readexp "${key}PRCF${set}$line"] $f1 $f2] ]
	}
	pref*-get {
	    regsub pref $parm {} num
	    set f [expr {24+$num}]
	    if {[string toupper [string range [readexp "${key}PRCF$set"] $f $f]] == "Y"} {
		return 1
	    }
	    return 0
	}
	default {
	    set msg "Unsupported profdefinfo access: parm=$parm action=$action"
	    tk_dialog .badexp "Code Error" $msg error 0 Exit 
	}
    }
}

# get March-Dollase preferred orientation information
# use MDprefinfo hist phase axis-number parm action value
#    ratio    -- ratio of xtallites in PO direction vs random (>1 for more)
#    fraction -- fraction in this direction, when more than one axis is used
#    h k & l  -- indices of P.O. axis
#    ratioref -- flag to vary ratio
#    fracref  -- flag to vary fraction
#    damp     -- damping value
#    type     -- model type (0 = P.O. _|_ to beam, 1 = || to beam)
#    new      -- creates a new record with default values (set only)
proc MDprefinfo {histlist phaselist axislist parm "action get" "value {}"} {
    foreach phase $phaselist hist $histlist axis $axislist {
	if {$phase == ""} {set phase [lindex $phaselist end]}
	if {$hist == ""} {set hist [lindex $histlist end]}
	if {$axis == ""} {set axis [lindex $axislist end]}
	if {$hist < 10} {
	    set hist " $hist"
	}
	if {$axis > 9} {
	    set axis "0"
	}
	set key "HAP${phase}${hist}PREFO${axis}"
	switch -glob ${parm}-$action {
	    ratio-get {
 		return [string trim [string range [readexp $key] 0 9]]
	    }
	    ratio-set {
		if ![validreal value 10 6] {return 0}
		setexp $key $value 1 10
	    }
	    fraction-get {
 		return [string trim [string range [readexp $key] 10 19]]
	    }
	    fraction-set {
		if ![validreal value 10 6] {return 0}
		setexp $key $value 11 10
	    }
	    h-get {
		set h [string trim [string range [readexp $key] 20 29]]
		# why not allow negative h values?
		#		if {$h < 1} {return 0}
		return $h
	    }
	    h-set {
		if ![validreal value 10 2] {return 0}
		setexp $key [format %10.5f $value] 21 10
	    }
	    k-get {
		set k [string trim [string range [readexp $key] 30 39]]
		#		if {$k < 1} {return 0} 
		return $k
	    }
	    k-set {
		if ![validreal value 10 2] {return 0}
		setexp $key [format %10.5f $value] 31 10
	    }
	    l-get {
		set l [string trim [string range [readexp $key] 40 49]]
		#if {$l < 1} {return 0}
		return $l
	    }
	    l-set {
		if ![validreal value 10 2] {return 0}
		setexp $key [format %10.5f $value] 41 10
	    }
	    ratioref-get {
		if {[string toupper \
			[string range [readexp $key] 53 53]] == "Y"} {
		    return 1
		}
		return 0
	    }
	    ratioref-set {
		if $value {
		    setexp $key "Y" 54 1
		} else {
		    setexp $key "N" 54 1
		}
	    }
	    fracref-get {
		if {[string toupper \
			[string range [readexp $key] 54 54]] == "Y"} {
		    return 1
		}
		return 0
	    }
	    fracref-set {
		if $value {
		    setexp $key "Y" 55 1
		} else {
		    setexp $key "N" 55 1
	      }
	    }
	    damp-get {
		set val [string trim [string range [readexp $key] 59 59]]
		if {$val == " "} {return 0}
		return $val
	    }
	    damp-set {
		setexp $key $value 60 1
	    }
	    type-get {
		set val [string trim [string range [readexp $key] 64 64]]
		if {$val == " "} {return 0}
		return $val
	    }
	    type-set {
		# only valid settings are 0 & 1
		if {$value != "0" && $value != "1"} {set value "0"}
		setexp $key $value 65 1
	    }
	    new-set {
		makeexprec $key
		setexp $key \
			{  1.000000  1.000000  0.000000  0.000000  1.000000   NN    0    0} \
			1 68
	    }
	    default {
		set msg "Unsupported MDprefinfo access: parm=$parm action=$action"
		tk_dialog .badexp "Error in readexp" $msg error 0 Exit 
	    }

	}

    }
}

# write the .EXP file
proc expwrite {expfile} {
    global exparray
    set blankline \
     "                                                                        "
    set fp [open ${expfile} w]
    fconfigure $fp -translation crlf -encoding ascii
    set keylist [lsort [array names exparray]]
    # reorder the keys so that VERSION comes 1st
    set pos [lsearch -exact $keylist {     VERSION}]
    set keylist "{     VERSION} [lreplace $keylist $pos $pos]"
    foreach key $keylist {
	puts $fp [string range \
		"$key$exparray($key)$blankline" 0 79]
    }
    close $fp
}

# history commands -- delete all but last $keep history records, 
# renumber if $renumber is true
proc DeleteHistory {keep renumber} {
    global exparray
    foreach y [lrange [lsort -decreasing \
	    [array names exparray {    HSTRY*}]] $keep end] {
	unset exparray($y)
    }
    if !$renumber return
    # renumber 
    set i 0
    foreach y [lsort -increasing \
	    [array names exparray {    HSTRY*}]] {
	set key [format "    HSTRY%3d" [incr i]]
	set exparray($key) $exparray($y)
	unset exparray($y)
    }
    # list all history
    #    foreach y [lsort -decreasing [array names exparray {    HSTRY*}]] {puts "$y $exparray($y)"}
}

proc CountHistory {} {
    global exparray
    return [llength [array names exparray {    HSTRY*}]]
}

# set the phase flags for histogram $hist to $plist
proc SetPhaseFlag {hist plist} {
    # make a 2 digit key -- hh
    if {$hist < 10} {
	set hh " $hist"
    } else {
	set hh $hist
    }
    set key "HST $hh NPHAS"
    set str {}
    foreach iph {1 2 3 4 5 6 7 8 9} {
	if {[lsearch $plist $iph] != -1} {
	    append str {    1}
	} else {
	    append str {    0}	    
	}
    }
    setexp $key $str 1 68
}

# erase atom $atom from phase $phase
# update the list of atom types, erasing the record if not needed.
proc EraseAtom {atom phase} {
    set type [atominfo $phase $atom type]
    if {$type == ""} return
    if {$atom < 10} {
	set key "CRS$phase  AT  $atom"
    } elseif {$atom < 100} {
	set key "CRS$phase  AT $atom"
    } else {
	set key "CRS$phase  AT$atom"
    }
    # delete the records for the atom
    global exparray
    foreach k [array names exparray ${key}*] {
	delexp $k
    }
    # change the number of atoms in the phase
    phaseinfo $phase natoms set [expr {[phaseinfo $phase natoms] -1}]

    # now adjust numbers in "EXPR ATYP" records and delete, if needed.
    set natypes [readexp " EXPR  NATYP"]
    if {$natypes == ""} return
    set j 0
    for {set i 1} {$i <= $natypes} {incr i} {
	incr j
	if {$j <10} {
	    set key " EXPR ATYP $j"
	} else {
	    set key " EXPR ATYP$j"
	}
	while {![existsexp $key]} {
	    incr j
	    if {$j > 99} {
		return
	    } elseif {$j <10} {
		set key " EXPR ATYP $j"
	    } else {
		set key " EXPR ATYP$j"
	    }
	}
	set keytype [string trim [string range $exparray($key) 2 9]]
	if {$type == $keytype} {
	    # found the type record
	    set val [string trim [string range $exparray($key) 10 14]]
	    incr val -1
	    # if this is the last reference, remove the record,
	    # otherwise, decrement the counter
	    if {$val <= 0} {
		incr natypes -1 
		validint natypes 5
		setexp " EXPR  NATYP" $natypes 1 5
		delexp $key
	    } else {
		validint val 5
		setexp $key $val 11 5
	    }
	    return
	}
    }
}

# compute equivalent anisotropic temperature factor for Uequiv
proc CalcAniso {phase Uequiv} {
    foreach var {a b c alpha beta gamma} {
    	set $var [phaseinfo $phase $var]
    }

    set G(1,1) [expr {$a * $a}]
    set G(2,2) [expr {$b * $b}]
    set G(3,3) [expr {$c * $c}]
    set G(1,2) [expr {$a * $b * cos($gamma*0.017453292519943)}]
    set G(2,1) $G(1,2)
    set G(1,3) [expr {$a * $c * cos($beta *0.017453292519943)}]
    set G(3,1) $G(1,3)
    set G(2,3) [expr {$b * $c * cos($alpha*0.017453292519943)}]
    set G(3,2) $G(2,3)

    # Calculate the volume**2
    set v2 0.0
    foreach i {1 2 3} {
        set J [expr {($i%3) + 1}]
        set K [expr {(($i+1)%3) + 1}]
        set v2 [expr {$v2+ $G(1,$i)*($G(2,$J)*$G(3,$K)-$G(3,$J)*$G(2,$K))}]
    }
    if {$v2 > 0} {
	set v [expr {sqrt($v2)}]
	foreach i {1 2 3} {
	    set i1 [expr {($i%3) + 1}]
	    set i2 [expr {(($i+1)%3) + 1}]
	    foreach j {1 2 3} {
		set j1 [expr {($j%3) + 1}]
		set j2 [expr {(($j+1)%3) + 1}]
		set C($j,$i) [expr {(\
			$G($i1,$j1) * $G($i2,$j2) - \
			$G($i1,$j2)  * $G($i2,$j1)\
			)/ $v}]
	    }
	}
	set A(1,2) [expr {0.5 * ($C(1,2)+$C(2,1)) / sqrt( $C(1,1)* $C(2,2) )}]
	set A(1,3) [expr {0.5 * ($C(1,3)+$C(3,1)) / sqrt( $C(1,1)* $C(3,3) )}]
	set A(2,3) [expr {0.5 * ($C(2,3)+$C(3,2)) / sqrt( $C(2,2)* $C(3,3) )}]
	foreach i {1 1 2} j {2 3 3} {
	    set A($i,$j) [expr {0.5 * ($C($i,$j) + $C($j,$i)) / \
		    sqrt( $C($i,$i)* $C($j,$j) )}]
	    # clean up roundoff
	    if {abs($A($i,$j)) < 1e-5} {set A($i,$j) 0.0}
	}
    } else {
	set A(1,2) 0.0
	set A(1,3) 0.0
	set A(2,3) 0.0
    }
    return "$Uequiv $Uequiv $Uequiv \
	    [expr {$Uequiv * $A(1,2)}] \
	    [expr {$Uequiv * $A(1,3)}] \
	    [expr {$Uequiv * $A(2,3)}]"
}

#======================================================================
# conversion routines
#======================================================================

# convert x values to d-space
proc tod {xlist hst} {
    global expmap
    if {[string range $expmap(htype_$hst) 2 2] == "T"} {
	return [toftod $xlist $hst]
    } elseif {[string range $expmap(htype_$hst) 2 2] == "C"} {
	return [tttod $xlist $hst]
    } elseif {[string range $expmap(htype_$hst) 2 2] == "E"} {
	return [engtod $xlist $hst]
    } else {
	return {}
    }
}

# convert tof to d-space
proc toftod {toflist hst} {
    set difc [expr {[histinfo $hst difc]/1000.}]
    set difc2 [expr {$difc*$difc}]
    set difa [expr {[histinfo $hst difa]/1000.}]
    set zero [expr {[histinfo $hst zero]/1000.}]
    set ans {}
    foreach tof $toflist {
	if {$tof == 0.} {
	    lappend ans 0.
	} elseif {$tof == 1000.} {
	    lappend ans 1000.
	} else {
	    set td [expr {$tof-$zero}]
	    lappend ans [expr {$td*($difc2+$difa*$td)/ \
		    ($difc2*$difc+2.0*$difa*$td)}]
	}
    }
    return $ans
}

# convert two-theta to d-space
proc tttod {twotheta hst} {
    set lamo2 [expr {0.5 * [histinfo $hst lam1]}]
    set zero [expr [histinfo $hst zero]/100.]
    set ans {}
    set cnv [expr {acos(0.)/180.}]
    foreach tt $twotheta {
	if {$tt == 0.} {
	    lappend ans 99999.
	} elseif {$tt == 1000.} {
	    lappend ans 0.
	} else {
	    lappend ans [expr {$lamo2 / sin($cnv*($tt-$zero))}]
	}
    }
    return $ans
}

# convert energy (edx-ray) to d-space 
# (note that this ignores the zero correction)
proc engtod {eng hst} {
    set lam [histinfo $hst lam1]
    set zero [histinfo $hst zero]
    set ans {}
    set v [expr {12.398/(2.0*[sind[expr ($lam/2.0)]])}]
    foreach e $eng {
	if {$e == 0.} {
	    lappend ans 1000.
	} elseif {$e == 1000.} {
	    lappend ans 0.
	} else {
	    lappend ans [expr {$v/$e}]
	}
    }
    return $ans
}

# convert x values to Q
proc toQ {xlist hst} {
    global expmap
    if {[string range $expmap(htype_$hst) 2 2] == "T"} {
	return [toftoQ $xlist $hst]
    } elseif {[string range $expmap(htype_$hst) 2 2] == "C"} {
	return [tttoQ $xlist $hst]
    } elseif {[string range $expmap(htype_$hst) 2 2] == "E"} {
	return [engtoQ $xlist $hst]
    } else {
	return {}
    }
}
# convert tof to Q
proc toftoQ {toflist hst} {
    set difc [expr {[histinfo $hst difc]/1000.}]
    set difc2 [expr {$difc*$difc}]
    set difa [expr {[histinfo $hst difa]/1000.}]
    set zero [expr {[histinfo $hst zero]/1000.}]
    set 2pi [expr {4.*acos(0.)}]
    set ans {}
    foreach tof $toflist {
	if {$tof == 0.} {
	    lappend ans 99999.
	} elseif {$tof == 1000.} {
	    lappend ans 0.
	} else {
	    set td [expr {$tof-$zero}]
	    lappend ans [expr {$2pi * \
		    ($difc2*$difc+2.0*$difa*$td)/($td*($difc2+$difa*$td))}]
	}
    }
    return $ans
}

# convert two-theta to Q
proc tttoQ {twotheta hst} {
    set lamo2 [expr {0.5 * [histinfo $hst lam1]}]
    set zero [expr [histinfo $hst zero]/100.]
    set ans {}
    set cnv [expr {acos(0.)/180.}]
    set 2pi [expr {4.*acos(0.)}]
    foreach tt $twotheta {
	if {$tt == 0.} {
	    lappend ans 0.
	} elseif {$tt == 1000.} {
	    lappend ans 1000.
	} else {
	    lappend ans [expr {$2pi * sin($cnv*($tt-$zero)) / $lamo2}]
	}
    }
    return $ans
}
# convert energy (edx-ray) to Q 
# (note that this ignores the zero correction)
proc engtoQ {eng hst} {
    set lam [histinfo $hst lam1]
    set zero [histinfo $hst zero]
    set ans {}
    set v [expr {12.398/(2.0*[sind[expr ($lam/2.0)]])}]
    set 2pi [expr {4.*acos(0.)}]
    foreach e $eng {
	if {$e == 0.} {
	    lappend ans 0.
	} elseif {$e == 1000.} {
	    lappend ans 1000.
	} else {
	    lappend ans [expr {$2pi * $e / $v}]
	}
    }
    return $ans
}
proc sind {angle} {
    return [expr {sin($angle*acos(0.)/90.)}]
}

# convert d-space values to 2theta, TOF or KeV
proc fromd {dlist hst} {
    global expmap
    if {[string range $expmap(htype_$hst) 2 2] == "T"} {
	set difc [expr {[histinfo $hst difc]/1000.}]
	set difa [expr {[histinfo $hst difa]/1000.}]
	set zero [expr {[histinfo $hst zero]/1000.}]
	set ans {}
	foreach d $dlist {
	    if {$d == 0.} {
		lappend ans 0.
	    } elseif {$d == 1000.} {
		lappend ans 1000.
	    } else {
		lappend ans [expr {$difc*$d + $difa*$d*$d + $zero}]
	    }
	}
	return $ans
    } elseif {[string range $expmap(htype_$hst) 2 2] == "C"} {
	set lamo2 [expr {0.5 * [histinfo $hst lam1]}]
	set zero [expr [histinfo $hst zero]/100.]
	set ans {}
	set cnv [expr {180./acos(0.)}]
	foreach d $dlist {
	    if {$d == 99999.} {
		lappend ans 0
	    } elseif {$d == 0.} {
		lappend ans 1000.
	    } else {
		lappend ans [expr {$cnv*asin($lamo2/$d) + $zero}]
	    }
	}
	return $ans
    } elseif {[string range $expmap(htype_$hst) 2 2] == "E"} {
	set lam [histinfo $hst lam1]
	set zero [histinfo $hst zero]
	set v [expr {12.398/(2.0*[sind[expr ($lam/2.0)]])}]
	set ans {}
	foreach d $dlist {
	    if {$d == 1000.} {
		lappend ans 0
	    } elseif {$d == 0.} {
		lappend ans 1000.
	    } else {
		lappend ans [expr {$v/$d}]
	    }
	}
	return $ans
    } else {
	return {}
    }
}

# convert Q values to 2theta, TOF or KeV
proc fromQ {Qlist hst} {
    global expmap
    if {[string range $expmap(htype_$hst) 2 2] == "T"} {
	set difc [expr {[histinfo $hst difc]/1000.}]
	set difa [expr {[histinfo $hst difa]/1000.}]
	set zero [expr {[histinfo $hst zero]/1000.}]
	set ans {}
	foreach Q $Qlist {
	    if {$Q == 0.} {
		lappend ans 1000.
	    } elseif {$Q == 99999.} {
		lappend ans 1000.
	    } else {
		set d [expr {4.*acos(0.)/$Q}]
		lappend ans [expr {$difc*$d + $difa*$d*$d + $zero}]
	    }
	}
	return $ans
    } elseif {[string range $expmap(htype_$hst) 2 2] == "C"} {
	set lamo4pi [expr {[histinfo $hst lam1]/(8.*acos(0.))}]
	set zero [expr [histinfo $hst zero]/100.]
	set ans {}
	set cnv [expr {180./acos(0.)}]
	foreach Q $Qlist {
	    if {$Q == 0.} {
		lappend ans 0
	    } elseif {$Q == 1000.} {
		lappend ans 1000.
	    } else {
		lappend ans [expr {$cnv*asin($Q*$lamo4pi) + $zero}]
	    }
	}
	return $ans
    } elseif {[string range $expmap(htype_$hst) 2 2] == "E"} {
	set lam [histinfo $hst lam1]
	set zero [histinfo $hst zero]
	set v [expr {12.398/(2.0*[sind[expr ($lam/2.0)]])}]
	set ans {}
	set 2pi [expr {4.*acos(0.)}]
	foreach Q $Qlist {
	    if {$Q == 1000.} {
		lappend ans 0
	    } elseif {$Q == 0.} {
		lappend ans 1000.
	    } else {
		lappend ans [expr {$Q * $v/$2pi}]
	    }
	}
	return $ans
    } else {
	return {}
    }
}
