# @(#)windows.tcl	1.4	12/07/93

# windows.tcl : Create the main windows of the application.  


# Satellite name list.
#	This list will be updated whenever new satellite data is loaded.
#	We'll also have to update the "Satellite" menu when that happens,
#	but that isn't implemented yet.

set satNames {FAST EUVE}


# Make the menu bar

proc CreateMenuBar {m} {
	global satNames
	global fastorb_library

	frame $m -background SteelBlue
	
# 	FILE MENU
	menubutton $m.file -text "File" -menu $m.file.mnu -underline 0
	menu $m.file.mnu
	$m.file.mnu add command -label "Load..." \
	  -state disabled -underline 0
	$m.file.mnu add command -label "Save..." \
	  -state disabled -underline 0
	$m.file.mnu add command -label "Print" \
	  -state disabled -underline 0
	$m.file.mnu add separator
	$m.file.mnu add command -label "Execute tcl command..." \
	  -command {execute_command}
	$m.file.mnu add separator
	$m.file.mnu add command -label "Quit" \
	  -command {destroy .} -underline 0
	  
	menubutton $m.view -text "View" -menu $m.view.mnu -underline 0
	menu $m.view.mnu
	$m.view.mnu add command -label "World Map" -command {} -underline 0
	$m.view.mnu add command -label "Globe" -state disabled -underline 0
	$m.view.mnu add command -label "Ground Station..." \
	  -state disabled -underline 0

	menubutton $m.satellite -text "Satellite" -menu $m.satellite.mnu \
	  -underline 0
	menu $m.satellite.mnu
	set i 0
	set l [llength $satNames]
	while {$i < $l} {
	    set sat [lindex $satNames $i]
	    $m.satellite.mnu add command -label $sat \
		-command [concat CreateSatWindow $sat -menu $m.satellite.mnu $sat]
	    incr i 1
	}

	button $m.properties -text "Properties" -command {}

	button $m.print -text "Print" \
		-command [concat exec $fastorb_library/xdpr \
			-id [winfo id [winfo parent $m]] -screen -device ps -gray 4]
	
	pack append $m	\
	  $m.file 	{left padx 2}	\
	  $m.view	{left padx 2}	\
	  $m.satellite	{left padx 2}	\
	  $m.print	{right padx 2}	\
	  $m.properties	{right padx 2}
	
	tk_menuBar $m $m.file $m.view $m.satellite
	tk_bindForTraversal .
	
}


# Main Display Window
#  This will hold the current "map" display on the main window of
#  the orbit display program
proc CreateDisplayWindow {w} {
	global dispWinCurrent
	
	frame $w -geometry 800x400 -background chartreuse

	label $w.notyet -text "Please wait..."

	set dispWinCurrent $w.notyet

	place $w.notyet -in $w -relx 0.5 -rely 0.5
	
}

# Time Display bar
proc CreateClockBar {w} {

	global utcYear utcDoy utcTime localYear localDay localMonth localTime

	frame $w -background grey

	frame $w.utc -background grey
	label $w.utc.title -text UTC
	label $w.utc.year -textvar utcYear
	label $w.utc.doy  -textvar utcDoy
	label $w.utc.time -textvar utcTime
	pack append $w.utc \
	   $w.utc.title	{left padx 2} \
	   $w.utc.year	{left padx 2} \
	   $w.utc.doy	{left padx 2} \
	   $w.utc.time	{left padx 2}

	frame $w.local -background grey
	label $w.local.title -text "Local time"
	label $w.local.day  -textvar localDay
	label $w.local.month -textvar localMonth
	label $w.local.year -textvar localYear
	label $w.local.time  -textvar localTime
	pack append $w.local \
	   $w.local.title	{left padx 2} \
	   $w.local.year	{left padx 2}\
	   $w.local.month	{left padx 2} \
	   $w.local.day	{left padx 2} \
	   $w.local.time	{left padx 2}

	pack append $w \
	   $w.utc	{left padx 30} \
	   $w.local	{left padx 30}
}

# Status message bar

proc CreateStatusWindow {w} {

	global status_msg
	label $w -textvar status_msg -background Red

}


