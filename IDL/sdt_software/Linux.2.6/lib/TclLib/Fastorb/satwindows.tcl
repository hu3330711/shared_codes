#
#  @(#)satwindows.tcl	1.2 12/07/93	UCB SSL

# --------------------------------------------------------------
# Satellite data windows
#
#   Each satallite has a data display window associated with it.  Several
#   different quantities can be plotted; satPlotNames holds the internal
#   names of the plotted quantities.  satPlotLabels holds the associated
#   plot labels (for menu entries, plot titles, etc.)

set satPlotNames { \
	latitude \
	longitude \
	altitude \
	mlatitude \
	mlongitude \
	mlt \
	ilatitude \
	ilongitude \
	magangle \
}

set satPlotLabels { \
	"Latitude" \
	"Longitude" \
	"Altitude" \
	"Magnetic Latitude" \
	"Magnetic Longitude" \
	"Magnetic Local Time" \
	"Invariant Latitude" \
	"Invariant Longitude" \
	"Magnetic Field Angle" \
}

set satElemNames { \
	orbit \
	epoch \
	axis \
	ecc \
	inc \
	node \
	aperigee \
	manomaly \
}

set satElemLabels { \
	"Orbit" \
	"Epoch" \
	"Axis" \
	"Eccentricity" \
	"Inclination" \
	"RA of Asc. Node" \
	"Arg. of Perigee" \
	"Mean Anomaly" \
}
	

# The satellite display window is a toplevel window containing a menu
# bar, a plot display window, a scrollbar (for viewing all the plots)
# and a status window for various numeric data.  The satellite window
# names take the form:  .sat<satellite>  e.g.  ".satFAST".
#
# Subwindows of satellite display window w are named as follows:
#	$w.menuBar
#	$w.graphWin		Satellite data plots
#	$w.graphScroll		Scrollbar controlling $w.graphWin
#	$w.statWin		Text display of satellite parameters

proc CreateSatWindow {sat args} {
	set w .sat${sat}

	toplevel $w
	wm title $w $sat

	# Optional arguments identify calling menu entry to disable
	# or enable when window is created or destroyed
	#  -menu menupath index
	set dismisscmd {}
	if {[lindex $args 0] == "-menu" } {
	    set mnu [lindex $args 1]
	    set indx [lindex $args 2]
	    $mnu entryconfigure $indx -state disabled
	    set dismisscmd [concat $mnu entryconfigure $indx -state normal]	}
	bind $w <Destroy> [concat DismissSatWindow $sat $dismisscmd]

	# Create the main parts of the satellite window and pack them
	# into place
	CreateSatMenuBar $w.menuBar $w.graphWin $sat
	CreateSatGraphWindow $w.graphWin [concat $w.graphScroll set]
	scrollbar $w.graphScroll \
		-command [concat ScrollSatPlots $w.graphWin]
	CreateSatStatusWindow $w.statWin $sat

	pack append $w \
	   $w.menuBar	{top fill} \
	   $w.graphWin	{left fill expand} \
	   $w.statWin	{right fill} \
	   $w.graphScroll	{right fill} 

	wm maxsize $w 1200 1000
}



# Satellite menu bar

proc CreateSatMenuBar {m g sat} {
	global satPlotNames
	global satPlotLabels
	global fastorb_library

	set satState  sat${sat}State


	frame $m -background SteelBlue

# 	VIEW BUTTON
	menubutton $m.view -text "View" -menu $m.view.mnu -underline 0
	menu $m.view.mnu
	$m.view.mnu add command -label [concat $sat "Plots"]
	set i 0
	set l [llength $satPlotNames]
	while {$i < $l} {
	    set p [lindex $satPlotNames $i]
	    global ${satState}_${p}
	    set ${satState}_${p} 0
	    $m.view.mnu add checkbutton -label $i \
		-command [concat ToggleSatPlot $g.plots $sat $p] \
		-label [lindex $satPlotLabels $i] \
		-variable ${satState}_${p}
	    incr i 1
	}

	button $m.print -text "Print" \
		-command [concat PrintSatWindow [winfo parent $m]]
	button $m.properties -text "Properties"

	pack append $m \
	   $m.view	{left padx 2} \
	   $m.print	{right padx 2} \
	   $m.properties	{right padx 2}

	tk_menuBar $m $m.view
	tk_bindForTraversal .
}

proc PrintSatWindow {w} {
	global fastorb_library

	exec $fastorb_library/xdpr \
		-id [winfo id $w] -screen -device ps -gray 4
}

# Satellite Graph Window
#   This will hold several time-series plots, scrollable through the
#   visable portion of the graph window.  Scrolling is done with the
#   'place' geometry manager, since frames do not support scrolling on
#   their own.  The scrolling interface is analagous to that of the
#   tk widgets that do scroll:  The scrollbar is set up to call 
#   'ScrollSatPlots' with the name of the Graph window and the position
#   (in "scroll units") of the top edge of the visible portion of the
#   scrollabel region.  The Graph window, in turn, is given a command
#   to call (normally '<scrollbar> set') to inform the scrollbar of
#   changes in the position or size of the scrollable window.

# CreateSatGraphWindow {g scrollcmd}
# Create the satellite graph window, and all of its subwindows (including
# the plot windows, which aren't immediately displayed.  Establish a
# binding to call the supplied "scrollcmd" to inform the scrollbar of
# scrolling changes.

proc CreateSatGraphWindow {g scrollcmd} {
	global satPlotNames
	global satPlotLabels

	# The outer frame will be packed into the satellite display window
	frame $g  -width 600 -height 400

	# The inner frame will be packed with individual plot canvases
	# and will scroll through the outer frame
	frame $g.plots -background blue
	place $g.plots -in $g -relwidth 1.0 -relx 0.0

	# Create all the plot canvases, but don't map any of them yet;
	# the user will select the ones she wants with the View menu
	# Bind 'rescale' (from worldgraphics.tcl) to each canvas to
	# handle window size changes
	set i 0
	set l [llength $satPlotNames]
	while {$i < $l} {
	    set p [lindex $satPlotNames $i]
	    canvas $g.plots.$p -height 100 -relief sunken
	    $g.plots.$p create text 10 10 -text [lindex $satPlotLabels $i] \
		-anchor "nw"
	    bind $g.plots.$p <Configure> [concat rescale $g.plots.$p %w %h]
	    incr i 1
	}

	# Adjust the scrollbar appearance to reflect scroll-related changes.
	# This works like the -yScrollCommand in a scrollable widget:
	# The command executed is "scrollcmd" followed by 4 numbers as
	# for a scrollbar 'set' command.
	bind $g.plots <Configure> \
		[concat AdjustScroll "{" $scrollcmd "}" $g %h %y]
}

# AdjustScroll is used by satellite graph windows to handle <Configure>
# events and inform their scrollbars of any geometry changes in the
# scrollable portion of the window.

proc AdjustScroll {cmd win totUnits y} {
	set winUnits [winfo height $win]
	set first [expr {0 - $y}]
	set last  [expr {$winUnits - $y - 1}]
	set script [concat $cmd $totUnits $winUnits $first $last]
	eval $script
}

# ScrollSatPlots {g n}
#   Scroll the satellite plot frame within the given satellite graph window
#   'g' so that position 'n' is at the top edge of the visible region.  
#   Scrolling is clipped at the top and bottom edges of the satellite plots
#   to prevent them from scrolling out of view.

proc ScrollSatPlots {g n} {
	if {$n < 0} {
	    set newy 0
	} else {
	    set gheight [winfo height $g]
	    set gpheight [winfo height $g.plots]
	    if {$n > $gpheight - $gheight} {
		set newy [expr {$gheight - $gpheight}]
	    } else {
	        set newy [expr {0 - $n}]
	    }
	}
	place $g.plots -y $newy
}



# ToggleSatPlot makes a satellite data plot canvas visible or invisible
# depending on the state of the associated state variable.  The state
# variable names have the form:  sat<Satellite>State_<plot>
#	e.g.  satFASTState_longitude
#
# The View menu of the satellite window allows toggles the state variables
# to 1 (active) or 0 (inactive) in response to the user's selections.

proc ToggleSatPlot {gp sat p} {
	set satState sat${sat}State_${p}
	global $satState

	if { [set $satState] == 1 } {
		pack append $gp ${gp}.${p} {top fillx}
	} else {
		pack unpack ${gp}.${p}
	}
}

# Satellite Status window
#  Shows certain slowly-changing satellite parameters, such as satellite
#  name, orbit number, Keplerian elements.  The current values for
#  these items are kept in global variables named according to 
#  the form:  sat<Satellite>Data_<variable>
#	e.g.  satFASTData_satellite
#	      satFASTData_orbit

proc SatStatFrame {sw sat lbl val} {
	set var sat${sat}Data_${val}
	global $var

	frame $sw
	label $sw.lbl -text ${lbl}:
	label $sw.val -textvariable $var -relief sunken

	pack append $sw $sw.lbl {left frame nw padx 2}
	pack append $sw $sw.val {right frame ne padx 2}

	return $sw
}

proc CreateSatStatusWindow {sw sat} {
	global satElemNames
	global satElemLabels

	frame $sw

	label $sw.lbl -text [concat "Satellite:" $sat]
	pack append $sw $sw.lbl {top fillx}

	set i 0
	set l [llength $satElemNames]
	while {$i < $l} {
		set var [lindex $satElemNames $i]
		set lbl [lindex $satElemLabels $i]
		SatStatFrame $sw.$var $sat $lbl $var
		pack append $sw $sw.$var {top fillx padx 3 pady 3}
		incr i
	}

	button $sw.dismiss -text "Dismiss" \
	     -command [concat destroy .sat${sat}]
	pack append $sw $sw.dismiss {bottom pady 3}
}



# Do any necessary clean up and destroy the satellite status window
# This procedure is bound to <Destroy> events on the satellite display
# window
proc DismissSatWindow {sat args} {
	# Execute optional command in args first
	if {[llength $args] > 0} {
		eval $args
	}

	# Then do other TBD clean up tasks for the given satellite SAT

	
}
	
