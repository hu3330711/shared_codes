# @(#)stats.tcl	1.2	07/30/93

# Orbit quantities windows.  These windows continuously display updated
#  orbit quantities during a plot.  The values that are to be updated
#  are defined by the SetUpdateList procedure, which takes as arguments
#  a list of tcl variables to be updated.  These variables are associated
#  with C code variables by means of the variable hash table VarTbl.  Only
#  one update list can be active at a time, so here it is set when this
#  file is sourced, and so either or both of these windows can be open
#  at any given time.  The effect of having a very long update list is
#  unknown in terms of performance.  The variables in the update list
#  must be global Tcl variables.

SetUpdateList orb_time orb_x orb_y orb_z orb_vx orb_vy orb_vz


# This is a convenience procedure for making a horizontal display of a
#  Tcl variable.  The display has label l, and continuously displays
#  the variable v.

proc StatFrame {w l v} {

	global $v
	
	frame $w
	label $w.lbl -text $l
	label $w.val -textvar $v -width 20 -relief sunken
	pack append $w		\
	  $w.lbl	{left fillx expand}	\
	  $w.val	{right fillx expand}
	  
}


# Position information.  These coordinates are in whatever units Mike
#  Temerin's orbit propagator (orbit_no_drag) uses.

proc CreatePositionStatsWindow {} {

	global orb_time
	global orb_x
	global orb_y
	global orb_z
	global stat_window
	
	toplevel .position
	
	set p .position
	set stat_window .position
	
	StatFrame $p.time "Time:" orb_time
	StatFrame $p.x "X-coord:" orb_x
	StatFrame $p.y "Y-coord:" orb_y
	StatFrame $p.z "Z-coord:" orb_z
	
	button $p.dis -text "Dismiss" -command {destroy .position}

	pack append $p	\
	  $p.time		{top fill expand}	\
	  $p.x			{top fill expand}	\
	  $p.y			{top fill expand}	\
	  $p.z			{top fill expand}	\
	  $p.dis		{top}
	  
}


# Velocity quantities.

proc CreateVelocityStatsWindow {} {

	global orb_time
	global orb_vx
	global orb_vy
	global orb_vz
	global stat_window
	
	toplevel .velocity
	
	set p .velocity
	set stat_window .velocity
	
	StatFrame $p.time "Time:" orb_time
	StatFrame $p.x "X-vel:" orb_vx
	StatFrame $p.y "Y-vel:" orb_vy
	StatFrame $p.z "Z-vel:" orb_vz
	
	button $p.dis -text "Dismiss" -command {destroy .velocity}

	pack append $p	\
	  $p.time		{top fill expand}	\
	  $p.x			{top fill expand}	\
	  $p.y			{top fill expand}	\
	  $p.z			{top fill expand}	\
	  $p.dis		{top}
	  
}


# For simplicity, all quantity display windows should probably be
#  defined in this file.  The procedure StatWindow can be called at
#  any time with a single argument indicating which window to create.
#  This intermediary routine allows for possible expansion of the
#  functionality of quantity windows, in case eventually there is some
#  other procedure that should be executed every time a quantity window
#  is created.

proc StatWindow {t} {

	case $t in	\
	  {pos}		{CreatePositionStatsWindow}		\
	  {vel}		{CreateVelocityStatsWindow}
	  
}

