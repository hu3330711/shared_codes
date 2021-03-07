# Scaled TCL/TK canvas graphics
#
#	@(#)worldgraphics.tcl	1.3 12/07/93	UCB SSL
#
#	A "context" consists of a canvas pathname, together with the
#	world coordinates of the lower left and upper right corners
#	of the canvas:  {<pathname> <xll> <yll> <xur> <yur>}
#
#	Note that these procedures use the canvas -width and -height
#	properties to determine the scale of the mapping.  These will
#	not automatically be the same as the width and height of the
#	window, and so must be initialized by a 'canvas configure'
#	command before drawing any scaled items on the canvas.
#
#	The 'rescale' procedure changes the -width and -height
#	properties according to the scaling parameters.

# proc scalecoords {context coords}
#	Convert a list of world coordinates to canvas coordinates, using
#	the given "context".

proc scalecoords {context coords} {
	set cnvs [lindex $context 0]
	set wldxll [lindex $context 1]
	set wldyll [lindex $context 2]
	set wldxur [lindex $context 3]
	set wldyur [lindex $context 4]

	set winxll 0.0
	set winyll [lindex [$cnvs configure -height] 4]

	set winxur [lindex [$cnvs configure -width] 4]
	set winyur 0.0

	return [worldscale [concat $wldxll $wldyll $wldxur $wldyur] \
		   [concat $winxll $winyll $winxur $winyur] \
		   $coords]

}


# proc scalecreate {context item coords args}
#	Scalecreate creates an "item" on the canvas (any of the items
#	supported by the canvas widget) specified in the world coordinates
#	defined by "context".  Any options for the newly created item
#	follow the coordinates.
#
#	Example:
#		set ctxt {.c -1 -1 1 1}
#		scalecreate $ctxt polygon {-.5 -.5 -.5 .5 .5 .5 .5 -.5}
# 

proc scalecreate {context item coords args} {
	set cnvs [lindex $context 0]

	set cmd [concat $cnvs "create" $item [scalecoords $context $coords]]
	set cmd [concat $cmd $args]

	eval $cmd
}
	
# proc rescale {cnvs width height}
#	Rescale all the items on the given canvas "cnvs" to fit the new
#	width and height of the canvas.  This is intended to handle 
#	configure events when the parent window is resized.
#
#	The width and height parameters are in canvas coordinates
#	(i.e. pixels).
#
#	Example:
#		bind canvaspath <Configure> {rescale canvaspath %w %h}
#

proc rescale {cnvs width height} {
	set xratio [expr {1.0 * $width / [lindex [$cnvs configure -width] 4]}]
	set yratio [expr {1.0 * $height / [lindex [$cnvs configure -height] 4]}]

	$cnvs configure -width $width -height $height
	$cnvs scale all 0 0 $xratio $yratio
}


# proc setscale {cnvs width height}
#	Set the width and height properties of the canvas "cnvs"
#

proc setscale {cnvs width height} {
	$cnvs configure -width $width -height $height
}
