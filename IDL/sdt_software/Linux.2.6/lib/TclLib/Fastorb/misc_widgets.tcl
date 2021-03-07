# @(#)misc_widgets.tcl	1.2	07/30/93

# This file just defines some miscellaneous widgets that are useful to
#  have.


# StdScale:  Create a horizontal scale widget with name w, label t, minimum
#  and maximum values min and max, tick interval delta, floating-point
#  resolution res, and size s (defaults to 300 pixels wide if 0).  The
#  scale command used here is a modification of the "scale" widget described
#  in the Tk(1) manual pages.  This is actually a floating-point scale
#  widget, which, in our copy of the Tk library, replaces the standard
#  scale widget.  If the -resolution argument is ommitted, it should be
#  exactly as an integer scale widget, as per the Tk(1) man page.  scale
#  widgets don't come with very useful bindings, so I've defined some here.
#  The focus is grabbed by the scale widget whenver the pointer enters it,
#  and released upon exit.  Left (right) arrows will decrement (increment)
#  the value of the scale by res.  <Ctrl>-Left (<Ctrl>-Right) arrows will
#  decrement (increment) the scale value by 10*res.

proc StdScale {w t min max delta res s} {

	global cur_scale
	
	if {$s == 0} {set l 300} {set l $s}
	scale $w -orient hor -label $t -from $min -to $max \
	  -tickinterval $delta -length $l -showvalue True \
	  -resolution $res -relief raised
	
	bind $w <Enter> "set cur_scl $w ; set cur_scl_res $res ; focus $w"
	bind $w <Leave> "focus [winfo parent $w]"
	bind $w <Left> {$cur_scl set [expr {[$cur_scl get] - $cur_scl_res}]}
	bind $w <Right> {$cur_scl set [expr {[$cur_scl get] + $cur_scl_res}]}
	bind $w <Control-Left> \
	  {$cur_scl set [expr {[$cur_scl get] - [expr {10*$cur_scl_res}]}]}
	bind $w <Control-Right> \
	  {$cur_scl set [expr {[$cur_scl get] + [expr {10*$cur_scl_res}]}]}

	focus $w
}


# EntryBox : Create an entry box named b, with label widget b.msg and entry
#  widget b.val.

proc EntryBox {b msg s} {

    frame $b
    label $b.msg -text "$msg"
    entry $b.val -relief sunken -width $s
    pack append $b	\
      $b.msg {left}	\
      $b.val {right}
    EnableEntryBox $b
}


proc EnableEntryBox {l} {

	foreach b $l {
	    if {[winfo exists $b.val]} {
		$b.val configure -state normal -background #ffe4c4
	    }
	}
	
}


proc DisableEntryBox {l} {

	foreach b $l {
	    if {[winfo exists $b.val]} {
		$b.val configure -state disabled -background Red
	    }
	}
	
}


