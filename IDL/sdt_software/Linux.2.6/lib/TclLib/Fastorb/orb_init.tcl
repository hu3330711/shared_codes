# @(#)orb_init.tcl	1.5	12/07/93

# orbit_init.tcl should really just source all the other scripts, and
# make sure that the main startup window pops up (by executing "update",
# which is a Tk procedure that forces execution of all pending events).

# tk_library is set by libtk.a.  It is either the value of the
#  TK_LIBRARY environment variable (which should be set to the location
#  of tk.h, which is probably /disks/fast/software/integration/SunOS.5.2/
#  lib/TclLib/Tk), or, if this is not set, to the compiled in value.

# fastorb_library is set by the fastorb executable.  Generally scripts
#  should probably be assumed to be in $fastorb_library, except possibly
#  each user's personal startup script (".fastorbrc" in the home directory?
#  This isn't implemented yet anyway).

# info is a Tcl builtin (see Tcl(1)) that returns information about the
#  interpreter.  set is used to set variables.  The square brackets are
#  equivalent to backquoting (`...`) in the C-shell.  So [info library]
#  is the result of executing "info library", which returns the location
#  of the Tcl initialization scripts.  This is either the path in the env.
#  variable TCL_LIBRARY (probably /disks/fast/software/integration/SunOS.5.2/
#  lib/TclLib/Tcl), or, failing that, the compiled in value.

set tcl_library [info library]

# These two files *must* be sourced by any Tcl/Tk application.  init.tcl
#  defines the Tcl builtin procedures, and tk.tcl defines the Tk class
#  bindings (i.e., what to do what a button widget is pushed, etc.)

source $tcl_library/init.tcl
source $tk_library/tk.tcl

# These are the rest of the initialization scripts that define various
#  procedures for fastorb.

source $fastorb_library/misc_widgets.tcl
source $fastorb_library/worldgraphics.tcl
source $fastorb_library/file.tcl
source $fastorb_library/satwindows.tcl
source $fastorb_library/windows.tcl
source $fastorb_library/worldmap.tcl

# Initialize some tcl/tk variables

# Status message
set status_msg {}

# Build the window--these procedures are defined in windows.tcl
CreateMenuBar .menuBar
CreateDisplayWindow .dispWin
CreateStatusWindow .status
CreateClockBar .clockBar

# pack is one of two Tk geometry managers.  The basic idea is that .menuBar
#  will be put at the top of the window ".", then .dispWin underneath that,
#  and lastly .status.

pack append .	\
  .menuBar	{top fill}	\
  .dispWin	{top fill expand}	\
  .clockBar	{top fill}	\
  .status	{top fill}

# Now allow the plot window to be rescaled, and read in the worldmap
#  definitions.

wm maxsize . 1200 1000
update

# Set up world map display.  This should really be done in the
#  map display thread; it'll eventually be moved there.

#  Make the map canvas and set it up to fill the .dispWin frame
canvas .dispWin.map
place .dispWin.map -in .dispWin -relheight 1.0 -relwidth 1.0

#  Bind <Configure> events to rescale the map in case the window is
#  resized
bind .dispWin.map <Configure> {rescale .dispWin.map %w %h}

#  'update' is needed in order to set the window size
update

#  Set the map canvas width and height properties to match the window
#  so that scaling works right
setscale .dispWin.map [winfo width .dispWin.map] [winfo height .dispWin.map]

CreateWorldMap .dispWin.map

# This sets the default focus for keyboard events to go to the main window.
#  Other widgets (e.g., text and entry widgets) may temporarily grab the
#  keyboard focus; when they release it, this line guarantees that the
#  main window will get it back (otherwise there might just not be any
#  focus window for keyboard events).

focus default .

# Make sure everything comes up on the screen.

update

# This is used to set the status message.  This routine will probably
#  get changed when the user interface is changed to support multiple
#  windows, since there won't be a permanent place for messages to appear
#  (probably warnings will be put in pop-up windows).

proc set_status_msg {m} {

	global status_msg
	set status_msg $m
}


