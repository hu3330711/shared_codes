# @(#)file.tcl	1.3	07/27/93

proc execute_command {} {

    toplevel .cmdEntry
    EntryBox ".cmdEntry.cmd" "Enter command:" 40
    frame .cmdEntry.buttons
    button .cmdEntry.buttons.exec -text "Execute" -borderwidth 5 -command {
    	set cmd [.cmdEntry.cmd.val get]
    	if [catch $cmd res] {set_status_msg $res}
    	destroy .cmdEntry
    }
    button .cmdEntry.buttons.can -text "Cancel" -command {destroy .cmdEntry}
    pack append .cmdEntry.buttons \
      .cmdEntry.buttons.exec	{left fill expand} \
      .cmdEntry.buttons.can	{left fill expand}
    
    pack append .cmdEntry \
      .cmdEntry.cmd {top fill expand}	\
      .cmdEntry.buttons {top fill expand}
      
    focus .cmdEntry.cmd.val
    bind .cmdEntry.cmd.val <Return> {.cmdEntry.buttons.exec invoke}
    
    tkwait window .cmdEntry
    
}

