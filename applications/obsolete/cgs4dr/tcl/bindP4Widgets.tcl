proc bindP4Widgets {taskname} {
#+
# This procedure binds the actions to all the "active" widgets in the
# p4 interface.
#-
    global P4Widgets

# Command buttons.
    $P4Widgets(PLOT) configure -command "p4Plot $taskname"
    $P4Widgets(CURSOR) configure -command "p4Cursor $taskname"
    $P4Widgets(SET) configure -command "p4Set $taskname"
   #$P4Widgets(CLEAR) configure -command "p4Clear $taskname"
    $P4Widgets(CONFIGS) configure -command "p4Configs $taskname"

# Special binding for DATA entry field i.e. accept <CR> as go for plotting
    bind $P4Widgets(DATA) <Return> "p4Plot $taskname"

# Trace verbose output
    trace variable P4Widgets(VERBOSE) w "cgs4drVerbose $taskname"
    set P4Widgets(VERBOSE) 0

# Put a trace on the port number variable to load all the attribute widgets
# when the port is changed.
    trace variable P4Widgets(PORT_NO) w newP4Port
}

proc newP4Port {name el op} {
    initP4Widgets
}
