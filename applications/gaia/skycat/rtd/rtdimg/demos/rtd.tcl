# E.S.O. - VLT project
#
# "@(#) $Id: rtd.tcl,v 1.19 1998/10/28 17:41:16 abrighto Exp $" 
#
# rtd.tcl - real-time image display application
#
# Usage: rtd ?options? (using link to rtd.sh script)
#    or: rtdimage_wish rtd.tcl ?options?
#
# where ?options? are the same as the "public" variables in the main
# itcl class Rtd and take the form: -option value ...`
#
# See man page rtd(1) for a complete description.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 20 May 95   created


set rtd_usage {
Usage: rtd ?fitsFile? ?-option value ...?

Options:
    -file <file>               - fits file to load ('-' for stdin)
    -port <port>               - listen for remote cmds on port (default: 0 = choose port)
    -camera <name>             - camera name: def: \$RTD_CAMERA, if set, otherwise \$RTDSIMULATOR
    -float_panel bool          - put info panel in a popup window (default: 0)
    -panel_layout <layout>     - panel layout, one of: "saoimage", "reverse" or "default" 
    -min_scale <n>             - minimum scale for magnification menu (default: -10).
    -max_scale <n>             - maximum scale for magnification menu (default: 20).
    -default_cmap <cmap>       - default colormap
    -default_itt <itt>         - default intensity transfer table
    -colorramp_height <pixels> - height of color bar
    -with_colorramp bool       - display the color bar (default)
    -with_warp bool            - add bindings to move mouse ptr with arrow keys (default: 1).
    -with_grid bool            - Include a WCS grid button (default: 0 = off).
    -with_zoom_window bool     - display the zoom window (default))
    -with_pan_window bool      - display the pan window (default))
    -zoom_factor <n>           - zooming factor (default: 4)
    -zoom_width <n>            - width of zoom window (default: 152)
    -zoom_height <n>           - height of zoom window (default: 152)
    -pan_width <n>             - width of pan window (default: 152)
    -pan_height <n>            - height of pan window (default: 152)
    -colorramp_height <n>      - height of colorramp window (default: 12)
    -disp_image_icon bool      - display a copy of the image in the icon (default)
    -scrollbars bool           - display scrollbars (not displayed by default)
    -dozoom bool               - turn on zoom window (default)
    -usexshm bool              - use X shared mem, if available (default)
    -verbose bool              - print diagnostic messages
    -debug bool                - debug flag: turn on real-time simulation
    -interval <msecs>          - for real-time simulation: update interval in ms (def: 500)
    -testprog <name>           - prog to use for real-time simulation (default: tRtd)
}

# The startup script rtd.sh(.in) sets these environment variables to
# point to the tcl/tk script dirs. We need to know this mainly when
# loading packages dynamically from shared libraries, since the
# auto_path tcl variable determines the search path for the pkgIndex.tcl
# files used to load the shared libraries at run time.
foreach pkg {BLT RTD} {
    if {[info exists env(${pkg}_LIBRARY)]} {
	lappend auto_path $env(${pkg}_LIBRARY)
    }
}

set tk_strictMotif 1
catch {tk appname Rtd}
tk_focusFollowsMouse

# load the required packages, if it is not already loaded
foreach pkg {Rtd} {
    if {[catch {package require $pkg} msg]} {
	puts "error loading $pkg package: $msg"
	exit 1
    }
}

utilPrintErrors

# Start the application class Rtd:
#
# Note that "start" is a "member" proc in the TopLevelWidget class
# (of which Rtd is a subclass). It creates an instance of the Rtd 
# application class and handles options and error checking.

util::TopLevelWidget::start Rtd "-file" "$rtd_usage"

