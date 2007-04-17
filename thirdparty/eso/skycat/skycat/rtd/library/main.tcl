# -*-tcl-*-
#
# E.S.O. - VLT project
#
# "@(#) $Id: main.tcl,v 1.1 2006/01/28 23:03:26 abrighto Exp $" 
#
# rtd - main entry point for real-time image display application
#
# Usage: rtd ?options?
#
# where ?options? are the same as the "public" variables in the main
# itcl class Rtd and take the form: -option value ...`
#
# See man page rtd(1) for a complete description.
#
# who           when       what
# --------     ---------   ----------------------------------------------
# A.Brighton   20/05/95    created
# pbiereic     16/08/99    added option -help
# pbiereic     05/02/03    added rtd_versionId so that the version can
#                          be printed with ´what rtd´ (see SPR).
# A.Brighton   29/12/05    rewrote for new setup

package require Rtd

set rtd_versionId {@(#) $Id: main.tcl,v 1.1 2006/01/28 23:03:26 abrighto Exp $}

set rtd_usage {
Usage: rtd ?fitsFile? ?-option value ...?

Options:
    -attach bool               - attach to camera
    -camera <name>             - camera name: def: \$RTD_CAMERA, if set, otherwise \$RTDSIMULATOR
    -colorramp_height <n>      - height of colorramp window (default: 12)
    -colorramp_height <pixels> - height of color bar
    -debug bool                - debug flag: turn on real-time simulation
    -default_cmap <cmap>       - default colormap
    -default_itt <itt>         - default intensity transfer table
    -disp_image_icon bool      - display a copy of the image in the icon (default)
    -dozoom bool               - turn on zoom window (default)
    -file <file>               - fits file to load ('-' for stdin)
    -float_panel bool          - put info panel in a popup window (default: 0)
    -help                      - print this text
    -image_directory           - default directory for loading images
    -interval <msecs>          - for real-time simulation: update interval in ms (def: 500)
    -max_scale <n>             - maximum scale for magnification menu (default: 20).
    -min_scale <n>             - minimum scale for magnification menu (default: -10).
    -pan_height <n>            - height of pan window (default: 152)
    -pan_width <n>             - width of pan window (default: 152)
    -panel_layout <layout>     - panel layout, one of: "saoimage", "reverse", "default" 
    -panel_orient <orient>     - panel orientation, one of: "horizontal", "vertical" 
    -port <port>               - listen for remote cmds on port (default: 0 = choose port)
    -subsample bool            - use subsampling when shrinking the image (default: 1 = on).
    -sampmethod <n>            - sampling method when subsample=0 (default: 0 = max value).
    -rtd_geometry              - window geometry
    -rtd_title                 - string for title bar
    -scrollbars bool           - display scrollbars (not displayed by default)
    -testprog <name>           - prog to use for real-time simulation (default: tRtd)
    -usexshm bool              - use X shared mem, if available (default)
    -verbose bool              - print diagnostic messages
    -with_colorramp bool       - display the color bar (default)
    -with_grid bool            - Include a WCS grid button (default: 0 = off).
    -with_pan_window bool      - display the pan window (default))
    -with_warp bool            - add bindings to move mouse ptr with arrow keys (default: 1).
    -with_zoom_window bool     - display the zoom window (default))
    -xscale <n>                - default scaling factor
    -yscale <n>                - default scaling factor
    -zoom_factor <n>           - zooming factor (default: 4)
    -zoom_height <n>           - height of zoom window (default: 152)
    -zoom_width <n>            - width of zoom window (default: 152)
}

if {[info exists argv]} {
    if {[regexp -- "-help" $argv]} {
	puts $rtd_usage
	exit 0
    }
}

set tk_strictMotif 1
catch {tk appname Rtd}
tk_focusFollowsMouse

if { [catch {utilPrintErrors} msg ] } {
    puts "Error when autoloading 'utilPrintErrors'.\nauto_path=$auto_path\nCheck installation:\n\n$msg"
    exit 1
}

# XXX temp hack until panel editor is fixed to only require Rtd when needed
image delete [image create rtdimage]

# Start the application class Rtd:
#
# Note that "start" is a "member" proc in the TopLevelWidget class
# (of which Rtd is a subclass). It creates an instance of the Rtd 
# application class and handles options and error checking.

# Specify a list of valid options (workaround for tcl or itcl bug (?) that
# crashes app if option is unknown...)
set optlist [list \
	-attach \
	-camera \
	-color_scale \
	-colorramp_height \
	-debug \
	-default_cmap \
	-default_itt \
	-disp_image_icon \
	-dozoom \
	-drag_scroll \
	-feedback \
	-file  \
	-float_panel \
	-image_directory \
	-interval \
	-max_scale \
	-min_scale \
	-pan_height \
	-pan_width \
	-panel_layout \
	-panel_orient \
	-pickobjectorient \
	-port \
	-rapid_frame_command \
	-regioncommand \
	-remote \
	-rtd \
	-rtd_geometry \
	-rtd_title \
	-scrollbars \
	-shm_data \
	-shm_header \
	-shorthelpwin \
	-subsample \
	-sampmethod \
	-testprog \
	-updatePick \
	-use_zoom_view \
	-usexshm \
	-usexsync \
	-verbose \
	-with_colorramp \
	-with_grid \
	-with_pan_window \
	-with_perftest \
	-with_warp 1 \
	-with_zoom_window \
	-xscale \
	-yscale \
	-zoom_factor \
	-zoom_height \
	-zoom_view_propagate \
	-zoom_width \
	]

util::TopLevelWidget::start Rtd "-file" "$rtd_usage" "" 1 $optlist

