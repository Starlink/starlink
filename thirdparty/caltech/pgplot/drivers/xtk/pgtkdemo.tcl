#!pgtkdemo
#-----------------------------------------------------------------------
# Create an unmapped prompt dialog.
#
# This is split into a top section and a bottom section.
# The top section contains a title and an entry widget $w.top.entry.
# The bottom section contains three buttons, $w.bot.ok, $w.bot.cancel
# and $w.bot.help. Only the "close" button is assigned a command.
# The other buttons should be set by the caller. Note that the help
# button is displayed disabled
#
# Note that the dialog is not initially mapped. To display it temporarily
# use the command {wm deiconify $w} and then when it is no longer required
# call {wm withdraw $w}.
#
# Input:
#  w        The name to give the widget.
#  title    The title to give the dialog.
#  label    The message to place above the entry widget.
#-----------------------------------------------------------------------
proc create_prompt_dialog {w title msg} {
#
# Create the toplevel dialog window withdrawn.
#
    toplevel $w -class dialog
    wm withdraw $w
    wm title $w $title
    wm iconname $w Dialog
#
# Create the top and bottom frames.
#
    frame $w.top -relief raised -bd 1
    pack $w.top -side top -fill both -expand 1
    frame $w.bot -relief raised -bd 1
    pack $w.bot -side bottom -fill both -expand 1
#
# Create a label and an entry widget in the top frame.
#
    message $w.top.msg -justify left -width 8c -anchor w -text $msg
    entry $w.top.entry -relief sunken -bd 2 -width 30
    pack $w.top.msg $w.top.entry -side top -anchor w
#
# Create three buttons in the bottom frame.
#
    button $w.bot.ok -text OK
    button $w.bot.cancel -text Cancel -command "wm withdraw $w"
    button $w.bot.help -text Help -state disabled
    pack $w.bot.ok $w.bot.cancel $w.bot.help -side left -expand 1 -pady 2m -padx 2m
#
# Arrange for carriage-return to invoke the OK key.
#
    bind $w <Return> "$w.bot.ok invoke"
}

#-----------------------------------------------------------------------
# Create an unmapped help dialog.
#
# Note that the dialog is not initially mapped. To display it temporarily
# use the command {wm deiconify $w} and then when it is no longer required
# call {wm withdraw $w}.
#
# Input:
#  w           The name to give the widget.
#  title       The dialog title.
#  text        The text to display in the widget.
#-----------------------------------------------------------------------
proc create_help_dialog {w title text} {
#
# Create the dialog container and tell the window-manager what to call
# both it and its icon.
#
    toplevel $w -class dialog
    wm withdraw $w
    wm title $w $title
    wm iconname $w Dialog
#
# Create the top-half of the dialog and display display the usage message
# in it.
#
    frame $w.top -relief raised -bd 1
    message $w.top.msg -width 12c -text $text
    pack $w.top.msg -side left -expand 1 -fill both
#
# Create the bottom half of the dialog and place a single OK button in
# it. Arrange that pressing the OK button unmaps the dialog.
#
    frame $w.bot -relief raised -bd 1
    button $w.bot.ok -text OK -command "wm withdraw $w"
    pack $w.bot.ok -pady 2m
#
# Arrange for carriage-return to invoke the OK key.
#
    bind $w <Return> "$w.bot.ok invoke"
#
# Place the widgets in their assigned places top and bottom.
#
    pack $w.top $w.bot -side top -fill both -expand 1
}

#-----------------------------------------------------------------------
# Create a labelled option menu.
#
# The name of the menu widget will be $w.menu and the option-menu value
# will be maintained in a global variable of name global$w.menu.
#
# Input:
#  w          The name for the frame-widget that encloses the menu.
#  label      The label to place to the left of the option-menu button.
#  cmd        The command to be called whenever the option-menu value
#             is changed. This will be called as a "trace variable"
#             callback, whenever global$w.menu is written to.
#  name_list  The list of option names.
#-----------------------------------------------------------------------
proc create_option_menu {w label cmd name_list} {
#
# Create a frame to enclose the menu.
#
    frame $w
#
# Create the option-menu label.
#
    label $w.label -text $label
#
# Get the name of the variable this is to be used to trace menu-value
# changes.
#
    set var global$w.menu
    global $var
#
# Create the option menu.
#
    eval tk_optionMenu $w.menu $var $name_list
    trace variable $var w $cmd
#
# Set the width of the menu button to be the maxmimum of all
# menu options. This removes the need for dynamic resizing.
#
    set maxwidth 0
    foreach name $name_list {
	set length [string length $name]
	if [ expr $length > $maxwidth ] {
	    set maxwidth $length
	}
    }
    $w.menu configure -width $maxwidth
#
# Place the label to the left of the menu button.
#
    pack $w.label $w.menu -side left
}

#-----------------------------------------------------------------------
# Create an unmapped save-image dialog.
#-----------------------------------------------------------------------
proc create_save_dialog {w} {
    create_prompt_dialog $w "Save image" "Enter a PGPLOT device string:"
    $w.bot.ok configure -command "wm withdraw $w;update;save_image_callback $w"
}

#-----------------------------------------------------------------------
# This function is called when the user presses the OK button of the
# save-image dialog.
#
# Input:
#  w         The name of the save dialog.
#-----------------------------------------------------------------------
proc save_image_callback {w} {
    pgdemo save [$w.top.entry get]
}

#-----------------------------------------------------------------------
# Draw the currently selected image function.
#-----------------------------------------------------------------------
proc draw_image {args} {
    upvar #0 global.function.menu mode_menu
#
# Display a busy-cursor.
#
    . configure -cursor {watch}
    .imagearea.pgplot configure -cursor {}
    update
#
# Display the new function.
#
    pgdemo function $mode_menu
#
# Reset the cursor.
#
    . configure -cursor {}
    .imagearea.pgplot configure -cursor {crosshair black white}
    update
#
# Arm the cursor of the image window for the selection of a slice.
#
    prepare_for_slice
}

#-----------------------------------------------------------------------
# Recolor the current image.
#-----------------------------------------------------------------------
proc recolor_image {args} {
    upvar #0 global.colors.menu color_menu
#
# Change the colors.
#
  pgdemo recolor_image $color_menu
#
# Redraw the current image if necessary.
#
  if [.imagearea.pgplot cget -share] {
    draw_image
  }
}

#-----------------------------------------------------------------------
# Arm the image-widget cursor such that when the user next presses a
# mouse button or key within the image window the start of a slice
# will be selected.
#-----------------------------------------------------------------------
proc prepare_for_slice {args} {
  .imagearea.pgplot setcursor norm 0.0 0.0 1
  bind .imagearea.pgplot <ButtonPress> {start_slice %x %y}
}

#-----------------------------------------------------------------------
# This is used as a pgplot image-widget cursor callback. It augments the
# cursor in the image window with a line rubber-band anchored at the
# selected cursor position and registers a new callback to receive both
# the current coordinates and coordinates of the end of the slice when
# selected.
#
# Input:
#  wx wy   The X-window coordinates of the position that the user selected
#          with the cursor.
#-----------------------------------------------------------------------
proc start_slice {wx wy} {
  set pg .imagearea.pgplot
#
# Convert from X coordinates to world coordinates.
#
  set x [$pg world x $wx]
  set y [$pg world y $wy]
  $pg setcursor line $x $y 3
  bind $pg <ButtonPress> "end_slice $x $y %x %y"
}

#-----------------------------------------------------------------------
# This image-window pgplot-cursor callback is registered by start_slice.
# It receives the start coordinates of a slice from start_slice and
# the coordinate of the end of the slice from the callback arguments
# provided by the pgplot widget.
#
# Input:
#  x1 y1          The coordinate of the start of the slice in the image
#                 window. These values were supplied when the callback
#                 was registered by start_slice.
#  wx2 wy2        The X-window coordinate of the end of the slice.
#-----------------------------------------------------------------------
proc end_slice {x1 y1 wx2 wy2} {
  set pg .imagearea.pgplot
  prepare_for_slice
  pgdemo slice $x1 $y1 [$pg world x $wx2] [$pg world y $wy2]
}

#-----------------------------------------------------------------------
# This procedure creates the main menubar of the application.
#
# Input:
#  w            The name to give the widget.
#-----------------------------------------------------------------------
proc create_main_menubar {w} {
#
# Create a raised frame for the menubar.
#
    frame $w -relief raised -bd 2 -width 11c
#
# Create the file menu.
#
    menubutton $w.file -text File -menu $w.file.menu
    menu $w.file.menu -tearoff 0
    $w.file.menu add command -label {Save image as} -command {
	wm deiconify .save
        raise .save
    }
    $w.file.menu add separator
    $w.file.menu add command -label {Quit} -command {exit}
#
# Arrange that Alt-Q will abort the application.
#
    bind all <Alt-KeyPress-q> {exit}
#
# Create the help menu.
#
    menubutton $w.help -text Help -menu $w.help.menu
    menu $w.help.menu -tearoff 0
    $w.help.menu add command -label {Usage} -command {
	wm deiconify .usage_help
        raise .usage_help
    }
#
# Pack all but the help menu at the left side of the menubar.
#
    pack $w.file -side left
#
# Pack the help menu against the right edge of the menubar, as specified
# by the Motif style guide.
#
    pack $w.help -side right
}

#-----------------------------------------------------------------------
# Create an area in which to display the world coordinates of the cursor
# when it is over the image window.
#
# Input:
#  w            The name to give the frame widget that encloses the area.
#-----------------------------------------------------------------------
proc create_world_labels {w} {
#
# Enclose the area in a frame.
#
    frame $w -width 11c -height 1c
#
# Create a static title label.
#
    label $w.title -text "World coordinates: "
#
# Create the X and Y labels for displaying the respective coordinates.
#
    label $w.x -width 12 -anchor w
    label $w.y -width 12 -anchor w
    pack $w.title -side left -anchor w
    pack $w.x $w.y -side left -anchor w -padx 2m
}

#-----------------------------------------------------------------------
# Create the area that contains the image PGPLOT window.
#
# Input:
#  w            The name to give the frame widget that encloses the area.
#-----------------------------------------------------------------------
proc create_image_area {w} {
#
# Frame the workarea.
#
  frame $w -width 11c -height 11c
#
# Create the PGPLOT image window.
#
  pgplot $w.pgplot -share true -width 10c -height 10c -mincolors 25 -maxcolors 64 -bd 2 -bg black -fg white
#
# Create horizontal and vertical scroll-bars and have them
# call the pgplot xview and yview scroll commands to scroll the
# image within the window.
#
  scrollbar $w.xscroll -command "$w.pgplot xview" -orient horizontal
  scrollbar $w.yscroll -command "$w.pgplot yview" -orient vertical
#
# Tell the PGPLOT widget how to update the scrollbar sliders.
#
  $w.pgplot configure -xscrollcommand "$w.xscroll set"
  $w.pgplot configure -yscrollcommand "$w.yscroll set"
#
# Position the PGPLOT widget and the scrollbars.
#
  pack $w.xscroll -side bottom -fill x
  pack $w.yscroll -side right -fill y
  pack $w.pgplot -side left -fill both -expand true
#
# Bind motion events to the world coordinate x and y label widgets.
#
  bind .imagearea.pgplot <Motion> {report_motion %W %x %y}
}

#-----------------------------------------------------------------------
# This procedure is called whenever cursor motion is detected in the
# the image widget. It displays the world coordinates of the cursor
# in previously created label widgets.
#
# Input:
#  pg     The image pgplot widget.
#  x y    The X-window coordinates of the cursor.
#-----------------------------------------------------------------------
proc report_motion {pg x y} {
  global tcl_precision
  set tcl_precision 3
  .world.x configure -text "X=[$pg world x $x]"
  .world.y configure -text "Y=[$pg world y $y]"
  set tcl_precision 6
}

#-----------------------------------------------------------------------
# Create the area that contains the slice PGPLOT window.
#
# Input:
#  w            The name to give the frame widget that encloses the area.
#-----------------------------------------------------------------------
proc create_slice_area {w} {
#
# Frame the workarea.
#
  frame $w -width 11c -height 6c
#
# Create the PGPLOT slice window.
#
  pgplot $w.pgplot -share true -width 10c -height 5c -maxcolors 2 -bd 2 -bg black -fg white
#
# Position the PGPLOT widget.
#
  pack $w.pgplot -side left -fill both -expand true
#
# Arrange for the plot to be redrawn whenever the widget is resized.
#
  bind $w.pgplot <Configure> {pgdemo redraw_slice}
}

#-----------------------------------------------------------------------
# This is the main procedure of this script.
#-----------------------------------------------------------------------

# Set the title of the application window and its icon.

wm title . "Pgtkdemo"
wm iconname . "Pgtkdemo"

# Prevent other applications from sending commands to this one!

rename send {}

# Override selected widget defaults.

option add *font -Adobe-Times-Medium-R-Normal-*-140-* widgetDefault

# Set default widget colors.

set bg "#bfe5ff"
set alt_bg "#00ddff"
. configure -bg $bg
option add *background $bg widgetDefault
option add *activeBackground $bg widgetDefault
option add *activeForeground blue widgetDefault
option add *highlightBackground $bg widgetDefault
option add *troughColor $bg widgetDefault
option add *Scrollbar.width 3m widgetDefault
option add *Scrollbar.background $alt_bg widgetDefault
option add *Scrollbar*Foreground $alt_bg widgetDefault
option add *Button*Background $alt_bg widgetDefault
option add *Button*activeBackground $alt_bg widgetDefault
option add *Button*activeForeground black widgetDefault
option add *Menubutton*activeForeground black widgetDefault

# If the user uses a window-manager function to kill the demo
# arrange for the demo to exit quietly.

wm protocol . WM_DELETE_WINDOW {exit}

# Create the menu-bar.

create_main_menubar .menubar

# Create label widgets for use in displaying image world coordinates.

create_world_labels .world

# Create a PGPLOT window with scroll bars, and enclose them in a frame.
# This is the image window.

create_image_area .imagearea
#
# Create the function-selection option menu.
#
create_option_menu .function "Select a display function:" draw_image {
	"cos(R)sin(A)" "sinc(R)" "exp(-R^2/20.0)" "sin(A)" "cos(R)" "(1+sin(6A))exp(-R^2/100)"
}

#
# Create the colormap-selection option menu.
#
create_option_menu .colors "Select a color table:" recolor_image {
 grey rainbow heat aips
}

# Create a PGPLOT window with scroll bars, and enclose them in a frame.
# This is the slice window.

create_slice_area .slicearea

# Create dialogs for later display.

create_save_dialog .save
create_help_dialog .usage_help {Usage information} {
    To see a slice through the displayed image, move the mouse into
    the image display window and use any mouse button to select the
    two end points of a line.

    To display a different image select a new image function from the
    "Select a display function" option menu.
}

# Place the menubar at the top of the main window and the work-areas
# underneath it.

pack .menubar -side top -fill x
pack .world -side top -anchor w
pack .imagearea -side top -fill both -expand true
pack .function -side top -fill x
pack .colors -side top -fill x
pack .slicearea -side top -fill both -expand true

# Create the pgdemo command.

create_pgdemo pgdemo [.imagearea.pgplot device] [.slicearea.pgplot device]

# Windows in Tk do not take on their final sizes until the whole
# application has been mapped. This makes it impossible for the
# PGPLOT widget to correctly guess what size of pixmap to allocate
# when it starts the first page. To avoid this problem, force Tk
# to create all of the windows before drawing the first plot.

update idletasks

# Draw the initial image.

draw_image

