proc B1MotionBind {x y} {
#+
#  Name:
#    B1MotionBind
#
#  Purpose:
#    Process pointer motion over the image with button 1 pressed.
#
#  Arguments:
#    x
#       The screen X coord.
#    y
#       The screen Y coord.
#
#  Globals:
#     GWM_CAN (Read)
#        The name of the canvas widget holding the GWM image.
#     GWM_MODE (Read)
#        The interaction mode determining how to process button clicks
#        and motion in the GWM canvas item. Modes are:
#           0 - The user specifies image features by clicking on them.
#           1 - The user starts a new mask polygon, or edits an existing one.
#           2 - The user completes a started mask polygon.
#           3 - The user selects a label by clicking on an existing feature.
#           4 - The user selects a position and the image is redisplayed
#               centred on the supplied position.
#     GWM_ROOTI (Read)
#        The position index of the vertex being pointed at, or the position
#        index of the vertex at the start of the vector being pointed at,
#        or null if neither a vertex nor a vector is being pointed at.
#     GWM_ROOTX (Read)
#        The canvas X coordinate at which the button was pressed.
#     GWM_ROOTY (Read)
#        The canvas Y coordinate at which the button was pressed.
#     GWM_SEL_AREA (Read and Write)
#        The bounds of the selected area in the order xmin, xmax, ymin,
#        ymax. 
#     GWM_VID0 (Read)
#        The canvas item id for the vertex being pointed at (if any).
#     GWM_VID1 (Read)
#        The canvas item id for the vector ending at the vertex being 
#        pointed at (if any).
#     GWM_VID2 (Read)
#        The canvas item id for the vector starting at the vertex being 
#        pointed at (if any).
#-
   global GWM_CAN
   global GWM_MODE
   global GWM_ROOTX
   global GWM_ROOTI
   global GWM_ROOTY
   global GWM_SELCOL
   global GWM_SEL_AREA
   global GWM_VID0
   global GWM_VID1
   global GWM_VID2
   global POINTER_PXY
   global POINTER_CXY

# Convert the screen coords to canvas coords.
   set cx [$GWM_CAN canvasx $x]
   set cy [$GWM_CAN canvasy $y]

# If a cross-hair has been requested instead of a pointer, then move the
# positions of the lines making up the cross hair.
   Xhair $cx $cy

# Store the pixel coordinates of the pointer in POINTER_PXY.
   set pxy [GwmCanToNDF $cx $cy]
   set px [lindex $pxy 0]
   set py [lindex $pxy 1]
   set POINTER_PXY [format "( %.1f, %.1f )" $px $py ]

# The global variable GWM_MODE determines how events over the canvas are 
# processed. Pointer motion with button 1 pressed usually results in
# an area being selected. The only exception to this is if we are in mode
# 1 ("edit an existing polygon") AND we are pointing at a polygon vertex,
# or if we are in mode 4 ("re-centre the image"). Do nothing in mode 4.
   if { $GWM_MODE == 4 } {

# Check for other cases.
   } elseif { $GWM_MODE != 1 || $GWM_ROOTI == "" } {

# Find the min and max values on each axis of the selected area. The
# position at which the button was pressed (GWM_ROOTx,GWM_ROOTY) gives one 
# corner of the box, and the current cursor position gives the other.
      if { $cx < $GWM_ROOTX } {
         set xmin $cx
         set xmax $GWM_ROOTX
      } {
         set xmax $cx
         set xmin $GWM_ROOTX
      }
       if { $cy < $GWM_ROOTY } {
         set ymin $cy
         set ymax $GWM_ROOTY
      } {
         set ymax $cy
         set ymin $GWM_ROOTY
      }

# If there is currently no recorded selected area, create the canvas item
# (a rectangle) marking the area. Otherwise, configure the existing
# canvas item to match the current selected area.
      if { $GWM_SEL_AREA == "" } {
         $GWM_CAN create rectangle $xmin $ymin $xmax $ymax -outline $GWM_SELCOL -tags sbox
      } {
         $GWM_CAN coords sbox $xmin $ymin $xmax $ymax
      }

# Record the current selected area.
      set GWM_SEL_AREA [list $xmin $ymin $xmax $ymax]

# If we are in mode 1 ("edit an existing polygon"), and the button was
# pressed over a vertex, drag the vertex.
   } {

# Set the coordinates of the vertex marker to the current pointer coords.
      $GWM_CAN coords $GWM_VID0 $cx $cy

# Move the end of the vector which ends at the vertex.
      set coords [$GWM_CAN coords $GWM_VID1]
      $GWM_CAN coords $GWM_VID1 [lindex $coords 0] [lindex $coords 1] $cx $cy

# Move the start of the vector which starts at the vertex.
      set coords [$GWM_CAN coords $GWM_VID2]
      $GWM_CAN coords $GWM_VID2 $cx $cy [lindex $coords 2] [lindex $coords 3] 
    }
}

proc BeginUF {} {
#+
#  Name:
#     BeginUF
#
#  Purpose:
#     Mark the start of a temporary file context. Calls to this procedure
#     should be matched by calls to EndUF.
#
#  Arguments:
#     None
#
#  Returned Value:
#     An integer context identifier which can be passed to EndUF.

#  Globals:
#     IFILE (Read)
#        Temporary file names created by UniqueFile are stored in PolMap's
#        temporary ADAM_USER directory so that they are deleted when
#        PolMap terminates. They have a name of the form PolMap<i> where 
#        <i> is an integer, which is different for each file and
#        increases monotonically throughout the execution of PolMap. IFILE
#        records the value of i used in the previous call to UniqueFile.
#     IFILE_STACK (Write)
#        A stack on which is stored the value of IFILE corresponding to
#        the first temporary file to be created in the new context.
#-
   global IFILE
   global IFILE_STACK
   set nentry [llength $IFILE_STACK]
   Push IFILE_STACK [expr $IFILE + 1]
   return $nentry
}

proc Blink {w option value1 value2 interval} {
#+
#  Name:
#    Blink
#
#  Purpose:
#    Blinks a widget option.
#
#  Arguments:
#    w
#       The widget path.
#    option
#       The option to be blinked (eg "-foreground").
#    value1
#       The first option value to use (eg "red" ).
#    value2
#       The second option value to use (eg "green" ).
#    interval
#       The interval between flashes in milliseconds.
#
#  Globals:
#    STOP_BLINK (Read and Write)
#       If this is not null on entry then the blinking is stopped, and  
#       the option value is set to the value of STOP_BLINK (which is then
#       reset to null).
#-
   global STOP_BLINK

   if { [winfo exists $w] } {
      if { $STOP_BLINK == "" } {
         $w configure $option $value1
         after $interval [list Blink $w $option $value2 $value1 $interval]
      } { 
         $w configure $option $STOP_BLINK
         set STOP_BLINK ""
      }
   }
}

proc Cancel {args} {
#+
#  Name:
#     Cancel
#
#  Purpose:
#     Perform the commands associated with the "Cancel" button. Also
#     disable the cancel button if necessary.
#
#  Arguments:
#     args
#        If any argument is supplied then nothing is cancelled, but
#        the state of the button is checked, and it is disabled if
#        necessary (i.e. if there is no area selection, and there is no
#        incomplete mask polygon).
#
#  Globals:
#     GWM_CANCEL (Read)
#        The path to the "Cancel" button.
#     GWM_MODE (Read and Write)
#        The interaction mode determining how to process button clicks
#        and motion in the GWM canvas item. Modes are:
#           0 - The user specifies image features by clicking on them.
#           1 - The user starts a new mask polygon, or edits an existing one.
#           2 - The user completes a started mask polygon.
#           3 - The user selects a label by clicking on an existing feature.
#           4 - The user selects a position and the image is redisplayed
#               centred on the supplied position.
#     GWM_SEL_AREA (Read and Write)
#        The bounds of the selected area in the order xmin, xmax, ymin,
#        ymax. 
#     GWM_V0 (Read)
#        The index of the first vertex of the incomplete polygon.
#  Notes:
#    - 
#-
   global GWM_CANCEL
   global GWM_MODE
   global PRE_MODE4
   global GWM_SEL_AREA
   global GWM_V0

# Perform the cancel operation so long as no arguments were supplied.
   if { $args == "" } {

# If we are in mode 4, revert to the previous mode.
      if { $GWM_MODE == 4 } {
         SetMode $PRE_MODE4

# Cancel any selected area. Note, we need to check GWM_SEL_AREA
# explicitly (rather than just relying on CancelArea to do it),
# because CancelArea calls this procedure, so we could end up in an
# infinite call loop if we are not careful.
      } elseif { $GWM_SEL_AREA != "" } {
         CancelArea

# If no area was cancelled, cancel any incomplete mask polygon. This
# involves deleting the positions making up the polygon, and setting the 
# interaction mode back to 1 (i.e. "start or edit a polygon" mode).
      } elseif { $GWM_MODE == 2 } {
         DelPosn $GWM_V0 1
      }
   }

# If there is neither a currrent area selection nor an incomplete mask,
# nor are we in GWM_MODE 4, disable the cancel button.
   if { $GWM_SEL_AREA == "" && $GWM_MODE != 2 && $GWM_MODE != 4 } {
      $GWM_CANCEL configure -state disabled
   }
}

proc CancelArea {} {
#+
#  Name:
#     CancelArea
#
#  Purpose:
#     Cancel an area selection.
#
#  Arguments:
#     None.
#
#  Globals:
#     GWM_CAN (Read)
#        Path to the canvas containing the GWM image display.
#     GWM_DELETE (Read)
#        Path to the "Delete" button.
#     GWM_SEL_AREA (Read and Write)
#        The bounds of the selected area in the order xmin, xmax, ymin,
#        ymax. Set to a null string on exit.
#     GWM_ZOOM (Read)
#        Path to the "Zoom" button.
#     
#
#  Notes:
#    - The Zoom and Delete buttons are disabled, and the Cancel button
#    will also be disabled if there is no reason to retain it in a normal
#    state.
#-
   global GWM_CAN
   global GWM_DELETE
   global GWM_SEL_AREA
   global GWM_ZOOM

# Do nothing if no area has been selected.
   if { $GWM_SEL_AREA != "" } {

# Delete the canvas rectangle item marking the box.
      $GWM_CAN delete sbox

# Indicate that there i snow no area selected.
      set GWM_SEL_AREA ""

# Disable the GWM_ZOOM and GWM_DELETE buttons.
      $GWM_ZOOM configure -state disabled
      $GWM_DELETE configure -state disabled

# Check that the GWM_CANCEL button is in the correct state.
      Cancel check
   }
}

proc CheckMsg {action val} {
#+
#  Name:
#    CheckMsg
#
#  Purpose:
#    Checks messages created by an ADAM action for error messages. If an
#    error message is found, it is added to a global list of error messages.
#
#  Arguments:
#    action
#       The current action.
#    val
#       The value of the ADAM error message.
#
#  Globals:
#    ADAM_ERRORS (Read and Write)
#       The current list of ADAM error messages.
#    ATASK_OUTPUT (Read and Write)
#       Any non-error messages are appended to this list. Each
#       message is stored as a new element in the list.
#    LOGFILE_ID (Read)
#       The file id for any logfile to which all messages should be
#       written.
#-
   global ADAM_ERRORS
   global ATASK_OUTPUT
   global LOGFILE_ID

# Write all messages to the log file if there is one.
   if { $LOGFILE_ID != "" } {
      puts $LOGFILE_ID "$action: $val"
   }

# Error messages are distinguished from other informational messages
# by starting with one or more exclamation marks. Ignore the supplied
# message if it does not start with an exclamation mark. Otherwise,
# add it to the list (on a new line), and indicate an error has occurred.
   if { [regexp {^!+(.*)} $val match mess] } {
      if { [info exists ADAM_ERRORS] } {
         if { [string length $mess] > 30 } { 
            set ADAM_ERRORS "$ADAM_ERRORS\n$mess"
         } {
            set ADAM_ERRORS "$ADAM_ERRORS $mess"
         }
      } {
         set ADAM_ERRORS $mess
      }

# If the message is not an error message, append it as a new element to
# the list stored in ATASK_OUTPUT.
   } { 
      lappend ATASK_OUTPUT $val
   }
}

proc CheckRF {task} {
#+
#  Name:
#     CheckRF
#
#  Purpose:
#     Check that the AMS rendevous file for a task still exists. If it
#     does not (for some reason it seems to be occasionally deleted by
#     the StarTcl system, turning the process into a zombie), then the 
#     task is killed and re-loaded.
#
#  Arguments:
#     The task to be checked (previously loaded using LoadTask).
#
#  Returned Value:
#     Returns 1 if the rendevous file still exists, and zero if it 
#     did not exist (in which case the task will have been re-loaded).
#
#  Globals:
#     RENDEVOUS (Read)
#        A 1-d array, indexed by task name, storing the path to the
#        task's rendevous file.
#     TASK_FILE
#        A 1-d array, indexed by task name, storing the path to the
#        task's executable binary file (as supplied to LoadTask).
#-
   global RENDEVOUS
   global TASK_FILE

   if { ![file exists $RENDEVOUS($task)] } {
      $task kill
      Message "$task rendevous file ($RENDEVOUS($task)) has dissappeared! Re-loading the task."
      LoadTask $task $TASK_FILE($task)
      set ret 0
   } {
      set ret 1
   }

   return $ret   

}

proc CheckVal {value pause max min} {
#+
#  Name:
#    CheckVal
#
#  Purpose:
#    Check that a text string entered in a "widget" created using procedure
#    "Value" is a valid numerical value. If it is invalid, it is
#    over-written with the previous value (which should be available in 
#    global variable OLD_VAL). If it is valid, the image is re-drawn with
#    the new scalings (optionally after a short pause to allow the other
#    scaling to be set as well).
#
#  Arguments:
#    value
#       The name (note, NOT the value) of the global variable holding the
#       text string.
#    pause
#       Should a short pause be made before re-displaying the image?
#    max
#       Max allowed value.
#    min
#       Min allowed value.
#
#  Globals:
#     OLD_VAL (Read)
#        The previous value of the text string (which was a valid
#        numerical value).
#     GWM_REDISPLAY_CANCELLED (read and Write)
#        Was a previous redisplay of the image cancelled because the
#        user looked like he may be about to enter a new scaling value?
#     GWM_REDISPLAY_REQUESTED (read and Write)
#        Was a redisplay of the image requested?
#-
   global OLD_VAL
   global GWM_REDISPLAY_REQUESTED
   global GWM_REDISPLAY_CANCELLED

   upvar #0 $value val

# Check the format of the supplied string is correct. If not, re-instate
# the previous string.
   if { [scan $val "%g" val] < 1 } { set val $OLD_VAL }

# Limit the value to the supplied bounds.
   if { $val < $min } {
      set val $min
   } elseif { $val > $max } {
      set val $max
   }

# If the value has changed, or a previous redisplay was cancelled, 
# schedule a redisplay of the image.
   if { $val != $OLD_VAL || $GWM_REDISPLAY_CANCELLED } {
      set GWM_REDISPLAY_CANCELLED 0
      set OLD_VAL $val

# Set the length of the pause before the image is redisplayed.
      if { $pause } {
         set time 2500
      } {
         set time 0
      }

# Schedule the redisplay.
      set GWM_REDISPLAY_REQUESTED 1
      after $time {
         if { $GWM_REDISPLAY_REQUESTED } {
            set GWM_REDISPLAY_REQUESTED 0
            GwmUpdate
         }
      }
   }
}

proc ClearPosns {args} {
#+
#  Name:
#     ClearPosns
#
#  Purpose:
#     Erase the canvas markers and vectors for a set of positions.
#
#  Arguments:
#    args
#       An optional argument holding the name of the position list
#       to use.  If this is not supplied, it defaults to the value 
#       of global variable GWM_CURRENT_LIST.
#
#  Globals:
#     PNTID (Read and Write)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas item identifiers associated with the positions in the 
#        list. A value of -1 indicates that no marker is currently 
#        drawn for the position.
#     PNTVID (Read and Write)
#        A 1-d array indexed by list name. Each element is a list of canvas 
#        item identifiers associated with the vectors between positions in 
#        the list. A value of -1 indicates that no line is currently drawn 
#        for the position. A null string indicates that no vector is defined.
#-
   global GWM_CAN
   global PNTID
   global PNTVID

# Store the list name to use, and create the list if necessary.
   set list [GetList $args]

# Do nothing if the list is empty.
   if { [info exists PNTID($list)] } {
      set size [llength $PNTID($list)]
      if { $size > 0 } {

# Delete any canvas items marking the positions in the list. The item
# indices stored in the list are set to -1 if the marker is not currently 
# drawn. Do both the position markers and the vectors joining adjacent
# vertices in a polygon.
         for {set i 0} {$i < $size} {incr i} {
            set id [lindex $PNTID($list) $i]

            if { $id != -1 } {
               $GWM_CAN delete $id
               set PNTID($list) [lreplace $PNTID($list) $i $i -1]
            }

            set vid [lindex $PNTVID($list) $i]
            if { $vid != -1 && $vid != "" } {
               $GWM_CAN delete $vid
               set PNTVID($list) [lreplace $PNTVID($list) $i $i -1]
            }
         }
      }
   }
}

proc ColMenu {menu label var} {
#+
#  Name:
#     ColMenu
#
#  Purpose:
#     Add an item to the Colours sub-menu in the main Options menu.
#
#  Arguments:
#     menu
#        The path to the Colours sub-menu.
#     label
#        The label to use for the new item (eg "Current Objects").
#     var
#        The name of the global variable to assign the colour string 
#        to (eg "GWM_POLCOL").
#
#  Notes:
#    - The SetColours command is called whenever any of the colours are
#    changed.
#-

# Create a lower case version of the variable name.
   set lvar [string tolower $var]

# Create the new item to the Colours menu.
   $menu add cascade -label $label -menu $menu.$lvar

# Create the sub-menu.
   set thismenu [menu $menu.$lvar]

# Add the list of colours to the new sub-menu.
   foreach col "red blue green cyan yellow magenta black white" {
      $thismenu add radiobutton -label $col -variable $var \
                                -value $col -selectcolor $col \
                                -command "SetColours $var"
   }

}

proc Confirm {message} {
#+
#  Name:
#    Confirm
#
#  Purpose:
#    Get the user to confirm an operation. The supplied text is displayed
#    and the user presses one of two buttons. An indication of which
#    button was pressed is returned.
#
#  Arguments:
#    message
#       The message to display. 
#
#  Returned Value:
#   Zero is returned if the operation should be cancelled, one is
#   returned if it is ok to proceed.
#
#-

# Display the dialog box and get the user's response.
   set but [dialog .confirm "PolMap Confirmation" $message {} 0 OK Cancel]

# Return the answer.
   return [expr 1 - $but]
}

proc DecVal {value max min} {
#+
#  Name:
#    DecVal
#
#  Purpose:
#    Called to decrement the numerical value associated with a "widget" 
#    created by the procedure "Value".
#
#  Arguments:
#    value
#       The name (note, NOT the value) of the global variable holding the
#       the value to be decremented.
#    max
#       The maximum allowed value.
#    min
#       The minimum allowed value.
#
#  Globals:
#    DECS (Read)
#       The number of times the value has been decremented since the
#       decrement button ws pressed.
#-
   global DECS
   upvar #0 $value val
    set f [expr 0.01 * ( $max - $min )]
    if { $DECS == 1 } {
      set inc "0.1"
   } elseif { $DECS < 4 } {
      set inc "0.0"
   } elseif { $DECS < 10 } {
      set inc "0.2"
   } elseif { $DECS < 20 } {
      set inc "0.5"
   } elseif { $DECS < 30 } {
      set inc "1.0"
   } else {
      set inc "2.0"
   }
    set val [expr $val - $f * $inc ]
    if { $val > $max } {
      set val $max
   } elseif { $val < $min } {
      set val $min
   }

}

proc Delete {} {
#+
#  Name:
#     Delete
#
#  Purpose:
#     Delete all positions within the selected area.
#
#  Arguments:
#     None.
#
#  Globals:
#     GWM_SEL_AREA (Read)
#        The bounds of the selected area in the order xmin, xmax, ymin,
#        ymax. 
#-
   global GWM_SEL_AREA

# Issue a warning and return if there is no selected area.
   if { $GWM_SEL_AREA == "" } { 
      Message "Select an area by clicking and dragging over the image before using the \"Delete\" command."
      return 
   }

# Store the number of current position.
   set size [NumPosn ""]

# Store the bounds of the selected area.
   set xmin [lindex $GWM_SEL_AREA 0]
   set ymin [lindex $GWM_SEL_AREA 1]
   set xmax [lindex $GWM_SEL_AREA 2]
   set ymax [lindex $GWM_SEL_AREA 3]

# Loop round each current position.
   for {set i 0} {$i < $size} {incr i} {

# Get the canvas coordinates of this position.
      set cx [GetPosn $i CX]
      set cy [GetPosn $i CY]

# If it is within the selected area, delete it, and then point to the 
# previous position and reduce the size of the list by one.
      if { $cx >= $xmin && $cx <= $xmax && $cy >= $ymin && $cy <= $ymax } {
         DelPosn $i 0
         incr i -1
         incr size -1
      }
   }

# Cancel the area selection.
   CancelArea
}

proc DelPosn {i all args} {
#+
#  Name:
#    DelPosn
#
#  Purpose:
#    Deletes an position list entry.
#
#  Arguments:
#    i
#       The index of the position, starting at zero. 
#    all
#       If 1, then all positions connected by vectors to the supplied
#       position will also be deleted.
#    args
#       An optional argument holding the name of the position list
#       to use.  If this is not supplied, it defaults to the value 
#       of global variable GWM_CURRENT_LIST.
#
#  Globals:
#     GWM_CAN (Read)
#        Path to the canvas containing the GWM image display.
#     PNTCX (Write)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas X coordinates. 
#     PNTCY (Write)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas Y coordinates. 
#     PNTID (Write)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas item identifiers associated with the positions in the 
#        list. A value of -1 indicates that no marker is currently 
#        drawn for the position.
#     PNTNXT (Write)
#        A 1-d array indexed by list name. Each element is a list of 
#        integers representing indices within the lists given by PNTxxx. 
#        Each integer gives the index of the next position along the 
#        edge of a polygon. The vector starting at position index i, 
#        ends at position index given by the i'th element of
#        PNTNXT. If this value is null ("") then position i is not part of
#        a polygon. If this value is -1, then the end of the vector starting
#        position i is unspecified (in this case the polygon is not
#        closed).
#     PNTPX (Write)
#        A 1-d array indexed by list name. Each element is a list of pixel 
#        X coordinates. 
#     PNTPY (Write)
#        A 1-d array indexed by list name. Each element is a list of pixel 
#        Y coordinates. 
#     PNTVID (Write)
#        A 1-d array indexed by list name. Each element is a list of canvas 
#        item identifiers associated with the vectors between positions in 
#        the list. A value of -1 indicates that no line is currently drawn 
#        for the position. A null string indicates that no vector is defined.
#     GWM_V0 (Read and Write)
#        The index of the position corresponding to the first vertex in
#        an incomplete (i.e. open) polygon.
#     GWM_V1 (Read and Write)
#        The index of the position corresponding to the last vertex in
#        an incomplete (i.e. open) polygon.
#     GWM_VCX0 (Write)
#        The canvas X coordinates at the first vertex in an incomplete 
#        (i.e. open) polygon.
#     GWM_VCY0 (Write)
#        The canvas Y coordinates at the first vertex in an incomplete 
#        (i.e. open) polygon.
#     GWM_VID2 (Write)
#        The canvas item id of the vector joining the last vertex in an
#        incomplete (i.e. open) polygon, to the pointer.
#
#-
   global GWM_CAN
   global PNTCY
   global PNTCX
   global PNTID
   global PNTNXT
   global PNTPY
   global PNTPX
   global PNTVID
   global GWM_V0
   global GWM_V1     
   global GWM_VCX0
   global GWM_VCY0
   global GWM_VID2

# Store the list name to use, and create the list if necessary.
   set list [GetList $args]

# Note the next vertex in the polygon (if any).
   set next [lindex $PNTNXT($list) $i]

# If in a polygon, get the number of vertices in the same polygon.
   if { $next != "" } {
      set nvert [NumPosn $i $list]
   }

# Get the size of the list.
   if { [info exists PNTID($list)] } {
      set size [llength $PNTID($list)]
   } {
      set size 0
   }

# If no position with the supplied index exists, leave the lists unchanged.
   if { $i >= 0 && $i < $size } {

# Delete any canvas item marking the position.
      set id [lindex $PNTID($list) $i]
      if { $id != -1 } {
         $GWM_CAN delete $id
         set PNTID($list) [lreplace $PNTID($list) $i $i -1 ]
      }

# If the position has an associated vector, delete it. Store the end
# coordinates of the line first.
      set idedge [lindex $PNTVID($list) $i]
      if { $idedge != "" && $idedge != -1 } {
         set coords [$GWM_CAN coords $idedge]
         set xn [lindex $coords 2]
         set yn [lindex $coords 3]
         $GWM_CAN delete $idedge
         set PNTVID($list) [lreplace $PNTVID($list) $i $i -1 ]
      } {
         set xn ""
      }

# Store the index of the position to which any vector associated with the
# current position pointed.
      set k [lindex $PNTNXT($list) $i]

# If the position being deleted is the first in a polygon, modify the
# index and coords of the first position.
      if { $i == $GWM_V0 } {
         if { $k != "" && $k != -1 } {
            set GWM_V0 $k
            set GWM_VCX0 [lindex $PNTCX($list) $k]
            set GWM_VCY0 [lindex $PNTCX($list) $k]
         } {
            set GWM_V0 ""
            set GWM_VCX0 ""
            set GWM_VCY0 ""
         }
      }

# Check for any vectors associated with other positions which end at the
# position being deleted. Modify such vectors so that they end instead at
# the position at which the vector associated with the current position
# ended. If no vector was associated with the current position, delete all
# vectors which end at the current position. Also, update the index of the
# position to which these vectors point.
      set jj ""
      for {set j 0} {$j < $size} {incr j} {
         if { [lindex $PNTNXT($list) $j] == $i } {
            set jj $j
            set lid2 [lindex $PNTVID($list) $j]

            if { $lid2 != "" && $lid2 != -1 } {
               if { $xn != "" } {
                  set coords [$GWM_CAN coords $lid2]
                  set cxbeg [lindex $coords 0]
                  set cybeg [lindex $coords 1]
                  $GWM_CAN coords $lid2 $cxbeg $cybeg $xn $yn
               } {
                  $GWM_CAN delete $lid2
                  set PNTVID($list) [lreplace $PNTVID($list) $j $j -1 ]
               }
            }
             set PNTNXT($list) [lreplace $PNTNXT($list) $j $j $k]
          }
      }

# If the position being deleted is the last in a polygon, modify the
# index and coords of the last position.
      if { $i == $GWM_V1 } {
         if { $jj != "" } {
            set GWM_V1 $jj
            set GWM_VID2 $lid2
         } {
            set GWM_V1 ""
            set GWM_VID2 ""
         }
      }

# Remove the entry from the lists.
      set PNTCX($list) [lreplace $PNTCX($list) $i $i]
      set PNTCY($list) [lreplace $PNTCY($list) $i $i]
      set PNTPX($list) [lreplace $PNTPX($list) $i $i]
      set PNTPY($list) [lreplace $PNTPY($list) $i $i]
      set PNTID($list) [lreplace $PNTID($list) $i $i]
      set PNTVID($list) [lreplace $PNTVID($list) $i $i]
      set PNTNXT($list) [lreplace $PNTNXT($list) $i $i]

# Decrement the length of the list.
      incr size -1

# References to positions indices higher than the one just deleted now need
# to be reduced by one.
      for {set j 0} {$j < $size} {incr j} {
         set l [lindex $PNTNXT($list) $j]
         if { $l > $i } {
            set l [expr $l - 1]
            set PNTNXT($list) [lreplace $PNTNXT($list) $j $j $l]
         }
      }

# If the supplied position was connected to another (different)
# position, delete that one if required.
      if { $all } {
         if { $k != "" && $k != $i && $k != -1 } {
            if { $k > $i } { set k [expr $k - 1] }
            DelPosn $k 1 $list
         }

      } {

# If the deleted position was part of a polygon with less than 4 vertices, 
# delete the entire polygon. If there was only a single vertex in the
# polygon, then it will already have been deleted.
         if { $next != "" } {
            if { $nvert > 1 && $nvert < 4 } {
               DelPosn $next 1 $list
            }
         }
      }
   }

# If we have just deleted an entire polygon, switch back to mode 1.
   if { $all } { SetMode 1 }

}

proc DoubleClick {name args} {
#+
#  Name:
#    DoubleClick
#
#  Purpose:
#    Coordinates execution of commands bound to double and single clicks
#    of a button in a widget. It can be used to prevent execution of a 
#    single click command if a double click command has also been activated
#    by the same click. The single click command should call this
#    procedure with "args" not supplied, and should only proceed if it 
#    returns a zero value. The double click command should call this
#    procedure immediately on entry, specifying any arbitrary value for 
#    "args" (the returned value should be ignored).
#
#  Arguments:
#     name
#        The name of a global variable by which the single and double click 
#        commands want to communicate.
#     args
#        If any extra arguments are supplied, then the global variable with 
#        name supplied in "name" is set to 1, and the procedure returns
#        immediately. Otherwise, the global variable is set to zero, the 
#        procedure pauses for 250 milliseconds, and then returns the value of
#        the global variable (which should have been set to 1 during the pause
#        if the corresponding double click command started up).
#
#  Returned Value:
#     A boolean flag indicating if the double click command has been 
#     activated.
#-
   upvar #0 $name trigger

   if { $args == "" } {
      set trigger 0
      after 250 {set a 1}
      tkwait variable a
   } {
      set trigger 1
   }

   return $trigger
}   

proc DrawPosns {args} {
#+
#  Name:
#     DrawPosns
#
#  Purpose:
#     Draws the canvas markers and vectors for a set of positions.
#
#  Arguments:
#     args
#        An optional argument holding the name of the position list
#        to use.  If this is not supplied, it defaults to the value 
#        of global variable GWM_CURRENT_LIST.
#
#  Globals:
#     GWM_CAN (Read)
#        Path to the canvas containing the GWM image display.
#     GWM_V0 (Write)
#        The index of the position corresponding to the first vertex in
#        an incomplete (i.e. open) polygon.
#     GWM_V1 (Write)
#        The index of the position corresponding to the last vertex in
#        an incomplete (i.e. open) polygon.
#     GWM_VCX0 (Write)
#        The canvas X coordinates at the first vertex in an incomplete 
#        (i.e. open) polygon.
#     GWM_VCY0 (Write)
#        The canvas Y coordinates at the first vertex in an incomplete 
#        (i.e. open) polygon.
#     GWM_VID2 (Write)
#        The canvas item id of the vector joining the last vertex in an
#        incomplete (i.e. open) polygon, to the pointer.
#     PNTCX (Read)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas X coordinates. 
#     PNTCY (Read)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas Y coordinates. 
#     PNTID (Read)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas item identifiers associated with the positions in the 
#        list. A value of -1 indicates that no marker is currently 
#        drawn for the position.
#     PNTNXT (Read)
#        A 1-d array indexed by list name. Each element is a list of 
#        integers representing indices within the lists given by PNTxxx. 
#        Each integer gives the index of the next position along the 
#        edge of a polygon. The vector starting at position index i, 
#        ends at position index given by the i'th element of
#        PNTNXT. If this value is null ("") then position i is not part of
#        a polygon. If this value is -1, then the end of the vector starting
#        position i is unspecified (in this case the polygon is not
#        closed).
#     PNTPX (Read)
#        A 1-d array indexed by list name. Each element is a list of pixel 
#        X coordinates. 
#     PNTPY (Read)
#        A 1-d array indexed by list name. Each element is a list of pixel 
#        Y coordinates. 
#     PNTVID (Read)
#        A 1-d array indexed by list name. Each element is a list of canvas 
#        item identifiers associated with the vectors between positions in 
#        the list. A value of -1 indicates that no line is currently drawn 
#        for the position. A null string indicates that no vector is defined.
#
#  Notes:
#     -  If an incomplete (i.e. open) polygon is drawn, then the GWM
#     interaction mode is set to 2 ("Complete an incomplete polygon").
#-
   global GWM_CAN
   global GWM_POLCOL
   global PNTCX
   global PNTCY
   global PNTID
   global PNTNXT
   global PNTPX
   global PNTPY
   global PNTVID
   global GWM_V0
   global GWM_V1     
   global GWM_VCX0
   global GWM_VCY0
   global GWM_VID2

# Store the list name to use, and create the list if necessary.
   set list [GetList $args]

# Do nothing if the list is empty.
   if { [info exists PNTID($list)] } {
      set size [llength $PNTID($list)]
      if { $size > 0 } {

# Store the colour to use, and get the pixel coordinates to use.
         set colour $GWM_POLCOL
         set pxlist $PNTPX($list)
         set pylist $PNTPY($list)

# Convert the pixel coordinates to canvas coordinates.
         set PNTCX($list) ""
         set PNTCY($list) ""

         for {set i 0} {$i < $size} {incr i} {
            set px [lindex $pxlist $i]
            set py [lindex $pylist $i]
            set cxy [GwmNDFToCan $px $py]
            if { $cxy == "" } { return } 
            lappend PNTCX($list) [lindex $cxy 0]
            lappend PNTCY($list) [lindex $cxy 1]
         }

# Now loop round them all again, drawing any vectors.
         for {set i 0} {$i < $size} {incr i} {

# Get the index of the position at the "far end" of the vector (if any)
# which starts at the current position.
            set next [lindex $PNTNXT($list) $i]

# If this value is not null, there is a vector.
            if { $next != "" } {

# If there is a vector to be drawn, get the canvas coordinates at the 
# current (starting) position.
               set cx [lindex $PNTCX($list) $i]
               set cy [lindex $PNTCY($list) $i]

# A value of -1 for $next indicates that the vector end should be bound to
# the pointer. We need some extra information in global to do this...
               if { $next == -1 } {

# Find the index (GWM_V0) of the position at the start of the incomplete polygon. 
# This is a positions which does not have any other position pointing to
# it.
                  set j $i
                  while { $j < $size } {

                     set GWM_V0 $j
                     set j 0
                  
                     while { $j < $size && [lindex $PNTNXT($list) $j] != $GWM_V0 } {
                        incr j
                     }

                  }

# Store the canvas coordinates at this position.
                  set GWM_VCX0 [lindex $PNTCX($list) $GWM_V0]
                  set GWM_VCY0 [lindex $PNTCY($list) $GWM_V0]

# Initially make the vector zero length. 
                  set cxn $cx
                  set cyn $cy

# Enter GWM interaction mode 2 ("complete an unfinished mask polygon").
                  SetMode 2

# Any value other than -1 for $next indicates that the vector ends at the
# position with index $next. Get the canvas coords of that position.
               } {
                  set cxn [lindex $PNTCX($list) $next]
                  set cyn [lindex $PNTCY($list) $next]
               }

# If a canvas item marking the vector has not already been created,
# create one now, and store its index.
               set vid [lindex $PNTVID($list) $i]
               if { $vid == -1 } {
                  set vid [$GWM_CAN create line $cx $cy $cxn $cyn -fill $colour -tags vectors]
                  set PNTVID($list) [lreplace $PNTVID($list) \
                                                       $i $i $vid]

# If a canvas item already exists, configure it to have the correct
# properties.
               } {
                  $GWM_CAN coords $vid $cx $cy $cxn $cyn 
                  $GWM_CAN itemconfigure $vid -fill $colour
               }                   

# If this is the "loose" end of an incomplete mask polygon, store the
# list index of the position, and the canvas item id of the vector in
# global.
               if { $next == -1 } {
                  set GWM_VID2 $vid
                  set GWM_V1 $i
               }
            }
         }

# Now loop round them all again, drawing the markers over the top of the
# vectors (so that the vectors can be picked up easily in GWM_MODE 1).
         for {set i 0} {$i < $size} {incr i} {

# Get the canvas coordinates at the position.
            set cx [lindex $PNTCX($list) $i]
            set cy [lindex $PNTCY($list) $i]

# Draw the posiion marker.
            MarkPosn $i $list
         }
      }
   }
}

proc EndUF {context leave} {
#+
#  Name:
#     EndUF
#
#  Purpose:
#     Mark the end of a temporary file context. Calls to this procedure
#     should be matched by calls to BeginUF. This procedure deletes all
#     temprary files created since the correspinding call to BeginUF, except
#     for any files included in the argument "leave".
#
#  Arguments:
#     context 
#        A context identifier returned by BeginUF. All contexts contained
#        with the specified context are also ended.
#     leave
#        A list of files which are to be escaped into the next higher
#        context. 
#
#  Globals:
#     ADAM_USER (Read)
#       The path to the temporary ADAM_USER directory used by PolMap.
#     IFILE (Read)
#        Temporary file names created by UniqueFile are stored in PolMap's
#        temporary ADAM_USER directory so that they are deleted when
#        PolMap terminates. They have a name of the form PolMap<i> where 
#        <i> is an integer, which is different for each file and
#        increases monotonically throughout the execution of PolMap. IFILE
#        records the value of i used in the previous call to UniqueFile.
#     IFILE_STACK (Write)
#        A stack on which is stored the value of IFILE corresponding to
#        the first temporary file created in the current context.
#-
   global IFILE
   global IFILE_STACK
   global ADAM_USER

# Loop round each value of IFILE used in this context. This starts with
# the value stored by the corresponding call to BeginUF, and ends with
# the current value.
   set levels [expr [llength $IFILE_STACK] - $context ]
   set ifile_start [Pop IFILE_STACK $levels]
   for {set i $ifile_start} {$i <= $IFILE} {incr i} {

# Construct the corresponding file name (NB, make sure this next line keeps
# in step with any changes made in rocedure UniqueFile).
      set file "$ADAM_USER/PolMap$i"

# If the file exists (with any file extension), but has not been included
# in the supplied list of files to be escaped, then delete the file.
      if { [lsearch -exact $leave $file] == -1 } {
         foreach f [glob -nocomplain ${file}.*] {
            catch "exec rm -f $f"
         }
      }
   }
}

proc exit {args} {
#+
#  Name:
#    exit
#
#  Purpose:
#    Shutdown the tcl script, cleaning up PolMap internals in the process.
#
#  Arguments:
#    args
#       The exit integer status value.
#
#  Globals:
#    ADAM_TASKS (Read)
#       A list of the names of the ADAM tasks started up by PolMap.
#    ADAM_USER (Read)
#       The path to the temporary ADAM_USER directory used by PolMap.
#    ATASK (Read)
#       Was this script activated form the PolMap ATASK?
#    OLD_ADAM_USER (Read)
#       The original value of the ADAM_USER environment variable, or a null
#       string if ADAM_USER was not defined.
#    OLDKAPPA (Read)
#       A list of process id.s for any KAPPA processes which were running
#       when PolMap was started.
#
#  Notes:
#    - This command replaces the built-in Tcl "exit" command, which should
#    have been renamed as "tcl_exit".
#-
   global ADAM_TASKS
   global ADAM_USER
   global ATASK
   global env
   global LOGFILE_ID
   global OLD_ADAM_USER
   global OLD_AGI_USER
   global OLDKAPPA

# Close any log file.
   if { $LOGFILE_ID != "" } { close $LOGFILE_ID }

# Kill all the ADAM tasks started up by PolMap.
   foreach task $ADAM_TASKS {
      if { [info commands $task] != "" } {
         $task kill
      }
   }

# Delete the temporary ADAM_USER directory created at the start.
   catch "exec rm -rf $ADAM_USER"

# Trap pids for all current KAPPA processes.
   if { ![catch {exec ps | grep kappa | grep -v grep | \
                 awk {{print $1}}} newkappa] } {
      set newkappa {}
   }

# Kill any new processes (i.e ones which are not in the list of KAPPA
# processes which were active when PolMap started).
   foreach newpid $newkappa {
      set dokill 1

      foreach oldpid $OLDKAPPA {
         if { $newpid == $oldpid } {
            set dokill 0
         }
      }

      if { $dokill } {
         catch "exec kill -SYS $newpid"
      }
    }

# Re-instate the original ADAM_USER and AGI_USER environment variables.
   if { $OLD_ADAM_USER != "" } {
      set env(ADAM_USER) $OLD_ADAM_USER
   } {
      unset env(ADAM_USER)
   }

   if { $OLD_AGI_USER != "" } {
      set env(AGI_USER) $OLD_AGI_USER
   } {
      unset env(AGI_USER)
   }

# We now Tcl "exit" command if we are not running from the A-task, or
# by destroying the main window if we are running from the A-task. We do not 
# use the built-in Tcl "exit" command in this case because "exit" kills 
# the current process, which would result in the a-task dying in an 
# uncontrolled manner. Note, "adamtask.tcl" may have set up a binding which 
# causes this procedure ("exit") to be called when the main window is 
# destroyed. This would put us into an infinite loop, so delete any 
# <Destroy> binding first.
   if { $ATASK } {
      bind . <Destroy> ""
      destroy .
   } {
      tcl_exit
   }

}

proc Extension {ndf comp type value old} {
#+
#  Name:
#     Extension
#
#  Purpose:
#     Retrive the current value (if any) for a specified NDF extension
#     component, and optionally replace it with a new value.
#
#  Arguments:
#     ndf
#        The NDF to be used.
#     comp
#        The name of the extension component.
#     type
#        The HDS type of the extension component.
#     value
#        The new value to assign to the component. If this is null, the
#        component is left unchanged.
#     old
#        The name of a variable in which to place the current value of
#        the component. This is set to null if the component does not
#        exist in the supplied NDF. If the supplied name is null,
#        then no value is returned.
#
#  Returned Value:
#     1 for success, zero if anything went wrong in KAPPA:SETEXT.
#
#  Globals:
#     ATASK_OUTPUT (Read and Write)
#        A list containing any non-error messages produced by the most 
#        recently run A-task. Each message is stored as a new element in 
#        the list.
#
#  Notes:
#    - All operations act on an extension with name and type equal to
#    "POLPACK".
#-

   global ATASK_OUTPUT

# Assume success.
   set ok 1

# See if the extension item exists, and get its value if it does.
   set old_value ""
   if { [Obey ndfpack setext "cname=$comp ndf=$ndf loop=no option=get xname=POLPACK xtype=POLPACK"] } {
      foreach mess $ATASK_OUTPUT {
         if { [regexp -nocase "^ *$comp +$type\[^ \]* +(\[^ \]+\$)" $mess match old_value] } {
            break
         } {
            set old_value ""
         }
      }
   }

# If a new value has been supplied...
   if { $value != "" } {

# Delete any existing item.
      if { $old_value != "" } {
         if { ![Obey ndfpack setext "cname=$comp ndf=$ndf loop=no option=erase xname=POLPACK xtype=POLPACK"] } {
            set ok 0
         } 
      }

# And add the new item.
      if { $ok && ![Obey ndfpack setext "cname=$comp cvalue=$value ndf=$ndf loop=no option=put xname=POLPACK xtype=POLPACK ctype=$type shape=0"] } {
         set ok 0
      }
   }

# Return the old value if a variable has been supplied for it.
   if { $old != "" } { 
      upvar $old old_val
      set old_val $old_value
   }

# Return the status.
   return $ok
}

proc FindHelp {x y} {
#+
#  Name:
#     FindHelp
#
#  Purpose:
#     Find the htx cross-reference label associated with particular
#     root coordinates.
#
#  Arguments:
#     x y
#        The X and Y root coordinates.
#
#  Returned Value:
#     The HTX cross-reference label assocaited with the given position,
#     or a null string if there is no associated label.
#
#  Globals:
#    HELP_LABELS (Read)
#       A 1-D array index by widget name. Each element is an htx
#       cross-reference label to be displayed if the widget is selected
#       using "Help on pointer".
#     
#-
   global HELP_LABELS

# Find the lowest level window at the given root coordinates.    
   set w [winfo containing $x $y]

# Assume there is no label associated with this position.
   set help_label ""

# Loop until we find a label or we have checked all levels in the
# widget's family tree.
   while { $w != "" } {

# If this widget has a label, return it.
      if { [info exists HELP_LABELS($w)] } {
         set help_label $HELP_LABELS($w)
         break

# Otherwise, find the father of the current widget.
      } {
         set w [winfo parent $w]
      }
   }

# Return the label
   return $help_label

}

proc FindPosn {names values args} {
#+
#  Name:
#     FindPosn
#
#  Purpose:
#     Find a position which satisfies the supplied criteria.
#
#  Arguments:
#     names
#        A list of the position parameters which are to be matched. These
#        can be selected from:
#           PX - The pixel X coordinate of the position.
#           PY - The pixel Y coordinate of the position.
#           CX - The canvas X coordinate of the position.
#           CY - The canvas Y coordinate of the position.
#           ID - The canvas item id of the marker for the position (-1
#                if no marker is currently drawn).
#           VID - The canvas item id of the line joining the position to
#                the position given by parameter NXT (-1 if no line is 
#                currently drawn, and null ("") if NXT is undefined).
#           NXT - The index of the position corresponding to the next 
#                vertex in a polygonal mask (-1 if there is no "next
#                vertex" (i.e. if the polygon is open) and null ("") if this 
#                position is not part of a polygon). 
#     values
#        A list of values corresponding to the parameter names supplied
#        in "names".
#     args
#        An optional argument holding the name of the position list
#        to use.  If this is not supplied, it defaults to the value 
#        of global variable GWM_CURRENT_LIST.
#
#  Returned Value:
#     The index of the first position with the supplied parameter values
#     (the values of other unspecified parameters are ignored). A null
#     value is returned if no matching position is found.
#
#  Globals:
#     PNTCX (Read)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas X coordinates. 
#     PNTCY (Read)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas Y coordinates. 
#     PNTID (Read)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas item identifiers associated with the positions in the 
#        list. A value of -1 indicates that no marker is currently 
#        drawn for the position.
#     PNTNXT (Read)
#        A 1-d array indexed by list name. Each element is a list of 
#        integers representing indices within the lists given by PNTxxx. 
#        Each integer gives the index of the next position along the 
#        edge of a polygon. The vector starting at position index i, 
#        ends at position index given by the i'th element of
#        PNTNXT. If this value is null ("") then position i is not part of
#        a polygon. If this value is -1, then the end of the vector starting
#        position i is unspecified (in this case the polygon is not
#        closed).
#     PNTPX (Read)
#        A 1-d array indexed by list name. Each element is a list of pixel 
#        X coordinates. 
#     PNTPY (Read)
#        A 1-d array indexed by list name. Each element is a list of pixel 
#        Y coordinates. 
#     PNTVID (Read)
#        A 1-d array indexed by list name. Each element is a list of canvas 
#        item identifiers associated with the vectors between positions in 
#        the list. A value of -1 indicates that no line is currently drawn 
#        for the position. A null string indicates that no vector is defined.
#
#-
   global PNTCX
   global PNTCY
   global PNTID
   global PNTNXT
   global PNTPX
   global PNTPY
   global PNTVID

# Initialise the returned value.
   set ret ""

# Store the list name to use, and create the list if necessary.
   set list [GetList $args]

# Do nothing if the list is empty.
   if { [info exists PNTID($list)] } {
      set size [llength $PNTID($list)]
      if { $size > 0 } {

# Loop round each position.
         for {set i 0} { $i < $size } { incr i } {
            set ret $i

# Check each supplied parameter value
            set j 0
            foreach name $names {  
               set supval [lindex $values $j]
               incr j

# Get the value of the parameter for the current position.
               upvar #0 PNT$name array
               set curval [lindex $array($list) $i]

# If it is different to the supplied position, set ret null and break out of 
# the loop. Pixel coordinates are given a tolerance of +/- 2 pixels.
               if { $name == "PX" || $name == "PY" } {
                  if { $curval > [expr $supval + 2] || $curval < [expr $supval - 2] } {
                     set ret ""
                     break
                  }
               } {
                  if { $curval != $supval } {
                     set ret ""
                     break
                  }
               }
            }

# If we have found a match, break out of the loop.
            if { $ret != "" } {
               break
            }
         }
      }
   }

# Return the index.
   return $ret
}

proc Finish {save} {
#+
#  Name:
#     Finish
#
#  Purpose:
#     Exit PolMap, warning the user if the output images have not yet
#     been saved.
#
#  Arguments:
#     save
#        Should the output images be saved before asking the user whether
#        or not to exit?
#
#  Globals:
#     RESAVE (Read)
#        Set to zero if the the mappings and masks have not changed since
#        the last time the output images were saved. Set to a non-zero
#        value if the output images are out-of-date with respect to the 
#        mappings and/or masks.
#-
   global RESAVE

# If required, save the output images.
   if { $save && $RESAVE } { Save }

# Construct a suitable confirmation question, depending on whether or not
# the output images have been saved.
   if { $RESAVE } {
      set quest "The output images have not yet been saved!\n\nQuit PolMap?"   
   } {
      set quest "Exit PolMap?"   
   }

# See if the user confirms the intention to exit. If so, exit.
   if { [Confirm $quest] } { exit }

}

proc GetItems {} {
#+
#  Name:
#     GetItems
#
#  Purpose:
#     Display a dialog box, and get a list of status items to display.
#
#  Arguments:
#     None.
#
#  Globals:
#     CB_COL (Read) 
#        The colour to use for the check buttons in the dialog box.
#     GETITEMS_EXIT (Read and Write)
#        Used to communicate with the widgets.
#     SAREA (Read)
#         Should the status area be displayed?
#     SI_LABELS (Read)
#        A list containing the label for each status item, in the
#        order they were defined.
#     SI_LIST (Read and Write)
#        A list containing integer identifiers for each status item to be
#        displayed. These integers are indices into the SI_LABELS, SI_HELPS
#        and SI_VARS lists.
#     SI_ON (Write)
#        A list containing flags indicating if each of the status items
#        is displayed or not.
#     SI_VARS (Read)
#        A list containing the global variable name for each status item, 
#        in the order they were defined.
#     
#  Notes:
#    - 
#-
   global CB_COL
   global GETITEMS_EXIT
   global SAREA 
   global SI_LABELS
   global SI_LIST
   global SI_ON
   global SI_VARS

# Create the top level window for the dialogue box, and set its title.
   set top .items
   set topf [MakeDialog $top "Select status items" 1]

# Save the list of status item identifiers for the items curently
# displayed.
   set si_list_old $SI_LIST 

# Find the length of the longest label.
   set maxl 0
   foreach label $SI_LABELS {
      set l [string length $label]
      if { $l > $maxl } { set maxl $l }
   }   

# Create a frame to put the checkbuttons in.
   set fr0 [frame $topf.fr0]
   pack $fr0 -side top
   SetHelp $fr0 ".  Click the buttons corresponding to the items of status information to be displayed."

# Create two columns within this frame.
   set fr1 [frame $fr0.fr1]
   set fr2 [frame $fr0.fr2]
   pack $fr1 $fr2 -side left -anchor n -padx 4m -pady 4m

# Create a check button for each status item, place them alternately in
# the left and right of the two frames just created.
   set f $fr1   
   for {set i 0} {$i < [llength $SI_VARS]} {incr i} {

      if { [lsearch -exact $SI_LIST  $i] > -1 } {
         set SI_ON($i) 1
      } {
         set SI_ON($i) 0
      }

      set cb($i) [checkbutton $f.cb_$i -selectcolor $CB_COL -variable SI_ON($i) \
                  -highlightthickness 0 -width $maxl -anchor nw \
                  -text "[lindex $SI_LABELS $i]" -command \
        "if { \$SI_ON($i) } {
            lappend SI_LIST $i
         } {
            set j \[lsearch \$SI_LIST $i \]
            if { \$j > -1 } {
               set SI_LIST \[lreplace \$SI_LIST \$j \$j]
            }
         }"]
        
      pack $cb($i) -side top -pady 1m -anchor nw 

      if { $f == $fr1 } {
         set f $fr2
      } {
         set f $fr1
      }
   }

# Create a frame for the buttons.
   set fr3 [frame $topf.fr3]
   pack $fr3 -side top -expand 1 -fill x

# Create the OK, Apply and Cancel buttons.
   set b1 [button $fr3.ok -text "OK" -command "set GETITEMS_EXIT ok"]
   set b2 [button $fr3.apply -text "Apply" -command "set GETITEMS_EXIT apply"]
   set b3 [button $fr3.cancel -text "Cancel" -command "set GETITEMS_EXIT cancel"]
   pack $b1 $b2 $b3 -padx 1m -side left -expand 1
   SetHelp $b1 ".  Press to close the dialog box, re-displaying the status area with the selected items."
   SetHelp $b2 ".  Press to re-display the status area without exiting."
   SetHelp $b3 ".  Press to close the dialog box, ignoring any changes made to the list of displayed status items."

# Create the ClearAll and SetAll and Help buttons.
   set b4 [button $fr3.cl -text "ClearAll" -command "set GETITEMS_EXIT clear"]
   set b5 [button $fr3.st -text "SetAll" -command "set GETITEMS_EXIT set"]
   set b6 [button $fr3.help -text "Help" -command "set GETITEMS_EXIT help"]
   pack $b4 $b5 $b6 -padx 1m -side left -expand 1
   SetHelp $b4 ".  Press to clear all check buttons."
   SetHelp $b5 ".  Press to set all check buttons."
   SetHelp $b6 ".  Press to see more help on this window."

# Ensure that closing the window from the window manager is like pressing
# the Cancel button.
   wm protocol $top WM_DELETE_WINDOW "set GETITEMS_EXIT cancel"

# Loop until an exit button is pressed.
   set exit 0
   while { !$exit } {

# Wait for the user to press a button.
      tkwait variable GETITEMS_EXIT

# If the cancel button was pressed, re-instate the original value of
# SI_LIST, re-display the status area if any changes have been "applied",
# and exit.
      if { $GETITEMS_EXIT == "cancel" } {
         if { $SI_LIST != $si_list_old } {
            set SI_LIST $si_list_old 
            if { $SAREA } {
               StatusArea 0
               StatusArea 1              
            }
         }
         set exit 1

# If the Apply button was pressed, re-draw the status area if it is currently 
# displayed, but do not exit.
      } elseif { $GETITEMS_EXIT == "apply" } {
         if { $SAREA } {
            StatusArea 0
            StatusArea 1              
         }

# If the OK button was pressed, re-draw the status area if it is currently 
# displayed, and exit.
      } elseif { $GETITEMS_EXIT == "ok" } {
         if { $SAREA } {
            StatusArea 0
            StatusArea 1              
         }
         set exit 1

# If the ClearALl button was pressed, clear all the check button variables.
      } elseif { $GETITEMS_EXIT == "clear" } {
         for {set i 0} {$i < [llength $SI_VARS]} {incr i} {
            if { $SI_ON($i) } { $cb($i) invoke }
         }

# If the SetAll button was pressed, set all the check button variables.
      } elseif { $GETITEMS_EXIT == "set" } {
         for {set i 0} {$i < [llength $SI_VARS]} {incr i} {
            if { !$SI_ON($i) } { $cb($i) invoke }
         }

# If the Help button was pressed, display help.
      } elseif { $GETITEMS_EXIT == "help" } {
         ShowHelp "POLMAP_STATUS_ITEMS_DIALOG" 
      }
   }

# Destroy the dialog box.
   destroy $top

}

proc GetList {args} {
#+
#  Name:
#     GetList
#
#  Purpose:
#     Obtain the name of the positions list to use, and ensure the arrays 
#     holding the list exist.
#
#  Arguments:
#    args
#       An optional argument holding the name of the position list
#       to use.  If this is not supplied, it defaults to the value 
#       of global variable GWM_CURRENT_LIST.
#
#  Returned Value:
#     The position list to use.
#
#  Globals:
#     GWM_CURRENT_LIST (Write)
#        The name of the default list to use if "args" is not supplied.
#     PNTCX (Write)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas X coordinates. 
#     PNTCY (Write)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas Y coordinates. 
#     PNTID (Write)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas item identifiers associated with the positions in the 
#        list. A value of -1 indicates that no marker is currently 
#        drawn for the position.
#     PNTNXT (Write)
#        A 1-d array indexed by list name. Each element is a list of 
#        integers representing indices within the lists given by PNTxxx. 
#        Each integer gives the index of the next position along the 
#        edge of a polygon. The vector starting at position index i, 
#        ends at position index given by the i'th element of
#        PNTNXT. If this value is null ("") then position i is not part of
#        a polygon. If this value is -1, then the end of the vector starting
#        position i is unspecified (in this case the polygon is not
#        closed).
#     PNTPX (Write)
#        A 1-d array indexed by list name. Each element is a list of pixel 
#        X coordinates. 
#     PNTPY (Write)
#        A 1-d array indexed by list name. Each element is a list of pixel 
#        Y coordinates. 
#     PNTVID (Write)
#        A 1-d array indexed by list name. Each element is a list of canvas 
#        item identifiers associated with the vectors between positions in 
#        the list. A value of -1 indicates that no line is currently drawn 
#        for the position. A null string indicates that no vector is defined.
#-
   global GWM_CURRENT_LIST
   global PNTCX
   global PNTCY
   global PNTPX
   global PNTPY
   global PNTID
   global PNTVID
   global PNTNXT

# Store the list name to use.
   if { $args != "" } {
      set list [lindex $args 0]      
   } {
      set list ""
   }  

   if { $list == "" } {
      set list $GWM_CURRENT_LIST
   }

# Create the list if it does not already exist.
   if { ![info exists PNTID($list)] } {
      set PNTCX($list) ""
      set PNTCY($list) ""
      set PNTPX($list) ""
      set PNTPY($list) ""
      set PNTID($list) ""
      set PNTVID($list) ""
      set PNTNXT($list) ""
   }

# Return the list name.
   return $list

}

proc GetParam {task param} {
#+
#  Name:
#    GetParam
#
#  Purpose:
#    Get a parameter value from an ADAM task.
#
#  Arguments:
#    task
#       The name of the task (eg "kapview").
#    param
#       The name of the parameter in the form "action:param" 
#       (eg "datapic:ncx1").
#
#  Returned Value:
#    The parameter value.
#
#  Globals:
#    PAR_VALUE (Write)
#       The most recently acquired parameter value.
#
#  Notes:
#    - This procedure does not return until the parameter value has been
#    obtained.
#
#-
   global PAR_VALUE

   global PAR_GOT

# Issue the request for the parameter value.
   set PAR_GOT 0
   $task get $param -getresponse {set PAR_VALUE %V;set PAR_GOT 1}

# Wait until the request has been fulfilled.
   WaitFor PAR_GOT

# Return the parameter value.
   return $PAR_VALUE
}

proc GetPars {vars ntypes nlabels nlimits title help dhelp} {
#+
#  Name:
#     GetPars
#
#  Purpose:
#     Obtain a set of values of various data types from the user.
#     A dialog box is displayed, containing a set of labeled entry
#     widgets, check buttons and radio buttons. Checks are made that 
#     the supplied values are acceptable.
#
#  Arguments:
#     vars
#        A list of the names of the global variable which are to recieve
#        the entered values. The initial values of these variables are 
#        displayed in the entry boxes. NB, these variables must be GLOBAL.
#     ntypes
#        The name of a 1-D array indexed by variable name, holding the data 
#        type for the the variable. This must be one of:
#        _REAL - A floating point value.
#        _INTEGER - An integer value.
#        _LOGICAL - A boolean value. True is returned as 1 and false as 0.
#        _CHAR[*length] - A string. If the supplied, the length determines
#                         the width of the associated entry box.
#        _CHOICE - A choice from the strings supplied in the variable's
#                  entry in the "limits" array.
#     nlabels
#        The name of a 1-D array indexed by variable name, holding the labels 
#        to display next to each entry box. These may be null if no label is required.
#     nlimits
#        The name of a 1-D array indexed by variable name. Each element is a 
#        list holding values which restrict the values which can be taken by
#        the corresponding variable. For _REAL and _INEGER, the first item 
#        in the list is the minimum allowed value, and the second is the 
#        maximum allowed value. If not supplied, no limits are imposed. For
#        _CHOICE, the items in the list are the allowed string values for the
#        variable.
#     title
#        A title for the dialog box window.
#     help
#        An htx cross-reference label into the hypertext documentation to be 
#        followed if the Help button is pressed. If this is null then no Help
#        button is created.
#     dhelp
#        The dynamic help string to display while the pointer is over the
#        dialog box.
#
#  Returned Value:
#     One if the "OK" button is pressed, and zero if the "Cancel" button
#     is pressed.
#
#  Globals:
#     B_FONT (Read)
#        The default font used for buttons.
#     CB_COL (Read)
#        The colour to use for the check buttons in the dialog box.
#     INPUTS_BUTTON (Write)
#        Used to communicate with the buttons in the dialog box. It holds
#        the label of the most recently pressed button.
#     RB_COL (Read)
#        The colour to use for the radio buttons in the dialog box.
#-
   global tcl_precision
   global B_FONT
   global CB_COL
   global INPUTS_BUTTON
   global RB_COL

# Create the empty dialog box.
   set top .inputs
   set topf [MakeDialog $top $title 1]

# Store the dynamic help text.
   SetHelp $top $dhelp

# Access the arrays declared in the calling procedure.
   upvar $ntypes types
   upvar $nlabels labels
   upvar $nlimits limits

# Create a frame to hold the variable controls.
   set cf [frame $topf.cf -relief sunken -bd 2]
   pack $cf -padx 2m -pady 2m -fill x -expand 1

# Loop round every supplied variable.
   set n [llength $vars]
   for {set i 0} {$i < $n} {incr i} {

# Access the variable using the local variable "var"
      set varname [lindex $vars $i]
      upvar #0 $varname var

# Store the supplied values, so that they can be restored if required.
      lappend olds $var

# Get a lower case version of the variable name.
      set lvar [string tolower $varname]

# Create a frame with this name.
      set lvar [string tolower $varname]
      set f1 [frame $cf.$lvar]
      pack $f1 -side top -pady 2m -padx 1m -anchor w -fill y -expand 1

# Get the type for this variable.
      set type $types($varname)

# Get the limits list, and the length of the limits list.
      if { [info exists limits($varname)] } {
         set lims $limits($varname)
         set nlims [llength $lims]
      } {
         set lims ""
         set nlims 0
      }

# Get the label (if supplied).
      if { [info exists labels($varname)] } {
         set label $labels($varname)
      } {
         set label ""
      }

# Create the textual label (if supplied, and except for _LOGICALs).
      if { $label != "" && $type != "_LOGICAL" } {
         set lb [label $f1.lb -text $label -font $B_FONT]
         pack $lb -side left -anchor n
      }

# Deal with each type in turn...

# _REAL...
      if { $type == "_REAL" } {

# Set the width for the data entry boxes.
         set wid [expr $tcl_precision + 6]

# Extract the min and max values from the limits list.
         if { $nlims > 0 } { 
            set min [lindex $lims 0]
            if { $nlims > 1 } { 
               set max [lindex $lims 1]
            } {
               set max {\"\"}
            }
         } {
            set min {\"\"}
            set max {\"\"}
         }

# Create the data entry box, limiting the value to the supplied max and
# min (if any).
         set com "if { $max != \\\"\\\" && \\\$$varname > $max } { 
                     set $varname $max
                  } elseif { $min != \\\"\\\" && \\\$$varname < $min } { 
                     set $varname $min
                  }"
         set vl [RealValue $f1.vl $wid $varname $com]
         pack $vl -side left 

# _INTEGER...
      } elseif { $type == "_INTEGER" } {

# Extract the min and max values from the limits list.
         if { $nlims > 0 } { 
            set min [lindex $lims 0]
            if { $nlims > 1 } { 
               set max [lindex $lims 1]
            } {
               set max {\"\"}
            }
         } {
            set min {\"\"}
         }

# Ensure they are integer.
         if { $min != {\"\"} } { set min [expr round( $min )] }
         if { $max != {\"\"} } { set max [expr round( $max )] }

# Create the data entry box, limiting the value to integer values between the 
# supplied max and min (if any).
         set com "set $varname \\\[expr round( \\\$$varname ) \\\]
                  if { $max != \\\"\\\" && \\\$$varname > $max } { 
                     set $varname $max
                  } elseif { $min != \\\"\\\" && \\\$$varname < $min } { 
                     set $varname $min
                  }"
         set vl [RealValue $f1.vl 12 $varname $com]
         pack $vl -side left 

# _LOGICAL
      } elseif { $type == "_LOGICAL" } {
         set cb [checkbutton $f1.cb -text $label -variable $varname \
                             -selectcolor $CB_COL -font $B_FONT]
         pack $cb -side left 

# _CHAR
      } elseif { [regexp {^_CHAR} $type] } {

# Extract the character length from the supplied type. Use 30 if no
# length was supplied.
         if { ![regexp {^_CHAR\*([0-9]+)} $type match wid] } {
            set wid 30
         }

# Create the string entry box.
         set vl [StringValue $f1.vl $wid $varname "" -font $B_FONT]
         pack $vl -side left 

# _CHOICE
      } elseif { $type == "_CHOICE" } {

# Create a frame to hold the radio-buttons.
         set rbf [frame $f1.rbf]
         pack $rbf -side left 

# Create a radiobutton for each option.
         set ii 0
         foreach string $lims {
            set rb [radiobutton $rbf.$ii -text $string -value $string \
                                -variable $varname -selectcolor $RB_COL]
            pack $rb -side top -anchor w 
            incr ii
         }

      }

   }

# Create the button bar.
   set butfrm [frame $topf.butfrm]
   pack $butfrm -fill both -expand 1

   set b1 [button $butfrm.ok -text "OK" -command "set INPUTS_BUTTON ok"]
   set b2 [button $butfrm.cancel -text "Cancel" -command "set INPUTS_BUTTON cancel"]
   set b3 [button $butfrm.restore -text "Restore" -command "set INPUTS_BUTTON  restore"]

   SetHelp $b1 ".  Press to close the dialog box, adopting the currently displayed values."
   SetHelp $b2 ".  Press to close the dialog box, cancelling the operation."
   SetHelp $b3 ".  Press to restore the original values."

   pack $b1 $b2 $b3 -side left -expand 1 -padx 2m   

   if { $help != "" } {
      set b4 [button $butfrm.help -text "Help" -command "set INPUTS_BUTTON help"]
      SetHelp $b4 ".  Press to display more detailed help information."
      pack $b4 -side left -expand 1 -padx 2m
   }

# If there is only one variable being obtained, then create a binding so
# that pressing the <Return> key behaves like clicking the OK button.
   if { $n == 1 } { bind $top <Return> "set INPUTS_BUTTON ok" }

# Ensure that closing the window from the window manager is like pressing
# the Cancel button.
   wm protocol $top WM_DELETE_WINDOW "set INPUTS_BUTTON cancel"

# Loop until an exit button is pressed.
   set exit 0
   while { !$exit } {

# Wait for the user to press a button.
      tkwait variable INPUTS_BUTTON

# If the cancel button was pressed, re-instate the original values, and
# exit, returning 0.
      if { $INPUTS_BUTTON == "cancel" } {
         set ret 0
         set exit 1
         for {set i 0} {$i < $n} {incr i} {
            upvar #0 [lindex $vars $i] var
            set var [lindex $olds $i]
         }

# If the OK button was pressed, exit with the current values, returning 1.
      } elseif { $INPUTS_BUTTON == "ok" } {
         set ret 1
         set exit 1

# If the Restore button was pressed, restore the original values but do
# not exit.
      } elseif { $INPUTS_BUTTON == "restore" } {
         for {set i 0} {$i < $n} {incr i} {
            upvar #0 [lindex $vars $i] var
            set var [lindex $olds $i]
         }

# If the Help button was pressed, display help.
      } elseif { $INPUTS_BUTTON == "help" } {
         ShowHelp $help 
      }
   }

# Destroy the dialog box.
   destroy $top

   return $ret

}

proc GetPosn {i name args} {
#+
#  Name:
#     GetPosn
#
#  Purpose:
#     Get a parameter value for a position.
#
#  Arguments:
#     i
#        The index (zero-based) of the position for which information
#        is to be returned.
#     name
#        The name of the parameter value to be returned. This should be
#        one of:
#           PX - The pixel X coordinate of the position.
#           PY - The pixel Y coordinate of the position.
#           CX - The canvas X coordinate of the position.
#           CY - The canvas Y coordinate of the position.
#           ID - The canvas item id of the marker for the position (-1
#                if no marker is currently drawn).
#           VID - The canvas item id of the line joining the position to
#                the position given by parameter NXT (-1 if no line is 
#                currently drawn, and null ("") if NXT is undefined).
#           NXT - The index of the position corresponding to the next 
#                vertex in a polygonal mask (-1 if there is no "next
#                vertex" (i.e. if the polygon is open) and null ("") if this 
#                position is not part of a polygon). 
#     args
#        An optional argument holding the name of the position list
#        to use.  If this is not supplied, it defaults to the value 
#        of global variable GWM_CURRENT_LIST.
#
#  Returned Value:
#     The parameter value.
#     
#  Globals:
#     PNTCX (Read)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas X coordinates. 
#     PNTCY (Read)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas Y coordinates. 
#     PNTID (Read)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas item identifiers associated with the positions in the 
#        list. A value of -1 indicates that no marker is currently 
#        drawn for the position.
#     PNTNXT (Read)
#        A 1-d array indexed by list name. Each element is a list of 
#        integers representing indices within the lists given by PNTxxx. 
#        Each integer gives the index of the next position along the 
#        edge of a polygon. The vector starting at position index i, 
#        ends at position index given by the i'th element of
#        PNTNXT. If this value is null ("") then position i is not part of
#        a polygon. If this value is -1, then the end of the vector starting
#        position i is unspecified (in this case the polygon is not
#        closed).
#     PNTPX (Read)
#        A 1-d array indexed by list name. Each element is a list of pixel 
#        X coordinates. 
#     PNTPY (Read)
#        A 1-d array indexed by list name. Each element is a list of pixel 
#        Y coordinates. 
#     PNTVID (Read)
#        A 1-d array indexed by list name. Each element is a list of canvas 
#        item identifiers associated with the vectors between positions in 
#        the list. A value of -1 indicates that no line is currently drawn 
#        for the position. A null string indicates that no vector is defined.
#
#-
   global PNTCX
   global PNTCY
   global PNTID
   global PNTNXT
   global PNTPX
   global PNTPY
   global PNTVID

# Initialise the returned value.
   set ret ""

# Store the list name to use, and create the list if necessary.
   set list [GetList $args]

# Do nothing if the list is empty, or an invalid index has been supplied.
   if { [info exists PNTID($list)] } {
      set size [llength $PNTID($list)]
      if { $i > -1 && $i < $size } {

# Get the parameter value.
         upvar #0 PNT$name array
         if { [info exists array($list)] } {
            set ret [lindex $array($list) $i]
         } {
            set ret ""
         }
      }
   }
    return $ret
}

proc GetSec {imsec imv secv} {
#+
#  Name:
#     GetSec
#
#  Purpose:
#     Split up an image section string into separate image name and
#     section strings.
#
#  Arguments:
#     imsec
#        The supplied image section string.
#     imv
#        The name of a variable in which to place image name.
#     secv
#        The nameof a variable in which to place the section string. This
#        is returned holding a null string if the image section string
#        supplied does not contain a section string.
#-
   upvar $imv image
   upvar $secv section

   if { ![regexp {(.+)(\(.*\))} $imsec match image section] } {
      set image $imsec
      set section ""
   }
}

proc GwmCanToNDF {cx cy} {
#+
#  Name:
#    GwmCanToNDF
#
#  Purpose:
#    Convert canvas coordinates to NDF pixel coordinates.
#
#  Arguments:
#    cx
#       The X canvas coordinate.
#    cy
#       The Y canvas coordinate.
#
#  Returned Value:
#    The X and Y pixel coordinates as a list of two values.
#
#  Globals:
#    GWM_CX (Read)
#       The X offset for converting from canvas
#       coordinates to NDF pixel coordinates.
#    GWM_CY (Read)
#       The Y offset for converting from canvas
#       coordinates to NDF pixel coordinates.
#    GWM_MX (Read)
#       The X scale factor for converting from canvas
#       coordinates to NDF pixel coordinates.
#    GWM_MY (Read)
#       The Y scale factor for converting from canvas
#       coordinates to NDF pixel coordinates.
#
#  Notes:
#    -  This is the inverse of procedure GwmNDFToCan
#-

   global GWM_CX
   global GWM_CY
   global GWM_MX
   global GWM_MY             

# Get the pixel coordinates in the displayed image. 
   set px [expr $GWM_CX + $GWM_MX * $cx ] 
   set py [expr $GWM_CY + $GWM_MY * $cy ]

# Return the coordinates.
   return [list $px $py]

}

proc GwmChange {} {
#+
#  Name:
#     GwmChange
#
#  Purpose:
#     Change the mode of display for the background image. The kapview
#     task is killed and re-loaded, and the display re-created. This is
#     necessary since running KAPPA:CONTOUR followed by KAPPA:DISPLAY
#     without first re-starting the kapview task causes an "HDS locator 
#     invalid" message to be displayed, and the task eventually dies.
#     Dont ask me why!!!
#-
   global TASK_FILE
   kapview kill
   LoadTask kapview $TASK_FILE(kapview)
   GwmUpdate
}   

proc GwmClear {} {
#+
#  Name:
#     GwmClear
#
#  Purpose:
#     Clears the GWM window
#
#-
   global GWM_DEVICE
   global GWM_DISPLAY_DATA
   global GWM_DISPLAY_NCONT

# Erase the image display. 
   Obey kapview gdclear "device=$GWM_DEVICE" 1
   set GWM_DISPLAY_DATA "" 
   set GWM_DISPLAY_NCONT 0
}

proc GwmContour {} {
#+
#  Name:
#    GwmContour
#
#  Purpose:
#    Display a contour map of a section of an image, and store 
#    global values needed by GwmNDFToCan and GwmCanToNDF (which convert 
#    between canvas coordinates and NDF pixel coordinates).
#
#  Arguments:
#    None.
#
#  Globals:
#    GWM_CX (Write)
#       The X offset for converting from canvas coordinates to NDF 
#       pixel coordinates.
#    GWM_CY (Write)
#       The Y offset for converting from canvas coordinates to NDF 
#       pixel coordinates.
#    GWM_DEVICE (Read)
#       The GNS device name for the GWM canvas item.
#    GWM_DISPLAY_DATA (Read and Write)
#       The image section currently displayed.
#    GWM_DISPLAY_LOCK (Read and Write)
#       Was the scaling locked when the current display was drawn?
#    GWM_NCONT (Read and Write)
#       The number of requested contours.
#    GWM_DISPLAY_NCONT (Read and Write)
#       The number of contours in the current display.
#    GWM_DISPLAY_PHI (Read and Write)
#       The upper percentile used when the current display was drawn.
#    GWM_DISPLAY_PLO (Read and Write)
#       The lower percentile used when the current display was drawn.
#    GWM_IMAGE (Read)
#       The name of the image to be displayed (without any section
#       specifier).
#    GWM_LOCK_SCALE (Read)
#       Should the image be displayed with the scaling implied by
#       the supplied values of GWM_SCALO and GWM_SCAHI? Otherwise,
#       the percentiles GWM_PLO and GWM_PHI are used to define the 
#       scaling.
#    GWM_MX (Write)
#       The X scale factor for converting from canvas coordinates to 
#       NDF pixel coordinates.
#    GWM_MY (Write)
#       The Y scale factor for converting from canvas coordinates to NDF 
#       pixel coordinates.
#    GWM_PHI (Read)
#       The requested lower percentile.
#    GWM_PLO (Read)
#       The requested lower percentile.
#    GWM_SCAHI (Read and Write)
#       The data value corresponding to white.
#    GWM_SCALO (Read and Write)
#       The data value corresponding to black.
#    GWM_SECTION (Read and Write)
#       The requested image section (eg "(10:200,23:68)" ).
#    GWM_SIZE (Read)
#       The size of the square GWM canvas item (in screen pixels).
#-
   global ATASK_OUTPUT
   global GWM_CONCOL
   global GWM_CONTLEVS
   global GWM_CX
   global GWM_CY
   global GWM_DEVICE
   global GWM_DISPLAY_PHI
   global GWM_DISPLAY_NCONT
   global GWM_DISPLAY_LOCK
   global GWM_DISPLAY_PLO
   global GWM_DISPLAY_DATA
   global GWM_FORCE
   global GWM_IMAGE
   global GWM_LOCK_SCALE
   global GWM_MX
   global GWM_MY
   global GWM_NCONT
   global GWM_PHI
   global GWM_PLO
   global GWM_SCAHI
   global GWM_SCALO
   global GWM_SECTION
   global GWM_SIZE

#  Return immediately if there is no background image.
   if { $GWM_IMAGE == "" } { return }

# Tell the user what is happening.
   set told [SetInfo "Contouring the image. Please wait... " 0]

# Set a flag to indicate that the image has not yet been displayed.
   set ok 0

# Concatenate the image name and section specifier to get the full
# image section specification.
   set data "${GWM_IMAGE}${GWM_SECTION}"

# If the new display would look exactly like the current display, exit
# without further action. This is the case if the caller has forced a
# re-display, or the image section has changed, or if the percentiles have 
# changed and the scaling is not
# locked, or if the scaling was previously locked and is now not locked,
# or if the wrong number of contours is displayed.
   if { $GWM_FORCE || $data != $GWM_DISPLAY_DATA || 
            ( ( $GWM_PLO != $GWM_DISPLAY_PLO || $GWM_PHI != $GWM_DISPLAY_PHI ) &&
              !$GWM_LOCK_SCALE ) || 
            ( $GWM_NCONT != $GWM_DISPLAY_NCONT ) ||
            ( !$GWM_LOCK_SCALE && $GWM_DISPLAY_LOCK ) } {

# Clear the flag which forces re-display.
      set GWM_FORCE 0 

# If the number of contours required has changed, ensure that the scaling
# is not locked.
      if { $GWM_NCONT != $GWM_DISPLAY_NCONT } {
         set GWM_LOCK_SCALE 0
      }

# If required, get a list of GWM_NCONT contour levels expressed as 
# percentiles evenly spaced between GWM_PLO and GWM_PHI. Construct a 
# suitable parameter string for KAPPA:HISTAT. 
      if { !$GWM_LOCK_SCALE } {
         set pd [expr ( $GWM_PHI - $GWM_PLO )/( $GWM_NCONT - 1 ) ]
         set pars "percentiles=\["
         for {set i 0} {$i < $GWM_NCONT} {incr i} {
            append pars [expr $GWM_PLO + $i*$pd]
            append pars ","
         }
         append pars "\]"

# Find the data values corresponding to the required percentiles.
         if { [Obey kappa histat "ndf=$data $pars"] } {

# Get the percentile values form the screen output produced by HISTAT.
# Note, do not use GetParam since the startcl buffer for parameter text
# can overflow if more than 4 contour levels are used. Substitute E's for
# D's.
            set GWM_CONTLEVS ""
            foreach mess $ATASK_OUTPUT {
               if { [regexp {percentile : (.+)} $mess match val] } {
                  regsub -nocase D $val E result
                  if { $GWM_CONTLEVS == "" } {
                     set GWM_CONTLEVS "\[$result"
                  } {
                     append GWM_CONTLEVS ",$result"
                  }
               }
            }
            append GWM_CONTLEVS "\]"
         }
      }

# Construct a suitable parameter string for KAPPA:CONTOUR.
      set pars "mode=free noaxes nokey concol=$GWM_CONCOL heights=$GWM_CONTLEVS"

# Erase the image display. 
      GwmClear

# Display the contour map.
      if { [Obey kapview contour "ndf=\"$data\" $pars device=$GWM_DEVICE" ] } {

# Indicate that the image has been displayed.
         set ok 1
         set GWM_DISPLAY_NCONT $GWM_NCONT
         set GWM_DISPLAY_DATA $data
         set GWM_DISPLAY_PLO $GWM_PLO
         set GWM_DISPLAY_PHI $GWM_PHI
         set GWM_DISPLAY_LOCK $GWM_LOCK_SCALE

# Note the first and last data values.
         set vlst [split $GWM_CONTLEVS ",\[\]"]
         set scalow [lindex $vlst 1]          
         set scahigh [lindex $vlst [expr [llength $vlst] - 2 ] ]          
         set GWM_SCALO [format "%.5g" $scalow]
         set GWM_SCAHI [format "%.5g" $scahigh]

# Use datapic to get the bounds of the DATA picture just created in 
# normalised device coordinates and NDF pixels. These NDC values extend
# from 0 to 1 on both axes.
         Obey polpack datapic "device=$GWM_DEVICE" 1
         regsub -nocase D [GetParam polpack datapic:result] E result
         scan $result "' %f %f %f %f %f %f %f %f '" ncx1 ncx2 ncy1 ncy2 \
                                                    wcx1 wcx2 wcy1 wcy2
      
# Calculate the offsets and scaling factors for converting from canvas
# coordinates to NDF pixels.
         set cx1 [expr $ncx1 * ( $GWM_SIZE - 1 )]
         set cx2 [expr $ncx2 * ( $GWM_SIZE - 1 )]
         set cy1 [expr $GWM_SIZE * ( 1.0 - $ncy1 )]
         set cy2 [expr $GWM_SIZE * ( 1.0 - $ncy2 )]
   
         set GWM_MX [expr ( $wcx2 - $wcx1 ) / ( $cx2 - $cx1 ) ]
         set GWM_CX [expr $wcx1 - $GWM_MX * $cx1]
         set GWM_MY [expr ( $wcy2 - $wcy1 ) / ( $cy2 - $cy1 ) ]
         set GWM_CY [expr $wcy1 - $GWM_MY * $cy1]

      }
   }

# Cancel the informative text set earlier in this procedure.
   if { $told } { SetInfo "" 0 }

}

proc GwmCreate {frmname size colours gwmname} {
#+
#  Name:
#     GwmCreate
#
#  Purpose:
#     Create a GWM canvas item with associated buttons, etc, and initialise
#     associated global variables.
#
#  Arguments:
#     frmname
#        The name to be used for the frame to contain all the
#        widgets created by this procedure.
#     size
#        The dimension of the square GWM canvas item to create, in screen 
#        pixels.
#     colours
#        The number of colours required in the GWM canvas item. A null
#        string is returned if the requested number of colours cannot 
#        be obtained.
#     gwmname 
#        The name for the GWM canvas item.
#
#  Returned Value:
#     The name of the frame containing all the widgets created by this
#     procedure, or a null string if anything goes wrong.
#
#  Globals:
#     The following globals are likely to be of interest to callers of
#     this function:
#
#     B_FONT (Read)
#        The font to use for the "Lock Scaling" check button label.
#     CB_COL (Read)
#        The colour to use for the "Lock Scaling" check button.
#     GWM_BADCOL (Write)
#        The colour to use for bad pixels in the image display. 
#        Initialised to "cyan".
#     GWM_CAN (Write)
#        The name of the canvas widget containing the GWM canvas item.
#     GWM_CURRENT_LIST (Write)
#        The name of the positions list to be included in the display.
#        This list is drawn as a series of closed polygons. If it is null,
#        then no positions list is displayed.
#     GWM_DEVICE (Write)
#        The GNS device name by which the GWM display can be accessed.
#     GWM_FLEFT (Write)
#        The name of the frame containing the Display Controls. Other
#        items can be packed in here if necessary.
#     GWM_FRIGHT (Write)
#        The name of the frame containing the canvas widget and
#        information string. Other items can be packed in here if necessary.
#     GWM_PHI (Write)
#        The upper percentile defining the scaling of the displayed image. 
#        Initialised to "95.0".
#     GWM_PLO (Write)
#        The upper percentile defining the scaling of the displayed image. 
#        Initialised to "5.0".
#     GWM_POLCOL (Write)
#        The colour to use for any polygons drawn over the image display. 
#        Initialised to "red".
#     GWM_XHAIR (Write)
#        Should a cross-hair be used instead of the usual pointer while in
#        the canvas? Initilised to 0 (no).
#     GWM_XHRCOL (Write)
#        The colour to use for the cross-hair over the image display.
#        Initialised to "yellow".
#     RB_FONT (Read)
#        The font to use for the informational message displayed above
#        the image display.
#
#
#  The remaining globals are used internally and are not likely to be of 
#  interest to callers of this function:
#
#     GWM_BLACK (Write)
#        The name of the frame containing thre "% black" widgets.
#     GWM_CANCEL  (Write)
#        The name of the "Cancel" button.
#     GWM_CENTRE  (Write)
#        The name of the "Centre" button.
#     GWM_DELETE  (Write)
#        The name of the "Delete" button.
#     GWM_DISPLAY_DATA (Write)
#        The image section currently displayed. Initialise to null.
#     GWM_DISPLAY_NCONT (Write)
#        The number of contours currently displayed. Zero for a greyscale
#        image. Initialised to zero.
#     GWM_DISPLAY_LOCK (Write)
#        Was the scaling locked when the current display was drawn?
#        Initialise to 0 (no).
#     GWM_DISPLAY_PHI (Write)
#        The upper percentile used when the current display was drawn.
#        Initialise to null.
#     GWM_DISPLAY_PLO (Write)
#        The lower percentile used when the current display was drawn.
#        Initialise to null.
#     GWM_DRAWN_LIST (Write)
#        The name of the positions list currently displayed. Initialised to 
#        null.
#     GWM_LOCK_SCALE (Write)
#        Should images be displayed with the scaling implied by
#        the previous values of GWM_SCALO and GWM_SCAHI? Otherwise,
#        the percentiles GWM_PLO and GWM_PHI are used to define the 
#        scaling. Initialised to 0 (no).
#     GWM_MODE (Write)
#        The current interaction mode, which determines how to process 
#        button clicks and motion in the GWM canvas item. Initialise to 0.
#     GWM_REDISPLAY_CANCELLED (Write)
#        Was a previous redisplay of the image cancelled because the
#        user looked like he may be about to enter a new scaling value?
#        Initialised to 0 (no).
#     GWM_REDISPLAY_REQUESTED (Write)
#        Was a redisplay of the image requested? Initialised to 0 (no).
#     GWM_ROOTI (Write)
#        The position index of the vertex being pointed at, or the position
#        index of the vertex at the start of the vector being pointed at,
#        or null if neither a vertex nor a vector is being pointed at.
#        Initialised to null.
#     GWM_SECTION_STACK (Write)
#        A stack of the previously displayed sections. This is stored as
#        a list with the oldest section is at the end of the list. 
#        Initialised to null.
#     GWM_SEL_AREA (Write)
#        The bounds of the selected area in the order xmin, xmax, ymin,
#        ymax. Initialised to null.
#     GWM_SIZE (Write)
#        The size of the GWM canvas item in screen pixels.
#     GWM_UNZOOM  (Write)
#        The name of the "Unzoom" button.
#     GWM_V0 (Write)
#        The index of the first vertex of the incomplete polygon.
#        Initialised to null.
#     GWM_V1 (Write)
#        The index of the position corresponding to the last vertex in
#        an incomplete (i.e. open) polygon. Initialised to null.
#     GWM_VID0 (Write)
#        The canvas item id for the vertex being pointed at (if any).
#        Initialised to null.
#     GWM_VID1 (Write)
#        The canvas item id for the vector ending at the vertex being 
#        pointed at (if any). Initialised to null.
#     GWM_VID2 (Write)
#        The canvas item id for the vector starting at the vertex being 
#        pointed at (if any). Initialised to null.
#     GWM_WHITE (Write)
#        The name of the frame containing thre "% black" widgets.
#     GWM_XHAIR_IDH (Write)
#        The canvas item identifier for the horizontal line forming the
#        cross-hair. Set null if no line exists. Initialised to null.
#     GWM_XHAIR_IDV (Write)
#        The canvas item identifier for the vertical line forming the
#        cross-hair. Set null if no line exists. Initialised to null.
#     GWM_ZOOM  (Write)
#        The name of the "Zoom" button.
#-
   global B_FONT         
   global CB_COL 
   global GWM_BADCOL
   global GWM_CONCOL
   global GWM_BLACK
   global GWM_CAN
   global GWM_CANCEL 
   global GWM_CENTRE 
   global GWM_CURRENT_LIST 
   global GWM_DELETE 
   global GWM_DEVICE
   global GWM_DISPLAY_NCONT
   global GWM_DISPLAY_DATA
   global GWM_DISPLAY_LOCK
   global GWM_DISPLAY_PHI
   global GWM_DISPLAY_PLO
   global GWM_DRAWN_LIST
   global GWM_FLEFT
   global GWM_FRIGHT
   global GWM_LOCK_SCALE 
   global GWM_MODE
   global GWM_PHI
   global GWM_PLO
   global GWM_POLCOL
   global GWM_REDISPLAY_CANCELLED
   global GWM_REDISPLAY_REQUESTED
   global GWM_ROOTI
   global GWM_SECTION_STACK
   global GWM_SEL_AREA
   global GWM_SIZE
   global GWM_UNZOOM 
   global GWM_V0
   global GWM_V1
   global GWM_VID0
   global GWM_VID1
   global GWM_VID2
   global GWM_WHITE
   global GWM_XHAIR
   global GWM_XHAIR_IDH
   global GWM_XHAIR_IDV
   global GWM_XHRCOL
   global GWM_ZOOM 
   global GWM_FORCE
   global RB_FONT

# Initialise some global variables.
   set GWM_BADCOL 	"cyan" 
   set GWM_CONCOL 	"black" 
   set GWM_CURRENT_LIST ""
   set GWM_DISPLAY_NCONT 0
   set GWM_DISPLAY_DATA ""
   set GWM_DISPLAY_LOCK 0
   set GWM_DISPLAY_PHI 	""
   set GWM_DISPLAY_PLO 	""
   set GWM_DRAWN_LIST 	""
   set GWM_LOCK_SCALE 	0
   set GWM_MODE 	0
   set GWM_PHI 		95.0 
   set GWM_PLO 		5.0 
   set GWM_POLCOL 	"red" 
   set GWM_REDISPLAY_CANCELLED 0
   set GWM_REDISPLAY_REQUESTED 0
   set GWM_ROOTI 	""
   set GWM_SECTION_STACK ""
   set GWM_SEL_AREA 	""
   set GWM_SIZE 	$size
   set GWM_V0 		""
   set GWM_V1 		""
   set GWM_VID0 	""
   set GWM_VID1 	""
   set GWM_VID2 	""
   set GWM_XHAIR 	0 
   set GWM_XHAIR_IDH 	""
   set GWM_XHAIR_IDV 	""
   set GWM_XHRCOL 	"yellow" 
   set GWM_FORCE 	0

# Create a container-frame to hold everything else, with the given name.
   set f1 [frame $frmname]

# Create two frames within this container-frame, and pack them side by
# side. The one on the left will contain the controls, and the one on the
# right will contain the canvas and the current info message.
   set GWM_FLEFT [frame $f1.left -bd 2 -relief groove]
   pack $GWM_FLEFT -side left -expand 1 -fill both -pady 3m -ipadx 2m -ipady 2m

   set GWM_FRIGHT [frame $f1.right]
   pack $GWM_FRIGHT -side right -expand 1 -fill both -pady 3m -ipadx 2m -ipady 2m

# Create a label reflecting the current info message (stored in GWM_INFO).
# Pack it at the top of the right hand frame.
   set finfo [label $GWM_FRIGHT.info -textvariable GWM_INFO \
                        -relief raised -bd 2 -font $RB_FONT -pady 1m]
   pack $finfo -padx 3m -pady 2m -expand 1 -fill x

# Create and pack the display controls in the left hand frame...

# Create and pack the label containing the title.
   pack [label $GWM_FLEFT.clab -text "Display Controls:"] -side top -fill x \
                                                          -pady 1m -expand 1

# Create the buttons...
   set GWM_ZOOM   [button $GWM_FLEFT.zoom   -text Zoom   -width 6 -command Zoom -state disabled ]
   set GWM_UNZOOM [button $GWM_FLEFT.unzoom -text Unzoom -width 6 -command UnZoom1 -state disabled ]
   set GWM_CENTRE [button $GWM_FLEFT.centre -text Centre -width 6 -command {SetMode 4} ]
   set GWM_DELETE [button $GWM_FLEFT.delete -text Delete -width 6 -command {Delete} -state disabled ]
   set GWM_CANCEL [button $GWM_FLEFT.cancel -text Cancel -width 6 -command {Cancel} -state disabled ]

# Pack the buttons.
   pack $GWM_ZOOM $GWM_UNZOOM $GWM_CENTRE $GWM_DELETE $GWM_CANCEL \
        -padx 5 -pady 2 -expand 1

# Set up the help to be displayed at the bottom of the screen when the
# pointer is over the buttons.
   SetHelp $GWM_ZOOM   ".  Click to zoom the image so that only the selected area is displayed.   (Click and drag over the image to select an area)." POLMAP_ZOOM
   SetHelp $GWM_UNZOOM ".  Single click to undo the previous Zoom or Centre operation.\n.  Double click to undo all zoom and Centre operations." POLMAP_UNZOOM
   SetHelp $GWM_CENTRE ".  Click this button and then point and click with the mouse to re-display the image centred on the pointer position." POLMAP_CENTRE
   SetHelp $GWM_DELETE ".  Click to delete any marker within the selected area.   (Click and drag over the image to select an area)." POLMAP_DELETE
   SetHelp $GWM_CANCEL ".  Click to:\n    - cancel an area selection\n    - abort the construction of a polygon\n    - cancel the selection of a new image centre" POLMAP_CANCEL

# If the Unzoom buton is double clicked, call procedure UnZoom2.
   bind $GWM_UNZOOM <Double-ButtonPress-1> {UnZoom2}

# Create the "widgets" for entering the lower (black) and upper (white) 
# percentile values.
   set GWM_BLACK [Value $GWM_FLEFT.black "% black" 6 GWM_PLO 100 0 CheckVal]
   set GWM_WHITE [Value $GWM_FLEFT.white "% not white" 6 GWM_PHI 100 0 CheckVal]
   pack $GWM_WHITE $GWM_BLACK -pady 5 -expand 1

   SetHelp $GWM_BLACK ".  Specify the percentage of pixels to be displayed black." POLMAP_BLACK
   SetHelp $GWM_WHITE ".  Specify the percentage of pixels to be displayed black or grey (i.e. not white)." POLMAP_NOT_WHITE

# Create a check button which locks the data scaling of displayed images.
   set lock [checkbutton $GWM_FLEFT.lock -text "Lock Scaling" \
                 -variable GWM_LOCK_SCALE -selectcolor $CB_COL -font $B_FONT \
                 -command \
                 "if { \$GWM_LOCK_SCALE } {
                     ${GWM_BLACK}.ent configure -state disabled
                     ${GWM_WHITE}.ent configure -state disabled
                  } {
                     ${GWM_BLACK}.ent configure -state normal
                     ${GWM_WHITE}.ent configure -state normal
                     GwmUpdate
                  }"]

   SetHelp $lock ".  Check to use the current data limits to display subsequent images. This causes the percentiles entered in the \"% black\" and \"% not white\" entry boxes to be ignored." POLMAP_LOCK_SCALING
   pack $lock -pady 5 -expand 1

# Create the canvas in the right hand frame, under the current info
# message.
   set GWM_CAN [canvas $GWM_FRIGHT.can -height $size -width $size]
   pack $GWM_CAN -padx 3m -expand 1

# Append the process id to the name of the GWM canvas item to ensure it is 
# unique, and create the GNS graphics device name which can be used by
# KAPPA etc.
   set name $gwmname
   append name "_[pid]"
   set GWM_DEVICE "xw;$name"

# Create a GWM canvas items which fills the canvas. Return a null string
# if this fails.
   set er [catch {set gwm [$GWM_CAN create gwm 0 0 -height $size \
                                  -width $size -name $name \
                                  -mincolours $colours -tags gwm]} mess]
   if { $er } {
      puts $mess
      set f1 ""

# If succesfull...
   } {

# Reset the pointer coordinates string to null when the pointer leaves
# the canvas, and ensure any cross hair is lowered below the GWM canvas
# item so that it cannot be seen. Raise it back again when the pointer
# enters the canvas.
      bind $GWM_CAN <Leave> "+
         set POINTER_PXY \"\"
         set POINTER_CXY \"\"
         if { \$GWM_XHAIR_IDH != \"\" } {
            $GWM_CAN lower \$GWM_XHAIR_IDH $gwm
            $GWM_CAN lower \$GWM_XHAIR_IDV $gwm
         }"
   
      bind $GWM_CAN <Enter> "+
         if { \$GWM_XHAIR_IDH != \"\" } {
            set cx \[$GWM_CAN canvasx %x\]
            set cy \[$GWM_CAN canvasy %y\]
            $GWM_CAN coords \$GWM_XHAIR_IDH 0 \$cy $size \$cy
            $GWM_CAN coords \$GWM_XHAIR_IDV \$cx 0 \$cx $size
            $GWM_CAN raise \$GWM_XHAIR_IDH $gwm
            $GWM_CAN raise \$GWM_XHAIR_IDV $gwm
         }"


# Execute procedure SingleBind when button 1 is clicked over the canvas.
      $GWM_CAN bind current <ButtonPress-1> "SingleBind %x %y"

# Execute procedure ReleaseBind when button 1 is released over the canvas.
      $GWM_CAN bind current <ButtonRelease-1> "ReleaseBind %x %y"

# Execute procedure B1MotionBind when the pointer is moved over the canvas 
# with button 1 pressed.
      $GWM_CAN bind current <B1-Motion> "B1MotionBind %x %y"

# Execute procedure MotionBind when the pointer is moved over the canvas 
# with no buttons pressed.
      $GWM_CAN bind current <Motion> "MotionBind %x %y"

# Establish the graphics and image display devices.
      Obey kapview lutable "mapping=linear coltab=grey device=$GWM_DEVICE" 1
      Obey kapview paldef "device=$GWM_DEVICE" 1
      Obey kapview palentry "device=$GWM_DEVICE palnum=0 colour=$GWM_BADCOL" 1
      Obey kapview gdclear "device=$GWM_DEVICE" 1

   }

# Return the name of the created container-frame.
   return $f1

}

proc GwmDisplay {} {
#+
#  Name:
#    GwmDisplay
#
#  Purpose:
#    Display a greyscale representation of a section of an image, and store 
#    global values needed by GwmNDFToCan and GwmCanToNDF (which convert 
#    between canvas coordinates and NDF pixel coordinates).
#
#  Arguments:
#    None.
#
#  Globals:
#    GWM_CX (Write)
#       The X offset for converting from canvas coordinates to NDF 
#       pixel coordinates.
#    GWM_CY (Write)
#       The Y offset for converting from canvas coordinates to NDF 
#       pixel coordinates.
#    GWM_DEVICE (Read)
#       The GNS device name for the GWM canvas item.
#    GWM_DISPLAY_DATA (Read and Write)
#       The image section currently displayed.
#    GWM_DISPLAY_LOCK (Read and Write)
#       Was the scaling locked when the current display was drawn?
#    GWM_DISPLAY_PHI (Read and Write)
#       The upper percentile used when the current display was drawn.
#    GWM_DISPLAY_PLO (Read and Write)
#       The lower percentile used when the current display was drawn.
#    GWM_IMAGE (Read)
#       The name of the image to be displayed (without any section
#       specifier).
#    GWM_LOCK_SCALE (Read)
#       Should the image be displayed with the scaling implied by
#       the supplied values of GWM_SCALO and GWM_SCAHI? Otherwise,
#       the percentiles GWM_PLO and GWM_PHI are used to define the 
#       scaling.
#    GWM_MX (Write)
#       The X scale factor for converting from canvas coordinates to 
#       NDF pixel coordinates.
#    GWM_MY (Write)
#       The Y scale factor for converting from canvas coordinates to NDF 
#       pixel coordinates.
#    GWM_PHI (Read)
#       The requested lower percentile.
#    GWM_PLO (Read)
#       The requested lower percentile.
#    GWM_SCAHI (Read and Write)
#       The data value corresponding to white.
#    GWM_SCALO (Read and Write)
#       The data value corresponding to black.
#    GWM_SECTION (Read and Write)
#       The requested image section (eg "(10:200,23:68)" ).
#    GWM_SIZE (Read)
#       The size of the square GWM canvas item (in screen pixels).
#-
   global GWM_CX
   global GWM_CY
   global GWM_DEVICE
   global GWM_IMAGE
   global GWM_LOCK_SCALE
   global GWM_MX
   global GWM_MY
   global GWM_PHI
   global GWM_PLO
   global GWM_SCAHI
   global GWM_SCALO
   global GWM_SECTION
   global GWM_SIZE
   global GWM_DISPLAY_DATA
   global GWM_DISPLAY_LOCK
   global GWM_DISPLAY_PHI
   global GWM_DISPLAY_PLO
   global GWM_DISPLAY_NCONT

#  Return immediately if there is no background image.
   if { $GWM_IMAGE == "" } { return }

# Tell the user what is happening.
   set told [SetInfo "Displaying the image. Please wait... " 0]

# Set a flag to indicate that the image has not yet been displayed.
   set ok 0

# Concatenate the image name and section specifier to get the full
# image section specification.
   set data "${GWM_IMAGE}${GWM_SECTION}"

# If the new display would look exactly like the current display, exit
# without further action. This is the case if the image section has
# changed, or if the percentiles have changed and the scaling is not
# locked, or if the scaling was previously locked and is now not locked,
# or if a contour map is currently displayed.
   if { $data != $GWM_DISPLAY_DATA || 
            ( ( $GWM_PLO != $GWM_DISPLAY_PLO || $GWM_PHI != $GWM_DISPLAY_PHI ) &&
              !$GWM_LOCK_SCALE ) || 
            ( $GWM_DISPLAY_NCONT > 0 ) ||
            ( !$GWM_LOCK_SCALE && $GWM_DISPLAY_LOCK ) } {

# When using KAPPA:DISPLAY, the centre has to be specified explicitly,
# otherwise StarTcl problems ensue.  Get a string describing the pixel 
# coordinates to put at at the centre of the display. This is just the 
# centre of the supplied section. Report an error if the centre cannot 
# be found.
      set centre [SecCen $GWM_SECTION]
      if { $centre == "" } {
         Message "String \"$GWM_SECTION\" is not a valid section."
      } {
         set cx [lindex $centre 0]
         set cy [lindex $centre 1]
         set centre "\[$cx,$cy\]"
   
# Decide on the scaling parameters to use with KAPPA:DISPLAY. First, check 
# for the user having locked the scaling. 
         if { $GWM_LOCK_SCALE && [info exists GWM_SCALO] } {
            set pars "mode=scale low=$GWM_SCALO high=$GWM_SCAHI"

# Next, if the data range is very low, use "flash" mode (otherwise
# DISPLAY will report an error), otherwise use the specified percentiles.
# Use KAPPA:STATS to find the image statistics, remembering to replace
# D exponents (which are not recognised by Tcl) by E exponents.
         } {
            if { [Obey kappa stats "ndf=$data"] } {
               regsub -nocase D [GetParam kappa stats:mean] E mean
               regsub -nocase D [GetParam kappa stats:sigma] E sigma
   
               if { $sigma == 0.0 || (
                    $mean != 0.0 && [expr abs($sigma/$mean)] < 1.0E-4
                    ) } {
                  set pars "mode=flash"
                  regsub -nocase D [GetParam kappa stats:minimum] E scalow
                  regsub -nocase D [GetParam kappa stats:maximum] E scahigh
               } {
                  set pars "mode=perc percentiles=\[$GWM_PLO,$GWM_PHI\]"
               }
            }
         }

# Erase the image display. 
         GwmClear

# Display the image section centred correctly. 
         if { [Obey kapview display "in=\"$data\" $pars badcol=0 device=$GWM_DEVICE \
                                     cosys=world xmagn=! ymagn=! centre=$centre" ] } {

# Indicate that the image has been displayed.
            set ok 1
            set GWM_DISPLAY_DATA $data
            set GWM_DISPLAY_PLO $GWM_PLO
            set GWM_DISPLAY_PHI $GWM_PHI
            set GWM_DISPLAY_LOCK $GWM_LOCK_SCALE

# Get the used scaling limits (replace D exponents with E). If the data
# range was too small to display, use the values established earlier.
            if { $pars != "mode=flash" } {
               regsub -nocase D [GetParam kapview display:scalow] E scalow
               regsub -nocase D [GetParam kapview display:scahigh] E scahigh
            }
            set GWM_SCALO [format "%.5g" $scalow]
            set GWM_SCAHI [format "%.5g" $scahigh]

# Use datapic to get the bounds of the DATA picture just created in 
# normalised device coordinates and NDF pixels. These NDC values extend
# from 0 to 1 on both axes.
            Obey polpack datapic "device=$GWM_DEVICE" 1
            regsub -nocase D [GetParam polpack datapic:result] E result
            scan $result "' %f %f %f %f %f %f %f %f '" ncx1 ncx2 ncy1 ncy2 \
                                                    wcx1 wcx2 wcy1 wcy2
      
# Calculate the offsets and scaling factors for converting from canvas
# coordinates to NDF pixels.
            set cx1 [expr $ncx1 * ( $GWM_SIZE - 1 )]
            set cx2 [expr $ncx2 * ( $GWM_SIZE - 1 )]
            set cy1 [expr $GWM_SIZE * ( 1.0 - $ncy1 )]
            set cy2 [expr $GWM_SIZE * ( 1.0 - $ncy2 )]
   
            set GWM_MX [expr ( $wcx2 - $wcx1 ) / ( $cx2 - $cx1 ) ]
            set GWM_CX [expr $wcx1 - $GWM_MX * $cx1]
            set GWM_MY [expr ( $wcy2 - $wcy1 ) / ( $cy2 - $cy1 ) ]
            set GWM_CY [expr $wcy1 - $GWM_MY * $cy1]
   
         }
      }
   }

# Cancel the informative text set earlier in this procedure.
   if { $told } { SetInfo "" 0 }

}

proc GwmNDFToCan {px py} {
#+
#  Name:
#    GwmNDFToCan
#
#  Purpose:
#    Convert NDF pixel coordinates to canvas coordinates.
#
#  Arguments:
#    px
#       The X NDF pixel coordinate.
#    py
#       The Y NDF pixel coordinate.
#
#  Returned Value:
#    The X and Y canvas coordinates as a list of two values, or a null
#    string if anything goes wrong.
#
#  Globals:
#    GWM_CX (Read)
#       The X offset for converting from canvas coordinates to NDF pixel 
#       coordinates.
#    GWM_CY (Read)
#       The Y offset for converting from canvas coordinates to NDF pixel 
#       coordinates.
#    GWM_MX (Read)
#       The X scale factor for converting from canvas coordinates to NDF 
#       pixel coordinates.
#    GWM_MY (Read)
#       The Y scale factor for converting from canvas coordinates to NDF 
#       pixel coordinates.
#
#  Notes:
#    -  This is the inverse of procedure GwmCanToNDF
#-

   global GWM_CX
   global GWM_CY
   global GWM_MX
   global GWM_MY

# Get the canvas coordinates. 
   if { $px != "" && $py != "" } { 
      set cxy [list [ expr ( $px - $GWM_CX ) / $GWM_MX] [ expr ( $py - $GWM_CY ) / $GWM_MY] ]
   } {
      set cxy ""
   }

# Return the coordinates.
   return $cxy
}

proc GwmUpdate {} {
#+
#  Name:
#     GwmUpdate
#
#  Purpose:
#     Update all components of the canvas area to reflect requested changes.
#
#  Arguments:
#     None.
#
#  Globals:
#     GWM_BACK (Read)
#        If "CONTOUR" then the background image is shown as a contour map.
#        If "GREY" then the background image is shown as a greyscale image.
#        If any other value, then no background image is displayed.
#     GWM_CURRENT_LIST (Read and Write)
#        The name of the positions list to be displayed. Set to null
#        if no list is to be displayed.
#     GWM_DEVICE (Read)
#        The GNS device name for the GWM canvas item.
#     GWM_DRAWN_LIST (Read and Write)
#        The name of the positions list currently displayed. Set to null
#        if no list is displayed.
#-
   global GWM_BACK
   global GWM_CURRENT_LIST
   global GWM_DEVICE
   global GWM_DRAWN_LIST

# Cancel any selected area.
   CancelArea

# Erase any displayed positions list.
   if { $GWM_DRAWN_LIST != "" } {
      ClearPosns $GWM_DRAWN_LIST
   }

# Re-draw the background image.
   if { $GWM_BACK == "CONTOUR" } {
      GwmContour
   } elseif { $GWM_BACK == "GREY" } {
      GwmDisplay
   } {
      GwmClear
   }

# Produce the vector map.
   VectorMap

# Draw any requested positions list, and save its name.
   if { $GWM_CURRENT_LIST != "" } {
      DrawPosns
      set GWM_DRAWN_LIST $GWM_CURRENT_LIST
   }

}

proc HdsDel {object} {
#+
#  Name:
#     HdsDel
#
#  Purpose:
#     Delete the hds container file containing a specified object.
#
#  Arguments:
#     object
#        The path to an HDS object contained in the container file which
#        is to be deleted (eg "PolMap100.TRN_2.TRANSFORM" ).
#-

# The name of the container file (without the .sdf file extension is the
# string in front of the first dot.
   if { [regexp {^([^.]*)} $object match file] } {

# Add on the file extension (.sdf) and attempt to delete the file.
      catch "exec rm -f ${file}.sdf"
   }
}

proc HelpArea {} {
#+
#  Name:
#     HelpArea
#
#  Purpose:
#     Create or destroy the frame displaying help information at the
#     bottom of the main window.
#
#  Arguments:
#     None.
#
#  Globals:
#     F4 (Read and Write)
#        The name of the frame to contain help information.
#     HAREA (Read)
#        Is help information to be displayed?
#     HLP_FONT (Read)
#        The font in which to display help information.
#-
   global F4
   global HAREA
   global HLP_FONT
   global TOP

# If required, create the help frame (if it has not already been created).
   if { $HAREA } {
      if { $F4 == "" } {

# Find the pixel size of the font. Use 10 if the font string cannot be
# parsed.
         if { ![regexp {^-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-([^-]+)} $HLP_FONT \
                match pixsize] } { set pixsize 10 }

# Find the pixels in 6 characters.
         if { [scan $pixsize %d pxsiz] == 0 } {
            set pxsiz 14
         }         
         set height [expr 6 * $pxsiz]

# The width is the requested width of the whole window.
         update idletasks
         set width [winfo width .]
         set width [expr 0.9 * $width]

# Create the frame to enclose the help text.
         set F4 [frame $TOP.help -relief groove -bd 2]
         pack $F4 -fill x

# Create a dummy frame with height but no width to act as a vertical strut.
# Geometry propagation is turn off for this frame so that its requested
# size will be retained even though nothing is put in the frame. This
# strut is used to keep the help area the same size even if the message text 
# within it requires a narrower area.
         set strut [frame $F4.strut -width 0 -height $height]
         pack propagate $strut 0
         pack $strut -side left

# Create a message widget to display dynamic help information about 
# the widget underneath the pointer.
         set hlab [message $F4.lab -justify left -textvariable HELP \
                            -anchor w -font $HLP_FONT -width $width]
         pack $hlab -fill x -expand 1

# Set up the help for the help area.
         SetHelp $F4 "An area which shows brief help on the object under the pointer. More detailed help can be obtained using the Help menu." POLMAP_HELP_AREA
      }

# If required, destroy the help frame (if it has not already been destroyed).
   } {
      if { $F4 != "" } {
         destroy $F4
         set F4 ""
      }
   }
}

proc Helper {x y} {
#+
#  Name:
#     Helper
#
#  Purpose:
#     Selects the text to display in the help area. 
#
#  Arguments:
#     x y
#        The root X and Y coordinates of the pointer.
#
#  Globals:
#     HELPS (Read)
#        An array holding the help messages for all widgets, indexed by 
#        widget name.
#     HELP (Write)
#        The text to be displayed in the help area.
#-

   global HELPS
   global HELP

# Find the lowest level widget under the pointer.
   set w [winfo containing $x $y]

# Check all the ancestors of this widget. This loop will be broken out of when
# a widget is found which has an associated help message.
   while { $w != "" } {
      if { [info exists HELPS($w)] } {
         set HELP $HELPS($w)
         break
      }
      set w [winfo parent $w]
   }

# If no suitable widget was found, store a null help string.
   if { $w == "" } {
      set HELP ""
   }
}

proc IncVal {value max min} {
#+
#  Name:
#    IncVal
#
#  Purpose:
#    Called to increment the numerical value associated with a "widget" 
#    created by the procedure "Value".
#
#  Arguments:
#    value
#       The name (note, NOT the value) of the global variable holding the
#       the value to be incremented.
#    max
#       The maximum allowed value.
#    min
#       The minimum allowed value.
#
#  Globals:
#    INCS (Read)
#       The number of times the value has been incremented since the
#       increment button ws pressed.
#-
   global INCS
   upvar #0 $value val
    set f [expr 0.01 * ( $max - $min )]
    if { $INCS == 1 } {
      set inc "0.1"
    } elseif { $INCS < 4 } {
      set inc "0.0"
    } elseif { $INCS < 10 } {
      set inc "0.2"
    } elseif { $INCS < 20 } {
      set inc "0.5"
    } elseif { $INCS < 30 } {
      set inc "1.0"
    } else {
      set inc "2.0"
   }
    set val [expr $val + $f * $inc ]
    if { $val > $max } {
      set val $max
   } elseif { $val < $min } {
      set val $min
   }
}

proc LoadOptions {} {
#+
#  Name:
#     LoadOptions
#
#  Purpose:
#     Copy the option values supplied by the PolMap atask (if any)
#     to global variables where they can be access and modified.
#
#  Arguments:
#     None.
#
#  Globals:
#      ATASK_NCONT (Read)
#         The value of NCONT supplied by the A-task.
#      ATASK_HAREA (Read)
#         The value of HAREA supplied by the A-task.
#      ATASK_BACK (Read)
#         The value of GWM_BACK supplied by the A-task.
#      ATASK_SAREA (Read)
#         The value of SAREA supplied by the A-task.
#      ATASK_SI (Read)
#         A string representing the SI_LIST list.
#      GWM_BACK (Write)
#         The form of background image required.
#      GWM_NCONT (Write)
#         The number of contours to display
#      HAREA (Write)
#         Should the help area be displayed?
#      SAREA (Write)
#         Should the status area be displayed?
#      SI_LIST (Write)
#         A list of indices identifying the status items to be displayed
#         in the status area.
#      SI_VARS (Read)
#         A list of all global variable names available for display in the
#         status area.
#
#-
   global ATASK_BADCOL
   global ATASK_CONCOL
   global ATASK_NCONT
   global ATASK_BACK
   global ATASK_HAREA
   global ATASK_LOGFILE
   global ATASK_PHI
   global ATASK_PLO
   global ATASK_SAREA
   global ATASK_SELCOL
   global ATASK_SI
   global ATASK_XHAIR
   global ATASK_XHRCOL
   global GWM_BADCOL      
   global GWM_CONCOL      
   global GWM_NCONT
   global CHAR_LIST
   global CHAR_STOP
   global HAREA
   global GWM_BACK
   global LOGFILE_ID
   global PHI_REQ
   global PLO_REQ
   global SAREA
   global GWM_SELCOL
   global SI_LIST
   global SI_VARS
   global GWM_XHAIR
   global GWM_XHRCOL

# If the Atask has specified a value for the option, copy it from the
# variable used to communicate with the atask, to a variable which
# can be used and modified within the script. If no value was supplied,
# set a default.
   if { [info exists ATASK_HAREA] } {
     set HAREA $ATASK_HAREA
   } {
     set HAREA 1
   }

# Do the same for the other options.
   if { [info exists ATASK_BACK] } {
     set GWM_BACK [string trim $ATASK_BACK]
   } {
     set GWM_BACK ""
   }

   if { [info exists ATASK_XHAIR] } {
      set GWM_XHAIR $ATASK_XHAIR
   } {
      set GWM_XHAIR 0
   }

   if { [info exists ATASK_NCONT] } {
      set GWM_NCONT $ATASK_NCONT
   } {
      set GWM_NCONT 4
   }

   if { [info exists ATASK_XHRCOL] } {
     set GWM_XHRCOL [string trim $ATASK_XHRCOL]
   } {
     set GWM_XHRCOL "yellow"
   }
   SetColours GWM_XHRCOL

   if { [info exists ATASK_PLO] } {
     set PLO_REQ [format "%5.1f" $ATASK_PLO]
   } {
     set PLO_REQ 5.0
   }

   if { [info exists ATASK_PHI] } {
     set PHI_REQ [format "%5.1f" $ATASK_PHI]
   } {
     set PHI_REQ 95.0
   }

   if { [info exists ATASK_BADCOL] } {
     set GWM_BADCOL [string trim $ATASK_BADCOL]
   } {
     set GWM_BADCOL "cyan"
   }
   SetColours GWM_BADCOL

   if { [info exists ATASK_CONCOL] } {
     set GWM_CONCOL [string trim $ATASK_CONCOL]
   } {
     set GWM_CONCOL "black"
   }

   if { [info exists ATASK_SELCOL] } {
     set GWM_SELCOL [string trim $ATASK_SELCOL]
   } {
     set GWM_SELCOL "red"
   }

   if { [info exists ATASK_SAREA] } {
     set SAREA $ATASK_SAREA
   } {
     set SAREA 1
   }

   if { [info exists ATASK_LOGFILE] } {
      if { [string tolower $ATASK_LOGFILE] == "stdout" } {
         set LOGFILE_ID stdout
      } {
         set LOGFILE_ID [open $ATASK_LOGFILE w]
      }
   } { 
      set LOGFILE_ID ""
   }

# SI_LIST is done differently. Each character in ATASK_SI represents an
# integer to be appended to the SI_LIST list. Initialise SI_LIST.
   set SI_LIST ""

# If a value for ATASK_SI has been supplied...
   if { [info exists ATASK_SI] } {

# Get the first character in the supplied string.
      set c [string index $ATASK_SI 0]

# Set the index of the next character to be checked.
      set i 1

# Loop round until a STOP character is found.
      while { $c != $CHAR_STOP } {

# Find the index of the current character within the string CHAR_LIST, and
# append this integer value (representing a status item index) to SI_LIST.
         lappend SI_LIST [string first $c $CHAR_LIST]

# Move on to the next character in the supplied string.
         set c [string index $ATASK_SI $i]
         incr i
      }

# After the STOP character there should be an integer value giving the
# number of available status items. Get this value from the string. There
# may be extra items available now (if the user hasn't used PolMap for
# a long time). Any new items are displayed.
      scan [string range $ATASK_SI $i end] "%d" n

   } {   
      set n 0
   }

# Indicate that any unspecified items should be displayed.
   for {set i $n} {$i < [llength $SI_VARS]} {incr i} {
      lappend SI_LIST $i
   }

}

proc LoadTask {task file} {
#+
#  Name:
#    LoadTask
#
#  Purpose:
#    Load an ADAM task so that it can be used.
#
#  Arguments:
#    task
#      The name by which the task is to be known 
#      (eg "kapview").
#    file
#      The file containing the executable image 
#      (eg "/star/bin/kappa/kapview_mon").
#
#  Notes:
#    -  This procedure shuts down the whole application if the task 
#    cannot be loaded.
#
#  Globals:
#    ADAM_TASKS (Write)
#       A list of the names of the ADAM tasks started up by PolMap.

#-
   global ADAM_TASKS
   global ADAM_USER
   global RENDEVOUS
   global TASK_FILE

# Load the task.
   set taskload [list adamtask $task $file ]
   if {[catch $taskload error] != 0} {
      Message "Error loading task $task (file $file): \"$error\". Aborting..."
      exit 1
   }

# Poll for the task to attach to the message system.
   set count 0
   while {[$task path] == 0} {
      after 100
      incr count
      if {$count > 100} {
         Message "Timed out waiting for task \"$task\" (file $file) to start. Aborting..." 
         $task kill
         exit 1
      }
   }

# Append the name of the task to the list of tasks started up so far.
   lappend ADAM_TASKS $task

# Save the name of the rendevous file.
   foreach rfile [glob $ADAM_USER/${task}_*] {
      if { [regexp "${task}_\[0-9\]+\$" $rfile] } {
         set RENDEVOUS($task) $rfile
         break
      }
   }

   if { ![info exists RENDEVOUS($task)] } {
      Message "Cannot find the rendevous file for $task."
      exit 1
   }
   set TASK_FILE($task) $file

}

proc MakeDialog {w title grab} {
#+
#  Name:
#     MakeDialog
#
#  Purpose:
#     Create an empty dialog box. It should be destroyed using
#     "destroy $w" when no longer needed.
#
#  Arguments:
#     w
#        The name of the toplevel window to create.
#     title
#        The title to display above the toplevel window.
#     grab
#        Should the toplevel window grab all X events?
#
#  Returned Value:
#     The path to a frame in which the dialog box components can be packed.
#
#  Globals:
#     TOP (Read)
#        The path to the main application window.
#-
   global TOP

# Create the top level window for the dialogue box, and set its title.
# It inherits the (potentially private) colour map used by the main
# application window.
   set top [toplevel $w -colormap $TOP]
   wm title $top "PolMap - $title"

# Set up a binding so that this window is automatically raised above the
# main PolMap window each time it becomes partially or totally obscured.
   bind $top <Visibility> "+if { \"%s\" != \"VisibilityUnobscured\" } {raise $top .}"

# Attempt put a grab on this window, so that other windows become
# inactive. This is a bit fragile so put the grab inside a catch so that
# an error in grab will not abort the application.
   if { $grab } { catch "grab $top" }

# Create a frame to hold everything else so that we can have a null
# border round the other widgets.
   set topf0 [frame $top.f0 -bd 3 -relief raised]
   set topf [frame $topf0.f ]

# Pack the frame holding everything else.
   pack $topf
   pack $topf0 -padx 2m -pady 2m -ipadx 2m -ipady 2m

# Return the name of the frame to contain everything else.
   return $topf
}

proc MarkBind {i list} {
#+
#  Name:
#     MarkBind
#
#  Purpose:
#     Set up bindings for entering and leaving a position marker.
#
#  Arguments:
#     i
#        The index of the position.
#     list
#        The name of the positions list containing the position.
#-
   global GWM_CAN

# Get the canvas item identifier for the marker. Only proceed if a marker
# is currently displayed.
   set id [GetPosn $i ID $list]   
   if { $id != -1 } {

# Get the pixel coordinates desribing the position.
      set px [GetPosn $i PX $list]
      set py [GetPosn $i PY $list]

# Format the pixel coordinates.
      set pxy [format "( %.1f, %.1f )" $px $py ]

# Assign the position's pixel coordinates to PXY when the pointer enters it. 
      $GWM_CAN bind  $id <Enter>  "set PXY \"$pxy\""

# Set PXY null when the pointer leaves the position.
      $GWM_CAN bind $id <Leave> "set PXY {}"
   }
}

proc MarkPosn {i args} {
#+
#  Name:
#     MarkPosn
#
#  Purpose:
#     Draw the canvas marker for a single position.
#
#  Arguments:
#     i
#        The index of the position to be marked.
#    args
#       An optional argument holding the name of the position list
#       to use.  If this is not supplied, it defaults to the value 
#       of global variable GWM_CURRENT_LIST.
#
#  Returned Value:
#      The canvas item identifier for the marker.
#
#  Globals:
#     GWM_CAN (Read)
#        Path to the canvas containing the GWM image display.
#-
   global GWM_CAN
   global GWM_POLCOL
   global PNTCX
   global PNTCY
   global PNTID
   global POLPACK_DIR

# Store the list name to use, and create the list if necessary.
   set list [GetList $args]

# Get the canvas coordinates for the marker.
   set cx [lindex $PNTCX($list) $i]
   set cy [lindex $PNTCY($list) $i]

# If a canvas item marking the position has not already been created,
# create one now, and store its index.
   set id [lindex $PNTID($list) $i]
   if { $id == -1 } {
      set id [$GWM_CAN create bitmap $cx $cy -bitmap @$POLPACK_DIR/vertex.bit \
                          -foreground $GWM_POLCOL -tags vertices]
      set PNTID($list) [lreplace $PNTID($list) $i $i $id]

# If a canvas item already exists, configure it to have the correct
# properties.
   } {
      $GWM_CAN coords $id $cx $cy
      $GWM_CAN itemconfigure $id -bitmap @$POLPACK_DIR/vertex.bit -foreground $GWM_POLCOL -tags vertices
   }                   

# Set up bindings which assign the position's label to the global LABEL
# when the pointer enters the marker, and sets LABEL null when the
# pointer leaves the marker. Also, store the pixel coordinates in
# PXY.
   MarkBind $i $list

# Return the marker canvas id.
   return $id
}

proc MenuHelp {win label text} {
#+
#  Name:
#     MenuHelp
#
#  Purpose:
#     Establish the help text to display when the pointer is over
#     a specified entry in a specified menu.
#
#  Arguments:
#     win
#        The name of the menu.
#     label
#        The textual label for the menu entry.
#     text
#        The help information to display.
#
#  Globals:
#     MENUHELPS (Write)
#        A 2d array indexed by widget path and entry label, holding
#        the help text strings for all menu entries.
#-
   global MENUHELPS

# Store the supplied help text.
   set MENUHELPS($win,$label) $text

# Arrange for a null help string to be displayed when the pointer
# initially enters the menu. This will be changed by the MenuMotionBind
# procedure.
   SetHelp $win ""
}

proc MenuMotionBind {win y} {
#+
#  Name:
#     MenuBind
#
#  Purpose:
#     Displays help as the pointer moves over a menu. It should be bound
#     to motion over all menus.
#
#  Arguments:
#     win
#        The name of the window currently under the pointer.
#     y
#        The y coordinate of the pointer.
#
#  Globals:
#     HELP (Write)
#        The current help text to display.
#     MENUHELPS (Read)
#        The help text for each entry of each menu.
#-
   global HELP
   global MENUHELPS

# Ignore separators...
   if { [$win type @$y] != "separator" } {

# Get the label from the menu entry under the pointer.
      set label [$win entrycget @$y -label]

# Get the help text associated with this menu entry
      if { [info exists MENUHELPS($win,$label)] } {
         set HELP $MENUHELPS($win,$label)
      } {
         set HELP ""
      }
   } {
      set HELP ""
   }
}

proc Message {message} {
#+
#  Name:
#    Message
#
#  Purpose:
#    Display a dialogue box displaying a message, and wait for the
#    user to press the "OK" button.
#
#  Arguments:
#    message
#       The message to display. 
#
#  Globals:
#    TOP (Read)
#        The path to the main application window.
#-
   global TOP

# If the top level window has not yet been created, then write the
# message to standard output.
   if { ![info exists TOP] } {
      puts $message

# Otherwise, display the message in a dialog box.
   } {
      dialog .msg "PolMap - Message..." $message {} 0 OK
   }

}

proc MotionBind {x y} {
#+
#  Name:
#    MotionBind
#
#  Purpose:
#    Process pointer motion over the image with no buttons pressed.
#
#  Arguments:
#    x
#       The screen X coord.
#    y
#       The screen Y coord.
#
#  Globals:
#     GWM_CAN (Read)
#        The name of the canvas widget holding the GWM image.
#     GWM_MODE (Read)
#        The interaction mode determining how to process button clicks
#        and motion in the GWM canvas item. Modes are:
#           0 - The user specifies image features by clicking on them.
#           1 - The user starts a new mask polygon, or edits an existing one.
#           2 - The user completes a started mask polygon.
#           3 - The user selects a label by clicking on an existing feature.
#           4 - The user selects a position and the image is redisplayed
#               centred on the supplied position.
#     GWM_VID2 (Read)
#        The canvas item id for the vector starting at the previous vertex,
#        which ends at the current pointer position.
#-
   global GWM_CAN
   global GWM_MODE
   global GWM_VID2
   global POINTER_PXY
   global POINTER_CXY

# Convert the screen coords to canvas coords.
   set cx [$GWM_CAN canvasx $x]
   set cy [$GWM_CAN canvasy $y]

# If a cross-hair has been requested instead of a pointer, then move the
# positions of the lines making up the cross hair.
   Xhair $cx $cy

# Store the canvas coordinates of the pointer in POINTER_CXY.
   set POINTER_CXY [format "( %.1f, %.1f )" $cx $cy]

# Store the pixel coordinates of the pointer in POINTER_PXY.
   set pxy [GwmCanToNDF $cx $cy]
   set px [lindex $pxy 0]
   set py [lindex $pxy 1]
   set POINTER_PXY [format "( %.1f, %.1f )" $px $py ]

# The global variable GWM_MODE determines how events over the canvas are 
# processed. If we are in mode 2 ("enter a new polygon"), the "loose" end 
# of the most recent vector is bound to the pointer.
   if { $GWM_MODE == 2 } {

# Move the "loose" end of the vector which starts at the previous vertex,
# so that it follows the pointer.
      set coords [$GWM_CAN coords $GWM_VID2]
      $GWM_CAN coords $GWM_VID2 [lindex $coords 0] [lindex $coords 1]  $cx $cy 

   }
}
    
proc NumPosn {v0 args} {
#+
#  Name:
#     NumPosn
#
#  Purpose:
#     Returns the number of positions in a list.
#
#  Arguments:
#     v0
#        If a non-null value is supplied, then the returned value is the
#        number of positions in the polygon starting at position with
#        index $v0. Otherwise, the number of positions of any description
#        is returned.
#     args
#        An optional argument holding the name of the position list
#        to use.  If this is not supplied, it defaults to the value 
#        of global variable GWM_CURRENT_LIST.
#
#  Returned Value:
#        The number of positions.
#
#  Globals:
#     PNTID (Read)
#        A 1-d array indexed by list name. Each element
#        is a list of canvas item identifiers associated with the
#        positions in the list. 
#     PNTNXT (Read)
#        A 1-d array indexed by list name. Each element
#        is a list of integers representing indices within the lists 
#        given by PNTxxx. Each integer gives the index of the next position
#        along the edge of a polygon. The vector starting at position
#        index i, ends at position index given by the i'th element of
#        PNTNXT. If this value is null ("") then position i is not part of
#        a polygon. If this value is -1, then the end of the vector starting
#        position i is unspecified (in this case the polygon is not
#        closed).
#
#-
   global PNTID
   global PNTNXT

# Store the list name to use, and create the list if necessary.
   set list [GetList $args]

# If the total number of positions in the list is required...
   if { $v0 == "" } {

# Get the size of the list.
      if { [info exists PNTID($list)] } {
         set size [llength $PNTID($list)]
      } {
         set size 0
      }

# If the number of vertices in the polygon is required...
   } {

# Loop round counting the vertices until we arrive back at the start, or an
# unattached vertex is found.
      set nxt [lindex $PNTNXT($list) $v0]
      if { $nxt != "" } {
         set size 1
         while { $nxt != $v0 && $nxt != -1 } { 
            incr size
            set nxt [lindex $PNTNXT($list) $nxt]
         }
      } {
         set size 0
      }
   }

# Return it.
   return $size
}

proc Obey {task action params args} {
#+
#  Name:
#    Obey
#
#  Purpose:
#    Executes an ADAM application. 
#
#  Arguments:
#    task
#       The name of the task containing the application (eg "kapview").
#    action
#       The name of the application (eg "display").
#    params
#       Any command line parameter assignments to pass to the 
#       application. A null string must be supplied if no 
#       command line parameter assignments are needed.
#    args
#       o  If the optional string "noreport" is supplied, then any error
#       messages generated by the action are not displayed. 
#       o  If the name of a currently defined global variable is supplied, 
#       then the variable is assumed to be a 1-D array, indexed by A-task
#       parameter name. The associated values are the values to supply for 
#       the A-task's parameters if they are prompted for. 
#       o  The presence of any other value after "params" causes
#       the whole TCL application to abort if the specified action
#       does not complete succesfully.
#
#  Returned Value:
#    If the application completes succesfully, then 1 is returned.
#    Otherwise, 0 is returned.
#
#  Globals:
#    ACTION (Write)
#      Name of current action in the form "task:action".
#    ADAM_ERRORS (Write)
#      The messages from the most recent ADAM application to fail.
#    ATASK_OUTPUT (Write)
#       Any non-error messages generated by the action are appended to 
#       this list. Each message is stored as a new element in the list.
#    STATUS (Write)
#      The status string returned by the action.
#
#  Notes:
#    - The Task must already have been loaded using LoadTask.
#    - Any error messages created by the action are displayed in a dialog
#    box, unless the optional argument "args" has the value "noreport".
#    - This procedure does not return until the application has finished.
#    In the mean time, the display is "frozen" so that no further actions
#    can be initiated.
#-
   global ACTION
   global ADAM_ERRORS
   global ATASK_OUTPUT
   global CANCEL_OP
   global LOGFILE_ID
   global STATUS

# Return without action if the opoeration was canceleed.
   if { $CANCEL_OP } { return 0 }

# Store the current action being performed in global.
   set ACTION "$task:$action"

# Classify the optional argument (if supplied). By default, parameter
# requests are replied to be sending a PAR__NULL (!) value, errors do
# not cause the application to abort, and errors are reported.
   set param_req "$task paramreply %R \!"
   set abort 0
   set report 1

   if { $args != "" } {

# See if an array of parameter values has been supplied.
      upvar #0 $args plist
      if { [info exists plist] } {
         set param_req "if { \[info exists ${args}(%n)\] } {
                           $task paramreply %R \$${args}(%n)
                        } {
                           $task paramreply %R \!
                        }"

# Otherwise, see if error reports are to be ignored.
      } elseif { $args == "noreport" } {
         set report 0

# Otherwise, abort on an error.         
      } else {
         set abort 1
      }
   }

# Check that the AMS rendevous file still exists. If it doesn,t kill the
# task and reload it.
   CheckRF $task

# Clear any current ADAM messages.
   set ADAM_ERRORS {}
   set ATASK_OUTPUT {}

# Write the command we are to obey to the log file if there is one.
   if { $LOGFILE_ID != "" } {
      puts $LOGFILE_ID "\n$task $action $params..."
   }

# Start the action. Any messages generated by the action are processed
# by procedure CheckMsg. Error messages are appended to ADAM_ERRORS, other
# messages are thrown away. Parameter requests are responded to by
# sending a null (!) value. The variable STATUS is set when the
# action completes.
   set STATUS ""   
   $task obey $action $params -inform "CheckMsg $action %V" \
                      -endmsg {set STATUS "%S"} \
                      -paramreq $param_req

# Wait until the action is finished. Check that the Rendevous file exists
# every 200 milliseconds. If the WaitFor command aborts early, try
# re-executing the obey command. Do not re-execute the command if it was
# terminated due to a user requested cancel (indicated by CANCEL_OP being
# non-zero).
   if { ![WaitFor STATUS [list CheckRF $task] 200] && !$CANCEL_OP } {
      set ADAM_ERRORS {}
      set ATASK_OUTPUT {}

      if { $LOGFILE_ID != "" } {
         puts $LOGFILE_ID "\n$task $action $params..."
      }

      set STATUS ""   
      $task obey $action $params -inform "CheckMsg $action %V" \
                         -endmsg {set STATUS "%S"} \
                         -paramreq $param_req

      if { ![WaitFor STATUS [list CheckRF $task] 200] && !$CANCEL_OP } {
         Message "Problems with rendevous file! Aborting..."
         exit 1
      }
   }

# Set the return status. If any error messages were generated, assume the
# action failed.
   if { $ADAM_ERRORS != "" } {
      set ok 0
   } {
      set ok 1
   }

# Display any error messages.
   if { !$ok } {
      if { $report } {
         Message "$task action \"$action\" failed.\n$ADAM_ERRORS"
      }

# If failure is fatal, shut down.
      if { $abort } {exit 1}
   }

# Indicate that we are no longer executing an ADAM action.
   set ACTION ""

# Return the status. Return zero if the operation was cancelled by the
# user.
   if { $CANCEL_OP } { set ok 0 }

   return $ok
}

proc PixIndSection {imsec} {
#+
#  Name:
#     PixIndSection
#
#  Purpose:
#     Return a section string in standard form (pixel index bounds).
#
#  Arguments:
#     imsec
#       An image name, with or without a section specifier. The section
#       specifier may be of any sort (eg pixel coordinates instead of
#       indices, centre and extent instead of bounds, etc)
#
#  Returned Value:
#     The standard pixel index bounds specifier (eg "(10:234,34:345)" ).
#-

# Use KAPPA:NDFTRACE to find the pixel index bounds of the supplied image.
   Obey ndfpack ndftrace "ndf=\"$imsec\" quiet" 1

# Get the lower and upper bounds.
   regsub -nocase D [GetParam ndfpack ndftrace:lbound] E lbound
   regsub -nocase D [GetParam ndfpack ndftrace:ubound] E ubound

# Extract the individual bounds.
   set ok 0
   if { [regexp {\[([-+0-9]+),([-+0-9]+)\]} $lbound a xlo ylo] } {
      if { [regexp {\[([-+0-9]+),([-+0-9]+)\]} $ubound a xhi yhi] } {
         set ok 1
      }
   }

# Report a message if the bounds could not be found, and use arbitrary
# bounds.
   if { !$ok } {
      Message "Failed to determine the bounds of image \"$imsec\" - using default bounds of (1:512,1:512)."
      set xlo 1
      set ylo 1
      set xhi 512
      set yhi 512
   } 

# Return the section string.
   return "($xlo:$xhi,$ylo:$yhi)"    
}

proc Pop {stack args} {
#+
#  Name:
#    Pop
#
#  Purpose:
#    Returns and removes the top value in the supplied FILO stack.
#
#  Arguments:
#    stack
#       The name (NOT the value) of a global list variable holding the stack. 
#       On exit, the list holds one less element than on entry.
#    args
#        An optional argument giving the number of levels to pop off the
#        stack. It defaults to 1. If it is supplied as -1, then the 
#        the first (bottom) entry is returned and the stack is emptied. If it
#        is supplied as 0, then the top entry on the stack is returned, but 
#        it is not removed from the stack.
#
#  Returned Value:
#    The required stack element, or an empty string if the supplied stack 
#    was empty.
#-

   upvar #0 $stack stk

   if { $args == "" } {
      set levels 1
   } { 
      set levels $args
   }

   if { $levels == -1 } {
      set ret [lindex $stk end]
      set stk ""

   } elseif { $levels == 0 } {
      set ret [lindex $stk 0]

   } {
      set ret [lindex $stk [expr $levels - 1] ]
      set stk [lrange $stk $levels end]
   } 

   return $ret
}

proc Push {stack value} {
#+
#  Name:
#    Push
#
#  Purpose:
#    Enter a new value onto the top of the supplied FILO stack.
#
#  Arguments:
#    stack
#       The name (NOT the value) of a global list variable holding the stack. 
#       On exit, the list holds one more element than on entry.
#    value
#       The value to be pushed onto stack.
#
#  Returned Value:
#    The supplied value.
#
#  Notes:
#    - The new entry is stored at index 0 in the list, and existing entries
#    are moved up to make room for it.
#-
   upvar #0 $stack stk
   set stk [linsert $stk 0 $value]
   return $value
}

proc RealValue {name width value command args} {
#+
#  Name:
#    RealValue
#
#  Purpose:
#    Create a simple numerical value entry "widget". 
#
#  Arguments:
#    name
#      The name of the "entry" to create (eg ".wm.maxval")
#    width
#      The number of characters in the text entry widget.
#    value
#      The name (NOT the value) of the global variable to receive the 
#      numerical value. Note, this must be a *global* variable.
#    command
#      A command to execute after a valid value has been assigned to the
#      variable.
#    args 
#      Any further options to pass to the command which creates the
#      "entry" widget (optional).
#
#  Returned Value:
#    The name of the entry widget.
#
#  Globals:
#    OLD_VAL (Write)
#      The previous (valid) value displayed in the text entry widget.
#-

   global $value
   upvar #0 $value varr

# Create the text entry widget. The text in this widget mirrors the value in
# the supplied global variable.
   eval entry $name -width $width -relief sunken -bd 2 -textvariable $value \
          -justify center $args

# When the pointer enters the text entry area, select the entire current
# contents of the widget so that typing a single character will delete it.
# Also take the focus, and save the current numerical value so that it
# can be re-instated if the user enters a duff value
   bind $name <Enter> \
      "if { \[$name cget -state\] == \"normal\" } {
          $name select from 0
          $name select to end

          focus $name

          set OLD_VAL \$$value
       }"

# When <Return> is pressed or the focus leaves the current entry, check that 
# the current text represents a valid value (if not, the old value will be 
# re-instated). 
   set check "if { \[$name cget -state\] == \"normal\" } {
                set $value \[string trim \$$value\]
                if { \[scan \$$value \"%%g\" $value\] < 1 } { 
                   set $value \$OLD_VAL 
                } elseif { \$OLD_VAL != \$$value } {
                   eval \"$command\"
                }
             }"
   bind $name <Return> $check
   bind $name <FocusOut> $check

# Store a command to process termination of data entry. Clear the current 
# selection, pass the focus back to the window which had it before, and 
# check that the current text represents a valid value (if not, the old 
# value will be re-instated).
   set done "if { \[$name cget -state\] == \"normal\" } {
                $name select clear
                set $value \[string trim \$$value\]
                if { \[scan \$$value \"%%g\" $value\] < 1 } { 
                   set $value \$OLD_VAL 
                } elseif { \$OLD_VAL != \$$value } {
                   eval \"$command\"
                }
             }"

# Execute this command when the pointer leaves the text entry area, when
# <Tab> is pressed.
   bind $name <Leave> $done
   bind $name <Tab> $done

# Return the name of the created entry widget.
   return $name
}

proc ReCentre {cx cy} {
#+
#  Name:
#    ReCentre
#
#  Purpose:
#    This re-displays the image centred at the supplied position.
#
#  Arguments:
#    cx
#       The canvas X coordinate at which the click occurred.
#    cy
#       The canvas Y coordinate at which the click occurred.
#
#  Globals:
#     GWM_SECTION (Read)
#       The displayed image section (eg "(10:200,23:68)" ).
#-
   global GWM_MODE
   global PRE_MODE4
   global GWM_SECTION
   global GWM_SECTION_STACK
   global GWM_UNZOOM

# Convert the canvas coords to NDF pixel coords.
   set pxy [GwmCanToNDF $cx $cy]
   set px [lindex $pxy 0]
   set py [lindex $pxy 1]

# Find a section which maps onto the displayed section but fills the
# entire GWM display area.
   set fullsec [ScreenSec $GWM_SECTION]

# Find the pixel indices at the centre of this section.
   set centre [SecCen $fullsec]
   set pcx [lindex $centre 0]
   set pcy [lindex $centre 1]

# Find the shift from the currently displayed centre to the required
# centre.
   set dx [expr $px - $pcx + 0.5 ]
   set dy [expr $py - $pcy + 0.5 ]

# Shift each pixel index bound in the section by the required amount.
   set sec [SecList $fullsec]
   if { $sec != "" } {
      set lx [expr round( [lindex $sec 0] + $dx )]
      set ux [expr round( [lindex $sec 1] + $dx )]
      set ly [expr round( [lindex $sec 2] + $dy )]
      set uy [expr round( [lindex $sec 3] + $dy )]

# Save the currently displayed section on the section stack so that it can be
# restored later using the Unzoom button.
      Push GWM_SECTION_STACK $GWM_SECTION
      $GWM_UNZOOM configure -state normal

# If we are in mode 4, reset the mode to the mode which was active before
# mode 4 was entered.
      if { $GWM_MODE == 4 } { SetMode $PRE_MODE4 }

# Display the modified section.
      set GWM_SECTION "($lx:$ux,$ly:$uy)"
      GwmUpdate

# Report an error if the section was invalid.
   } {
      Message "String \"$sec\" is not a valid section."
   }

}

proc ReleaseBind {x y} {
#+
#  Name:
#    ReleaseBind
#
#  Purpose:
#    Process button-1 releases over the image. 
#
#  Arguments:
#    x
#       The screen X coord.
#    y
#       The screen Y coord.
#
#  Globals:
#     GWM_CAN (Read)
#        The name of the canvas widget holding the GWM image.
#     GWM_CANCEL (Read)
#        The path to the "Cancel" button.
#     GWM_DELETE (Read)
#        Path to the "Delete" button.
#     LABEL (Read)
#        The label of the feature currently being pointed at.
#     GWM_MODE (Read and Write)
#        The interaction mode determining how to process button clicks
#        and motion in the GWM canvas item. Modes are:
#           0 - The user specifies image features by clicking on them.
#           1 - The user starts a new mask polygon, or edits an existing one.
#           2 - The user completes a started mask polygon.
#           3 - The user selects a label by clicking on an existing feature.
#           4 - The user selects a position and the image is redisplayed
#               centred on the supplied position.
#     GWM_ROOTI (Read)
#        The position index of the vertex being pointed at, or the position
#        index of the vertex at the start of the vector being pointed at,
#        or null if neither a vertex nor a vector is being pointed at.
#     GWM_ROOTX (Read)
#        The canvas X coordinate at which the button was pressed.
#     GWM_ROOTY (Read)
#        The canvas Y coordinate at which the button was pressed.
#     GWM_SEL_AREA (Read and Write)
#        The bounds of the selected area in the order xmin, xmax, ymin,
#        ymax. Set to a null string on exit.
#     GWM_V0 (Read and Write)
#        The index of the position corresponding to the first vertex in
#        an incomplete (i.e. open) polygon.
#     GWM_V1 (Read and Write)
#        The index of the position corresponding to the last vertex in
#        an incomplete (i.e. open) polygon.
#     GWM_VCX0 (Read and Write)
#        The canvas X coordinates at the first vertex in an incomplete 
#        (i.e. open) polygon.
#     GWM_VCY0 (Read and Write)
#        The canvas Y coordinates at the first vertex in an incomplete 
#        (i.e. open) polygon.
#     GWM_VID0 (Read)
#        The canvas item id for the vertex being pointed at (if any).
#     GWM_VID2 (Read and Write)
#        The canvas item id of the vector joining the last vertex in an
#        incomplete (i.e. open) polygon, to the pointer.
#     GWM_ZOOM (Read)
#        Path to the "Zoom" button.
#-
   global GWM_CAN
   global GWM_CANCEL
   global GWM_POLCOL
   global GWM_DELETE
   global LABEL    
   global GWM_MODE
   global GWM_ROOTI
   global GWM_ROOTX
   global GWM_ROOTY
   global GWM_SEL_AREA
   global GWM_V0
   global GWM_V1
   global GWM_VCX0
   global GWM_VCY0
   global GWM_VID0
   global GWM_VID2
   global GWM_ZOOM

# If there is a selected area, check that it is of significant size. If
# it isn't, cancel it. 
   if { $GWM_SEL_AREA != "" } {
      set dx [expr [lindex $GWM_SEL_AREA 2] - [lindex $GWM_SEL_AREA 0] ]
      set dy [expr [lindex $GWM_SEL_AREA 3] - [lindex $GWM_SEL_AREA 1] ]
      if { $dx < 4 && $dy < 4 } {
         CancelArea
      }
   }

# If there is still a selected area, activate the relevant buttons.
   if { $GWM_SEL_AREA != "" } {
      $GWM_ZOOM configure -state normal
      $GWM_DELETE configure -state normal
      $GWM_CANCEL configure -state normal

# Otherwise, what we do depends on the interaction mode.
   } {

# Mode 1 - "Edit an existing mask polygon"
      if { $GWM_MODE == 1 } {

# If we have been dragging a vertex, store the new coordinates of the
# vertex.
         if { $GWM_ROOTI != "" } {
            set cxy [$GWM_CAN coords $GWM_VID0]
            set cx [lindex $cxy 0]
            set cy [lindex $cxy 1]
            SetPosn $GWM_ROOTI "CX CY" [list $cx $cy]

# If we are not dragging a vertex, then we must have initiated a new
# polygon. Record the initial position and enter mode 2.
         } {

# Record the new position. The index of the "next" vertex is as yet unknown
# so set NXT to -1.
            set newi [SetPosn -1 "CX CY NXT" [list $GWM_ROOTX $GWM_ROOTY -1]]

# Create a vector attached to the new position. It initially has zero
# length. Store the canvas item id for this vector.
            set vid [$GWM_CAN create line $GWM_ROOTX $GWM_ROOTY $GWM_ROOTX $GWM_ROOTY -fill $GWM_POLCOL -tags vectors]
            SetPosn $newi VID $vid

# Create the marker for the new positon, and store its canvas id.
            set id [MarkPosn $newi] 
            SetPosn $newi ID $id

# Store the global values needed to construct the new polygon.
            set GWM_V0 $newi
            set GWM_V1 $newi
            set GWM_VCX0 $GWM_ROOTX
            set GWM_VCY0 $GWM_ROOTY
            set GWM_VID2 $vid

# Enter mode 2.
            SetMode 2    
         }

# Mode 2 - "Enter a new mask polygon"
      } elseif { $GWM_MODE == 2 } {

# If we are pointing well away from the first vertex in the polygon, add
# a new vertex to the polygon.
        if { $GWM_ROOTX > [expr $GWM_VCX0 + 4] || $GWM_ROOTX < [expr $GWM_VCX0 - 4] ||
             $GWM_ROOTY > [expr $GWM_VCY0 + 4] || $GWM_ROOTY < [expr $GWM_VCY0 - 4] } {

# Record the new position. The index of the "next" vertex is as yet unknown
# so set NXT to -1.
            set newi [SetPosn -1 "CX CY NXT" [list $GWM_ROOTX $GWM_ROOTY -1]]

# We now know the index of the "next" vertex for the previous vertex.
# Record it.
            SetPosn $GWM_V1 NXT $newi

# Create a vector attached to the new position. It initially has zero
# length. Store the canvas item id for this vector.
            set vid [$GWM_CAN create line $GWM_ROOTX $GWM_ROOTY $GWM_ROOTX $GWM_ROOTY -fill $GWM_POLCOL -tags vectors]
            SetPosn $newi "VID" $vid

# Create the marker for the new positon, and store its canvas id.
            set id [MarkPosn $newi] 
            SetPosn $newi ID $id

# Store the global values needed to construct the new polygon.
            set GWM_V1 $newi
            set GWM_VID2 $vid

# If we are pointing close to the original vertex, close the polygon.
         } {

# First count the number of vertices in the polygon. If it is less than
# 3, delete the entire polygon.
            if { [NumPosn $GWM_V0] < 3 } {
               Message "Polygon has less than 3 vertices and will be deleted."
               DelPosn $GWM_V0 1 

# Otherwise...
            } {
 
# Raise the marker for the first vertex so that it is on top of the vector
# which ends there. This ensures that the marker is picked up in
# preference to the vector (for instance when pointing at it with the
# mouse).
               $GWM_CAN raise [GetPosn $GWM_V0 ID] $GWM_VID2

# Record the index of the "next" vertex for the current vertex , and
# enter mode 1.
               SetPosn $GWM_V1 NXT $GWM_V0
               SetMode 1
            }
         }

# Mode 4 - "Re-centre the image"
      } elseif { $GWM_MODE == 4 } {

# Store the pixel coordinates of the pointer in POINTER_PXY.
         set cx [$GWM_CAN canvasx $x]
         set cy [$GWM_CAN canvasy $y]

# Re-display the image with the new centre.
         ReCentre $cx $cy

      }
   }
}

proc Save {} {
#+
#  Name:
#     Save
#
#  Purpose:
#     Resample each input image using the currently defined mappings and
#     masks, to create the required output images.
#
#  Arguments:
#     None.
#
#  Returned Value:
#     One for success, zero for failure.
#
#  Globals:
#     ATASK_OUTPUT (Read)
#        Any non-error messages are appended to this list. Each
#        message is stored as a new element in the list.
#     FITTYPE (Read)
#        A textual description of the mapping to be used for the image mapping.
#     INTERP (Read)
#        The interpolation method to use when resampling the input images. 
#     MAP_RX (Write)
#        The anti-clockwise rotation from the input image's X axis to the
#        output image's X axis (in degrees).
#     MAP_RY (Write)
#        The anti-clockwise rotation from the input image's X axis to the
#        output image's Y axis (in degrees).
#     MAPTYPE (Read)
#        A list containing the textual descriptions of the available
#        mapping types.
#     OEFITTYPE (Read)
#        A textual description of the mapping to be used for the OE mapping.
#     OUTIMS (Read)
#        A 2-D array, indexed by input image and ray, giving the name of
#        the corresponding output image.
#     O_RAY_MASK (Read)
#        An integer representing the "O-ray mask" object type.
#     PNTNXT (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of integers representing indices within the lists 
#        given by PNTxxx. Each integer gives the index of the next position
#        along the edge of a polygon. The vector starting at position
#        index i, ends at position index given by the i'th element of
#        PNTNXT. If this value is null ("") then position i is not part of
#        a polygon. If this value is -1, then the end of the vector starting
#        position i is unspecified (in this case the polygon is not
#        closed).
#     PNTPX (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel X coordinates. 
#     PNTPY (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel Y coordinates. 
#     RESAVE (Read and Write)
#        Set to zero if the the mappings and masks have not changed since
#        the last time the output images were saved. Set to a non-zero
#        value if the output images are out-of-date with respect to the 
#        mappings and/or masks.
#     SKYOFF (Read)
#        Should sky subtraction be performed? If not, then the input image
#        name is returned unchanged.
#     SKY_AREA (Read)
#        One of the integer values which may be assigned to SKY_METHOD:
#        indicates that sky background is to be performed by fitting
#        surfaces to areas within the supplied object frames (i.e. the 
#        O and E sky areas).
#     SKY_METHOD (Read)
#        The sky subtraction method to use; $SKY_AREA or $SKY_FRAME. Any
#        other value results in no sky subtraction being performed (in which
#        case the supplied input data is simply copied to the output).
#
#-
   global ATASK_OUTPUT
   global CANCEL_OP
   global DBEAM
   global EFFECTS_MAPPINGS
   global FITTYPE
   global INTERP
   global MAPTYPE
   global MAP_RX
   global MAP_RY
   global OEFITTYPE
   global OS
   global OUTIMS
   global O_RAY_MASK
   global O_RAY_SKY   
   global PNTNXT
   global PNTPX 
   global PNTPY
   global POLPACK_DIR
   global POLY
   global RESAVE
   global SAFE
   global S_FONT
   global SKYOFF
   global SKY_AREA
   global SKY_METHOD

# Begin a temporary file context.
   set tfc [BeginUF]

# |Display a warning and return if the images have already been saved.
   if { !$RESAVE } { 
      Message "The output images are already up-to-date with respect to the current mappings and masks."
      return 1 
   }

# Tell the user what is happening.
   set told [SetInfo "Creating new output images. Please wait... " 0]

# Assume failure.
   set ok 0

# In single-beam mode we just use the O-ray lists.
   if { $DBEAM } {
      set rays "O E"
   } {
      set rays "O"
   }

# Ensure that all the required mappings are available. Return without
# action if any are missing.
   if { [AllMappings] } {

# Save the name of the first (reference) image.
      set im0 [lindex $IMAGES 0]

# If we are in dual-beam mode,  ensure that the reference image has an O-ray 
# mask. Since we already know that all mappings are available, all other 
# masks can be created from this mask if necessary.
      if { $DBEAM && ![CreateMask $im0 $O_RAY_MASK] } {
         Message "Masks defining the O and E ray areas have not yet been supplied."

# If the sky to be subtracted is defined within the object frames, 
# ensure that sky areas have been defined.
      } elseif { $SKYOFF && $SKY_METHOD == $SKY_AREA && ![CreateMask $im0 $O_RAY_SKY] } {
         Message "The areas containing sky have not yet been supplied."

# Otherwise, process each image section in turn...
      } {

# Create the top level window for the dialogue box which displays progress.
         set top .progress
         set topf [MakeDialog $top "Progress,,," 1]
         SetHelp $topf ".  This dialog box indicates the progress which has been made towards creating and saving the output images."

# Create abd pack all the items in the dialog box.
         set back "#b0b0b0"

         set fr0 [frame $topf.fr0]
         pack $fr0 -side top -fill both -expand 1

         set imlist [frame $fr0.imlist -bd 2 -relief raised -background $back]
         SetHelp $imlist ".  A list of all the input images. A tick mark is displayed next to the images which have been completed. "
         pack [label $imlist.lb -text " Input images " -font $S_FONT -background $back] -side top -pady 1m

         set pfrm [frame $fr0.pfrm]
         SetHelp $pfrm ".  A list of the processing stages involved in creating the output images. Tick marks are displayed next to the stages which have been completed, and the current stage is highlighted in red. "
         pack $imlist $pfrm -side left -fill both -padx 4m -pady 4m

         foreach image $IMAGES {
            set fr [frame $imlist.$image -background $back]
            pack $fr -side top -fill x

            set lb($image) [label $fr.lb -text $image -anchor w \
                            -background $back]
            pack $lb($image) -side left -padx 2m -fill x
            
            set tick($image) [label $fr.tick -foreground $back \
                              -background $back -bitmap @$POLPACK_DIR/tick.bit]
            pack $tick($image) -side right
         }

         set image ""
         set fr1 [frame $pfrm.fr1]
         pack $fr1 -side top -anchor w -fill both -expand 1
         set l1 [label $fr1.l1 -text "Doing image: " -font $S_FONT]
         set l2 [label $fr1.l2 -textvariable image]
         pack $l1 $l2 -side left -padx 6m 

         set fr234 [frame $pfrm.fr234 -background $back -bd 2 -relief raised]
         pack $fr234 -side left -padx 5m -pady 3m -fill both -expand 1
         pack [label $fr234.lb -text "Processing stages" -font $S_FONT -background $back] -side top 

         set fr2 [frame $fr234.fr2 -background $back]
         set fr3 [frame $fr234.fr3 -background $back]
         set fr4 [frame $fr234.fr4 -background $back]
         pack $fr2 $fr3 $fr4 -side left -fill y -expand 1

         if { $DBEAM } {
            pack [label $fr2.r0 -text " " -background $back -height 1] \
                 [label $fr3.r0 -text "O" -background $back -height 1] \
                 [label $fr4.r0 -text "E" -background $back -height 1] \
                 -side top -fill both -expand 1
         }

         if { $SKYOFF } {
            set selab [label $fr2.r0b -text "Sky estimation:" -background $back]
            set setick(O) [label $fr3.r0b -foreground $back \
                            -background $back -bitmap @$POLPACK_DIR/tick.bit]
            set setick(E) [label $fr4.r0b -foreground $back \
                            -background $back -bitmap @$POLPACK_DIR/tick.bit]
            pack $selab     -side top -anchor w -fill y -expand 1
            pack $setick(O) -side top -anchor w -fill y -expand 1
            pack $setick(E) -side top -anchor w -fill y -expand 1
   
            set sslab [label $fr2.r1 -text "Sky subtraction:" -background $back]
            set sstick(O) [label $fr3.r1 -foreground $back \
                            -background $back -bitmap @$POLPACK_DIR/tick.bit]
            set sstick(E) [label $fr4.r1 -foreground $back \
                            -background $back -bitmap @$POLPACK_DIR/tick.bit]
            pack $sslab     -side top -anchor w -fill y -expand 1
            pack $sstick(O) -side top -anchor w -fill y -expand 1
            pack $sstick(E) -side top -anchor w -fill y -expand 1
         } {
            set selab "no.such.widget"
            set setick(O) "no.such.widget"
            set setick(E) "no.such.widget"
            set sslab "no.such.widget"
            set sstick(O) "no.such.widget"
            set sstick(E) "no.such.widget"
         }
         
         set melab [label $fr2.r2 -text "Mask extraction:" -background $back]
         set metick(O) [label $fr3.r2 -foreground $back \
                         -background $back -bitmap @$POLPACK_DIR/tick.bit]
         set metick(E) [label $fr4.r2 -foreground $back \
                         -background $back -bitmap @$POLPACK_DIR/tick.bit]
         pack $melab     -side top -anchor w -fill y -expand 1
         pack $metick(O) -side top -anchor w -fill y -expand 1
         pack $metick(E) -side top -anchor w -fill y -expand 1

         set ialab [label $fr2.r3 -text "Image alignment:" -background $back]
         set iatick(O) [label $fr3.r3 -foreground $back \
                         -background $back -bitmap @$POLPACK_DIR/tick.bit]
         set iatick(E) [label $fr4.r3 -foreground $back \
                         -background $back -bitmap @$POLPACK_DIR/tick.bit]
         pack $ialab     -side top -anchor w -fill y -expand 1
         pack $iatick(O) -side top -anchor w -fill y -expand 1
         pack $iatick(E) -side top -anchor w -fill y -expand 1

         set hilab [label $fr2.r4 -text "Header information:" -background $back]
         set hitick(O) [label $fr3.r4 -foreground $back \
                         -background $back -bitmap @$POLPACK_DIR/tick.bit]
         set hitick(E) [label $fr4.r4 -foreground $back \
                         -background $back -bitmap @$POLPACK_DIR/tick.bit]
         pack $hilab     -side top -anchor w -fill y -expand 1
         pack $hitick(O) -side top -anchor w -fill y -expand 1
         pack $hitick(E) -side top -anchor w -fill y -expand 1

# Create the button bar.
         set butfrm [frame $topf.butfrm]
         pack $butfrm -side top -fill both -expand 1

         set b1 [button $butfrm.close -text "Close" -command "destroy $top"]
         set b2 [button $butfrm.cancel -text "Cancel" -command {set CANCEL_OP 1}]

         SetHelp $b1 ".  Press to close the dialog box, proceeding silently with the operation."
         SetHelp $b2 ".  Press to close the dialog box, cancelling the operation."

         pack $b1 $b2 -side left -expand 1 -padx 2m -pady 2m

# Ensure that closing the window from the window manager is like pressing
# the Close button.
         wm protocol $top WM_DELETE_WINDOW "destroy $top"

# Ensure X events continue to be delived to the progress dialog box even
# while ADAM tasks are being executed.
         set old_safe $SAFE
         set SAFE $top

# Process each of the input images in turn.
         set ok 1
         foreach imsec $IMSECS {

# Begin a temporary file context.
            set tfci [BeginUF]

# Make the ticks invisible in the progress report.
            Wop $setick(O) configure -foreground $back
            Wop $setick(E) configure -foreground $back
            Wop $sstick(O) configure -foreground $back
            Wop $sstick(E) configure -foreground $back
            Wop $metick(O) configure -foreground $back
            Wop $metick(E) configure -foreground $back
            Wop $iatick(O) configure -foreground $back
            Wop $iatick(E) configure -foreground $back
            Wop $hitick(O) configure -foreground $back
            Wop $hitick(E) configure -foreground $back

# Extract the image name section string from the image-section string.
            GetSec $imsec image section

# Do the sky subtraction.
            set image2 [SkySub $imsec $image 1 $selab $setick(O) $setick(E) \
                                               $sslab $sstick(O) $sstick(E) ]
            if { $image2 == "" } {
               set ok 0
               break
            }

# If the "align" effect has been applied to this image, the positions
# lists and mappings will refer to the aligned image, not the original
# image. Temporarily remove the effects of "Align" so that the positions
# list, etc, refer to the original image. To do this we first combine all
# the mappings together into a single mapping.
            set tot_map [TotalMap $image]

# Now undo the effects of this mapping.
            if { $tot_map != "ref" && $tot_map != "" } {
               if { ![MappingMod $image $tot_map 1] } {
                  Message "Cannot undo the effects applied to image \"$image\"."
                  set tot_map "ref"
                  set ok 0
                  break
               }
            } {
               set tot_map "ref"
            }

# Extract the required ray areas into separate images.
            foreach ray $rays {
               upvar #0 ${ray}_RAY_MASK obj

# Highlight the "mask extraction" label in red in the progress dialog box.
               Wop $melab configure -foreground red

# If in dual-beam mode, ensure that the mask is available.
               set got_mask [CreateMask $image $obj] 
               if { $DBEAM && !$got_mask } { 
                  set ok 0
                  break 
               }

# If we have a mask, find the upper and lower limits of the pixel coordinates 
# for the mask and create a standard section specifier from them.
               if { $got_mask } {
                  set sect [BoundBox $PNTPX($image,$obj) $PNTPY($image,$obj) "" 0]
                  if { $sect == "" } {
                     set ok 0
                     break
                  }

# Extract the mask area.
                  set maskarea [Segment ${image2}${sect} "" $image $obj]

# If there is no mask, just copy the whole of the supplied image section.
               } {
                  set maskarea [UniqueFile]
                  set in "${image2}${section}"
                  if { ![Obey ndfpack ndfcopy "in=$in out=$maskarea"] } {
                     set ok 0
                     break
                  }
               }

# Make the "mask extraction" label (and associated tick mark) in the 
# progress dialog box revert to black, and highlight the "image alignment" 
# label in red.
               Wop $melab configure -foreground black
               Wop $metick($ray) configure -foreground black
               Wop $ialab configure -foreground red 

# Determine the output image name.
               set outim $OUTIMS($image,$ray)

# We now determine the mapping to be used to align this image. For O-ray
# masks the image mapping is used (ie the mapping from $image to the first
# (reference) image), together with a unit OE mapping (indicated by the
# string "ref")..
               if { $ray == "O" } {
                  set map1 "ref"
                  set map2 [ImageMapping $image]

# For E-ray masks the genuine OE mapping (from E to 0) is used with 
# the image mapping.
               } {
                  set map1 [OEMapping $image]
                  set map2 [ImageMapping $image]
               }

# Concatenate the two mappings.
               set map [ConcMap $map1 0 $map2 0]

# If a mapping exists find the bounds of the mask area in the transformed
# image. These bounds are passed on to CCDPACK:TRANNDF as the required
# bounds for the output image so that the output image contains only the
# area of interest.
               if { $map != "" } {

                  if { $got_mask } {
                     set sect [BoundBox $PNTPX($image,$obj) \
                                        $PNTPY($image,$obj) $map 0]
                     if { $sect == "" } {
                        set ok 0
                        break
                     } 
                  } {
                     set sect ""
                  }         

# Transform the mask area using the mapping (if defined). 
                  if { ![TranImage $maskarea $map $outim $sect] } {
                     set ok 0
                     break
                  }

# Make the "image alignment" label (and associated tick mark) in the 
# progress dialog box revert to black, and highlight the "header information"
# label in red.
                  Wop $ialab configure -foreground black
                  Wop $iatick($ray) configure -foreground black
                  Wop $hilab configure -foreground red

# Get the numerical index of the fit type used between images.
                  foreach fittype [array names MAPTYPE] {
                     if { $MAPTYPE($fittype) == $FITTYPE } { break }
                  }

# Get the numerical index of the fit type used between O and E rays.
                  foreach oefittype [array names MAPTYPE] {
                     if { $MAPTYPE($oefittype) == $OEFITTYPE } { break }
                  }

# If either of these fit types allows shear, then the concatenated
# mapping may involve shear.
                  if { $oefittype == 5 || $fittype == 5 } { 
                     set shear 1
                  } {
                     set shear 0
                  }

# Calculate the rotations, magnifications and shifts corresponding to the
# mapping.
                  DescMap $map
                  ConvMap 1 ""

# Add the required components to the POLPACK extension...
# The ray label (only in dual-beam mode).
                  if { $DBEAM } {
                     if { ![Extension $outim RAY _CHAR*1 $ray ""] } {
                        set ok 0
                        break
                     }
                  }

# The axis rotations. If the input image already has an X axis rotation,
# add the new rotation onto it.
                  if { ![Extension $outim ROTATION _REAL "" old_rot] } { 
                     set ok 0 
                     break
                  }

                  if { $old_rot != "" } {
                     set rot [expr $old_rot + $MAP_RX]
                  } {
                     set rot $MAP_RX
                  }

# Write out the X axis rotation.
                  if { ![Extension $outim ROTATION _REAL $rot ""] } {
                     set ok 0
                     break
                  }

# If the input image already has an Y axis rotation, add the new rotation 
# onto it (and note that the output may contain shear).
                  if { ![Extension $outim YROTATION _REAL "" old_rot] } { 
                     set ok 0 
                     break
                  }

                  if { $old_rot != "" } {
                     set yrot [expr $old_rot + $MAP_RY]
                     set shear 1
                  } {
                     set yrot $MAP_RY
                  }

# If the output image may contain shear, write out the YROTATION keyword.
                  if { $shear } {
                     if { ![Extension $outim YROTATION _REAL $yrot ""] } {
                        set ok 0
                        break
                     }
                  }

# The image identifier. This is only assigned a value if the IMGID component 
# does not already exist. In this case, the name of the input image is
# used.
                  if { ![Extension $outim IMGID _CHAR "" old_plate] } { 
                     set ok 0 
                     break
                  }

                  if { $old_plate == "" } {
                     set plate [file tail $image]
                     if { ![Extension $outim IMGID _CHAR $plate ""] } { 
                        set ok 0 
                        break
                     }
                  }

# Make the "header information" label (and associated tick mark) in the 
# progress dialog box revert to black.
                  Wop $hilab configure -foreground black
                  Wop $hitick($ray) configure -foreground black
                  update idletasks

# Null mappings should not happen.
               } {
                  set ok 0
                  break            
               }
            }

# Now re-apply the effects of the effects mappings to the positions lists
# associated with the current image.
            if { $tot_map != "ref" } {
               if { ![MappingMod $image $tot_map 0] } {
                  Message "Cannot re-instate the effects applied to image \"$image\"."
                  set ok 0
                  break
               }
            }

# Set the foreground colour of the image's tick in the progress dialog
# box to black.
            Wop $tick($image) configure -foreground black

# Delete all the temporary files created in this pass through the loop.
            EndUF $tfci ""

# Leave the image loop if an error has occurred.
            if { !$ok } { break }

         }

# Ensure X events are ignored while ADAM tasks are being executed.
         set SAFE $old_safe

# Destroy the progress dialog box.
         if { [winfo exists $top] } { 
            after 1000
            destroy $top 
         }
      }
   }

# If an error occurred, give a contextual message, and delete any output 
# images which were created.
   if { !$ok } { 
      Message "The registered images could not be saved." 
      set CANCEL_OP 0
      foreach image $IMAGES {
         foreach ray $rays {
            set file $OUTIMS($image,$ray)
            catch "exec rm -f ${file}.*"
         }
      }

# If no error occurred, indicate that the output images do not need to
# be re-saved until the mappings have changed or the masks have changed.
   } {
      set RESAVE 0
   }

# Delete all the temporary files created in this procedure.
   EndUF $tfc ""

# Cancel the informative text set earlier in this procedure.
   if { $told } { SetInfo "" 0 }

   return $ok

}

proc SaveOptions {} {
#+
#  Name:
#     SaveOptions
#
#  Purpose:
#     Copy the option values currently in use to the variables which are
#     read back by the PolMap atask upon completion. They will then be
#     stored in the Atask's parameter file.
#
#  Arguments:
#     None.
#
#  Globals:
#      ATASK_BACK (Write)
#         The value of GWM_BACK sent to the A-task.
#      ATASK_HAREA (Write)
#         The value of HAREA sent to the A-task.
#      ATASK_SAREA (Write)
#         The value of SAREA sent to the A-task.
#      ATASK_SI (Write)
#         A string representing the SI_LIST list.
#      GWM_BACK (Read)
#         The form of the displayed background image.
#      HAREA (Read)
#         Should the help area be displayed?
#      SAREA (Read)
#         Should the status area be displayed?
#      SI_LIST (Read)
#         A list of indices identifying the status items to be displayed
#         in the status area. These indices point into the SI_VARS,
#         SI_LABELS and SI_HELPS lists.
#      SI_VARS (Read)
#         A list of all the variables which are available to be displayed
#         in the status area.
#
#-
   global ATASK_NCONT
   global ATASK_BADCOL
   global ATASK_CONCOL
   global ATASK_BACK
   global ATASK_HAREA
   global ATASK_PHI
   global ATASK_PLO
   global ATASK_SAREA
   global ATASK_SELCOL
   global ATASK_SI
   global ATASK_XHAIR
   global ATASK_XHRCOL          
   global GWM_BADCOL
   global GWM_CONCOL
   global CHAR_LIST
   global CHAR_STOP
   global GWM_BACK
   global HAREA
   global PHI_REQ
   global PLO_REQ
   global SAREA
   global GWM_SELCOL            
   global GWM_NCONT
   global SI_LIST
   global SI_VARS
   global GWM_XHAIR
   global GWM_XHRCOL

# Only proceed if the operation is confirmed...
   if { [Confirm "Save current options values?"] } {

# Just take a copy of each of the current options values.
     set ATASK_NCONT $GWM_NCONT
     set ATASK_HAREA $HAREA
     set ATASK_BACK $GWM_BACK
     set ATASK_SAREA $SAREA
     set ATASK_SELCOL $GWM_SELCOL
     set ATASK_BADCOL $GWM_BADCOL
     set ATASK_CONCOL $GWM_CONCOL
     set ATASK_XHRCOL $GWM_XHRCOL
     set ATASK_XHAIR $GWM_XHAIR
     set ATASK_PLO $PLO_REQ
     set ATASK_PHI $PHI_REQ

# The list of item integer identifiers in SI_LIST are combined into
# a single string for passing to the ATASK.
     set ATASK_SI ""
     for {set i 0} {$i < [llength $SI_LIST]} {incr i} {
        set id [lindex $SI_LIST $i]
        append ATASK_SI [string index $CHAR_LIST $id]
     }
     append ATASK_SI $CHAR_STOP
     append ATASK_SI [llength $SI_VARS]
   }

}

proc ScreenSec {section} {
#+
#  Name:
#     ScreenSec
#
#  Purpose:
#     Expand the supplied section in one direction so that it is 
#     square (and will thus fill the GWM display area).
#
#  Arguments:
#     section
#        The section string to be expanded.
#
#  Returned Value:
#     The expanded section string, or the supplied section string
#     if it cannot be expanded.
#-

# Initialise the returned section to equal the supplied section.
   set newsec $section

# Get a list containing the x and y bounds extracted from the supplied
# string. 
   set sec [SecList $section]

# Find the pixel indices at the centre of the supplied section.
   set cc [SecCen $section]

# Only proceed if these both succeeded.
   if { $sec != "" && $cc != "" } {

# Extract the coordinates from the lists.
      set xl [lindex $sec 0]
      set xu [lindex $sec 1]
      set yl [lindex $sec 2]
      set yu [lindex $sec 3]

      set xc [lindex $cc 0]
      set yc [lindex $cc 1]

# Find the width and height of the section.
      set width [expr $xu - $xl + 1 ]      
      set height [expr $yu - $yl + 1 ]      

# If the width is smaller than the height, adjust the x bounds to make
# the section square.
      if { $width < $height } {
         set xl [expr floor( $xc - 0.5 * $height ) ]
         set xu [expr $xl + $height - 1 ]

# Otherwise, if the width is greater than the height, adjust the y bounds 
# to make the section square.
      } elseif { $width > $height } {
         set yl [expr floor( $yc - 0.5 * $width ) ]
         set yu [expr $yl + $width - 1 ]
      }

# Construct a new section string.
      set newsec "($xl:$xu,$yl:$yu)"    
   } 

   return $newsec
}

proc SecCen {section} {
#+
#  Name:
#    SecCen
#
#  Purpose:
#    Find the centre pixel coordinates of the supplied NDF section.
#
#  Arguments:
#    section
#      A section string (eg "(30:40,50:60)").
#
#  Returned Value:
#      A list of two values; the x and y pixel indices (integers) at the 
#      centre of the section.
#-
   set sec [SecList $section]
   if { $sec != "" } {
      set xlo [lindex $sec 0]
      set xhi [lindex $sec 1]
      set ylo [lindex $sec 2]
      set yhi [lindex $sec 3]
      set cx [expr round( 0.5*( $xlo + $xhi - 0.1 ) )]
      set cy [expr round( 0.5*( $ylo + $yhi - 0.1 ) )]

      return [list $cx $cy]
   } {
      return {}
   }
}

proc SecList {section} {
#+
#  Name:
#    SecList
#
#  Purpose:
#    Extracts the coordinates from an NDF section and returns them as a
#    list.
#
#  Arguments:
#    section
#      A section string (eg "(30:40,50:60)").
#
#  Returned Value:
#      A list of values in the order xlo, xhi, ylo, yhi, or an empty string
#      of the supplied string is not a valid section.
#-
   if { [regexp {\(([-+0-9\.]+):([-+0-9\.]+),([-+0-9\.]+):([-+0-9\.]+)\)} $section \
        match xlo xhi ylo yhi] } {
      return [list $xlo $xhi $ylo $yhi]
   } {
      return {}
   }
}

proc SelectFont {font} {
#+
#  Name:
#     SelectFont
#
#  Purpose:
#     Pick a nice font matching the supplied font pattern.
#
#  Arguments:
#     font
#        A font pattern suitable for use with xlsfont.
#
#  Returned Value:
#     A specific font matching the the supplied pattern. The first
#     matching font returned by xlsfonts is used, with the proviso that
#     font families are searched in the following order:
#        helvetica 
#        lucida 
#        fixed 
#        clean 
#        courier 
#        times 
#        charter 
#        new century schoolbook
#
#     A null string is returned if no matching font can be found belonging
#     to any of these families.
#-

# Initialise the returned font.
   set rfont ""

# Run xlsfonts to get a list of all matching fonts.
   if { ![catch "exec  xlsfonts -fn \"$font\"" fonts] } {

# Check each acceptable font family...
      foreach n [list helvetica lucida fixed clean courier times charter "new century schoolbook"] {

# Find the index of the first matching font in the current family. If
# found, get the full font name and leave the loop.
         set i 0
         while { $i > -1 } { 
            set i [lsearch -regexp $fonts "^-\[^-\]+-$n" ]
            set rfont [lindex $fonts $i]
            if { ![catch "button .test -font $rfont"] } {
               destroy .test
               break
            } {
               set rfont ""
               set fonts [lreplace $fonts $i $i]
            }
         }
         if { $rfont != "" } { break }
      }

# If the xlsfonts command failed display the message.
   } { 
      Message "An error occurred using xlsfonts to list fonts...\n $fonts"
   }

# Return the font.
   return $rfont
}

proc Seq {com delay id count} {
#+
#  Name:
#    Seq
#
#  Purpose:
#    Initiates a timed sequence of commands.
#
#  Arguments:
#    com
#       The command to execute in the timed sequence.
#    delay
#       The number of milliseconds between executions of the 
#       command given by "com".
#    id
#       A string which can be used to identify the sequence.
#    count
#       The name (note, NOT the value) of a variable in which to write
#       the number of entries made into the command so far.
#
#  Globals:
#    SEQ_STOP
#       If this is set to the id of the current sequence, then the
#       sequence is terminated, and SEQ_STOP is reset to an empty string.
#
#  Notes:
#    -  The sequence can be terminated by setting the global variable
#    SEQ_STOP to the id supplied when the sequence was initiated.
#-
   global SEQ_STOP

   upvar $count cnt

   set cnt [expr $cnt + 1 ]

   if { $SEQ_STOP != $id } {
      eval "$com"
      after $delay [list Seq $com $delay $id $count]
   } {
      set SEQ_STOP 0
   }
}

proc SetColours {var} {
#+
#  Name:
#     SetColours
#
#  Purpose:
#     Change the colours of any currently displayed parts of the display
#     to match the current colour selections in the Options menu.
#
#  Arguments:
#     var
#        The name of the global variable containing the colour which has
#        just been set by the user.
#
#  Globals:
#     GWM_BADCOL (Read)
#       The colour for the background (eg "cyan").
#     GWM_CONCOL (Read)
#       The colour for contours.
#     GWM_CAN (Read)
#        The name of the canvas widget holding the GWM image.
#     GWM_DEVICE (Read)
#       The GNS name of the graphice device.
#     GWM_POLCOL (Read)
#       The colour with which to draw polygons. 
#     GWM_SELCOL (Read)
#       The colour with which to mark the selected area.
#-
   global GWM_BADCOL
   global GWM_CONCOL
   global GWM_CAN
   global GWM_DEVICE
   global GWM_DISPLAY_NCONT
   global GWM_POLCOL
   global GWM_SELCOL
   global GWM_XHAIR_IDV
   global GWM_XHAIR_IDH
   global GWM_FORCE
   global GWM_XHRCOL

# Change the colour of any displayed selection box.
   if { $var == "GWM_SELCOL" } {
      $GWM_CAN itemconfigure sbox -outline $GWM_SELCOL

# Re-draw the contour map if its colour has changed.
   } elseif { $var == "GWM_CONCOL" } {
      if { $GWM_DISPLAY_NCONT > 0 } { 
         set GWM_FORCE 1
         GwmContour 
      }

# Re-draw the polygons if their colour has changed.
   } elseif { $var == "GWM_POLCOL" } {
      DrawPosns

# Change the colour of entry zero in the KAPPA pallette. This is used
# to mark missing pixels and the contour background. 
   } elseif { $var == "GWM_BADCOL" } {
      Obey kapview palentry "device=$GWM_DEVICE palnum=0 colour=$GWM_BADCOL" 1

# Change the colour of the cross-hair if it is curently in use.
   } elseif { $var == "GWM_XHRCOL" } {
      if { $GWM_XHAIR_IDH != "" } {
         $GWM_CAN itemconfigure $GWM_XHAIR_IDH -fill $GWM_XHRCOL
         $GWM_CAN itemconfigure $GWM_XHAIR_IDV -fill $GWM_XHRCOL
      }
   }

}

proc SetHelp {widget help args} {
#+
#  Name:
#    SetHelp
#
#  Purpose:
#    Set the text to appear at the bottom of the screen when the pointer
#    passes over a specified widget.
#
#  Arguments:
#    widget
#       The name of the widget (eg ".fr1.button").
#    help
#       The text to display. 
#    args
#       An optional htx cross-reference label to be associated with the
#       widget.
#
#  Globals:
#    HELP_LABELS (Write)
#       A 1-D array index by widget name. Each element is an htx
#       cross-reference label to be displayed if the widget is selected
#       using "Help on pointer".
#    HELPS (Write)
#       An array holding the current help text for each widget.
#
#-
   global HELPS
   global HELP_LABELS

# Store the supplied text.
   set HELPS($widget) $help

# Store the htx label for the widget (if any).
   if { $args != "" } {
      set HELP_LABELS($widget) $args
   }

# Ensure that the displayed help text is up-to-date. 
   Helper [winfo pointerx .] [winfo pointery .]
   
}

proc SetInfo {text def} {
#+
#  Name:
#     SetInfo
#
#  Purpose:
#     Decide on the text to be displayed above the GWM image.
#
#  Arguments:
#     text
#        A text string which can be displayed above the GWM image.
#     def
#        If non-zero, the the supplied text string is low priority and 
#        should be displayed only when there is no other more important 
#        text to be displayed. If zero, then the text is high priority
#        and is displayed immediately, so long as no other high priority
#        text is currently being displayed.
#
#  Returned Value:
#     One if the supplied text was adopted as either the current or
#     default text. Zero otherwise (eg if high priority text was supplied,
#     but there was already an high priority text string active).
#
#  Globals:
#     CURRENT_INFO (Read and Write)
#        The current high priority text.
#     DEFAULT_INFO (Read and Write)
#        The current low priority text.
#     GWM_INFO (Write)
#        The current text to be displayed.
#
#  Notes:
#     - If a null string ("") is supplied for "text", and "def" is non-zero,
#     then the low priority text (if any) will be displayed.
#-
   global CURRENT_INFO
   global DEFAULT_INFO
   global GWM_INFO

# If the supplied text is to be displayed whenever there is no current
# text, then save it in DEFAULT_INFO for later use.
   if { $def } { 
      set DEFAULT_INFO $text
      set told 1

# If the caller is suggesting a new value for the current text, only use
# it if any other previously suggested value has been cancelled.
   } {
      if { $text == "" || $CURRENT_INFO == "" } {
         set CURRENT_INFO $text
         set told 1
      } {
         set told 0
      }
   }

# Now decide on the text to be displayed. If we have a non-null current
# text value, then display it. otherwise display the default value.
   if { $CURRENT_INFO != "" } {
      set GWM_INFO $CURRENT_INFO
   } {
      set GWM_INFO $DEFAULT_INFO
   }   

   return $told
}

proc SetMode {mode} {
#+
#  Name:
#     SetMode
#
#  Purpose:
#     Change the GWM canvas interaction mode.
#
#  Arguments:
#     mode
#        The required interaction mode. This determines how to process button 
#        clicks and motion in the GWM canvas item. Modes are:
#           0 - The user specifies image positions by clicking on them.
#           1 - The user starts a new polygon, or edits an existing one.
#           2 - The user completes a started mask polygon.
#           3 - The user selects a label by clicking on an existing feature.
#           4 - The user selects a position and the image is redisplayed
#               centred on the supplied position.
#
#  Globals:
#     GWM_CAN (Read)
#        The name of the canvas widget holding the GWM image.
#     GWM_CANCEL (Read)
#        The path to the "Cancel" button.
#     GWM_MODE (Write)
#        The new interaction mode.
#-
   global GWM_CURRENT_LIST
   global GWM_CAN   
   global GWM_CANCEL
   global GWM_MODE
   global M4_CURSOR
   global OLD_CURSOR
   global M4_XHAIR
   global PRE_MODE4
   global GWM_V0
   global GWM_V1     
   global GWM_VCX0
   global GWM_VCY0
   global GWM_VID2
   global GWM_XHAIR

# If the previous mode was mode 4, change the cursor back to its previous
# value, switching the cross-hair back on again if required. Also,
# disable the cancel button.
   if { [info exists M4_XHAIR] } {
      $GWM_CAN config -cursor [Pop CURSOR_STACK]
      if { $M4_XHAIR } {
         set GWM_XHAIR 1
         Xhair 0 0
      }
      unset M4_XHAIR
      $GWM_CANCEL configure -state disabled
   }

# Select the required mode.
   set old_mode $GWM_MODE
   set GWM_MODE $mode

   if { $mode == 0 } {
      SetHelp $GWM_CAN ".  Click and drag to select an area of the image." POLMAP_MODE_0
      SetInfo "Pan and zoom the image..." 1
      set GWM_CURRENT_LIST ""
      GwmUpdate

   } elseif { $mode == 1 } {
      SetHelp $GWM_CAN ".  Click on a vertex and drag to move the vertex.\n.  Click on a polygon edge to insert a new vertex.\n.  Click anywhere else to start a new polygonal mask.\n.  Click anywhere else and drag to select an area." POLMAP_MODE_1
      SetInfo "Edit or create a polygon..." 1
      GwmUpdate

   } elseif { $mode == 2 } {
      SetHelp $GWM_CAN ".  Click on the first vertex to close the polygon.\n.  Click anywhere else to add another vertex to the polygon.\n.  Click and drag to select an area." POLMAP_MODE_2
      $GWM_CANCEL configure -state normal
      SetInfo "Complete the construction of a polygon..." 1

   } elseif { $mode == 3 } {

   } elseif { $mode == 4 } {
      if { $GWM_XHAIR } {
         set M4_XHAIR 1
         set GWM_XHAIR 0
         Xhair 0 0
      } {
         set M4_XHAIR 0
      }
      Push CURSOR_STACK [$GWM_CAN cget -cursor]
      $GWM_CAN config -cursor circle

      set PRE_MODE4 $old_mode
      $GWM_CANCEL configure -state normal
      SetHelp $GWM_CAN ".  Click to re-display the image centred on the pointer position." POLMAP_MODE_4
      SetInfo "Identify a new centre for the displayed image..." 1

   } {
      Message "Internal error - SetMode - mode $mode is illegal"
      exit 1
   }

# Reset the globals used in the contruction of a polygon if we are not
# now constructing a polygon (note we may go back to finish the polygon
# if we have gone into mode 4). These values are set in mode 1.
   if { $mode == 0 || $mode == 1 || $mode == 3 || ( $mode == 4 && $old_mode != 1 && $old_mode != 2 ) } {
      set GWM_V0 "" 
      set GWM_V1 ""
      set GWM_VCX0 ""
      set GWM_VCY0 ""
      set GWM_VID2 ""
   }

}

proc SetPosn {i names values args} {
#+
#  Name:
#     SetPosn
#
#  Purpose:
#     Set parameter values for a position. Any displayed canvas items
#     associated with the position are modified accordingly.
#
#  Arguments:
#     i
#        The index (zero-based) of the position for which information
#        is to be set. If this is supplied as -1, then a new position is 
#        appended to the end of the list.
#     names
#        A list of the names of the parameters to be set. These should be
#        taken from:
#           PX - The pixel X coordinate of the position.
#           PY - The pixel Y coordinate of the position.
#           CX - The canvas X coordinate of the position.
#           CY - The canvas Y coordinate of the position.
#           ID - The canvas item id of the marker for the position (-1
#                if no marker is currently drawn).
#           VID - The canvas item id of the line joining the position to
#                the position given by parameter NXT (-1 if no line is 
#                currently drawn, and null ("") if NXT is undefined).
#           NXT - The index of the position corresponding to the next 
#                vertex in a polygonal mask (-1 if there is no "next
#                vertex" (i.e. if the polygon is open) and null ("") if this 
#                position is not part of a polygon). 
#     values
#        A list of parameter values corresponding to the names in "names".
#     args
#        An optional argument holding the name of the position list
#        to use.  If this is not supplied, it defaults to the value 
#        of global variable GWM_CURRENT_LIST.
#
#  Returned Value:
#     The index of the set position. A null string is returned if the
#     supplied index is out of bounds.
#     
#  Globals:
#     GWM_CAN (Read)
#        Path to the canvas containing the GWM image display.
#     PNTCX (Read and Write)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas X coordinates. 
#     PNTCY (Read and Write)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas Y coordinates. 
#     PNTID (Read and Write)
#        A 1-d array indexed by list name. Each element is a list of 
#        canvas item identifiers associated with the positions in the 
#        list. A value of -1 indicates that no marker is currently 
#        drawn for the position.
#     PNTNXT (Read and Write)
#        A 1-d array indexed by list name. Each element is a list of 
#        integers representing indices within the lists given by PNTxxx. 
#        Each integer gives the index of the next position along the 
#        edge of a polygon. The vector starting at position index i, 
#        ends at position index given by the i'th element of
#        PNTNXT. If this value is null ("") then position i is not part of
#        a polygon. If this value is -1, then the end of the vector starting
#        position i is unspecified (in this case the polygon is not
#        closed).
#     PNTPX (Read and Write)
#        A 1-d array indexed by list name. Each element is a list of pixel 
#        X coordinates. 
#     PNTPY (Read and Write)
#        A 1-d array indexed by list name. Each element is a list of pixel 
#        Y coordinates. 
#     PNTVID (Read and Write)
#        A 1-d array indexed by list name. Each element is a list of canvas 
#        item identifiers associated with the vectors between positions in 
#        the list. A value of -1 indicates that no line is currently drawn 
#        for the position. A null string indicates that no vector is defined.
#     GWM_V0 (Read)
#        The index of the position corresponding to the first vertex in
#        an incomplete (i.e. open) polygon.
#     GWM_V1 (Read)
#        The index of the position corresponding to the last vertex in
#        an incomplete (i.e. open) polygon.
#     GWM_VCX0 (Write)
#        The canvas X coordinates at the first vertex in an incomplete 
#        (i.e. open) polygon.
#     GWM_VCY0 (Write)
#        The canvas Y coordinates at the first vertex in an incomplete 
#        (i.e. open) polygon.
#     GWM_VID2 (Write)
#        The canvas item id of the vector joining the last vertex in an
#        incomplete (i.e. open) polygon, to the pointer.
#-
   global GWM_CAN
   global PNTCX
   global PNTCY
   global PNTID
   global PNTNXT
   global PNTPX
   global PNTPY
   global PNTVID
   global GWM_V0
   global GWM_V1
   global GWM_VCX0
   global GWM_VCY0
   global GWM_VID2

# Store the list name to use, and create the list if necessary.
   set list [GetList $args]

# Get the size of the list.
   if { [info exists PNTID($list)] } {
      set size [llength $PNTID($list)]
   } {
      set size 0
   }

# If a new position is being appended to the list, append a dummy
# position containing default values now which will be filled with the 
# supplied values later.
   if { $i == -1 } {
      set ret $size
      incr size
      lappend PNTCX($list) ""
      lappend PNTCY($list) ""
      lappend PNTPX($list) ""
      lappend PNTPY($list) ""
      lappend PNTID($list) -1
      lappend PNTVID($list) ""
      lappend PNTNXT($list) ""
   } {
      set ret $i
   }

# Do nothing if the index is out of bounds.
   if { $ret > -1 && $ret < $size } {

# Initialise flags indicating what new values have been supplied.
      set cxymod 0
      set pxymod 0
      set nxtmod 0
      set idmod 0
      set vidmod 0

# Loop round each supplied value.  
      for {set j 0} {$j < [llength $names]} {incr j} {
         set val [lindex $values $j]
         set name [lindex $names $j]

# Note what has been supplied.
         if { $name == "CX" || $name == "CY" } {
            set cxymod 1
         } elseif { $name == "PX" || $name == "PY" } {
            set pxymod 1
         } elseif { $name == "NXT" } {
            set nxtmod 1
         } elseif { $name == "ID" } {
            set idmod 1
         } elseif { $name == "VID" } {
            set vidmod 1
         }         

# Set the value.
         upvar #0 PNT$name array
         if { [info exists array($list)] } {
            set array($list) [lreplace $array($list) $ret $ret $val]
         }
      }

# If the canvas coordinates have been supplied, but the pixel coordinates
# weren't, recalculate the pixel coordinates to ensure they are still valid.
      if { $cxymod && !$pxymod } {
         set cx [lindex $PNTCX($list) $ret]
         set cy [lindex $PNTCY($list) $ret]
         set pxy [GwmCanToNDF $cx $cy]
         set px [lindex $pxy 0]
         set py [lindex $pxy 1]
         set PNTPX($list) [lreplace $PNTPX($list) $ret $ret $px]
         set PNTPY($list) [lreplace $PNTPY($list) $ret $ret $py]

# If the pixel coordinates have been supplied, but the canvas coordinates
# weren't, recalculate the canvas coordinates to ensure they are still valid.
      } elseif { !$cxymod && $pxymod } {
         set px [lindex $PNTPX($list) $ret]
         set py [lindex $PNTPY($list) $ret]
         set cxy [GwmNDFToCan $px $py]
         if { $cxy == "" } { return "" } 
         set cx [lindex $cxy 0]
         set cy [lindex $cxy 1]
         set PNTCX($list) [lreplace $PNTCX($list) $ret $ret $cx]
         set PNTCY($list) [lreplace $PNTCY($list) $ret $ret $cy]
      }

# Configure the canvas items to the new canvas coordinates if they have
# changed. Also update the end posaition of any vectors which end at the
# modified position.
      if { $pxymod || $cxymod } {
         set cx [lindex $PNTCX($list) $ret]
         set cy [lindex $PNTCY($list) $ret]
         set px [lindex $PNTPX($list) $ret]
         set py [lindex $PNTPY($list) $ret]
         set id [lindex $PNTID($list) $ret]
         if { $id != -1 } {
            $GWM_CAN coords $id $cx $cy
         }

         set vid [lindex $PNTVID($list) $ret]
         if { $vid != -1 && $vid != "" } {
            set coords [$GWM_CAN coords $vid]
            set cx0 [lindex $coords 2]
            set cy0 [lindex $coords 3]
            $GWM_CAN coords $vid $cx $cy $cx0 $cy0
         }

         set j 0
         foreach nx $PNTNXT($list) {
            if { $nx == $ret } {
               set vid [lindex $PNTVID($list) $j]
               if { $vid != -1 && $vid != "" } {
                  set coords [$GWM_CAN coords $vid]
                  set cx0 [lindex $coords 0]
                  set cy0 [lindex $coords 1]
                  $GWM_CAN coords $vid $cx0 $cy0 $cx $cy 
               }
            }
            incr j
         }
       }

# If NXT was modified (the index of the next vertex in the polygon),
# configure the vector to end at the new "next" vertex.
      if { $nxtmod } {
         set vid [lindex $PNTVID($list) $ret]

         if { $vid != -1 && $vid != "" } {
            set nx [lindex $PNTNXT($list) $ret]

            if { $nx != -1 && $nx != "" } {
               set cx [lindex $PNTCX($list) $ret]
               set cy [lindex $PNTCY($list) $ret]
               set cx0 [lindex $PNTCX($list) $nx]
               set cy0 [lindex $PNTCY($list) $nx]
               $GWM_CAN coords $vid $cx $cy $cx0 $cy0
            } {
               $GWM_CAN delete $vid
               set PNTVID($list) [lreplace $PNTVID($list) $ret $ret $nx]
            }
         }
      }

# If the first vertex in a polygon have been changed, store the new values. 
      if { $ret == $GWM_V0 } {
         if { $pxymod || $cxymod } {
            set GWM_VCX0 $cx
            set GWM_VCY0 $cy
         }
      }

# If the last vertex in a polygon have been changed, store the new values. 
      if { $ret == $GWM_V1 } {
         if { $vidmod } {
            set GWM_VID2 [lindex $PNTVID($list) $ret]
         }
      }

# Update the bindings to be activated when the pointer enters or 
# leaves the marker.
      MarkBind $ret $list

# If the index is out of bounds, return a null value.
   } { 
      set ret ""
   }

# Return the index of the modified or new position.
   return $ret

}

proc ShowHelp {label} {
#+
#  Name:
#     ShowHelp
#
#  Purpose:
#     Create a hypertext browser (if none are already available), and
#     display the specified section of the PolMap documentation.
#
#  Arguments:
#     label 
#        An htx cross-reference label into the PolMap document. If this
#        is "pointer", then the pointer is used to identify the object about
#        which help is required. The cursor becomes a question mark and 
#        the window under the pointer when it is next clicked is the one
#        about which help is displayed. If a null string is supplied,
#        then the procedure issues a warning message that no help is
#        available.
#
#  Globals:
#     SAFE (Read)
#        The path to a window which can receive notification of all events
#        while we are waiting. This should be a window which ignores all 
#        events (except those set up here).
#     HELP_LABEL (Read and Write)
#        The htx label identified using the pointer.
#     POLPACK_HELP (Read)
#        The path to the directory containing POLPACK hypertext documents.
#
#-
   global SAFE
   global HELP_LABEL
   global POLPACK_HELP

# If the pointer is to be used to generate the label...
   if { $label == "pointer" } {

# Arrange for all X events to be delivered to the SAFE window.
      catch "grab set $SAFE"

# Save the old cursor and switch on a "question mark" cursor.
      set old_cursor [. cget -cursor]
      . config -cursor question_arrow

# Set up a binding so that when any button is clicked, the htx label
# associated with the widget under the pointer is returned. If the 
# lowest level widget doesn't have a label, work up through the family
# tree until an ancestor is found which does have a help label.
      bind $SAFE <Button> {set HELP_LABEL [FindHelp %X %Y]}

# Wait for a button to be pressed, then copy the selected htx label to
# "label".
      tkwait variable HELP_LABEL
      set label $HELP_LABEL

# Reinstate the old cursor.
      . config -cursor $old_cursor

# Delete the binding set up above.
      bind $SAFE <Button> ""

# Release the grab.
      grab release $SAFE 
   }

# If no label is available, report an error.
   if { $label == "" } {
      Message "Sorry - no help available on the selected object."

# Otherwise, translate the special label POLMAP_CONTENTS into a null
# string so that it goes to the top of the PolMap help document.
   } {
      if { $label == "POLMAP_CONTENTS" } {      
         set label ""
      }

# Save the old cursor and switch on a "watch" cursor.
      set old_cursor [. cget -cursor]
      . config -cursor watch

# Run showme to get the full url of a local file to be displayed. Warn 
# the user if an error occurs. 
      if { [catch {exec showme -l -n $POLPACK_HELP/polmap $label} url] } {
         . config -cursor $old_cursor
         Message "Showme failed to find the help documentation - $url"

# If OK, use CCDShowHelp to display the required information in a WWW
# browser. The browser to use is determined by the HTX_BROWSER
# environment variable (netscape is used if HTX_BROWSER is not defined).
      } { 
         CCDShowHelp $url
         . config -cursor $old_cursor
      }
   }
}

proc SingleBind {x y} {
#+
#  Name:
#    SingleBind
#
#  Purpose:
#    Process single clicks of button 1 over the image. 
#
#  Arguments:
#    x
#       The screen X coord.
#    y
#       The screen Y coord.
#
#  Globals:
#     GWM_CAN (Read)
#        The name of the canvas widget holding the GWM image.
#     GWM_MODE (Read)
#        The interaction mode determining how to process button clicks
#        and motion in the GWM canvas item. Modes are:
#           0 - The user specifies image features by clicking on them.
#           1 - The user starts a new mask polygon, or edits an existing one.
#           2 - The user completes a started mask polygon.
#           3 - The user selects a label by clicking on an existing feature.
#           4 - The user selects a position and the image is redisplayed
#               centred on the supplied position.
#     GWM_ROOTI (Write)
#        The position index of the vertex being pointed at, or the position
#        index of the vertex at the start of the vector being pointed at,
#        or null if neither a vertex nor a vector is being pointed at.
#     GWM_ROOTX (Write)
#        The canvas X coordinate at which the button was pressed.
#     GWM_ROOTY (Write)
#        The canvas Y coordinate at which the button was pressed.
#     GWM_VID0 ( Write)
#        The canvas item id for the vertex being pointed at (if any).
#     GWM_VID1 ( Write)
#        The canvas item id for the vector ending at the vertex being 
#        pointed at (if any).
#     GWM_VID2 ( Write)
#        The canvas item id for the vector starting at the vertex being 
#        pointed at (if any).

#-
   global GWM_CAN
   global GWM_MODE
   global GWM_POLCOL
   global GWM_ROOTI
   global GWM_ROOTX
   global GWM_ROOTY
   global GWM_VID0
   global GWM_VID1
   global GWM_VID2
   global POLPACK_DIR

# Cancel any existing selected area.
   CancelArea

# Convert the screen coords to canvas coords, and record this position as
# the "root" position which is availabel for use by other procedures.
   set GWM_ROOTX [$GWM_CAN canvasx $x]
   set GWM_ROOTY [$GWM_CAN canvasy $y]

# The global variable GWM_MODE determines how events over the canvas are 
# processed. The only case which needs any special treatment is if
# the user is editing an existing polygon (mode 1).
   if { $GWM_MODE == 1 } {

# Get the canvas id of the current item.
      set id0 [$GWM_CAN find withtag current]

# Get the list index of any position with this id.
      set GWM_ROOTI [FindPosn ID $id0]

# If a position was found with this item id, we must be pointing at a
# polygon vertex which is to be dragged. Store information about this
# vertex in global for use in B1MotionBind.
      if { $GWM_ROOTI != "" } {
         set GWM_VID0 [GetPosn $GWM_ROOTI ID]
         set GWM_VID2 [GetPosn $GWM_ROOTI VID]
         set j [FindPosn NXT $GWM_ROOTI]
         set GWM_VID1 [GetPosn $j VID]

# If we are not pointing at a vertex, try to find a vector with the
# current id.
      } {
         set GWM_ROOTI [FindPosn VID $id0]

# If one was found, insert a new vertex into the polygon at the cursor
# position.
         if { $GWM_ROOTI != "" } {
            set nxt [GetPosn $GWM_ROOTI NXT]
            set cx2 [GetPosn $GWM_ROOTI CX]
            set cy2 [GetPosn $GWM_ROOTI CY]
            set vid [$GWM_CAN create line $GWM_ROOTX $GWM_ROOTY $cx2 $cy2 -fill $GWM_POLCOL -tags vectors]
            set id [$GWM_CAN create bitmap $GWM_ROOTX $GWM_ROOTY -bitmap @$POLPACK_DIR/vertex.bit -foreground $GWM_POLCOL -tags vertices]
            set newi [SetPosn -1 "CX CY ID VID NXT" [list $GWM_ROOTX $GWM_ROOTY $id $vid $nxt]]
            SetPosn $GWM_ROOTI NXT $newi
            set GWM_VID0 $id
            set GWM_VID2 $vid
            set GWM_VID1 [GetPosn $GWM_ROOTI VID]
            set GWM_ROOTI $newi               
         }
      }
   } {
      set GWM_ROOTI ""
   }
}

proc Spacer {name h w} {
#+
#  Name:
#     Spacer
#
#  Purpose:
#     Create a null object of fixed size to use as a spacer.
#
#  Arguments:
#     name
#        The path to the widget to be created.
#     h
#        The height required (eg "4m", etc).
#     w
#        The width required (eg "4m", etc).
#
#  Returned Value:
#     The path to the spacer object.
#
#-
   set spacer [frame $name -height $h -width $w ]
   pack propagate $spacer 0
   return $spacer
}

proc StatusArea {on} {
#+
#  Name:
#     StatusArea
#
#  Purpose:
#     Display or erase the area containing status information. Only
#     the selected items of status information (specified by SI_LIST)
#     are displayed.
#
#  Arguments:
#     on
#        Should the area be displayed? If not, it is erased.
#
#  Globals:
#     F2 (Read)
#        The name of the frame widget containing the GWM canvas.
#     F3 (Read and Write)
#        The name of the frame containing the status area.
#     OPTSMENU (Read)
#        The name of the options menu.
#     SI_HELPS (Read)
#        A list containing the help text for each status item, in the
#        order they were defined.
#     SI_LABELS (Read)
#        A list containing the label for each status item, in the
#        order they were defined.
#     SI_LIST (Read)
#        A list containing integer identifiers for each status item to be
#        displayed. These integers are indices into the SI_LABELS, SI_HELPS
#        and SI_VARS lists.
#     SI_VARS (Read)
#        A list containing the global variable name for each status item, 
#        in the order they were defined.
#     SI_WIDTHS (Read)
#        A list of widths for the status items, in the order they were
#        defined.
#     S_BFONT (Read)
#        The font to use for the variable values.
#     S_FONT (Read)
#        The font to use for the labels.
#
#  Notes:
#    - The status items are drawn in the order they appear in the SI_LIST
#    list. The first item is drawn at the top of the left column. The
#    next one is drawn at the top of the right column. Subsequent items
#    are drawn alternately in the left and right columns under the ones
#    previously drawn.
#-
   global F2
   global F3
   global OPTSMENU
   global SI_HELPS
   global SI_LABELS 
   global SI_LIST
   global SI_VARS
   global SI_WIDTHS
   global S_BFONT
   global S_FONT
   global TOP
   global IMAGE

# Display the status area?
   if { $on } {

# Do nothing if it is already displayed.
      if { $F3 == "" } {

# Enable the Status Items entry in the Options menu.
         $OPTSMENU entryconfigure "Status Items..." -state normal

# Create the frame to enclose the text. Pack it just after the frame
# containing the GWM image and controls.
         set F3 [frame $TOP.status -relief groove -bd 2]
         pack $F3 -fill x -after $F2

# Divide the status area up into two columns.
         set col(0) [frame $F3.scol1]
         set col(1) [frame $F3.scol2]
         pack $col(0) $col(1) -side left -anchor n -padx 2m -pady 2m -expand 1

# We first find the required width for each of the two columns...
# Initialise the widths to zero.
         set maxwid(0) 0
         set maxwid(1) 0

# Put the first item in the left column. Subsequent items swap between
# right and left columns.
         set icol 0

# Loop round each item identifier in the SI_LIST list.
         for {set i 0} {$i < [llength $SI_LIST]} {incr i} {
            set id [lindex $SI_LIST $i]

# Get the label and the expected value width for the identified status item.
            set label [lindex $SI_LABELS $id]
            set expwid [lindex $SI_WIDTHS $id]

# The total width of the item is the sum of the two.        
            set totwid [expr [string length $label] + $expwid ]

# Update the longest item in this column.
            if { $totwid > $maxwid($icol) } {
               set maxwid($icol) $totwid
            }

# Prepare to use the other column for the next item.
            if { $icol } {
               set icol 0
            } {
               set icol 1
            }            
         }

# Now we have the column widths, create the status items...
# Put the first item in the left column. Subsequent items swap between
# right and left columns.
         set icol 0

# Loop round each item identifier in the SI_LIST list.
         for {set i 0} {$i < [llength $SI_LIST]} {incr i} {
            set id [lindex $SI_LIST $i]

# Get the label and help strings, and the variable name, and expected
# value width for the identified status item.
            set label [lindex $SI_LABELS $id]
            set help [lindex $SI_HELPS $id]
            set var [lindex $SI_VARS $id]

# Produce a lower case version of the variable name.
            set lvar [string tolower $var]

# Store the name of the column's frame in which this item is to be put.
            set c $col($icol)

# Create the status item. Each item has its own frame which contains two 
# label widgets. The left hand label contains the fixed label text, the
# right hand label contains the variable text. The length of the second
# label is set so that the total length of the two labels is always
# equal to the $maxwid value.
            set fr [frame $c.$lvar]
            pack $fr -side top -anchor nw -expand 1

            set len [string length $label]
            set remaining [expr $maxwid($icol) - $len]
            set frl1 [label $fr.l1 -text $label -font $S_FONT -anchor w]
            set frl2 [label $fr.l2 -textvariable $var -anchor w \
                                   -font $S_BFONT -width $remaining]
            pack $frl1 -side left 
            pack $frl2 -side left -expand 1 -fill x

# Set the help text to display when the pointer is over the item's frame.
            SetHelp $fr $help POLMAP_STATUS_AREA

# Prepare to use the other column for the next item.
            if { $icol } {
               set icol 0
            } {
               set icol 1
            }            
         }
      }

# If required, destroy the status frame (if it has not already been destroyed)
# and disable the Status Items entry in the Options menu.
   } {
      if { $F3 != "" } {
         destroy $F3
         set F3 ""
         $OPTSMENU entryconfigure "Status Items..." -state disabled
      }
   }
}

proc StatusItem {var label help width} {
#+
#  Name:
#     StatusItem
#
#  Purpose:
#     Define an item of status information which can be displayed in the
#     status area.
#
#  Arguments:
#     var
#        The name of the global variable to be displayed in the status
#        item.
#     label
#        The constant text to display to the left of the variable value.
#     help
#        The text to display in the help area when the pointer is over the
#        status item.
#     width
#        The number of characters in the longest expected value of the item.
#
#  Globals:
#     SI_LABELS (Write)
#        A list of labels for the status items, in the order they were
#        defined.
#     SI_HELPS (Write)
#        A list of help strings for the status items, in the order they were
#        defined.
#     SI_VARS (Write)
#        A list of variable names for the status items, in the order they were
#        defined.
#     SI_WIDTHS (Write)
#        A list of widths for the status items, in the order they were
#        defined.
#-
   global SI_LABELS
   global SI_HELPS
   global SI_VARS
   global SI_WIDTHS

   lappend SI_LABELS "$label"
   lappend SI_HELPS "$help"
   lappend SI_VARS "$var"
   lappend SI_WIDTHS $width
}

proc StringValue {name width value command args} {
#+
#  Name:
#    StringValue
#
#  Purpose:
#    Create a simple text entry "widget". 
#
#  Arguments:
#    name
#      The name of the "entry" to create (eg ".wm.maxval")
#    width
#      The number of characters in the text entry widget.
#    value
#      The name (NOT the value) of the global variable to receive the 
#      numerical value. Note, this must be a *global* variable.
#    command
#      A command to execute after a valid value has been assigned to the
#      variable.
#    args 
#      Any further options to pass to the command which creates the
#      "entry" widget (optional).
#
#  Returned Value:
#    The name of the entry widget.
#
#  Globals:
#    OLD_VAL (Write)
#      The previous (valid) value displayed in the text entry widget.
#-

   global $value
   upvar #0 $value varr

# Create the text entry widget. The text in this widget mirrors the value in
# the supplied global variable.
   eval entry $name -width $width -relief sunken -bd 2 -textvariable $value \
          -justify left $args

# When the pointer enters the text entry area, select the entire current
# contents of the widget so that typing a single character will delete it.
# Also take the focus.
   bind $name <Enter> \
      "if { \[$name cget -state\] == \"normal\" } {
          $name select from 0
          $name select to end

          focus $name

       }"

# When the pointer leaves the text entry area, clear the current selection,
# pass the focus back to the window which had it before, and perform any
# supplied command.
   bind $name <Leave> \
      "if { \[$name cget -state\] == \"normal\" } {
          $name select clear
          eval \"$command\"
       }"

# Also do the command if RETURN is pressed.
   bind $name <Return> \
      "if { \[$name cget -state\] == \"normal\" } {
          eval \"$command\"
       }"

# Return the name of hte created entry widget.
   return $name
}

proc Top {stack} {
#+
#  Name:
#    Top
#
#  Purpose:
#    Returns the top value from the supplied stack without removing it
#    from the stack.
#
#  Arguments:
#    stack
#       The name (NOT the value) of a global list variable holding the stack. 
#       The list is unchanged on exit.
#
#  Returned Value:
#    The top value on the stack.
#-
    upvar #0 $stack stk
    return [lindex $stk 0]
}

proc UniqueFile {} {
#+
#  Name:
#     UniqueFile
#
#  Purpose:
#     Returns a unique file name for which no file currently exists.
#     These files are created in the temporary ADAM_USER directory
#     created by PolMap, and so do not need to be deleted when finished
#     with as they will all be deleted when the temporary ADAM_USER
#     directory is deleted when PolMap exits.
#
#  Arguments:
#     None.
#
#  Returned Value:
#     The file name.
#
#  Globals:
#     ADAM_USER (Read)
#        The path to the temporary ADAM_USER directory used by PolMap.
#     IFILE (Read and Write)
#        File names have a name of the form PolMap<i> where <i> is an 
#        integer, which is different for each file. IFILE
#        records the value of i used in the previous call to this
#        function. The first value of i considered is one greater than
#        that used last time.
#
#-
   global ADAM_USER
   global IFILE

   incr IFILE
   set file "$ADAM_USER/PolMap$IFILE"

   while { [llength [glob -nocomplain ${file}.*] ] != 0 } {
      incr IFILE
      set file "$ADAM_USER/PolMap$IFILE"
   }

   return $file
}

proc UnZoom1 {} {
#+
#  Name:
#     UnZoom1
#
#  Purpose:
#     Update the display to show the previous section, in response to a
#     single click on the Unzoom button. Adjacent sections which are
#     identical are skipped over.
#
#  Arguments:
#     None.
#
#  Globals:
#     GWM_SECTION (Write)
#        The section to be displayed next.
#     GWM_SECTION_STACK (Read and Write)
#         A stack of the previously displayed sections. This is stored as
#         a list with the oldest section is at the end of the list.
#     GWM_UNZOOM (Read)
#         The path to the Unzoom button.
#-
   global GWM_SECTION
   global GWM_SECTION_STACK
   global GWM_UNZOOM

# Only proceed if this is not part of a double click sequence.
   if { ![DoubleClick GWM_UNZOOM_CLICK] } {

# If the section stack is not empty...
      while { [llength $GWM_SECTION_STACK] > 0 } { 

# Pop the top section off the section stack.
         set GWM_SECTION [Pop GWM_SECTION_STACK]

# If this section is not the same as the one below it, leave the loop.
         if { $GWM_SECTION != [Top GWM_SECTION_STACK] } { break }

      }

# Disable the unzoom button when the stack is emptied.
      if {  [llength $GWM_SECTION_STACK] == 0 } {
         $GWM_UNZOOM configure -state disabled
      }

# Update the display.
      GwmUpdate
   }

}

proc UnZoom2 {} {
#+
#  Name:
#     UnZoom2
#
#  Purpose:
#     Update the display to show the original section, in response to a
#     double click on the Unzoom button.
#
#  Arguments:
#     None.
#
#  Globals:
#     GWM_SECTION (Write)
#        The section to be displayed next.
#     GWM_SECTION_STACK (Read and Write)
#         A stack of the previously displayed sections. This is stored as
#         a list with the oldest section is at the end of the list.
#     GWM_UNZOOM (Read)
#         The path to the Unzoom button.
#-
   global GWM_SECTION
   global GWM_SECTION_STACK
   global GWM_UNZOOM

# Indicate that this is part of a double click sequence.
   DoubleClick GWM_UNZOOM_CLICK 1

# If there is anything on the stack, get the bottom section off 
# the section stack.
   if { [llength $GWM_SECTION_STACK] > 0 } {
      set GWM_SECTION [Pop GWM_SECTION_STACK -1]
   }

# Ensure the section stack is empty, and disable the Unzoom button.
   set GWM_SECTION_STACK ""
   $GWM_UNZOOM configure -state disabled

# Update the display.
   GwmUpdate

}

proc Value {name label width value max min checker} {
#+
#  Name:
#    Value
#
#  Purpose:
#    Create a numerical value entry "widget", with arrows for incrementing
#    and decrementing the value.
#
#  Arguments:
#    name
#      The name of the "widget" to create (eg ".wm.maxval")
#    label
#      A textual label for the new "widget".
#    width
#      The number of characters in the text entry widget.
#    value
#      The name (NOT the value) of the global variable to receive the 
#      numerical value. Note, this must be a *global* variable.
#    max
#      The maximum allowed numerical value.
#    min
#      The minimum allowed numerical value.
#    checker
#      The name of a procedure which can be called to check a supplied
#      numerical value. See "CheckVal" as an example. 
#
#  Returned Value:
#    The name of the frame containing the other widgets.
#
#  Globals:
#    B_FONT (Read)
#      The font to use for the labels.
#    DECS (Write)
#      The number of times the value has been decremented since the
#      decrement button was pressed.
#    INCS (Write)
#      The number of times the value has been incremented since the
#      increment button was pressed.
#    OLD_VAL (Write)
#      The previous (valid) value displayed in the text entry widget.
#    GWM_REDISPLAY_CANCELLED (Read and Write)
#      Was a previous redisplay of the image cancelled because the
#      user looked like he may be about to enter a new scaling value?
#    GWM_REDISPLAY_REQUESTED (Read and Write)
#      Was a redisplay of the image requested?
#    SEQ_STOP (Write)
#      The identifier of the sequence of timed commands which is to be 
#      terminated.
#-

   global $value
   global B_FONT
   global SEQ_STOP
   global DECS
   global INCS
   global POLPACK_DIR

# Create a frame to contain the whole thing.
   frame $name -relief flat

# Create the label.
   label $name.label -text $label -font $B_FONT

# Create the text entry widget. The text in this widget mirrors the value in
# the supplied global variable.
   entry $name.ent -width $width -relief sunken -bd 2 -textvariable $value \
          -justify center

# When the pointer enters the text entry area, select the entire current
# contents of the widget so that typing a single character will delete it.
# Also take the focus, and save the current numerical value so that it
# can be re-instated if the user enters a duff value
   bind $name.ent <Enter> \
      "if { \$GWM_REDISPLAY_REQUESTED } {
          set GWM_REDISPLAY_REQUESTED 0
          set GWM_REDISPLAY_CANCELLED 1
       } 
       $name.ent select from 0
       $name.ent select to end

       focus $name.ent

       set OLD_VAL \$$value"

# When the pointer leaves the text entry area, clear the current selection,
# pass the focus back to the main window, and check that the
# current text represents a valid value (if not, the old value will be
# re-instated).
   bind $name.ent <Leave> \
      "$name.ent select clear
       focus .
       $checker $value 1 $max $min"

# Also check the text value if RETURN is pressed.
   bind $name.ent <Return> "$checker $value 0 $max $min"

# Create the decrement button for the left end of the text entry widget.
# This is a button displaying an arrow bitmap.
   button $name.left -bitmap @$POLPACK_DIR/left_arrow.bit -command {}

# Set the active colours to the normal colours so that the button doesn't
# change colour when the pointer is over it (it will still change relief
# when it is pressed though).
   $name.left configure -activebackground [lindex [$name.left configure -background] end]
   $name.left configure -activeforeground [lindex [$name.left configure -foreground] end]

# When mouse button 1 is pressed, reset the number of decrements
# performed so far to zero, and call "Seq" to initiate a sequence of 
# calls to "DecVal", separated by 100 milliseconds.
   bind $name.left <ButtonPress-1> \
      "global GWM_REDISPLAY_REQUESTED
       global GWM_REDISPLAY_CANCELLED
       if { \$GWM_REDISPLAY_REQUESTED } {
          set GWM_REDISPLAY_REQUESTED 0
          set GWM_REDISPLAY_CANCELLED 1
       } 
       set DECS 0
       set OLD_VAL \$$value
       Seq \"DecVal $value $max $min\" 100 $name-2 DECS"

# When mouse button 1 is released, set the value of the global SEQ_STOP
# variable to the ID associated with the DecVal procedure (i.e. $name-2).
# This causes the sequence of timed calls to DecVal to terminate. 
   bind $name.left <ButtonRelease-1> \
      "set SEQ_STOP $name-2
       $checker $value 1 $max $min"

# Do the same for the increment button.
   button $name.right -bitmap @$POLPACK_DIR/right_arrow.bit -command {}
   $name.right configure -activebackground [lindex [$name.right configure -background] end]
   $name.right configure -activeforeground [lindex [$name.right configure -foreground] end]
   bind $name.right <ButtonPress-1> \
      "global GWM_REDISPLAY_REQUESTED
       global GWM_REDISPLAY_CANCELLED
       if { \$GWM_REDISPLAY_REQUESTED } {
          set GWM_REDISPLAY_REQUESTED 0
          set GWM_REDISPLAY_CANCELLED 1
       } 
       set OLD_VAL \$$value
       set INCS 0
       Seq \"IncVal $value $max $min\" 100 $name-1 INCS"

   bind $name.right <ButtonRelease-1> \
      "set SEQ_STOP $name-1
       $checker $value 1 $max $min"

# Pack the separate items into the frame.
   pack $name.label
   pack $name.left $name.ent $name.right -side left -expand 1

   return $name
}

proc WaitFor {name args} {
#+
#  Name:
#     WaitFor
#
#  Purpose:
#     Pause the caller until a named global variable changes its value.
#     Meanwhile, events are directed to a nominated "safe" window. This
#     "freezes" the display so that further actions cannot be initiated by 
#     the user
#
#  Arguments:
#     name
#        The name (NOT the value) of the global variable to be watched.
#     args
#        An optional list argument. If supplied, the first element should
#        be a command and the second element should be a time in milliseconds. 
#        The supplied command will be executed after each period of the 
#        specified time, until the variable is changed. If the delay time
#        is not supplied it defaults to 100 milliseconds. If the suppleid
#        command returns a zero value, then the loop is aborted prematurely.
#
#  Returned Value:
#     Zero if a supplied command returned a zero value or the
#     CANCEL_OP variable was set to a non-zero value (in which
#     case the delay is aborted prematurely), and one otherwise.
#
#  Globals:
#     SAFE (Read)
#        The path to a window which can receive notifivcation of all events
#        while we are waiting. This should be a window which ignores all 
#        events.
#
#  Notes:
#    - This procedure should be used in place of tkwait, which should NOT
#    be used.
#-
   global GWM_CAN
   global SAFE
   global CANCEL_OP

# Access the supplied variable using the local name "VAR".
   upvar #0 $name VAR

# Save the original value of the global variable being watched.
   set orig $VAR

# Save the old cursors and switch on a "clock" cursor.
   set old_cursor [. cget -cursor]
   . config -cursor watch

   if { [info exists CAN] } {
      set old_cancur [$GWM_CAN cget -cursor]
      $GWM_CAN config -cursor watch
   }

# Indicate that no gran has yet been made by this procedure.
   set grabset 0

# See if any command has been supplied.
   set nargs [llength $args]
   if { $nargs > 0 } {
      set com [lindex $args 0]
      if { $nargs > 1 } {
         set delay [lindex $args 1]
      } {
         set delay 100
      }
   } {
      set com ""
      set delay 100
   }

# Wait until the variable changes value, or the operation is cancelled ...
   set ret 1
   while { $VAR == $orig } {

# Attempt to set a grab on a "safe" window so that all button
# presses and mouse movements will be ignored. If succesful, note
# that we will need to release the grab.
      if { !$grabset } {
         if { ![catch "grab set $SAFE"] } {
            set grabset 1
         }
      }

# Execute any supplied command.
      if { $com != "" } {
         set ret [eval "$com"]
         if { !$ret } { break }
      }

# Break out of the loop if CANCEL_OP was set to a non-zero value.
      if { $CANCEL_OP } { break }

# Pause and then repeat.
      after $delay {set a 1}
      tkwait variable a

   }

# Release the grab set above (if any).
   if { $grabset } {
      grab release $SAFE
   }

# Revert to the previous cursors.
   . config -cursor $old_cursor

   if { [info exists CAN] } {
      $GWM_CAN config -cursor $old_cancur
   }

# Return zero if the operation has been cancelled.
   if { $CANCEL_OP } { set ret 0 }

   return $ret
}

proc Wop {w args} {
#+
#  Name:
#     Wop
#
#  Purpose:
#     Execute a widget command, gaurding against the possibility of the 
#     widget no longer existing.
#
#  Arguments:
#     w
#        The name of the widget.
#     args
#        Further arguments to pass to the widget command.
#
#  Returned Value:
#     The returned value from the widget command, or a null string if
#     the widget no longer exists.
#-
   if { [winfo exists $w] } {
      set ret [eval $w $args]
   } { 
      set ret ""
   }
   return $ret
}

proc Xhair {cx cy} {
#+
#  Name:
#     Xhair
#
#  Purpose:
#     Set the current cross-hair position, creating the cross-hair if it
#     is required but does not currently exist, or deleting it if it
#     exists but is not required.
#
#  Arguments:
#     cx
#        The canvas x coordinate of the pointer. Ignored if the
#        cross-hair is not in use.
#     cy
#        The canvas y coordinate of the pointer. Ignored if the
#        cross-hair is not in use.
#
#  Globals:
#     GWM_CAN (Read)
#        The name of the canvas widget holding the GWM image.
#     POLPACK_DIR (Read)
#        The path to the directory containing the POLPACK bitmaps.
#     GWM_SIZE (Read)
#        The size of the square canvas in screen pixels.
#     GWM_XHAIR (Read)
#        If non-zero then a cross-hair is required. If zero then no
#        cross-hair is required.
#     GWM_XHAIR_IDV (Read and Write)
#        The canvas item identifier for the vertical line forming the
#        cross-hair. Set null if no line exists.
#     GWM_XHAIR_IDH (Read and Write)
#        The canvas item identifier for the horizontal line forming the
#        cross-hair. Set null if no line exists.
#     GWM_XHRCOL (Read)
#        The colour for the cross-hair.
#-
   global GWM_CAN
   global POLPACK_DIR
   global GWM_SIZE
   global GWM_XHAIR
   global GWM_XHAIR_IDV
   global GWM_XHAIR_IDH 
   global GWM_XHRCOL

# If we require a cross-hair...
   if { $GWM_XHAIR } {

# but no cross hair canvas lines currently exist...
      if { $GWM_XHAIR_IDH == "" } {

# Save the current cursor on a stack, and set a null bit map as the
# current cursor (this causes no cursor to be visible).
         Push CURSOR_STACK [$GWM_CAN cget -cursor]
         $GWM_CAN configure -cursor [list @$POLPACK_DIR/blank.bit white]

# Create the two canvas lines forming the cross hair and set their coordinates.
         set GWM_XHAIR_IDH [$GWM_CAN create line 0 $cy $GWM_SIZE $cy -fill $GWM_XHRCOL]
         set GWM_XHAIR_IDV [$GWM_CAN create line $cx 0 $cx $GWM_SIZE -fill $GWM_XHRCOL]

# If the cross-hair already exists, set their coordinates.
      } {
         $GWM_CAN coords $GWM_XHAIR_IDH 0 $cy $GWM_SIZE $cy
         $GWM_CAN coords $GWM_XHAIR_IDV $cx 0 $cx $GWM_SIZE
      }

# If no cross-hair is required...
   } {

# but the cross-hair currently exists...
      if { $GWM_XHAIR_IDH != "" } {

# Re-instate the cursor from the top of the cursor stack.
         $GWM_CAN configure -cursor [Pop CURSOR_STACK]

# Delete the cross hair canvas items.
         $GWM_CAN delete $GWM_XHAIR_IDH
         $GWM_CAN delete $GWM_XHAIR_IDV

# Set the global variables to indicate that we currently do not have a
# cross hair.
         set GWM_XHAIR_IDH ""
         set GWM_XHAIR_IDV ""
      }
   }
}

proc Zoom {} {
#+
#  Name:
#     Zoom
#
#  Purpose:
#     Update the display to show just the selected area.
#
#  Arguments:
#     None.
#
#  Globals:
#     GWM_SEL_AREA (Read)
#        The bounds of the selected area in the order xmin, xmax, ymin,
#        ymax. 
#
#-
   global GWM_SEL_AREA
   global GWM_SECTION_STACK
   global GWM_SECTION
   global SECTION_REQ
   global GWM_UNZOOM

# Do nothing if there is no selected area.
   if { $GWM_SEL_AREA != "" } {

# Get the bounds in canvas coordinates of the selected area.
      set cxlo [lindex $GWM_SEL_AREA 0]      
      set cylo [lindex $GWM_SEL_AREA 1]      
      set cxhi [lindex $GWM_SEL_AREA 2]      
      set cyhi [lindex $GWM_SEL_AREA 3]      

# Convert these to pixel coordinates. Note, the Y axis is reversed since the
# TK origin is at the UPPER left corner.
      set pxyl [GwmCanToNDF $cxlo $cyhi]
      set pxlo [lindex $pxyl 0]
      set pylo [lindex $pxyl 1]
      set pxyl [GwmCanToNDF $cxhi $cylo]
      set pxhi [lindex $pxyl 0]
      set pyhi [lindex $pxyl 1]

# Convert these to pixel indices.
      set ipxlo [expr round($pxlo) + 1 ]
      set ipylo [expr round($pylo) + 1 ]
      set ipxhi [expr round($pxhi) ]
      set ipyhi [expr round($pyhi) ]

# Only accept the Zoom request if the selected area is larger than 4
# pixels on each edge.
      set dx [expr $ipxhi - $ipxlo]
      set dy [expr $ipyhi - $ipylo]
      if { $dx > 4 && $dy > 4 } {

# Save the currently displayed section on the section stack so that it can be
# restored later using the Unzoom button, and enable the Unzoom button.
         Push GWM_SECTION_STACK $GWM_SECTION
         $GWM_UNZOOM configure -state normal

# Display the modified section. 
         set GWM_SECTION "($ipxlo:$ipxhi,$ipylo:$ipyhi)"
         GwmUpdate
       } {
         Message "Selected area is too small to display."
         CancelArea
      }
   }
}


#-------------------------------------------------------------------
#  The following procedures are for debugging purposes.

proc StackDump {} {
   for {set i 1} { $i < [info level] } { incr i} {
      puts [info level $i]
   }
   puts " "
}

proc Watch {name} {
   global $name
   trace variable $name w pp
}

proc pp {a b c} {
   upvar #0 $a x
   puts "\n------------------------"
   puts "Variable $a set to $x"
   StackDump
   puts "++++++++++++++++++++++++\n"
}


#+
#  Name:
#     
#
#  Purpose:
#     
#
#  Arguments:
#     
#
#  Returned Value:
#     
#
#  Globals:
#     
#
#  Notes:
#    - 
#-





proc VectorMap {} {
#+
#  Name:
#    VectorMap
#
#  Purpose:
#    Display a vector map, and store global values needed by GwmNDFToCan 
#    and GwmCanToNDF (which convert between canvas coordinates and NDF 
#    pixel coordinates).
#
#  Arguments:
#    None.
#
#  Globals:
#    GWM_CX (Write)
#       The X offset for converting from canvas coordinates to NDF 
#       pixel coordinates.
#    GWM_CY (Write)
#       The Y offset for converting from canvas coordinates to NDF 
#       pixel coordinates.
#    GWM_DEVICE (Read)
#       The GNS device name for the GWM canvas item.
#    GWM_DISPLAY_VDATA (Read and Write)
#       The vector data currently displayed.
#    GWM_MX (Write)
#       The X scale factor for converting from canvas coordinates to 
#       NDF pixel coordinates.
#    GWM_MY (Write)
#       The Y scale factor for converting from canvas coordinates to NDF 
#       pixel coordinates.
#    GWM_SECTION (Read)
#       The requested vector map section (eg "(10:200,23:68)" ).
#    GWM_SIZE (Read)
#       The size of the square GWM canvas item (in screen pixels).
#-
   global GWM_BACK
   global GWM_CX
   global GWM_CY
   global GWM_DEVICE
   global GWM_MX
   global GWM_MY
   global GWM_SECTION
   global GWM_SIZE
   global GWM_DISPLAY_VDATA
   global VEC_CAT

# Tell the user what is happening.
   set told [SetInfo "Displaying the vector map. Please wait... " 0]

# Initialise the parameter string for POLPLOT...
   set pars "cat=$VEC_CAT colmag=p colang=theta colx=x coly=y clear=no noaxes nokey"

#  If there is a background image the vectors are displayed in alignment
#  with the backgrounmd image. In this case the background image defines
#  the area of the vector map which is to be displayed. If no background
#  image is being displayed, then we need to determine explicitly the area 
#  of the vector map which is to be displayed.
   if { $GWM_BACK != "CONTOUR" && $GWM_BACK != "GREY" } {
      set bnds [SecList $GWM_SECTION]
      append pars " lbnd=\[[lindex $bnds 0],[lindex $bnds 2]\]"
      append pars " ubnd=\[[lindex $bnds 1],[lindex $bnds 3]\]"
   }

# Display the vector map
   if { [Obey polpack polplot "$pars device=$GWM_DEVICE" ] } {

# Indicate that the image has been displayed.
      set GWM_DISPLAY_VDATA $VEC_CAT

# Use datapic to get the bounds of the DATA picture just created in 
# normalised device coordinates and NDF pixels. These NDC values extend
# from 0 to 1 on both axes.
      Obey polpack datapic "device=$GWM_DEVICE" 1
      regsub -nocase D [GetParam polpack datapic:result] E result
      scan $result "' %f %f %f %f %f %f %f %f '" ncx1 ncx2 ncy1 ncy2 \
                                                 wcx1 wcx2 wcy1 wcy2
      
# Calculate the offsets and scaling factors for converting from canvas
# coordinates to NDF pixels.
      set cx1 [expr $ncx1 * ( $GWM_SIZE - 1 )]
      set cx2 [expr $ncx2 * ( $GWM_SIZE - 1 )]
      set cy1 [expr $GWM_SIZE * ( 1.0 - $ncy1 )]
      set cy2 [expr $GWM_SIZE * ( 1.0 - $ncy2 )]
   
      set GWM_MX [expr ( $wcx2 - $wcx1 ) / ( $cx2 - $cx1 ) ]
      set GWM_CX [expr $wcx1 - $GWM_MX * $cx1]
      set GWM_MY [expr ( $wcy2 - $wcy1 ) / ( $cy2 - $cy1 ) ]
      set GWM_CY [expr $wcy1 - $GWM_MY * $cy1]
   }

# Cancel the informative text set earlier in this procedure.
   if { $told } { SetInfo "" 0 }

}

proc MakeCat {} {
#+
#  Name:
#     MakeCat
#
#  Purpose:
#     Create a catalogue of polarisation vectors from a Stokes vectors cube.
#
#  Globals:
#     CUBE (Read)
#        The name of the input Stokes vector cube supplied by the A-task.
#     VEC_CAT (Read and Write)
#        The name of the catalogue holding polarisation vectors. This is
#        created in the temporary ADAM_USER directory.
#-
   global CUBE
   global VEC_CAT

# Tell the user what is happening.
   set told [SetInfo "Calculating polarisation vectors. Please wait... " 0]

# The catalogue is created as a temporary file within the POLMAP
# ADAM_USER directory. Get a new name.
   set cat [UniqueFile]

# Create the parameter string for POLVEC...
   set pars "in=$CUBE cat=$cat accept"

# Create the vector catalogue. If succesful, replace any old vector
# catalogue with the new one.
   if { [Obey polpack polvec "$pars"] } {
      if { [info exists VEC_CAT] } {
         catch "exec rm -f ${VEC_CAT}.*"
      }
      set VEC_CAT $cat
   }

# Cancel the informative text set earlier in this procedure.
   if { $told } { SetInfo "" 0 }
}
