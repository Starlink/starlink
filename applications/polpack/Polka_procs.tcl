#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Name:
#     Polka_procs.tcl
#
#  Purpose:
#     Defines Tcl procedures needed by POLKA.

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     DSB: David S. Berry (STARLINK)
#
#  History:
#     4-JUN-1998 (DSB):
#        Original version
#     22-JUN-1998 (DSB):
#        Modified Save to avoid long integer image identifiers (i.e. IMGID
#        values) producing "integer value too large to represent" message.
#        Added RestoreMask and DumpMask procedures.
#     3-NOV-1998 (DSB):
#        LoadTask now writes error messages to the terminal screen as well
#        as a Tk window. DrawGwm modified to use KAPPA V0.13 version of
#        DISPLAY. Procedure TranImage modified to propgate WCS from input
#        to output. POLPACK:SEGMENT replaced by KAPPA:SEGMENT.
#     12-NOV-1998 (DSB):
#        Default sky areas are now not used if they do not fall within the
#        image (see CheckSky).
#     24-FEB-1999 (DSB):
#        Modified to allow creation of Stokes vectors in single-beam mode.
#     22-APR-1999 (DSB):
#        Proc OEMapping changed to avoid expensive search for a default
#        OEMapping if supplied image has no OEMap. A Default OEMapping is
#        now stored in global DEF_OEMAP.
#     29-APR-1999 (DSB):
#        Added proc SendBack and modified proc exit to call SendBack.
#     15-SEP-1999 (DSB):
#        Modified CreateMask to look for the same mask in other images,
#        before looking for the other mask in the specified image. See
#        TMG mail 2/9/99, 3/99/99 (saved in PINE on 15/9/99).
#     28-FEB-2000 (DSB):
#        Modified call to KAPPA:SEGMENT to do away with COSYS parameter.
#     19-APR-2000 (DSB):
#        Added USEWCS=NO to CCDPACK:TRANNDF parameter list.
#     14-APR-2003 (DSB):
#        Use KAPPA:REGRID instead of CCDPACK:TRANNDF, and KAPPA:WCSADD
#        instead of KAPPA:TRANMAKE.
#     27-FEB-2015 (DSB):
#        Remove KAPRH dependency. This requires removing FITTYPE=5 option.
#---------------------------------------------------------------------------

proc Accept {} {
#+
#  Name:
#     Accept
#
#  Purpose:
#     Searches for current features at the canvas coordinates at which
#     the reference features are drawn.
#
#  Arguments:
#     None.
#
#  Globals:
#     REFIM_DISP (Read)
#        The image in which the reference features were defined.
#     REFOBJ_DISP
#        The type of reference features (should be $O_RAY_FEATURES or
#        $E_RAY_FEATURES).
#
#-
   global REFIM_DISP
   global REFOBJ_DISP

# Tell the user what is happening.
   set told [SetInfo "Searching for features at displayed positions. Please wait..." 0]

# Initialise the number of bad positions to zero.
   set nbad 0

# Loop round each of the displayed reference features.
   set size [NumPosn "" $REFIM_DISP $REFOBJ_DISP]
   for {set i 0} {$i < $size} {incr i} {

# Get the canvas coordinates at which this reference feature is displayed,
# together with its label.
      set cx [GetPosn $i CX $REFIM_DISP $REFOBJ_DISP]
      set cy [GetPosn $i CY $REFIM_DISP $REFOBJ_DISP]
      set lab [GetPosn $i LBL $REFIM_DISP $REFOBJ_DISP]

# See if there is already a current feature with the same label as the
# reference feature. If not, append this position to the list of
# positions to be accepted.
      if { [FindPosn LBL $lab 0] == "" } {
         lappend cxs $cx
         lappend cys $cy
         lappend labs $lab
      }
   }

# If there are any positions to be accepted, centroid each feature and
# create an entry in the relevant positions list.
   if { [info exists cxs] } {
      GetFeature $cxs $cys $labs
   }

# Cancel the informative text set earlier in this procedure.
   if { $told } { SetInfo "" 0 }

}

proc AllMappings {} {
#+
#  Name:
#     AllMappings
#
#  Purpose:
#     Attempt to make all mappings for all images up-to-date. Displays
#     an error message identifying any which cannot be made up-to-date.
#
#  Arguments:
#     None.
#
#  Returned Value:
#     1 if all Mappings are usable on exit. Zero otherwise.
#
#  Globals:
#     DBEAM (Read)
#        Is Polka being run in dual-beam mode?
#     IMAGES (Read)
#        A list of the input images (without any section specifiers).
#     IMMAP (Read)
#        An array of lists (one for each image), each giving the mapping
#        from the supplied image to the reference (first) image. Each
#        mapping is a list of 6 values representing a general linear
#        transformation.
#-
   global DBEAM
   global IMAGES
   global IMMAP

# Tell the user what is happening.
   set told [SetInfo "Determining all mappings. Please wait... " 0]

# Indicate that as yet there are no missing image mappings.
   set missing ""

# Store the name of the first (reference) image.
   set im0 [lindex $IMAGES 0]

# Loop round each image...
   foreach image $IMAGES {

# Create the mappings for this image.
      Mappings $image

# If no image mapping could be created for this image, add the image name
# onto a list of such image names. The reference image is always
# considered to be registered (with itself) but no mapping structure is
# actually created for it.
      if { ![info exists IMMAP($image)] && $image != $im0 } {
         lappend missing $image
      }
   }

# Construct an error message identifying the missing mappings.
   set mess ""

   if { $missing != "" } {
      append mess "The following images cannot yet be registered with image \"$im0\":\n"
      foreach image $missing {
         append mess "   $image\n"
      }
   }

   if { [OEMapping $im0] == "" && $DBEAM } {
      if { $missing == "" } {
         append mess "The "
      } {
         append mess "\nIn addition, the "
      }
      append mess "mapping between the E and O rays cannot yet be found."
   }

# Display the error message if required, and set the returned value.
   if { $mess != "" } {
      Message $mess
      set ok 0
   } {
      set ok 1
   }

# Cancel the informative text set earlier in this procedure.
   if { $told } { SetInfo "" 0 }

   return $ok

}

proc AutoLabel {} {
#+
#  Name:
#     AutoLabel
#
#  Purpose:
#     Automatically generate a feature label and return it.
#
#  Arguments:
#     None.
#
#  Returned Value:
#     The label.
#
#  Globals:
#     NEXT_LABEL (Read and Write)
#       The integer index fo the next automatically generated label.
#-
   global NEXT_LABEL
   incr NEXT_LABEL
   return "$NEXT_LABEL"
}

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
#     CAN (Read)
#        The name of the canvas widget holding the GWM image.
#     MODE (Read)
#        The interaction mode determining how to process button clicks
#        and motion in the GWM canvas item. Modes are:
#           0 - The user specifies image features by clicking on them.
#           1 - The user starts a new mask polygon, or edits an existing one.
#           2 - The user completes a started mask polygon.
#           3 - The user selects a label by clicking on an existing feature.
#     MOVE (Read)
#        Was the shift key pressed when the button was clicked? If so, the
#        whole polygon is dragged instead of a single vertex.
#     ROOTI (Read)
#        The position index of the vertex being pointed at, or the position
#        index of the vertex at the start of the vector being pointed at,
#        or null if neither a vertex nor a vector is being pointed at.
#     ROOTX (Read)
#        The canvas X coordinate at which the button was pressed.
#     ROOTY (Read)
#        The canvas Y coordinate at which the button was pressed.
#     SELECTED_AREA (Read and Write)
#        The bounds of the selected area in the order xmin, xmax, ymin,
#        ymax.
#     VID0 (Read)
#        The canvas item id for the vertex being pointed at (if any).
#     VID1 (Read)
#        The canvas item id for the vector ending at the vertex being
#        pointed at (if any).
#     VID2 (Read)
#        The canvas item id for the vector starting at the vertex being
#        pointed at (if any).
#-
   global CAN
   global MODE
   global MOVE
   global ROOTX
   global ROOTI
   global ROOTY
   global SELCOL
   global SELECTED_AREA
   global VID0
   global VID1
   global VID2
   global POINTER_PXY
   global ANCX
   global ANCY
   global ROOTAG

# Convert the screen coords to canvas coords.
   set cx [$CAN canvasx $x]
   set cy [$CAN canvasy $y]

# If a cross-hair has been requested instead of a pointer, then move the
# positions of the lines making up the cross hair.
   Xhair $cx $cy

# Store the pixel coordinates of the pointer in POINTER_PXY.
   set pxy [CanToNDF $cx $cy]
   set px [lindex $pxy 0]
   set py [lindex $pxy 1]
   set POINTER_PXY [format "( %.1f, %.1f )" $px $py ]

# The global variable MODE determines how events over the canvas are
# processed. Pointer motion with button 1 pressed usually results in
# an area being selected. The only exception to this is if we are in mode
# 1 ("edit an existing polygon") AND we are pointing at a polygon vertex,
# or if we are in mode 4. Do nothing in mode 4.
   if { $MODE == 4 } {

# Check for other cases.
   } elseif { $MODE != 1 || $ROOTI == "" } {

# Find the min and max values on each axis of the selected area. The
# position at which the button was pressed (ROOTx,ROOTY) gives one
# corner of the box, and the current cursor position gives the other.
      if { $cx < $ROOTX } {
         set xmin $cx
         set xmax $ROOTX
      } {
         set xmax $cx
         set xmin $ROOTX
      }
       if { $cy < $ROOTY } {
         set ymin $cy
         set ymax $ROOTY
      } {
         set ymax $cy
         set ymin $ROOTY
      }

# If there is currently no recorded selected area, create the canvas item
# (a rectangle) marking the area. Otherwise, configure the existing
# canvas item to match the current selected area.
      if { $SELECTED_AREA == "" } {
         $CAN create rectangle $xmin $ymin $xmax $ymax -outline $SELCOL -tags sbox
      } {
         $CAN coords sbox $xmin $ymin $xmax $ymax
      }

# Record the current selected area.
      set SELECTED_AREA [list $xmin $ymin $xmax $ymax]

# If we are in mode 1 ("edit an existing polygon"), and the button was
# pressed over a vertex, drag the vertex if the shift key was not pressed.
   } elseif { !$MOVE } {

# Set the coordinates of the vertex marker to the current pointer coords.
      $CAN coords $VID0 $cx $cy

# Move the end of the vector which ends at the vertex.
      set coords [$CAN coords $VID1]
      $CAN coords $VID1 [lindex $coords 0] [lindex $coords 1] $cx $cy

# Move the start of the vector which starts at the vertex.
      set coords [$CAN coords $VID2]
      $CAN coords $VID2 $cx $cy [lindex $coords 2] [lindex $coords 3]

# If the shift key was pressed, drag the whole polygon.
   } {

# Find the offsets from the previous position.
      set dx [expr $cx - $ANCX]
      set dy [expr $cy - $ANCY]
      set ANCX $cx
      set ANCY $cy

# Move all canvas items which have the same tag as the root vertex.
      $CAN move $ROOTAG $dx $dy

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
#        Temporary file names created by UniqueFile are stored in Polka's
#        temporary POLKA_SCRATCH directory so that they are deleted when
#        Polka terminates. They have a name of the form polka<i> where
#        <i> is an integer, which is different for each file and
#        increases monotonically throughout the execution of Polka. IFILE
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

proc BoundBox {px py map inv} {
#+
#  Name:
#     BoundBox
#
#  Purpose:
#     Returns an NDF section string describing the bounding box which
#     just encloses the supplied pixel coordinates.
#
#  Arguments:
#     px
#        The X pixel coordinates.
#     py
#        The Y pixel coordinates.
#     map
#        If not blank, then the supplied pixel coordinates are mapped
#        using the mapping before the bounding box is evaluated.
#     inv
#        If this is non-zero, then the inverse mapping specified by "map"
#        is used (if a mapping was supplied).
#
#  Returned Value:
#     The section string, or a blank string if anything went wrong.
#-

# Assume failure.
   set sect ""

# If required map the supplied pixel coordinates.
   if { $map != "" } {
      set ok [TranList $map $inv $px $py mpx mpy]
   } {
      set mpx $px
      set mpy $py
      set ok 1
   }

# If the mapping was succesful, find the upper and lower limit of the
# mapped x axis values.
   if { $ok } {
      set xlo [lindex $mpx 0]
      set xhi $xlo
      foreach px $mpx {
         if { $px > $xhi } {
            set xhi $px
         } elseif { $px < $xlo } {
            set xlo $px
         }
      }

# Do the same for the Y axis.
      set ylo [lindex $mpy 0]
      set yhi $ylo
      foreach py $mpy {
         if { $py > $yhi } {
            set yhi $py
         } elseif { $py < $ylo } {
            set ylo $py
         }
      }

# Create an NDF section string, describing these limits. Convert the
# floating point pixel coordinates obtained above to integer pixel indices.
      set xlo [expr int( $xlo + 1.0 )]
      set ylo [expr int( $ylo + 1.0 )]
      set xhi [expr int( $xhi + 1.0 )]
      set yhi [expr int( $yhi + 1.0 )]
      set sect "($xlo:$xhi,$ylo:$yhi)"
   }

   return $sect

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
#     CANCEL (Read)
#        The path to the "Cancel" button.
#     MODE (Read and Write)
#        The interaction mode determining how to process button clicks
#        and motion in the GWM canvas item. Modes are:
#           0 - The user specifies image features by clicking on them.
#           1 - The user starts a new mask polygon, or edits an existing one.
#           2 - The user completes a started mask polygon.
#           3 - The user selects a label by clicking on an existing feature.
#     SELECTED_AREA (Read and Write)
#        The bounds of the selected area in the order xmin, xmax, ymin,
#        ymax.
#     V0 (Read)
#        The index of the first vertex of the incomplete polygon.
#  Notes:
#    -
#-
   global CANCEL
   global MODE
   global PRE_MODE4
   global SELECTED_AREA
   global V0

# Perform the cancel operation so long as no arguments were supplied.
   if { $args == "" } {

# If we are in mode 4, revert to the previous mode.
      if { $MODE == 4 } {
         SetMode $PRE_MODE4

# Cancel any selected area. Note, we need to check SELECTED_AREA
# explicitly (rather than just relying on CancelArea to do it),
# because CancelArea calls this procedure, so we could end up in an
# infinite call loop if we are not careful.
      } elseif { $SELECTED_AREA != "" } {
         CancelArea

# If no area was cancelled, cancel any incomplete mask polygon. This
# involves deleting the positions making up the polygon, and setting the
# interaction mode back to 1 (i.e. "start or edit a polygon" mode).
      } elseif { $MODE == 2 } {
         DelPosn $V0 1
      }
   }

# If there is neither a currrent area selection nor an incomplete mask,
# nor are we in MODE 4, disable the cancel button.
   if { $SELECTED_AREA == "" && $MODE != 2 && $MODE != 4 } {
      $CANCEL configure -state disabled
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
#     CAN (Read)
#        Path to the canvas containing the GWM image display.
#     DELETE (Read)
#        Path to the "Delete" button.
#     SELECTED_AREA (Read and Write)
#        The bounds of the selected area in the order xmin, xmax, ymin,
#        ymax. Set to a null string on exit.
#     ZOOM (Read)
#        Path to the "Zoom" button.
#
#
#  Notes:
#    - The Zoom and Delete buttons are disabled, and the Cancel button
#    will also be disabled if there is no reason to retain it in a normal
#    state.
#-
   global CAN
   global DELETE
   global EDITMENU
   global SELECTED_AREA
   global ZOOM

# Do nothing if no area has been selected.
   if { $SELECTED_AREA != "" } {

# Delete the canvas rectangle item marking the box.
      $CAN delete sbox

# Indicate that there i snow no area selected.
      set SELECTED_AREA ""

# Disable the ZOOM and DELETE buttons (etc).
      $ZOOM configure -state disabled
      $DELETE configure -state disabled
      $EDITMENU entryconfigure Delete -state disabled
      $EDITMENU entryconfigure Copy -state disabled

# Check that the CANCEL button is in the correct state.
      Cancel check
   }
}

proc CanDelete {id} {
#+
#  Name:
#     CanDelete
#
#  Purpose:
#     Delete a canvas item, executing any <Leave> bindings associated with the
#     item first.
#
#  Arguments:
#     id
#        The canvas identifier for the item to be deleted.
#
#  Globals:
#     CAN (Read)
#        The name of the canvas widget holding the GWM image.
#
#-
   global CAN
   global CURITEM

# If the item being deleted has set the canvas cursor (see proc CursorBind)
# we need to reset the cursor before deleting the item.
   if { [info exists CURITEM] && $CURITEM == $id } {

# Check each tag associated with the item.
      foreach tag [$CAN gettags $id] {

# If there is a <Leave> binding assocaited with this tag, execute it.
         if { ![catch {set cmd [$CAN bind $tag <Leave>] } ] } {
            eval $cmd
         }
      }
   }

# Delete the canvas item.
   $CAN delete $id
}

proc CanToNDF {cx cy} {
#+
#  Name:
#    CanToNDF
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
#    CX (Read)
#       The X offset for converting from canvas
#       coordinates to NDF pixel coordinates.
#    CY (Read)
#       The Y offset for converting from canvas
#       coordinates to NDF pixel coordinates.
#    MX (Read)
#       The X scale factor for converting from canvas
#       coordinates to NDF pixel coordinates.
#    MY (Read)
#       The Y scale factor for converting from canvas
#       coordinates to NDF pixel coordinates.
#
#  Notes:
#    -  This is the inverse of procedure NDFToCan
#-

   global CX
   global CY
   global MX
   global MY

# Get the pixel coordinates in the displayed image.
   set px [expr $CX + $MX * $cx ]
   set py [expr $CY + $MY * $cy ]

# Return the coordinates.
   return [list $px $py]

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

proc CheckRef {} {
#+
#  Name:
#    CheckRef
#
#  Purpose:
#    Ensure that the reference objects are not the same as the current
#    objects. If the displayed and reference images are the same,
#    then the "Reference:" radio button corresponding to the current
#    objects is disabled. If this radio button is currently
#    selected, then "None" is selected automatically instead.
#
#  Arguments:
#    None
#
#  Globals:
#    CUROBJ_DISP (Read)
#       The type of the currently displayed current objects.
#    IMAGE_DISP (Read)
#       The currently displayed image.
#    RB_REF (Read)
#       An array holding the names of the radiobuttons which are used to
#       select the reference object type.
#    REFIM_DISP (Read)
#       The current reference image.
#    REFOBJ_DISP (Read)
#       The type of the currently displayed reference objects.
#    RRB_DISABLED (Read and Write)
#       The name of the disabled "Reference:" radiobutton (if any).
#-
   global ACCEPT
   global CUROBJ_DISP
   global E_RAY_FEATURES
   global IMAGE_DISP
   global NONE
   global O_RAY_FEATURES
   global RB_REF
   global REFIM_DISP
   global REFOBJ_DISP
   global RRB_DISABLED

# If the reference and current images are the same, then the reference
# objects cannot be of the same type as the current objects.
   if { $REFIM_DISP == $IMAGE_DISP } {

# If the wrong reference button is currently disabled (the "correct" button
# is the one for the same type as the current objecs)...
      if { $RRB_DISABLED != $RB_REF($CUROBJ_DISP) } {

# ... re-enable any disabled button...
         if { $RRB_DISABLED != "" } {
            $RRB_DISABLED configure -state normal
         }

# ... and disable the "correct" button.
         $RB_REF($CUROBJ_DISP) configure -state disabled
         set RRB_DISABLED $RB_REF($CUROBJ_DISP)
      }

# If the button just disabled was previously selected, select "None"
# instead.
      if { $REFOBJ_DISP == $CUROBJ_DISP } { $RB_REF($NONE) invoke }

# If the reference and displayed image are different, there is no need to
# disable any button. Enable any previously disabled button.
   } {
      if { $RRB_DISABLED != "" } {
         $RRB_DISABLED configure -state normal
         set RRB_DISABLED ""
      }
   }

# Also set the state of the "Accept" button, which can only be used if
# both current and reference objects are image features (i.e. not masks).
   if { ( $REFOBJ_DISP == $O_RAY_FEATURES ||
          $REFOBJ_DISP == $E_RAY_FEATURES ) &&
        ( $CUROBJ_DISP == $O_RAY_FEATURES ||
          $CUROBJ_DISP == $E_RAY_FEATURES ) } {
      $ACCEPT configure -state normal
   } {
      $ACCEPT configure -state disabled
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

proc CheckSky {image object} {
#+
#  Name:
#    CheckSky
#
#  Purpose:
#    Checks that the supplied mask or sky object overlaps the supplied image.
#    At least one vertex must be more than 2 pixels away from the nearest
#    image edge for the area to be usable.
#
#  Arguments:
#     image
#        The name of the image.
#     object
#        The object to be checked. This should be one of O_RAY_MASK,
#        O_RAY_SKY, E_RAY_MASK or E_RAY_SKY.
#
#  Globals:
#     E_RAY_MASK (Read)
#        An integer representing the "E-ray mask" object type.
#     E_RAY_SKY (Read)
#        An integer representing the "E-ray sky" object type.
#     O_RAY_MASK (Read)
#        An integer representing the "O-ray mask" object type.
#     O_RAY_SKY (Read)
#        An integer representing the "O-ray sky" object type.
#     PNTPX (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel X coordinates.
#     PNTPY (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel Y coordinates.
#     SECTIONS (Read)
#        An array, indexed by image name, holding the pixel bounds of
#        the image as a standard NDF section specifier (eg "(100:230,-10:20)").
#
#  Returned Value:
#     1 if the supplied mask or sky area overlaps the supplied image.
#     Zero otherwise.
#
#-
   global E_RAY_MASK
   global E_RAY_SKY
   global O_RAY_MASK
   global O_RAY_SKY
   global SECTIONS
   global PNTPX
   global PNTPY

# Initialise
   set ret 0

# Get the pixel co-ordinate bounds for the image.
   set sec [SecList $SECTIONS($image)]
   if { $sec != "" } {
      set lx [expr [lindex $sec 0] - 1]
      set ux [lindex $sec 1]
      set ly [expr [lindex $sec 2] - 1]
      set uy [lindex $sec 3]

# Loop round each vertex in the mask or sky area.
      set size [llength $PNTPX($image,$object)]
      for {set i 0} {$i < $size} {incr i} {
         set px [lindex $PNTPX($image,$object) $i]
         set py [lindex $PNTPY($image,$object) $i]

# If this vertex is well within the image, set the returned flag and
# leave the loop.
         if { $px < ($ux - 3) && $px > ($lx + 3) && \
              $py < ($uy - 3) && $py > ($ly + 3) } {
            set ret 1
            break
         }
      }
   }

# Return the answer.
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
#     REDISPLAY_CANCELLED (read and Write)
#        Was a previous redisplay of the image cancelled because the
#        user looked like he may be about to enter a new scaling value?
#     REDISPLAY_REQUESTED (read and Write)
#        Was a redisplay of the image requested?
#-
   global OLD_VAL
   global REDISPLAY_REQUESTED
   global REDISPLAY_CANCELLED

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
   if { $val != $OLD_VAL || $REDISPLAY_CANCELLED } {
      set REDISPLAY_CANCELLED 0
      set OLD_VAL $val

# Set the length of the pause before the image is redisplayed.
      if { $pause } {
         set time 2500
      } {
         set time 0
      }

# Schedule the redisplay.
      set REDISPLAY_REQUESTED 1
      after $time {
         if { $REDISPLAY_REQUESTED } {
            set REDISPLAY_REQUESTED 0
            UpdateDisplay
         }
      }
   }
}

proc Clear {image obj} {
#+
#  Name:
#     Clear
#
#  Purpose:
#     Deletes position and mapping information.
#
#  Arguments:
#     image
#        The name of the image to clear. If this is blank, then all
#        images are cleared.
#     obj
#        The objects to be cleared. If this is null then all objects are
#        cleared are cleared from the specified images.
#
#  Globals:
#     E_RAY_FEATURES (Read)
#        An integer representing the "E-ray features" object type.
#     E_RAY_MASK (Read)
#        An integer representing the "E-ray mask" object type.
#     IMAGES (Read)
#        A list of the supplied images (without sections).
#     IMSECS (Read)
#        A list of the input image sections as supplied by the user.
#     O_RAY_FEATURES (Read)
#        An integer representing the "O-ray features" object type.
#     O_RAY_MASK (Read)
#        An integer representing the "O-ray mask" object type.
#-
   global E_RAY_FEATURES
   global E_RAY_MASK
   global IMAGES
   global O_RAY_FEATURES
   global O_RAY_MASK
   global O_RAY_SKY
   global E_RAY_SKY
   global REDRAW
   global REFALN
   global IMMAP
   global IMAGE_DISP
   global IMSEC_DISP
   global IMSEC_FIRST
   global IMSEC_REQ
   global OEMAP
   global OBJTYPE
   global SECTION_DISP
   global SECTION_STACK

# Only proceed if the operation is confirmed...
   if { $obj == "" } {
      set mess "Clear all features, masks, effects and mappings "
   } {
      set mess "Clear the $OBJTYPE($obj) "
   }

   if { $image == "" } {
      append mess "for all images?"
      set list $IMAGES
   } {
      append mess "for image\"$image\"?"
      set list $image
   }

   if { $image == "" && $obj == "" } {
      set redisp 1
   } {
      set redisp 0
   }

   if { [Confirm $mess] } {

# Clear the "Draw Aligned" checkbutton.
      set REFALN 0
      $REDRAW configure -state disabled

# Loop round all images...
      foreach image $list {

# Undo any effects applied to the image.
         Effects $image "Undo All" 1

# Only delete mappings if a specific object has not been specified.
         if { $obj == "" } {

# Delete the OE mapping (if any) for this image.
            if { [info exists OEMAP($image)] } {
               unset OEMAP($image)
            }

# Delete the image mapping (if any) for this image.
            if { [info exists IMMAP($image)] } {
               unset IMMAP($image)
            }
         }

# Loop round each object type...
         foreach object [list $O_RAY_FEATURES $E_RAY_FEATURES $O_RAY_MASK \
                              $E_RAY_MASK $O_RAY_SKY $E_RAY_SKY] {

# Only delete them if they have been selected.
            if { $obj == $object || $obj == "" } {

# Continue deleting the first position in the list until there are no
# positions left.
               while { [NumPosn "" $image $object] > 0 } {
                  DelPosn 0 0 $image $object
               }
            }
         }
      }

# If everything is being cleared, re-display the first image.
      if { $redisp } {
         set IMSEC_REQ $IMSEC_FIRST
         UpdateDisplay gwm
      }
   }
}

proc ClearRef {} {
#+
#  Name:
#     ClearRef
#
#  Purpose:
#     Erase reference objects from the display.
#
#  Arguments:
#     None.
#
#  Globals:
#     REFIM_DISP (Read)
#        The image from which the currently displayed reference objects
#        are derived.
#     REFOBJ_DISP (Read and Write)
#        The type of reference objects currently displayed.
#        Set to NONE on exit.
#-
   global REFOBJ_DISP
   global REFIM_DISP
   global NONE

# Only clear the reference objects if some are currently displayed.
   if { $REFOBJ_DISP != $NONE } {
      ClearPosns $REFIM_DISP $REFOBJ_DISP
      set REFOBJ_DISP $NONE
   }
}

proc ClearCur {} {
#+
#  Name:
#     ClearCur
#
#  Purpose:
#     Erase current objects from the display.
#
#  Arguments:
#     None.
#
#  Globals:
#     CUROBJ_DISP (Read and Write)
#        The type of current objects currently displayed.
#        Set to NONE on exit.
#-
   global CUROBJ_DISP
   global NONE

# Only clear the current objects if some are currently displayed.
   if { $CUROBJ_DISP != $NONE } {
      ClearPosns
      set CUROBJ_DISP $NONE
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
#     args
#        An optional list argument holding the image from which the
#        positions are derived, and the type of objects to be cleared.
#        If these are not supplied, they default to $IMAGE_DISP and
#        $CUROBJ_DISP.
#
#  Globals:
#     CUROBJ_DISP (Read)
#        The type of the current objects displayed.
#     IMAGE_DISP (Read)
#        The displayed image (without section).
#     PNTID (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas item identifiers associated with the
#        positions in the list. A value of -1 indicates that no marker is
#        currently drawn for the position.
#     PNTVID (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas item identifiers associated with the
#        vectors between positions in the list. A value of -1 indicates that
#        no line is currently drawn for the position. A blank string
#        indicates that no vector is defined.
#-
   global CAN
   global CUROBJ_DISP
   global IMAGE_DISP
   global PNTID
   global PNTVID

# Store the image and object type.
   if { $args == "" } {
      set image $IMAGE_DISP
      set object $CUROBJ_DISP
   } {
      set image [lindex $args 0]
      set object [lindex $args 1]
   }

# Do nothing if the list is empty.
   if { [info exists PNTID($image,$object)] } {
      set size [llength $PNTID($image,$object)]
      if { $size > 0 } {

# Delete any canvas items marking the positions in the list. The item
# indices stored in the list are set to -1 if the marker is not currently
# drawn. Do both the position markers and the vectors joining adjacent
# vertices in a polygon.
         for {set i 0} {$i < $size} {incr i} {

            set id [lindex $PNTID($image,$object) $i]
            if { $id != -1 } {
               CanDelete $id
               set PNTID($image,$object) [lreplace $PNTID($image,$object) $i $i -1]
            }

            set vid [lindex $PNTVID($image,$object) $i]
            if { $vid != -1 && $vid != "" } {
               CanDelete $vid
               set PNTVID($image,$object) [lreplace $PNTVID($image,$object) $i $i -1]
            }
         }
      }
   }
}

proc ClearGwm {args} {
#+
#  Name:
#     ClearGwm
#
#  Purpose:
#     Clear the GWM image display.
#
#  Arguments:
#     args
#        An optional argument which, if supplied, indicates that the
#        same image is about to be re-displayed. In this case IMSEC_DISP
#        is left unchanged. The value supplied for "args" is insignificant.
#
#  Globals:
#     IMSEC_DISP (Write)
#        The currently didsplayed image (as supplied by the user). Set to a
#        null string on exit (unless "args" is supplied).
#-
   global DEVICE
   global IMSEC_DISP

# Clear the GWM display and the delete all AGI pictures. Abort Polka
# if an error occurs.
   Obey kapview gdclear "device=$DEVICE" 1

# Set the globals IMSEC_DISP blank to indicate that no image
# is currently displayed (unless the same image is about to be re-displayed).
   if { $args == "" } {
      set IMSEC_DISP ""
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
#        to (eg "CURCOL").
#
#  Notes:
#    - The SetColours command is called whenever any of the colours are
#    changed.
#-
   global COLMENU

# Create a lower case version of the variable name.
   set lvar [string tolower $var]

# Create the new item to the Colours menu.
   $menu add cascade -label $label -menu $menu.$lvar

# Create the sub-menu, and store its name in global array COLMENU.
   set thismenu [menu $menu.$lvar]
   set COLMENU($var) $thismenu

# Add the list of colours to the new sub-menu.
   foreach col "red blue green cyan yellow magenta black" {
      $thismenu add radiobutton -label $col -variable $var \
                                -value $col -selectcolor $col \
                                -command "SetColours $var"
   }

}

proc ConcMap {map1 inv1 map2 inv2} {
#+
#  Name:
#     ConcMap
#
#  Purpose:
#     Create a new mapping by concatenating two supplied mappings.
#
#  Arguments:
#     map1
#        The mapping to be applied first. In the form of
#        a list of 6 parameter values.
#     inv1
#        Should the inverse mapping from map1 be used?
#     map2
#        The mapping to be applied second. In the form of
#        a list of 6 parameter values.
#     inv2
#        Should the inverse mapping from map2 be used?
#
#  Returned Value:
#     A list of 6 parameter values desribing the mapping, or a blank
#     string if anything went wrong. The string "ref" is returned to
#     represent a unit mapping.
#-

# Assume the mapping is undefined.
   set map3 ""

# If required, invert the mappings.
   if { $inv1 } { set map1 [InvMap $map1] }
   if { $inv2 } { set map2 [InvMap $map2] }

# Check both mappings are defined.
   if { $map1 != "" && $map2 != "" } {

# If both mappings are unit mappings, return a unit mapping.
      if { $map1 == "ref" && $map2 == "ref" } {
         set map3 "ref"

# If one of the two mappings is a unit mapping, return the other one.
      } elseif { $map1 == "ref" } {
         set map3 $map2

      } elseif { $map2 == "ref" } {
         set map3 $map1

# If neither mapping is a unit mapping...
      } {

# Extract the parameter values from the lists.
         set a1 [lindex $map1 0]
         set a2 [lindex $map1 1]
         set a3 [lindex $map1 2]
         set a4 [lindex $map1 3]
         set a5 [lindex $map1 4]
         set a6 [lindex $map1 5]

         set b1 [lindex $map2 0]
         set b2 [lindex $map2 1]
         set b3 [lindex $map2 2]
         set b4 [lindex $map2 3]
         set b5 [lindex $map2 4]
         set b6 [lindex $map2 5]

# Evaluate the parameters describing the concatentation.
         set ab1 [expr $b1 + $b2*$a1 + $b3*$a4]
         set ab2 [expr       $b2*$a2 + $b3*$a5]
         set ab3 [expr       $b2*$a3 + $b3*$a6]

         set ab4 [expr $b4 + $b5*$a1 + $b6*$a4]
         set ab5 [expr       $b5*$a2 + $b6*$a5]
         set ab6 [expr       $b5*$a3 + $b6*$a6]

# Contruct the return list.
         set map3 [list $ab1 $ab2 $ab3 $ab4 $ab5 $ab6]
      }
   }

   return $map3

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
   global F_OWNER

# Set the F_OWNER variable so that this window is handed the focus by the
# main Polka window. Set the current value to be re-instated later.
   set old_f_owner $F_OWNER
   set F_OWNER .confirm

# Display the dialog box and get the user's response.
   set but [dialog .confirm "Polka Confirmation" $message {} 0 OK Cancel]

# Restablish the original value of F_OWNER.
   set F_OWNER $old_f_owner

# Return the answer.
   return [expr 1 - $but]
}

proc ConvMap {gotc type} {
#+
#  Name:
#     ConvMap
#
#  Purpose:
#     Convert between a 6-coefficient representation of a linear mapping,
#     and a "shift,rotation,magnification" representation.
#
#  Arguments:
#     gotc
#        If non-zero, then the coefficient values in MAP_C are converted to
#        shift, rotation and magnification values. Otherwise, the opposite
#        is done.
#     type
#        The current mapping type (1-4), or null. If "gotc" is non-zero, then
#        this determines the default rotations and magnifications returned
#        if any of the C coefficients are not supplied. If "gotc" is zero,
#        then any restrictions on the mapping implied by the map type are
#        imposed on the supplied shifts, rotations and magnifications.
#
#  Returned Value:
#     Zero if any of the supplied values were blank (in which case all the
#     returned values will also be blank), and one otherwise.
#
#  Globals:
#     MAP_SX (Read and Write)
#       The shift of the magnified X axis.
#     MAP_SY (Read and Write)
#       The shift of the magnified Y axis.
#     MAP_RX (Read and Write)
#       The anti-clockwise rotation of the original X axis (in degrees).
#     MAP_MX (Read and Write)
#       The magnification of the rotated X axis.
#     MAP_RY (Read and Write)
#       The anti-clockwise rotation of the original Y axis (in degrees).
#     MAP_MY (Read and Write)
#       The magnification of the rotated Y axis.
#     MAP_C (Read and Write)
#       An array of 6 coefficient values.
#     RTOD (Read)
#       The conversion factor from radians to degrees.
#-
   global MAP_SX
   global MAP_SY
   global MAP_RX
   global MAP_MX
   global MAP_RY
   global MAP_MY
   global MAP_C
   global RTOD

# Assume the mapping is undefined.
   set ok 0

# First convert coefficient values into shift, rot and mag values.
   if { $gotc } {

# Initialise the returned values.
      set MAP_SX ""
      set MAP_SY ""

      if { $type == 1 } {
         set MAP_RX 0.0
         set MAP_RY 0.0
         set MAP_MX 1.0
         set MAP_MY 1.0

      } elseif { $type == 2 } {
         set MAP_RX ""
         set MAP_RY ""
         set MAP_MX 1.0
         set MAP_MY 1.0

      } elseif { $type == 3 } {
         set MAP_RX 0.0
         set MAP_RY 0.0
         set MAP_MX ""
         set MAP_MY ""

      } {
         set MAP_RX ""
         set MAP_RY ""
         set MAP_MX ""
         set MAP_MY ""
      }

# If a mapping was supplied, do the conversions.
      if { $MAP_C(1) != "" && $MAP_C(2) != "" && $MAP_C(3) != "" &&
           $MAP_C(4) != "" && $MAP_C(5) != "" && $MAP_C(6) != "" } {
         set c1  $MAP_C(1)
         set c2  $MAP_C(2)
         set c3  $MAP_C(3)
         set c4  $MAP_C(4)
         set c5  $MAP_C(5)
         set c6  $MAP_C(6)

         set MAP_SX $c1
         set MAP_SY $c4
         set MAP_MX [expr hypot( $c2, $c5 )]
         set MAP_MY [expr hypot( $c3, $c6 )]
         set MAP_RX [expr atan2( -$c5, $c2 )*$RTOD]
         set MAP_RY [expr atan2( $c3, $c6 )*$RTOD]

         set ok 1
      }

# Now convert shift, rot and mag values into coefficient values.
   } {

# Initialise the returned values.
      set MAP_C(1) ""
      set MAP_C(2) ""
      set MAP_C(3) ""
      set MAP_C(4) ""
      set MAP_C(5) ""
      set MAP_C(6) ""

# If the mapping type is restrictive, use the X axis values of
# magnification and/or rotation in place of the supplied Y values.
      if { $type == 1 } {
         set MAP_MX 1.0
         set MAP_MY 1.0
         set MAP_RX 0.0
         set MAP_RY 0.0

      } elseif { $type == 2 } {
         set MAP_MX 1.0
         set MAP_MY 1.0
         set MAP_RY $MAP_RX

      } elseif { $type == 3 } {
         set MAP_MY $MAP_MX
         set MAP_RX 0.0
         set MAP_RY 0.0

      } elseif { $type == 4 } {
         set MAP_MY $MAP_MX
         set MAP_RY $MAP_RX

      }

# If any of the supplied values are blank, return blank values.
      if { $MAP_SX != "" && $MAP_SY != "" &&
           $MAP_MX != "" && $MAP_MY != "" &&
           $MAP_RX != "" && $MAP_RY != "" } {

# Do the conversions.
         set MAP_C(1) $MAP_SX
         set MAP_C(2) [expr $MAP_MX*cos($MAP_RX/$RTOD)]
         set MAP_C(3) [expr $MAP_MY*sin($MAP_RY/$RTOD)]
         set MAP_C(4) $MAP_SY
         set MAP_C(5) [expr -$MAP_MX*sin($MAP_RX/$RTOD)]
         set MAP_C(6) [expr $MAP_MY*cos($MAP_RY/$RTOD)]

         set ok 1

      }
   }
   return $ok
}

proc Copy {} {
#+
#  Name:
#     Copy
#
#  Purpose:
#     Take a copy of any complete polygons inside the currently selected
#     area. The copy can be accessed using procedures SetPosn, GetPosn,
#     etc, by supplying the above procedures with two optional trailing
#     arguments specifying the object and image, which should both be
#    equal to "copy".
#
#  Arguments:
#     None.
#-
   global PASTE
   global EDITMENU
   global SELECTED_AREA
   global PNTCY
   global PNTCX
   global PNTID
   global PNTLBL
   global PNTNXT
   global PNTPY
   global PNTPX
   global PNTVID
   global PNTTAG
   global MODE

# Do nothing if there is no selected area, or we are not in MODE 1
# ("Create or edit a polygon").
   if { $SELECTED_AREA != "" && $MODE == 1 } {

# Erase any previous copied object.
      if { $PASTE } {
         set PASTE 0
         $EDITMENU entryconfigure Paste -state disabled
         unset PNTCY(copy,copy)
         unset PNTCX(copy,copy)
         unset PNTID(copy,copy)
         unset PNTLBL(copy,copy)
         unset PNTNXT(copy,copy)
         unset PNTPY(copy,copy)
         unset PNTPX(copy,copy)
         unset PNTVID(copy,copy)
         unset PNTTAG(copy,copy)
      }

# Store the number of current position.
      set size [NumPosn ""]

# Store the bounds of the selected area.
      set xmin [lindex $SELECTED_AREA 0]
      set ymin [lindex $SELECTED_AREA 1]
      set xmax [lindex $SELECTED_AREA 2]
      set ymax [lindex $SELECTED_AREA 3]

# Loop round each current position.
      for {set i 0} {$i < $size} {incr i} {

# Get the canvas coordinates of theposition, and the index of the next vertex.
         set cx [GetPosn $i CX]
         set cy [GetPosn $i CY]
         set nxt [GetPosn $i NXT]

# Loop round adjacent vertices, checking that they are within the selected
# area. Leave the loop if the position is not a vertex, if we arrive
# back at the original vertex.
         set ok 0
         while { $cx >= $xmin && $cx <= $xmax &&
                 $cy >= $ymin && $cy <= $ymax &&
                 $nxt != -1 && $nxt != "" } {

# If the next vertex is the original vertex, we have been round the entire
# polygon without going outside the selected area. Set a flag to indicate
# that the current position can be copied, ane leave the loop.
            if { $nxt == $i } {
               set ok 1
               break
            }

# Get the value for this position.
            set cx [GetPosn $nxt CX]
            set cy [GetPosn $nxt CY]
            set nxt [GetPosn $nxt NXT]
         }

# If this position is a vertex of a polygon which is enclosed within the
# selected area, then add it to the copy list.
         if { $ok } {
            set PASTE 1
            set cx [GetPosn $i CX]
            set cy [GetPosn $i CY]
            set nxt [GetPosn $i NXT]
            set tag [GetPosn $i TAG]
            set ident($i) [SetPosn -1 "CX CY VID NXT TAG" [list $cx $cy -1 $nxt $tag] copy copy]
         }
      }

# Cancel the area selection.
      CancelArea

# If a polygon was copied, enable the Paste item in the Edit menu, and
# correct the indices of the "next" vertices to refer to the arrays
# holding the copied positions.
      if { $PASTE } {
         $EDITMENU entryconfigure Paste -state normal

         for { set i 0 } { $i < [NumPosn "" copy copy] } { incr i } {
            SetPosn $i "NXT" $ident([GetPosn $i NXT copy copy])
         }
      }
   }
}

proc CreateMask {image object} {
#+
#  Name:
#    CreateMask
#
#  Purpose:
#    This procedure creates a default mask for the supplied image and
#    ray if it does not already have a mask. A search is made for an
#    image for which the required mask is defined. If found, the first
#    such mask is returned. If no such mask is found, all images are
#    checked (starting with the specified image) to see if any has both
#    an OE mapping and a defined "other" mask. The first such image
#    found is used to create the required mask by mapping its "other"
#    mask using its OE mapping.
#
#  Arguments:
#    image
#       The image to which the mask refers.
#    object
#       The identifier ($O_RAY_MASK, $E_RAY_MASK ) for the
#       mask to be created.
#
#  Returned Value:
#    One if the mask is available, zero otherwise.
#
#  Globals:
#     E_RAY_MASK (Read)
#        An integer representing the "E-ray mask" object type.
#     IMAGES (Read)
#        A list of the available images (without section strings).
#     O_RAY_MASK (Read)
#        An integer representing the "O-ray mask" object type.
#     PNTPX (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel X coordinates.
#-
   global E_RAY_MASK
   global IMAGES
   global O_RAY_MASK
   global OBJTYPE
   global PNTPX

# Assume success.
   set ok 1

# If the requested mask already exists, do nothing.
   if { [llength $PNTPX($image,$object)] == 0 } {

# Tell the user what is happening.
      set told [SetInfo "Creating the default $OBJTYPE($object). Please wait... " 0]

# Assume for the moment that no mask can be created.
      set ok 0

# Get the name of the other ray's mask. Decide whether to use the forward
# or inverse OE mapping (forward goes from E to O) to create the required
# mask from the other mask (if possible).
      if { $object == $O_RAY_MASK } {
         set other $E_RAY_MASK
         set inv 0
      } {
         set other $O_RAY_MASK
         set inv 1
      }

# Search for an image with a mask of the same type. Copy the first
# one found.
      foreach im $IMAGES {
         if { [llength $PNTPX($im,$object)] > 0 } {
            TranPXY "ref" 0 $im $object $image $object
            set ok 1
            break
         }
      }

# If no usable mask was found...
      if { !$ok } {

#  If the specified image has the "other" mask, use it, together with the
#  image's OE mapping to define the required mask.
         set map [OEMapping $image]
         if { $map != "" && [llength $PNTPX($image,$other)] > 0 } {
            TranPXY $map $inv $image $other $image $object
            set ok 1
         }
      }

# If no usable mask was found...
      if { !$ok } {

# Search for an image with a mask of the other type AND an OE mapping. Map
# the first such mask found. Do not include the specified image first since
# this was checked earlier.
         foreach im $IMAGES {
            if { $im != $image } {
               set map [OEMapping $im]
               if { $map != "" && [llength $PNTPX($im,$other)] > 0 } {
                  TranPXY $map $inv $im $other $image $object
                  set ok 1
                  break
               }
            }
         }
      }

# Cancel the informative text set earlier in this procedure.
      if { $told } { SetInfo "" 0 }

   }

   return $ok
}

proc CreateSky {image object} {
#+
#  Name:
#    CreateSky
#
#  Purpose:
#    This procedure creates a default sky area for the supplied image and
#    ray if it does not already have one.
#
#    If the image has one sky area but not the other, and also has an OE
#    mapping, then the OE mapping is used to create the missing sky area,
#    based on the existing sky area. Otherwise,
#    if the image has an image mapping (i.e. from the image to the
#    reference image), and the reference image has a mask, then the image
#    mapping is used to create the missing mask based on the reference
#    images's mask.
#
#  Arguments:
#    image
#       The image to which the mask refers.
#    object
#       The identifier ($O_RAY_SKY, $E_RAY_SKY ) for the
#       sky area to be created.
#
#  Returned Value:
#    One if the sky area is available, zero otherwise.
#
#  Globals:
#     IMAGES (Read)
#        A list of the available images (without section strings).
#     PNTPX (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel X coordinates.
#-
   global E_RAY_SKY
   global IMAGES
   global IMAGE_PREV
   global O_RAY_SKY
   global OBJTYPE
   global PNTPX
   global RESAVE

# Assume success.
   set ok 1

# Indicate that we do not need to issue a warning about sky areas not
# overlapping the image.
   set warn 0

# If the requested sky area already exists, do nothing.
   if { [llength $PNTPX($image,$object)] == 0 } {

# Tell the user what is happening.
      set told [SetInfo "Creating the default $OBJTYPE($object). Please wait... " 0]

# Assume for the moment that no sky area can be created.
      set ok 0

# Get the name of the other ray's sky area. Decide whether to use the forward
# or inverse OE mapping (forward goes from E to O) to create the required
# sky area from the other sky area (if possible).
      if { $object == $O_RAY_SKY } {
         set other $E_RAY_SKY
         set inv 0
      } {
         set other $O_RAY_SKY
         set inv 1
      }

# Create a list of images to search for a sky area. Start with the
# previously displayed image (if any).
      if { [info exists IMAGE_PREV] && $IMAGE_PREV != "" } {
         set imlist "$IMAGE_PREV $IMAGES"
      } {
         set imlist "$IMAGES"
      }

# If the supplied image has the other sky area, and also has an OE mapping,
# transform the supplied image's other sky area.
      if { $other != "" && [llength $PNTPX($image,$other)] > 0 } {
         set map [OEMapping $image]
         if { $map != "" } {
            set oldrs $RESAVE
            TranPXY $map $inv $image $other $image $object

# Check that the new sky area overlaps the image. If it does not, the sky
# area cannot be used so erase it.
            if { ![CheckSky $image $object] } {
               set ok 0
               set warn 1
               Erase $image $object
               set RESAVE $oldrs

            } {
               set ok 1
            }
         }
      }

# If the sky area has not yet been created, and if the current image has an
# image mapping...
      set map0 [ImageMapping $image]
      if { !$ok && $map0 != "" } {

# ... search for an image which has the required sky area, and an image
# mapping. The previously displayed image is checked first.
         foreach im $imlist {
            set map1 [ImageMapping $im]
            if { $map1 != "" && [llength $PNTPX($im,$object)] > 0 } {

# Create the require sky area from this image's sky area.
               set map [ConcMap $map1 0 $map0 1]
               if { $map != "" } {
                  set oldrs $RESAVE
                  TranPXY $map 0 $im $object $image $object

# Check that the new sky area overlaps the image. If it does not, the sky
# area cannot be used so erase it.
                  if { ![CheckSky $image $object] } {
                     set ok 0
                     set warn 1
                     Erase $image $object
                     set RESAVE $oldrs

# If the new sky area overlaps the image, we can use it. So leave the loop.
                  } {
                     set ok 1
                     break
                  }

               }
            }
         }
      }

# If we still do not have a sky area, go through the images again, this time
# looking for one with the other sky area and both image and OE mappings.
# The previously displayed image is checked first.
      if { !$ok && $other != "" } {
         set map0 [ImageMapping $image]
         if { $map0 != "" } {
            foreach im $imlist {
               set map1 [ImageMapping $im]
               set map2 [OEMapping $im]
               if { $map1 != "" && $map2 != "" &&
                    [llength $PNTPX($im,$other)] > 0 } {

# Create the require sky area from this image's other sky area.
                  set map [ConcMap [ConcMap $map2 $inv $map1 0] 0 $map0 1]
                  if { $map != "" } {
                     set oldrs $RESAVE
                     TranPXY $map 0 $im $other $image $object

# Check that the new sky area overlaps the image. If it does not, the sky
# area cannot be used so erase it.
                     if { ![CheckSky $image $object] } {
                        set ok 0
                        set warn 1
                        Erase $image $object
                        set RESAVE $oldrs

# If the new sky area overlaps the image, we can use it. So leave the loop.
                     } {
                        set ok 1
                        break
                     }
                  }
               }
            }
         }
      }

# Cancel the informative text set earlier in this procedure.
      if { $told } { SetInfo "" 0 }

# If a sky area could not be created because the existing sky areas were
# off the image, issue a warning.
      if { !$ok && $warn } {
         Message "Cannot use existing sky areas because they do not fall within the displayed image. Please supply new sky areas for this image."
      }
   }

   return $ok
}

proc CursorBind {tags cursors} {
#+
#  Name:
#     CursorBind
#
#  Purpose:
#     Set up binding for canvas items with specified tags so that specified
#     cursors are displayed when the pointer is placed over the items.
#
#  Arguments:
#     tags
#        A list of canvas tags.
#     cursors
#        A list of cursor specifications, which should be in one-to-one
#        correspondance with the elements in "tags". The specified cursor
#        is displayed whenever the pointer is placed over a canvas item
#        with the corresponding tag. If a null value is supplied, then
#        the bindings are removed items with the corresponding tag.
#
#-

   global CAN

# Loop round each corresponding pair of tag and cursor.
   for {set i 0} {$i < [llength $tags]} {incr i} {
      set tag [lindex $tags $i]
      set cursor [lindex $cursors $i]

# If a null cursor has been supplied, remove the bindings.
      if { $cursor == "" } {
         $CAN bind $tag <Enter> ""
         $CAN bind $tag <Leave> ""

# Otherwise set up new bindings for entering and leaving the canvas item.
      } {
         $CAN bind $tag <Enter> \
            "set citem \[$CAN find withtag current\]
             if { \$citem != \$CURITEM } {
                Push CURSOR_STACK \[$CAN cget -cursor\]
                $CAN config -cursor $cursor
                set CURITEM \$citem
             }"
         $CAN bind $tag <Leave> \
            "set citem \[$CAN find withtag current\]
             if { \$citem == \$CURITEM } {
                $CAN config -cursor \[Pop CURSOR_STACK\]
                set CURITEM \"\"
             }"
      }
   }
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
#     SELECTED_AREA (Read)
#        The bounds of the selected area in the order xmin, xmax, ymin,
#        ymax.
#-
   global SELECTED_AREA

# Issue a warning and return if there is no selected area.
   if { $SELECTED_AREA == "" } {
      Message "Select an area by clicking and dragging over the image before using the \"Delete\" command."
      return
   }

# Store the number of current position.
   set size [NumPosn ""]

# Store the bounds of the selected area.
   set xmin [lindex $SELECTED_AREA 0]
   set ymin [lindex $SELECTED_AREA 1]
   set xmax [lindex $SELECTED_AREA 2]
   set ymax [lindex $SELECTED_AREA 3]

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
#    Deletes an image position.
#
#  Arguments:
#    i
#       The index of the position, starting at zero.
#    all
#       If 1, then all positions connected by vectors to the supplied
#       position will also be deleted.
#    args
#       An optional list argument holding the image from which the
#       positions are derived, and the type of objects to be drawn.
#       If these are not supplied, they default to $IMAGE_DISP and
#       $CUROBJ_DISP.
#
#  Globals:
#     CAN (Read)
#        Path to the canvas containing the GWM image display.
#     CUROBJ_DISP (Read)
#        The type of the current objects displayed.
#     IMAGE_DISP (Read)
#        The displayed image (without section).
#     PNTCX (Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas X coordinates.
#     PNTCY (Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas Y coordinates.
#     PNTID (Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas item identifiers associated with the
#        positions in the list. A value of -1 indicates that no marker is
#        currently drawn for the position.
#     PNTLBL (Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of labels associated with the positions in the list.
#     RECALC_OEMAP (Write)
#        A 1-d array indexed by image. Each element is a logical flag
#        indicating if the image features for the corresponding image
#        have changed since the image's E to O mapping was last found.
#     RECALC_IMMAP (Write)
#        A 1-d array indexed by image. Each element is a logical flag
#        indicating if the image features for the corresponding image
#        have changed since the mapping from the image to the first
#        (reference) image was last found. The flag is set for all
#        images if the image features for the first (reference) image
#        are changed.
#     PNTNXT (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of integers representing indices within the lists
#        given by PNTxxx. Each integer gives the index of the next position
#        along the edge of a polygon. The vector starting at position
#        index i, ends at position index given by the i'th element of
#        PNTNXT. If this value is blank ("") then position i is not part of
#        a polygon. If this value is -1, then the end of the vector starting
#        position i is unspecified (in this case the polygon is not
#        closed).
#     PNTPX (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel X coordinates.
#     PNTPY (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel Y coordinates.
#     PNTTAG (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas tags (one for each position).
#     PNTVID (Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas item identifiers associated with the
#        vectors between positions in the list. A value of -1 indicates that
#        no line is currently drawn for the position. A blank string
#        indicates that no vector is defined.
#     V0 (Read and Write)
#        The index of the position corresponding to the first vertex in
#        an incomplete (i.e. open) polygon.
#     V1 (Read and Write)
#        The index of the position corresponding to the last vertex in
#        an incomplete (i.e. open) polygon.
#     VCX0 (Write)
#        The canvas X coordinates at the first vertex in an incomplete
#        (i.e. open) polygon.
#     VCY0 (Write)
#        The canvas Y coordinates at the first vertex in an incomplete
#        (i.e. open) polygon.
#     VID2 (Write)
#        The canvas item id of the vector joining the last vertex in an
#        incomplete (i.e. open) polygon, to the pointer.
#
#-
   global CAN
   global CUROBJ_DISP
   global E_RAY_FEATURES
   global IMAGES
   global IMAGE_DISP
   global O_RAY_FEATURES
   global PNTCY
   global PNTCX
   global PNTID
   global PNTLBL
   global PNTNXT
   global PNTPY
   global PNTPX
   global PNTVID
   global PNTTAG
   global RECALC_IMMAP
   global RECALC_OEMAP
   global RESAVE
   global V0
   global V1
   global VCX0
   global VCY0
   global VID2

# Store the image and object type.
   if { $args == "" } {
      set image $IMAGE_DISP
      set object $CUROBJ_DISP
   } {
      set image [lindex $args 0]
      set object [lindex $args 1]
   }

# Note the next vertex in the polygon (if any).
   set next [lindex $PNTNXT($image,$object) $i]

# If in a polygon, get the number of vertices in the same polygon.
   if { $next != "" } {
      set nvert [NumPosn $i $image $object]
   }

# Get the size of the list.
   if { [info exists PNTID($image,$object)] } {
      set size [llength $PNTID($image,$object)]
   } {
      set size 0
   }

# If no position with the supplied index exists, leave the lists unchanged.
   if { $i >= 0 && $i < $size } {

# Delete any canvas item marking the position.
      set id [lindex $PNTID($image,$object) $i]
      if { $id != -1 } {
         CanDelete $id
         set PNTID($image,$object) [lreplace $PNTID($image,$object) $i $i -1 ]
      }

# If the position has an associated vector, delete it. Store the end
# coordinates of the line first.
      set idedge [lindex $PNTVID($image,$object) $i]
      if { $idedge != "" && $idedge != -1 } {
         set coords [$CAN coords $idedge]
         set xn [lindex $coords 2]
         set yn [lindex $coords 3]
         CanDelete $idedge
         set PNTVID($image,$object) [lreplace $PNTVID($image,$object) $i $i -1 ]
      } {
         set xn ""
      }

# Store the index of the position to which any vector associated with the
# current position pointed.
      set k [lindex $PNTNXT($image,$object) $i]

# If the position being deleted is the first in a polygon, modify the
# index and coords of the first position.
      if { $i == $V0 } {
         if { $k != "" && $k != -1 } {
            set V0 $k
            set VCX0 [lindex $PNTCX($image,$object) $k]
            set VCY0 [lindex $PNTCX($image,$object) $k]
         } {
            set V0 ""
            set VCX0 ""
            set VCY0 ""
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
         if { [lindex $PNTNXT($image,$object) $j] == $i } {
            set jj $j
             set lid2 [lindex $PNTVID($image,$object) $j]
            if { $lid2 != "" && $lid2 != -1 } {
               if { $xn != "" } {
                  set coords [$CAN coords $lid2]
                  set cxbeg [lindex $coords 0]
                  set cybeg [lindex $coords 1]
                  $CAN coords $lid2 $cxbeg $cybeg $xn $yn
               } {
                  CanDelete $lid2
                  set PNTVID($image,$object) [lreplace $PNTVID($image,$object) $j $j -1 ]
               }
            }
             set PNTNXT($image,$object) [lreplace $PNTNXT($image,$object) $j $j $k]
          }
      }

# If the position being deleted is the last in a polygon, modify the
# index and coords of the last position.
      if { $i == $V1 } {
         if { $jj != "" } {
            set V1 $jj
            set VID2 $lid2
         } {
            set V1 ""
            set VID2 ""
         }
      }

# Decrement the number of times the position's label is used.
      set label [lindex $PNTLBL($image,$object) $i]
      Labels $label 0

# Remove the entry from the lists.
      set PNTCX($image,$object) [lreplace $PNTCX($image,$object) $i $i]
      set PNTCY($image,$object) [lreplace $PNTCY($image,$object) $i $i]
      set PNTPX($image,$object) [lreplace $PNTPX($image,$object) $i $i]
      set PNTPY($image,$object) [lreplace $PNTPY($image,$object) $i $i]
      set PNTID($image,$object) [lreplace $PNTID($image,$object) $i $i]
      set PNTLBL($image,$object) [lreplace $PNTLBL($image,$object) $i $i]
      set PNTVID($image,$object) [lreplace $PNTVID($image,$object) $i $i]
      set PNTNXT($image,$object) [lreplace $PNTNXT($image,$object) $i $i]
      set PNTTAG($image,$object) [lreplace $PNTTAG($image,$object) $i $i]

# Indicate that we will need to re-save the output images.
      set RESAVE 1

# Indicate that the mappings based on this position list will need to be
# re-calculated (but not if we are modifying a mask - the mappings are
# based on feature positions, not mask positions).
      if { $object == $O_RAY_FEATURES || $object == $E_RAY_FEATURES } {
         set RECALC_OEMAP($image) 1
         set RECALC_IMMAP($image) 1

# If the current image is the first (reference) image, then all image
# mappings will need to be re-calculated, because all mappings go to the
# reference image.
         if { $image != [lindex $IMAGES 0] } {
            foreach im $IMAGES {
               set RECALC_IMMAP($im) 1
            }
         }
      }

# Decrement the length of the list.
      incr size -1

# References to positions indices higher than the one just deleted now need
# to be reduced by one.
      for {set j 0} {$j < $size} {incr j} {
         set l [lindex $PNTNXT($image,$object) $j]
         if { $l > $i } {
            set l [expr $l - 1]
            set PNTNXT($image,$object) [lreplace $PNTNXT($image,$object) $j $j $l]
         }
      }

# If the supplied position was connected to another (different)
# position, delete that one if required.
      if { $all } {
         if { $k != "" && $k != $i && $k != -1 } {
            if { $k > $i } { set k [expr $k - 1] }
            DelPosn $k 1 $image $object
         }

      } {

# If the deleted position was part of a polygon with less than 4 vertices,
# delete the entire polygon. If there was only a single vertex in the
# polygon, then it will already have been deleted.
         if { $next != "" } {
            if { $nvert > 1 && $nvert < 4 } {
               DelPosn $next 1 $image $object
            }
         }
      }
   }

# If we have just deleted an entire polygon , switch back to mode 1.
   if { $all } { SetMode 1 }

}

proc DescMap {map} {
#+
#  Name:
#     DescMap
#
#  Purpose:
#     Find the coefficients describing a mapping, returning them as a
#     list, and storing them in global array MAP_C.
#
#  Arguments:
#     The string "ref", or a list of 6 parameter values.
#
#  Returned Value:
#     A list of 6 coeffecients describing the linear mapping, or a null
#     string if the mapping cannot be analysed. or "ref" for a unit
#     mapping.
#
#  Globals:
#     MAP_C (Write)
#        An array of 6 coefficients describing the linear mapping as:
#            XX = C1 + C2*X + C3*Y
#            YY = C4 + C5*X + C6*Y
#-
   global MAP_C

# Assume failure.
   set ok 0
   set MAP_C(1) ""
   set MAP_C(2) ""
   set MAP_C(3) ""
   set MAP_C(4) ""
   set MAP_C(5) ""
   set MAP_C(6) ""
   set ret ""

# If the supplied mapping refers to the reference image, return
# a unit mapping.
   if { $map == "ref" } {
      set MAP_C(1) 0.0
      set MAP_C(2) 1.0
      set MAP_C(3) 0.0
      set MAP_C(4) 0.0
      set MAP_C(5) 0.0
      set MAP_C(6) 1.0

      set ret "ref"
      set ok 1

# If the supplied mapping, is a 6 parameter list, copy the values to MAP_C.
  } elseif { [llength $map] == 6 } {
      set MAP_C(1) [lindex $map 0]
      set MAP_C(2) [lindex $map 1]
      set MAP_C(3) [lindex $map 2]
      set MAP_C(4) [lindex $map 3]
      set MAP_C(5) [lindex $map 4]
      set MAP_C(6) [lindex $map 5]

      set ret $map
      set ok 1

# Otherwise, report an error.
   } elseif { $map != "" } {
      Message "Polka: unsupported map value $map supplied to DescMap."
   }

   if { !$ok } {
      set ret ""
      set MAP_C(1) ""
      set MAP_C(2) ""
      set MAP_C(3) ""
      set MAP_C(4) ""
      set MAP_C(5) ""
      set MAP_C(6) ""
   }

   return $ret
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

proc DrawGwm {} {
#+
#  Name:
#    DrawGwm
#
#  Purpose:
#    Display a section of an image, and store global values which can
#    be used to convert from normalised device coordinates to NDF
#    pixel coordinates.
#
#  Arguments:
#    None.
#
#  Globals:
#    CX (Write)
#       The X offset for converting from canvas
#       coordinates to NDF pixel coordinates.
#    CY (Write)
#       The Y offset for converting from canvas
#       coordinates to NDF pixel coordinates.
#    DMIN (Read and Write)
#       The value added on to the data before taking the log in order
#       to ensure that the data values are positive. An array with a value
#       for each image.
#    IMAGE_DISP (Write)
#       The name of the displayed unscaled base image (with no section
#       specifier).
#    IMSEC_DISP (Write)
#       The displayed unscaled base image section (as supplied,
#       potentially with a section specifier).
#    IMSEC_REQ (Read)
#       The unscaled base image section to be displayed. The string
#       includes an NDF section specifier expressed in standard form (i.e.
#       as two ranges of pixel indices).
#    MX (Write)
#       The X scale factor for converting from canvas
#       coordinates to NDF pixel coordinates.
#    MY (Write)
#       The Y scale factor for converting from canvas
#       coordinates to NDF pixel coordinates.
#    PHI_DISP (Write)
#       The lower percentile used to display the image.
#    PLO_DISP (Write)
#       The lower percentile used to display the image.
#    PHI_REQ (Read)
#       The requested lower percentile.
#    PLO_REQ (Read)
#       The requested lower percentile.
#    SECTION_REQ (Read and Write)
#       The requested image section (eg "(10:200,23:68)" ).
#    SECTION_DISP (Write)
#       The displayed image section (eg "(10:200,23:68)" ).
#    SECTION_STACK (Write)
#       A stack of displayed image sections.
#    VIEW (Read)
#       Specifies how to choose the section to be displayed when a new
#       image is selected:
#
#       If VIEW=UNZOOMED then the whole image is displayed when a new
#       image is selected.
#
#       If VIEW=ZOOMED then only the current section from the new image
#       is displayed.
#
#-
   global CX
   global CY
   global DEVICE
   global DMIN
   global EFFECTSMENU
   global EFFECTS_STACK
   global IMAGE_DISP
   global IMAGE_PREV
   global IMAGE_STACK
   global IMAGES
   global IMSEC_DISP
   global IMSEC_REQ
   global LOCK_SCALE
   global MX
   global MY
   global PHI_DISP
   global PHI_REQ
   global PLO_DISP
   global PLO_REQ
   global SCAHIGH
   global SCALOW
   global SECTION_DISP
   global SECTION_REQ
   global SECTION_STACK
   global SIZE
   global UNZOOM
   global VIEW

# Tell the user what is happening.
   set told [SetInfo "Displaying the image. Please wait... " 0]

# Set a flag to indicate that the image has not yet been displayed.
   set ok 0

# If a new image is being displayed...
   if { $IMSEC_REQ != $IMSEC_DISP } {

# Extract the image name and section string from the requested image
# section string. Save the old displayed image first.
      set IMAGE_PREV $IMAGE_DISP
      GetSec $IMSEC_REQ IMAGE_DISP section0
      set section0 [ScreenSec $section0]

# Transform this section to take account of any effects currently applied to
# the selected image.
      set map [TotalMap $IMAGE_DISP]
      if { $map != "ref" && $map != "" } {
         set section0 [TranSec $section0 $map 0]
      }

# If we are viewing the unzoomed images, or if this is the first image
# to be displayed, display the whole image as supplied (i.e unzoomed),
# and clear the section stack.
      if { $VIEW == "Unzoomed" || $IMAGE_PREV == "" } {
         set SECTION_REQ $section0
         set SECTION_STACK ""

# Otherwise, the new image is drawn at the same zoom factor as the old
# image, and inherits the old section stack.
      } {

# Replace the bottom entry on the section stack with a section which
# will result in the whole image being displayed if the UnZoom button
# is pressed repeatedly.
         if { $section0 != $SECTION_REQ } {
            if { [llength $SECTION_STACK] > 0 } {
               set SECTION_STACK [lreplace $SECTION_STACK end end $section0]
            } {
               Push SECTION_STACK $section0
            }
         } {
            set SECTION_STACK ""
         }

# Replace the current section, and each of the sections on the section stack
# with a section with the same zoom and centre, but which fills the
# entire GWM display area.
         set SECTION_REQ [ScreenSec $SECTION_DISP]

         set newstack ""
         for {set i 0} {$i < [llength $SECTION_STACK]} {incr i} {
            lappend newstack [ScreenSec [lindex $SECTION_STACK $i] ]
         }
         set SECTION_STACK $newstack

      }

# If the section stack is empty, disable the UnZoom button. Otherwise,
# enable it.
      if { [llength $SECTION_STACK] == 0 } {
         $UNZOOM configure -state disabled
      } {
         $UNZOOM configure -state normal
      }

# Set the text to display in the status area describing the sky subtraction
# method.
      SkyOff

   }

# Get the image to be displayed.
   set data [Top IMAGE_STACK($IMAGE_DISP)]

# Combine the transformed image with the required section identifier.
   append data $SECTION_REQ

# Get a string describing the pixel coordinates to put at at the centre
# of the display. This is just the centre of the supplied section. Report
# an error if the centre cannot be found.
   set centre [SecCen $SECTION_REQ]
   if { $centre == "" } {
      Message "String \"$SECTION_REQ\" is not a valid section."
   } {
      set cx [lindex $centre 0]
      set cy [lindex $centre 1]
      set centre "$cx,$cy"

# Decide on the scaling to use. Check for the user having locked the
# scaling. If the data range being very low, use "flash" mode (otherwise
# DISPLAY will report an error), otherwise use the specified percentiles.
      if { $LOCK_SCALE && [info exists SCALOW] } {
         set pars "mode=scale low=$SCALOW high=$SCAHIGH"

      } {
         if { [Obey kappa stats "ndf=$data"] } {
            set ngood [GetParamED kappa stats:numgood]
            if { $ngood > 2 } {
               set maxm [GetParamED kappa stats:maximum]
               set minm [GetParamED kappa stats:minimum]
               puts "DATA $data"
               puts "MAXM $maxm"
               puts "MINM $minm"

               if { abs( $maxm/2.0 - $minm/2.0 ) <
                    2.0E-4 * ( abs($maxm)/2.0 + abs($minm)/2.0 ) } {
                  set pars "mode=flash"
                  set scalow $minm
                  set scahigh $maxm
               } {
                  set pars "mode=perc percentiles=\[$PLO_REQ,$PHI_REQ\]"
               }

            } {
               set pars "mode=scale low=0 high=1"
            }
         }
      }

# Display the image section centred correctly.
      if { [Obey kapview display "in=\"$data\" $pars badcol=0 device=$DEVICE \
                                  axes=no centre=\"$centre\"" ] } {

# Indicate that the image has been displayed.
         set ok 1

# Get the used scaling limits (replace D exponents with E). If the data
# range was too small to display, use the values established earlier.
         if { $pars != "mode=flash" } {
            set scalow [GetParamED kapview display:scalow]
            set scahigh [GetParamED kapview display:scahigh]
         }
         set SCALOW [format "%.5g" $scalow]
         set SCAHIGH [format "%.5g" $scahigh]

# Use picin to get the bounds of the DATA picture just created in
# normalised device coordinates and NDF pixels. These NDC values extend
# from 0 to 1 on both axes (and in general are therefore not square).
         Obey kapview picin "device=$DEVICE current=yes name=DATA frame=ndc" 1
         set ncx1 [GetParamED kapview picin:x1]
         set ncx2 [GetParamED kapview picin:x2]
         set ncy1 [GetParamED kapview picin:y1]
         set ncy2 [GetParamED kapview picin:y2]

         Obey kapview picin "device=$DEVICE current=yes name=DATA frame=pixel" 1
         set wcx1 [GetParamED kapview picin:x1]
         set wcx2 [GetParamED kapview picin:x2]
         set wcy1 [GetParamED kapview picin:y1]
         set wcy2 [GetParamED kapview picin:y2]

# Calculate the offsets and scaling factors for converting from canvas
# coordinates to NDF pixels.
         set cx1 [expr $ncx1 * ( $SIZE - 1 )]
         set cx2 [expr $ncx2 * ( $SIZE - 1 )]
         set cy1 [expr $SIZE * ( 1.0 - $ncy1 )]
         set cy2 [expr $SIZE * ( 1.0 - $ncy2 )]

         set MX [expr ( $wcx2 - $wcx1 ) / ( $cx2 - $cx1 ) ]
         set CX [expr $wcx1 - $MX * $cx1]
         set MY [expr ( $wcy2 - $wcy1 ) / ( $cy2 - $cy1 ) ]
         set CY [expr $wcy1 - $MY * $cy1]

# Indicate that the requested image has been displayed.
         set IMSEC_DISP $IMSEC_REQ
         set SECTION_DISP $SECTION_REQ
         set PHI_DISP $PHI_REQ
         set PLO_DISP $PLO_REQ
      }
   }

# If the image was not displayed, clear the GWM display.
   if { !$ok } {
      ClearGwm
   }

# Set the state of the effects undo buttons.
   if { [llength $EFFECTS_STACK($IMAGE_DISP)] > 0 } {
      $EFFECTSMENU entryconfigure "Undo" -state normal
      $EFFECTSMENU entryconfigure "Undo All" -state normal
   } {
      $EFFECTSMENU entryconfigure "Undo" -state disabled
      $EFFECTSMENU entryconfigure "Undo All" -state disabled
   }

# Cancel the informative text set earlier in this procedure.
   if { $told } { SetInfo "" 0 }

}

proc DrawPosns {ref reg args} {
#+
#  Name:
#     DrawPosns
#
#  Purpose:
#     Draws the canvas markers and vectors for a set of positions.
#
#  Arguments:
#     ref
#        If non-zero, then the objects are drawn as reference objects,
#        otherwise they are drawn as current objects.
#     reg
#        Should the positions be mapped into the frame of the current
#        objects? This is only used if "ref" is non-zero.
#     args
#        An optional list argument holding the image from which the
#        positions are derived, and the type of objects to be drawn.
#        If these are not supplied, they default to $IMAGE_DISP and
#        $CUROBJ_REQ.
#
#  Globals:
#     CAN (Read)
#        Path to the canvas containing the GWM image display.
#     CUROBJ_REQ (Read)
#        The type of the current objects to be displayed.
#     IMAGE_DISP (Read)
#        The displayed image (without section).
#     PNTCX (Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas X coordinates.
#     PNTCY (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas Y coordinates.
#     PNTID (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas item identifiers associated with the
#        positions in the list. A value of -1 indicates that no marker is
#        currently drawn for the position.
#     PNTLBL (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of labels associated with the positions in the list.
#     PNTNXT (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of integers representing indices within the lists
#        given by PNTxxx. Each integer gives the index of the next position
#        along the edge of a polygon. The vector starting at position
#        index i, ends at position index given by the i'th element of
#        PNTNXT. If this value is blank ("") then position i is not part of
#        a polygon. If this value is -1, then the end of the vector starting
#        position i is unspecified (in this case the polygon is not
#        closed).
#     PNTPX (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel X coordinates.
#     PNTPY (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel Y coordinates.
#     PNTTAG (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas tags (one for each position).
#     PNTVID (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas item identifiers associated with the
#        vectors between positions in the list. A value of -1 indicates that
#        no line is currently drawn for the position. A blank string
#        indicates that no vector is defined.
#     V0 (Write)
#        The index of the position corresponding to the first vertex in
#        an incomplete (i.e. open) polygon.
#     V1 (Write)
#        The index of the position corresponding to the last vertex in
#        an incomplete (i.e. open) polygon.
#     VCX0 (Write)
#        The canvas X coordinates at the first vertex in an incomplete
#        (i.e. open) polygon.
#     VCY0 (Write)
#        The canvas Y coordinates at the first vertex in an incomplete
#        (i.e. open) polygon.
#     VID2 (Write)
#        The canvas item id of the vector joining the last vertex in an
#        incomplete (i.e. open) polygon, to the pointer.
#
#  Notes:
#     -  If an incomplete (i.e. open) polygon is drawn, then the GWM
#     interaction mode is set to 2 ("Complete an incomplete polygon").
#-
   global CAN
   global CURCOL
   global CUROBJ_REQ
   global IMAGE_DISP
   global PNTCX
   global PNTCY
   global PNTID
   global PNTLBL
   global PNTNXT
   global PNTPX
   global PNTPY
   global PNTTAG
   global PNTVID
   global REFCOL
   global V0
   global V1
   global VCX0
   global VCY0
   global VID2

# Store the image and object type.
   if { $args == "" } {
      set image $IMAGE_DISP
      set object $CUROBJ_REQ
   } {
      set image [lindex $args 0]
      set object [lindex $args 1]
   }

# Do nothing if the list is empty.
   if { [info exists PNTID($image,$object)] } {
      set size [llength $PNTID($image,$object)]
      if { $size > 0 } {

# Store the colour to use, and get the pixel coordinates to use.
# Reference positions may be mapped into the frame of the current objects
# (if the user has selcted the relevant option), but current objects are
# always displayed at their specified pixel coordinates (i.e. without
# mapping).
         if { $ref } {
            set vtag "rvectors"
            set colour $REFCOL
            if { $reg } {
               MapRefs pxlist pylist
            } {
               set pxlist $PNTPX($image,$object)
               set pylist $PNTPY($image,$object)
            }
         } {
            set vtag "vectors"
            set colour $CURCOL
            set pxlist $PNTPX($image,$object)
            set pylist $PNTPY($image,$object)
         }

# Convert the pixel coordinates to canvas coordinates.
         set PNTCX($image,$object) ""
         set PNTCY($image,$object) ""

         for {set i 0} {$i < $size} {incr i} {
            set px [lindex $pxlist $i]
            set py [lindex $pylist $i]
            set cxy [NDFToCan $px $py]
            if { $cxy == "" } { return }
            lappend PNTCX($image,$object) [lindex $cxy 0]
            lappend PNTCY($image,$object) [lindex $cxy 1]
         }

# Now loop round them all again, drawing any vectors.
         for {set i 0} {$i < $size} {incr i} {

# Get the index of the position at the "far end" of the vector (if any)
# which starts at the current position.
            set next [lindex $PNTNXT($image,$object) $i]

# If this value is not null, there is a vector.
            if { $next != "" } {

# If there is a vector to be drawn, get the canvas coordinates at the
# current (starting) position.
               set cx [lindex $PNTCX($image,$object) $i]
               set cy [lindex $PNTCY($image,$object) $i]

# A value of -1 for $next indicates that the vector end should be bound to
# the pointer. We need some extra information in global to do this...
               if { $next == -1 } {

# We ignore incomplete reference masks (i.e. we don't bind the last vertex
# to the pointer).
                  if { !$ref } {

# Find the index (V0) of the position at the start of the incomplete polygon.
# This is a positions which does not have any other position pointing to
# it.
                     set j $i
                     while { $j < $size } {

                        set V0 $j
                        set j 0

                        while { $j < $size && [lindex $PNTNXT($image,$object) $j] != $V0 } {
                           incr j
                        }

                     }

# Store the canvas coordinates at this position.
                     set VCX0 [lindex $PNTCX($image,$object) $V0]
                     set VCY0 [lindex $PNTCY($image,$object) $V0]

# Initially make the vector zero length.
                     set cxn $cx
                     set cyn $cy

# Enter GWM interaction mode 2 ("complete an unfinished mask polygon").
                     SetMode 2
                  }

# Any value other than -1 for $next indicates that the vector ends at the
# position with index $next. Get the canvas coords of that position.
               } {
                  set cxn [lindex $PNTCX($image,$object) $next]
                  set cyn [lindex $PNTCY($image,$object) $next]
               }

# If a canvas item marking the vector has not already been created,
# create one now, and store its index.
               set vid [lindex $PNTVID($image,$object) $i]
               set tag [lindex $PNTTAG($image,$object) $i]
               if { $vid == -1 } {
                  set vid [$CAN create line $cx $cy $cxn $cyn -fill $colour -tags [list $vtag $tag]]
                  set PNTVID($image,$object) [lreplace $PNTVID($image,$object) \
                                                       $i $i $vid]

# If a canvas item already exists, configure it to have the correct
# properties.
               } {
                  $CAN coords $vid $cx $cy $cxn $cyn
                  $CAN itemconfigure $vid -fill $colour -tags [list $vtag $tag]
               }

# If this is the "loose" end of an incomplete mask polygon, store the
# list index of the position, and the canvas item id of the vector in
# global.
               if { $next == -1 && !$ref } {
                  set VID2 $vid
                  set V1 $i
               }
            }
         }

# Now loop round them all again, drawing the markers over the top of the
# vectors (so that the vectors can be picked up easily in MODE 1).
         for {set i 0} {$i < $size} {incr i} {

# Get the canvas coordinates at the position.
            set cx [lindex $PNTCX($image,$object) $i]
            set cy [lindex $PNTCY($image,$object) $i]

# Isolated positions (indicated by a null NXT value) are taken to be image
# "features". Positions which are connected to other positions are taken to
# be mask "vertices". Draw the appropriate marker.
            set next [lindex $PNTNXT($image,$object) $i]
            if { $next == "" } {
               MarkPosn $i $ref 0 ""
            } {
               MarkPosn $i $ref 1 ""
            }
         }
      }
   }
}

proc DrawCur {} {
#+
#  Name:
#     DrawCur
#
#  Purpose:
#     Draw the current object markers on the GWM canvas.
#
#  Arguments:
#     None
#
#  Globals:
#     CUROBJ_DISP (Read and Write)
#        The type of current object currently displayed.
#     REFOBJ_REQ (Read)
#        The requested type of current object to be displayed.
#     TEST_ID (Read)
#       The canvas identifier for the current "candidate feature marker". Set
#       blank if there is no candidate feature.
#-
   global CUROBJ_DISP
   global CUROBJ_REQ
   global E_RAY_MASK
   global E_RAY_SKY
   global IMAGE_DISP
   global NONE
   global O_RAY_MASK
   global O_RAY_SKY
   global TEST_ID

# If a mask (O,E or sky) has been requested, endeavour to ensure that
# the mask exists. If a mask does not already exist, then an attempt is
# made to create one from the other defined masks and mappings, on the
# assumption that all masks should be roughly the same.
   if { $CUROBJ_REQ == $O_RAY_MASK || $CUROBJ_REQ == $E_RAY_MASK } {
      CreateMask $IMAGE_DISP $CUROBJ_REQ
   }

   if { $CUROBJ_REQ == $O_RAY_SKY || $CUROBJ_REQ == $E_RAY_SKY } {
      CreateSky $IMAGE_DISP $CUROBJ_REQ
   }

# Only draw the objects if the type is not "None".
   if { $CUROBJ_REQ != $NONE } {
      DrawPosns 0 0
   }

# If the image was updated while waiting for a label for a candidate
# feature, redraw the circle which marks the candidate feature.
   if { $TEST_ID != "" } {
      TestFea
   }

# Indicate that the requested values are now displayed.
   set CUROBJ_DISP $CUROBJ_REQ

# Re-configure the reference objects to use the new mapping.
   DrawRef
}

proc DrawRef {} {
#+
#  Name:
#     DrawRef
#
#  Purpose:
#     Draw the reference markers on the GWM canvas.
#
#  Arguments:
#     None
#
#  Globals:
#     REFIM_DISP (Read and Write)
#        The image from which the currently displayed reference objects
#        are derived.
#     REFIM_REQ (Read)
#        The requested image from which the displayed reference objects
#        should be derived.
#     REFOBJ_DISP (Read and Write)
#        The type of reference objects currently displayed.
#     REFOBJ_REQ (Read)
#        The requested type of reference objects to be displayed.
#     REFALN (Read)
#        The value of the "Draw Aligned" checkbutton.
#-
   global E_RAY_MASK
   global E_RAY_SKY
   global NONE
   global O_RAY_MASK
   global O_RAY_SKY
   global REDRAW
   global REFALIGN
   global REFALN
   global REFIM_DISP
   global REFIM_REQ
   global REFOBJ_DISP
   global REFOBJ_REQ

# Only draw the objects if the type is not "None".
   if { $REFOBJ_REQ != $NONE } {

# Enable the "Draw Aligned" check button.
      $REFALIGN configure -state normal

# If ref. objects are being drawn aligned, enable the Redraw button.
      if { $REFALN } {
         $REDRAW configure -state normal
      } {
         $REDRAW configure -state disabled
      }

# If a reference mask (O, E or sky) has been requested, endeavour to ensure
# that the mask exists. If a mask does not already exist, then an attempt is
# made to create one from the other defined masks and mappings, on there
# assumption that all masks should be roughly the same.
      if { $REFOBJ_REQ == $O_RAY_MASK || $REFOBJ_REQ == $E_RAY_MASK } {
         CreateMask $REFIM_REQ $REFOBJ_REQ
      }

      if { $REFOBJ_REQ == $O_RAY_SKY || $REFOBJ_REQ == $E_RAY_SKY } {
         CreateSky $REFIM_REQ $REFOBJ_REQ
      }

# Draw the markers.
      DrawPosns 1 $REFALN $REFIM_REQ $REFOBJ_REQ

# If no reference objects are being drawn, disabled the "Draw Aligned"
# and "Re-draW" buttons.
   } {
      $REFALIGN configure -state disabled
      $REDRAW configure -state disabled
   }

# Indicate that the requested values are now displayed.
   set REFOBJ_DISP $REFOBJ_REQ
   set REFIM_DISP $REFIM_REQ

}

proc Dump {file} {
#+
#  Name:
#     Dump
#
#  Purpose:
#     Dump the current positions lists, masks, and options to a text file
#     which can be restored later (using procedure Restore).
#
#  Arguments:
#     file
#        If supplied non-blank, then the dump is written to the specified
#        file (any existing file with the same name is over-written).
#        Otherwise, the user is asked to supply a file name.
#
#
#  Returned Value:
#     1 if a succesful dump was performed, zero otherwise.
#
#-
   global E_RAY_FEATURES
   global E_RAY_MASK
   global IMAGES
   global O_RAY_FEATURES
   global O_RAY_MASK
   global O_RAY_SKY
   global E_RAY_SKY
   global IMMAP
   global OEMAP
   global PNTPX
   global PNTPY
   global PNTNXT
   global PNTLBL
   global PNTTAG
   global PROT_OEMAP
   global PROT_IMMAP

# Assume no dump is made.
   set ret 0

# Open the supplied file, or open a file specified by the user.
   if { $file != "" } {
      if { [catch { set fd [open $file "w"]} ] } {
         set ok 0
      } {
         set ok 1
      }

   } {
      set ok [OpenFile "w" "Dump output file" \
                           "Give name of dump file to create:" file fd]
   }

# Only proceed if a file was opened.
   if { $ok } {

# Tell the user what is happening.
      set told [SetInfo "Dumping current positions lists, etc, to disk..." 0]

# Loop round all images...
      foreach image $IMAGES {
         puts $fd "Image $image"

# Write the OE Mapping to the output file.
         if { [info exists OEMAP($image)] && [llength $OEMAP($image)] == 6 } {
            puts $fd $OEMAP($image)
         } {
            puts $fd ""
         }

# Write the image Mapping to the output file.
         if { [info exists IMMAP($image)] && [llength $IMMAP($image)] == 6 } {
            puts $fd $IMMAP($image)
         } {
            puts $fd ""
         }

# Dump the mapping protection flags.
         if { [info exists PROT_OEMAP($image)] } {
            puts $fd $PROT_OEMAP($image)
         } {
            puts $fd ""
         }

         if { [info exists PROT_IMMAP($image)] } {
            puts $fd $PROT_IMMAP($image)
         } {
            puts $fd ""
         }


# Loop round each object type...
         foreach object [list $O_RAY_FEATURES $E_RAY_FEATURES $O_RAY_MASK \
                              $E_RAY_MASK $O_RAY_SKY $E_RAY_SKY] {

# Write out the number of posisions in this list.
            puts $fd [llength $PNTPX($image,$object)]

# Write out the arrays holding information describing the list.
            puts $fd $PNTPX($image,$object)
            puts $fd $PNTPY($image,$object)
            puts $fd $PNTNXT($image,$object)
            puts $fd $PNTLBL($image,$object)
            puts $fd $PNTTAG($image,$object)
         }
      }

# Save the names and values of the global variables holding the current
# option settings...
      puts $fd "Options:"
      foreach var "XHRCOL CURCOL BADCOL REFCOL SELCOL PSF_SIZE INTERP FITTYPE OEFITTYPE SKYPAR VIEW XHAIR SKYOFF HAREA SAREA" {
         upvar #0 $var gvar
         puts $fd [list $var $gvar]
      }

# Close the output dump file.
      close $fd

# Indicate a dump was made.
      set ret 1

# Cancel the informative text set earlier in this procedure.
      if { $told } { SetInfo "" 0 }

   }

   return $ret
}

proc DumpImage {file} {
#+
#  Name:
#     DumpImage
#
#  Purpose:
#     Dump the current positions list and masks for the displayed image to a
#     text file which can be restored later (using procedure Restore).
#
#  Arguments:
#     file
#        If supplied non-blank, then the dump is written to the specified
#        file (any existing file with the same name is over-written).
#        Otherwise, the user is asked to supply a file name.
#
#
#  Returned Value:
#     1 if a succesful dump was performed, zero otherwise.
#
#-
   global E_RAY_FEATURES
   global E_RAY_MASK
   global IMAGE_DISP
   global O_RAY_FEATURES
   global O_RAY_MASK
   global O_RAY_SKY
   global E_RAY_SKY
   global PNTPX
   global PNTPY
   global PNTNXT
   global PNTLBL
   global PNTTAG

# Assume no dump is made.
   set ret 0

# Open the supplied file, or open a file specified by the user.
   if { $file != "" } {
      if { [catch { set fd [open $file "w"]} ] } {
         set ok 0
      } {
         set ok 1
      }

   } {
      set ok [OpenFile "w" "Dump output file" \
                           "Give name of dump file to create:" file fd]
   }

# Only proceed if a file was opened.
   if { $ok } {

# Tell the user what is happening.
      set told [SetInfo "Dumping image positions list and masks to disk..." 0]

# Loop round all images...
      set image $IMAGE_DISP

# Write out a description of the file contents.
      puts $fd "Polka image dump"
      puts $fd "Image: $image"

# Loop round each object type...
      foreach object [list $O_RAY_FEATURES $E_RAY_FEATURES $O_RAY_MASK \
                           $E_RAY_MASK $O_RAY_SKY $E_RAY_SKY] {

# Write out the number of posisions in this list.
         puts $fd [llength $PNTPX($image,$object)]

# Write out the arrays holding information describing the list.
         puts $fd $PNTPX($image,$object)
         puts $fd $PNTPY($image,$object)
         puts $fd $PNTNXT($image,$object)
         puts $fd $PNTLBL($image,$object)
         puts $fd $PNTTAG($image,$object)
      }

# Close the output dump file.
      close $fd

# Indicate a dump was made.
      set ret 1

# Cancel the informative text set earlier in this procedure.
      if { $told } { SetInfo "" 0 }

   }

   return $ret
}

proc DumpMask {file} {
#+
#  Name:
#     DumpMask
#
#  Purpose:
#     Dump the current O and E ray masks from the first (reference) image, to
#     a text file which can be restored later (using procedure RestoreMask).
#
#  Arguments:
#     file
#        If supplied non-blank, then the dump is written to the specified
#        file (any existing file with the same name is over-written).
#        Otherwise, the user is asked to supply a file name.
#
#
#  Returned Value:
#     1 if a succesful dump was performed, zero otherwise.
#
#-
   global DBEAM
   global IMAGES
   global E_RAY_MASK
   global O_RAY_MASK
   global PNTPX
   global PNTPY
   global PNTNXT
   global PNTLBL
   global PNTTAG

# Assume no dump is made.
   set ret 0

# Indicate that no dump has yet been specified.
   set ok 0

# Store the name of the first (reference) image.
   set im0 [lindex $IMAGES 0]

# If this procedure was entered in order to create a backup dump for
# RestoreMask ( i.e. if a file was specified in the argument list)...
   if { $file != "" } {

# Open the supplied file without checking that there is anything to dump.
      if { ![catch { set fd [open $file "w"]} ] } {
         set ok 1
      }

# Otherwise, check that there is something to dump.
   } {

# Check that an O-ray mask is available for the reference image.
      if { ![CreateMask $im0 $O_RAY_MASK] } {
         Message "The mask defining the O-ray areas in $im0 has not yet been supplied."

# If we are in dual-beam mode, check that the E-ray mask is available for the
# reference image
      } elseif { ![CreateMask $im0 $E_RAY_MASK] && $DBEAM } {
         Message "The mask defining the E-ray areas in $im0 has not yet been supplied."

# If these checks were passed, open a file specified by the user.
      } {
         set ok [OpenFile "w" "Dump output file" \
                              "Give name of dump file to create:" file fd]
      }
   }

# Only proceed if a file was opened.
   if { $ok } {

# Tell the user what is happening.
      set told [SetInfo "Dumping current masks to disk..." 0]

# Store a string in the text file indicating that it only contains a mask.
      puts $fd "Masks only"

# Loop round each mask...
      foreach object [list $O_RAY_MASK $E_RAY_MASK] {

# Write out the number of positions in this mask list.
         puts $fd [llength $PNTPX($im0,$object)]

# Write out the arrays holding information describing the list.
         puts $fd $PNTPX($im0,$object)
         puts $fd $PNTPY($im0,$object)
         puts $fd $PNTNXT($im0,$object)
         puts $fd $PNTLBL($im0,$object)
         puts $fd $PNTTAG($im0,$object)
      }

# Close the output dump file.
      close $fd

# Indicate a dump was made.
      set ret 1

# Cancel the informative text set earlier in this procedure.
      if { $told } { SetInfo "" 0 }

   }

   return $ret
}

proc EditMapping {image mapping} {
#+
#  Name:
#     EditMapping
#
#  Purpose:
#     Allows the user to examine and edit the coefficients describing
#     a mapping.
#
#  Arguments:
#     image
#       The image with which the mapping is associated.
#     mapping
#       If "im" then the image mapping associated with image "$image is
#       displayed. Otherwise, the OE mapping associated with the image is
#       displayed. Ignored if "$image" is null.
#
#  Globals:
#     B_FONT (Read)
#        The default font used for buttons.
#     CB_COL (Read)
#        The colour to use for the check buttons in the dialog box.
#     EDMAP_EXIT (Write)
#        Used to communicate with the buttons in the dialog box. It holds
#        the label of the most recently pressed button.
#     FITTYPE (Read)
#        A textual description of the mapping to be used for the image mapping.
#     IMAGES (Read)
#        A list of the input images (without any section specifiers).
#     IMMAP (Read and Write)
#        An array of lists (one for each image), each giving the mapping
#        from the supplied image to the reference (first) image. Each
#        mapping is a list of 6 values representing a general linear
#        transformation.
#     INV (Write)
#        Is the inverse mapping displayed?
#     MAP_C (Write)
#        An array of 6 coefficients describing the linear mapping as:
#            XX = C1 + C2*X + C3*Y
#            YY = C4 + C5*X + C6*Y
#     MAP_MX (Write)
#        The magnification of the rotated X axis implied by the values of
#        MAP_C.
#     MAP_MY (Write)
#        The magnification of the rotated Y axis implied by the values of
#        MAP_C.
#     MAP_RX (Write)
#        The anti-clockwise rotation of the original X axis implied by the
#        values of MAP_C (in degrees).
#     MAP_RY (Write)
#        The anti-clockwise rotation of the original X axis implied by the
#        values of MAP_C (in degrees).
#     MAP_SX  (Write)
#        The shift of the magnified X axis implied by the values of MAP_C.
#     MAP_SY  (Write)
#        The shift of the magnified Y axis implied by the values of MAP_C.
#     MAPTYPE (Read)
#        A list containing the textual descriptions of the available
#        mapping types.
#     MENUBACK (Read)
#        The background colour for the menu bar in the main Polka window.
#     OEFITTYPE (Read)
#        A textual description of the mapping to be used for the OE mapping.
#     OEMAP (Read and Write)
#        An array of mappings (one for each image), each being a list of
#        6 parameter values giving the linear mapping from E to O ray.
#     PROT_IMMAP (Read and Write)
#        A 1-d array indexed by image. Each element is either "normal" or
#        "disabled", and specified whether the image mapping associated
#        with the image may be changed. A value of "normal" means that the
#        image mapping may be changed; a value of "disabled" means that it
#        may not be changed.
#     PROT_OEMAP (Read and Write)
#        A 1-d array indexed by image. Each element is either "normal" or
#        "disabled", and specified whether the OE mapping associated
#        with the image may be changed. A value of "normal" means that the
#        mapping may be changed; a value of "disabled" means that it
#        may not be changed.
#     RB_COL (Read)
#        The colour to use for the radiobuttons in the dialog box.
#     RB_FONT (Read)
#        The default font used for radiobuttons.
#     RECALC_IMMAP (Read and Write)
#        A 1-d array indexed by image. Each element is a logical flag
#        indicating if the image features for the corresponding image
#        have changed since the image mapping was last found.
#     RECALC_OEMAP (Read and Write)
#        A 1-d array indexed by image. Each element is a logical flag
#        indicating if the image features for the corresponding image
#        have changed since the image's E to O mapping was last found.
#     tcl_precision (Read)
#        The number of significant digits used by tcl when formatting a
#        numerical value (i.e. the result of an expr command).
#
#-
   global B_FONT
   global CB_COL
   global DBEAM
   global EDMAP_EXIT
   global FITTYPE
   global IMAGES
   global IMMAP
   global INV
   global MAP_C
   global MAP_MX
   global MAP_MY
   global MAP_RX
   global MAP_RY
   global MAP_SX
   global MAP_SY
   global MAPTYPE
   global MENUBACK
   global OEFITTYPE
   global OEMAP
   global PROT_IMMAP
   global PROT_OEMAP
   global RB_COL
   global RB_FONT
   global RECALC_IMMAP
   global RECALC_OEMAP
   global RESAVE
   global tcl_precision

# First deal with image mappings.
   if { $mapping == "im" } {

# Attempt to get the image mapping.
      set map [ImageMapping $image]
      set map_var IMMAP($image)
      set rec_var RECALC_IMMAP($image)
      set map_type $FITTYPE

# Set up other required constants.
      set im0 [lindex $IMAGES 0]
      set for_text "Display forward mapping (from \"$image\" to \"$im0\")"
      set inv_text "Display inverse mapping (from \"$im0\" to \"$image\")"

      set pvar PROT_IMMAP($image)
      if { $image == $im0 } {
         set pstate disabled
         set rvstate disabled
         set header_text "This dialog box describes the unit mapping which maps image \"$image\" onto itself. NB, this mapping may not be changed."
      } {
         set pstate normal
         set rvstate $PROT_IMMAP($image)
         set header_text "This dialog box describes the mappings between image \"$image\" and image \"$im0\"."
      }

# Now deal with OE mappings.
   } {
      set map [OEMapping $image]
      set map_var OEMAP($image)
      set rec_var RECALC_OEMAP($image)
      set rvstate $PROT_OEMAP($image)
      set pstate normal
      set header_text "This dialog box describes the mappings between the O and E rays in image \"$image\"."
      set pvar PROT_OEMAP($image)
      set map_type $OEFITTYPE

      set for_text "Display forward mapping (from the E-ray to the O-ray)"
      set inv_text "Display inverse mapping (from the O-ray to the E-ray)"

   }

# Get the coefficient values in the full 6 parameter fit from the mapping.
# Create a warning message if the mapping is not defined.
   if { [DescMap $map] == "" } {
      append header_text "\n\n(Currently undefined)"
   }

# Get the numerical index of the fit type to use when creating new mappings.
   foreach fittype [array names MAPTYPE] {
     if { $MAPTYPE($fittype) == $map_type } { break }
   }

# Find the shifts, rotations and magnifications impled by the 6
# coefficients.
   ConvMap 1 $fittype

# Save the original coefficients in case they need to be restored.
   for {set i 1} {$i<7} {incr i} {
      set old_c($i) $MAP_C($i)
   }

# Determine which of the entry boxes will be read-only, depending on the
# map type. Initially, protect everything.
   for {set i 1} {$i < 7} {incr i} {
      set rv($i) disabled
   }

   set rv(MX) disabled
   set rv(MY) disabled
   set rv(RX) disabled
   set rv(RY) disabled
   set rv(SX) disabled
   set rv(SY) disabled

# Only write-enable anything if the mapping is not protected.
   if { $rvstate != "disabled" } {

# The shift in x and y are writable for all mapping types.
      set rv(SX) normal
      set rv(SY) normal

# For "Shift and rotation" mappings, enable the X axis rotation. The
# Y axis value is forced to mirror the X value.
      if { $fittype == 2 } {
         set rv(RX) normal

# For "Shift and magnification" mappings, enable the X axis magnification. The
# Y axis value is forced to mirror the X value.
      } elseif { $fittype == 3 } {
         set rv(MX) normal

# For "Shift, rotation and magnification" mappings, enable the X axis
# rotation and magnification. The Y axis values are forced to mirror the X
# values.
      } elseif { $fittype == 4 } {
         set rv(RX) normal
         set rv(MX) normal

# For a full 6 parameter fit, everything is writable.
      } elseif { $fittype == 5 } {
         Message "POLKA: fittype 5 is no longer supported (programming error)."
      }
   }

# Create the top level window for the dialogue box.
   set top .edmaps
   set topf [MakeDialog $top "Edit a mapping" 1]

# Create a message describing the mapping. The width of the message cannot
# be specified in characters, so we use the default width initially.
   set fr2a [frame $topf.fr2a -relief ridge -bd 4 ]
   pack $fr2a -side top -pady 4m -ipadx 4m -ipady 4m
   set header [message $fr2a.header -text $header_text -justify center ]

# Find the pixel size of the font used by the Message. Use 10 if the
# font string cannot be parsed.
   set font [$header cget -font]
   if { ![regexp {^-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-([^-]+)} $font \
                  match pixsize] } { set pixsize 10 }

# Find the width (in pixels) of 40 characters, assuming an aspect ratio
# of 1.5 (it would be nice to use the character width from the font
# specification instead of making this assumption, but some fonts have a
# character width of zero!.
   set width [expr ( 40 * $pixsize) / 1.5 ]

# Reconfigure the Message to this width, and pack it.
   $header configure -width $width
   pack $header -side top  -expand 1

# Set the width for the data entry boxes.
   set wid [expr $tcl_precision + 6]

# Create a frame holding a table of shifts, rotations, and magnifications
# on the X and Y axes.
   set fr2 [frame $topf.fr2 -bd 2 -relief raised -background $MENUBACK]
   pack $fr2 -padx 2m -pady 4m -ipadx 2m -ipady 2m -fill x -expand 1

   set fa [frame $fr2.fa -background $MENUBACK]
   pack $fa -side left -fill y
   pack [label $fa.l1 -text " " -width 7 -font $B_FONT -background $MENUBACK] -side top
   pack [label $fa.l2 -text "X" -width 7 -font $B_FONT -background $MENUBACK] \
        [label $fa.l3 -text "Y" -width 7 -font $B_FONT -background $MENUBACK] -side top -fill y -expand 1

   set fc [frame $fr2.fc -background $MENUBACK]
   pack $fc -side left -expand 1
   set fcv1 [RealValue $fc.v1 $wid MAP_RX "ConvMap 0 $fittype" -font $B_FONT -state $rv(RX)]
   set fcv2 [RealValue $fc.v2 $wid MAP_RY "ConvMap 0 $fittype" -font $B_FONT -state $rv(RY)]
   pack [label $fc.l1 -text "Rotation" -width 14 -background $MENUBACK] \
        $fcv1 $fcv2 -side top -fill x -expand 1
   SetHelp $fcv1 ".  The clockwise rotation (in degres) of the X axis produced by the mapping."
   SetHelp $fcv2 ".  The clockwise rotation (in degres) of the Y axis produced by the mapping."

   set fd [frame $fr2.fd -background $MENUBACK]
   pack $fd -side left -expand 1
   set fdv1 [RealValue $fd.v1 $wid MAP_MX "ConvMap 0 $fittype" -font $B_FONT -state $rv(MX)]
   set fdv2 [RealValue $fd.v2 $wid MAP_MY "ConvMap 0 $fittype" -font $B_FONT -state $rv(MY)]
   pack [label $fd.l1 -text "Magnification" -width 14 -background $MENUBACK] $fdv1 $fdv2 -side top -fill x -expand 1
   SetHelp $fdv1 ".  The magnification of the X axis produced by the mapping."
   SetHelp $fdv2 ".  The magnification of the Y axis produced by the mapping."

   set fb [frame $fr2.fb -background $MENUBACK]
   pack $fb -side left -expand 1
   set fbv1 [RealValue $fb.v1 $wid MAP_SX "ConvMap 0 $fittype" -font $B_FONT -state $rv(SX)]
   set fbv2 [RealValue $fb.v2 $wid MAP_SY "ConvMap 0 $fittype" -font $B_FONT -state $rv(SY)]
   pack [label $fb.l1 -text "Shift" -width 14 -background $MENUBACK] $fbv1 $fbv2 -side top -fill x -expand 1
   SetHelp $fbv1 ".  The shift of origin (in pixels) along the X axis produced by the mapping."
   SetHelp $fbv2 ".  The shift of origin (in pixels) along the Y axis produced by the mapping."

# Create a frame holding a table of C1 to C6.
   set fr3 [frame $topf.fr3 -bd 2 -relief raised -background $MENUBACK]
   pack $fr3 -padx 2m -pady 4m -ipadx 2m -ipady 2m -fill x -expand 1 -side top

   set fe [frame $fr3.fe -background $MENUBACK]
   pack $fe -side top -padx 2m -pady 2m -fill y -expand 1
   SetHelp $fe ".  The equation giving the transformed X value (\"XX\") as a function of the original X and Y values."
   set c1 [RealValue $fe.c1 $wid MAP_C(1) "ConvMap 1 $fittype" -font $B_FONT -state $rv(1)]
   set c2 [RealValue $fe.c2 $wid MAP_C(2) "ConvMap 1 $fittype" -font $B_FONT -state $rv(2)]
   set c3 [RealValue $fe.c3 $wid MAP_C(3) "ConvMap 1 $fittype" -font $B_FONT -state $rv(3)]
   pack [label $fe.l1 -text "XX  =  " -width 7 -font $B_FONT -background $MENUBACK] $c1 \
        [label $fe.l2 -text "  +  " -font $B_FONT -background $MENUBACK] $c2 \
        [label $fe.l3 -text " * X  +  " -font $B_FONT -background $MENUBACK] $c3 \
        [label $fe.l4 -text " * Y" -font $B_FONT -background $MENUBACK] -side left -expand 1

   set ff [frame $fr3.ff -background $MENUBACK]
   pack $ff -side top -padx 2m -pady 2m -fill y -expand 1
   SetHelp $ff ".  The equation giving the transformed Y value (\"YY\") as a function of the original X and Y values."
   set c4 [RealValue $ff.c4 $wid MAP_C(4) "ConvMap 1 $fittype" -font $B_FONT -state $rv(4)]
   set c5 [RealValue $ff.c5 $wid MAP_C(5) "ConvMap 1 $fittype" -font $B_FONT -state $rv(5)]
   set c6 [RealValue $ff.c6 $wid MAP_C(6) "ConvMap 1 $fittype" -font $B_FONT -state $rv(6)]
   pack [label $ff.l1 -text "YY  =  " -width 7 -font $B_FONT -background $MENUBACK] $c4 \
        [label $ff.l2 -text "  +  " -font $B_FONT -background $MENUBACK] $c5 \
        [label $ff.l3 -text " * X  +  " -font $B_FONT -background $MENUBACK] $c6 \
        [label $ff.l4 -text " * Y" -font $B_FONT -background $MENUBACK] -side left -expand 1

# Create a frame holding a label describing the type of mapping.
   set fr1a [frame $topf.fr1a -relief groove -bd 2]
   pack $fr1a -padx 2m -pady 3m -fill x -expand 1 -side top -anchor w
   SetHelp $fr1a ".  The type of mapping which can be created by typing new values into the displayed entry boxes.\n(To change this mapping type, use the \"Options\" menu.)"
   pack [label $fr1a.l1 -text "Type for new mappings:" -font $RB_FONT] \
        [label $fr1a.l2 -text $map_type -font $B_FONT] -padx 2m -side left

# Create a frame holding two radio buttons selecting the forward or
# inverse mapping.
   set fr1 [frame $topf.fr1 -relief groove -bd 2]
   pack $fr1 -padx 2m -pady 3m -fill x -expand 1 -side top -anchor w
   SetHelp $fr1 ".  Select the mapping to be edited."

   set INV 0
   set rb1 [radiobutton $fr1.rb1 -text $for_text -variable INV -value 0 \
                              -selectcolor $RB_COL -command "InvMapC $fittype"]

   set rb2 [radiobutton $fr1.rb2 -text $inv_text -variable INV -value 1 \
                              -selectcolor $RB_COL -command "InvMapC $fittype"]

   pack $rb1 $rb2 -side top -padx 2m -pady 1m -anchor w

# Create the OK, Clear, Cancel, Restore, Help and (if required) OEMap
# buttons, but don't pack them yet.
   set butfrm [frame $topf.butfrm]
   set b1 [button $butfrm.ok -text "OK" -command "set EDMAP_EXIT ok"]
   set b2 [button $butfrm.clear -text "Clear" -command "set EDMAP_EXIT clear" -state $rvstate]
   set b3 [button $butfrm.cancel -text "Cancel" -command "set EDMAP_EXIT cancel"]
   set b4 [button $butfrm.restore -text "Restore" -command "set EDMAP_EXIT  restore" -state $rvstate]
   set b5 [button $butfrm.help -text "Help" -command "set EDMAP_EXIT help"]

   SetHelp $b1 ".  Press to close the dialog box, adopting the currently displayed mapping parameters."
   SetHelp $b2 ".  Press to clear the mapping parameters."
   SetHelp $b3 ".  Press to close the dialog box, re-instating the original mapping parameters."
   SetHelp $b4 ".  Press to restore the original mapping."
   SetHelp $b5 ".  Press to see more help on this window."

   if { $mapping == "im" && $DBEAM } {
      set state "normal"
      set help ".  Press to create a new dialog box to edit the O-E mapping associated with this image. This mapping will be used in preference to the default O-E mapping."
   } {
      set state "disabled"
      set help " "
   }

   set b6 [button $butfrm.oemap -text "OEmap" -command "set EDMAP_EXIT oemap" -state $state]
   SetHelp $b6 $help

# Create a frame holding the write protection button, and pack them.
   set fr1b [frame $topf.fr1b -relief groove -bd 2]
   pack $fr1b -padx 2m -pady 3m -fill x -expand 1 -side top -anchor w
   SetHelp $fr1b ".  Check to protect the mapping from future changes."
   set cb1 [checkbutton $fr1b.cb1 -text "Protect Mapping" -variable $pvar \
            -state $pstate -offvalue normal -onvalue disabled \
            -selectcolor $CB_COL -command \
            "$b2 configure -state \$$pvar
             $b4 configure -state \$$pvar
             $fbv1 configure -state \$$pvar
             $fbv2 configure -state \$$pvar
             $fcv1 configure -state \$$pvar
             $fcv2 configure -state \$$pvar
             $fdv1 configure -state \$$pvar
             $fdv2 configure -state \$$pvar
             $c1 configure -state \$$pvar
             $c2 configure -state \$$pvar
             $c3 configure -state \$$pvar
             $c4 configure -state \$$pvar
             $c5 configure -state \$$pvar
             $c6 configure -state \$$pvar"]
   pack $cb1 -padx 2m -pady 1m -anchor w

# Now pack the OK, Clear, Cancel, Restore, Help and (if required) OEMap
# buttons so that they appear at the bottom of the dialog box.
   pack $butfrm -fill x -expand 1
   pack $b1 $b2 $b3 $b4 $b5 -side left -expand 1
   pack $b6 -side left -expand 1

# Ensure that closing the window from the window manager is like pressing
# the Cancel button.
   wm protocol $top WM_DELETE_WINDOW "set EDMAP_EXIT cancel"

# Loop until an exit button is pressed.
   set exit 0
   while { !$exit } {

# Wait for the user to press a button.
      tkwait variable EDMAP_EXIT

# If the cancel button was pressed, exit without changing the stored
# mapping.
      if { $EDMAP_EXIT == "cancel" } {
         set exit 1

# If the OK button was pressed, create a new mapping, and exit.
      } elseif { $EDMAP_EXIT == "ok" } {

# Get the forward mapping.
         if { $INV } {
            set INV 0
            InvMapC $fittype
         }

# If the forward mapping is not defined, delete any existing mapping.
         if {  $MAP_C(1) == "" || $INV } {
            if { [info exists $map_var] } {
               unset $map_var
            }

# Otherwise, see if the the mapping was changed.
         } {
            set changed 0
            for {set i 1} {$i < 7} {incr i} {
               if { $MAP_C($i) != $old_c($i) } {
                  set changed 1
                  break
               }
            }

# If it has, save it, and indicate that we will need to re-save the
# output images.
            if { $changed } {
               if { $MAP_C(1) == 0.0 && $MAP_C(2) == 1.0 && $MAP_C(3) == 0.0 &&
                    $MAP_C(4) == 0.0 && $MAP_C(5) == 0.0 && $MAP_C(6) == 1.0 } {
                  set $map_var "ref"
               } {
                  set $map_var [list $MAP_C(1) $MAP_C(2) $MAP_C(3) $MAP_C(4) $MAP_C(5) $MAP_C(6)]
               }
               set RESAVE 1

            }
         }

# Re-configure the reference objects to use the new mapping.
         DrawRef

# Indicate that the dialog box should be closed.
         set exit 1

# If the Clear button was pressed, clear the values describing the mapping.
      } elseif { $EDMAP_EXIT == "clear" } {
         for {set i 1} {$i < 7} {incr i} {
            set MAP_C($i) ""
         }
         ConvMap 1 $fittype

# If the Restore button was pressed, restore the original values describing
# the forward mapping.
      } elseif { $EDMAP_EXIT == "restore" } {
         for {set i 1} {$i < 7} {incr i} {
            set MAP_C($i) $old_c($i)
         }
         ConvMap 1 $fittype
         set INV 0

# If the Help button was pressed, give more help.
      } elseif { $EDMAP_EXIT == "help" } {
         ShowHelp "POLKA_EDIT_MAPPING_DIALOG"

# If the OEMap button was pressed, exit the loop.
      } elseif { $EDMAP_EXIT == "oemap" } {
         set exit 1
      }
   }

# Destroy the dialog box.
   destroy $top

# If the OEMap button was pressed, allow the image's OE mapping to be
# edited.
   if { $EDMAP_EXIT == "oemap" } {
      EditMapping $image oe
   }
}

proc Effects {im effect nodisp} {
#+
#  Name:
#     Effects
#
#  Purpose:
#     Handle events generated by the "Effects" menu.
#
#  Arguments:
#     im
#        The image to which the effect is to be applied.
#     effect
#        The label of the entry from the Effects menu which was pressed.
#     nodisp
#        If this is non-zero then the results of the effect are not
#        displayed.
#
#  Globals:
#     EFFECTS_MAPPINGS (Read and Write)
#        A 1-d array, indexed by image name. Each element is a list
#        in which each entry gives the mapping introduced by the
#        corresponding effect. This mapping goes from pixel coords in
#        the previous top entry in the effects stack to the new top entry.
#        There is no entry for the supplied image (i.e. the number of
#        entries in this stack is the same as EFFECTS_STACK, i.e. one less
#        than in IMAGE_STACK). Each mapping is described by a list of 6
#        values giving the parameters of a full 2-D linear fit.
#     EFFECTS_STACK (Read and Write)
#        A 1-d array, indexed by image name. Each element is a list
#        containing textual descriptions of the effects applied to the
#        image. These descriptions correspond to the images in the
#        correspsonding element of the IMAGE_STACK list (except that the
#        original image - stored at the bottom of the IMAGE_STACK - does not
#        have a description).
#     FILLCON (Read and Write)
#        Set to "Variable value" if the Fill effect should fill bad pixels
#        using a varying value (KAPPA:FILLBAD). Set to "Constant value" if
#        a constant value (given by FILLVAL) should be used (KAPPA:NOMAGIC).
#     FILLVAL  (Read and Write)
#        The constant value with which the Fill effect should fill bad pixels.
#     FSIZE (Read and Write)
#        The filter size for the Filter effect (in pixels).
#     IMAGE_STACK (Read and Write)
#        A 1-d array index by image name. Each element is a list of file
#        names holding the result of each effect. Each effect operates on
#        images on the top of this stack, and results in a new image being
#        pushed onto the stack. The name of the supplied image section is
#        pre-loaded onto the stack, and is never removed (the Undo and Undo
#        All effects are disabled if the only image left on the stack is the
#        original image).
#     MEXP (Read and Write)
#        The expression used by the Maths effect.
#     PSF_SIZE (Read)
#        The feature size specified by the user.
#     SCAHIGH (Read)
#        The data value corresponding to white in the displayed image.
#     SCALOW (Read)
#        The data value corresponding to black in the displayed image.
#     SKYOFF (Read)
#        Should sky subtraction be performed? If not, then the input image
#        name is returned unchanged.
#     SSIZE (Read and Write)
#        The FWHM of the gaussian used by the Smooth effect (in pixels).
#     THRBAD (Read and Write)
#        Should the Threshold effect replace out-of-bounds pixels with
#        bad values? Otherwise, they are replaced by the limit value.
#     THRHI  (Read and Write)
#        The upper threshold used by the Threshold effect.
#     THRLO  (Read and Write)
#        The lower threshold used by the Threshold effect.
#
#-
   global ALIMG
   global EFFECTS_MAPPINGS
   global EFFECTS_STACK
   global FILLCON
   global FILLVAL
   global FSIZE
   global IMAGE_STACK
   global IMAGES
   global MEXP
   global PSF_SIZE
   global SCAHIGH
   global SCALOW
   global SELECTED_AREA
   global SKYOFF
   global SKYOP
   global SKY_METHOD
   global SKY_FRAME
   global SKYIMS
   global SKYPAR
   global SSIZE
   global THRBAD
   global THRHI
   global THRLO
   global UNZOOM

# Begin a temporary file context.
   set tfc [BeginUF]

# Do nothing if there is no current image.
   if { $im == "" } {
      Message "No image is currenly displayed."

# Otherwise, get the name of the image on the top of the image stack, and
# set a flag indicating that the display should be updated.
   } {
      set image [Top IMAGE_STACK($im)]
      set update 1

# Store the name of the first image.
      set im0 [lindex $IMAGES 0]

# Assume that no new image has been created.
      set file ""
      set desc ""

# The parameters of the linear mapping from any new image to the original
# supplied image (i.e. the image at the bottom of the image stack) are
# stored in the "c" list. Indicate that as yet no mapping has been
# produced.
      set c ""

# Tell the user what is happening.
      set told [SetInfo "Applying the \"$effect\" effect." 0]

#---------------------------------------------------------------
# Align - Resamples the displayed image to align it with another
# specified image using the current mappings. This can only be done
# if the required mappings are available.
      if { $effect == "Align" } {

# Assume we do not need to update the display.
         set update 0

# This effect may not be applied to the first image because the
# first image defines the output cooridnate frame, and the rest of
# Polka assumes it has a unit image mapping.
         if { $im == $im0 } {
            Message "The \"Align\" effect cannot be used on image \"$im0\"."
         } {

# Describe the parameter used to get the name of the image with which the
# displayed image is to be aligned (the "target" image).
            set types(ALIMG) "_CHOICE"
            set labels(ALIMG) "Align \"$im\" with image: "
            set limits(ALIMG) $IMAGES

# Set a default value for the target image name (the first image in the
# supplied list).
            if { ![info exists ALIMG] } {
               set ALIMG [lindex $IMAGES 0]
            }

# Get a value from the user for the target image name.
            if { [GetPars ALIMG types labels limits "Enter Align Parameters" \
                          "POLKA_ALIGN_EFFECT" \
                          ". Enter the parameters needed to perform the selected effect."] } {

# Get the mapping from the target image to the first image. If it is not
# yet defined, warn the user.
               set m1 [ImageMapping $ALIMG]
               if { $m1 == "" } {
                  set m2 ""
                  Message "The image features required to define the mapping between image \"$ALIMG\" and \"[lindex $IMAGES 0]\" have not yet been supplied."
               } {

# Get the mapping from the current image to the first image. If it is not
# yet defined, warn the user.
                  set m2 [ImageMapping $im]
                  if { $m2 == "" } {
                     Message "The image features required to define the mapping between image \"$im\" and \"[lindex $IMAGES 0]\" have not yet been supplied."
                  }
               }

# If both mappings are available...
               if { $m2 != "" } {

# Combine them to get a mapping from the current image to the target image.
# This is the mapping which must be applied to the image on the top of the
# effects stack to produce the required alignment.
                  if { $im != $ALIMG } {
                     set m21 [ConcMap $m2 0 $m1 1]
                  } {
                     set m21 "ref"
                  }

# Get the transformed image.
                  if { $m21 != "" } {
                     if { $m21 == "ref" } {
                        Message "The displayed image is already aligned with image \"$ALIMG\"."
                     } {
                        set out [UniqueFile]
                        set desc "Align (with image $ALIMG)"
                        if { [TranImage $image $m21 $out ""] } {

# Modify things (like positions lists, etc) to take account of the
# mapping just applied to the image. If anything goes wrong with this,
# things are left as they are (in which case give a warning).
                            if { ![MappingMod "" $m21 0] } {
                               Message "Unable to apply the effect \"$desc\" applied to image \"$im\"."
                               set desc ""

# Otherwise, store the values to be pushed onto the various stacks.
# Indicate that the display should be updated.
                           } {
                              set c $m21
                              set file $out
                              set update 1
                           }
                        }
                     }
                  }
               }
            }
         }

#---------------------------------------------------------------
# Filter - Applies a high pass filter to the displayed image. An attempt
# is made to reduce the effects of ringing round bright features.
      } elseif { $effect == "Filter" } {

# Set a default value for the filter size.
         if { ![info exists FSIZE] } {
            set FSIZE [expr 3.0*$PSF_SIZE]
         }

# Describe the filter size parameter.
         set types(FSIZE) "_REAL"
         set labels(FSIZE) "Filter size (in pixels): "
         set limits(FSIZE) [list  1.0 10000.0]

# Get a value from the user for the filter size.
         if { [GetPars FSIZE types labels limits "Enter Filter Parameters" \
                       "POLKA_FILTER_EFFECT" \
                       ". Enter the parameters needed to perform the selected effect."] } {

# First smooth the input image. The output file name is stored in
# variable "a1".
            set a1 [UniqueFile]
            set ok [Obey kappa gausmooth "in=$image fwhm=$FSIZE out=$a1"]

# If OK, take the difference between the smoothed image and the original.
# This will contain just the high frequencies, but bright features will
# have deep rings around them.
            if { $ok } {
               set a2 [UniqueFile]
               set ok [Obey kappa maths "exp=ia-ib ia=$image ib=$a1 out=$a2"]
            }

# To reduce the depth of these dark rings, we try smoothing the input
# image again, but this time excluding the bright features. First, remove
# the bright features from the above filtered image.
            if { $ok } {
               set a3 [UniqueFile]
               set ok [Obey kappa ffclean "in=$a2 out=$a3 box=$FSIZE clip=\[1,1,1,1\]"]
            }

# Next, transfer the bad pixel mask from this cleaned image to the
# original image.
            if { $ok } {
               set a4 [UniqueFile]
               set ok [Obey kappa maths "exp=ia*(1-0*ib) ia=$image ib=$a3 out=$a4"]
            }

# Next, smooth this new image (a copy of the original but with small, bright
# features removed). We allow the smoothing to fill in the holes left by
# the removal of the bright features.
            if { $ok } {
               set a5 [UniqueFile]
               set ok [Obey kappa gausmooth "in=$a4 wlim=1E-6 fwhm=$FSIZE out=$a5"]
            }

# See if there are any bad pixels left in the smoothed image.
            if { $ok } {
               if { [Obey kappa stats "ndf=$a5"] } {
                  set numbad [GetParamED kappa stats:numbad]
               } {
                  set ok 0
               }
            }

# Fill in any residual holes. FILLBAD reports an error if there are no bad
# values in the image, so we only run FILLBAD if there are any bad pixels
# to be removed.
            if { $ok && $numbad > 0 } {
               set a6 [UniqueFile]
               set ok [Obey kappa fillbad "in=$a5 out=$a6" noreport]
            } {
               set a6 $a5
            }

# Finally, subtract this smooth image from the original to get the
# filtered image.
            if { $ok } {
               set file [UniqueFile]
               set desc "Filter (Filter size = $FSIZE)"

               if { ![Obey kappa maths "exp=ia-ib ia=$image ib=$a6 out=$file"] } {
                  set ok 0
                  set file ""
                  set desc ""
                  set update 0
               }
            }

# If the user cancelled the effect, do not update the display.
         } {
            set update 0
         }

#---------------------------------------------------------------
# Maths - Applies an arbitrary algebraic expression to the other images
# on the stack, using KAPPA:MATHS. Image token IA refers to the image
# currently on the top of the image stack, IB refers to the one below it,
# etc.
      } elseif { $effect == "Maths" } {

# Get the maths expression to use.
         if { ![info exists MEXP] } { set MEXP "" }

         set types(MEXP) "_CHAR*50"
         set labels(MEXP) "Maths expression: "
         set limits(MEXP) ""

         if { [GetPars MEXP types labels limits "Enter Maths Parameters" \
                       "POLKA_MATHS_EFFECT" \
                       ". Enter the parameters needed to perform the selected effect."] } {

# Remove any spaces from the expression.
            regsub -all { } $MEXP "" MEXP

# Extract the image tokens (IA, etc) from the expression, and store them
# in list "ims".
            set exp $MEXP
            set ims ""
            while { [regexp -nocase -indices {(^|[^A-Z])(I[A-Z])([^A-Z]|$)} $exp match s imt e] } {
               lappend ims [string range $exp [lindex $imt 0] [lindex $imt 1]]
               set exp "[string range $exp 0 [lindex $s 1]][string range $exp [lindex $e 0] end]"
            }

# Warn the user if there are no image tokens in the expresion. Do not
# update the display.
            if { $ims == "" } {
               Message "There are no image tokens (such as \"IA\") in the Maths expression."
               set update 0

# Warn the user if there are any parameter tokens in the expresion. Do not
# update the display.
            } elseif { [regexp -nocase {(^|[^A-Z])(P[A-Z])([^A-Z]|$)} $MEXP match s pa e] } {
               Message "Parameter tokens such as \"$pa\" are not allowed in the Maths expression."
               set update 0

# Warn the user if there are any sub-expression tokens in the expresion. Do not
# update the display.
            } elseif { [regexp -nocase {(^|[^A-Z])(F[A-Z])([^A-Z]|$)} $MEXP match s fa e] } {
               Message "Sub-expression tokens such as \"$fa\" are not allowed in the Maths expression."
               set update 0

# If the expression is ok, sort the image tokens into alphabetical order.
            } {
               set ims [lsort $ims]

# Purge any repeated image tokens from the list.
               set i 0
               set j 1
               while { $j < [llength $ims] } {
                  if { [lindex $ims $i] == [lindex $ims $j] } {
                     set ims [lreplace $ims $j $j]
                  } {
                     incr i
                     incr j
                  }
               }

# Find the indices within the image stack corresponding to each image
# token. IA is the top-of-stack, IB is the one below the top, etc. Get
# the corresponding file name and construct a list (impar) of the ATASK
# parameter assignments required for each image token. If there are more
# image tokens than there are images on the stack, then warn the user,
# and do not update the display.
               set nstack [llength $IMAGE_STACK($im)]
               scan a %c a

               foreach imt $ims {
                  if { [regexp -nocase {I(.)} $imt match letter] } {
                     set letter [string tolower $letter]
                     scan $letter %c letter
                     set index [expr $letter - $a]

                     if { $index < $nstack } {
                        append impar " $imt=[lindex $IMAGE_STACK($im) $index] "
                     } {
                        Message "There are insufficient images on the stack to perform the requested Maths effect."
                        set update 0
                     }
                  }
               }

# If OK, get the name of the output image file.
               if { $update } {
                  set file [UniqueFile]
                  set desc "Maths (exp=$MEXP)"

# Invoke KAPPA:MATHS to do the work.
                  if { ![Obey kappa maths "exp=$MEXP $impar out=$file"] } {
                     set ok 0
                     set file ""
                     set desc ""
                     set update 0
                  }
               }
            }

# If the user cancelled the effect, do not update the display.
         } {
            set update 0
         }

#---------------------------------------------------------------
# Negate - Reverse the sign of every pixel value.
      } elseif { $effect == "Negate" } {
         set file [UniqueFile]
         set desc "Negate"
         if { ![Obey kappa maths "exp=-ia ia=$image out=$file"] } {
            set ok 0
            set file ""
            set desc ""
            set update 0
         }

#---------------------------------------------------------------
# Fit Sky - Estimate the sky background in the displayed image based on
# the current sky areas or supplied sky frames, and optionally subtract
# it from the displayed image.
      } elseif { $effect == "Fit Sky" } {

         if { !$SKYOFF } {
            Message "Sky subtraction is currently disabled. Select the \"Remove Sky\" item in the \"Options\" menu to enable it."
            set update 0
            set desc ""
            set ok 0

         } {

# Describe the parameter used to get the type of output image required.
            set types(SKYOP) "_CHOICE"
            set labels(SKYOP) "Required image: "
            set limits(SKYOP) [list "Fitted sky" "Difference between fitted sky and displayed image"]

# Set a default value for the image type.
            if { ![info exists SKYOP] } {
               set SKYOP "Fitted sky"
            }

# Get a value from the user for the image type.
            if { [GetPars SKYOP types labels limits "Enter Fit Sky Parameters" \
                          "POLKA_FITSKY_EFFECT" \
                          ". Enter the parameters needed to perform the selected effect."] } {

               if { $SKY_METHOD == $SKY_FRAME } {
                  set desc "Fit Sky (sky frame \"$SKYIMS($im)\""
               } {
                  set desc "Fit Sky (order $SKYPAR"
               }

               if { $SKYOP == "Fitted sky" } {
                  set sub 0
                  append desc " - fit returned)"
               } {
                  append desc " - sky corrected data returned)"
                  set sub 1
               }

               set file [SkySub $image $im $sub]
               if { $file == "" } {
                  set update 0
                  set desc ""
                  set ok 0
               }
            }
         }

#---------------------------------------------------------------
# Smooth - Apply gaussian smoothing.
      } elseif { $effect == "Smooth" } {

# Get the FWHM of the gaussian filter to use.
         if { ![info exists SSIZE] } {
            set SSIZE [expr 3.0*$PSF_SIZE]
         }

         set types(SSIZE) "_REAL"
         set labels(SSIZE) "Gaussian FWHM (in pixels): "
         set limits(SSIZE) [list  1.0E-6  10000.0]

         if { [GetPars SSIZE types labels limits "Enter Smoothing Parameters" \
                       "POLKA_SMOOTH_EFFECT" \
                       ". Enter the parameters needed to perform the selected effect."] } {

# Get the output image name.
            set file [UniqueFile]
            set desc "Smooth (Fwhm = $SSIZE)"

# Use KAPPA:GAUSMOOTH to do the work.
            if { ![Obey kappa gausmooth "in=$image fwhm=$SSIZE out=$file"] } {
               set ok 0
               set file ""
               set desc ""
               set update 0
            }

# If the user cancelled the effect, do not update the display.
         } {
            set update 0
         }

#---------------------------------------------------------------
# Stats - Display statistics of the data within the selected area of the
# currenrly displayed image.
      } elseif { $effect == "Stats" } {

# We do not need to update the display.
         set update 0

# Issue a warning and return if there is no selected area.
         if { $SELECTED_AREA == "" } {
            Message "Select an area by clicking and dragging over the image before using the \"Stats\" effect."

#  Otherwise...
         } {

# Get the bounds in canvas coordinates of the selected area.
            set cxlo [lindex $SELECTED_AREA 0]
            set cylo [lindex $SELECTED_AREA 1]
            set cxhi [lindex $SELECTED_AREA 2]
            set cyhi [lindex $SELECTED_AREA 3]

# Convert these to pixel coordinates. Note, the Y axis is reversed since the
# TK origin is at the UPPER left corner.
            set pxyl [CanToNDF $cxlo $cyhi]
            if { $pxyl == "" } { return }
            set pxlo [lindex $pxyl 0]
            set pylo [lindex $pxyl 1]
            set pxyl [CanToNDF $cxhi $cylo]
            set pxhi [lindex $pxyl 0]
            set pyhi [lindex $pxyl 1]

# Convert these to pixel indices.
            set ipxlo [expr round($pxlo) + 1 ]
            set ipylo [expr round($pylo) + 1 ]
            set ipxhi [expr round($pxhi) ]
            set ipyhi [expr round($pyhi) ]

# Create the NDF section specifier for the selected area.
            set section "($ipxlo:$ipxhi,$ipylo:$ipyhi)"

# Run KAPPA:STATS and display the output if succesful.
            if { [Obey kappa stats "ndf=${image}${section}"] } {
               set mess "\nPixel statistics within section $section:\n"
               append mess "Total data sum     \t\t:  [format "%.13g" [GetParamED kappa stats:total]]\n"
               append mess "Mean value         \t\t:  [format "%.13g" [GetParamED kappa stats:mean]]\n"
               append mess "Standard deviation \t:  [format "%.13g" [GetParamED kappa stats:sigma]]\n"
               append mess "Maximum value      \t:  [format "%.13g" [GetParamED kappa stats:maximum]]\n"
               append mess "Minimum value      \t:  [format "%.13g" [GetParamED kappa stats:minimum]]\n"
               append mess "No. of good pixels \t\t:  [GetParamED kappa stats:numgood]\n"
               append mess "Total no. of pixels\t\t:  [GetParamED kappa stats:numpix]\n"
               Message $mess
            }

# Cancel the area selection.
            CancelArea

         }

#---------------------------------------------------------------
# Fill - Replace any bad pixels in the image. If a constant fill value is
# supplied, use KAPPA:NOMAGIC, otherwise use KAPPA:FILLBAD.
      } elseif { $effect == "Fill" } {

# See if a constant or varying value is to be used, and get the constant
# value.
         if { ![info exists FILLCON] } {
            set FILLCON "Variable value"
            set FILLVAL 0.0
         }

         set vars [list FILLCON FILLVAL]

         set types(FILLCON) "_CHOICE"
         set labels(FILLCON) "Fill with: "
         set limits(FILLCON) [list "Variable value" "Constant value"]

         set types(FILLVAL) "_REAL"
         set labels(FILLVAL) "Constant fill value: "
         set limits(FILLVAL) ""

         if { [GetPars $vars types labels limits "Enter Filling Parameters" \
                       "POLKA_FILL_EFFECT" \
                       ". Enter the parameters needed to perform the selected effect."] } {

# Get the output image name.
            set file [UniqueFile]

# If a constant value us to be used, invoke KAPPA:NOMAGIC to do the work.
            if { $FILLCON == "Constant value" } {
               set desc "Fill (with value $FILLVAL)"
               if { ![Obey kappa nomagic "in=$image out=$file repval=$FILLVAL sigma=0.0"] } {
                  set ok 0
                  set file ""
                  set desc ""
                  set update 0
               }

# If a varying value us to be used, invoke KAPPA:FILLBAD to do the work.
            } {
               set desc "Fill (with varying value)"
               if { ![Obey kappa fillbad "in=$image out=$file"] } {
                  set ok 0
                  set file ""
                  set desc ""
                  set update 0
               }
            }

# If the user cancelled the effect, do not update the display.
         } {
            set update 0
         }

#---------------------------------------------------------------
# Threshold - Apply upper and lower limits to the pixel values.
# Out-of-bounds pixels can either be set bad, or set to the limit value.
      } elseif { $effect == "Threshold" } {

# Get the limits and see if the rejected pixels are to be set bad.
         if { ![info exists THRLO] } {
            set THRBAD 1
            set THRLO $SCALOW
            set THRHI $SCAHIGH
         }

         set vars [list THRLO THRHI THRBAD]

         set types(THRLO) "_REAL"
         set labels(THRLO) "Lower threshold value: "
         set limits(THRLO) ""

         set types(THRHI) "_REAL"
         set labels(THRHI) "Upper threshold value: "
         set limits(THRHI) ""

         set types(THRBAD) "_LOGICAL"
         set labels(THRBAD) "Remove out-of-bounds pixels?"
         set limits(THRBAD) ""

         if { [GetPars $vars types labels limits "Enter Thresholding Parameters" \
                       "POLKA_THRESHOLD_EFFECT" \
                       ". Enter the parameters needed to perform the selected effect."] } {

# Ensure the limits are in the correct order.
            if { $THRLO > $THRHI } {
               set temp $THRLO
               set THRLO $THRHI
               set THRHI $temp
            }

# Construct a list of ATASK parameter valus for the replacement values.
            if { $THRBAD } {
               set news "newlo=bad newhi=bad"
            } {
               set news "newlo=$THRLO newhi=$THRHI"
            }

# Decide on the output image name.
            set file [UniqueFile]
            set desc "Threshold (limits = \[ $THRLO, $THRHI \] )"

# Invoke KAPPA:THRESH to do the work.
            if { ![Obey kappa thresh "in=$image out=$file thrlo=$THRLO thrhi=$THRHI $news"] } {
               set ok 0
               set file ""
               set desc ""
               set update 0
            }

# If the user cancelled the effect, do not update the display.
         } {
            set update 0
         }

#---------------------------------------------------------------
# Log - Takes the log of the difference between each pixel value and the
# minimum pixel value in the image.
      } elseif { $effect == "Log" } {

# Invoke KAPPA:STATS to find the minimum pixel value in the image.
         if { [Obey kappa stats "ndf=$image"] } {

# Convert D exponents (as used by HDS) to E (as used by Tcl).
            set dmin [GetParamED kappa stats:minimum]

# Decide on the output image name.
            set file [UniqueFile]
            set desc "Log (offset = $dmin)"

# Invoke KAPPA:MATHS to do the work.
            if { ![Obey kappa maths "exp=log10(ia-pa) ia=$image pa=$dmin out=$file"] } {
               set ok 0
               set file ""
               set desc ""
               set update 0
            }
         }


#---------------------------------------------------------------
# Undo - Undo the most recently applied effect. This is done by poping
# the top entry of the image stack.
#
# Undo All - Undo all the applied effects. This is done by removing
# all but the bottom entry (the original image) from the image stack.
      } elseif { $effect == "Undo" || $effect == "Undo All" } {

# Store the number of effects to undo.
         if { $effect == "Undo" } {
            set n 1
         } {
            set n [expr [llength $IMAGE_STACK($im)] - 1]
         }

# Undo this many effects.
         for {set i 0} {$i < $n} {incr i} {

# If the mapping caused any changes to the positions lists (etc), undo them.
# This will be the case unless the mapping is a unit mapping (indicated by
# the string "ref"). If this fails, leave the effect in place, and give a
# warning.
            set m21 [Top EFFECTS_MAPPINGS($im)]
            if { $m21 != "ref" } {
               if { ![MappingMod "" $m21 1] } {
                  set eff [Top EFFECTS_STACK($im)]
                  Message "Unable to undo the effect \"$eff\" applied to image \"$im\"."
                  break
               }
            }

# We arrive here only if the above went OK. Pop the top entries from the
# stacks.
            Pop IMAGE_STACK($im)
            Pop EFFECTS_STACK($im)
            Pop EFFECTS_MAPPINGS($im)
         }

#---------------------------------------------------------------
# Show Effects - All other effects names result in a dialog box being created
# with a list of the effects applied to the currently displayed image.
      } {

# Indicate that we do not need to update the display.
         set update 0

# Construct a text string describing the effecst. These are taken from
# the descriptions store in EFFECTS_STACK. The last line refers to the
# top of stack, and is identified by appending the string "(displayed)" to
# the description.
         set nef [llength $EFFECTS_STACK($im)]
         if { $nef > 0 } {

            set text "The displayed image is derived from image \"$im\" using the following effect(s):\n\n"
            for {set i [expr $nef - 1 ]} {$i > -1} {incr i -1} {
               set effect [lindex $EFFECTS_STACK($im) $i]

               if { $i == 0 && $im != "" } {
                  append text "    $effect  (displayed)\n"
               } {
                  append text "    $effect\n"
               }
            }

         } {
            set text "No effects have been applied to the displayed image (\"$im\")."
         }

# Display the text string.
         Message $text
      }

# If a new image was created, containing the results of applying the
# selected effect, then push it onto the image stack. Also store a
# description of the effect on the effects stack, and the mapping
# from the new image to the previous image. If the effect just applied
# produced no geometric change, then the new image is assigned a unit
# mapping.
      if { $file != "" } {
         Push IMAGE_STACK($im) $file
         Push EFFECTS_STACK($im) $desc
         if { $c == "" } {
            set c "ref"
         }
         Push EFFECTS_MAPPINGS($im) $c
      }

# Cancel the informative text set earlier in this procedure.
      if { $told } { SetInfo "" 0 }

# If required, display the new image.
      if { $update && !$nodisp} {
         UpdateDisplay gwm
      }
   }

# Delete all the temporary files created in this procedure, except for the
# one which was pushed on the image stack.
   EndUF $tfc $file

}

proc MappingMod {image map undo} {
#+
#  Name:
#     MappingMod
#
#  Purpose:
#     Modify the positions lists, mappings, etc, relating to
#     an image, to take account of the effects of mapping the image using
#     the specified map.
#
#  Arguments:
#     image
#        The image to be used. If this is null, then the currently
#        displayed image is used.
#     map
#        The mapping which has been applied to the displayed image, for
#        which modifications to the positions lists (etc) are to be made.
#     undo
#        If this is non-zero, then the modifications caused by the
#        supplied mapping are removed instead of being applied. Note, in
#        this case the current effects stacks MUST STILL include the
#        effect which is being undone.
#
#  Returned Value:
#     1 for success, zero for failure (in which case the positions lists,
#     etc, are left unchanged).
#
#  Globals:
#     DEF_OEMAP (Read and Write)
#       The most recent OE Mapping to be found succesfully.
#     E_RAY_FEATURES (Read)
#        An integer representing the "E-ray features" object type.
#     E_RAY_MASK (Read)
#        An integer representing the "E-ray mask" object type.
#     IMAGE_DISP (Read)
#        The displayed image (without section).
#     IMMAP (Read and Write)
#        An array of lists (one for each image), each giving the mapping
#        from the supplied image to the reference (first) image. Each
#        mapping is a list of 6 values representing a general linear
#        transformation.
#     OEMAP (Read and Write)
#        An array of mappings (one for each image), each being a list of
#        6 parameter values giving the linear mapping from E to O ray.
#     O_RAY_FEATURES (Read)
#        An integer representing the "O-ray features" object type.
#     O_RAY_MASK (Read)
#        An integer representing the "O-ray mask" object type.
#     PNTPX (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel X coordinates.
#     PNTPY (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel Y coordinates.
#-
   global DEF_OEMAP
   global E_RAY_FEATURES
   global E_RAY_MASK
   global E_RAY_SKY
   global IMAGE_DISP
   global IMMAP
   global IMSEC_REQ
   global OEMAP
   global O_RAY_FEATURES
   global O_RAY_MASK
   global O_RAY_SKY
   global PNTPX
   global PNTPY
   global SECTION_STACK
   global SECTION_REQ

# Assume success.
   set ok 1

# Determine the inversion flags to use, depending on the specified
# mapping direction.
   if { $undo } {
      set for 1
      set back 0
   } {
      set for 0
      set back 1
   }

# If no image was supplied, use the currently displayed image.
   if { $image == "" } { set image $IMAGE_DISP }

# Save all the current information relating to the specified image. This
# is done so that it can be re-instated if anything goes wrong while
# calculating new values.
   foreach obj "$O_RAY_SKY $E_RAY_SKY $O_RAY_FEATURES $E_RAY_FEATURES $O_RAY_MASK $E_RAY_MASK" {
      set old_px($obj) $PNTPX($image,$obj)
      set old_py($obj) $PNTPY($image,$obj)
   }

   set old_immap [ImageMapping $image]
   set old_oemap [OEMapping $image]
   set old_defoemap $DEF_OEMAP
   set old_sec_stack $SECTION_STACK
   set old_sec_req $SECTION_REQ

# If the image being used is the displayed image, insert a new section at
# the bottom of the section stack which causes the entire mapped image to be
# displayed so that it fills the screen. The bottom entry on the stack
# always refers to the entire image. If there are no entries on the stack
# then the currently displayed section will be for the entire image, and
# so can be used instead.
   if { $image == $IMAGE_DISP } {
      if { $map != "ref" } {
         if { [llength $SECTION_STACK] > 0 } {
            set sec0 [lindex $SECTION_STACK end]
         } {
            set sec0 $SECTION_REQ
         }
         set sec0 [TranSec $sec0 $map $for]
         lappend SECTION_STACK $sec0
      }
   }

# Get the transformed positions lists.
   foreach obj "$O_RAY_SKY $E_RAY_SKY $O_RAY_FEATURES $E_RAY_FEATURES $O_RAY_MASK $E_RAY_MASK" {
      if { ![TranList $map $for $old_px($obj) $old_py($obj) \
                      PNTPX($image,$obj) \
                      PNTPY($image,$obj)] } {
         set ok 0
         break
      }
   }

# Modify the image and OE mappings for the displayed image by
# concatenating them with the supplied mapping.
   if { $ok } {
      if { $old_immap != "" } {
         set IMMAP($image) [ConcMap $map $back $old_immap 0]
         if { $IMMAP($image) == "" } {
            set ok 0
         }
      }
   }

   if { $ok } {
      if { $old_oemap != "" } {
         set OEMAP($image) [ConcMap [ConcMap $map $back $old_oemap 0] \
                                    0 $map $for]
         if { $OEMAP($image) == "" } {
            set ok 0
         }
      }
      if { $old_defoemap != "" } {
         set DEF_OEMAP [ConcMap [ConcMap $map $back $old_defoemap 0] \
                                    0 $map $for]
         if { $DEF_OEMAP == "" } {
            set ok 0
         }
      }
   }

# If anything went wrong, re-istate the original values.
   if { !$ok } {

      foreach obj "$O_RAY_SKY $E_RAY_SKY $O_RAY_FEATURES $E_RAY_FEATURES $O_RAY_MASK $E_RAY_MASK" {
         set PNTPX($image,$obj) $old_px($obj)
         set PNTPY($image,$obj) $old_py($obj)
      }

      if { old_immap != "" } {
         set IMMAP($image) $old_immap
      } elseif { [info exists IMMAP($image)] } {
         unset IMMAP($image)
      }

      if { old_oemap != "" } {
         set OEMAP($image) $old_oemap
      } elseif { [info exists OEMAP($image)] } {
         unset OEMAP($image)
      }

      set DEF_OEMAP $old_defoemap
      set SECTION_STACK $old_sec_stack
      set SECTION_REQ $old_sec_req
   }

   return $ok

}

proc MakeTrn {map} {
#+
#  Name:
#     MakeTrn
#
#  Purpose:
#     Create an AST MApping from a set of 6 linear mapping parameters.
#
#  Arguments:
#     map
#        A set of 6 linear mapping parameters, or "ref" for a unit mapping.
#
#  Returned Value:
#     The path to the text file holding the Mapping, or a null string if the
#     supplied mapping was undefined, or if KAPPA:WCSADD failed.
#-

# Initialise the returned string.
   set ret ""

# Only proceed if the supplied mapping is defined.
   if { $map != "" } {

# If a unit mapping has been specified, we will create an AST UnitMap.
      if { $map == "ref" } {
         set maptype "unit"
         set coeffs ""

# Otherwise, we will create a compound WinMap/MatrixMap. Construct a string
# holding all 6 coefficients, in a form suitable for passing to an A-task as
# the value for a vector parameter.
      } {
         set maptype "linear"
         set coeffs "\[[lindex $map 0]"
         for {set i 1} {$i < 6} {incr i} {
            append coeffs ",[lindex $map $i]"
         }
         append coeffs "\]"
      }

# Get the name for the text file to hold the AST Mapping.
      set trfile [UniqueFile]

# Create the new Mapping. Return the name of the Mapping file if succesful.
      if { [Obey ndfpack wcsadd "ndf=\! naxes=2 maptype=$maptype mapout=$trfile tr=$coeffs"] } {
         set ret ${trfile}
      }
   }

   return $ret
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
#    F_OWNER (Read and Write)
#        The name of the Polka widget which "owns" the focus. It is
#        temporarily changed to be the toplevel containing the dialogue
#        box. It is reset to its original value when the label has been
#        obtained.
#    TOP (Read)
#        The path to the main application window.
#-
   global F_OWNER
   global TOP
   global env

# If the top level window has not yet been created, message to standard
# output
   if { ![info exists TOP] } {
      puts $message

# Otherwise, display the message in a dialog box.
   } {

# Set the F_OWNER variable so that this window is handed the focus by the
# main Polka window. Set the current value to be re-instated later.
      set old_f_owner $F_OWNER
      set F_OWNER .msg

# Display the dialog box.
      dialog .msg "Polka - Message..." $message {} 0 OK

# Restablish the original value of F_OWNER.
      set F_OWNER $old_f_owner
   }

}

proc Erase {image object} {
#+
#  Name:
#     Erase
#
#  Purpose:
#     Erase an object. This includes clearing the associated markers on
#     the canvas, and clearing the global arrays holding information
#     describing the object.
#
#  Arguments:
#     image
#        The image with which the object is associated.
#     obj_out
#        The object type ($E_RAY_MASK, $O_RAY_MASK, etc)
#-
   global PNTCX
   global PNTCY
   global PNTID
   global PNTLBL
   global PNTNXT
   global PNTPX
   global PNTPY
   global PNTVID
   global PNTTAG
   global RESAVE

# Erase any canvas items currently associated with the output list.
   ClearPosns $image $object

# Nullify the output lists.
   set PNTPX($image,$object) ""
   set PNTPY($image,$object) ""
   set PNTCX($image,$object) ""
   set PNTCY($image,$object) ""
   set PNTLBL($image,$object) ""
   set PNTNXT($image,$object) ""
   set PNTID($image,$object) ""
   set PNTVID($image,$object) ""
   set PNTTAG($image,$object) ""

# Indicate the output images will need to be re-calculated.
   set RESAVE 1

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
#     POLKA_SCRATCH
#       The path to the temporary directory used to store temporary images
#       created by Polka.
#     IFILE (Read)
#        Temporary file names created by UniqueFile are stored in Polka's
#        temporary POLKA_SCRATCH directory which is deleted when
#        Polka terminates. They have a name of the form polka<i> where
#        <i> is an integer, which is different for each file and
#        increases monotonically throughout the execution of Polka. IFILE
#        records the value of i used in the previous call to UniqueFile.
#     IFILE_STACK (Write)
#        A stack on which is stored the value of IFILE corresponding to
#        the first temporary file created in the current context.
#-
   global IFILE
   global IFILE_STACK
   global POLKA_SCRATCH

# Loop round each value of IFILE used in this context. This starts with
# the value stored by the corresponding call to BeginUF, and ends with
# the current value.
   set levels [expr [llength $IFILE_STACK] - $context ]
   set ifile_start [Pop IFILE_STACK $levels]
   for {set i $ifile_start} {$i <= $IFILE} {incr i} {

# Construct the corresponding file name (NB, make sure this next line keeps
# in step with any changes made in procedure UniqueFile).
      set file "$POLKA_SCRATCH/polka$i"

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
#    Shutdown the tcl script, cleaning up Polka internals in the process.
#
#  Arguments:
#    args
#       The exit integer status value.
#
#  Globals:
#    ADAM_TASKS (Read)
#       A list of the names of the ADAM tasks started up by Polka.
#    ADAM_USER (Read)
#       The path to the temporary ADAM_USER directory used by Polka.
#    ATASK (Read)
#       Was this script activated form the polka ATASK?
#    OLD_ADAM_USER (Read)
#       The original value of the ADAM_USER environment variable, or a null
#       string if ADAM_USER was not defined.
#    OLDCCDPACK (Read)
#       A list of process id.s for any CCDPACK processes which were running
#       when Polka was started.
#    OLDKAPPA (Read)
#       A list of process id.s for any KAPPA processes which were running
#       when Polka was started.
#    POLKA_SCRATCH (Read)
#       The path to the directory used by Polka to store temporary NDFs.
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
   global OLDCCDPACK
   global OLDKAPPA
   global POLKA_SCRATCH

# Re-instate the original exit command in case anything goes wrong in
# this procedure.
   rename exit {}
   rename tcl_exit exit

# Close any log file.
   if { $LOGFILE_ID != "" } { close $LOGFILE_ID }

# Kill all the ADAM tasks started up by Polka.
   foreach task $ADAM_TASKS {
      if { [info commands $task] != "" } {
         $task kill
      }
   }

# Delete the temporary ADAM_USER directory created at the start.
   catch "exec rm -rf $ADAM_USER"

# Delete the POLKA_SCRATCH directory created at the start.
   catch "exec rm -rf $POLKA_SCRATCH"

# Trap pids for all current KAPPA processes.
   if { ![catch {exec ps | grep kappa | grep -v grep | \
                 awk {{print $1}}} newkappa] } {
      set newkappa {}
   }

# Kill any new processes (i.e ones which are not in the list of KAPPA
# processes which were active when Polka started).
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

# Kill any new CCDPACK processes in the same way.
   if { ![catch {exec ps | grep ccdpack | grep -v grep | \
                 awk {{print $1}}} newccdpack] } {
      set newccdpack {}
   }

   foreach newpid $newccdpack {

      set dokill 1
      foreach oldpid $OLDCCDPACK {
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

# Send the current options values back to the a-task by writing them to
# the communications file.
   SendBack

# Finally, kill the current process.
   exit
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
#     or a blank string if there is no associated label.
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

proc FindPosn {names values tol args} {
#+
#  Name:
#     FindPosn
#
#  Purpose:
#     Find an object position which satisfies the supplied criteria.
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
#           LBL - The textual label associated with a position.
#           TAG - A canvas tag associated with the position.
#     values
#        A list of values corresponding to the parameter names supplied
#        in "names".
#     tol
#        The tolerance to use for CX, CY, PX and PY equality.
#     args
#        An optional list argument holding the image from which the
#        positions are derived, and the type of objects to be searched.
#        If these are not supplied, they default to $IMAGE_DISP and
#        $CUROBJ_DISP.
#
#  Returned Value:
#     The index of the first position with the supplied parameter values
#     (the values of other unspecified parameters are ignored). A null
#     value is returned if no matching position is found.
#
#  Globals:
#     CUROBJ_DISP (Read)
#        The type of the current objects displayed.
#     IMAGE_DISP (Read)
#        The displayed image (without section).
#     PNTCX (Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas X coordinates.
#     PNTCY (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas Y coordinates.
#     PNTID (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas item identifiers associated with the
#        positions in the list. A value of -1 indicates that no marker is
#        currently drawn for the position.
#     PNTNXT (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of integers representing indices within the lists
#        given by PNTxxx. Each integer gives the index of the next position
#        along the edge of a polygon. The vector starting at position
#        index i, ends at position index given by the i'th element of
#        PNTNXT. If this value is blank ("") then position i is not part of
#        a polygon. If this value is -1, then the end of the vector starting
#        position i is unspecified (in this case the polygon is not
#        closed).
#     PNTPX (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel X coordinates.
#     PNTPY (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel Y coordinates.
#     PNTTAG (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas tags (one for each position).
#     PNTVID (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas item identifiers associated with the
#        vectors between positions in the list. A value of -1 indicates that
#        no line is currently drawn for the position. A blank string
#        indicates that no vector is defined.
#
#-
   global CUROBJ_DISP
   global IMAGE_DISP
   global PNTCX
   global PNTCY
   global PNTID
   global PNTLBL
   global PNTNXT
   global PNTPX
   global PNTPY
   global PNTVID
   global PNTTAG

# Initialise the returned value.
   set ret ""

# Store the image and object type.
   if { $args == "" } {
      set image $IMAGE_DISP
      set object $CUROBJ_DISP
   } {
      set image [lindex $args 0]
      set object [lindex $args 1]
   }

# Do nothing if the list is empty.
   if { [info exists PNTID($image,$object)] } {
      set size [llength $PNTID($image,$object)]
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
               set curval [lindex $array($image,$object) $i]

# If it is different to the supplied position, set ret null and break out of
# the loop. Pixel and canvas coordinates are given a tolerance.
               if { $name == "PX" || $name == "PY" ||
                    $name == "CX" || $name == "CY" } {
                  if { $curval > [expr $supval + $tol] || $curval < [expr $supval - $tol] } {
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
#     Exit Polka, warning the user if the output images have not yet
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
   global GOTOUT

# If required, save the output images.
   if { $save && $RESAVE } { Save }

# Construct a suitable confirmation question, depending on whether or not
# the output images have been saved.
   if { $RESAVE && $GOTOUT } {
      set quest "The output images have not yet been saved!\n\nQuit Polka?"
   } {
      set quest "Exit Polka?"
   }

# See if the user confirms the intention to exit. If so, exit.
   if { [Confirm $quest] } { exit }

}

proc Fit {labxy gx gy labuv gu gv fittype mess} {
#+
#  Name:
#     Fit
#
#  Purpose:
#     Estimates a linear mapping which maps the (gx,gy) positions onto
#     the (gu,gv) positions, imposing the restrictions implied by the
#     supplied fit type.
#
#  Arguments:
#     labxy
#        A list of the labels associated with the input positions.
#     gx
#        A list of X coordinates for the input positions.
#     gy
#        A list of Y coordinates for the input positions.
#     labuv
#        A list of the labels associated with the mapped positions.
#     gu
#        A list of X coordinates for the mapped positions.
#     gv
#        A list of Y coordinates for the mapped positions.
#     fittype
#        The numerical fittype to use.
#
#  Returned Value:
#     A list of 6 parameter values. A blank string is returned if the
#     mapping cannot be determined, and "ref" is returned for a unit mapping.
#-

# Assume failure.
   set ret ""

# Unless the fit type is a full 6 parameter fit (fittype 5) use a local
# procedure instead of CCDPACK:REGISTER, which constrains the magnification
# to be the same on both axes and is fast.
   if { $fittype != 5 } {
      set ret [Fit1234 $labxy $gx $gy $labuv $gu $gv $fittype]

# Otherwise, report an error (fittype 5 is no longer supported because it
# required KAPRH tasks, which have now gone).
   } {
      Message "POLKA: fittype 5 is no longer supported (programming error)."
   }

   return $ret
}

proc Fit1234 {lxy gx gy luv gu gv fittype} {
#+
#  Name:
#     Fit1234
#
#  Purpose:
#     Calculate a linear mapping from the (gx,gy) positions to the
#     (gu,gv) positions. Various constraints can be put on the fit by
#     specifying a suitable fittype. These are like the fit types 1 to 4
#     used by CCDPACK:REGISTER except that magnification is constrained
#     to be the same on both axes. it type 5 is not handled by this
#     procedure.
#
#  Arguments:
#     lxy
#        A list of the labels associated with the input positions.
#     gx
#        A list of X coordinates for the input positions.
#     gy
#        A list of Y coordinates for the input positions.
#     luv
#        A list of the labels associated with the mapped positions.
#     gu
#        A list of X coordinates for the mapped positions.
#     gv
#        A list of Y coordinates for the mapped positions.
#     fittype
#        The numerical fit type; 3 or 4.
#
#  Returned Value:
#     A list of 6 parameter values (in which c3 and c5 are always set to
#     zero). A blank string is returned if the mapping cannot be
#     determined, and "ref" is returned for a unit mapping.
#-

# Assume failure.
   set ret ""

# Get lists holding corresponding X and Y values from each list (i.e.
# positions which have the same label). Ignore positions with blank labels.
   set xylen [llength $lxy]
   set n 0
   for {set i 0} {$i < $xylen} {incr i} {
      set lab [lindex $lxy $i]
      if { $lab != "" } {
         set iuv [lsearch -exact $luv $lab]
         if { $iuv != -1 } {
            incr n
            lappend lx [lindex $gx $i]
            lappend ly [lindex $gy $i]
            lappend lu [lindex $gu $iuv]
            lappend lv [lindex $gv $iuv]
         }
      }
   }

# Only proceed if there are some common positions.
   if { $n > 0 } {
      while { 1 } {

# If there is only 1 position, we know what the answer must be. Set the
# coefficient values and leave the loop.
         if { $n == 1 } {
            set c1 [expr $lu - $lx ]
            set c2 1.0
            set c3 0.0
            set c4 [expr $lv - $ly ]
            break
         }

# Otherwise, find the required sums.
         set suy 0.0
         set svx 0.0
         set svy 0.0
         set sux 0.0
         set sv 0.0
         set sy 0.0
         set sx 0.0
         set su 0.0
         set sxx 0.0
         set syy 0.0

         for {set i 0} {$i < $n} {incr i} {
            set x [lindex $lx $i]
            set y [lindex $ly $i]
            set u [lindex $lu $i]
            set v [lindex $lv $i]

            set suy [expr $suy + $u * $y]
            set svx [expr $svx + $v * $x]
            set svy [expr $svy + $v * $y]
            set sux [expr $sux + $u * $x]
            set sv [expr $sv + $v]
            set sy [expr $sy + $y]
            set sx [expr $sx + $x]
            set su [expr $su + $u]
            set sxx [expr $sxx + $x * $x]
            set syy [expr $syy + $y * $y]
         }

# For fit type 1 (shift of origin only) things are very simple.
         if { $fittype == 1 } {
            set c1 [expr ( $su - $sx ) / $n ]
            set c2 1.0
            set c3 0.0
            set c4 [expr ( $sv - $sy ) / $n ]

# Now do other fit types.
# Find the denominator.
         } {
            set den [expr $n * ( $sxx + $syy ) - $sx * $sx - $sy * $sy]

# If the denominator is zero, assume a magnification of 1.0 and a
# rotation of zero.
            if { $den < 1.0E-20 && $den > -1.0E-20 } {
               set c2 1.0
               set c3 0.0

# Otherwise, find the magnification, and rotation.
            } {

               if { $fittype == 2 } {
                  set a [expr ($n * ( $svy + $sux ) - $sv * $sy - $su * $sx)]
                  set b [expr ($n * ( $suy - $svx ) - $su * $sy + $sv * $sx)]
                  set c [expr sqrt (  $a * $a + $b * $b )]
                  if { $c > 0.0 } {
                     set c2 [expr $a / $c ]
                     set c3 [expr $b / $c ]
                  } {
                     set c2 1.0
                     set c3 0.0
                  }

               } elseif { $fittype == 3 } {
                  set a [expr ($n * ( $svy + $sux ) - $sv * $sy - $su * $sx)]
                  set c2 [expr $a / $den ]
                  set c3 0.0

               } elseif { $fittype == 4 } {
                  set a [expr ($n * ( $svy + $sux ) - $sv * $sy - $su * $sx)]
                  set c2 [expr $a / $den ]
                  set c3 [expr ($n * ( $suy - $svx ) - $su * $sy + $sv * $sx) / $den]

               }
            }

# Find the offsets.
            set c1 [expr ( $su - $sx * $c2 - $c3 * $sy) / $n]
            set c4 [expr ( $sv - $sy * $c2 + $c3 * $sx) / $n]
         }

# Transform the xy positions using the mapping, and find the rms
# deviation between the xy and uv positions. Also make a list of all the
# residuals.
         set res2_list ""
         set sum2 0.0
         for {set i 0} {$i < $n} {incr i} {
            set x [lindex $lx $i]
            set y [lindex $ly $i]
            set u [lindex $lu $i]
            set v [lindex $lv $i]
            set du [expr $c1 + $c2 * $x + $c3 * $y - $u]
            set dv [expr $c4 - $c3 * $x + $c2 * $y - $v]
            set res2 [expr $du * $du + $dv * $dv]

            lappend res2_list $res2

            set sum2 [expr $sum2 + $res2]
         }
         set rms2 [expr $sum2 / $n ]

# If the rms deviation is ok (less than 0.1 pixel), leave the loop.
         if { $rms2 < 0.01 } {
            break

# Otherwise, remove points further than 1.5 sigma from the mean.
         } {
            set new_lx ""
            set new_ly ""
            set new_lu ""
            set new_lv ""
            set new_n 0

            set res2_limit [expr 2.25 * $rms2]
            for {set i 0} {$i < $n} {incr i} {
               if { [lindex $res2_list $i] <= $res2_limit } {
                  lappend new_lx [lindex $lx $i]
                  lappend new_ly [lindex $ly $i]
                  lappend new_lu [lindex $lu $i]
                  lappend new_lv [lindex $lv $i]
                  incr new_n
               }
            }

# Leave the loop if no points were rejected.
            if { $n == $new_n || $n == 0 } { break }

# Otherwise store the new lists and go round again.
            set lx $new_lx
            set ly $new_ly
            set lu $new_lu
            set lv $new_lv
            set n $new_n

         }
      }

# Return "ref" for a unit mapping.
      if { $c1 == 0.0 && $c2 == 1.0 && $c3 == 0.0 && $c4 == 0.0 } {
         set ret "ref"
      } {
         set c5 [expr - $c3 ]
         set c6 $c2
         set ret [list $c1 $c2 $c3 $c4 $c5 $c6]
      }
   }

   return $ret
}

proc GetFeature {cx cy rlabel} {
#+
#  Name:
#     GetFeature
#
#  Purpose:
#     Check if supplied canvas positions can be used as features,
#     and if so, adds the features to the list of current features.
#
#  Arguments:
#     cx, cy
#        A list of canvas coordinates of the initial guesses at the feature
#        positions.
#     rlabel
#        If this is not null, then it is a list of labels to be used as the
#        labels for the new features (and the user is not prompted for new
#        labels). Also, this suppresses the warning messages which are
#        otherwise displayed if a position already exists at the supplied
#        position, or if a position cannot be centroided.
#
#  Returned Value:
#      The number of features which could not be centroided.
#
#  Globals:
#     CAN (Read)
#        Path to the canvas containing the GWM image display.
#     IMAGE_DISP (Read)
#        The displayed image (without section).
#     PSF_SIZE (Read)
#        The typical size of a feature in pixel.
#     SECTION_DISP (Read)
#       The displayed image section (eg "(10:200,23:68)" ).
#     TEST_ID (Write)
#       The canvas idea for the current "candidate feature marker". Set
#       blank if there is no candidate feature.
#     TEST_PX (Write)
#       The pixel X coordinates at the candidate feature.
#     TEST_PY (Write)
#       The pixel Y coordinates at the candidate feature.
#-
   global CAN
   global CURCOL
   global IMAGE_DISP
   global IMAGE_STACK
   global SECTION_DISP
   global PSF_SIZE
   global TEST_ID
   global TEST_PX
   global TEST_PY

# Assume no bad values.
   set nbad 0

# See how many positions are to be procesed.
   set np [llength $cx]

# Take a copy of the supplied list of labels.
   set rlabs $rlabel

# Get the NDF pixel coordinates at the supplied canvas positions.
   for {set i 0} {$i < $np} {incr i} {
      set pxy [CanToNDF [lindex $cx $i] [lindex $cy $i] ]
      set ppx [lindex $pxy 0]
      set ppy [lindex $pxy 1]
      lappend px $ppx
      lappend py $ppy
   }

# If the positions are to be centroided...
   if { $PSF_SIZE > 0 } {

#  Write out the initiali pixel coordinates to a text file to be passed
#  to POLCENT.
      set tfile [UniqueFile]
      set tfile_id [open $tfile w]

      for {set i 0} {$i < $np} {incr i} {
         puts $tfile_id "[lindex $px $i] [lindex $py $i]"
      }

      close $tfile_id

# Select a name fo the POLCENT output text file.
      set tofile [UniqueFile]

# Calculate the box size and max shift values.
      set isize [expr 2 * $PSF_SIZE]
      set maxsh [expr 4 * $PSF_SIZE]

# Attempt to centroid them.
      set imsec "[Top IMAGE_STACK($IMAGE_DISP)]$SECTION_DISP"
      if { [Obey polpack polcent "ndf=\"$imsec\" maxshift=$maxsh isize=$isize infile=$tfile outfile=$tofile"] } {

# If succesful, read the accurate feature coordinates from the output
# file. Create a Tcl list containing the X and Y values, replacing "D"
# exponents by "E".
         set qx ""
         set qy ""
         set tofile_id [open $tofile r]

         while { [gets $tofile_id line] != -1 } {
            regsub -nocase -all D $line E line2
            lappend qx [lindex $line2 0]
            lappend qy [lindex $line2 1]
         }

         close $tofile_id

# Count and remove any positions which could not be centroided. These
# are flagged by polcent by returning -100000 for X and Y.
         unset px
         unset py
         unset rlabs
         for {set i 0} {$i < $np} {incr i} {
            set ppx [lindex $qx $i]
            if { $ppx != -100000 } {
               set ppy [lindex $qy $i]
               set lab [lindex $rlabel $i]
               lappend px $ppx
               lappend py $ppy
               lappend rlabs $lab
            } {
               incr nbad
            }
         }

#  Adjust the number of positions to exclude any which could not be
#  centroided.
         set np [expr $np - $nbad]

# Warn the user about the bad positions.
         if { $nbad > 1 } {
            Message "Accurate positions could not be found for $nbad features."
         } elseif { $nbad > 0 } {
            Message "An accurate position could not be found for 1 feature."
         }

# If the position could not be centroided, indicate we have no good
# positions.
      } {
         set px ""
         set py ""
         set nbad $np
         set np 0
      }
   }

# Loop round each good position.
   for {set i 0} {$i < $np} {incr i} {
      set ppx [lindex $px $i]
      set ppy [lindex $py $i]

      if { $rlabel != "" } {
         set rlb [lindex $rlabs $i]
      } {
         set rlb ""
      }

# See if a feature already exists at these pixel coordinates.
      if { [ FindPosn "PX PY" [list $ppx $ppy] 2] != "" } {

# If so then warn the user and ignore the position. The warning is not
# issued if this procedure has been entered as a result of the "Accept"
# button being pressed (as shown by "rlabel" not being blank).
         if { $rlb == "" } {
            Message "An image feature already exists at the specified position."
         }

# If there is no existing feature at this position, create a circle on the
# canvas at the accurate position. This is a temporary marker used to
# indicate that we have a "candidate feature". TestFea returns a list
# holding the X and Y canvas coordinates at the feature.
      } {
         set TEST_PX $ppx
         set TEST_PY $ppy
         TestFea

# Get a label for this position, if required.
         if { $rlb == "" } {
            set lab [GetLabel]
         } {
            set lab $rlb
         }

# Delete the temporary circle used to mark the candidate feature.
         $CAN delete $TEST_ID
         set TEST_ID ""

# If a label was given, create a new position and marker.
         if { $lab != "" } {
            set cxy [NDFToCan $ppx $ppy]
            if { $cxy == "" } { return }
            set cx [lindex $cxy 0]
            set cy [lindex $cxy 1]
            set newi [SetPosn -1 "PX PY CX CY LBL" [list $ppx $ppy $cx $cy $lab] ]
            set id [MarkPosn $newi 0 0 ""]
            SetPosn $newi ID $id
         }
      }
   }

   return $nbad
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
         ShowHelp "POLKA_STATUS_ITEMS_DIALOG"
      }
   }

# Destroy the dialog box.
   destroy $top

}

proc GetLabel {} {
#+
#  Name:
#    GetLabel
#
#  Purpose:
#    Obtain a string with which to label an image feature. Labels are
#    integers. A dialogue box is displayed
#    containing a list of all the known feature labels. The
#    user may select one by clicking on an entry in the list, or may choose
#    to use a new feature label (created automatically). Clicking on
#    an existing image feature in the GWM canvas item causes the
#    corresponding entry in the list box to be selected.
#
#  Arguments:
#    None
#
#  Returned Value:
#    The label to use for the image feature.
#
#  Globals:
#     AUTO_LABEL (Read)
#        Is this the first image to be assigned feature labels? If so, the
#        labels are created automatically without any user intervention.
#     LABBUT (Write)
#        Used to communicate with widget commands.
#     LAST_LABEL (Read and Write)
#        The label from the last image feature to be pointed at.
#     LABELS (Read and Write)
#        A list of all the labels used so far. If a new label is created,
#        then it is appended to this list.
#     LABEL_OFF (Read)
#        A list of the widgets which should be disabled while the
#        dialogue box is displayed.
#     LB (Write)
#        The path to the listbox widget containing the list of known
#        labels.
#     LB_B3 (Write)
#        The name of the "OK" button in the "Select Feature Label" dialog
#        box.
#     NLAB (Read)
#        An array of integers, one for each label in LABELS, giving the
#        number of features (on all images) which have the label.
#-
   global AUTO_LABEL
   global CUROBJ_DISP
   global LABBUT
   global LABELS
   global LABEL_OFF
   global LAST_LABEL
   global LB
   global LB_B3
   global NLAB
   global OBJTYPE

# If a label is required, then automatically select a new label if
# possible.
   if { $AUTO_LABEL } {
      set ret [AutoLabel]

# Otherwise, get the label from the user.
   } {

# Disable selected widgets while this dialog box is active.
      set states {}
      foreach w $LABEL_OFF {
         lappend states [$w cget -state]
         $w configure -state disabled
      }

# Indicate that clicks in the GWM display are now being used to select
# feature labels.
      SetMode 3

# Create the top level window for the dialogue box, and set its title.
      set top .feature
      set topf [MakeDialog $top "Select feature label" 0]

# Create and pack a frame for the "OK", "NEW", "CANCEL" and "HELP" buttons at
# the left hand side.
      set f3 [frame $topf.f3]
      pack $f3 -side left -fill y -padx 2m -expand 1

# Create and pack a frame for the listbox title.
      set f4 [frame $topf.f4]
      pack $f4 -side top -fill x -padx 2m

# Create the label and pack it.
      set lab [label $f4.label -text "Existing\nfeatures"]
      pack $lab -side left -padx 1m -pady 1m

# Create and pack the listbox and scroll bar.
      set LB [listbox $topf.lb -relief sunken -bd 2 -yscrollcommand \
              "$topf.sc set" -height 7 -width 7 -exportselection no ]
      SetHelp $LB ".  Click to highlight a label.\n.  Double click to select a label and exit."

      set sc [scrollbar $topf.sc -command "$LB yview" -width 10]
      pack $LB $sc -side left -fill y -padx 1m -pady 1m

# Set up a binding so that double clicking in the list box is like
# pressing the OK button.
      bind $LB <Double-Button-1> "
         set sel \[$LB curselection\]
         set sel0 \[lindex \$sel 0\]
         set LABBUT \[$LB get \$sel0\]
      "

# Enter all the currently used labels into the listbox. Also find the
# index of the current label (stored in global LABEL).
      set init 0
      set ilab 0
      foreach lab $LABELS {
         if { [info exists LAST_LABEL] && $lab == $LAST_LABEL } { set init $ilab }
         incr ilab
         if { $NLAB($lab) > 0 } {
            $LB insert end $lab
         }
      }

# Set the initial selection in the listbox to the current label.
      $LB selection set $init
      $LB see $init

# Create the buttons and pack them into the left hand frame.
      set b1 [button $f3.b1 -text "New" -width 6 -command {
                 set LABBUT [AutoLabel]
              }]
      SetHelp $b1 ".  Press to use an automatically created new label."

      set b2 [button $f3.b2 -text "Cancel" -width 6 -command {
                 set LABBUT ""
              }]
      SetHelp $b2 ".  Press to ignore the image feature."

      set LB_B3 [button $f3.b3 -text "OK" -width 6 -command "
                 set sel \[$LB curselection\]
                 set sel0 \[lindex \$sel 0\]
                 set LABBUT \[$LB get \$sel0\]
              "]
      SetHelp $LB_B3 ".  Press to use the highlighted label."

      set b4 [button $f3.b4 -text "Help" -width 6 \
                     -command {ShowHelp "POLKA_GET_LABEL_DIALOG" }]
      SetHelp $b4 ".  Display help information on the \"Select feature label\" window."

      pack $b4 $LB_B3 $b2 $b1 -side bottom -pady 2m

# Ensure that closing the window from the window manager is like pressing
# the Cancel button.
      wm protocol $top WM_DELETE_WINDOW "set LABBUT \"\""

# Loop until a valid label has been obtained.
      set LABBUT " "
      while { $LABBUT == " " } {

# Wait for the user to make a selection.
         tkwait variable LABBUT

# If the cancel button was pressed, leave the loop.
         if { $LABBUT != "" && $LABBUT != " " } {

# See if the label has already been used. If so, display an error
# message.
            if { [FindPosn LBL $LABBUT 0] != "" } {
               Message "This image already has an \"$OBJTYPE($CUROBJ_DISP)\"  position labelled \"$LABBUT\". Please select a new label."
               set LABBUT " "
            }
         }
      }

# Destroy the dialog box.
      destroy $top

# Assign the label.
      set ret $LABBUT

# Re-instate the original states of the widgets disabled when this
# procedure was entered.
      set i 0
      foreach w $LABEL_OFF {
         $w configure -state [lindex $states $i]
         incr i
      }

# Enter interaction mode 1 ("Enter image features").
      SetMode 0
    }
    return $ret

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
#        to display next to each entry box. These may be blank if no label is required.
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
#        followed if the Help button is pressed. If this is blank then no Help
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
   set nent 0
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
         incr nent

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
         incr nent

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
         incr nent

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

# If there is only one variable being obtained through an entry, then create a
# binding so that pressing the <Return> key behaves like clicking the OK
# button.
   if { $nent == 1 } { bind $top <Return> "set INPUTS_BUTTON ok" }

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
#           LBL - The textual label associated with a position.
#           TAG - A canvas tag associated with the position.
#     args
#        An optional list argument holding the image from which the
#        positions are derived, and the type of objects to be used.
#        If these are not supplied, they default to $IMAGE_DISP and
#        $CUROBJ_DISP.
#
#  Returned Value:
#     The parameter value.
#
#  Globals:
#     CUROBJ_DISP (Read)
#        The type of the current objects displayed.
#     IMAGE_DISP (Read)
#        The displayed image (without section).
#     PNTCX (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas X coordinates.
#     PNTCY (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas Y coordinates.
#     PNTID (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas item identifiers associated with the
#        positions in the list. A value of -1 indicates that no marker is
#        currently drawn for the position.
#     PNTNXT (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of integers representing indices within the lists
#        given by PNTxxx. Each integer gives the index of the next position
#        along the edge of a polygon. The vector starting at position
#        index i, ends at position index given by the i'th element of
#        PNTNXT. If this value is blank ("") then position i is not part of
#        a polygon. If this value is -1, then the end of the vector starting
#        position i is unspecified (in this case the polygon is not
#        closed).
#     PNTPX (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel X coordinates.
#     PNTPY (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel Y coordinates.
#     PNTTAG (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas tags (one for each position).
#     PNTVID (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas item identifiers associated with the
#        vectors between positions in the list. A value of -1 indicates that
#        no line is currently drawn for the position. A blank string
#        indicates that no vector is defined.
#
#-
   global CUROBJ_DISP
   global IMAGE_DISP
   global PNTCX
   global PNTCY
   global PNTID
   global PNTLBL
   global PNTNXT
   global PNTPX
   global PNTPY
   global PNTVID
   global PNTTAG

# Initialise the returned value.
   set ret ""

# Store the image and object type.
   if { $args == "" } {
      set image $IMAGE_DISP
      set object $CUROBJ_DISP
   } {
      set image [lindex $args 0]
      set object [lindex $args 1]
   }

# Do nothing if the list is empty, or an invalid index has been supplied.
   if { [info exists PNTID($image,$object)] } {
      set size [llength $PNTID($image,$object)]
      if { $i > -1 && $i < $size } {

# Get the parameter value.
         upvar #0 PNT$name array
         if { [info exists array($image,$object)] } {
            set ret [lindex $array($image,$object) $i]
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
#        is to be deleted (eg "polka100.TRN_2.TRANSFORM" ).
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
   global HSTRUT
   global HLAB

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
         set HSTRUT [frame $F4.strut -width 0 -height $height]
         pack propagate $HSTRUT 0
         pack $HSTRUT -side left

# Create a message widget to display dynamic help information about
# the widget underneath the pointer.
         set HLAB [message $F4.lab -justify left -textvariable HELP \
                            -anchor w -font $HLP_FONT -width $width]
         pack $HLAB -fill x -expand 1

# Set up the help for the help area.
         SetHelp $F4 "An area which shows brief help on the object under the pointer. More detailed help can be obtained using the Help menu." POLKA_HELP_AREA
      }

# If required, destroy the help frame (if it has not already been destroyed).
   } {
      if { $F4 != "" } {
         destroy $F4
         set F4 ""
      }
   }
}

proc Helper {} {
#+
#  Name:
#     Helper
#
#  Purpose:
#     Selects the text to display in the help area.
#
#  Arguments:
#     None.
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
   global F4
   global HSTRUT
   global HLAB
   global HAREA

# If the vertical strut which stops the help area collapsing when the
# the number of lines in the help area reduces, is smaller than the
# current height of the help area, extend it.
   if { $F4 != "" && $HAREA } {
      set whgt [winfo height $HLAB]
      set shgt [winfo height $HSTRUT]
      if { $shgt < $whgt } {
         $HSTRUT configure -height $whgt
      }
   }

# This function is usually invoked when the pointer enters or leaves a
# widget. The identification of the widget is not reliable if the pointer
# is on the boundary, so pause for 10 milliseconds to allow the pointer
# to get away from the boundary.
   after 10

# Find the lowest level widget under the pointer.
   set x [winfo pointerx .]
   set y [winfo pointery .]
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

proc InvMap {map} {
#+
#  Name:
#     InvMap
#
#  Purpose:
#     Invert a mapping.
#
#  Arguments:
#     map
#       The mapping, as a list of 6 parameter values.
#
#  Returned Value:
#     The new mapping, as a list of 6 parameter values. A null string is
#     returned if the inversion could not be performed, due to missing values,
#     or singularity. The string "ref" is returned to indicate a unit
#     mapping.
#-

# Assume the mapping is undefined.
   set ret ""

# If a unit mapping was supplied, return a unit mapping reference.
   if { $map == "ref" } {
      set ret "ref"

# Only proceed if the mapping is defined.
   } elseif { $map != "" } {

# Extract the values from the list.
      set c1 [lindex $map 0]
      set c2 [lindex $map 1]
      set c3 [lindex $map 2]
      set c4 [lindex $map 3]
      set c5 [lindex $map 4]
      set c6 [lindex $map 5]

# Check that all the required coefficients are available.
      if { $c1 != "" && $c2 != "" && $c3 != "" &&
           $c4 != "" && $c5 != "" && $c6 != "" } {

# Check that the mapping is not singular.
         set den [expr $c2*$c6 - $c5*$c3 ]
         if { $den != 0.0 } {

# Find the new coefficient values.
            set cc1 [expr ( $c4*$c3 - $c1*$c6 ) / $den ]
            set cc2 [expr $c6 / $den ]
            set cc3 [expr -$c3 / $den ]
            set cc4 [expr ( $c1*$c5 - $c4*$c2 ) / $den ]
            set cc5 [expr -$c5 / $den ]
            set cc6 [expr $c2 / $den ]

# Store them in the returned list.
            set ret [list $cc1 $cc2 $cc3 $cc4 $cc5 $cc6]
         }
      }
   }

   return $ret

}

proc InvMapC {fittype} {
#+
#  Name:
#     InvMapC
#
#  Purpose:
#     Invert a mapping supplied in global array MAP_C.
#
#  Arguments:
#     fittype
#        A textual description of the type of mapping being used.
#
#  Globals:
#     INV (Read and Write)
#        If non-zero, then the MAP_C array on exit should hold the
#        inverse mapping. Otherwise, it should hold the forward mapping.
#     MAP_C (Read and Write)
#        An array of 6 coefficients describing the linear mapping as:
#            XX = C1 + C2*X + C3*Y
#            YY = C4 + C5*X + C6*Y
#-
   global INV
   global MAP_C

# See if the supplied mapping is undefined. If it is return an undefined
# mapping.
   if { $MAP_C(1) == "" || $MAP_C(2) == "" || $MAP_C(3) == "" ||
        $MAP_C(4) == "" || $MAP_C(5) == "" || $MAP_C(6) == "" } {
      set MAP_C(1) ""
      set MAP_C(2) ""
      set MAP_C(3) ""
      set MAP_C(4) ""
      set MAP_C(5) ""
      set MAP_C(6) ""
      ConvMap 1 $fittype

# Otherwise, get a list of parameters from the global array of parameters.
   } {
      set map [list $MAP_C(1) $MAP_C(2) $MAP_C(3) $MAP_C(4) $MAP_C(5) $MAP_C(6)]

# Invert the mapping.
      set inv [InvMap $map]

# If the inverse mapping is a unit mapping, store the corresponding
# numerical values, and then create the corresponding "shift, rotation
# magnification" representation.
      if { $inv == "ref" } {
         set MAP_C(1) 0.0
         set MAP_C(2) 1.0
         set MAP_C(3) 0.0
         set MAP_C(4) 0.0
         set MAP_C(5) 0.0
         set MAP_C(6) 1.0
         ConvMap 1 $fittype

# Do the same for any other defined mappings.
      } elseif { $inv != "" } {
         set MAP_C(1) [lindex $inv 0]
         set MAP_C(2) [lindex $inv 1]
         set MAP_C(3) [lindex $inv 2]
         set MAP_C(4) [lindex $inv 3]
         set MAP_C(5) [lindex $inv 4]
         set MAP_C(6) [lindex $inv 5]
         ConvMap 1 $fittype

# If the mapping is not defined, issue a warning, and re-instate the
# original INV flag.
      } {
         Message "The mapping cannot be inverted."
         if { $INV } {
            set INV 0
         } {
            set INV 1
         }
      }
   }
}

proc ImageMapping {image} {
#+
#  Name:
#     ImageMapping
#
#  Purpose:
#     Return the mapping which transforms the pixel coordinates of a
#     certain position on the sky in the supplied image, into the pixel
#     coordinates of the same sky position in the first (reference)
#     image. If an up-to-date mapping is already available, then it is
#     returned. Otherwise, an attempt is made to determine a new mapping.
#     If no mapping can be created for the specified image (for
#     instance, if the required image features have not yet been given by
#     the user), then a null string is returned, but no error is reported.
#     No mapping is created for the reference image itself, and the string
#     "ref" is returned in this case. It is implicitly assumed that a
#     unit mapping exists for the reference image.
#
#  Arguments:
#     image
#        The name of the image.
#
#  Returned Value:
#     A list of 6 parameter values forming a linear mapping, or "ref" (for
#     a unit mapping), or null for an undefined mapping.
#
#  Globals:
#     E_RAY_FEATURES (Read)
#        An integer representing the "E-ray features" object type.
#     MAPTYPE (Read)
#        A list containing the textual descriptions of the available
#        mapping types.
#     FITTYPE (Read)
#        A textual description of the mapping to be used for the image mapping.
#     IMMAP (Read and Write)
#        An array of mappings (one for each image), each being a list of
#        the 6 parameter values representing the linear mapping. A value of
#        "ref" implies a unit mapping. A blank value implies no mapping.
#     O_RAY_FEATURES (Read)
#        An integer representing the "O-ray features" object type.
#     PNTLBL (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of labels associated with the positions in the list.
#     PNTPX (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel X coordinates.
#     PNTPY (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel Y coordinates.
#     PROT_IMMAP (Read and Write)
#        A 1-d array indexed by image. Each element is either "normal" or
#        "disabled", and specified whether the image mapping associated
#        with the image may be changed. A value of "normal" means that the
#        image mapping may be changed; a value of "disabled" means that it
#        may not be changed.
#     RECALC_IMMAP (Read and Write)
#        A 1-d array indexed by image. Each element is a logical flag
#        indicating if the image features for the corresponding image
#        have changed since the image mapping was last found.
#-
   global E_RAY_FEATURES
   global FITTYPE
   global IMAGES
   global IMMAP
   global MAPTYPE
   global O_RAY_FEATURES
   global PNTLBL
   global PNTPX
   global PNTPY
   global PROT_IMMAP
   global RECALC_IMMAP
   global OEMAP

# Initially assume the mapping can be found.
   set ok 1

# Store the name of the first (reference) image.
   set im0 [lindex $IMAGES 0]

# The first (reference) image does not have an explicit mapping stored for it.
# Instead, a unit mapping is implicitly assumed to exist. So do nothing if
# the supplied image is the first (reference) image.
   if { $image != $im0 } {

# First decide whether we need to calculate the mapping. We don't need
# to if the existing mapping is up-to-date with respect to the positions
# lists for the supplied image, and the first (reference) image.
# If an existing mapping is protected, we leave it unchanged.
      set calc 0
      if { ![info exists IMMAP($image)] } {
         set calc 1
         set PROT_IMMAP($image) normal
      } {
         if { $RECALC_IMMAP($image) && $PROT_IMMAP($image) == "normal" } {
            set calc 1
         }
      }

# Find a new mapping if necessary.
      if { $calc } {

# Tell the user what is happening.
         set told [SetInfo "Determining an image mapping. Please wait..." 0]

# Do each of the two images in turn...
         foreach im [list $im0 $image] {

# If the image has some O-ray features, these will be used to define the
# mapping.
            set obj $O_RAY_FEATURES
            set size [llength $PNTPX($im,$obj)]
            if { $size > 0 } {
               set lablist($im) $PNTLBL($im,$obj)
               set pxlist($im) $PNTPX($im,$obj)
               set pylist($im) $PNTPY($im,$obj)

# Otherwise, we can still find the mapping if the image has some E-ray
# features, and an E to O mapping.
            } {
               set ok 0
               set obj $E_RAY_FEATURES
               set lablist($im) $PNTLBL($im,$obj)
               set size [llength $PNTPX($im,$obj)]
               if { $size > 0 } {
                  set oemap [OEMapping $im 1]
                  if { $oemap != "" } {

# Map the E-ray features to get the O-ray features.
                     set ok [TranList $oemap 0 $PNTPX($im,$obj) \
                                      $PNTPY($im,$obj) pxlist($im) pylist($im)]
                  }
               }
            }

# Abort if no usable positions were found.
            if { !$ok } { break }
         }

# If the lists were produced OK, we can now find the mapping which
# registers them.
         if { $ok } {

# Get the numerical index of the fit type to use.
            foreach fittype [array names MAPTYPE] {
               if { $MAPTYPE($fittype) == $FITTYPE } { break }
            }

# Do the fit.
            set ret [Fit $lablist($image) $pxlist($image) $pylist($image) \
                         $lablist($im0) $pxlist($im0) $pylist($im0) \
                         $fittype "Image mapping for $image"]

# If the mapping could not be found, set zero status.
            if { $ret == "" } {
               set ok 0

# If succesfull, store the parameter values which make up the mapping.
            } {
               set IMMAP($image)  $ret
            }
         }

# If the mapping is now up-to-date with respect to the positions lists,
# clear the flag to indicate this.
         if { $ok } {
            set RECALC_IMMAP($image) 0
         }

# Cancel the informative text set earlier in this procedure.
         if { $told } { SetInfo "" 0 }
      }

# If an image mapping is available for this image, return it. Return a null
# string otherwise.
      if { [info exists IMMAP($image) ] } {
         set ret $IMMAP($image)
      } {
         set ret ""
      }

# If the supplied image is the first (reference) image, protect the
# associated unit mapping..
   } {
      set PROT_IMMAP($image) disabled
      set ret "ref"
   }

   return $ret
}

proc Labels {label add} {
#+
#  Name:
#    Labels
#
#  Purpose:
#    Increment or decrement the number of features with a specified label.
#
#  Arguments:
#    label
#      The label to be added or removed.
#    add
#      If add is non-zero, then the usage count of the supplied label is
#      incremented (it is also added to LABELS if it is a new label).
#      If add is zero, then the usage count of the label is decremented.
#      If this results in zero usage, it is removed from LABELS. The
#      supplied value is ignored if "label" is null.
#
#  Globals:
#    AUTO_LABEL (Write)
#      Should feature labels be generated automatically?
#    E_RAY_FEATURES (Read)
#      An integer representing the "E-ray features" object type.
#    CUROBJ_REQ (Read)
#      The type of the current objects to be displayed.
#    IMAGE_DISP (Read)
#      The displayed image (without section).
#    IMAGES (Read)
#      A list of the supplied images (without sections).
#    LABELS (Read and Write)
#      A list of all the feature labels currently in use.
#    NEXT_LABEL (Write)
#      The index of the next automatically generated label.
#    NLAB (Read and Write)
#      An array (indexed by label) giving the number of positions with
#      each label.
#    O_RAY_FEATURES (Read)
#      An integer representing the "O-ray features" object type.
#
#  Notes:
#    - This procedure also determines whether labels can be created
#    automatically or not, and sets the globals AUTO_LABEL and NEXT_LABEL
#    accordingly.
#    - Blank labels are ignored.
#-
   global AUTO_LABEL
   global CUROBJ_REQ
   global E_RAY_FEATURES
   global IMAGES
   global IMAGE_DISP
   global LABELS
   global NEXT_LABEL
   global NLAB
   global O_RAY_FEATURES
   global PNTLBL

# Ignore blank labels.
   if { $label != "" } {

# If we are incrementing the usage of a label...
      if { $add } {

# If an entry for the label already exists in NLAB, incrment it.
         if { [info exists NLAB($label)] } {
            incr NLAB($label)

# Otherwise, create a new entry in NLAB, setting it to 1. Also, append the
# label to LABELS, and increment the index of the next automatic label.
         } {
            set NLAB($label) 1
            lappend LABELS $label
         }

# If we are decrementing the usage of a label, ignore it unless an entry
# exists for the label in NLAB.
      } elseif { [info exists NLAB($label)] } {

# Decrement the usage. If this reesults in zero usage, remove the entry,
# and also remove the label from LABELS.
         incr NLAB($label) -1
         if { $NLAB($label) == 0 } {
            unset NLAB($label)
            set lindex [lsearch -exact $LABELS $label]
            if { $lindex != "" } {
               set LABELS [lreplace $LABELS $lindex $lindex]
            }
         }
      }
   }

# We now see if we can generate labels automatically. This is the case if
# the current image/object type combination is the only one to have any
# features with non-blank labels. Initially assume we can generate auto
# labels.
   set AUTO_LABEL 1

# Loop round all images...
   foreach image $IMAGES {

# Loop round each object type...
      foreach object [list $O_RAY_FEATURES $E_RAY_FEATURES] {

# Jump over the current image and object.
         if { $image != $IMAGE_DISP || $object != $CUROBJ_REQ } {

# If this image and object have any feature labels...
            if { [info exists PNTLBL($image,$object)] } {

# If any of the labels are not blank, set the AUTO_LABEL flag false and
# leave the loop.
               foreach lab $PNTLBL($image,$object) {
                  if { $lab != "" } {
                     set AUTO_LABEL 0
                     break
                  }
               }
            }
         }

         if { !$AUTO_LABEL } { break }
      }

      if { !$AUTO_LABEL } { break }
   }

# If there are currently no defined labels, reset the next auto label
# index.
   if { [llength $LABELS] == 0 } { set NEXT_LABEL 0 }

}

proc LoadOptions {} {
#+
#  Name:
#     LoadOptions
#
#  Purpose:
#     Copy the option values supplied in the communications file (if any)
#     to global variables where they can be access and modified.
#
#  Arguments:
#     None.
#
#  Globals:
#      ATASK (Write)
#         Set to 1 if the script has been activated by the polka atask
#         (as shown by the existence of any of the ATASK_... variables).
#      ATASK_HAREA (Read)
#         The value of HAREA supplied by the A-task.
#      ATASK_SAREA (Read)
#         The value of SAREA supplied by the A-task.
#      ATASK_PSF (Read)
#         The value of PSF_SIZE supplied by the A-task.
#      ATASK_SI (Read)
#         A string representing the SI_LIST list.
#      DBEAM (Read)
#         Is Polka being run in dual-beam mode?
#      HAREA (Write)
#         Should the help area be displayed?
#      SAREA (Write)
#         Should the status area be displayed?
#      PSF_SIZE (Write)
#         The typical size of a feature, in pixels.
#      SI_LIST (Write)
#         A list of indices identifying the status items to be displayed
#         in the status area.
#      SI_VARS (Read)
#         A list of all global variable names available for display in the
#         status area.
#
#-
   global ATASK
   global ATASK_BADCOL
   global ATASK_CURCOL
   global ATASK_FIT
   global ATASK_HAREA
   global ATASK_INTERP
   global ATASK_LOGFILE
   global ATASK_OEFIT
   global ATASK_PHI
   global ATASK_PLO
   global ATASK_POLMODE
   global ATASK_PSF
   global ATASK_REFCOL
   global ATASK_SAREA
   global ATASK_SELCOL
   global ATASK_SI
   global ATASK_SKYOFF
   global ATASK_VIEW
   global ATASK_XHAIR
   global ATASK_XHRCOL
   global ATASK_SKYPAR
   global BADCOL
   global CHAR_LIST
   global CHAR_STOP
   global CURCOL
   global DBEAM
   global FITTYPE
   global HAREA
   global INTERP
   global LOGFILE_ID
   global MAPTYPE
   global OEFITTYPE
   global PHI_REQ
   global PLO_REQ
   global POLMODE
   global PSF_SIZE
   global REFCOL
   global SAREA
   global SELCOL
   global SI_LIST
   global SI_VARS
   global SKYOFF
   global SKYPAR
   global STOKES
   global VIEW
   global XHAIR
   global XHRCOL

# Initially assume the script was not activated from the polka atask.
   set ATASK 0

# If the Atask has specified a value for the option, copy it from the
# variable used to communicate with the atask, to a variable which
# can be used and modified within the script. If no value was supplied,
# set a default.
   if { [info exists ATASK_HAREA] } {
     set HAREA $ATASK_HAREA
     set ATASK 1
   } {
     set HAREA 1
   }

# Do the same for the other options.
   if { [info exists ATASK_SKYOFF] } {
      set SKYOFF $ATASK_SKYOFF
      set ATASK 1
   } {
      set SKYOFF 1
   }

   if { [info exists ATASK_XHAIR] } {
      set XHAIR $ATASK_XHAIR
      set ATASK 1
   } {
      set XHAIR 0
   }

   if { [info exists ATASK_XHRCOL] } {
     set XHRCOL [string trim $ATASK_XHRCOL]
     set ATASK 1
   } {
     set XHRCOL "yellow"
   }

   if { [info exists ATASK_VIEW] } {
      if { [regexp -nocase {Unzoomed} $ATASK_VIEW] } {
        set VIEW "Unzoomed"
      } {
        set VIEW "Zoomed"
      }
      set ATASK 1
   } {
     set VIEW "Zoomed"
   }

   if { [info exists ATASK_INTERP] } {
     set INTERP $ATASK_INTERP
     set ATASK 1
   } {
     set INTERP Linear
   }

   if { [info exists ATASK_SKYPAR] } {
     set SKYPAR $ATASK_SKYPAR
     set ATASK 1
   } {
     set SKYPAR 0
   }

   if { [info exists ATASK_PLO] } {
     set PLO_REQ [format "%5.1f" $ATASK_PLO]
     set ATASK 1
   } {
     set PLO_REQ 5.0
   }

   if { [info exists ATASK_PHI] } {
     set PHI_REQ [format "%5.1f" $ATASK_PHI]
     set ATASK 1
   } {
     set PHI_REQ 95.0
   }

   if { [info exists ATASK_CURCOL] } {
     set CURCOL [string trim $ATASK_CURCOL]
     set ATASK 1
   } {
     set CURCOL "red"
   }

   if { [info exists ATASK_REFCOL] } {
     set REFCOL [string trim $ATASK_REFCOL]
     set ATASK 1
   } {
     set REFCOL "green"
   }

   if { [info exists ATASK_BADCOL] } {
     set BADCOL [string trim $ATASK_BADCOL]
     set ATASK 1
   } {
     set BADCOL "cyan"
   }

   if { [info exists ATASK_SELCOL] } {
     set SELCOL [string trim $ATASK_SELCOL]
     set ATASK 1
   } {
     set SELCOL "red"
   }

   if { [info exists ATASK_SAREA] } {
     set SAREA $ATASK_SAREA
     set ATASK 1
   } {
     set SAREA 1
   }

   if { [info exists ATASK_PSF] } {
      set PSF_SIZE $ATASK_PSF
      set ATASK 1
   } {
      set PSF_SIZE 3
   }

   if { [info exists ATASK_FIT] } {
      set FITTYPE $MAPTYPE($ATASK_FIT)
      set ATASK 1
   } {
      set FITTYPE $MAPTYPE(1)
   }

   if { $STOKES } {
      if { [info exists ATASK_POLMODE] } {
         if { [string toupper $ATASK_POLMODE] == "LINEAR" } {
            set POLMODE Linear
         } {
            set POLMODE Circular
         }
         set ATASK 1
      } {
         set POLMODE Linear
      }
   } {
      set POLMODE "<none>"
   }

# In single-beam mode, there are no O-E mappings, and so OEFITTYPE is not
# used.
   if { $DBEAM } {
      if { [info exists ATASK_OEFIT] } {
         set OEFITTYPE $MAPTYPE($ATASK_OEFIT)
         set ATASK 1
      } {
         set OEFITTYPE $MAPTYPE(1)
      }
   } {
      set OEFITTYPE $MAPTYPE(0)
   }

   if { [info exists ATASK_LOGFILE] } {
      if { [string tolower $ATASK_LOGFILE] == "stdout" } {
         set LOGFILE_ID stdout
      } {
         set LOGFILE_ID [open $ATASK_LOGFILE w]
      }
      set ATASK 1
   } {
      set LOGFILE_ID ""
   }

# SI_LIST is done differently. Each character in ATASK_SI represents an
# integer to be appended to the SI_LIST list. Initialise SI_LIST.
   set SI_LIST ""

# If a value for ATASK_SI has been supplied...
   if { [info exists ATASK_SI] } {
      set ATASK 1

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
# may be extra items available now (if the user hasn't used Polka for
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
#       A list of the names of the ADAM tasks started up by Polka.

#-
   global ADAM_TASKS
   global ADAM_USER
   global RENDEVOUS
   global TASK_FILE

# Load the task.
   set taskload [list adamtask $task $file ]
   if {[catch $taskload error] != 0} {
      puts "Error loading task $task (file $file): \"$error\". Aborting..."
      Message "Error loading task $task (file $file): \"$error\". Aborting..."
      exit 1
   }

# Poll for the task to attach to the message system.
   set count 0
   while {[$task path] == 0} {
      after 100
      incr count
      if {$count > 100} {
         puts "Timed out waiting for task \"$task\" (file $file) to start. Aborting..."
         Message "Timed out waiting for task \"$task\" (file $file) to start. Aborting..."
         $task kill
         exit 1
      }
   }

# Append the name of the task to the list of tasks started up so far.
   lappend ADAM_TASKS $task

# Save the name of the rendevous file.
   foreach rfile [glob -nocomplain $ADAM_USER/${task}_*] {
      if { [regexp "${task}_\[0-9\]+\$" $rfile] } {
         set RENDEVOUS($task) $rfile
         break
      }
   }

   if { ![info exists RENDEVOUS($task)] } {
      puts "Cannot find the rendevous file for $task."
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
#     F_OWNER (Read and Write)
#        The name of the Polka widget which "owns" the focus. It is
#        temporarily changed to be the toplevel containing the dialogue
#        box. It is reset to its original value when the label has been
#        obtained.
#     TOP (Read)
#        The path to the main application window.
#-
   global F_OWNER
   global TOP

# Create the top level window for the dialogue box, and set its title.
# It inherits the (potentially private) colour map used by the main
# application window.
   set top [toplevel $w -colormap $TOP]
   wm title $top "Polka - $title"

# Set the F_OWNER variable so that this window is handed the focus by the
# main Polka window. Set the current value to be re-instated later.
   set old_f_owner $F_OWNER
   set F_OWNER $top
   focus $top

# Attempt put a grab on this window, so that other windows become
# inactive. This is a bit fragile so put the grab inside a catch so that
# an error in grab will not abort the application.
   if { $grab } { catch "grab $top" }

# Create a frame to hold everything else so that we can have a blank
# border round the other widgets.
   set topf0 [frame $top.f0 -bd 3 -relief raised]
   set topf [frame $topf0.f ]

# Pack the frame holding everything else.
   pack $topf
   pack $topf0 -padx 2m -pady 2m -ipadx 2m -ipady 2m

# Create a binding so that when the dialog box is destroyed,
# the focus is handed back to the original window.
   bind $top <Destroy> "set F_OWNER $old_f_owner
                        focus $old_f_owner"

# Return the name of the frame to contain everything else.
   return $topf
}

proc Mappings {image} {
#+
#  Name:
#     Mappings
#
#  Purpose:
#     Ensure that the image and OE mappings for the supplied image are
#     up-to-date. New mappings are only created if the relevant positions
#     lists have changed since the current mappings were created.
#
#  Arguments:
#     image
#        The image for which mappings are required.
#
#  Returned Value:
#     1 is returned if both mappings are usable on exit. Otherwise,
#     zero is returned.
#
#  Globals:
#      DBEAM (Read)
#         Is Polka being run in dual-beam mode?
#-
   global DBEAM

# If we are in dual-beam mode, attempt to create the mapping from E to O
# ray for the supplied image.
   if { $DBEAM } {
      set oe_ok [OEMapping $image]
   } {
      set oe_ok 1
   }

# Attempt to create the mapping from the supplied image to the first
# (reference) image.
   set im_ok [ImageMapping $image]

# If both mappings were created OK, return 1 (success). Otherwise, return
# zero (failure).
   if { $im_ok != "" && $oe_ok != "" } {
      set ret 1
   } {
      set ret 2
   }

   return $ret
}

proc MapRefs {px_name py_name} {
#+
#  Name:
#     MapRefs
#
#  Purpose:
#     Returns pixel coordinates for the reference objects, mapped into
#     the frame of the current objects (if possible). If this
#     cannot be done (due to the required mappings not yet being known),
#     then an error message is displayed and the returned coordinates refer
#     to the original frame of the reference objects.
#
#  Arguments:
#     px_name
#        The name of the list in which to store the returned pixel X
#        coordinates.
#     py_name
#        The name of the list in which to store the returned pixel Y
#        coordinates.
#
#  Globals:
#     CUROBJ_REQ (Read)
#        The type of the current objects to be displayed.
#     IMAGE_DISP (Read)
#        The displayed image (without section).
#     O_RAY_FEATURES (Read)
#        An integer representing the "O-ray features" object type.
#     O_RAY_MASK (Read)
#        An integer representing the "O-ray mask" object type.
#     PNTPX (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel X coordinates.
#     PNTPY (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel Y coordinates.
#     REFIM_REQ (Read)
#        The requested image from which the displayed reference objects
#        should be derived.
#     REFOBJ_REQ (Read)
#        The requested type of reference objects to be displayed.
#-
   global CUROBJ_REQ
   global IMAGE_DISP
   global E_RAY_MASK
   global O_RAY_FEATURES
   global O_RAY_MASK
   global O_RAY_SKY
   global PNTPX
   global PNTPY
   global REDRAW
   global REFALN
   global REFIM_REQ
   global REFOBJ_REQ

   upvar $px_name lpx
   upvar $py_name lpy

# Tell the user what is happening.
   set told [SetInfo "Mapping reference objects. Please wait... " 0]

# See if the current objects refer to the O or E ray.
   if { $CUROBJ_REQ == $O_RAY_FEATURES ||
        $CUROBJ_REQ == $O_RAY_MASK ||
        $CUROBJ_REQ == $O_RAY_SKY } {
      set cur_ray "O"
   } {
      set cur_ray "E"
   }

# See if the reference objects refer to the O or E ray.
   if { $REFOBJ_REQ == $O_RAY_FEATURES ||
        $REFOBJ_REQ == $O_RAY_MASK ||
        $REFOBJ_REQ == $O_RAY_SKY } {
      set ref_ray "O"
   } {
      set ref_ray "E"
   }

# See if the reference object is a mask.
   if { $REFOBJ_REQ == $E_RAY_MASK ||
        $REFOBJ_REQ == $O_RAY_MASK } {
      set ref_mask 1
   } {
      set ref_mask 0
   }

# If the reference objects are E-ray objects, the first mapping maps them
# into the O-ray frame.
   if { $ref_ray == "E" } {
      set m1 [OEMapping $REFIM_REQ]
   } {
      set m1 "ref"
   }

# Get the mapping from the reference image to the first image. Masks are
# assumed to be aligned in all images, so use a unit ("ref") mapping if
# we are drawing a reference mask. If the current and reference images
# are the same, we do not need to go via the first image. In this case
# pretend that the first image is the reference image (i.e. use a unit
# mapping).
   if { $ref_mask || $REFIM_REQ == $IMAGE_DISP } {
      set m2 "ref"
   } {
      set m2 [ImageMapping $REFIM_REQ]
   }

# Combine it with the first mapping. This gives the mapping from the
# reference frame (E or O) to the O-ray frame of the first image.
   set m21 [ConcMap $m1 0 $m2 0]

# Get the mapping from the current image to the first image. If the
# current and reference images are the same, pretend the first image is
# the reference image.
   if { $ref_mask || $REFIM_REQ == $IMAGE_DISP } {
      set m3 "ref"
   } {
      set m3 [ImageMapping $IMAGE_DISP]
   }

# Combine this with the total mapping so far, to get the mapping from the
# reference frame to the O-ray frame in the current image.
   set m321 [ConcMap $m21 0 $m3 1]

# If the current objects are E-ray objects, then the final mapping, maps
# them from the O-ray to the E-ray frame.
   if { $cur_ray == "E" } {
      set m4 [OEMapping $IMAGE_DISP]
   } {
      set m4 "ref"
   }
   set map [ConcMap $m321 0 $m4 1]

# Initialise the returned positiosn to equal the supplied positions.
   set lpx $PNTPX($REFIM_REQ,$REFOBJ_REQ)
   set lpy $PNTPY($REFIM_REQ,$REFOBJ_REQ)

# If the required mapping is not defined, warn the user.
   if { $map == "" } {
      Message "At least one of the mappings required to align the reference features with the displayed image is not yet known. Reference features will therefore be drawn in their un-mapped positions."
      set REFALN 0
      $REDRAW configure -state disabled

# Otherwise, if the mapping is a unit mapping, leave the supplied positions
# unchanged.
   } elseif { $map != "ref" } {
      TranList $map 0 $lpx $lpy lpx lpy
   }

# Cancel the informative text set earlier in this procedure.
   if { $told } { SetInfo "" 0 }
}

proc MarkBind {i image object} {
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
#     image
#        The image to which the position refers.
#     object
#        The type of object described by the position.
#-
   global CAN

# Get the canvas item identifier for the marker. Only proceed if a marker
# is currently displayed.
   set id [GetPosn $i ID $image $object]
   if { $id != -1 } {

# Get the label and pixel coordinates desribing the position.
      set label [GetPosn $i LBL $image $object]
      set px [GetPosn $i PX $image $object]
      set py [GetPosn $i PY $image $object]

# Format the pixel coordinates.
      set pxy [format "( %.1f, %.1f )" $px $py ]

# Assign the position's label to LABEL when the pointer enters it. Also
# assign its pixel coordinates to PXY. Global LAST_LABEL is a latching
# value which stores the label of the last feature pointed at until a new
# feature is pointed at.
      $CAN bind  $id <Enter> \
        "if { \"$label\" != \"\" } {
            set LABEL \"$label\"
            set LAST_LABEL \$LABEL
         } {
            set LABEL \"<unknown>\"
         }
         set PXY \"$pxy\""

# Set LABEL and PXY blank when the pointer leaves the position.
      $CAN bind $id <Leave> \
         "set LABEL {}
          set PXY {}"
   }
}

proc MarkPosn {i ref vertex tags} {
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
#     ref
#        If non-zero, then a reference marker is drawn, otherwise a current
#        objects marker is drawn.
#     vertex
#        If non-zero, then the position is marked as a vertex, otherwise
#        it is marked as a feature.
#     tags
#        A list of additional tags to give to the created canvas item.
#
#  Returned Value:
#      The canvas item identifier for the marker.
#
#  Globals:
#     CAN (Read)
#        Path to the canvas containing the GWM image display.
#     CUROBJ_REQ (Read)
#        The type of the current objects to be displayed.
#     IMAGE_DISP (Read)
#        The displayed image (without section).
#     REFOBJ_REQ (Read)
#        The type of the reference objects displayed.
#     REFIM_REQ (Read)
#        The reference image (without section).
#     PNTCX (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas X coordinates.
#     PNTCY (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas Y coordinates.
#     PNTID (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas item identifiers associated with the
#        positions in the list. A value of -1 indicates that no marker is
#        currently drawn for the position.
#-
   global CAN
   global CURCOL
   global CUROBJ_REQ
   global IMAGE_DISP
   global REFCOL
   global REFOBJ_REQ
   global REFIM_REQ
   global PNTCX
   global PNTCY
   global PNTID
   global PNTTAG
   global POLPACK_DIR

# Store the colour, image and object type.
   if { $ref } {
      set colour $REFCOL
      set image $REFIM_REQ
      set object $REFOBJ_REQ
   } {
      set colour $CURCOL
      set image $IMAGE_DISP
      set object $CUROBJ_REQ
   }

# Set up the bitmap and tags depending on whether the position is a
# feature or a polygon vertex.
   if { !$vertex } {
      lappend tags features
      if { $ref } {
         set bitmap $POLPACK_DIR/rfeature.bit
      } {
         set bitmap $POLPACK_DIR/feature.bit
      }
   } {
      if { $ref } {
         lappend tags rvertices
         set bitmap $POLPACK_DIR/rvertex.bit
      } {
         lappend tags vertices
         set bitmap $POLPACK_DIR/vertex.bit
      }
   }

# Get the canvas coordinates for the marker.
   set cx [lindex $PNTCX($image,$object) $i]
   set cy [lindex $PNTCY($image,$object) $i]

# If a canvas item marking the position has not already been created,
# create one now, and store its index.
   set id [lindex $PNTID($image,$object) $i]
   lappend tags [lindex $PNTTAG($image,$object) $i]

   if { $id == -1 } {
      set id [$CAN create bitmap $cx $cy -bitmap @$bitmap \
                          -foreground $colour -tags $tags]
      set PNTID($image,$object) [lreplace $PNTID($image,$object) \
                                          $i $i $id]

# If a canvas item already exists, configure it to have the correct
# properties.
   } {
      $CAN coords $id $cx $cy
      $CAN itemconfigure $id -bitmap @$bitmap -foreground $colour -tags $tags
   }

# Set up bindings which assign the position's label to the global LABEL
# when the pointer enters the marker, and sets LABEL blank when the
# pointer leaves the marker. Also, store the pixel coordinates in
# PXY.
   MarkBind $i $image $object

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

# Arrange for a blank help string to be displayed when the pointer
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

# Ignore separators and tearoffs
   set mty [$win type @$y]
   if { $mty != "separator" && $mty != "tearoff" } {

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
#     CAN (Read)
#        The name of the canvas widget holding the GWM image.
#     LABEL (Read)
#        The label from the image feature under the pointer (if any).
#     LB (Read)
#        The name of the listbox widget containing a list of all the
#        image feature labels. It is part of the "Select feature label"
#        dialog box.
#     MODE (Read)
#        The interaction mode determining how to process button clicks
#        and motion in the GWM canvas item. Modes are:
#           0 - The user specifies image features by clicking on them.
#           1 - The user starts a new mask polygon, or edits an existing one.
#           2 - The user completes a started mask polygon.
#           3 - The user selects a label by clicking on an existing feature.
#     VID2 (Read)
#        The canvas item id for the vector starting at the previous vertex,
#        which ends at the current pointer position.
#-
   global CAN
   global LABEL
   global LB
   global MODE
   global VID2
   global POINTER_PXY
   global POINTER_CXY

# Convert the screen coords to canvas coords.
   set cx [$CAN canvasx $x]
   set cy [$CAN canvasy $y]

# If a cross-hair has been requested instead of a pointer, then move the
# positions of the lines making up the cross hair.
   Xhair $cx $cy

# Store the canvas coordinates of the pointer in POINTER_CXY.
   set POINTER_CXY [format "( %.1f, %.1f )" $cx $cy]

# Store the pixel coordinates of the pointer in POINTER_PXY.
   set pxy [CanToNDF $cx $cy]
   if { $pxy == "" } { return }

   set px [lindex $pxy 0]
   set py [lindex $pxy 1]
   set POINTER_PXY [format "( %.1f, %.1f )" $px $py ]

# The global variable MODE determines how events over the canvas are
# processed. If we are in mode 2 ("enter a new polygon"), the "loose" end
# of the most recent vector is bound to the pointer.
   if { $MODE == 2 } {

# Move the "loose" end of the vector which starts at the previous vertex,
# so that it follows the pointer.
      set coords [$CAN coords $VID2]
      $CAN coords $VID2 [lindex $coords 0] [lindex $coords 1]  $cx $cy

# If we are in mode 3 ("waiting for a feature label"), select the label
# of the feature under the pointer (if any) in the list box in the
# "Select feature label" dialog box.
   } elseif { $MODE == 3 } {

# Get a list of all the labels in the listbox.
      set labs [$LB get 0 end]

# Search it for the current feature's label (stored in global LABEL).
      if { $LABEL != "<unknown>" } {
         set j [lsearch -exact $labs $LABEL]
      } {
         set j -1
      }

# If found, select the corresponding entry in the list box.
      if { $j != -1 } {
         $LB selection clear 0 end
         $LB selection set $j
         $LB see $j
      }
   }
}

proc GetParamED {task param} {
#+
#  Name:
#     GetParamED
#
#  Purpose:
#     Returns the value of an ATASK parameter substituing "E" exponents for
#     "D" exponents. Also removes delimiting quotes.
#
#  Arguments:
#     task
#        The name of the task
#     param
#        The name of the parameter, in the form "<application>:<parameter>"
#
#  Returned Value:
#     The parameter value.
#
#-
   regsub -nocase -all D [GetParam $task $param] E res
   regexp {^\'(.*)\'$} $res match res
   return $res
}

proc NDFToCan { px py } {
#+
#  Name:
#    NDFToCan
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
#    The X and Y canvas coordinates as a list of two values, or a blank
#    string if anything goes wrong.
#
#  Globals:
#    CX (Read)
#       The X offset for converting from canvas
#       coordinates to NDF pixel coordinates.
#    CY (Read)
#       The Y offset for converting from canvas
#       coordinates to NDF pixel coordinates.
#    MX (Read)
#       The X scale factor for converting from canvas
#       coordinates to NDF pixel coordinates.
#    MY (Read)
#       The Y scale factor for converting from canvas
#       coordinates to NDF pixel coordinates.
#
#  Notes:
#    -  This is the inverse of procedure CanToNDF
#-

   global CX
   global CY
   global MX
   global MY

# Get the canvas coordinates.
   if { $px != "" && $py != "" } {
      set cxy [list [ expr ( $px - $CX ) / $MX] [ expr ( $py - $CY ) / $MY] ]
   } {
      set cxy ""
   }

# Return the coordinates.
   return $cxy
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
#        If a non-blank value is supplied, then the returned value is the
#        number of positions in the polygon starting at position with
#        index $v0. Otherwise, the number of positions of any description
#        is returned.
#     args
#        An optional list argument holding the image from which the
#        positions are derived, and the type of objects. If these are
#        not supplied, they default to $IMAGE_DISP and $CUROBJ_DISP.
#
#  Returned Value:
#        The number of positions.
#
#  Globals:
#     CUROBJ_DISP (Read)
#        The type of the current objects displayed.
#     IMAGE_DISP (Read)
#        The displayed image (without section).
#     PNTID (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas item identifiers associated with the
#        positions in the list.
#     PNTNXT (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of integers representing indices within the lists
#        given by PNTxxx. Each integer gives the index of the next position
#        along the edge of a polygon. The vector starting at position
#        index i, ends at position index given by the i'th element of
#        PNTNXT. If this value is blank ("") then position i is not part of
#        a polygon. If this value is -1, then the end of the vector starting
#        position i is unspecified (in this case the polygon is not
#        closed).
#
#-
   global IMAGE_DISP
   global CUROBJ_DISP
   global PNTID
   global PNTNXT

# Store the image and object type.
   if { $args == "" } {
      set image $IMAGE_DISP
      set object $CUROBJ_DISP
   } {
      set image [lindex $args 0]
      set object [lindex $args 1]
   }

# Return zero if the specified positions list does not exist.
   if { ![info exists PNTID($image,$object)] } {
      set size 0

# Otherwise...
   } {

#  If the total number of positions in the list is required...
      if { $v0 == "" } {

# Get the size of the list.
         set size [llength $PNTID($image,$object)]

# If the number of vertices in the polygon is required...
      } {

# Loop round counting the vertices until we arrive back at the start, or an
# unattached vertex is found.
         set nxt [lindex $PNTNXT($image,$object) $v0]
         if { $nxt != "" } {
            set size 1
            while { $nxt != $v0 && $nxt != -1 } {
               incr size
               set nxt [lindex $PNTNXT($image,$object) $nxt]
            }
         } {
            set size 0
         }
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
#       application. A blank string must be supplied if no
#       command line parameter assignments are needed.
#    args
#       o  If the optional string "noreport" is supplied, then any error
#       messages generated by the action are not displayed.
#       o  If the name of a currently defined global variable is supplied,
#       then the variable is assumed to be a 1-D array, indexed by A-task
#       parameter name. The associated values are the values to supply for
#       the A-task's parameters if they are prompted for.
#       o  The presence of any other non-blank value after "params" causes
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
      } elseif { [lindex $args 0] != "" } {
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

# Set the return status. If the final status does not contain the string
# DTASK__ACTCOMPLET, assume the action failed.
   if { ![regexp {DTASK__ACTCOMPLET} $STATUS] } {
      set ok 0
   } {
      set ok 1
   }

# Display any error messages.
   if { $report && $ADAM_ERRORS != "" } {
      Message "$task action \"$action\" reported:\n$ADAM_ERRORS"
   }

# If failure is fatal, shut down.
   if { !$ok && $abort } {exit 1}

# Indicate that we are no longer executing an ADAM action.
   set ACTION ""

# Return the status. Return zero if the operation was cancelled by the
# user.
   if { $CANCEL_OP } { set ok 0 }

   return $ok
}

proc OEMapping {image} {
#+
#  Name:
#     OEMapping
#
#  Purpose:
#     Return the mapping which transforms the pixel coordinates of a
#     given sky position in the E ray area, into the pixel coordinates
#     of the same sky positions within the O ray area of the specified image.
#     If an up-to-date mapping is already available, then it is returned.
#     Otherwise, an attempt is made to determine a new OE mapping.
#     If no OE mapping can be created for the specified image (for
#     instance, if the required image features have not yet been given by
#     the user), then a default mapping is returned if possible. This is
#     the most recent OE mapping to be determined (this is done on
#     the assumption that OE mappings will be more or less the same for
#     all images). No errors are reported if the mapping cannot be created
#     due to lack of image features.
#
#  Arguments:
#     image
#        The name of the image.
#
#  Returned Value:
#     A list of 6 parameter values describing the linear mapping.
#
#  Globals:
#     DEF_OEMAP (Read and Write)
#       The most recent OE Mapping to be found succesfully.
#     E_RAY_FEATURES (Read)
#        An integer representing the "E-ray features" object type.
#     MAPTYPE (Read)
#        A list containing the textual descriptions of the available
#        mapping types.
#     OEFITTYPE (Read)
#        A textual description of the mapping to be used for the OE mapping.
#     OEMAP (Read and Write)
#       An array of mappings (one for each image), each being a list of
#       6 parameter values.
#     O_RAY_FEATURES (Read)
#        An integer representing the "O-ray features" object type.
#     PNTLBL (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of labels associated with the positions in the list.
#     PNTPX (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel X coordinates.
#     PNTPY (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel Y coordinates.
#     PROT_OEMAP (Read and Write)
#        A 1-d array indexed by image. Each element is either "normal" or
#        "disabled", and specified whether the OE mapping associated
#        with the image may be changed. A value of "normal" means that the
#        mapping may be changed; a value of "disabled" means that it
#        may not be changed.
#     RECALC_OEMAP (Read and Write)
#        A 1-d array indexed by image. Each element is a logical flag
#        indicating if the image features for the corresponding image
#        have changed since the image's E to O mapping was last found.
#-
   global DEF_OEMAP
   global E_RAY_FEATURES
   global IMAGES
   global MAPTYPE
   global OEFITTYPE
   global OEMAP
   global O_RAY_FEATURES
   global PNTLBL
   global PNTPX
   global PNTPY
   global PROT_OEMAP
   global RECALC_OEMAP

# If this is the first time the mapping has been requested, create a
# default null mapping, and indicate it is not protected.
   if { ![info exists OEMAP($image)] } {
      set OEMAP($image) ""
      set PROT_OEMAP($image) normal

# Otherwise, if the stored OE mapping is out-of-date (i.e. if the image
# features have changed since the stored mapping was calculated) then we
# cannot use the stored mapping, so clear it. We only do this if the
# stored mapping is not protected.
   } {
      if { $RECALC_OEMAP($image) && $PROT_OEMAP($image) == "normal" } {
         set OEMAP($image) ""
      }
   }

# If we now have no mapping, we need to find a new mapping (if allowed).
   if { $OEMAP($image) == "" && $PROT_OEMAP($image) == "normal" } {

# Tell the user what is happening.
      set told [SetInfo "Determining an O-E mapping. Please wait... " 0]

# Get the numerical index of the fit type to use.
      foreach fittype [array names MAPTYPE] {
         if { $MAPTYPE($fittype) == $OEFITTYPE } { break }
      }

# Attempt to do the fit. This will return a null string if there are
# currently insufficient image features defined to do the fit.
      set ret [Fit $PNTLBL($image,$E_RAY_FEATURES) \
                   $PNTPX($image,$E_RAY_FEATURES) \
                   $PNTPY($image,$E_RAY_FEATURES) \
                   $PNTLBL($image,$O_RAY_FEATURES) \
                   $PNTPX($image,$O_RAY_FEATURES) \
                   $PNTPY($image,$O_RAY_FEATURES) $fittype \
                   "OE mapping for $image"]

# If the fit was done succesfully, store the mapping. Also store a copy
# of it as the current default OE Mapping.
      if { $ret != "" } {
         set OEMAP($image) $ret
         set DEF_OEMAP $ret

# If the fit could not be performed, use the current default OE mapping.
      } {
         set OEMAP($image) $DEF_OEMAP
      }

# Indicate the mapping is now up-to-date.
      set RECALC_OEMAP($image) 0

# Cancel the informative text set earlier in this procedure.
      if { $told } { SetInfo "" 0 }

   }

# Return the stored OE mapping.
   return $OEMAP($image)
}

proc OpenFile {mode title text lfile lfd} {
#+
#  Name:
#     OpenFile
#
#  Purpose:
#     Open a file for reading or writing, reporting any errors which occur.
#     The user is asked to conform that it is ok to over-write an existing
#     file.
#
#  Arguments:
#     mode
#        The access mode; "r" for read-only, anything else produces
#        write access.
#     title
#        The string to use as the dialog window title.
#     text
#        A string to display as a label above the entry widget.
#     lfile
#        The name of the variable to recieve the file name.
#     lfd
#        The name of the variable to recieve the file descriptor
#        for the opened file.
#
#  Returned Value:
#     Zero if no file was opened, one otherwise.
#-
   global F_OWNER
   global OPFILE_EXIT

   upvar $lfd fd
   upvar $lfile file

# Assume no file is opened
   set ret 0

# Create the top level window for the dialogue box.
   set top .openfile
   set topf [MakeDialog $top $title 1]

# Pack a label displaying the supplied text (if any).
   if { $text != "" } {
      pack [label $topf.lab -text $text] -expand 1 -fill x -side top -pady 2m
   }

# Create and pack an entry widget
   set ent [entry $topf.ent -width 40]
   pack $ent -side top -expand 1 -fill x -pady 4m

# Pre-load the current value of the file name (if not blank).
   if { [info exists file] && $file != "" } { $ent insert 0 $file }

# Bind <Return> in the entry to the OK button.
   bind $ent <Return> "set OPFILE_EXIT ok"

# Give the focus to the entry.
   set F_OWNER $ent
   focus $ent

# Create the OK and Cancel buttons, but don't pack them yet.
   set butfrm [frame $topf.butfrm]
   set b1 [button $butfrm.ok -text "OK" -command "set OPFILE_EXIT ok"]
   set b3 [button $butfrm.cancel -text "Cancel" -command "set OPFILE_EXIT cancel"]

   SetHelp $b1 ".  Press to close the dialog box, adopting the currently displayed file name."
   SetHelp $b3 ".  Press to close the dialog box, cancelling the current operation."

# Now pack the nuttons so that they appear at the bottom of the dialog box.
   pack $butfrm -fill x -expand 1
   pack $b1 $b3 -side left -expand 1

# Ensure that closing the window from the window manager is like pressing
# the Cancel button.
   wm protocol $top WM_DELETE_WINDOW "set OPFILE_EXIT cancel"

# Loop until an exit button is pressed.
   set exit 0
   while { !$exit } {

# Wait for the user to press a button.
      tkwait variable OPFILE_EXIT

# If the cancel button was pressed, exit returning zero.
      if { $OPFILE_EXIT == "cancel" } {
         set exit 1

# If the OK button was pressed, attempt to open the file, and exit.
      } elseif { $OPFILE_EXIT == "ok" } {

# Get the file name from the entry.
         set file [$ent get]

# First deal with cases where the file is to be read.
         if { $mode == "r" } {

#  Attempt to open the file for reading. Catch any error which occurs.
            if { [catch {set fd [open $file "r"]} msg] } {
               Message "File \"$file\" cannot be read:\n\n\"$msg\""
            } {
               set ret 1
            }

# Now deal with cases where the file is to be written.
         } {

# If the file already exists, get the user to confirm that it is
# OK to over-write it. If so, attempt to opne it for writing. Report any
# error.
            if { ![file exists $file] || [Confirm "Over-write existing file \"$file\"?"] } {
               if { [catch {set fd [open $file "w"]} msg] } {
                  Message "File \"$file\" cannot be opened for writing:\n\n\"$msg\""
               } {
                  set ret 1
               }
            }
         }

# If the file was opened succesfully, indicate that the dialog box should be
# closed.
         if { $ret } { set exit 1 }

      }
   }

# Destroy the dialog box, and return.
   destroy $top
   return $ret
}

proc Paste {} {
#+
#  Name:
#     Paste
#
#  Purpose:
#     Add any copied polygons onto the end of the current positions list.
#     The copied polygons are set up by procedure Copy.
#
#  Arguments:
#     None.
#-
   global PASTE
   global MODE
   global NPOLY

# Do nothing if there is nothing to past, or we are not in mode 1
# ("Create or edit a polygon").
   if { $PASTE && $MODE == 1 } {

# Get a unique string to attach to the canvas tags associated with the
# copy positions.
      append stag "C"
      append stag $NPOLY
      incr NPOLY

# Go round every vertex in the arrays to be copied.
      set size [NumPosn "" copy copy]
      set dx ""
      for {set i 0} {$i < $size} {incr i} {

# Move this vertex slightly so that the pasted polygons do not coincide
# with the original polygons. The first vertex is moved until it does not
# fall on an existing feature. Subsequent vertices are moved by the same
# amount. Also append a unique string to the position tags.
         set cx [GetPosn $i CX copy copy]
         set cy [GetPosn $i CY copy copy]

         if { $dx == "" } {
            set dx 0
            set ccx $cx
            set ccy $cy
            while { [FindPosn "CX CY" [list $ccx $ccy] 5] != "" } {
               incr dx 7
               set ccx [expr $cx + $dx]
               set ccy [expr $cy + $dx]
            }
         }

         set cx [expr $cx + $dx]
         set cy [expr $cy + $dx]

# Append a unique string to the position tags.
         set tag [GetPosn $i TAG copy copy]
         append tag $stag

# Add a new vertex to the current polygon.
         set nxt [GetPosn $i NXT copy copy]
         set ident($i) [SetPosn -1 "CX CY VID NXT TAG" [list $cx $cy -1 $nxt $tag]]
      }

# No go round the new vertices correcting the indices of the "next
# vertex" so that they refer to the current arrays instead of the copy
# arrays.
      for {set i 0} {$i < $size} {incr i} {
         SetPosn $ident($i) NXT $ident([GetPosn $i NXT copy copy])
      }

# Draw the new positions.
      DrawPosns 0 0

   }
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

#  Check it is 2-dimensional.
   set ndim [GetParamED ndfpack ndftrace:ndim]
   if { $ndim != 2 } {
      Message "Image \"$imsec\" has $ndim pixel axes - images must have 2 pixel axes."
      exit
   }

# Get the lower and upper bounds.
   set lbound [GetParamED ndfpack ndftrace:lbound]
   set ubound [GetParamED ndfpack ndftrace:ubound]

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
#    OLD_FOCUS (Write)
#      The window which has focus prior to giving focus to the text entry
#      widget.
#    OLD_VAL (Write)
#      The previous (valid) value displayed in the text entry widget.
#-
   global F_OWNER
   global OLD_VAL
   global $value
   upvar #0 $value varr

# Create the text entry widget. The text in this widget mirrors the value in
# the supplied global variable.
   eval entry $name -width $width -relief sunken -bd 2 -textvariable $value \
          -justify center $args
   set F_OWNER $name
   focus $name

# Save the current value of the variable.
   set OLD_VAL $varr

# When the pointer enters the text entry area, select the entire current
# contents of the widget so that typing a single character will delete it.
# Also take the focus, and save the current numerical value so that it
# can be re-instated if the user enters a duff value
   bind $name <Enter> \
      "if { \[$name cget -state\] == \"normal\" } {
          $name select from 0
          $name select to end

          set OLD_F_OWNER \$F_OWNER
          set F_OWNER $name
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
                set F_OWNER \$OLD_F_OWNER
                focus \$OLD_F_OWNER
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
#     SECTION_DISP (Read)
#       The displayed image section (eg "(10:200,23:68)" ).
#-
   global SECTION_DISP
   global SECTION_REQ
   global SECTION_STACK
   global UNZOOM

# Convert the canvas coords to NDF pixel coords.
   set pxy [CanToNDF $cx $cy]
   if { $pxy == "" } { return }
   set px [lindex $pxy 0]
   set py [lindex $pxy 1]

# Find a section which maps onto the displayed section but fills the
# entire GWM display area.
   set fullsec [ScreenSec $SECTION_DISP]

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
      Push SECTION_STACK $SECTION_DISP
      $UNZOOM configure -state normal

# Display the modified section.
      set SECTION_REQ "($lx:$ux,$ly:$uy)"
      UpdateDisplay

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
#     CAN (Read)
#        The name of the canvas widget holding the GWM image.
#     CANCEL (Read)
#        The path to the "Cancel" button.
#     DELETE (Read)
#        Path to the "Delete" button.
#     F_OWNER (Write)
#        The name of the Polka widget which "owns" the focus.
#     LB_B3 (Read)
#        The name of the "OK" button in the "Select Feature Label" dialog
#        box.
#     LABEL (Read)
#        The label of the feature currently being pointed at.
#     MODE (Read and Write)
#        The interaction mode determining how to process button clicks
#        and motion in the GWM canvas item. Modes are:
#           0 - The user specifies image features by clicking on them.
#           1 - The user starts a new mask polygon, or edits an existing one.
#           2 - The user completes a started mask polygon.
#           3 - The user selects a label by clicking on an existing feature.
#     MOVE (Read)
#        Was a whole polygon being dragged? Otherwise, a single vertex
#        was being dragged.
#     ROOTI (Read)
#        The position index of the vertex being pointed at, or the position
#        index of the vertex at the start of the vector being pointed at,
#        or null if neither a vertex nor a vector is being pointed at.
#     ROOTX (Read)
#        The canvas X coordinate at which the button was pressed.
#     ROOTY (Read)
#        The canvas Y coordinate at which the button was pressed.
#     SELECTED_AREA (Read and Write)
#        The bounds of the selected area in the order xmin, xmax, ymin,
#        ymax. Set to a null string on exit.
#     V0 (Read and Write)
#        The index of the position corresponding to the first vertex in
#        an incomplete (i.e. open) polygon.
#     V1 (Read and Write)
#        The index of the position corresponding to the last vertex in
#        an incomplete (i.e. open) polygon.
#     VCX0 (Read and Write)
#        The canvas X coordinates at the first vertex in an incomplete
#        (i.e. open) polygon.
#     VCY0 (Read and Write)
#        The canvas Y coordinates at the first vertex in an incomplete
#        (i.e. open) polygon.
#     VID0 (Read)
#        The canvas item id for the vertex being pointed at (if any).
#     VID2 (Read and Write)
#        The canvas item id of the vector joining the last vertex in an
#        incomplete (i.e. open) polygon, to the pointer.
#     VTAG (Read and Write)
#        The tag to give to all vertices and vectors making up the
#        current polygon.
#     ZOOM (Read)
#        Path to the "Zoom" button.
#-
   global CAN
   global CANCEL
   global CURCOL
   global CURITEM
   global DELETE
   global EDITMENU
   global F_OWNER
   global LB_B3
   global LABEL
   global NPOLY
   global MODE
   global MOVE
   global ROOTID
   global ROOTI
   global ROOTX
   global ROOTY
   global SELECTED_AREA
   global V0
   global V1
   global VCX0
   global VCY0
   global VID0
   global VID2
   global VTAG
   global ZOOM

# If there is a selected area, check that it is of significant size. If
# it isn't, cancel it.
   if { $SELECTED_AREA != "" } {
      set dx [expr [lindex $SELECTED_AREA 2] - [lindex $SELECTED_AREA 0] ]
      set dy [expr [lindex $SELECTED_AREA 3] - [lindex $SELECTED_AREA 1] ]
      if { $dx < 4 && $dy < 4 } {
         CancelArea
      }
   }

# If there is still a selected area, activate the relevant buttons.
   if { $SELECTED_AREA != "" } {
      $ZOOM configure -state normal
      $DELETE configure -state normal
      $CANCEL configure -state normal
      $EDITMENU entryconfigure Delete -state normal
      if { $MODE == 1 } {
         $EDITMENU entryconfigure Copy -state normal
      }

# Otherwise, what we do depends on the interaction mode.
   } {

# Mode 0 - "Enter image features"
      if { $MODE == 0 } {

# Get the pixel coordinates of the feature at the pointer position, and
# store and mark it as a new position.
         GetFeature $ROOTX $ROOTY ""

# Mode 1 - "Edit an existing mask polygon"
      } elseif { $MODE == 1 } {

# If we have been dragging a vertex, store the new coordinates of the
# vertex.
         if { $ROOTI != "" } {
            set cxy [$CAN coords $VID0]
            set cx [lindex $cxy 0]
            set cy [lindex $cxy 1]
            SetPosn $ROOTI "CX CY" [list $cx $cy]

# If the whole polygon has ben moved, do the same for the other
# vertices in the polygon.
            if { $MOVE } {
               set nxt [GetPosn $ROOTI NXT]
               while {$nxt != $ROOTI && $nxt != -1 && $nxt != "" } {
                  set id [GetPosn $nxt ID]
                  set cxy [$CAN coords $id]
                  set cx [lindex $cxy 0]
                  set cy [lindex $cxy 1]
                  SetPosn $nxt "CX CY" [list $cx $cy]
                  set nxt [GetPosn $nxt NXT]
               }

               set MOVE 0

            }

# If we are not dragging a vertex, then we must have initiated a new
# polygon. Record the initial position and enter mode 2.
         } {

# Create the tag to give to all vertices and edges of this new Polygon,
# and increment the number of polygons creaqted so far.
            set tag "P"
            append tag $NPOLY
            incr NPOLY

# Record the new position. The index of the "next" vertex is as yet unknown
# so set NXT to -1.
            set newi [SetPosn -1 "CX CY NXT TAG" [list $ROOTX $ROOTY -1 $tag]]

# Create a vector attached to the new position. It initially has zero
# length. Store the canvas item id for this vector.
            set vid [$CAN create line $ROOTX $ROOTY $ROOTX $ROOTY -fill $CURCOL -tags [list vectors $tag]]
            SetPosn $newi VID $vid

# Create the marker for the new positon, and store its canvas id.
            set id [MarkPosn $newi 0 1 vroot]
            SetPosn $newi ID $id
            set ROOTID $id

# Store the global values needed to construct the new polygon.
            set V0 $newi
            set V1 $newi
            set VCX0 $ROOTX
            set VCY0 $ROOTY
            set VID2 $vid
            set VTAG $tag

# Enter mode 2.
            SetMode 2
         }

# Mode 2 - "Enter a new mask polygon"
      } elseif { $MODE == 2 } {

# If we are pointing well away from the first vertex in the polygon, add
# a new vertex to the polygon.
        if { $ROOTX > [expr $VCX0 + 4] || $ROOTX < [expr $VCX0 - 4] ||
             $ROOTY > [expr $VCY0 + 4] || $ROOTY < [expr $VCY0 - 4] } {

# Record the new position. The index of the "next" vertex is as yet unknown
# so set NXT to -1.
            set newi [SetPosn -1 "CX CY NXT TAG" [list $ROOTX $ROOTY -1 $VTAG]]

# We now know the index of the "next" vertex for the previous vertex.
# Record it.
            SetPosn $V1 NXT $newi

# Create a vector attached to the new position. It initially has zero
# length. Store the canvas item id for this vector.
            set vid [$CAN create line $ROOTX $ROOTY $ROOTX $ROOTY -fill $CURCOL -tags [list vectors $VTAG]]
            SetPosn $newi "VID" $vid

# Create the marker for the new positon, and store its canvas id.
            set id [MarkPosn $newi 0 1 ""]
            SetPosn $newi ID $id

# Raise the marker of the first (root) vertex to the top of the display list.
            $CAN raise $ROOTID

# Store the global values needed to construct the new polygon.
            set V1 $newi
            set VID2 $vid

# If we are pointing close to the original vertex, close the polygon.
         } {

# Activate the commands which would occur if the pointer was moved away
# from the root vertex, and then remove the tag vroot from the root vertex.
            eval [$CAN bind vroot <Leave>]
            $CAN dtag $ROOTID vroot

# First count the number of vertices in the polygon. If it is less than
# 3, delete the entire polygon.
            if { [NumPosn $V0] < 3 } {
               Message "Polygon has less than 3 vertices and will be deleted."
               DelPosn $V0 1

# Otherwise...
            } {

# Raise the marker for the first vertex so that it is on top of the vector
# which ends there. This ensures that the marker is picked up in
# preference to the vector (for instance when pointing at it with the
# mouse).
               $CAN raise $ROOTID

# Record the index of the "next" vertex for the current vertex , and
# enter mode 1.
               SetPosn $V1 NXT $V0
               SetMode 1

# Add the tag "vertices" to the root vertex, and activate the commands which
# would occur if the pointer was moved into it.
               $CAN addtag vertices withtag $ROOTID
               eval [$CAN bind vertices <Enter>]

            }

         }

# Mode 3 - "Select a feature label"
      } elseif { $MODE == 3 } {

# Emulate the pressing of the "OK" button in the "Select Feature Label"
# dialog box.
         $LB_B3 invoke

# Mode 4 - "Re-centre the image"
      } elseif { $MODE == 4 } {

# Store the pixel coordinates of the pointer in POINTER_PXY.
         set cx [$CAN canvasx $x]
         set cy [$CAN canvasy $y]

# Re-display the image with the new centre.
         ReCentre $cx $cy

# All other modes are illegal.
      } {
         Message "Internal error - ReleaseBind - mode $MODE is illegal"
         exit 1
      }
   }
}

proc Restore {file} {
#+
#  Name:
#     Restore
#
#  Purpose:
#     Restore positions lists, masks, and options from a text file
#     previously created using procedure Dump.
#
#  Arguments:
#     file
#        If supplied non-blank, then the dump is read from the specified
#        file. Otherwise, the user is asked to supply a file name.
#
#
#  Returned Value:
#     1 if a succesful dump was performed, zero otherwise.
#-
   global E_RAY_FEATURES
   global E_RAY_MASK
   global IMAGES
   global O_RAY_FEATURES
   global O_RAY_MASK
   global O_RAY_SKY
   global E_RAY_SKY
   global IMMAP
   global OEMAP
   global COLMENU
   global XHAIR
   global SKYOFF
   global HAREA
   global SAREA
   global OPTSMENU
   global SKYPAR
   global PNTCY
   global PNTCX
   global PNTID
   global PNTNXT
   global PNTVID
   global PNTLBL
   global PROT_OEMAP
   global PROT_IMMAP

# Open an existing dump file, either the supplied one, or one specified by
# the user.
   if { $file != "" } {
      set backup 1
      if { [catch {set fd [open $file "r"]} mess] } {
         Message "Could not restored original positions lists, etc.\n\n\"$mess\"."
         set ok 0
      } {
         set ok 1
      }
      set info "Restoring original positions lists, etc."
   } {
      set backup 0
      set ok [OpenFile "r" "Dump file" "Give name of dump file to read:" file fd]
      set info "Restoring positions lists, etc, from disk."
   }

# Only proceed if a file was opened.
   if { $ok } {

# Tell the user what is happening.
      set told [SetInfo $info 0]

# Set a flag indicating that the supplied file is a valid dump file.
      set bad 0

# If the user supplied the dump file, dump the current positions, masks, etc,
# so that they can be restored if anything goes wrong. Abort if the dump fails.
      if { !$backup } {
         set safefile [UniqueFile]
         if { ![Dump $safefile] } {
            Message "Unable to create backup dump of current positions."
            set bad 1
         }
      }

# Loop round all images...
      foreach image $IMAGES {
         if { $bad } { break }

# Check that this is the start of a new image. If no more images are
# contained in the given file, break out of the image loop leaving
# the other images unchanged.
         if { [gets $fd line] == -1 } {
            set bad 1
            break
         }
         if { ![regexp {^Image } $line] } { break }

# Restore the the OE Mapping.
         if { [gets $fd line] == -1 } {
            set bad 1
            break
         }

         if { $line != "" } {
            if { [llength $line] == 6 } {
               set OEMAP($image) $line
               set DEF_OEMAP $line
            } {
               set bad 1
               break
            }
         } {
            catch { unset OEMAP($image) }
         }

# Restore the the Image Mapping.
         if { [gets $fd line] == -1 } {
            set bad 1
            break
         }

         if { $line != "" } {
            if { [llength $line] == 6 } {
               set IMMAP($image) $line
            } {
               set bad 1
               break
            }
         } {
            catch { unset IMMAP($image) }
         }

# Restore the mapping protection flags.
         if { [gets $fd line] == -1 } {
            set bad 1
            break
         }

         if { $line != "" } {
            set PROT_OEMAP($image) $line
         } {
            catch { unset PROT_OEMAP($image) }
         }

         if { [gets $fd line] == -1 } {
            set bad 1
            break
         }

         if { $line != "" } {
            set PROT_IMMAP($image) $line
         } {
            catch { unset PROT_IMMAP($image) }
         }

# Loop round each object type...
         foreach object [list $O_RAY_FEATURES $E_RAY_FEATURES $O_RAY_MASK \
                              $E_RAY_MASK $O_RAY_SKY $E_RAY_SKY] {

# Delete the existing positions.
            while { [NumPosn "" $image $object] > 0 } {
               DelPosn 0 0 $image $object
            }

# Get the number of positions in this list ($npnt).
            if { [gets $fd line] == -1 || [scan $line "%d" npnt] != 1 } {
               set bad 1
            }

# Read the arrays holding information describing the list.
            foreach item "PX PY NXT LBL TAG" {
               set aryname "PNT${item}"
               upvar #0 $aryname ary

               if { $bad || [gets $fd line] == -1 || [llength $line] != $npnt } {
                  set bad 1
               }

# Store the new array values.
               if { !$bad } {
                  set ary($image,$object) $line
               } {
                  set ary($image,$object) ""
               }
            }

#  The rest is only done if the Object was restired succesfully.
            if { !$bad } {

#  Set up the arrays holding the number of times each label is used.
               foreach lbl $PNTLBL($image,$object) {
                  Labels $lbl 1
               }

# Initialise the other required arrays to indicate that nothing is
# currently drawn.
               catch { unset PNTID($image,$object) }
               catch { unset PNTCX($image,$object) }
               catch { unset PNTCY($image,$object) }
               catch { unset PNTVID($image,$object) }
               foreach nxt $PNTNXT($image,$object) {
                  lappend PNTID($image,$object) -1
                  lappend PNTCX($image,$object) ""
                  lappend PNTCY($image,$object) ""
                  if { $nxt == "" } {
                     lappend PNTVID($image,$object) ""
                  } {
                     lappend PNTVID($image,$object) -1
                  }
               }
            }
         }
      }

# Find the line marking the start of the options.
      while { $line != "Options:" } {
         if { [gets $fd line] == -1 } {
            set bad 1
            break
         }
      }

# Only proceed if the file is good so far.
      if { !$bad } {

# Save the current states of some of the Options menu items.
         foreach var "XHAIR SKYOFF HAREA SAREA SKYPAR" {
            upvar #0 $var gvar
            set old_$var $gvar
         }

# Restore the values of the global variables holding the option settings...
         foreach var "XHRCOL CURCOL BADCOL REFCOL SELCOL PSF_SIZE INTERP FITTYPE OEFITTYPE SKYPAR VIEW XHAIR SKYOFF HAREA SAREA" {
            upvar #0 $var gvar

            if { [gets $fd line] == -1 || [llength $line] != 2 || [lindex $line 0] != $var } {
               set bad 1
               break
            }
            set gvar [lindex $line 1]
         }

# Now update the Options menu to reflect these restored values.
         if { !$bad } {

            if { $old_XHAIR != $XHAIR } {
               set XHAIR $old_XHAIR
               $OPTSMENU invoke "Use Cross-hair"
            }

            if { $old_SKYOFF != $SKYOFF } {
               set SKYOFF $old_SKYOFF
               $OPTSMENU invoke "Remove Sky"
            }

            if { $old_HAREA != $HAREA } {
               set HAREA $old_HAREA
               $OPTSMENU invoke "Display Help Area"
            }

            if { $old_SAREA != $SAREA } {
               set SAREA $old_SAREA
               $OPTSMENU invoke "Display Status Area"
            }

            if { $old_SKYPAR != $SKYPAR } {
               SkyOff
            }

            foreach var "XHRCOL CURCOL BADCOL REFCOL SELCOL" {
               upvar #0 $var gvar
               $COLMENU($var) invoke $gvar
            }

         }

      }

# Close the dump file.
      close $fd

# Cancel the informative text set earlier in this procedure.
      if { $told } { SetInfo "" 0 }

#  If the specified file was bad, try restoring the dump saved at the start
#  of this procedure. Do not do this if we are already restoring a saved
#  "backup" dump.
      if { $bad && !$backup } {
         Message "Syntax error encountered restoring positions from \"$file\". Last line read was:\n\n\"$line\""
         Restore $safefile
      }

#  Re-display the reference and current objects.
      UpdateDisplay ref

   }

}

proc RestoreImage {file} {
#+
#  Name:
#     RestoreImage
#
#  Purpose:
#     Restore positions lists and masks for the currently displayed image.
#
#  Arguments:
#     file
#        If supplied non-blank, then the dump is read from the specified
#        file. Otherwise, the user is asked to supply a file name.
#
#
#  Returned Value:
#     1 if a succesful dump was performed, zero otherwise.
#-
   global E_RAY_FEATURES
   global E_RAY_MASK
   global IMAGE_DISP
   global O_RAY_FEATURES
   global O_RAY_MASK
   global O_RAY_SKY
   global E_RAY_SKY
   global PNTCY
   global PNTCX
   global PNTID
   global PNTNXT
   global PNTVID
   global PNTLBL

# Open an existing dump file, either the supplied one, or one specified by
# the user.
   if { $file != "" } {
      set backup 1
      if { [catch {set fd [open $file "r"]} mess] } {
         Message "Could not restore original positions lists and masks.\n\n\"$mess\"."
         set ok 0
      } {
         set ok 1
      }
      set info "Restoring original positions lists and masks."
   } {
      set backup 0
      set ok [OpenFile "r" "Dump file" "Give name of dump file to read:" file fd]
      set info "Restoring positions lists and masks for currently displayed image from disk."
   }

# Only proceed if a file was opened.
   if { $ok } {

# Check the file contents.
      if { [gets $fd line] == -1 || $line != "Polka image dump" } {
         Message "The supplied dump file \"$file\" does not contain a dump created by the \"Dump/Displayed Image\" command in the File menu."
         close $fd
         return
      }

# Skip the next line which indicates the orignal image file.
      gets $fd line

# Tell the user what is happening.
      set told [SetInfo $info 0]

# Set a flag indicating that the supplied file is a valid dump file.
      set bad 0

# If the user supplied the dump file, dump the current positions, masks, etc,
# so that they can be restored if anything goes wrong. Abort if the dump fails.
      if { !$backup } {
         set safefile [UniqueFile]
         if { ![DumpImage $safefile] } {
            Message "Unable to create backup dump of current positions."
            set bad 1
         }
      }

# Check that no error has occured and sve the name of the currently
# displayed image.
      if { !$bad } {
         set image $IMAGE_DISP

# Loop round each object type...
         foreach object [list $O_RAY_FEATURES $E_RAY_FEATURES $O_RAY_MASK \
                              $E_RAY_MASK $O_RAY_SKY $E_RAY_SKY] {

# Delete the existing positions.
            while { [NumPosn "" $image $object] > 0 } {
               DelPosn 0 0 $image $object
            }

# Get the number of positions in this list ($npnt).
            if { [gets $fd line] == -1 || [scan $line "%d" npnt] != 1 } {
               set bad 1
            }

# Read the arrays holding information describing the list.
            foreach item "PX PY NXT LBL TAG" {
               set aryname "PNT${item}"
               upvar #0 $aryname ary

               if { $bad || [gets $fd line] == -1 || [llength $line] != $npnt } {
                  set bad 1
               }

# Store the new array values.
               if { !$bad } {
                  set ary($image,$object) $line
               } {
                  set ary($image,$object) ""
               }
            }

#  The rest is only done if the Object was restired succesfully.
            if { !$bad } {

#  Set up the arrays holding the number of times each label is used.
               foreach lbl $PNTLBL($image,$object) {
                  Labels $lbl 1
               }

# Initialise the other required arrays to indicate that nothing is
# currently drawn.
               catch { unset PNTID($image,$object) }
               catch { unset PNTCX($image,$object) }
               catch { unset PNTCY($image,$object) }
               catch { unset PNTVID($image,$object) }
               foreach nxt $PNTNXT($image,$object) {
                  lappend PNTID($image,$object) -1
                  lappend PNTCX($image,$object) ""
                  lappend PNTCY($image,$object) ""
                  if { $nxt == "" } {
                     lappend PNTVID($image,$object) ""
                  } {
                     lappend PNTVID($image,$object) -1
                  }
               }
            }
         }
      }

# Close the dump file.
      close $fd

# Cancel the informative text set earlier in this procedure.
      if { $told } { SetInfo "" 0 }

#  If the specified file was bad, try restoring the dump saved at the start
#  of this procedure. Do not do this if we are already restoring a saved
#  "backup" dump.
      if { $bad && !$backup } {
         Message "Syntax error encountered restoring positions from \"$file\". Last line read was:\n\n\"$line\""
         RestoreImage $safefile
      }

#  Re-display the reference and current objects.
      UpdateDisplay ref

   }

}

proc RestoreMask {file} {
#+
#  Name:
#     RestoreMask
#
#  Purpose:
#     Restore O and E ray masks from a dump file created by DumpMask.
#     Current masks are first cleared.
#
#  Arguments:
#     file
#        If supplied non-blank, then the dump is read from the specified
#        file. Otherwise, the user is asked to supply a file name.
#
#  Returned Value:
#     1 if a succesful dump was performed, zero otherwise.
#-
   global E_RAY_MASK
   global DBEAM
   global IMAGES
   global O_RAY_MASK
   global PNTCY
   global PNTCX
   global PNTID
   global PNTNXT
   global PNTVID
   global PNTLBL

# Open an existing dump file, either the supplied one, or one specified by
# the user.
   if { $file != "" } {
      set backup 1
      if { [catch {set fd [open $file "r"]} mess] } {
         Message "Could not restored original mask.\n\n\"$mess\"."
         set ok 0
      } {
         set ok 1
      }
      set info "Restoring original mask."
   } {
      set backup 0
      set ok [OpenFile "r" "Dump file" "Give name of mask dump file to read:" file fd]
      set info "Restoring masks from disk."
   }

# Only proceed if a file was opened.
   if { $ok } {

# Tell the user what is happening.
      set told [SetInfo $info 0]

# Set a flag indicating that the supplied file is a valid dump file.
      set bad 0

# If the user supplied the dump file, dump the current mask so that it
# can be restored if anything goes wrong. If the dump fails (for instance
# because the mask has not yet been set up), then continue anyway.
      if { !$backup } {
         set safefile [UniqueFile]
         if { ![DumpMask $safefile] } {
            set safefile ""
         }
      }

# Clear the O and E ray masks for all images.
      foreach image $IMAGES {

         while { [NumPosn "" $image $O_RAY_MASK] > 0 } {
            DelPosn 0 0 $image $O_RAY_MASK
         }

         while { [NumPosn "" $image $E_RAY_MASK] > 0 } {
            DelPosn 0 0 $image $E_RAY_MASK
         }

      }

# Save the name of the first (reference) image. Use a foreach loop so that we
# can use a break command to jump to the end.
      foreach image [lindex $IMAGES 0] {

# The first line should be "Masks only". Check that this is the case.
         if { [gets $fd line] == -1 } {
            set bad 1
            break
         }

         if { ![regexp {^Masks only} $line] } {
            set bad 1
            break
         }

#  Do each mask.
         foreach object [list $O_RAY_MASK $E_RAY_MASK] {

# Get the number of positions in the supplied mask ($npnt).
            if { [gets $fd line] == -1 || [scan $line "%d" npnt] != 1 } {
               set bad 1
            }

# Read the arrays from the text file holding information describing the list.
            foreach item "PX PY NXT LBL TAG" {
               set aryname "PNT${item}"
               upvar #0 $aryname ary

               if { $bad || [gets $fd line] == -1 || [llength $line] != $npnt } {
                  set bad 1
               }

# Store the new array values.
               if { !$bad } {
                  set ary($image,$object) $line
               } {
                  set ary($image,$object) ""
               }
            }

#  The rest is only done if the O-ray mask was restored succesfully.
            if { !$bad } {

#  Set up the arrays holding the number of times each label is used.
               foreach lbl $PNTLBL($image,$object) {
                  Labels $lbl 1
               }

# Initialise the other required arrays to indicate that nothing is
# currently drawn.
               catch { unset PNTID($image,$object) }
               catch { unset PNTCX($image,$object) }
               catch { unset PNTCY($image,$object) }
               catch { unset PNTVID($image,$object) }

               foreach nxt $PNTNXT($image,$object) {
                  lappend PNTID($image,$object) -1
                  lappend PNTCX($image,$object) ""
                  lappend PNTCY($image,$object) ""
                  if { $nxt == "" } {
                     lappend PNTVID($image,$object) ""
                  } {
                     lappend PNTVID($image,$object) -1
                  }
               }
            }
         }
      }

# Close the dump file.
      close $fd

# Cancel the informative text set earlier in this procedure.
      if { $told } { SetInfo "" 0 }

# If the specified file was bad, try restoring the dump saved at the start
# of this procedure. Do not do this if we are already restoring a saved
# "backup" dump.
      if { $bad && !$backup } {
         Message "Syntax error encountered restoring masks from \"$file\". Last line read was:\n\n\"$line\""
         if { $safefile != "" } { RestoreMask $safefile }
      }

# Re-display the reference and current objects.
      UpdateDisplay ref

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
#     IMAGES (Read)
#        A list of the input images (without any section specifiers).
#     IMSECS (Read)
#        A list of the input image sections as supplied by the user.
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
#        PNTNXT. If this value is blank ("") then position i is not part of
#        a polygon. If this value is -1, then the end of the vector starting
#        position i is unspecified (in this case the polygon is not
#        closed).
#     PNTPX (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel X coordinates.
#     PNTPY (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel Y coordinates.
#     POLMODE (Read)
#        'Linear' or 'Circular'; indicates the type of polarisation for
#        which Stokes parameters should be created.
#     REFONLY (Read)
#        Is the first image in the IMAGES list purely a reference image
#        for aligning the other images? If so, no output images will be
#        created from it and it will not be included in the calculation
#        of the Stokes parameters.
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
#     STKOUT (Read)
#        The name of the output cube to hold Stokes parameters (if $STOKES
#        is non-zero).
#     STOKES (Read)
#        If non-zero, then Stokes parameters are calculated and stored in
#        a cube with name given by $STKOUT.
#
#-
   global ATASK_OUTPUT
   global CANCEL_OP
   global DBEAM
   global EFFECTS_MAPPINGS
   global FITTYPE
   global IMAGES
   global IMAGE_STACK
   global IMSECS
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
   global POLMODE
   global POLPACK_DIR
   global POLY
   global REFONLY
   global RESAVE
   global SAFE
   global SIMAGE
   global SKYOFF
   global SKY_AREA
   global SKY_METHOD
   global STKOUT
   global STOKES
   global S_FONT
   global GOTOUT
   global WCSDOMAIN

# If no output files are required, tell the user.
   if { !$GOTOUT } {
      Message "No output data files were specified when Polka was run.\n\n(The Dump command in the File menu can still be used to dump the current feature positions, masks, etc.)"
      return 1
   }

# Display a warning and return if the images have already been saved.
   if { $STOKES } {
      set out "Stokes cube"
      set w1 "is"
   } {
      set out "intensity images"
      set w1 "are"
   }

   if { !$RESAVE } {
      Message "The output $out $w1 already up-to-date with respect to the current mappings and masks."
      return 1
   }

# Begin a temporary file context.
   set tfc [BeginUF]

# Tell the user what is happening.
   set told [SetInfo "Creating new output $out. Please wait... " 0]

# Assume failure.
   set ok 0

# In single-beam mode we just use the O-ray lists.
   if { $DBEAM } {
      set rays "O E"
   } {
      set rays "O"
   }

# Save the name of the first (reference) image.
   set im0 [lindex $IMAGES 0]

# Ensure that all the required mappings are available. Return without
# action if any are missing.
   if { [AllMappings] } {

# If we are in dual-beam mode,  ensure that the reference image has an O-ray
# mask. Since we already know that all mappings are available, all other
# masks can be created from this mask if necessary.
      if { $DBEAM && ![CreateMask $im0 $O_RAY_MASK] } {
         Message "Masks defining the O and E ray areas have not yet been supplied."

# If the sky to be subtracted is defined within the object frames,
# ensure that sky areas have been defined.
      } elseif { $SKYOFF && $SKY_METHOD == $SKY_AREA && ![CreateSky $im0 $O_RAY_SKY] } {
         Message "The areas containing sky have not yet been supplied."

# Otherwise, process each image section in turn...
      } {

# Create the top level window for the dialogue box which displays progress.
         set top .progress
         set topf [MakeDialog $top "Progress,,," 1]
         SetHelp $topf ".  This dialog box indicates the progress which has been made towards creating and saving the output images."

# Create and pack all the items in the dialog box.
         set back "#b0b0b0"

         set fr0 [frame $topf.fr0]
         pack $fr0 -side top -fill both -expand 1

         set imlist [frame $fr0.imlist -bd 2 -relief raised -background $back]
         SetHelp $imlist ".  A list of all the input images. A tick mark is displayed next to the images which have been completed. "
         pack [label $imlist.lb -text " Input images " -font $S_FONT -background $back] -side top -pady 1m

         set pfrm [frame $fr0.pfrm]
         SetHelp $pfrm ".  A list of the processing stages involved in creating the output images. Tick marks are displayed next to the stages which have been completed, and the current stage is highlighted in red. "
         pack $imlist $pfrm -side left -fill x -padx 4m -pady 4m

         if { $STOKES } {
            set stok [frame $topf.stok -background $back]
            pack $stok -side top -padx 5m -pady 2m -fill x -expand 1
            set stoklb [label $stok.lb -text "Producing Stokes parameters:" \
                                   -justify left -anchor w -background $back]
            pack $stoklb -side left -padx 2m
            set stoktick [label $stok.tick -foreground $back \
                                 -background $back -bitmap @$POLPACK_DIR/tick.bit]
            pack $stoktick -side right
         }

         foreach image $IMAGES {

# Ignore the first image if it used purely as a reference "pasteboard"
# and is not to be included in the output data sets.
            if { $REFONLY && $image == $im0 } continue

            regsub -all {\.} $image "" ni
            set ni [string tolower $ni]
            set fr [frame $imlist.$ni -background $back]
            pack $fr -side top -fill x

            set lb($image) [label $fr.lb -text "$image:" -anchor w \
                            -background $back]
            pack $lb($image) -side left -padx 2m -fill x

            set tick($image) [label $fr.tick -foreground $back \
                              -background $back -bitmap @$POLPACK_DIR/tick.bit]
            pack $tick($image) -side right

         }

         set image ""
         set fr1 [frame $pfrm.fr1]
         pack $fr1 -side top -anchor nw -fill x
         set l1 [label $fr1.l1 -text "Doing image: " -font $S_FONT]
         set l2 [label $fr1.l2 -textvariable SIMAGE]
         pack $l1 $l2 -side left -padx 6m

         set fr234 [frame $pfrm.fr234 -background $back -bd 2 -relief raised]
         pack $fr234 -side left -padx 5m -pady 3m -fill x -expand 1
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

# Open a text file to receive the name of all the aligned intensity images
# create below.
         set intfiles [UniqueFile]
         set intfiles_id [open $intfiles w]

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

# Ignore the first image if it used purely as a reference "pasteboard"
# and is not to be included in the output data sets.
            if { $REFONLY && $image == $im0 } continue

# Display the current image name.
            set SIMAGE $image

# Highlight the image's label in red in the progress dialog box.
            Wop $lb($image) configure -foreground red

# Get the name of a native NDF copy of the supplied image. This name
# is stored at the bottom (end) of the effects stack.
            set ndf_data [lindex $IMAGE_STACK($image) end]

# Do the sky subtraction, using the native NDF copy of the input data to
# avoid expensive data conversions.
            set image2 [SkySub $ndf_data $image 1 $selab $setick(O) $setick(E) \
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

# Get the name of a temporary file to hold the output image in NDF format.
               set outndf [UniqueFile]

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
# image. These bounds are passed on to KAPPA:REGRID as the required
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
                  if { ![TranImage $maskarea $map $outndf $sect] } {
                     set ok 0
                     break
                  }

# Ensure that the current WCS Frame is the same as that in the input image.
                  if { ![Obey ndfpack wcsframe "$outndf $WCSDOMAIN($image)"] } {
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

# Calculate the rotations, magnifications and shifts corresponding to the
# mapping.
                  DescMap $map
                  ConvMap 1 ""

# Add the required components to the POLPACK extension...
# The ray label (only in dual-beam mode).
                  if { $DBEAM } {
                     if { ![Extension $outndf RAY _CHAR*1 $ray ""] } {
                        set ok 0
                        break
                     }
                  }

# The image identifier. This is only assigned a value if the IMGID component
# does not already exist. In this case, the name of the input image is
# used.
                  if { ![Extension $outndf IMGID _CHAR "" old_plate] } {
                     set ok 0
                     break
                  }

                  if { ![string length $old_plate] } {
                     set plate [file tail $image]
                     if { ![Extension $outndf IMGID _CHAR $plate ""] } {
                        set ok 0
                        break
                     }
                  }

# Copy the temporary output NDF to the requiested output image.
                  if { ![Obey ndfpack ndfcopy "in=$outndf out=$outim"] } {
                     set ok 0
                     break
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

#  Write the name of the NDF version of the output image just created to the
#  text file opened earlier.
               puts $intfiles_id $outndf

# Add the name of the NDF to a list of files to be escaped to the next
# higher file context when EndUF is next called.
               lappend escapes $outndf

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

# Set the colours of the image's tick and label in the progress dialog
# box to black.
            Wop $tick($image) configure -foreground black
            Wop $lb($image) configure -foreground black

# Delete all the temporary files created in this pass through the loop,
# except the output NDF files which are required to create the Stokes
# vectors. Clear the list of escaped files.
            EndUF $tfci $escapes
            unset escapes

# Leave the image loop if an error has occurred.
            if { !$ok } { break }

         }

#  Close the text file to which were written the names of all the created
#  output images.
         close $intfiles_id

# Set the "current image" name blank.
         set SIMAGE " "

# If required calculate Stokes parameters.
         if { $ok && $STOKES } {

# Highlight the "Producing Stokes parameters" label in red.
            Wop $stoklb configure -foreground red
            update idletasks

#  Create the Stokes cube.
            if { ![Obey polpack polcal "in=^$intfiles pmode=$POLMODE out=$STKOUT maxit=30"] } {
               set ok 0
            }

# Make the "Producing Stokes parameters" label (and associated tick mark) in
# the progress dialog box revert to black.
            Wop $stoklb configure -foreground black
            Wop $stoktick configure -foreground black

         }

# Ensure X events are ignored while ADAM tasks are being executed.
         set SAFE $old_safe
         update idletasks

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

         if { $REFONLY && $image == $im0 } continue

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
#     read back by the polka atask upon completion. They will then be
#     stored in the Atask's parameter file.
#
#  Arguments:
#     None.
#
#  Globals:
#      ATASK_HAREA (Write)
#         The value of HAREA sent to the A-task.
#      ATASK_SAREA (Write)
#         The value of SAREA sent to the A-task.
#      ATASK_PSF (Write)
#         The value of PSF_SIZE sent to the A-task.
#      ATASK_SI (Write)
#         A string representing the SI_LIST list.
#      HAREA (Read)
#         Should the help area be displayed?
#      SAREA (Read)
#         Should the status area be displayed?
#      PSF_SIZE (Read)
#         The typicel size of a feature, in pixels.
#      SI_LIST (Read)
#         A list of indices identifying the status items to be displayed
#         in the status area. These indices point into the SI_VARS,
#         SI_LABELS and SI_HELPS lists.
#      SI_VARS (Read)
#         A list of all the variables which are available to be displayed
#         in the status area.
#
#-
   global ATASK_BADCOL
   global ATASK_CURCOL
   global ATASK_FIT
   global ATASK_HAREA
   global ATASK_INTERP
   global ATASK_OEFIT
   global ATASK_PHI
   global ATASK_PLO
   global ATASK_POLMODE
   global ATASK_PSF
   global ATASK_REFCOL
   global ATASK_SAREA
   global ATASK_SELCOL
   global ATASK_SI
   global ATASK_SKYOFF
   global ATASK_SKYPAR
   global ATASK_VIEW
   global ATASK_XHAIR
   global ATASK_XHRCOL
   global BADCOL
   global CHAR_LIST
   global CHAR_STOP
   global CURCOL
   global FITTYPE
   global HAREA
   global INTERP
   global MAPTYPE
   global OEFITTYPE
   global PHI_REQ
   global PLO_REQ
   global POLMODE
   global PSF_SIZE
   global REFCOL
   global SAREA
   global SELCOL
   global SI_LIST
   global SI_VARS
   global SKYOFF
   global SKYPAR
   global STOKES
   global VIEW
   global XHAIR
   global XHRCOL

# Only proceed if the operation is confirmed...
   if { [Confirm "Save current options values?"] } {

# Just take a copy of each of the current options values.
     set ATASK_HAREA $HAREA
     set ATASK_SAREA $SAREA
     set ATASK_PSF $PSF_SIZE
     set ATASK_CURCOL $CURCOL
     set ATASK_SELCOL $SELCOL
     set ATASK_REFCOL $REFCOL
     set ATASK_BADCOL $BADCOL
     set ATASK_XHRCOL $XHRCOL
     set ATASK_XHAIR $XHAIR
     set ATASK_SKYOFF $SKYOFF
     set ATASK_PLO $PLO_REQ
     set ATASK_PHI $PHI_REQ
     set ATASK_INTERP $INTERP
     set ATASK_VIEW $VIEW
     set ATASK_SKYPAR $SKYPAR
     if { $STOKES } { set ATASK_POLMODE $POLMODE }

     foreach fittype [array names MAPTYPE] {
        if { $MAPTYPE($fittype) == $FITTYPE } { break }
     }
     set ATASK_FIT $fittype

     foreach fittype [array names MAPTYPE] {
        if { $MAPTYPE($fittype) == $OEFITTYPE } { break }
     }
     set ATASK_OEFIT $fittype

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
         set xl [expr int( floor( $xc - 0.5 * $height ) ) ]
         set xu [expr int( floor( $xl + $height - 1 ) ) ]

# Otherwise, if the width is greater than the height, adjust the y bounds
# to make the section square.
      } elseif { $width > $height } {
         set yl [expr int( floor( $yc - 0.5 * $width ) ) ]
         set yu [expr int( floor( $yl + $width - 1 ) ) ]
      }

# Construct a new section string.
      set newsec "($xl:$xu,$yl:$yu)"
   }

   return $newsec
}

proc SearchHelp {} {
#+
#  Name:
#     SearchHelp
#
#  Purpose:
#     Search the POLKA on-line manual for a given word.
#-
   global WSRCH
   global EXSRCH
   global POLPACK_HELP

#  Store a description of the controls in the dialog box to be created by
#  GetPars.
   set vars [list WSRCH EXSRCH]
   set type(WSRCH) "_CHAR*30"
   set type(EXSRCH) "_LOGICAL"
   set labs(WSRCH) "Search for:"
   set labs(EXSRCH) "Exact search?"
   set limit(WSRCH) ""
   set limit(EXSRCH) ""

#  Set up initial default valus.
   if { ![info exists WSRCH] || ![info exists EXSRCH] } {
      set WSRCH ""
      set EXSRCH 0
   }

# Get the word to search for, and see if an exact search is required. Only
# proceed if the Cancel button is not pressed.
   if { [GetPars $vars type labs limit "Search for help on..." \
                 POLKA_HSEARCH \
                 ". Search the on-line help manual for a given word." ] } {

# Tell the user what is happening.
      set told [SetInfo "Searching for \"$WSRCH\". Please wait..." 0]

# Save the old cursor and switch on a "watch" cursor.
      set old_cursor [. cget -cursor]
      . config -cursor watch
      update idletasks

# Run findme to display the results of the search. Use the -h and -l switches
# to make it search the page headings and lines. Use the -s switch to make it
# arrange the results in order of significance. If an exact search is required
# use the -w and -c switches to ensure only whole words are matched, and case
# is significant. Use -q to suppress output to standard output.
      if { $EXSRCH } {
         set err [catch {exec findme -q -h -l -s -w -c  $WSRCH $POLPACK_HELP/polka} mess]
      } {
         set err [catch {exec findme -q -h -l -s $WSRCH $POLPACK_HELP/polka} mess]
      }

#  Re-instate the original cursor.
      . config -cursor $old_cursor
      update idletasks

# Report any error. Note, the "child process exited abnormally"n error
# does not stop things from working.
      if { $err && $mess != "child process exited abnormally" } {
         Message "Findme failed to find the help documentation...\n\n$mess"
      }

# Cancel the informative text set earlier in this procedure.
      if { $told } { SetInfo "" 0 }

   }
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

proc Segment {indata outdata image obj} {
#+
#  Name:
#     Segment
#
#  Purpose:
#     Uses KAPPA:SEGMENT to copy a region of one image into another image.
#     It assumes that all images have PIXEL as the current co-ordinate
#     Frame.
#
#  Arguments:
#     indata
#        The image section containing the data to be put inside the polygon
#        in the output image. If this is blank then the polygon is filled
#        with bad values.
#     outdata
#        The image section containing the data to be put outside the polygon
#        in the output image. If this is blank, then the polygon is
#        copied into an image of bad values.
#     image
#        The name of the supplied image which is associated with the mask
#        which defines the polygonal areas to be copied.
#     obj
#        The name of the mask object which defines the polygonal areas to
#        be copied (eg $O_RAY_MASK, $E_RAY_SKY, etc).
#
#  Returned Value:
#     The name of the output file, or blank if anything goes wrong.
#
#  Globals:
#     PNTNXT (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of integers representing indices within the lists
#        given by PNTxxx. Each integer gives the index of the next position
#        along the edge of a polygon. The vector starting at position
#        index i, ends at position index given by the i'th element of
#        PNTNXT. If this value is blank ("") then position i is not part of
#        a polygon. If this value is -1, then the end of the vector starting
#        position i is unspecified (in this case the polygon is not
#        closed).
#     PNTPX (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel X coordinates.
#     PNTPY (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel Y coordinates.
#-
   global PNTNXT
   global PNTPX
   global PNTPY

# Write the pixel coordinates of the mask to a set of text files in a format
# which can be used by KAPPA:SEGMENT. Each file holds a single polygon.
   set pntnxt $PNTNXT($image,$obj)
   set file [UniqueFile]
   set fileid [open $file w]
   set i 0
   set i0 0
   set npoly 0
   set polys ""

   while { $i != "" } {
      set px [lindex $PNTPX($image,$obj) $i]
      set py [lindex $PNTPY($image,$obj) $i]
      puts $fileid "$px $py"

      set j [lindex $pntnxt $i]
      set pntnxt [lreplace $pntnxt $i $i ""]
      set i $j

      if { $i == $i0 } {
         close $fileid
         set pfile $file
         incr npoly
         set POLY(POLY${npoly}) $pfile
         append polys "POLY${npoly}=$pfile "

         set i ""
         foreach nxt $pntnxt {
            if { $nxt != "" } {
               set i $nxt
               break
            }
         }

         if { $i != "" } {
            set i0 $i
            set file [UniqueFile]
            set fileid [open $file w]
         }
      }
   }

#  Supply a null value for the next polygon parameter to indicate the end
#  of the polygon files.
   incr npoly
   append polys "POLY${npoly}=!"

# Get the name of the output image in which to store the extracted mask
# area.
   set maskarea [UniqueFile]

# Run SEGMENT to extract the mask area into a temporary image.
   if { $outdata == "" } { set outdata "!" }
   if { ![Obey kappa segment "in1=$indata in2=$outdata mode=file out=$maskarea $polys"] } {
      set maskarea ""
   }

   return $maskarea
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

proc SendBack {} {
#+
#  Name:
#     SendBack
#
#  Purpose:
#     Writes the names and values of the global variables holding the
#     a-task options values to a text file. This procedure should be
#     called prior to exiting the Polka Tcl script. The a-task will
#     read the values in the file and store them in its HDS parameter
#     file.
#
#  Arguments:
#     None.
#
#  Globals:
#    COMFILE (Read)
#       The name of the text file in which to store the values of the global
#       variables to be passed back to the a-task.
#
#-
   global COMFILE
   global ATASK_SKYOFF
   global ATASK_XHAIR
   global ATASK_HAREA
   global ATASK_SAREA
   global ATASK_PSF
   global ATASK_SKYPAR
   global ATASK_SI
   global ATASK_VIEW
   global ATASK_BADCOL
   global ATASK_CURCOL
   global ATASK_REFCOL
   global ATASK_SELCOL
   global ATASK_XHRCOL
   global ATASK_FIT
   global ATASK_OEFIT
   global ATASK_PLO
   global ATASK_PHI
   global ATASK_POLMODE

   if { $COMFILE != "" } {

      catch {exec rm -f $comfile}

      set fd [open $COMFILE "w"]

      if { [info exists ATASK_SKYOFF] }  { puts $fd "ATASK_SKYOFF $ATASK_SKYOFF"}
      if { [info exists ATASK_XHAIR] }   { puts $fd "ATASK_XHAIR $ATASK_XHAIR"}
      if { [info exists ATASK_HAREA] }   { puts $fd "ATASK_HAREA $ATASK_HAREA"}
      if { [info exists ATASK_SAREA] }   { puts $fd "ATASK_SAREA $ATASK_SAREA"}
      if { [info exists ATASK_PSF] }     { puts $fd "ATASK_PSF $ATASK_PSF"}
      if { [info exists ATASK_SKYPAR] }  { puts $fd "ATASK_SKYPAR $ATASK_SKYPAR"}
      if { [info exists ATASK_SI] }      { puts $fd "ATASK_SI $ATASK_SI"}
      if { [info exists ATASK_VIEW] }    { puts $fd "ATASK_VIEW $ATASK_VIEW"}
      if { [info exists ATASK_BADCOL] }  { puts $fd "ATASK_BADCOL $ATASK_BADCOL"}
      if { [info exists ATASK_CURCOL] }  { puts $fd "ATASK_CURCOL $ATASK_CURCOL"}
      if { [info exists ATASK_REFCOL] }  { puts $fd "ATASK_REFCOL $ATASK_REFCOL"}
      if { [info exists ATASK_SELCOL] }  { puts $fd "ATASK_SELCOL $ATASK_SELCOL"}
      if { [info exists ATASK_XHRCOL] }  { puts $fd "ATASK_XHRCOL $ATASK_XHRCOL"}
      if { [info exists ATASK_FIT] }     { puts $fd "ATASK_FIT $ATASK_FIT"}
      if { [info exists ATASK_OEFIT] }   { puts $fd "ATASK_OEFIT $ATASK_OEFIT"}
      if { [info exists ATASK_PLO] }     { puts $fd "ATASK_PLO $ATASK_PLO"}
      if { [info exists ATASK_PHI] }     { puts $fd "ATASK_PHI $ATASK_PHI"}
      if { [info exists ATASK_POLMODE] } { puts $fd "ATASK_POLMODE $ATASK_POLMODE"}

      close $fd

   }
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
#     BADCOL (Read)
#       The colour with which to mark missing pixel data (eg "cyan").
#     CAN (Read)
#        The name of the canvas widget holding the GWM image.
#     CURCOL (Read)
#       The colour with which to mark current objects (eg "red").
#     CUROBJ_DISP (Read)
#       The type of the currently displayed current objects.
#     DEVICE (Read)
#       The GNS name of the graphice device.
#     E_RAY_MASK (Read)
#       The integer index corresponding to the E-ray mask objects.
#     IMAGE_DISP (Read)
#       The currently displayed image.
#     O_RAY_FEATURES (Read)
#       The integer index corresponding to the O-ray features objects.
#     NONE (Read)
#       The integer index corresponding to no reference objects.
#     RB_CUR (Read)
#        The names of the radiobutton widgets which are used to select
#        the current objects.
#     RB_REF (Read)
#        The names of the radiobutton widgets which are used to select
#        the reference objects.
#     REFCOL (Read)
#       The colour with which to mark reference objects (eg "green").
#     REFOBJ_DISP (Read)
#       The type of the currently displayed reference objects.
#     SELCOL (Read)
#       The colour with which to mark the selected area (eg "red").
#-
   global BADCOL
   global CAN
   global CURCOL
   global CUROBJ_DISP
   global DEVICE
   global E_RAY_FEATURES
   global E_RAY_MASK
   global E_RAY_SKY
   global IMAGE_DISP
   global NONE
   global O_RAY_FEATURES
   global O_RAY_MASK
   global O_RAY_SKY
   global RB_CUR
   global RB_REF
   global REFCOL
   global REFOBJ_DISP
   global SELCOL
   global XHAIR_IDH
   global XHAIR_IDV
   global XHRCOL

# Reconfigure the current objects if their colour has changed. Also set the
# colour used by the "Current:" radio buttons.
   if { $var == "CURCOL" } {
      DrawCur

      foreach i "$O_RAY_SKY $O_RAY_FEATURES $O_RAY_MASK $E_RAY_SKY $E_RAY_FEATURES $E_RAY_MASK" {
         $RB_CUR($i) configure -selectcolor $CURCOL
      }

# Reconfigure the reference objects if their colour has changed. Also set the
# colour used by the "Reference:" radio buttons.
   } elseif { $var == "REFCOL" } {
      DrawRef

      foreach i "$NONE $O_RAY_SKY $O_RAY_FEATURES $O_RAY_MASK $E_RAY_SKY $E_RAY_FEATURES $E_RAY_MASK" {
         $RB_REF($i) configure -selectcolor $REFCOL
      }

# Change the colour of any displayed selection box.
   } elseif { $var == "SELCOL" } {
      $CAN itemconfigure sbox -outline $SELCOL

# Change the colour of entry zero in the KAPPA pallette. This is used
# top mark missing pixels.
   } elseif { $var == "BADCOL" } {
      Obey kapview palentry "device=$DEVICE palnum=0 colour=$BADCOL" 1

# Change the colour of the cross-hair if it is curently in use.
   } elseif { $var == "XHRCOL" } {
      if { $XHAIR_IDH != "" } {
         $CAN itemconfigure $XHAIR_IDH -fill $XHRCOL
         $CAN itemconfigure $XHAIR_IDV -fill $XHRCOL
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
   Helper

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
#     INFO_TEXT (Write)
#        The current text to be displayed.
#
#  Notes:
#     - If a null string ("") is supplied for "text", and "def" is non-zero,
#     then the low priority text (if any) will be displayed.
#-
   global CURRENT_INFO
   global DEFAULT_INFO
   global INFO_TEXT

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

# Now decide on the text to be displayed. If we have a non-blank current
# text value, then display it. otherwise display the default value.
   if { $CURRENT_INFO != "" } {
      set INFO_TEXT $CURRENT_INFO
   } {
      set INFO_TEXT $DEFAULT_INFO
   }

# Ensure the text is displayed.
   update idletasks

   return $told
}

proc SetMode {mode} {
#+
#  Name:
#     SetMode
#
#  Purpose:
#     Change the canvas interaction mode.
#
#  Arguments:
#     mode
#        The required interaction mode. This determines how to process button
#        clicks and motion in the GWM canvas item. Modes are:
#           0 - The user specifies image features by clicking on them.
#           1 - The user starts a new mask polygon, or edits an existing one.
#           2 - The user completes a started mask polygon.
#           3 - The user selects a label by clicking on an existing feature.
#           4 - The user selects a position and the image is redisplayed
#               centred on the supplied position.
#
#  Globals:
#     CAN (Read)
#        The name of the canvas widget holding the GWM image.
#     CANCEL (Read)
#        The path to the "Cancel" button.
#     MODE (Write)
#        The new interaction mode.
#-
   global CAN
   global CANCEL
   global EDITMENU
   global MODE
   global M4_CURSOR
   global OLD_CURSOR
   global M4_XHAIR
   global PASTE
   global PRE_MODE4
   global SELECTED_AREA
   global V0
   global V1
   global VCX0
   global VCY0
   global VID2
   global XHAIR

# Disable the Copy and Paste entries in the Edit menu.
   $EDITMENU entryconfigure Copy -state disabled
   $EDITMENU entryconfigure Paste -state disabled

# If the previous mode was mode 4, change the cursor back to its previous
# value, switching the cross-hair back on again if required. Also,
# disable the cancel button.
   if { [info exists M4_XHAIR] } {
      $CAN config -cursor [Pop CURSOR_STACK]
      if { $M4_XHAIR } {
         set XHAIR 1
         Xhair 0 0
      }
      unset M4_XHAIR
      $CANCEL configure -state disabled
   }

# Select the required mode.
   set old_mode $MODE
   set MODE $mode

   if { $mode == 0 } {
      CursorBind [list vertices vectors] [list "" ""]
      SetHelp $CAN ".  Single click to indicate an image feature.\n.  Click and drag to select an area of the image." POLKA_MODE_0
      SetInfo "Identify star-like features in the image..." 1

   } elseif { $mode == 1 } {
      CursorBind [list vertices vectors] [list hand2 pencil]
      SetHelp $CAN ".  Click on a vertex and drag to move the vertex.\n.  Click on a vertex with the shift key pressed to drag the entire polygon.\n.  Click on a polygon edge to insert a new vertex.\n.  Click anywhere else to start a new polygonal mask.\n.  Click anywhere else and drag to select an area." POLKA_MODE_1
      SetInfo "Edit or create a polygonal mask..." 1
      if { $SELECTED_AREA != "" } {
         $EDITMENU entryconfigure Copy -state normal
      }
      if { $PASTE } {
         $EDITMENU entryconfigure Paste -state normal
      }

   } elseif { $mode == 2 } {
      CursorBind [list vertices vectors vroot] [list "" "" X_cursor]
      SetHelp $CAN ".  Click on the first vertex to close the polygon.\n.  Click anywhere else to add another vertex to the polygon.\n.  Click and drag to select an area." POLKA_MODE_2
      $CANCEL configure -state normal
      SetInfo "Complete the construction of a polygon..." 1

   } elseif { $mode == 3 } {
      CursorBind [list vertices vectors] [list "" ""]
      SetHelp $CAN ".  Position pointer over a feature to highlight the corresponding label in the list box.\n.  Click on a feature to close the dialog box and use the feature's label." POLKA_MODE_3
      SetInfo "Select a feature to inherit its label..." 1

   } elseif { $mode == 4 } {
      CursorBind [list vertices vectors] [list "" ""]

# Switch off the cross-hair if currently in use, save the current cursor
# type, and set up a circular cursor.
      if { $XHAIR } {
         set M4_XHAIR 1
         set XHAIR 0
         Xhair 0 0
      } {
         set M4_XHAIR 0
      }
      Push CURSOR_STACK [$CAN cget -cursor]
      $CAN config -cursor circle

      set PRE_MODE4 $old_mode
      $CANCEL configure -state normal
      SetHelp $CAN ".  Click to re-display the image centred on the pointer position." POLKA_MODE_4
      SetInfo "Identify a new centre for the displayed image..." 1

   } {
      Message "Internal error - SetMode - mode $mode is illegal"
      exit 1
   }

# Reset the globals used in the contruction of a polygon if we are not
# now constructing a polygon (note we may go back to finish the polygon
# if we have gone into mode 4). These values are set in mode 1.
   if { $mode == 0 || $mode == 1 || $mode == 3 || ( $mode == 4 && $old_mode != 1 && $old_mode != 2 ) } {
      set V0 ""
      set V1 ""
      set VCX0 ""
      set VCY0 ""
      set VID2 ""
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
#           LBL - The textual label associated with a position.
#           TAG - A canvas tag associated with the position.
#     values
#        A list of parameter values corresponding to the names in "names".
#     args
#        An optional list argument holding the image from which the
#        positions are derived, and the type of objects to be used.
#        If these are not supplied, they default to $IMAGE_DISP and
#        $CUROBJ_DISP.
#
#  Returned Value:
#     The index of the set position. A null string is returned if the
#     supplied index is out of bounds.
#
#  Globals:
#     CAN (Read)
#        Path to the canvas containing the GWM image display.
#     CUROBJ_DISP (Read)
#        The type of the current objects displayed.
#     IMAGE_DISP (Read)
#        The displayed image (without section).
#     PNTCX (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas X coordinates.
#     PNTCY (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas Y coordinates.
#     PNTID (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas item identifiers associated with the
#        positions in the list. A value of -1 indicates that no marker is
#        currently drawn for the position.
#     RECALC_OEMAP (Write)
#        A 1-d array indexed by image. Each element is a logical flag
#        indicating if the image features for the corresponding image
#        have changed since the image's E to O mapping was last found.
#     RECALC_IMMAP (Write)
#        A 1-d array indexed by image. Each element is a logical flag
#        indicating if the image features for the corresponding image
#        have changed since the mapping from the image to the first
#        (reference) image was last found. The flag is set for all
#        images if the image features for the first (reference) image
#        are changed.
#     PNTNXT (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of integers representing indices within the lists
#        given by PNTxxx. Each integer gives the index of the next position
#        along the edge of a polygon. The vector starting at position
#        index i, ends at position index given by the i'th element of
#        PNTNXT. If this value is blank ("") then position i is not part of
#        a polygon. If this value is -1, then the end of the vector starting
#        position i is unspecified (in this case the polygon is not
#        closed).
#     PNTPX (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel X coordinates.
#     PNTPY (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel Y coordinates.
#     PNTTAG (Read)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas tags (one for each position).
#     PNTVID (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas item identifiers associated with the
#        vectors between positions in the list. A value of -1 indicates that
#        no line is currently drawn for the position. A blank string
#        indicates that no vector is defined.
#     V0 (Read)
#        The index of the position corresponding to the first vertex in
#        an incomplete (i.e. open) polygon.
#     V1 (Read)
#        The index of the position corresponding to the last vertex in
#        an incomplete (i.e. open) polygon.
#     VCX0 (Write)
#        The canvas X coordinates at the first vertex in an incomplete
#        (i.e. open) polygon.
#     VCY0 (Write)
#        The canvas Y coordinates at the first vertex in an incomplete
#        (i.e. open) polygon.
#     VID2 (Write)
#        The canvas item id of the vector joining the last vertex in an
#        incomplete (i.e. open) polygon, to the pointer.
#-
   global CAN
   global CUROBJ_DISP
   global E_RAY_FEATURES
   global IMAGES
   global IMAGE_DISP
   global O_RAY_FEATURES
   global PNTCX
   global PNTCY
   global PNTID
   global PNTLBL
   global PNTNXT
   global PNTPX
   global PNTPY
   global PNTVID
   global PNTTAG
   global RECALC_IMMAP
   global RECALC_OEMAP
   global RESAVE
   global V0
   global V1
   global VCX0
   global VCY0
   global VID2

# Store the image and object type.
   if { $args == "" } {
      set image $IMAGE_DISP
      set object $CUROBJ_DISP
   } {
      set image [lindex $args 0]
      set object [lindex $args 1]
   }

# Get the size of the list.
   if { [info exists PNTID($image,$object)] } {
      set size [llength $PNTID($image,$object)]
   } {
      set size 0
   }

# If a new position is being appended to the list, append a dummy
# position containing default values now which will be filled with the
# supplied values later.
   if { $i == -1 } {
      set ret $size
      incr size
      lappend PNTCX($image,$object) ""
      lappend PNTCY($image,$object) ""
      lappend PNTPX($image,$object) ""
      lappend PNTPY($image,$object) ""
      lappend PNTID($image,$object) -1
      lappend PNTVID($image,$object) ""
      lappend PNTNXT($image,$object) ""
      lappend PNTLBL($image,$object) ""
      lappend PNTTAG($image,$object) ""
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
      set lblmod 0

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
         } elseif { $name == "LBL" } {
            set lblmod 1
            set new_label $val
            set old_label [lindex $PNTLBL($image,$object) $ret]
         }

# Set the value, and indicate that the mappings based on this position list
# will need to be re-calculated (unless this is a mask).
         upvar #0 PNT$name array
         if { [info exists array($image,$object)] } {
            set array($image,$object) [lreplace $array($image,$object) $ret $ret $val]

            if { $object == $O_RAY_FEATURES || $object == $E_RAY_FEATURES } {
               set RECALC_OEMAP($image) 1
               set RECALC_IMMAP($image) 1

# If the current image is the first (reference) image, then all image
# mappings will need to be re-calculated, because all mappings go to the
# reference image.
               if { $image != [lindex $IMAGES 0] } {
                  foreach im $IMAGES {
                     set RECALC_IMMAP($im) 1
                  }
               }
            }

# Indicate that the output images will need to be re-saved.
            set RESAVE 1
         }
      }

# If the canvas coordinates have been supplied, but the pixel coordinates
# weren't, recalculate the pixel coordinates to ensure they are still valid.
      if { $cxymod && !$pxymod } {
         set cx [lindex $PNTCX($image,$object) $ret]
         set cy [lindex $PNTCY($image,$object) $ret]
         set pxy [CanToNDF $cx $cy]
         if { $pxy == "" } { return "" }
         set px [lindex $pxy 0]
         set py [lindex $pxy 1]
         set PNTPX($image,$object) [lreplace $PNTPX($image,$object) $ret $ret $px]
         set PNTPY($image,$object) [lreplace $PNTPY($image,$object) $ret $ret $py]

# If the pixel coordinates have been supplied, but the canvas coordinates
# weren't, recalculate the canvas coordinates to ensure they are still valid.
      } elseif { !$cxymod && $pxymod } {
         set px [lindex $PNTPX($image,$object) $ret]
         set py [lindex $PNTPY($image,$object) $ret]
         set cxy [NDFToCan $px $py]
         if { $cxy == "" } { return "" }
         set cx [lindex $cxy 0]
         set cy [lindex $cxy 1]
         set PNTCX($image,$object) [lreplace $PNTCX($image,$object) $ret $ret $cx]
         set PNTCY($image,$object) [lreplace $PNTCY($image,$object) $ret $ret $cy]
      }

# Configure the canvas items to the new canvas coordinates if they have
# changed. Also update the end posaition of any vectors which end at the
# modified position.
      if { $pxymod || $cxymod } {
         set cx [lindex $PNTCX($image,$object) $ret]
         set cy [lindex $PNTCY($image,$object) $ret]
         set px [lindex $PNTPX($image,$object) $ret]
         set py [lindex $PNTPY($image,$object) $ret]

         set id [lindex $PNTID($image,$object) $ret]
         if { $id != -1 } {
            $CAN coords $id $cx $cy
         }

         set vid [lindex $PNTVID($image,$object) $ret]
         if { $vid != -1 && $vid != "" } {
            set coords [$CAN coords $vid]
            set cx0 [lindex $coords 2]
            set cy0 [lindex $coords 3]
            $CAN coords $vid $cx $cy $cx0 $cy0
         }

         set j 0
         foreach nx $PNTNXT($image,$object) {
            if { $nx == $ret } {
               set vid [lindex $PNTVID($image,$object) $j]
               if { $vid != -1 && $vid != "" } {
                  set coords [$CAN coords $vid]
                  set cx0 [lindex $coords 0]
                  set cy0 [lindex $coords 1]
                  $CAN coords $vid $cx0 $cy0 $cx $cy
               }
            }
            incr j
         }
       }

# If NXT was modified (the index of the next vertex in the polygon),
# configure the vector to end at the new "next" vertex.
      if { $nxtmod } {
         set vid [lindex $PNTVID($image,$object) $ret]

         if { $vid != -1 && $vid != "" } {
            set nx [lindex $PNTNXT($image,$object) $ret]

            if { $nx != -1 && $nx != "" } {
               set cx [lindex $PNTCX($image,$object) $ret]
               set cy [lindex $PNTCY($image,$object) $ret]
               set cx0 [lindex $PNTCX($image,$object) $nx]
               set cy0 [lindex $PNTCY($image,$object) $nx]
               $CAN coords $vid $cx $cy $cx0 $cy0
            } {
               CanDelete $vid
               set PNTVID($image,$object) [lreplace $PNTVID($image,$object) $ret $ret $nx]
            }
         }
      }

# If the first vertex in a polygon have been changed, store the new values.
      if { $ret == $V0 } {
         if { $pxymod || $cxymod } {
            set VCX0 $cx
            set VCY0 $cy
         }
      }

# If the last vertex in a polygon have been changed, store the new values.
      if { $ret == $V1 } {
         if { $vidmod } {
            set VID2 [lindex $PNTVID($image,$object) $ret]
         }
      }

# If the label has changed, increment the number of times the new label
# is used, and decrement the number of times the old label is used.
      if { $lblmod } {
         Labels $new_label 1
         Labels $old_label 0
      }

# Update the bindings to be activated when the pointer enters or
# leaves the marker.
      MarkBind $ret $image $object

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
#     display the specified section of the Polka documentation.
#
#  Arguments:
#     label
#        An htx cross-reference label into the polka document. If this
#        is "pointer", then the pointer is used to identify the object about
#        which help is required. The cursor becomes a question mark and
#        the window under the pointer when it is next clicked is the one
#        about which help is displayed. If a blank string is supplied,
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

# Otherwise, translate the special label POLKA_CONTENTS into a blank
# string so that it goes to the top of the Polka help document.
   } {
      if { $label == "POLKA_CONTENTS" } {
         set label ""
      }

# Save the old cursor and switch on a "watch" cursor.
      set old_cursor [. cget -cursor]
      . config -cursor watch

# Run showme to get the full url of a local file to be displayed. Warn
# the user if an error occurs.
      if { [catch {exec showme -l -n $POLPACK_HELP/polka $label} url] } {
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

proc SingleBind {x y shift} {
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
#    shift
#       Was the shift key pressed when the button was clicked?
#
#  Globals:
#     CAN (Read)
#        The name of the canvas widget holding the GWM image.
#     MODE (Read)
#        The interaction mode determining how to process button clicks
#        and motion in the GWM canvas item. Modes are:
#           0 - The user specifies image features by clicking on them.
#           1 - The user starts a new mask polygon, or edits an existing one.
#           2 - The user completes a started mask polygon.
#           3 - The user selects a label by clicking on an existing feature.
#     ROOTI (Write)
#        The position index of the vertex being pointed at, or the position
#        index of the vertex at the start of the vector being pointed at,
#        or null if neither a vertex nor a vector is being pointed at.
#     ROOTX (Write)
#        The canvas X coordinate at which the button was pressed.
#     ROOTY (Write)
#        The canvas Y coordinate at which the button was pressed.
#     VID0 ( Write)
#        The canvas item id for the vertex being pointed at (if any).
#     VID1 ( Write)
#        The canvas item id for the vector ending at the vertex being
#        pointed at (if any).
#     VID2 ( Write)
#        The canvas item id for the vector starting at the vertex being
#        pointed at (if any).

#-
   global CAN
   global CURCOL
   global CURITEM
   global MODE
   global MOVE
   global ROOTI
   global ROOTID
   global ROOTX
   global ROOTY
   global VID0
   global VID1
   global VID2
   global POLPACK_DIR
   global ANCX
   global ANCY
   global ROOTAG

# Cancel any existing selected area.
   CancelArea

# Convert the screen coords to canvas coords, and record this position as
# the "root" position which is available for use by other procedures.
   set ROOTX [$CAN canvasx $x]
   set ROOTY [$CAN canvasy $y]

# The global variable MODE determines how events over the canvas are
# processed. The only case which needs any special treatment is if
# the user is editing an existing polygon (mode 1).
   if { $MODE == 1 } {

# Get the canvas id of the current item.
      set id0 [$CAN find withtag current]

# Get the list index of any position with this id.
      set ROOTI [FindPosn ID $id0 0]
      set ROOTID $id0

# If a position was found with this item id, we must be pointing at a
# polygon vertex which is to be dragged. Store information about this
# vertex in global for use in B1MotionBind.
      if { $ROOTI != "" } {
         set VID0 [GetPosn $ROOTI ID]
         set vid0xy [$CAN coords $VID0]
         set ANCX [lindex $vid0xy 0]
         set ANCY [lindex $vid0xy 1]
         set VID2 [GetPosn $ROOTI VID]
         set j [FindPosn NXT $ROOTI 0]
         set VID1 [GetPosn $j VID]
         set MOVE $shift
         set ROOTAG [GetPosn $ROOTI TAG]

# If we are not pointing at a vertex, try to find a vector with the
# current id.
      } {
         set ROOTI [FindPosn VID $id0 0]

# If one was found, insert a new vertex into the polygon at the cursor
# position.
         if { $ROOTI != "" } {
            set nxt [GetPosn $ROOTI NXT]
            set cx2 [GetPosn $ROOTI CX]
            set cy2 [GetPosn $ROOTI CY]
            set tag [GetPosn $ROOTI TAG]
            set vid [$CAN create line $ROOTX $ROOTY $cx2 $cy2 -fill $CURCOL -tags [list vectors $tag]]
            set id [$CAN create bitmap $ROOTX $ROOTY -bitmap @$POLPACK_DIR/vertex.bit -foreground $CURCOL -tags [list vertices $tag]]
            set newi [SetPosn -1 "CX CY ID VID NXT TAG" [list $ROOTX $ROOTY $id $vid $nxt $tag]]
            SetPosn $ROOTI NXT $newi
            set VID0 $id
            set VID2 $vid
            set VID1 [GetPosn $ROOTI VID]
            set ROOTI $newi
            update idletasks
            eval [$CAN bind vertices <Enter>]

         }
      }
   } {
      set ROOTI ""
   }
}

proc SkyOff {} {
#+
#  Name:
#    SkyOff
#
#  Purpose:
#    Set the states of the sky area radio-buttons and the text displayed
#    in the status area to reflect a change in the "Remove Sky" option.
#
#  Arguments:
#    None
#
#  Globals:
#    CUROBJ_REQ (Read)
#       The current type of the current objects.
#    IMAGE_DISP (Read)
#       The currently displayed image.
#    RB_CUR (Read)
#       An array holding the names of the radiobuttons which are used to
#       select the current object type.
#    RB_REF (Read)
#       An array holding the names of the radiobuttons which are used to
#       select the reference object type.
#    REFOBJ_REQ (Read)
#       The current type of the reference objects.
#    E_RAY_SKY (Read)
#        An integer representing the "E-ray sky" object type.
#    NONE (Read)
#       The integer index corresponding to no reference objects.
#    O_RAY_FEATURES (Read)
#        An integer representing the "O-ray features" object type.
#    O_RAY_SKY (Read)
#        An integer representing the "O-ray sky" object type.
#    SKYIMS (Read)
#        A list of images containing sky data to be subtracted from the
#        supplied object frames.
#    SKYOFF (Read)
#        Should a sky background be subtracted?
#    SKYPAR (Read)
#        The order of the polynomial fit to use on each axis when
#        fitting a sky surface.
#    SKYTEXT (Write)
#        The text to appear in the status area describing the current sky
#        subtraction method.
#    SKYTEXT2 (Write)
#        The text to appear in the status area describing the current
#        number of fitting parameter used when fitting sky surfaces.
#    SKY_AREA (Read)
#        One of the integer values which may be assigned to SKY_METHOD:
#        indicates that sky background is to be performed by fitting
#        surfaces to areas within the supplied object frames (i.e. the
#        O and E sky areas).
#    SKY_FRAME (Read)
#        One of the integer values which may be assigned to SKY_METHOD:
#        indicates that sky background is to be performed by subtracting
#        the sky frame specified in SKYIMS.
#    SKY_METHOD (Read)
#        The sky subtraction method to use; $SKY_AREA or $SKY_FRAME.
#-
   global CUROBJ_REQ
   global DBEAM
   global E_RAY_SKY
   global IMAGE_DISP
   global NONE
   global O_RAY_FEATURES
   global O_RAY_SKY
   global RB_CUR
   global RB_REF
   global REFOBJ_REQ
   global SKYIMS
   global SKYOFF
   global SKYPAR
   global SKYTEXT
   global SKYTEXT2
   global SKY_AREA
   global SKY_FRAME
   global SKY_METHOD

# Get a list of the relevant sky areas.
   if { $DBEAM } {
      set skys [list $O_RAY_SKY $E_RAY_SKY]
   } {
      set skys $O_RAY_SKY
   }

# If sky subtraction is switched on...
   if { $SKYOFF } {

# Set the text for the status item describing the sky subtraction,
# and determine whether the sky area buttons should be enabled or disabled.
      if { $SKY_METHOD == $SKY_FRAME } {
         set SKYTEXT $SKYIMS($IMAGE_DISP)
         set SKYTEXT2 "(not applicable)"
         set state disabled
      } {
         set SKYTEXT "displayed image"
         set SKYTEXT2 $SKYPAR
         set state normal
      }

# Enable or disable all the sky buttons.
      foreach obj $skys {
         $RB_CUR($obj) configure -state $state
         $RB_REF($obj) configure -state $state
      }

# If sky subtraction is switched off...
   } {

# Set the text for the status item describing the sky subtraction.
      set SKYTEXT "(disabled)"
      set SKYTEXT2 "(disabled)"

# Do each sky area.
      foreach obj $skys {

# If the current object is a sky area, change it to "O Ray features".
         if { $CUROBJ_REQ == $obj } { $RB_CUR($O_RAY_FEATURES) invoke }

# If the reference object is a sky area, change it to "None".
         if { $REFOBJ_REQ == $obj } { $RB_REF($NONE) invoke }

# Disable all the sky buttons.
         $RB_CUR($obj) configure -state disabled
         $RB_REF($obj) configure -state disabled
      }
   }
}

proc SkySub {data image sub args} {
#+
#  Name:
#     SkySub
#
#  Purpose:
#     Perform sky subtraction on a specified image.
#
#  Arguments:
#     data
#        The name of the image section containing the data from which the
#        sky is to be subtracted. This may be the output from an Effect,
#        or may be one of the user-supplied images.
#     image
#        The name of the user supplied image from which the supplied data
#        is derived.
#     sub
#        If non-zero then the returned image is the difference between
#        the supplied data and the estimated sky background. Otherwise,
#        the returned data is the estimated sky background.
#     args
#        Any extra arguments should be the paths to the label widgets
#        within the "Save" progress dialog box which refer to sky
#        subtraction. See procedure "Save".
#
#  Returned Value:
#     The name of the file containing sky subtracted data, or blank if
#     anything went wrong.
#
#  Globals:
#     DBEAM (Read)
#        Is Polka being run in dual-beam mode?
#     E_RAY_MASK (Read)
#        An integer representing the "E-ray mask" object type.
#     O_RAY_MASK (Read)
#        An integer representing the "O-ray mask" object type.
#     E_RAY_SKY (Read)
#        An integer representing the "E-ray sky" object type.
#     O_RAY_SKY (Read)
#        An integer representing the "O-ray sky" object type.
#     PSF_SIZE (Read)
#        The feature size specified by the user.
#     SKYIMS (Read)
#        A list of images containing sky data to be subtracted from the
#        supplied object frames. This variable is ignored unless $SKY_METHOD
#        is $SKY_FRAME.
#     SKYOFF (Read)
#        Should sky subtraction be performed? If not, then the input image
#        name is returned unchanged.
#     SKYPAR (Read)
#        The order of the polynomial to use on each axis when
#        fitting a sky surface.
#     SKY_AREA (Read)
#        One of the integer values which may be assigned to SKY_METHOD:
#        indicates that sky background is to be performed by fitting
#        surfaces to areas within the supplied object frames (i.e. the
#        O and E sky areas).
#     SKY_FRAME (Read)
#        One of the integer values which may be assigned to SKY_METHOD:
#        indicates that sky background is to be performed by subtracting
#        the sky frames specified in $SKYIMS from the supplied object frames.
#     SKY_METHOD (Read)
#        The sky subtraction method to use; $SKY_AREA or $SKY_FRAME. Any
#        other value results in no sky subtraction being performed (in which
#        case the supplied input data is simply copied to the output).
#-
   global DBEAM
   global E_RAY_MASK
   global O_RAY_MASK
   global E_RAY_SKY
   global O_RAY_SKY
   global PSF_SIZE
   global SKYIMS
   global SKYOFF
   global SKYPAR
   global SKY_AREA
   global SKY_FRAME
   global SKY_METHOD

# Begin a temporary file context.
   set tfc [BeginUF]

# Extract the names of the widgets which are used to indicate progress.
   if { [llength $args] == 6 } {
      set selab [lindex $args 0]
      set setick(O) [lindex $args 1]
      set setick(E) [lindex $args 2]
      set sslab [lindex $args 3]
      set sstick(O) [lindex $args 4]
      set sstick(E) [lindex $args 5]
   } {
      set selab "no.such.window"
      set setick(O) "no.such.window"
      set setick(E) "no.such.window"
      set sslab "no.such.window"
      set sstick(O) "no.such.window"
      set sstick(E) "no.such.window"
   }

# If the sky background has been supplied in a separate image, subtract
# it from the object frame.
   if { $SKY_METHOD == $SKY_FRAME && $SKYOFF } {

# Indicate that the sky areas have been extracted, and highlight the
# "sky subraction" label to indicate that the sky is being subtracted.
      Wop $setick(O) configure -foreground black
      if { $DBEAM } { Wop $setick(E) configure -foreground black }
      Wop $sslab configure -foreground red

# Do the subtraction, then ensure that he bounds are the same as the
# supplied data (padding with bad pixels if necessary).
      set ssimage [UniqueFile]
      if { ![Obey kappa maths "exp=ia-ib ia=$data ib=$SKYIMS($image) out=$ssimage"] } {
         set ssimage ""
      } {
         if { ![Obey ndfpack setbound "ndf=$ssimage like=$data"] } {
            set ssimage ""
         }
      }

# Indicate that the sky areas have been subtracted.
      Wop $sstick(O) configure -foreground black
      if { $DBEAM } { Wop $sstick(E) configure -foreground black}
      Wop $sslab configure -foreground black

# If the sky background is defined by an area in the supplied object
# frame...
   } elseif { $SKY_METHOD == $SKY_AREA && $SKYOFF } {

# In single-beam mode we just use the O-ray area, and we can manage
# without an O-ray mask.
      if { $DBEAM } {
         set rays "O E"
         set need_mask 1
      } {
         set rays "O"
         set need_mask 0
      }

# Do each ray in turn...
      set ok 1
      foreach ray $rays {
         upvar #0 ${ray}_RAY_SKY skyobj
         upvar #0 ${ray}_RAY_MASK maskobj

# If a progress report is required, indicate that the sky areas are being
# extracted.
         Wop $selab configure -foreground red

# Issue a warning if the sky areas have not been identified.
         if { ![CreateSky $image $skyobj] } {
            Message "The ${ray}-ray areas containing sky have not been identified."
            set ok 0
            break
         }

# Issue a warning if an O or E ray mask is needed but has not been supplied.
         if { ![CreateMask $image $maskobj] && $need_mask } {
            Message "The ${ray}-ray mask has not been supplied."
            set ok 0
            break
         }

# Extract the sky areas into a separate image.
         set rawsky [Segment $data "" $image $skyobj]
         if { $rawsky == "" } {
            set ok 0
            break
         }

# Clean any features contained within the sky areas.
         set sky [UniqueFile]
         set fsize [expr 2.0*$PSF_SIZE]
         if { ![Obey kappa ffclean "in=$rawsky out=$sky clip=\[3,3,3\] box=$fsize"] } {
            set ok 0
            break
         }

# Fit a surface to the data within the sky areas.
         set fit($ray) [UniqueFile]

         if { ![Obey kappa surfit "in=$sky out=$fit($ray) evaluate=all fittype=poly order=$SKYPAR"] } {
            set ok 0
            break
         }

# If a progress report is required, un-highlight the "sky extraction" label
# and indicate that the sky has been extracted.
         Wop $selab configure -foreground black
         Wop $setick($ray) configure -foreground black
      }

# If a progress report is required, make the "sky subtraction" label red.
      Wop $sslab configure -foreground red

# If the fits to the sky areas for the required rays were obtained ok,
# combine them into a single sky image.
      if { $ok } {

# First deal with dual-beam cases...
         if { $DBEAM } {

# Extract the O-ray mask area from the O-ray sky fit image.
            set osky [Segment $fit(O) "" $image $O_RAY_MASK]
            if { $osky == "" } {
               set ok 0

# If OK, insert into this image the E-ray mask area from the E-ray sky
# fit image.
            } {
               set oesky [Segment $fit(E) $osky $image $E_RAY_MASK]
               if { $oesky == "" } {
                  set ok 0
               } {
                  set sky $oesky
               }
            }

# For single-beam cases. just use the whole O-ray fit.
         } {
            set sky $fit(O)
         }
      }


# Subtract the sky image from the supplied data.
      if { $ok } {
         if { $sub } {
            set ssimage [UniqueFile]

            if { ![Obey kappa maths "exp=ia-ib ia=$data ib=$sky out=$ssimage"] } {
               set ok 0
            }
         } {
            set ssimage $sky
         }
      }

# If a progress report is required, stop the "sky subtraction" label
# blinking, and indicate that the sky has been subtracted.
      Wop $sslab configure -foreground black
      Wop $sstick(O) configure -foreground black
      if { $DBEAM } { Wop $sstick(E) configure -foreground black }

# If anything went wrong, return a null string.
      if { !$ok } { set ssimage "" }

# Do no sky subtraction for any other type.
   } {
      set ssimage $image
   }

# Delete all the temporary files created in this procedure, except for the
# one being returned (if any).
   EndUF $tfc $ssimage

# Return the name of the image containing the sky subtracted data.
   return $ssimage

}

proc Spacer {name h w} {
#+
#  Name:
#     Spacer
#
#  Purpose:
#     Create a blank object of fixed size to use as a spacer.
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
         SetHelp $F3 ".  An area containing current options values, and other status information." POLKA_STATUS_AREA

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

# Reduce the required widths since the average character width used seems
# to be overly generous, resulting in more space being used by the labels
# than is needed.
         set maxwid(0) [expr int( 0.9 * $maxwid(0) )]
         set maxwid(1) [expr int( 0.9 * $maxwid(1) )]

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
            SetHelp $fr $help POLKA_STATUS_AREA

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
#    OLD_FOCUS (Write)
#      The window which has focus prior to giving focus to the text entry
#      widget.
#    OLD_VAL (Write)
#      The previous (valid) value displayed in the text entry widget.
#-
   global F_OWNER
   global $value
   upvar #0 $value varr

# Create the text entry widget. The text in this widget mirrors the value in
# the supplied global variable.
   eval entry $name -width $width -relief sunken -bd 2 -textvariable $value \
          -justify left $args
   set F_OWNER $name
   focus $name

# When the pointer enters the text entry area, select the entire current
# contents of the widget so that typing a single character will delete it.
# Also take the focus.
   bind $name <Enter> \
      "if { \[$name cget -state\] == \"normal\" } {
          $name select from 0
          $name select to end

          set OLD_F_OWNER \$F_OWNER
          set F_OWNER $name
          focus $name

       }"

# When the pointer leaves the text entry area, clear the current selection,
# pass the focus back to the window which had it before, and perform any
# supplied command.
   bind $name <Leave> \
      "if { \[$name cget -state\] == \"normal\" } {
          $name select clear
          set F_OWNER \$OLD_F_OWNER
          focus \$OLD_F_OWNER
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

proc TestFea {} {
#+
#  Name:
#     TestFea
#
#  Purpose:
#     Display a candidate feature marker at given pixel coordinates.
#
#  Arguments:
#     None.
#
#  Returned Value:
#     The canvas coords at the candidate feature.
#
#  Globals:
#     CAN (Read)
#        The name of the canvas widget holding the GWM image.
#     CURCOL (Read)
#       The colour with which to mark current objects (eg "red").
#     TEST_PX (Read)
#       The pixel X coordinates at the candidate feature.
#     TEST_PY (Read)
#       The pixel Y coordinates at the candidate feature.
#     TEST_ID (Read and Write)
#       The canvas identifier for the candidate feature marker.
#-
   global CAN
   global CURCOL
   global TEST_ID
   global TEST_PX
   global TEST_PY

# Convert the supplied pixel coordinates to canvas coordinates.
   set cxy [NDFToCan $TEST_PX $TEST_PY]
   if { $cxy == "" } { return "" }
   set cx [lindex $cxy 0]
   set cy [lindex $cxy 1]

# Find the coordinates defining the required marker.
   set xl [expr $cx - 3]
   set yl [expr $cy - 3]
   set xu [expr $cx + 3]
   set yu [expr $cy + 3]

# If there is currently no candidate feature, create a circle on the canvas
# at the supplied position.
   if { $TEST_ID == "" } {
      set TEST_ID [$CAN create oval $xl $yl $xu $yu -outline $CURCOL -tags features]

# If there is already a candidate feature, configure its marker.
   } {
      $CAN coords $TEST_ID $xl $yl $xu $yu
      $CAN raise $TEST_ID
   }

   return $cxy
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

proc TotalMap {image} {
#+
#  Name:
#     TotalMap
#
#  Purpose:
#     Return the total mapping from supplied image to the current top of
#     the effects stack for the specified image.
#
#  Arguments:
#     image
#        The name of the supplied image.
#
#  Returned Value:
#     The mapping, as a list of 6 values. Blank indicates that the
#     mapping is not defined.
#
#  Globals:
#     EFFECTS_MAPPINGS (Read)
#        A 1-d array, indexed by image name. Each element is a list
#        in which each entry gives the mapping introduced by the
#        corresponding effect. This mapping goes from pixel coords in
#        the previous top entry in the effects stack to the new top entry.
#        There is no entry for the supplied image (i.e. the number of
#        entries in this stack is the same as EFFECTS_STACK, i.e. one less
#        than in IMAGE_STACK). Each mapping is described by a list of 6
#        values giving the parameters of a full 2-D linear fit.
#-
   global EFFECTS_MAPPINGS

# Initialise the total mapping to a unit mapping.
   set tot_map "ref"

# Stacks are stored as lists with the top of stack at index zero.
# Find the index of the bottom of stack.
   set bot [expr [llength $EFFECTS_MAPPINGS($image)] - 1]

# Work backwards through the stack, starting at the bottom of stack (i.e.
# the list end), concatenating all the mappings.
   for {set i $bot} {$i >= 0} {incr i -1} {
      set tot_map [ConcMap $tot_map 0 [lindex $EFFECTS_MAPPINGS($image) $i] 0]
   }

   return $tot_map
}

proc TranImage {data map trandata section} {
#+
#  Name:
#     TranImage
#
#  Purpose:
#     Transform an image using a specified mapping.
#
#  Arguments:
#     data
#        The name of the input image.
#     map
#        A list holding the 6 coefficients representing the mapping.
#     trandata
#        The name of the output image to create.
#     section
#        The required section of the output image. If this is a null
#        string, then the output image is just big enough to contain the
#        entire input image.
#
#  Returned Value:
#     One for success, zero if KAPPA:REGRID failed.
#
#-
   global INTERP

# Assume success.
   set ok 1

# If the mapping is undefined, return zero.
   if { $map == "" } {
      set ok 0

# Otherwise, if a unit mapping has been supplied, just copy the required image
# section.
   } elseif { $map == "ref" } {
      append data $section
      if { ![Obey ndfpack ndfcopy "in=$data out=$trandata"] } {
         set ok 0
      }

# For any other mapping, construct an AST Mapping.
   } {
      set trn [MakeTrn $map]
      if { $trn != "" } {

# If only part of the output image is required, set up the relevant
# REGRID parameters.
         set shape "lbound=\! ubound=\!"

         if { $section != "" } {
            set sec [SecList $section]
            if { $sec != "" } {
               set xlo [lindex $sec 0]
               set xhi [lindex $sec 1]
               set ylo [lindex $sec 2]
               set yhi [lindex $sec 3]
               set shape "lbound=\[$xlo,$ylo\] ubound=\[$xhi,$yhi\]"
            }
         }

# Get the interpolation method in a form which KAPPA:REGRID can use.
         if { $INTERP == "Linear" } {
            set method LINEAR
         } {
            set method NEAREST
         }

         set ok [Obey kappa regrid "method=$method out=$trandata in=$data $shape mapping=$trn"]

# Delete the Mapping file.
         catch "exec rm -f $trn"
      }
   }
   return $ok
}

proc TranList {map inv px_i py_i px_out py_out} {
#+
#  Name:
#     TranList
#
#  Purpose:
#     Transforma a list of pixel coordinates using a supplied mapping.
#
#  Arguments:
#     map
#        A list of 6 coefficient values, or "ref" for a unit mapping.
#     inv
#        If non-zero then the inverse mapping is used. Otherwise the
#        forward mapping is used.
#     px_i
#        The list of x pixel coordinates to be transformed.
#     py_i
#        The list of y pixel coordinates to be transformed.
#     px_out
#        The name of the variable in the calling procedure in which to place
#        the list of transformed x pixel coordinates.
#     py_out
#        The name of the variable in the calling procedure in which to place
#        the list of transformed y pixel coordinates.
#
#  Returned Value:
#     1 for success, 0 for failure.
#-
   upvar $px_out px_o
   upvar $py_out py_o

# Assume failure.
   set ok 0

# If required, invert the mapping.
   if { $inv } { set map [InvMap $map] }

# For a unit mapping, just copy input to output.
   if { $map == "ref" } {
      set px_o $px_i
      set py_o $py_i
      set ok 1

# For any other defined mapping, transform each point in turn.
   } elseif { $map != "" } {

      set c1 [lindex $map 0]
      set c2 [lindex $map 1]
      set c3 [lindex $map 2]
      set c4 [lindex $map 3]
      set c5 [lindex $map 4]
      set c6 [lindex $map 5]

      set out_x ""
      set out_y ""

      set len [llength $px_i]
      for {set i 0} {$i < $len} {incr i} {

         set x [lindex $px_i $i]
         set y [lindex $py_i $i]

         lappend out_x [expr $c1 + $c2 * $x + $c3 * $y ]
         lappend out_y [expr $c4 + $c5 * $x + $c6 * $y ]

      }

      set px_o $out_x
      set py_o $out_y
      set ok 1
   }

   return $ok

}

proc TranPXY {map inv im_in obj_in im_out obj_out} {
#+
#  Name:
#     TranPXY
#
#  Purpose:
#     Create a new positions list from an existing one by mapping the
#     pixel coordinates of the supplied list using a supplied mapping.
#
#  Arguments:
#     map
#        Specifies a mapping to apply to the pixel coordinates. It should be
#        the name of an HDS container file (without the .sdf) containing a
#        transform structure.
#     inv
#        If non-zero, then the inverse of the mapping specified by "map" is
#        used. Otherwise the forward mapping is used.
#     im_in
#        The image assocaited with the positions to be mapped.
#     obj_in
#        The object type of the positions to be mapped.
#     im_out
#        The image with which to associate the mapped positions.
#     obj_out
#        The object type of the mapped positions.
#
#  Globals:
#     PNTPX (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel X coordinates.
#     PNTPY (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of pixel Y coordinates.
#     PNTTAG (Read and Write)
#        A 2-d array indexed by image and object type. Each element
#        is a list of canvas tags (one for each position).
#
#  Notes:
#     - The output list is not displayed on the screen by this procedure.
#     - The canvas coordinates in the output list are just a copy of the
#     input canvas coordinates and do not take into account the supplied
#     mapping.
#-
   global CAN
   global E_RAY_FEATURES
   global IMAGES
   global O_RAY_FEATURES
   global PNTCX
   global PNTCY
   global PNTID
   global PNTLBL
   global PNTNXT
   global PNTPX
   global PNTPY
   global PNTVID
   global PNTTAG
   global NPOLY
   global RECALC_IMMAP
   global RECALC_OEMAP

# Erase the output object.
   Erase $im_out $obj_out

# Unless we are creating a mask (which do not effect mappings), indicate
# that the mappings related to the returned positions lists will need to be
# recalculated.
   if { $obj_out == $O_RAY_FEATURES || $obj_out == $E_RAY_FEATURES } {
      set RECALC_OEMAP($im_out) 1
      set RECALC_IMMAP($im_out) 1

# If the current image is the first (reference) image, then all image
# mappings will need to be re-calculated, because all mappings go to the
# reference image.
      if { $im_out != [lindex $IMAGES 0] } {
         foreach im $IMAGES {
            set RECALC_IMMAP($im) 1
         }
      }

      set stag ""

# If we are creating masks, get a suffix for the canvas tags associated
# with each position.
   } {
      set stag "C"
      append stag $NPOLY
      incr NPOLY
   }

# If the input list is not empty.
   if { [info exists PNTPX($im_in,$obj_in)] } {
      set size [llength $PNTPX($im_in,$obj_in)]
      if { $size > 0 } {

# Copy the input lists to the output lists, indicating that markers and
# vectors currently are not drawn (by setting their canvas identifiers to
# -1). Do not copy the canvas coordinates since they may not take effects
# mappings into account.
         set PNTPX($im_out,$obj_out) $PNTPX($im_in,$obj_in)
         set PNTPY($im_out,$obj_out) $PNTPY($im_in,$obj_in)
         set PNTLBL($im_out,$obj_out) $PNTLBL($im_in,$obj_in)
         set PNTNXT($im_out,$obj_out) $PNTNXT($im_in,$obj_in)

         for {set i 0} {$i < $size} {incr i} {
            lappend PNTID($im_out,$obj_out) -1
            if { [lindex $PNTVID($im_in,$obj_in) $i] != "" } {
               lappend PNTVID($im_out,$obj_out) -1
            } {
               lappend PNTVID($im_out,$obj_out) ""
            }

# Append a unique string to the tags from the input lists.
            set tag [lindex $PNTTAG($im_in,$obj_in) $i]
            append tag $stag
            lappend PNTTAG($im_out,$obj_out) $tag

         }

# Transform the pixel coordinates and store them in the output position
# lists.
         TranList $map $inv $PNTPX($im_in,$obj_in) \
                  $PNTPY($im_in,$obj_in) PNTPX($im_out,$obj_out) \
                  PNTPY($im_out,$obj_out)
       }
   }
}

proc TranSec {section map inv} {
#+
#  Name:
#     TranSec
#
#  Purpose:
#     Transform a section string using a given mapping. The returned
#     section just encompasses the entire suplied section, after mapping it
#     using the supplied mapping.
#
#  Arguments:
#     section
#        The section string to be mapped.
#
#  Returned Value:
#     The mapped section string, or a blank string if anything goes wrong.
#-

# Assume failure.
   set newsec ""

# Extract the bounds from the section string.
   set sec [SecList $section]
   if { $sec != "" } {
      set lx [lindex $sec 0]
      set ux [lindex $sec 1]
      set ly [lindex $sec 2]
      set uy [lindex $sec 3]

# Form list of pixel coordinates at the four corners of the section.
      set px "$lx $lx $ux $ux"
      set py "$ly $uy $uy $ly"

# Map these positions and then find a section string describing ther
# bounding box.
      set newsec [BoundBox $px $py $map $inv]
   }

   return $newsec

}

proc UniqueFile {} {
#+
#  Name:
#     UniqueFile
#
#  Purpose:
#     Returns a unique file name for which no file currently exists.
#     These files are created in the POLKA_SCRATCH directory
#     created by Polka, and so do not need to be deleted when finished
#     with as they will all be deleted when the temporary ADAM_USER
#     directory is deleted when Polka exits.
#
#  Arguments:
#     None.
#
#  Returned Value:
#     The file name.
#
#  Globals:
#     POLKA_SCRATCH (Read)
#        The path to the POLKA_SCRATCH directory.
#     IFILE (Read and Write)
#        File names have a name of the form polka<i> where <i> is an
#        integer, which is different for each file. IFILE
#        records the value of i used in the previous call to this
#        function. The first value of i considered is one greater than
#        that used last time.
#
#-
   global POLKA_SCRATCH
   global IFILE

   incr IFILE
   set file "$POLKA_SCRATCH/polka$IFILE"

   while { [llength [glob -nocomplain ${file}.*] ] != 0 } {
      incr IFILE
      set file "$POLKA_SCRATCH/polka$IFILE"
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
#     SECTION_REQ (Write)
#        The section to be displayed next.
#     SECTION_STACK (Read and Write)
#         A stack of the previously displayed sections. This is stored as
#         a list with the oldest section is at the end of the list.
#     UNZOOM (Read)
#         The path to the Unzoom button.
#-
   global SECTION_REQ
   global SECTION_STACK
   global UNZOOM

# Only proceed if this is not part of a double click sequence.
   if { ![DoubleClick UNZOOM_CLICK] } {

# If the section stack is not empty...
      while { [llength $SECTION_STACK] > 0 } {

# Pop the top section off the section stack.
         set SECTION_REQ [Pop SECTION_STACK]

# If this section is not the same as the one below it, leave the loop.
         if { $SECTION_REQ != [Top SECTION_STACK] } { break }

      }

# Disable the unzoom button when the stack is emptied.
      if {  [llength $SECTION_STACK] == 0 } {
         $UNZOOM configure -state disabled
      }

# Update the display.
      UpdateDisplay
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
#  Globals:
#     SECTION_REQ (Write)
#        The section to be displayed next.
#     SECTION_STACK (Read and Write)
#         A stack of the previously displayed sections. This is stored as
#         a list with the oldest section is at the end of the list.
#     UNZOOM (Read)
#         The path to the Unzoom button.
#-
   global SECTION_REQ
   global SECTION_STACK
   global UNZOOM

# Indicate that this is part of a double click sequence.
   DoubleClick UNZOOM_CLICK 1

# If there is anything on the stack, get the bottom section off
# the section stack.
   if { [llength $SECTION_STACK] > 0 } {
      set SECTION_REQ [Pop SECTION_STACK -1]
   }

# Ensure the section stack is empty, and disable the Unzoom button.
   set SECTION_STACK ""
   $UNZOOM configure -state disabled

# Update the display.
   UpdateDisplay

}

proc UpdateDisplay {args} {
#+
#  Name:
#     UpdateDisplay
#
#  Purpose:
#     Update the display to reflect requested changes.
#
#  Arguments:
#     args
#        An optional argument equal to "gwm", "ref" or "cur". Its
#        presence forces redisplay at the specified depth even if nothing
#        has changed.
#
#  Globals:
#      CUROBJ_DISP (Read)
#          The type of object currently being entered.
#      CUROBJ_REQ (Read)
#          The type of object to be entered.
#      IMSEC_DISP (Read)
#          The currently displayed image (as supplied by the user -
#          potetially including a section string).
#      IMSEC_REQ (Read)
#          The unscaled base image section to be displayed. The string
#          includes an NDF section specifier expressed in standard form (i.e.
#          as two ranges of pixel indices).
#      PHI_DISP (Read)
#          The current upper percentile for the displayed image scaling.
#      PHI_REQ (Read)
#          The requested upper percentile for the displayed image scaling.
#      PLO_DISP (Read)
#          The current lower percentile for the displayed image scaling.
#      PLO_REQ (Read)
#          The requested lower percentile for the displayed image scaling.
#      REFIM_DISP (Read)
#          The current image from which reference objects are derived.
#      REFIM_REQ (Read)
#          The requested image from which to derive reference objects.
#      REFOBJ_DISP (Read)
#          The type of reference object currently displayed.
#      REFOBJ_REQ (Read)
#          The requested type of reference object.
#      SECTION_DISP (Read)
#          The currently displayed section (eg "(12:234,35:256)" ).
#      SECTION_REQ (Read)
#          The requested section (eg "(12:234,35:256)" ).
#-
   global CUROBJ_DISP
   global CUROBJ_REQ
   global E_RAY_MASK
   global E_RAY_SKY
   global IMSEC_DISP
   global IMSEC_REQ
   global MODE
   global NONE
   global O_RAY_FEATURES
   global O_RAY_MASK
   global O_RAY_SKY
   global PHI_DISP
   global PHI_REQ
   global PLO_DISP
   global PLO_REQ
   global RB_CUR
   global RB_REF
   global REFALIGN
   global REFALN
   global REFIM_DISP
   global REFIM_REQ
   global REFOBJ_DISP
   global REFOBJ_REQ
   global SECTION_DISP
   global SECTION_REQ

# Cancel any selected area.
   CancelArea

# If a new image is being displayed, set the reference objects to "None",
# set the Current objects to "O-ray features", and turn off the
# "Draw-alinged" button.
   if { $IMSEC_REQ != $IMSEC_DISP } {
      $RB_REF($NONE) select
      $RB_CUR($O_RAY_FEATURES) select
      $REFALIGN deselect
   }

# The display is layered. At the bottom is the GWM image. Next comes the
# markers (canvas items) for the reference objects. The markers for the
# current objects are at the top. When lower layers are re-drawn they
# wipe out the higher layers, and so the higher layers also need to be
# re-drawn even if they have not changed...

# If the requested image or section is not the same as for the displayed image,
# then everything will need to be re-drawn.
   if { $IMSEC_REQ != $IMSEC_DISP ||
        $SECTION_REQ != $SECTION_DISP ||
        $PLO_REQ != $PLO_DISP ||
        $PHI_REQ != $PHI_DISP ||
        $args == "gwm" } {
      set drawgwm 1
      set drawref 1
      set drawcur 1

# If the image section is unchanged, but the reference objects have changed
# then we need not re-draw the image section.
   } elseif { $REFOBJ_REQ != $REFOBJ_DISP ||
              $REFIM_REQ != $REFIM_DISP ||
              $args == "ref" } {
      set drawgwm 0
      set drawref 1
      set drawcur 1

# If the reference objects are unchanged, but the current objects have changed
# then we need not re-draw the reference objects.
   } elseif { $CUROBJ_REQ != $CUROBJ_DISP ||
              $args == "cur" } {
      set drawgwm 0
      set drawref 0
      set drawcur 1

# If nothing has changed, we don't need to re-draw anything.
   } {
      set drawgwm 0
      set drawref 0
      set drawcur 0
   }

# Erase the display components which will change...
   if { $drawcur } { ClearCur }
   if { $drawref } { ClearRef }
   if { $drawgwm } { ClearGwm 1 }

# Set the interaction mode depending on the type of object being entered.
# Do not change the mode if we are in the process of identifying an image
# feature.
   if { $MODE != 3 } {
      if { $CUROBJ_REQ == $O_RAY_MASK || $CUROBJ_REQ == $E_RAY_MASK ||
           $CUROBJ_REQ == $O_RAY_SKY || $CUROBJ_REQ == $E_RAY_SKY } {
         SetMode 1
      } {
         SetMode 0
      }
   }

# Re-draw the display components which have changed.
   if { $drawgwm } { DrawGwm }
   if { $drawref } { DrawRef }
   if { $drawcur } { DrawCur }

# If necesary, disable one of the radio buttons used to select the
# reference object type so that the current and reference objects cannot be
# identical.
   CheckRef

# Set a flag indicating if features labels can be generated automatically.
# This is the case if no labels yet exist.
   Labels "" 1

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
#    OLD_FOCUS (Write)
#      The window which has focus prior to giving focus to the text entry
#      widget.
#    OLD_VAL (Write)
#      The previous (valid) value displayed in the text entry widget.
#    REDISPLAY_CANCELLED (Read and Write)
#      Was a previous redisplay of the image cancelled because the
#      user looked like he may be about to enter a new scaling value?
#    REDISPLAY_REQUESTED (Read and Write)
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
      "if { \$REDISPLAY_REQUESTED } {
          set REDISPLAY_REQUESTED 0
          set REDISPLAY_CANCELLED 1
       }
       $name.ent select from 0
       $name.ent select to end

       set OLD_F_OWNER \$F_OWNER
       set F_OWNER $name.ent
       focus $name.ent

       set OLD_VAL \$$value"

# When the pointer leaves the text entry area, clear the current selection,
# pass the focus back to the window which had it before, and check that the
# current text represents a valid value (if not, the old value will be
# re-instated).
   bind $name.ent <Leave> \
      "$name.ent select clear
      set F_OWNER \$OLD_F_OWNER
      focus \$OLD_F_OWNER
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
      "global REDISPLAY_REQUESTED
       global REDISPLAY_CANCELLED
       if { \$REDISPLAY_REQUESTED } {
          set REDISPLAY_REQUESTED 0
          set REDISPLAY_CANCELLED 1
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

# Do the sdame for the increment button.
   button $name.right -bitmap @$POLPACK_DIR/right_arrow.bit -command {}
   $name.right configure -activebackground [lindex [$name.right configure -background] end]
   $name.right configure -activeforeground [lindex [$name.right configure -foreground] end]
   bind $name.right <ButtonPress-1> \
      "global REDISPLAY_REQUESTED
       global REDISPLAY_CANCELLED
       if { \$REDISPLAY_REQUESTED } {
          set REDISPLAY_REQUESTED 0
          set REDISPLAY_CANCELLED 1
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
   global CAN
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
      set old_cancur [$CAN cget -cursor]
      $CAN config -cursor watch
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
      $CAN config -cursor $old_cancur
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
#     The returned value from the widget command, or a blank string if
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
#     CAN (Read)
#        The name of the canvas widget holding the GWM image.
#     POLPACK_DIR (Read)
#        The path to the directory containing the POLPACK bitmaps.
#     SIZE (Read)
#        The size of the square canvas in screen pixels.
#     XHAIR (Read)
#        If non-zero then a cross-hair is required. If zero then no
#        cross-hair is required.
#     XHAIR_IDV (Read and Write)
#        The canvas item identifier for the vertical line forming the
#        cross-hair. Set blank if no line exists.
#     XHAIR_IDH (Read and Write)
#        The canvas item identifier for the horizontal line forming the
#        cross-hair. Set blank if no line exists.
#     XHRCOL (Read)
#        The colour for the cross-hair.
#-
   global CAN
   global POLPACK_DIR
   global SIZE
   global XHAIR
   global XHAIR_IDV
   global XHAIR_IDH
   global XHRCOL

# If we require a cross-hair...
   if { $XHAIR } {

# but no cross hair canvas lines currently exist...
      if { $XHAIR_IDH == "" } {

# Save the current cursor on a stack, and set a blank bit map as the
# current cursor (this causes no cursor to be visible).
         Push CURSOR_STACK [$CAN cget -cursor]
         $CAN configure -cursor [list @$POLPACK_DIR/blank.bit white]

# Create the two canvas lines forming the cross hair and set their coordinates.
         set XHAIR_IDH [$CAN create line 0 $cy $SIZE $cy -fill $XHRCOL]
         set XHAIR_IDV [$CAN create line $cx 0 $cx $SIZE -fill $XHRCOL]

# If the cross-hair already exists, set their coordinates.
      } {
         $CAN coords $XHAIR_IDH 0 $cy $SIZE $cy
         $CAN coords $XHAIR_IDV $cx 0 $cx $SIZE
      }

# If no cross-hair is required...
   } {

# but the cross-hair currently exists...
      if { $XHAIR_IDH != "" } {

# Re-instate the cursor from the top of the cursor stack.
         $CAN configure -cursor [Pop CURSOR_STACK]

# Delete the cross hair canvas items.
         $CAN delete $XHAIR_IDH
         $CAN delete $XHAIR_IDV

# Set the global variables to indicate that we currently do not have a
# cross hair.
         set XHAIR_IDH ""
         set XHAIR_IDV ""
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
#     SELECTED_AREA (Read)
#        The bounds of the selected area in the order xmin, xmax, ymin,
#        ymax.
#
#-
   global SELECTED_AREA
   global SECTION_STACK
   global SECTION_DISP
   global SECTION_REQ
   global UNZOOM

# Do nothing if there is no selected area.
   if { $SELECTED_AREA != "" } {

# Get the bounds in canvas coordinates of the selected area.
      set cxlo [lindex $SELECTED_AREA 0]
      set cylo [lindex $SELECTED_AREA 1]
      set cxhi [lindex $SELECTED_AREA 2]
      set cyhi [lindex $SELECTED_AREA 3]

# Convert these to pixel coordinates. Note, the Y axis is reversed since the
# TK origin is at the UPPER left corner.
      set pxyl [CanToNDF $cxlo $cyhi]
      if { $pxyl == "" } { return }
      set pxlo [lindex $pxyl 0]
      set pylo [lindex $pxyl 1]
      set pxyl [CanToNDF $cxhi $cylo]
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
# restored later using the Unzoom button.
         Push SECTION_STACK $SECTION_DISP
         $UNZOOM configure -state normal

# Display the modified section.
         set SECTION_REQ "($ipxlo:$ipxhi,$ipylo:$ipyhi)"
         UpdateDisplay
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


proc Transfer {} {
#+
#  Name:
#     Transfer
#
#  Purpose:
#     Transfer the current features from the currently selected image to
#     a group of other images.
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
   global BACKCOL
   global CB_COL
   global IMAGES
   global IM_TRAN
   global POLPACK_DIR
   global TRAN_LIST
   global TRANSFER_EXIT
   global IMAGE_DISP

# Create the top level window for the dialogue box, and set its title.
   set top .items
   set topf [MakeDialog $top "Transfer features, etc" 1]

# Find the length of the longest image name.
   set maxl 0
   foreach image $IMAGES {
      set l [string length $image]
      if { $l > $maxl } { set maxl $l }
   }

# Create a frame to put the checkbuttons in.
   set fr0 [frame $topf.fr0]
   pack $fr0 -side top
   SetHelp $fr0 ".  Click the buttons corresponding to the images to which the current features are to be transferred.\nAny which already have any features are indicated by a tick mark."

# Create four columns within this frame.
   set fr1 [frame $fr0.fr1]
   set fr2 [frame $fr0.fr2]
   set fr3 [frame $fr0.fr3]
   set fr4 [frame $fr0.fr4]
   pack $fr1 $fr2 $fr3 $fr4 -side left -anchor n -padx 4m -pady 4m

# Indicate that no images have yet been selected.
   set TRAN_LIST ""

# Create a check button for each image name, place them alternately in
# the frames just created.
   set f $fr1
   for {set i 0} {$i < [llength $IMAGES]} {incr i} {
      set IM_TRAN($i) 0
      set im [lindex $IMAGES $i]

# Each line in the Frame consists of a checkbutton, with a label at the
# end of it, consisting of a tick mark. Create the frame for this line.
      set lfrm [frame $f.f_$i]

# If this image is the currently displayed image, then disable the check
# button.
      if { $im == $IMAGE_DISP } {
         set state "disabled"
      } {
         set state normal
      }

#  Create the checkbutton within this frame.
      set cb($i) [checkbutton $lfrm.cb -selectcolor $CB_COL -variable IM_TRAN($i) \
                  -highlightthickness 0 -width $maxl -anchor nw \
                  -text "$im" -state $state -command \
        "if { \$IM_TRAN($i) } {
            lappend TRAN_LIST $i
         } {
            set j \[lsearch \$TRAN_LIST $i \]
            if { \$j > -1 } {
               set TRAN_LIST \[lreplace \$TRAN_LIST \$j \$j]
            }
         }"]

         pack $cb($i) -side left

# Pack a label containing a tick mark to the right of the checkbutton. If
# image has features make the tick visible by setting its foreground
# colour to red. Otherwise, hide the tick by setting its foreground
# colour to the background colour.
      if { [HasFea $im] } {
         set col red
      } {
         set col $BACKCOL
      }
      set tick($im) [label $lfrm.tk -bitmap @$POLPACK_DIR/tick.bit \
                                    -foreground $col]
      pack $tick($im) -side left

# Pack the line frame.
      pack $lfrm -side top -pady 1m -anchor nw

# Swap frames.
      if { $f == $fr1 } {
         set f $fr2
      } elseif { $f == $fr2 } {
         set f $fr3
      } elseif { $f == $fr3 } {
         set f $fr4
      } {
         set f $fr1
      }
   }

# Create a frame for the buttons.
   set fr5 [frame $topf.fr5]
   pack $fr5 -side top -expand 1 -fill x

# Create the OK and Cancel buttons.
   set b1 [button $fr5.ok -text "OK" -command "set TRANSFER_EXIT ok"]
   set b3 [button $fr5.cancel -text "Cancel" -command "set TRANSFER_EXIT cancel"]
   pack $b1 $b3 -padx 1m -side left -expand 1
   SetHelp $b1 ".  Press to close the dialog box, transferring the features from the current image to all the selected images."
   SetHelp $b3 ".  Press to close the dialog box without making any changes to the selected images."

# Create the ClearAll and SetAll and Help buttons.
   set b4 [button $fr5.cl -text "ClearAll" -command "set TRANSFER_EXIT clear"]
   set b5 [button $fr5.st -text "SetAll" -command "set TRANSFER_EXIT set"]
   set b6 [button $fr5.help -text "Help" -command "set TRANSFER_EXIT help"]
   pack $b4 $b5 $b6 -padx 1m -side left -expand 1
   SetHelp $b4 ".  Press to clear all check buttons."
   SetHelp $b5 ".  Press to set all check buttons."
   SetHelp $b6 ".  Press to see more help on this window."

# Ensure that closing the window from the window manager is like pressing
# the Cancel button.
   wm protocol $top WM_DELETE_WINDOW "set TRANSFER_EXIT cancel"

# Loop until an exit button is pressed.
   set exit 0
   while { !$exit } {

# Wait for the user to press a button.
      tkwait variable TRANSFER_EXIT

# If the cancel button was pressed, exit without doing anything.
      if { $TRANSFER_EXIT == "cancel" } {
         set exit 1

# If the OK button was pressed, transfer all features from the current
# image to the selected images.
      } elseif { $TRANSFER_EXIT == "ok" } {

# Tell the user what is happening.
         set told [SetInfo "Transferring feature positions. Please wait..." 0]

# Transfer the positions.
         for {set i 0} {$i < [llength $IMAGES]} {incr i} {
            if { $IM_TRAN($i) } {
               set im [lindex $IMAGES $i]
               TransFea $IMAGE_DISP $im
               $tick($im) configure -foreground red
               update idletasks
            }
         }
         set exit 1

# Cancel the informative text set earlier in this procedure.
         if { $told } { SetInfo "" 0 }

# If the ClearAll button was pressed, clear all the check button variables.
      } elseif { $TRANSFER_EXIT == "clear" } {
         for {set i 0} {$i < [llength $IMAGES]} {incr i} {
            if { $IM_TRAN($i) } { $cb($i) invoke }
         }

# If the SetAll button was pressed, set all the check button variables.
      } elseif { $TRANSFER_EXIT == "set" } {
         for {set i 0} {$i < [llength $IMAGES]} {incr i} {
            if { !$IM_TRAN($i) } { $cb($i) invoke }
         }

# If the Help button was pressed, display help.
      } elseif { $TRANSFER_EXIT == "help" } {
         ShowHelp "POLKA_TRANSFER_DIALOG"
      }
   }

# Destroy the dialog box.
   destroy $top

}


proc HasFea {image} {
#+
#  Name:
#     HasFea
#
#  Purpose:
#     Returns 0 or 1 indicating if there are currently any features
#     associated with the supplied image.
#
#  Arguments:
#     image
#        The name of the image.
#
#  Globals:
#     E_RAY_FEATURES (Read)
#        An integer representing the "E-ray features" object type.
#     O_RAY_FEATURES (Read)
#        An integer representing the "O-ray features" object type.
#-
   global E_RAY_FEATURES
   global O_RAY_FEATURES

# Initialise the returned value to indicate that the image has no
# features.
   set ret 0

# Loop round each object type...
   foreach object [list $O_RAY_FEATURES $E_RAY_FEATURES] {

# If there are any positions in this list, return the value 1.
      if { [NumPosn "" $image $object] > 0 } {
         set ret 1
         break
      }
   }

   return $ret
}

proc TransFea {in out} {
#+
#  Name:
#     TranFea
#
#  Purpose:
#     Transfer all features from image $in to image $out.
#
#  Arguments:
#     in
#        The name of the source image.
#     out
#        The name of the destination image.
#
#  Globals:
#     E_RAY_FEATURES (Read)
#        An integer representing the "E-ray features" object type.
#     O_RAY_FEATURES (Read)
#        An integer representing the "O-ray features" object type.
#-
   global E_RAY_FEATURES
   global O_RAY_FEATURES
   global E_RAY_MASK
   global O_RAY_MASK
   global E_RAY_SKY
   global O_RAY_SKY
   global REFIM_DISP
   global REFOBJ_DISP
   global OEMAP
   global IMMAP
   global PNTPX
   global PNTPY
   global PNTLBL

#  See if the destination image currently has any defined features.
   set hasfea [HasFea $out]

#  If so, confirm that it is ok to replace them. Return if not.
   if { $hasfea && ![Confirm "Replace features in image\"$out\"?"] } {
      return
   }

# Delete any existing features, plus masks and sky areas.
   if { $hasfea } {

# Delete the OE mapping (if any) for this image.
      if { [info exists OEMAP($out)] } {
         unset OEMAP($out)
      }

# Delete the image mapping (if any) for this image.
      if { [info exists IMMAP($out)] } {
         unset IMMAP($out)
      }

# Loop round each object type...
      foreach object [list $O_RAY_FEATURES $E_RAY_FEATURES $O_RAY_MASK \
                           $E_RAY_MASK $O_RAY_SKY $E_RAY_SKY] {

# Continue deleting the first position in the list until there are no
# positions left.
         while { [NumPosn "" $out $object] > 0 } {
            DelPosn 0 0 $out $object
         }
      }

# If this image is the reference image, clear the reference objects.
      if { $out == $REFIM_DISP } {ClearRef}

   }

# Transfer the O and E ray features, centroiding each feature position in
# the destination image.
   foreach object [list $O_RAY_FEATURES $E_RAY_FEATURES] {
      GetCents $PNTPX($in,$object) $PNTPY($in,$object) $PNTLBL($in,$object) \
               $out $object
   }

# If this image is the reference image, re-draw the reference objects.
   if { $out == $REFIM_DISP } {DrawRef}
}

proc GetCents {px py rlabel image object} {
#+
#  Name:
#     GetCent
#
#  Purpose:
#     Check if supplied pixel positions can be used as features,
#     and if so, adds the features to the list of current features.
#     No markers are displayed or used. The image should not be the
#     currently displayed image.
#
#  Arguments:
#     px, py
#        A list of pixel coordinates of the initial guesses at the feature
#        positions.
#     rlabel
#        A list of labels to be used for the new features. This should
#        have the same number of elements as pc and py.
#     image
#        The image in which to do the centroiding.
#     object
#         The object type; O_RAY_FEATURES or E_RAY_FEATURES.
#
#  Returned Value:
#      The number of features which could not be centroided.
#
#  Globals:
#     PSF_SIZE (Read)
#        The typical size of a feature in pixel.
#-
   global IMAGE_STACK
   global PSF_SIZE

# Assume no bad values.
   set nbad 0

# Take a copy of the supplied labels.
   set rlabs $rlabel

# See how many positions are to be procesed.
   set np [llength $px]

# If the positions are to be centroided, replace the supplied lists of
# pixel coordinates with accurate coordinates.
   if { $PSF_SIZE > 0 } {

# Write out the initial pixel coordinates to a text file to be passed
# to POLCENT.
      set tfile [UniqueFile]
      set tfile_id [open $tfile w]

      for {set i 0} {$i < $np} {incr i} {
         puts $tfile_id "[lindex $px $i] [lindex $py $i]"
      }

      close $tfile_id

# Select a name for the POLCENT output text file.
      set tofile [UniqueFile]

# Calculate the box size and max shift values.
      set isize [expr 2 * $PSF_SIZE]
      set maxsh [expr 4 * $PSF_SIZE]

# Attempt to centroid them (use the image including all the effects
# applied by the user).
      set imsec "[Top IMAGE_STACK($image)]"
      if { [Obey polpack polcent "ndf=\"$imsec\" maxshift=$maxsh isize=$isize infile=$tfile outfile=$tofile"] } {

# If succesful, read the accurate feature coordinates from the output
# file. Create a Tcl list containing the X and Y values, replacing "D"
# exponents by "E".
         set qx ""
         set qy ""
         set tofile_id [open $tofile r]

         while { [gets $tofile_id line] != -1 } {
            regsub -nocase -all D $line E line2
            lappend qx [lindex $line2 0]
            lappend qy [lindex $line2 1]
         }

         close $tofile_id

# Count and remove any positions which could not be centroided. These
# are flagged by polcent by returning -100000 for X and Y.
         unset px
         unset py
         unset rlabs
         for {set i 0} {$i < $np} {incr i} {
            set ppx [lindex $qx $i]
            if { $ppx != -100000 } {
               set ppy [lindex $qy $i]
               set lab [lindex $rlabel $i]
               lappend px $ppx
               lappend py $ppy
               lappend rlabs $lab
            } {
               incr nbad
            }
         }

#  Adjust the number of positions to exclude any which could not be
#  centroided.
         set np [expr $np - $nbad]

# Warn the user about the bad positions.
         if { $nbad > 1 } {
            Message "Accurate positions could not be found for $nbad features in image \"$image\"."
         } elseif { $nbad > 0 } {
            Message "An accurate position could not be found for 1 feature in image \"$image\"."
         }

# If the position could not be centroided, indicate we have no good
# positions.
      } {
         set px ""
         set py ""
         set nbad $np
         set np 0
      }
   }

# Loop round each good position.
   for {set i 0} {$i < $np} {incr i} {

# Get its pixel positions.
      set ppx [lindex $px $i]
      set ppy [lindex $py $i]

# See if a feature already exists at these pixel coordinates.
# If there is no existing feature at this position, use it.
      if { [ FindPosn "PX PY" [list $ppx $ppy] 2 $image $object] == "" } {

# Get the  label for this position.
         set lab [lindex $rlabs $i]

# Create a new position.
         set newi [SetPosn -1 "PX PY LBL" [list $ppx $ppy $lab] $image $object ]
      }
   }

   return $nbad
}

