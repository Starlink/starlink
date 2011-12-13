class Gwmview {
#+
#  Name:
#     Gwmview

#  Purpose:
#     Provide a window which can hold a GWM display.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Description:
#     This class provides a mega-widget for holding a GWM canvas item
#     on a canvas.  The GWM item can be destroyed and recreated at
#     a different size on the same widget.  As well as the GWM display
#     itself this widget contains buttons which are likely to be
#     useful in conjunction with it, in particular enough buttons for
#     the user to be able to do the following:
#        - Indicate that the widget is no longer needed
#        - Specify what kind of colour table is wanted (not yet done)
#        - Specify a zoom level
#
#     The value of the zoomlevel is significant to the widget in that
#     the 'view'-type coordinates with which one talks to this widget
#     are mapped to pixel values using it.
#
#     This widget does any necessary worrying about scrolling the canvas,
#     resizing the window, and so on.  Resizing is allowed, but there
#     is a minimum size determined by the size of the buttons panel
#     (currently not properly implemented).
#
#     Some methods supplied by this widget require or return coordinates
#     on the GWM item.  In these cases 'view coordinates' are used;
#     the origin and extent of the GWM window in view coordinates is set
#     when it is constructed with the makegwm method, so that it can
#     effectively be addressed in any coordinate system of your choice.
#     The methods canv2view and view2canv are supplied for converting
#     between this coordinate system and Tk canvas coordinates which
#     are needed if the canvas needs to be addressed directly, for
#     instance to create canvas item bindings.

#  Notes:
#     The canvas on which the GWM item is displayed can be accessed
#     by using its widget name, which is held in the public variable
#     canvas.  The following tag names are used to refer to items
#     on the canvas:
#        - gwmitem  -- The GWM item
#
#     The display method contains elements of a workaround for a bug in
#     the PGPLOT driver for the GWM widget.  This is fixed in PGPLOT
#     5.2.0-6, but not in the Spring 2000 release.  When the fixed version
#     can be relied on to be there, this should be removed.

#  Public Methods:
#     activate
#        Sets the status so that the widget is ready to be used.  This
#        method should be called after sufficient configuration has been
#        done that the widget can usefully be displayed and the user
#        can begin to interact with it.  Before calling this method
#        the widget is likely to be in a non-interacting state; many
#        of the other methods will not take any effect until the
#        widget has been put into an active status.
#
#     canvas
#        Returns the name of the canvas widget on which any GWM item
#        will be drawn.  This name will not change during the lifetime
#        of the Gwmview widget.
#
#     canv2view cx cy
#        Converts canvas coordinates to viewer coordinates.  Returns a
#        two-element list giving viewer coordinates corresponding to
#        the arguments.
#           - cx       -- X coordinate in Tk canvas coordinates
#           - cy       -- Y coordinate in Tk canvas coordinates
#
#     childsite
#        Returns the path of a frame into which windows can be placed.
#
#     deactivate
#        Put the widget into the 'inactive' status.  After this call many
#        of the other methods may not reflect their activity in the
#        visible state of the widget.  Inquiring the state of the widget,
#        for instance using the points method, should continue to work
#        however.  This method can be called to prevent further user
#        interaction with the widget and is called when the 'Done'
#        button is pushed.  This method can be overridded by children
#        of this class to do additional shutdown, for instance to
#        remove bindings (see the unbindall method).
#
#     devname
#        Returns the name of the graphics device associated with the GWM,
#        as used for submitting to ADAM tasks.
#
#     display
#        Updates the display with respect to the current state of the
#        widget.  It is called, for instance, when the zoomfactor is
#        updated.  This may not do much in this widget, but widgets
#        which inherit from it can override this method to do whatever
#        display updating they may require.  This method may be
#        called more often than is necessary, so if it is going to do
#        anything slow then it ought to check whether a redisplay is
#        really required before doing it.
#
#     gwmname
#        Returns the name of the GWM.
#
#     makegwm xorigin yorigin xsize ysize ?name?
#        Actually makes the GWM display, as a GWM item on a scrolled
#        canvas whose name can be got using the canvas method.
#           - xorigin  -- X view coordinate of the bottom left corner
#           - yorigin  -- Y view coordinate of the bottom left corner
#           - xsize    -- X extent in view coordinates of the GWM item
#           - ysize    -- Y extent in view coordinates of the GWM item
#           - ?name?   -- Name of the GWM (a unique name will be generated
#                         if this is not given)
#
#        The size of the resulting GWM display will be [xy]size * zoomfactor
#        screen pixels.  Note that since zoomfactor depends on the
#        pixelsize variable as well as the zoomlevel variable,
#        pixelsize must have a sensible value in terms of the chosen
#        view coordinate system in order to give you a sensibly-sized
#        GWM widget.
#
#        Only one GWM display is allowed per Gwmview widget.  If one
#        already exists, using this method will replace it with a new one.
#        The name of the GWM and its corresponding device should be got
#        using the gwmname or devname methods respectively.
#
#     maxcanvas
#        Returns a number a bit bigger than the larger of the width and
#        the height of the most recently created GWM item.  The intention
#        is that this can be used to limit the size of canvas of future
#        canvases created to be not too much bigger than the last one,
#        since this is likely to avoid trouble for the user when switching
#        between display of a small and a much larger image.
#        Note this returns a value in screen (canvas) coordinates, not
#        view coordinates.
#
#     removegwm
#        Removes any existing GWM display from the widget.  It is
#        not an error to call this method if there is no extant GWM
#        display.
#
#     unbindall
#        Removes all bindings to items on the canvas on which the GWM item
#        resides.
#
#     view2canv vx vy
#        Converts viewer coordinates to canvas coordinates.  Returns a
#        two-element list giving canvas coordinates corresponding to
#        the arguments.
#           - vx       -- X coordinate in Gwmview viewer coordinates
#           - vy       -- Y coordinate in Gwmview viewer coordinates
#
#     Gwmview also inherits all the public methods of Ccdtop.
#
#     Public variables (configuration options):
#
#     lutable = list
#        This is a one or two element list giving the configuration of
#        the colour lookup table.  The first element gives the name
#        of the table (COLTAB parameter of KAPPA's LUTABLE task) and
#        the second element, if present, gives the map type (MAPPING
#        parameter of LUTABLE) - if absent the value LINEAR is used.
#
#     pixelsize = real
#        This variable is provided for convenience, to modify the way
#        the zoomfactor variable is accessed.  It should be set to
#        the (approximate) linear size of a pixel (e.g. an NDF pixel)
#        in view coordinates.  This variable controls the actual size
#        in screen pixels of the GWM created by the makegwm method.
#
#     zoom = real
#        A factor giving the number of screen pixels to an NDF pixel.
#
#     zoomfactor = real
#        A factor converting from view coordinates to Gwm coordinates
#        (differs from zoom by a factor of pixelsize).
#
#     Gwmview also inherits all the public variables of Ccdtop.

#  Copyright:
#     Copyright (C) 2000-2001 Central Laboratory of the Research
#     Councils. All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     11-OCT-2000 (MBT):
#        Original version.
#     19-JUL-2001 (MBT):
#        Removed all knowledge of points from this file - now handled
#        at a higher level.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Inheritance.
      inherit Ccdtop


########################################################################
#  Constructor.
########################################################################
      constructor { args } {

#  Add control groups to the control panel.
         addgroup zoom Zoom
         addgroup action Control

#  Construct control widgets.
         set panel [ panel ]
         itk_component add zoom {
            zoomcontrol $panel.zoom \
               -min 0.05 \
               -max 12 \
               -value 1 \
               -valuevar zoom
         }
         itk_component add exit {
            buttoncontrol $panel.exit \
               -text "Done" \
               -cmd [ code $this deactivate ] \
               -balloonstr "Finish with this window"
         }
         itk_component add help {
            helpcontrol $panel.help
         } {
            usual
            keep -helptext
         }

#  Add control widgets to the control panel.
         addcontrol $itk_component(zoom) zoom
         addcontrol $itk_component(help) action
         addcontrol $itk_component(exit) action

#  Add the frame to hold the actual GWM viewing window.
         set interior [ Ccdtop::childsite ]
         itk_component add viewarea {
            frame $interior.viewarea -relief groove -borderwidth 2
         }
         itk_component add canvas {
            iwidgets::scrolledcanvas $itk_component(viewarea).canvas
         } {
            usual
            rename -textbackground \
                   -canvasbackground canvasBackground Background
         }
         pack $itk_component(canvas) -fill both -expand 1
         set canvas $itk_component(canvas)

#  Add info panel.
         itk_component add info {
            label $interior.info -relief groove
         }

#  Add childsite panel.
         itk_component add gwmchildsite {
            frame $interior.gwmchildsite
         }

#  Pack widgets into the hull.
         pack $itk_component(gwmchildsite) -side bottom -fill x -expand 0
         pack $itk_component(info) -side bottom -fill x -expand 0
         pack $itk_component(viewarea) -side bottom -fill both -expand 1

#  Do requested configuration.
         eval itk_initialize $args
      }


########################################################################
#  Public methods.
########################################################################

#-----------------------------------------------------------------------
      public method makegwm { xorigin yorigin xsize ysize { name "" } } {
#-----------------------------------------------------------------------
#  Keep a record of the size of the canvas.
         set gwmdims [ list $xsize $ysize ]
         set gwmorigin [ list $xorigin $yorigin ]

#  Clear out any GWM items which already exist on the canvas.
         removegwm

#  Get the GWM name.
         if { $name == "" } {
            set name [ winfo id $itk_interior ]
         }
         set gwmname $name

#  There seems to be a bug in the GWM canvas item and/or its PGPLOT driver.
#  It means that it is effectively only possible to plot correctly on a
#  GWM canvas item if its creation coordinates, and its current coordinates,
#  are {0 0}.  Thus this method always puts the GWM item at the origin,
#  but returns the distance away from its requested coordinates that it
#  is.  The calling application can then do a $canvas move gwmitem to
#  move it after it has performed whatever PGPLOT operations it needs to.
#  If this bug gets fixed, then the 'set corigin $properorigin' line below
#  could get uncommented.

#  Get the position of the origin in canvas coordinates.
         set corigin [ view2canv $xorigin [ expr $yorigin + $ysize ] ]

#  Create the GWM item itself.
         $canvas create gwm \
            [ lindex $corigin 0 ] [ lindex $corigin 1 ] \
            -name $name \
            -width [ expr $xsize * $zoomfactor ] \
            -height [ expr $ysize * $zoomfactor ] \
            -tags gwmitem

#  Set the lookup table.
         configure -lutable $lutable
      }


#-----------------------------------------------------------------------
      public method view2canv { vx vy } {
#-----------------------------------------------------------------------
         set cx [ expr $vx * $zoomfactor ]
         set cy [ expr - $vy * $zoomfactor ]
         return [ list $cx $cy ]
      }


#-----------------------------------------------------------------------
      public method canv2view { cx cy } {
#-----------------------------------------------------------------------
         set vx [ expr $cx * ( 1.0 / $zoomfactor ) ]
         set vy [ expr - $cy * ( 1.0 / $zoomfactor ) ]
         return [ list $vx $vy ]
      }


#-----------------------------------------------------------------------
      public method unbindall {} {
#-----------------------------------------------------------------------
#  Unset all canvas bindings.  Because of the way that bind works
#  (a binding is to a tag, not an item) we can't just use 'all' for the
#  tabs to remove the bindings (though maybe there is some easier way?).
         catch { unset taghash }
         foreach item [ $canvas find all ] {
            foreach tag [ $canvas gettags $item ] {
               set taghash($tag) 1
            }
         }
         foreach tag [ array names taghash ] {
            foreach sequence [ $canvas bind $tag ] {
               $canvas bind $tag $sequence {}
            }
         }
      }


#-----------------------------------------------------------------------
      public method removegwm {} {
#-----------------------------------------------------------------------
         $canvas delete gwmitem
         set gwmname ""
      }


#-----------------------------------------------------------------------
      public method activate {} {
#-----------------------------------------------------------------------
         configure -status "active"
      }


#-----------------------------------------------------------------------
      public method deactivate {} {
#-----------------------------------------------------------------------
         configure -status "inactive"
      }


#-----------------------------------------------------------------------
      public method display {} {
#-----------------------------------------------------------------------
      }


#-----------------------------------------------------------------------
      public method gwmname {} {
#-----------------------------------------------------------------------
         return $gwmname
      }


#-----------------------------------------------------------------------
      public method devname {} {
#-----------------------------------------------------------------------
         return "xw;$gwmname"
      }


#-----------------------------------------------------------------------
      public method canvas {} {
#-----------------------------------------------------------------------
         return $canvas
      }


#-----------------------------------------------------------------------
      public method maxcanvas {} {
#-----------------------------------------------------------------------
         set dim [ expr $zoomfactor * [ max [ lindex $gwmdims 0 ] \
                                            [ lindex $gwmdims 1 ] ] ]
         return [ expr $dim * 1.4 ]
      }


#-----------------------------------------------------------------------
      public method zoominc { factor inc } {
#-----------------------------------------------------------------------
         return [ $itk_component(zoom) zoominc $factor $inc ]
      }


#-----------------------------------------------------------------------
      public method childsite { } {
#-----------------------------------------------------------------------
         return $itk_component(gwmchildsite)
      }


########################################################################
#  Private methods.
########################################################################


########################################################################
#  Public variables.
########################################################################


#-----------------------------------------------------------------------
      public variable pixelsize 1 {
#-----------------------------------------------------------------------
         configure -zoomfactor [ expr $zoom * ( 1.0 / $pixelsize ) ]
      }


#-----------------------------------------------------------------------
      public variable zoomfactor 1 {
#-----------------------------------------------------------------------
         display
      }


#-----------------------------------------------------------------------
      public variable zoom { 1 } {
#-----------------------------------------------------------------------
         configure -zoomfactor [ expr $zoom * ( 1.0 / $pixelsize ) ]
      }


#-----------------------------------------------------------------------
      public variable status inactive {
#-----------------------------------------------------------------------
         if { $status == "active" } {
            display
         }
      }


#-----------------------------------------------------------------------
      public variable lutable { grey } {
#-----------------------------------------------------------------------
         set table [ lindex $lutable 0 ]
         if { [ llength $lutable ] > 1 } {
            set maptype [ lindex $lutable 1 ]
         } else {
            set maptype "linear"
         }
         set device [ devname ]
         if { $status == "active" } {
            # taskrun lutable \
            #    "coltab=$table mapping=$maptype device=$device reset"
         }
      }


########################################################################
#  Private variables.
########################################################################

#  Instance Variables.
      private variable canvas          ;# Name of the canvas widget
      private variable gwmdims {0 0}   ;# Dimensions of the GWM item
      private variable gwmorigin {0 0} ;# View coordinate origin of GWM item
      private variable gwmname ""      ;# Name of the GWM item

   }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Gwmview {
      keep -background -cursor -foreground -canvasbackground
   }
   option add *Gwmview.canvasBackground black widgetDefault


########################################################################
#  Constructor alias
########################################################################

   proc gwmview { pathname args } {
      uplevel Gwmview $pathname $args
   }

# $Id$
