   itcl_class Ccd_toplevel {

#+
#  Name:
#     Ccd_toplevel

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines the class of CCDPACK top-level widgets.

#  Description:
#     This class creates a top-level widget with the standard CCDPACK
#     configuration. The positioning of new top-level widgets is set
#     relative to the position of the top left hand corner of the
#     toplevel widgets which are the parents. Thus parents of many
#     toplevel widgets have them displayed overlaid and gradually offset
#     down to the bottom right.
#
#     This class also provides a method "busy" which can be used to make
#     all toplevel widgets display a cursor, except possibly the current
#     (i.e. $oldthis) object.

#  Invocations:
#
#        Ccd_toplevel window [-option value]...
#
#     This command create an instance of a Ccd-toplevel object and
#     returns a command "window" for manipulating it via the methods
#     and configuration options described below. Configuration options
#     may be appended to the command.
#
#        window configure -configuration_options value
#
#     Applies any of the configuration options (after the widget
#     instance has been created).
#
#        window method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#
#        -CCDbitmap bitmap
#
#     This option configures the bitmap associated with the top-level
#     widget. This defaults to the CCDPACK standard bitmap. If another
#     bitmap is specified then it is essential that the full path name
#     is supplied.
#
#        -title "string"
#
#     A title for the window title bar.
#
#        -width value
#
#     Width of window.
#
#        -height value
#
#     Height of window.

#  Methods:
#     constructor [-option value]...
#        This method is invoked automatically by the class command and
#	 creates the "Ccd_toplevel" widget with a default configuration,
#	 except when overridden by command line options.
#     destructor
#        Destroys the "class" instance, invoked by the "delete" method.
#     configure [-option value]...
#        Activates the configuration options. If no configuration value
#	 is given then the current value of any known option is returned
#	 in a form similar (but not identical to) the Tk widget command.
#     busy option self
#	 This operates using the blt::busy command. The possible options
#	 being "hold" and "forget". If "hold" is used then all
#	 top-level windows (created by this class) are put on hold,
#	 but, only if they are not already busy. If the current window
#	 is not to be made busy then self should be set false (0). When
#	 an object is destroyed (via the delete method), then any
#	 objects placed on hold by it are released using the "forget"
#	 option, this can also be invoked explicitly. So using these
#	 methods a series of top-level objects can be placed on hold in
#	 any order and they will then be released in the same order
#	 (any holds in effect before an invocation will not be affected
#	 by later "holds" and "forget"s unless it is invoked from the
#	 correct object -- the one that placed the hold).
#     sethelp document label
#        Sets the default help for all widgets within the scope of
#        the top-level window. Document is a name like sun139 and
#        label an xlabel within it (without the xref_ part).

#  Inheritance:
#     This class inherits Ccd_base and its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     28-MAR-1994 (PDRAPER):
#     	 Original version.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4. Commented out ::rename in destructor, no
#        longer needed. Toplevels now resize by default.
#     25-MAY-1995 (PDRAPER):
#        Added wm command to trap destruction by window manager.
#     13-JUN-1995 (PDRAPER):
#        Added -width and -height configuration parameters.
#     11-APR-1997 (PDRAPER)
#        Converted to itcl2.2. blt_busy now just called busy.
#     {enter_changes_here}

#-

#  Inheritances:
      inherit Ccd_base

#.

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construction creates a instance of the class and configures it with
#  the default and command-line options.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { config } {

#  Make a suggestion to the window manager about the positioning of
#  this window. Use a cascade approach, that slightly offsets to the
#  right and down from the position of the parent.
         set parent [ winfo parent $oldthis ]
         if { ! [ info exists xinc($parent) ] } {
            set xinc($parent) 0
            set yinc($parent) 0
         }
         incr xinc($parent) 25
         incr yinc($parent) 25
         set xr [expr [winfo rootx $parent] +$xinc($parent)]
         set yr [expr [winfo rooty $parent] +$yinc($parent)]
         wm geometry $oldthis +$xr+$yr

#  Check options database for values to override widget defaults. Look for more
#  specific option of having a class specified, if this fails try for less
#  specific class.
         set opt [ _getoption "CCDbitmap CCDBitmap CCDBitMap"]
         if { $opt != {} } { set CCDbitmap $opt }

#  Set default configurations.
         configure -CCDbitmap          $CCDbitmap
         configure -title              $title
         configure -width              $width
         configure -height             $height

#  Define sub-component widgets for configuration via the wconfig
#  method.
         set widgetnames($oldthis:toplevel) $oldthis

#  Remember all the names of top-level widgets.
         set twidgets($tcount) $oldthis
         incr tcount

#  Trap window manager based destruction.
         wm protocol $oldthis WM_DELETE_WINDOW "$oldthis kill $oldthis"

#  Re-establish the stacking order.
         configure -stacked $stacked
      }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Destructor "method delete". Deletes the object.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Destructor decrements the position indicator and removes any
#  cursors created by this window.
      destructor  {

#  Release any widgets made busy by this.
         busy forget 0
         if $exists {
            set parent [winfo parent $oldthis]
            incr xinc($parent) -25
            incr yinc($parent) -25
            if { $xinc($parent) < 0 } { set xinc($parent) 0 }
            if { $yinc($parent) < 0 } { set yinc($parent) 0 }
         }

#  And remove this widget from lists.
         if { $tcount > 0 } {
            for { set i [expr $tcount -1]} { $i > -1 } { incr i -1 } {
               set widget $twidgets($i)
               if { [winfo exists $widget] && "$widget" != "$oldthis" } {
                  ::bind Ccd_restack <Expose> "$widget _restack;break"
               } elseif { "$widget" == "$oldthis" } {
                  set twidgets($i) ""
                  if { $i != [expr $tcount -1] } {
                     set newcount 0
                     for { set j 0 } { $j < $tcount } { incr j } { 
                        if { $twidgets($j) != "" } { 
                           incr newcount
                           set twidgets($newcount) $twidgets($j)
                        } 
                     }
                     set tcount $newcount
                  } else { 
                     incr tcount -1
                  }
               }
            }
         } else {
            set tcount 0
         }
      }
      
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#  Method to set all toplevel widgets busy, except possibly the current one.
#  Windows which are already busy are not modified, unless the request
#  is from the window that made the window busy.
      method busy { option self } {
         if { ! $self } {
            set also $oldthis
	 } else {
            set also {}
	 }
         switch $option {
            hold {
               for { set i 0 } { $i < $tcount } { incr i } {
                  set widget $twidgets($i)
                  if { [ winfo exists $widget ] } {
                     if { $widget != $also } {

#  If the widget is already busy then do nothing.
                        if { ! [ info exists widgetbusy($widget) ] } {
                           ::busy hold $widget
                           set widgetbusy($widget) $oldthis
                        }
                     }
                  }
               }
	    }
	    forget {
               for { set i 0 } { $i < $tcount } { incr i } {
                  set widget $twidgets($i)
                  if { [ winfo exists $widget ] } {
                     if { $widget != $also } {

#  Only remove the busy flag if this is the correct window to do this.
                        if { [ info exists widgetbusy($widget) ] } {
                           if { $widgetbusy($widget) == $oldthis } {
                              ::busy forget $widget
                              unset widgetbusy($widget)
                           }
                        }
		     }
		  }
	       }
	    }
	    default { error "Unknown option \"$option\"" }
	 }
      }

#  Set the default help for the window.
      method sethelp { docname label } {
         if $exists {
            Ccd_base::sethelp $oldthis $docname $label
         }
      }

#  Change/establish the stacking order.
      method _restack {} {
         raise $twidgets([expr $tcount -1])
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Set the bitmap.

      public CCDbitmap ccdbitmap {
         if $exists {
            if { $CCDbitmap == "ccdbitmap" } {

#  Standard bitma, so use standard routine.
               CCDSetIconBitmap $oldthis
            } else {

#  Non-standard bitmap, assume path name is complete.
               wm iconbitmap $oldthis @$CCDbitmap
            }
	 }
      }

#  Set the title in top-level title bar.
      public title {} {
         if $exists {
            wm title $oldthis "$title"
         }
      }

      public width {} {
         if { $width != "" } {
            if $exists {
               $oldthis configure -width $width
            }
         }
      }

      public height {} {
         if { $height != "" } {
            if $exists {
               $oldthis configure -height $height
            }
         }
      }

      public stacked 1 { 
         if { $exists && $stacked } { 
            ::bindtags $oldthis "Ccd_restack Ccd_toplevel $oldthis all"
            ::bind Ccd_restack <Expose> "$oldthis _restack;break"
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Common and protected variables.  Common are visible to all instances
#  of this class, protected to just this instance (both are available
#  anywhere in the scope of this class and in derived classes).
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Increments in positions for next top-level window dependents.
      common xinc
      common yinc

#  Record all the names of any top-level widgets for use when
#  controlling their properties as a whole (i.e. for setting the busy
#  cursor). The structure of this is an array indexed by the current
#  count of widgets.
      common twidgets
      common tcount 0

#  Common array of the names of top-level widgets which made others
#  busy. This control is used to release the busy status of top-levels
#  when the top-level that made them busy is deleted.
      common widgetbusy

#  End of class defintion.
   }
# $Id$
