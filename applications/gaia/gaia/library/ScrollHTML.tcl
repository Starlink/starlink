#+
#  Name:
#     ScrollHTML

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     HTML rendering widget with scrollbars.

#  Description:
#     This class uses Richard Hipps TkHTML widget and adds scrollbars
#     to it. It also provides a default set of methods for rendering a
#     given page and provides a history mechanism for returning to
#     prior and later pages that have been visited.

#  Invocations:
#
#        ScrollHTML window [-option value]...
#
#     This command create an instance of a scrolltext and returns a
#     command "window" for manipulating it via the methods and
#     configuration options described below. Configuration options may
#     be appended to the command.
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
#     See itk_option definitions below


#  Methods:
#     See method definitions below.

#  Inheritance:
#     FrameWidget

#  Authors:
#     PWD: Peter W. Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     02-MAY-2001 (PDRAPER):
#     	 Original version.
#     {enter_further_changes_here}

#-

itk::usual ScrollHTML {}

itcl::class gaia::ScrollHTML {

   #  Inheritances:
   inherit util::FrameWidget

   #  Constructor.
   #  ------------
   constructor {args} {

      #  Create the label.
      itk_component add label {
         label $w_.label -text "$itk_option(-label)"
      }

      #  Create the scrollbars.
      itk_component add vscroll {
         scrollbar $w_.vscroll -orient vertical
      }

      #  Horizontal fits into frame that has a corner padding element.
      itk_component add hframe {
         frame $w_.hframe -borderwidth 0
      }
      set rwidth [winfo reqwidth $itk_component(hscroll)]

      itk_component add corner {
         frame $itk_component(hframe).corner -width $rwidth
      }

      itk_component add hscroll {
         scrollbar $itk_component(hframe).hscroll -orient horizontal
      }

      #  Create the html widget adding all the necessary callbacks for
      #  a simple rendering system.
      itk_component add html {
         ::html $w_.html \
            -yscrollcommand "$itk_component(vscroll) set" \
            -xscrollcommand "$itk_component(hscroll) set" \
            -formcommand [code $this formcommand_] \
            -imagecommand [code $this imagecommand_]\
            -scriptcommand [code $this scriptcommand_] \
            -appletcommand [code $this appletcommand_] \
            -hyperlinkcommand [code $this hyperlinkcommand_] \
            -underlinehyperlinks 1 \
            -tablerelief raised
      } {
         keep \
            -width -height -exportselection -foreground -background \
            -padx -pady -tablerelief -underlinehyperlinks
      }
      $itk_component(hscroll) configure -command "$itk_component(html) xview"
      $itk_component(vscroll) configure -command "$itk_component(html) yview"

      #  Now pack everything into place.
      pack $itk_component(label) -side top -fill x
      pack $itk_component(hframe) -side bottom -fill x
      pack $itk_component(corner) -side right -fill y
      pack $itk_component(hscroll) -side bottom -fill x
      pack $itk_component(vscroll) -side right  -fill y
      pack $itk_component(html) -expand true -fill both

      #  Do initialisations.
      eval itk_initialize $args
   }

   #  Methods.
   #  --------

   #  This method is called for each form element. These are just set
   #  to the not available image for types that require a window.
   protected method formcommand_ {n cmd args} {
      switch $cmd {
         select -
         textarea -
         input {
            set w [lindex $args 0]
            label $w -image [scope nogifsm]
         }
      }
   }

   #  This method is called for every <IMG> markup. The arguments are
   #  the value of the "src" parameter (in a resolved form), followed
   #  by the other parameters (width, height, everything else).
   protected method imagecommand_ {args} {

      #  Get the image source.
      set imgsrc [lindex $args 0]

      #  If we have this image already then re-use it.
      if { [info exists images_($imgsrc)] } {
         return $images_($imgsrc)
      }
      
      #  Otherwise create the image. Failures (i.e. not available)
      #  return a small gray image.
      if {[catch {image create photo -file $imgsrc} img]} {
         return smgray
      }
      set images_($imgsrc) $img
      return $img
   }

   #  This method is called for every <SCRIPT> markup. Does nothing.
   protected method scriptcommand_ {args} {
   }

   #  This method is called for every <APPLET> markup. Just shows a
   #  label with the applet name.
   protected method appletcommand_ {w arglist} {
      label $w -text "The Applet $w" -bd 2 -relief raised
   }

   #  This method is called when the user clicks on a hyperlink. There
   #  are really two cases to deal with here, when a new page is to be
   #  loaded and when we need to move to an anchor in the current page.
   protected method hyperlinkcommand_ { url } {
      
      #  If the file is same as last time look for an anchor.
      set pattern "$current_\#"
      set len [string length $pattern]
      incr len -1
      if { [string range $url 0 $len] == $pattern } {
         incr len
         $itk_component(html) yview [string range $new $len end]
      } else {
         loadfile_ $url
      }
   }
   

   #  Protected variables:
   #  --------------------

   #  Array of all the loaded images. The index if the file name.
   protected variable images_

   #  Name of the file that we're looking at.
   protected variable current_ {}

   #  Configuration options:
   #  ----------------------

   #  Set text of label.
   itk_option define -label scrolltextlabel ScrolltextLabel {} {
      if { [info exists itk_component(label)] } {
         $itk_component(label) configure -text "$itk_option(-label)"
      }
   }


   #  Shared images
   #  -------------
   #  These images are used in place of GIFs or of form elements
   common biggray {
      image create photo biggray -data {
         R0lGODdhPAA+APAAALi4uAAAACwAAAAAPAA+AAACQISPqcvt
         D6OctNqLs968+w+G4kiW5omm6sq27gvH8kzX9o3n+s73/g8M
         CofEovGITCqXzKbzCY1Kp9Sq9YrNFgsAO///
      }
   }
   common smgray {
      image create photo smgray -data {
         R0lGODdhOAAYAPAAALi4uAAAACwAAAAAOAAYAAACI4SPqcvtD
         6OctNqLs968+w+G4kiW5omm6sq27gvH8kzX9m0VADv/
      }
   }
   common nogifbig {
      image create photo nogifbig -data {
         R0lGODdhJAAkAPEAAACQkADQ0PgAAAAAACwAAAAAJAAkAAACm
         ISPqcsQD6OcdJqKM71PeK15AsSJH0iZY1CqqKSurfsGsex08X
         uTuU7L9HywHWZILAaVJssvgoREk5PolFo1XrHZ29IZ8oo0HKE
         YVDYbyc/jFhz2otvdcyZdF68qeKh2DZd3AtS0QWcDSDgWKJXY
         +MXS9qY4+JA2+Vho+YPpFzSjiTIEWslDQ1rDhPOY2sXVOgeb2
         kBbu1AAADv/
      }
   }
   common nogifsm {
      image create photo nogifsm -data {
         R0lGODdhEAAQAPEAAACQkADQ0PgAAAAAACwAAAAAEAAQAAACN
         ISPacHtD4IQz80QJ60as25d3idKZdR0IIOm2ta0Lhw/Lz2S1J
         qvK8ozbTKlEIVYceWSjwIAO///
      }
   }
}
