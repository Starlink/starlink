#+
#  Name:
#     GaiaImageSpectrum

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Extends the RtdImageSpectrum class to add the ability to 
#     save the spectrum to a file. 

#  Description:
#     This class extends RtdImageSpectrum so that the currently 
#     selected line across the line can be extracted and stored
#     as an image. It does this by adding an extra button 
#     to the lower toolbar which initiates a fileselection dialog. 
#     It then uses the foreign image command writeslice to extract the
#     line of data. 

#  Invocations:
#
#        GaiaImageSpectrum object_name [configuration options]
#
#     This creates an instance of a GaiaImageSpectrum object. 
#     The return is the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Methods:
#
#        save_as
#
#     This method creates a fileselection window to get a file name.
#     It then extracts the current line into an image.
#
#        make_buttons
#
#     This method overrides RtdImageSpectrum to create the buttons
#     at the bottom of the display. It adds the "save as..." option.

#  Notes:
#     Originally this routine used the FIGARO application SLICE, but 
#     has been changed to use an internal routine instead. This is
#     because SLICE cannot deal with the situation when axes are
#     monotonically decreasing (which is pretty fundermental
#     when dealing with celestial coordinate systems). SLICE also
#     always insists on using axes values, which hardly ever useful
#     for us.

#  Inheritance:
#     This class inherits the abilities of RtdImageSpectrum.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  Copyright:
#     Copyright (C) 1997 Central Laboratory of the Research Councils

#  History:
#     16-JUL-1997 (PDRAPER):
#        Original version.
#     26-NOV-1997 (PDRAPER):
#        Modified to use foreign command to write slice.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaImageSpectrum {}

class gaia::GaiaImageSpectrum {
   inherit rtd::RtdImageSpectrum

   #  Constructor: create a new instance of this class. Note we need
   #  to force the base constructor to be invoked first
   constructor {args} { 
      eval RtdImageSpectrum::constructor $args
   } {
      eval itk_initialize $args
   }
 
   #  Destructor - clean up when deleted.
   destructor {
   }
   
   #  Create an NDF or text file with the slice contents.
   method save_as {} {   

      #  Get the coordinates of the line endpoints.
      lassign [$canvas_ coords $itk_option(-line_id)] x0 y0 x1 y1

      #  Replot the distribution of pixel values to get the number 
      #  of pixels we want.
      set numValues [$image_ spectrum $graph_ elem $x0 $y0 $x1 $y1 canvas \
                        $graph_.xVector $graph_.yVector]

      #  Get the pixel indices of the end-points.
      $image_ convert coords $x0 $y0 canvas xs ys image
      $image_ convert coords $x1 $y1 canvas xe ye image

      #  Get the name of the current image (.sdf is special and needs
      #  to be stripped).
      set image [$image_ cget -file]
      if { $image != "" } {
         lassign [fileName $image] image slice
         if { [file extension $image] == ".sdf" } {
            set image "[file rootname $image]${slice}"
         }
         
         #  Get a name for the slice.
         set slice [filename_dialog "." "*.sdf" $w_]
         if { [file extension $slice] == ".sdf" } {
            set slice [file rootname $slice]
         }

         #  And create it.
         busy {
            $image_ foreign writeslice \
               "-file $slice -line $xs $ys $xe $ye -nelem $numValues -name $image"
         }
      } else {
         error_dialog "No image is displayed"
      }
   }
   private method unlock_ {} {
      blt::busy release $w_
   }
   
   
   #  Make the button frame at the bottom of the window.
   method make_buttons {} {
      pack [set b [frame $w_.buttons -borderwidth 2 -relief groove]] \
         -side bottom -fill x
      pack \
         [button $b.print -text "Print..." -command [code $this print]] \
         [button $b.save -text "Save as..." -command [code $this save_as]] \
         [button $b.close -text "Close" -command [code $this quit]] \
         -side left -expand 1 -padx 2m -pady 2m
   }

   #  Protected variables.
   protected variable figslice_ {}
   protected variable pixel2axis_ {}
}
