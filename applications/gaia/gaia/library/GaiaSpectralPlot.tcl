#+
#  Name:
#     GaiaSpectralPlot

#  Type of Module:
#     [incr Tk] widget

#  Purpose:
#     Creates a window for controlling a spectral plot.

#  Description:
#     This class creates a top level window that displays and configures
#     a spectral_plot canvas item. The spectrum displayed is a 1D section
#     of an NDF.


#  Invocations:
#
#        GaiaSpectralPlot object_name [configuration options]
#
#     This creates an instance of a GaiaPhotom object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#     See itk_option definitions below.

#  Methods:
#     See declarations.

#  Inheritance:
#     TopLevelWidget.

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     06-MAR-2006 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaSpectralPlot {}

itcl::class gaia::GaiaSpectralPlot {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: Spectral plot ($itk_option(-number))"
      wm geometry $w_ 700x300

      #  Add an options menu for setting options that should probably
      #  be defined once only per-session, or infrequently.
      add_menubar
      set File [add_menubutton "File" left]
      configure_menubutton File -underline 0
      set Options [add_menubutton "Options" left]
      configure_menubutton Options -underline 0

      #  Add window help.
      add_help_button patchusage "On Window..."

      #  Add option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      $short_help_win_ add_menu_short_help $File \
         {New window} {Create a clone window}

      #  Set the exit menu item.
      $File add command -label Exit -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]

      #  Create the canvas.
      itk_component add canvas {
         canvas $w_.canvas
      }
      pack $itk_component(canvas) -fill both -expand 1
   }

   #  Destructor:
   #  -----------
   destructor  {

   }

   #  Methods:
   #  --------

   #  Create a new instance of this object.
   protected method clone_me_ {} {
      if { $itk_option(-clone_cmd) != {} } {
         eval $itk_option(-clone_cmd)
      }
   }

   #  Close down.
   method close {} {
      wm withdraw $w_
   }


   #  Display a spectrum, must be a 1D NDF, so usually a section. The axis
   #  defines the WCS axis that should be used for the plot X axis.
   #  If autoscale
   #  is true, then the plot should be rescaled so that the spectrum
   #  fits. Otherwise the existing plot bounds are used.
   public method display {ndfname axis autoscale} {

      #  Open the NDF and map its data.
      set ndfid [ndf::open "$ndfname"]
      lassign [ndf::map $ndfid] adr nel type

      if { $autoscale || $spectrum_ == {} } {
         if { $spectrum_ =={} } {
            
            #  Create the spectral_plot, only done once.
            set spectrum_ [$itk_component(canvas) create spectral_plot \
                              pointer $adr $nel $type \
                              -x 50 -y 25 -width 600 -height 200 \
                              -linecolour blue -linewidth 1 \
                              -gridoptions "Grid=0" \
                              -showaxes 1]
         }

         #  Set the frameset used by the plot. This also causes a autoscale.
         $itk_component(canvas) itemconfigure $spectrum_ \
            -frameset [ndf::getwcs $ndfid $axis]
         
         #  Also set the NDF data units.
         $itk_component(canvas) itemconfigure $spectrum_ \
            -dataunits "[ndf::getc $ndfid units]" \
            -datalabel "[ndf::getc $ndfid label]"
      }

      #  Pass in the data.
      $itk_component(canvas) coords $spectrum_ pointer $adr $nel $type

      #  Finished with the NDF.
      ndf::close $ndfid
   }

   #  Make the spectral_plot item scale to fit the full size of the canvas.
   public method fitxy {} {
      if { $spectrum_ != {} } {
         $itk_component(canvas) scale $spectrum_ -1 -1 -1 -1
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of rtdimage widget.
   itk_option define -rtdimage rtdimage Rtdimage {} {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  The spectral_plot item.
   protected variable spectrum_ {}

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
