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

      #  Create the canvas. Pack inside a frame so that we can get the resize
      #  events and the new geometry to get the apparent size of canvas right.
      itk_component add canvasframe {
         frame $w_.canvasframe -relief groove -bd 4
      }
      itk_component add canvas {
         canvas $itk_component(canvasframe).canvas
      }
      pack $itk_component(canvasframe) -fill both -expand 1
      pack $itk_component(canvas) -fill both -expand 1
      bind $itk_component(canvasframe) <Configure> [code $this resize %w %h]

      #  Make window resizes, resize the canvas.
      bind $w_ <Configure> +[code $this fitxy]
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
   public method close {} {
      reset
      wm withdraw $w_
   }

   #  Reset so that a new spectrum will be created.
   public method reset {} {
      $itk_component(canvas) delete $spectrum_
      if { $itk_option(-canvas) != {} } {
         $itk_option(-canvas) delete $spectrum2_
      }
      set spectrum_ {}
      set spectrum2_ {}
   }


   #  Display a spectrum, must be a 1D NDF, so usually a section. The axis
   #  defines the WCS axis that should be used for the plot X axis.
   #  If autoscale
   #  is true, then the plot should be rescaled so that the spectrum
   #  fits. Otherwise the existing plot bounds are used.
   public method display {ndfname axis autoscale {x 0} {y 0}} {

      #  Open the NDF and map its data.
      set ndfid [ndf::open "$ndfname"]
      lassign [ndf::map $ndfid] adr nel type

      if { $autoscale || $spectrum_ == {} } {
         if { $spectrum_ == {} } {

            #  Create the main spectral_plot, only done once.
            set spectrum_ [$itk_component(canvas) create spectral_plot \
                              pointer $adr $nel $type \
                              -x 25 -y 5 -width 650 -height 200 \
                              -linecolour blue -linewidth 1 \
                              -gridoptions "Grid=0,DrawTitle=0" \
                              -showaxes 1]
         }
         if { $itk_option(-canvas) != {} && $spectrum2_ == {} } {
            set spectrum2_ [$itk_option(-canvas) create spectral_plot \
                               pointer $adr $nel $type \
                               -x 0 -y 0 -width 200 -height 200 \
                               -linecolour blue -linewidth 1 \
                               -showaxes 0 -anchor "center" \
                               -tags $itk_option(-ast_tag) \
                               -fixedscale 1]
         }

         #  Set the frameset used by the plot. This also causes a autoscale.
         set frameset [ndf::getwcs $ndfid $axis]
         $itk_component(canvas) itemconfigure $spectrum_ -frameset $frameset
         if { $spectrum2_ != {} } {
            $itk_option(-canvas) itemconfigure $spectrum2_ -frameset $frameset
         }

         #  Also set the NDF data units.
         $itk_component(canvas) itemconfigure $spectrum_ \
            -dataunits "[ndf::getc $ndfid units]" \
            -datalabel "[ndf::getc $ndfid label]"
      }

      #  Pass in the data.
      $itk_component(canvas) coords $spectrum_ pointer $adr $nel $type
      if { $spectrum2_ != {} } {
         if { $x != 0 || $y != 0 } {
            #$itk_option(-canvas) itemconfigure $spectrum2_ -x $x -y $y
         }
         $itk_option(-canvas) coords $spectrum2_ pointer $adr $nel $type
      }

      #  Finished with the NDF.
      ndf::close $ndfid
   }

   #  Make the spectral_plot item scale to fit the full size of the canvas.
   public method fitxy { args } {
      $itk_component(canvas) scale $spectrum_ -1 -1 -1 -1

      # Fudge immediate update.
      $itk_component(canvas) itemconfigure $spectrum_ -showaxes 1
   }

   #  Set the anchor point for the spectrum.
   public method anchor {x y} {
      $itk_component(canvas) itemconfigure $spectrum_ -x $x -y $y
   }

   #  Resize the canvas to fit the size of enclosing frame.
   public method resize {cw ch} {
      set fh [expr [winfo height $itk_component(canvasframe)]-10]
      if { $fh < $ch } {
         $itk_component(canvas) configure -height $fh
      }
      set fw [expr [winfo width $itk_component(canvasframe)]-10]
      if { $fw < $cw } {
         $itk_component(canvas) configure -width $fw
      }
      $itk_component(canvas) configure -scrollregion "0 0 $fw $fh"
      fitxy
   }


   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of RtdImage widget.
   itk_option define -rtdimage rtdimage Rtdimage {} {}

   #  The canvas, could be local or in the main image.
   itk_option define -canvas canvas Canvas {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  Tag to use for any graphics. Matches the ast_tag value used in GAIA
   #  to avoid the main canvas scaling the plot.
   itk_option define -ast_tag ast_tag Ast_Tag ast_element

   #  Protected variables: (available to instance)
   #  --------------------

   #  The main spectral_plot item.
   protected variable spectrum_ {}

   #  The secondary spectral_plot item.
   protected variable spectrum2_ {}

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
