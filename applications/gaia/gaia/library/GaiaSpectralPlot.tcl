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
#     of an NDF. The section can also be displayed, in a small format, on
#     another canvas, and have a reference position displayed (usually the
#     coordinate of an image slice being displayed. 

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

      #  When window is closed, we want to withdraw all graphics
      #  interactions.
      wm protocol $w_ WM_DELETE_WINDOW [code $this close]

      #  Add an options menu for setting options that should probably
      #  be defined once only per-session, or infrequently.
      add_menubar
      set File [add_menubutton "File" left]
      configure_menubutton File -underline 0
      set Options [add_menubutton "Options" left]
      configure_menubutton Options -underline 0

      #  Add window help.
      add_help_button patchusage "On Window..."

      #  Set the exit menu item.
      $File add command -label Exit -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]
      $short_help_win_ add_menu_short_help $File \
         {Exit} {Close this window}

      #  Options are whether to show the secondary plot or not. Also
      #  requires an extra canvas.
      set show_plot2_ 0
      $Options add checkbutton \
         -label {Show thumbnail plot} \
         -variable [scope show_plot2_] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this toggle_show_plot2_]
      add_menu_short_help $Options {Show thumbnail plot}  \
         {Show a thumbnail copy of plot in main window}

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
      reset
   }

   #  Methods:
   #  --------

   #  Close down.
   public method close {} {
      reset
      wm withdraw $w_
      if { $itk_option(-close_cmd) != {} } {
         eval $itk_option(-close_cmd)
      }
   }

   #  Reopen
   public method open {} {
      wm deiconify $w_
      if { $itk_option(-open_cmd) != {} } {
         eval $itk_option(-open_cmd)
      }
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
   public method display {ndfname axis autoscale {x {}} {y {}}} {

      #  Open the NDF and map its data.
      set ndfid [ndf::open "$ndfname"]
      lassign [ndf::map $ndfid] adr nel type

      #  Create the main spectral_plot.
      if { $spectrum_ == {} } {
         set autoscale 1
         set spectrum_ [$itk_component(canvas) create spectral_plot \
                           pointer $adr $nel $type \
                           -x 25 -y 5 -width 650 -height 200 \
                           -linecolour blue -linewidth 1 \
                           -gridoptions "Grid=0,DrawTitle=0" \
                           -showaxes 1]
         make_ref_line_
         set_to_ref_coord_
      }

      #  Create the secondary plot, put this at the given x and y.
      if { $itk_option(-canvas) != {} && $spectrum2_ == {} && $show_plot2_ } {
         set autoscale 1
         set spectrum2_ [$itk_option(-canvas) create spectral_plot \
                            pointer $adr $nel $type \
                            -x $x -y $y -width 200 -height 200 \
                            -linecolour blue -linewidth 1 \
                            -showaxes 0 \
                            -tags $itk_option(-ast_tag) \
                            -fixedscale 1]
      }

      #  When autoscaling (or just created one of the plots), set the frameset
      #  and the NDF data units.
      if { $autoscale } {
         set frameset [ndf::getwcs $ndfid $axis]
         $itk_component(canvas) itemconfigure $spectrum_ -frameset $frameset
         if { $spectrum2_ != {} } {
            $itk_option(-canvas) itemconfigure $spectrum2_ -frameset $frameset
         }
         $itk_component(canvas) itemconfigure $spectrum_ \
            -dataunits "[ndf::getc $ndfid units]" \
            -datalabel "[ndf::getc $ndfid label]"
      }

      #  Pass in the data.
      $itk_component(canvas) coords $spectrum_ pointer $adr $nel $type
      if { $spectrum2_ != {} } {
         $itk_option(-canvas) coords $spectrum2_ pointer $adr $nel $type

         #  Translate the secondary plot.
         if { $x != {} && $y != {} } {
            set dx [expr $x - [$itk_option(-canvas) itemcget $spectrum2_ -x]]
            set dy [expr $y - [$itk_option(-canvas) itemcget $spectrum2_ -y]]
            $itk_option(-canvas) move $spectrum2_ $dx $dy
         }
      }

      #  Finished with the NDF.
      ndf::close $ndfid
   }

   #  Make the reference line item. This should be refreshed to the reference
   #  coordinate as necessary.
   protected method make_ref_line_ {} {
      if { $itk_option(-show_ref_line) } {
         lassign [$itk_component(canvas) bbox $spectrum_] x0 y0 x1 y1
         if { $x0 != {} } { 
            set ref_line_ [$itk_component(canvas) create line $x0 $y0 $x0 $y1 \
                              -width 1 -fill red]
         }
      }
   }

   #  Set the reference line coordinate. Use a special coords that
   #  takes a world coordinate and returns a canvas coordinate. 
   public method set_ref_coord {xcoord} {
      if { $spectrum_ != {} && $ref_line_ != {} } {
         set xref_ [$itk_component(canvas) coords $spectrum_ convert $xcoord]
         set_to_ref_coord_
      }
   }

   #  Move reference line to the reference coordinate.
   public method set_to_ref_coord_ {} {
      if { $spectrum_ != {} && $ref_line_ != {} } {
         lassign [$itk_component(canvas) coords $ref_line_] x0 y0 x1 y1
         $itk_component(canvas) move $ref_line_ [expr $xref_ - $x0] 0
      }
   }

   #  Make the spectral_plot item scale to fit the full size of the canvas.
   public method fitxy { args } {
      $itk_component(canvas) scale $spectrum_ -1 -1 -1 -1

      # Resize the reference line.
      if { $ref_line_ != {} } {
         $itk_component(canvas) delete $ref_line_
         make_ref_line_
         set_to_ref_coord_
      }
      
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

   #  Handle a change in the show_plot2_ variable.
   protected method toggle_show_plot2_ {} {
      if { ! $show_plot2_ && $spectrum2_ != {} } {
         #  Make spectrum disappear.
         if { $itk_option(-canvas) != {} } {
            $itk_option(-canvas) delete $spectrum2_
         }
         set spectrum2_ {}
      }
   }


   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of RtdImage widget.
   itk_option define -rtdimage rtdimage Rtdimage {} {}

   #  The canvas, could be local or in the main image.
   itk_option define -canvas canvas Canvas {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Tag to use for any graphics. Matches the ast_tag value used in GAIA
   #  to avoid the main canvas scaling the plot.
   itk_option define -ast_tag ast_tag Ast_Tag ast_element

   #  Command to execute when the "open" method is invoked.
   itk_option define -open_cmd open_cmd Open_Cmd {}

   #  Command to execute when the "close" method is invoked.
   itk_option define -close_cmd close_cmd Close_Cmd {}

   #  Whether to show the coordinate ref line.
   itk_option define -show_ref_line show_ref_line \
      Show_Ref_Line 1

   #  Protected variables: (available to instance)
   #  --------------------

   #  The main spectral_plot item.
   protected variable spectrum_ {}

   #  Whether to show the secondary plot.
   protected variable show_plot2_ 0

   #  The secondary spectral_plot item.
   protected variable spectrum2_ {}

   #  The reference coordinate, units of the spectral axis.
   protected variable xref_ 0
   
   #  Reference line item.
   protected variable ref_line_ {}

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
