#+
#  Name:
#     MultiGraphPrint

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Print multiple BLT graphs in one pass.

#  Description:
#     This class define an object that will print the contents of a
#     series of BLT graphs widgets to either a printer or file.

#  Invocations:
#
#        MultiGraphPrint object_name [configuration options]
#
#     This creates an instance of a MultiGraphPrint object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Configuration options:

#  Methods:

#  Inheritance:
#     util::CanvasPrint

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     11-JUL-2000 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual MultiGraphPrint {}

itcl::class gaia::MultiGraphPrint {

   #  Inheritances:
   #  -------------
   inherit util::CanvasPrint

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Print the content of all graphs as postscript to the given file
   #  descriptor
   public method print {fd} {
      global ::$w_.color ::$w_.rotate ::$w_.colormap ::$w_.fit_to_page
      if { [set $w_.fit_to_page] } { 
         set fit yes
      } else {
         set fit no
      }
      foreach graph $itk_option(-graphs) {

         $graph postscript configure \
            -colormode [set $w_.color] \
            -paperwidth [$w_.pagesize.width get] \
            -paperheight [$w_.pagesize.height get] \
            -landscape [set $w_.rotate] \
            -padx 0 \
            -pady 0 \
            -center 1 \
            -decorations 0 \
            -maxpect $fit
         
         set cmd [list $graph postscript output]
         puts $fd [eval $cmd]
      }
   }

   #  Use PrintDialog::ok to get the file descriptor (CanvasPrint::ok
   #  returns filename)
   public method ok {args} {
      PrintDialog::ok $args
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #   Graphs to print
   itk_option define -graphs graphs Graphs {}

   #  Protected variables: (available to instance)
   #  --------------------


   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
