#+
#  Name:
#     GaiaAstTransform

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Transforms a set of RA/Dec positions from a specified celestial
#     coordinate system to that of a displayed rtdimage. This class is
#     intended for use in transforming input coordinates (say from
#     external catalogue or text files) to the same celestial coordinates
#     as the image.

#  Description:

#  Invocations:
#
#        GaiaAstTransform object_name [configuration options]
#
#     This creates an instance of a GaiaAstTransform object. The return is
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
#     This object inherits no other classes.

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     DD-MMM-YYYY (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

#itk::usual GaiaAstTransform {}

itcl::class <scope>::GaiaAstTransform {
   
   #  Inheritances:
   #  -------------
   
   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {
      
      #  Evaluate any options [incr Tk].
      #  eval itk_initialize $args
   }   

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Configuration options: (public variables)
   #  ----------------------
   
   #  Protected variables: (available to instance)
   #  --------------------


   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
