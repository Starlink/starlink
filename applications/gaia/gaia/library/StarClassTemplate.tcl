#+
#  Name:
#     <StarClass>

#  Type of Module:
#     [incr Tcl] class

#  Purpose:

#  Description:

#  Invocations:
#
#        <StarClass> object_name [configuration options]
#
#     This creates an instance of a <StarClass> object. The return is
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
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     24-JUN-1996 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#-

#.

class <scope>::<StarClass> {
   
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
