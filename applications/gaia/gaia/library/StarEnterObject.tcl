#+
#  Name:
#     StarEnterObject

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Extends the enter object class to pick out X and Y fields, rather
#     than RA and DEC.

#  Invocations:
#
#        StarEnterObject object_name [configuration options]
#
#     This creates an instance of a StarEnterObject object. The return is
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
#     None at this level.

#  Methods:
#
#        picked_object list
#
#     Method invoked when object is picked. This sets the X and Y
#     entry fields.

#  Inheritance:
#     EnterObject

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     19-DEC-1997 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#-

#.

class gaia::StarEnterObject {
   
   #  Inheritances:
   #  -------------
   
   inherit EnterObject

   #  Constructor:
   #  ------------
   constructor {args} {
      eval itk_initialize $args
   }   

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Override the picked_object method to pick out the X and Y fields
   #  instead of the RA and DEC ones.
   public method picked_object {list} {
      lassign $list x y ra dec equinox fwhmX fwhmY symetry object background
      set_entry x $x
      set_entry y $y
   }

   #  Configuration options: (public variables)
   #  ----------------------
   
   #  Protected variables: (available to instance)
   #  --------------------


   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
