proc ccdInquireBoolean { prompt args } {

#+
#  Name:
#     ccdInquireBoolean

#  Type of Module:
#     Tcl procedure.

#  Purpose:
#     Inquires a logical value from the user.

#  Description:
#     This routine prompts the user for a logical value. The string appears
#     in the form "$prompt (y or n) > ". Logical replies are any
#     strings starting with the characters "Y","y","T","t" for true
#     and "N","n","F","f" for false as in the ADAM parameter system.

#  Arguments:
#     prompt = string (read)
#        The prompt string. The choice (y or n) is added to this.
#     args = string (read)
#        Optional value indicating the default. This should be "y" or "n". 
#        If unset the default is "n".

#  Return value:
#     ccdInquireBoolean = string
#        Set to "y" if the return is TRUE and "f" if false.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     21-SEP-1995 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#-

#.

#  Get a logical return y or n.

   set answer [ccdInquire "$prompt (y or n)"]
   switch -regexp $answer {
      {^[Yy].*|^[Tt].*} {
         set answer "y"
      }
      {^[Nn].*|^[Ff].*} {
         set answer "n"
      }
      default {
	 if { "$args" != "" } { 
	    set answer $args
         } else { 
            set answer "n"
	 }
      }
   }
   return $answer
}
# $Id$
