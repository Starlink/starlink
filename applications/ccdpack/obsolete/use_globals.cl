procedure use_globals( action )

#+
#  Name:
#     use_globals

#  Purpose:
#     Controls the use of global parameters in IRAF CCDPACK.

#  Language:
#     IRAF CL

#  Type of Module:
#     CL procedure.

#  Description:
#     This procedure controls the use of global parameters by CCDPACK
#     when being used from IRAF CL. If it is given an single argument
#     "yes" then all CCDPACK commands will honour the values set by 
#     CCDSETUP under all occasions except when a parameter value is
#     specified on the command-line. If the value "no" is given then
#     any values set by the CCDSETUP command will be ignored and all
#     parameters which where globally associated will behave like 
#     any other parameter (i.e. you can set them using eparam and they
#     may prompt for a value).
#
#     You must issue this command before running other CCDPACK
#     commands as it is used only when a task is first activitated. To
#     change the existing behaviour leave CCDPACK, then reinitialise
#     and finally issue the use_globals command.

#  Usage:
#     use_globals [action]

#  ADAM Parameters:
#     action = boolean
#        If set to "yes" then CCDPACK commands will honour any 
#        global associations.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     21-JUN-1997 (PDRAPER):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Parameters:
bool action {yes,prompt="Use CCDPACK global variables"}

#.
begin
   bool laction
   laction = action
   reset CCDPACK_GLOBALS=(laction)
   print (" ")
   if ( laction ) {
      print ("Global variables will now be used. Use the commands:")
      print (" ")
      print ("   CCDSETUP, CCDSHOW and CCDCLEAR.")
      print (" ")
      print ("to set, examine and clear them.")
   } else {
      print ("Global variables will now be ignored. The commands:")
      print (" ")
      print ("   CCDSETUP, CCDSHOW and CCDCLEAR")
      print (" ")
      print ("are now invalid.")
   }
   print (" ")
   print ("Remember to restart any CCDPACK tasks already running.")
   print (" ")
   keep
end
