   proc CCDFixConvert { prefix } {
#+
#  Name:
#     CCDFixConvert

#  Purpose:
#     Set up temporary file names for on the fly NDF conversions.

#  Description:
#     This routine works around problems which occur when a data file
#     in a foreign file format is open from two processes at once.
#     In CCDPACK this can happen when a C/Fortran A-task spawns a
#     child process to run ccdwish.
#
#     When the first process accesses a foreign data file the NDF 
#     library may create a temporary HDS container file to hold it 
#     in native form.  If the second process accesses the same 
#     foreign file, NDF will overwrite the same HDS container
#     file with the same converted data.  When the second process
#     exits or releases the NDF, the temporary file will be deleted,
#     so that the first process can no longer find it.
#
#     This routine tinkers with environment variables so that the 
#     created HDS container file is given a different name in each
#     instance.  This means that the processes are working with
#     different native versions of the foreign data.  It is still
#     inefficient, in that it requires an extra conversion and an
#     extra copy simultaneously on disk, but it will avoid error
#     conditions.
#
#     See SSN/20 for documentation of the environment variables
#     accessed here.

#  Arguments:
#     prefix = string
#        A string to prepend to the normal name for a temporary HDS
#        container file to distinguish it from the one used prior 
#        to calling this routine.

#  Authors:
#     MBT: Mark Taylor (STARLINK)

#  History:
#     26-JUL-2001 (MBT):
#        Original version.

#-

#  Global variables.
      global env

#  Prepend a string to each of the temporary environment variable names
#  so that they are different from what they were before.  The prefix
#  must go after the last-appearing directory separator ("/") if one
#  exists, otherwise at the start of the string.
      foreach envar [ array names env NDF_TEMP_* ] {
         set ival $env($envar)
         set slashpos [ string last / $ival ]
         if { $slashpos > -1 } {
            set oval [ string range $ival 0 $slashpos ]
            set oval "$oval$prefix"
            set oval "$oval[ string range $ival [ expr $slashpos + 1 ] end ]"
         } else {
            set oval $prefix$ival
         }
         set env($envar) $oval
      }
   }
# $Id$
