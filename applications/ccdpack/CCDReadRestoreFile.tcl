   proc CCDReadRestoreFile { file } {
#+
#  Name:
#     CCDReadRestoreFile

#  Purpose:
#     Restores CCDPACK global parameters from a text file.

#  Language:
#     Tcl/Tk

#  Description:
#     This routine reads in the values from the given file and decodes
#     the contents into values for known CCDPACK global parameters. The
#     contents of the file are as those allowed by the CCDSETUP
#     application, namely:
#
#       GLOBAL_PARNAME = value1[,value2][,values3] etc.
#
#     GLOBAL_PARNAME must be one of the parameters as used by CCDSETUP.
#     These are:
#        ADC
#	 BOUNDS
#        DEFERRED
#        DIRECTION
#        EXTENT
#        GENVAR
#        LOGFILE
#        LOGTO
#        MASK
#        NDFNAMES
#        PRESERVE
#        RNOISE
#        SATURATE
#        SATURATION
#        SETSAT
#
#     Lines may be continued using the character "-" at the end of the
#     line, comments may be present. These use the characters "#" and
#     "!".

#  Arguments:
#     file = string (read)
#        The name of the text file containing the commands used to set
#	 the global associations.

#  Returned Value:
#     No value is returned (see Global parameters).

#  Global parameters:
#     CCDglobalpars = array (write)
#        The resultant global parameters are set using the elements
#	 CCDglobalpars(GLOBAL_PARNAME). If a value already exists this is
#	 overwritten. Global parameters with no value are not set.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     22-FEB-1994 (PDRAPER):
#     	 Original version.
#     4-MAR-1994 (PDRAPER):
#     	 Now named CCDReadRestoreFile.
#     {enter_further_changes_here}

#-

#  Declare global parameters:
   global CCDglobalpars

#.

#  Check that file is readable.
   if [file readable $file]  {

#  Open the file.
      set fileid [open $file r]
      while { [set line [CCDReadTextLine $fileid] ] != "" } {

#  Have a line of information to decode. Look for the keyword
#  which we expect.
         if { [string match "*=*" $line ] } {
            switch -regexp $line {
               (^ADC|^adc)              { set keyword "ADC" }
               (^BOUNDS|^bounds)        { set keyword "BOUNDS" }
               (^DEFERRED|^deferred)    { set keyword "DEFERRED" }
               (^DIRECTION|^direction)  { set keyword "DIRECTION" }
               (^EXTENT|^extent)        { set keyword "EXTENT" }
               (^GENVAR|^genvar)        { set keyword "GENVAR" }
               (^LOGFILE|^logfile)      { set keyword "LOGFILE" }
               (^LOGTO|^logto)          { set keyword "LOGTO" }
               (^MASK|^mask)            { set keyword "MASK" }
               (^NDFNAMES|^ndfnames)    { set keyword "NDFNAMES" }
               (^PRESERVE|^preserve)    { set keyword "PRESERVE" }
               (^RNOISE|^rnoise)        { set keyword "RNOISE" }
               (^SATURATE|^saturate)    { set keyword "SATURATE" }
               (^SATURATION|saturation) { set keyword "SATURATION" }
               (^SETSAT|^setsat)        { set keyword "SETSAT" }
               default { CCDIssueInfo "Unrecognised keyword ($line)"
                         set keyword "default" }
            }

#  Decode statement. Need the trailing value (after the equals sign).
            set equalsat [string first "=" $line]
            incr equalsat
            set trailing [string range $line $equalsat end]
	    set trailing [string trim $trailing]
            set CCDglobalpars($keyword) $trailing

#  Couldn't match this line.
         } else {
            CCDIssueInfo "Unrecognisable expression: $line"
         }
      }

#  Close the file.
      close $fileid

#  Couldn't read the file.
   } else {
      CCDIssueInfo "Failed to open file $file (not readable)"
   }

#  End of procedure.
   }
# $Id$
