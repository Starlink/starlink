proc CCDFileToNDFName { filename } {
#+
#  Name:
#     CCDFileToNDFName 

#  Purpose:
#     Changes a filename into an NDF name or foreign data image.

#  Type of Module:
#     Tcl/Tk

#  Description:
#     This routine removes any ".sdf" extensions from filenames
#     and complains if the file extension isn't one of the known
#     formats (if xreduce is setup for foreign data access).

#  Return
#     CCDFileToNDFName = filename (write)
#        The image name.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     3-MAR-1994 (PDRAPER):
#     	 Original version.
#     16-APR-1997 (PDRAPER):
#        Modified to check against known file extensions other 
#        than ".sdf". This is to support foreign formats.
#     {enter_changes_here}

#-

   global CCDimagefilters

#.

#  Check filename for .sdf extension. If it exists then strip it off.
   set nameext [file extension $filename]
   if { $nameext != "" && $nameext != ".sdf" } {
      
#  The filename cannot be that of an NDF, so check if it is otherwise
#  known as an image.
      if { $CCDimagefilters != "" } { 
         if { [string first $nameext $CCDimagefilters] == 0 } { 
            CCDIssueInfo "$filename is not a recognised image"
            set NDFname {}
         } else {

#  Almost certainly a recognised foreign file type so just use it.
            set NDFname $filename
         }
      }
   } else {
      
#  It's an NDF so just get the rootname of the file.
      set NDFname [file rootname $filename]
   }
   
#  End of procedure
   return $NDFname
}
# $Id$
