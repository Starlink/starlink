   proc CCDContainerFile { ndfname } {
#+
#  Name:
#     CCDContainerFile

#  Purpose:
#     Returns the filename associated with an image.

#  Description:
#     This routine returns the filename in which an NDF structure lives.
#     Often this will just be the name of the structure with '.sdf'
#     appended, but in the case of HDS container files it may be more
#     complicated.  It works because whenever the ndgexpand command
#     is used to look inside HDS container files, the calling code
#     updates the CCDndfcontainers global array with a record of
#     what NDFs were found in what container files.  If there is no
#     corresponding entry in CCDndfcontainers, this routine returns
#     the input value, since it is presumably the name of a non-HDS file.

#  Arguments:
#     ndfname = string (read)
#        The name of an NDF structure.

#  Return value:
#     The name of the container file in which the NDF structure lives.

#  Global variables:
#     CCDndfcontainers = array (read)
#        An array mapping every NDF structure so far encountered to its
#        HDS container file.

#  Authors:
#     MBT: Mark Taylor (STARLINK)

#  History:
#     15-JUN-2001 (MBT):
#        Original version.

#-

#  Global variables:
      global CCDndfcontainers

#  If we've got a record of this one, return the name of the HDS 
#  container file.
      if { [ array names CCDndfcontainers $ndfname ] == $ndfname } {
         return $CCDndfcontainers($ndfname)

#  Otherwise, return the input name.
      } else {
         return $ndfname
      }
   }
# $Id$
