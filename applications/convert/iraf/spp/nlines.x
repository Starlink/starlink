include <syserr.h>
include <imhdr.h>
include "imfort.h"
  
define	IDB_RECLEN	80
  
# NLINES.X -- Returns number of header lines in user area of IRAF image
  
procedure nlines (im, nlin, err)

#  Name:
#     nlines

#  Return number of 80 character header lines in an IRAF image

#  Language:
#     SPP

#  Invocation:
#     CALL ADLINE( IM, NLIN, ERR )

#  Description:
#     See the comments in the code.

#  Arguments:
#     IM = INTEGER (Given)
#        The IRAF image descriptor required to access image.
#     NLIN = INTEGER (Given)
#        The number of lines in the image header.
#     ERR = INTEGER (Given)
#        The imfort error indicator.

#  Authors:
#     Suzanne Jacoby (NOAO, Kitt Peak)
#     RAHM: Rhys Morris (STARLINK, University of Wales, Cardiff)
#     {enter_new_authors_here}

#  History:
#     25-SEP-1992 (sjacoby):
#        Original version of IMCCRD mailed to RAHM from sjacoby. IMCCRD
#        added header lines to an IRAF image.
#     14-OCT-1992 (RAHM)
#        Added STARLINK style prologue, a few comments and converted it
#        to a different program, nlines.x. This returns the number of
#        header lines (or cards) in an IRAF image. 

#  Bugs:
#     {note_any_bugs_here}
#-     
  
pointer  im              # imfort image descriptor
int nlin                 # The number of header lines present
int err                  # The imfort error indicator
  
pointer rp, ua

begin

# Get pointer to start of user area.
       ua = IM_USERAREA(im)

# Find end of the user area counting the number of newlines along
# the way
       nlin = 0 

       for (rp=ua;  Memc[rp] != EOS;  rp=rp+1)
          {
          if (Memc[rp] == '\n')
          nlin = nlin + 1
          }
  
end



