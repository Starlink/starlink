include <syserr.h>
include <imhdr.h>
include "imfort.h"
  
define	IDB_RECLEN	80
  
# NHIST.X -- Returns number of history lines in user area of IRAF image
  
procedure nhist (im, nlin, nchars )

#  Name:
#     nhist

#  Purpose:
#     Returns number of history lines in an IRAF image

#  Language:
#     SPP

#  Invocation:
#     call nhist( im, nlin, nchars )

#  Description:
#     The history area of an IRAF image can be up to 511 characters
#     long, individual history items within this area are seperated by
#     newlines. If the History are gets full, then later entries are
#     truncated or lost. This SPP procedure searches through the
#     HISTORY area counting newlines, then the number of empty strings
#     is subtracted from this number to give the true number of history
#     items.
#     See the comments in the code for more details.

#  Arguments:
#     IM = INTEGER (Given)
#        The IRAF image descriptor required to access image.
#     NLIN = INTEGER (Given and Returned)
#        The number of lines in the image header.
#     NCHARS = INTERGER (Given and Returned)
#        The number of characters in the whole history string.

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
#        to a different program, nhist.x

#  Bugs:
#     {note_any_bugs_here}
#-     
  
pointer  im              # imfort image descriptor
int nlin                 # Number of history lines
int nchars               # Number of character in whole of history string

  
pointer	sp, rp, hist
#char str[SZ_IMHIST]      

#%      CHARACTER * (600) HISTCD


begin

# Set stack pointer
       call smark (sp)

# Initialise space for the string to contain the history information.
# SZ_IMHIST is defined in imhdr.h at present (Oct 92) it is 511.
#
# Get array to hold history string, pointed to by pointer `hist'.
       call salloc (hist, SZ_IMHIST, TY_CHAR)

# There follows some debugging code which may be useful again

# Copy the history string which is accessed via IM_HISTORY(im) into
# a string str declared to be same size as history area.
#       call amovc (IM_HISTORY(im), str, SZ_IMHIST)      

# Convert str to a FORTRAN77 string HISTCD.
#       call f77pak(str, histcd, 511 )
#%      write(*,*)'history are contains:',histcd

# Copy the history string which is accessed via IM_HISTORY(im) into
# the string pointed to by hist.
       call amovc (IM_HISTORY(im), memc[hist], SZ_IMHIST)      

# Search thorugh the History string counting the number of newlines along
# the way
       nlin = 0
       for (rp=hist;  Memc[rp] != EOS;  rp=rp+1)
          { if ( memc[rp] == '\n' ) nlin = nlin + 1 }

# Output the number of characters in the history area. rp is a pointer
# to the end of the string, and hist is the pointer to the start of the
# string, obtained by the call to salloc() earlier.
       nchars = rp - hist

# Release the stack pointer
       call sfree(sp)

end

