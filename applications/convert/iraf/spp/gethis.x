include <syserr.h>
include <imhdr.h>
include "imfort.h"
  
define	IDB_RECLEN	80
  
# GETHIS.X -- Returns history lines in user area of IRAF image
  
procedure gethis (im, hlin, hislin )

#  Name:
#     gethis

#  Returns number of history lines in an IRAF image

#  Language:
#     SPP

#  Invocation:
#     CALL gethis( IM, hlin, hisLIN )

#  Description:
#     See the comments in the code.

#  Arguments:
#     IM = INTEGER (Given)
#        The IRAF image descriptor required to access image.
#     HLIN = INTEGER (Given)
#        The number of the history line in the history string.
#     HISLIN = CHARACTER*(80) (Returned)
#        The line to be returned to the calling routine

#  Authors:
#     Suzanne Jacoby (NOAO, Kitt Peak)
#     RAHM: Rhys Morris (STARLINK, University of Wales, Cardiff)
#     {enter_new_authors_here}

#  History:
#     25-SEP-1992 (sjacoby):
#        Original version of IMCCRD mailed to RAHM from sjacoby. IMCCRD
#        added header lines to an IRAF image.
#     25-OCT-1992 (RAHM)
#        Added STARLINK style prologue, a few comments and converted it
#        to a different program, gethis.x

#  Bugs:
#     {note_any_bugs_here}
#-     
  
pointer  im              # imfort image descriptor
int hlin                 # Number of history lines
%       character*(70) hislin

  
pointer	sp, rp, hist,start,endc
int n, len
char str[70]
begin

# Set stack pointer
        call smark (sp)

# Initialise space for the string to contain the history information.
# SZ_IMHIST is defined in imhdr.h at present (Oct 92) it is 511.
        call salloc (hist, SZ_IMHIST, TY_CHAR)

#  Initialise string pointed to by hist
      for (rp=hist;  Memc[rp] != EOS;  rp=rp+1)
         {
         Memc[rp] = ' '
         }

# Copy the history string which is accessed via IM_HISTORY(im) into
# the string pointed to by hist.
        call amovc (IM_HISTORY(im), memc[hist], SZ_IMHIST)

# Search thorugh the History string counting the number of newlines along
# the way
        start = hist
        n = 0
        for (rp=hist;  Memc[rp] != EOS;  rp=rp+1)
           {
           if ( memc[rp] == '\n' )
              {
              n = n + 1

              if (n == (hlin-1) ) start = rp+1

              if (n == hlin ) endc = rp
              }
           }

        len = endc - start

        call amovc (Memc[start], str, len)

# Convert to a FORTRAN77 string
        call f77pak(str, hislin, len)

#%       write(*,*)'history line is',hislin

# Release the stack pointer
       call sfree(sp)

end

