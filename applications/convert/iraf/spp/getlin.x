include <syserr.h>
include <imhdr.h>
include "imfort.h"
  
define	IDB_RECLEN	80

# GETLIN -- Extract header lines from an IRAF image  
  
procedure getlin (im, cardno, card)

#  Name:
#     getlin

#  Extracts a 80 character line to an IRAF image.

#  Language:
#     SPP

#  Invocation:
#     CALL GETLIN( IM, CARDNO, CARD )

#  Description:
#     See the comments in the code.

#  Arguments:
#     IM = INTEGER (Given)
#        The IRAF image descriptor required to access the image.
#     CARD = CHARACTER * ( 80 ) (Returned)
#        The FITS card to be read form the IRAF image.
#     CARDNO = INTEGER (Given)

#  Authors:
#     Suzanne Jacoby (NOAO, Kitt Peak)
#     RAHM: Rhys Morris (STARLINK, University of Wales, Cardiff)
#     {enter_new_authors_here}

#  History:
#     25-SEP-1992 (sjacoby):
#        Original version called IMCCRD which added header lines to
#        an IRAF image.
#     28-SEP-1992 (RAHM)
#        Added STARLINK style prologue, a few comments and changed the
#        code to read a line from the header. The routine is to be used
#        in conjunction with NLINES.X, which returns the number of
#        header lines in an image.

#  Bugs:
#     {note_any_bugs_here}
#-     
  
pointer  im                       # imfort image descriptor
%	character*(*) card
int      cardno                   # the number of the card to be
                                  # returned

pointer	sp, cp, rp, ua, start, endc
int	 n, len
  
begin
      call smark (sp)
      call salloc (cp, SZ_LINE, TY_CHAR)
    
      ua = IM_USERAREA(im)

      start = ua
      n = 0

# Find the line numbered cardno, and get pointers to its begining and
# end.

      for (rp=ua;  Memc[rp] != EOS;  rp=rp+1)
         {
         if (Memc[rp] == '\n')
            {
            n=n+1

            if (n == (cardno-1) )
             start = rp+1

            if (n == cardno )
            endc = rp
            }
         }

# Find the length of the file.
        len = endc - start

#  Move only 80 characters, we do not want the '\n' to be copied. FITS standard
#  does not require a newline at the end of each line. Line termination is
#  taken care of as each array element is 80 chars long.

#  Initialise memc[cp]
      for (rp=cp;  Memc[rp] != EOS;  rp=rp+1)
         {
         Memc[rp] = ' '
         }

        call amovc (Memc[start], Memc[cp], len)

        call f77pak ( Memc[cp], card,  SZ_LINE)

        call sfree(cp)

        call sfree (sp)
end



