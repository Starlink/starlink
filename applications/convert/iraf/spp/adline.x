include <syserr.h>
include <imhdr.h>
include "imfort.h"
  
define	IDB_RECLEN	80
  
# IMCCRD -- Copy an existing FITS format header card to the user area
# Changed name to adline (RAHM)
  
procedure adline (im, card)

#  Name:
#     adline

#  Adds a 80 character line to an IRAF image.

#  Language:
#     SPP

#  Invocation:
#     CALL ADLINE( IM, CARD )

#  Description:
#     See the comments in the code.

#  Arguments:
#     IM = INTEGER (iven)
#        The IRAF image descriptor of the file to which the card is to
#        be written.
#     CARD = CHARACTER * ( 80 ) (Given)
#        The FITS card to be written to the IRAF image.

#  Authors:
#     Suzanne Jacoby (NOAO, Kitt Peak)
#     RAHM: Rhys Morris (STARLINK, University of Wales, Cardiff)
#     MJC: Malcolm J. Currie (STARLINK)
#     {enter_new_authors_here}

#  History:
#     25-SEP-1992 (sjacoby):
#        Original version.
#     28-SEP-1992 (RAHM):
#        Added STARLINK style prologue and a few comments.
#     1997 August 19 (MJC):
#        Revised to pad strings to eighty characters from code provided
#        by Frank Valdes (IRAF).

#  Bugs:
#     {note_any_bugs_here}
#-     
  
pointer  im              # imfort image descriptor
%	character*(*) card
  
pointer	sp, cp, rp, ua, op
int	max_lenuserarea
  
begin

#  Set stack pointer
   call smark (sp)

#  Reserve some stack memory
   call salloc (cp, SZ_LINE, TY_CHAR)
  
#  Convert the FORTRAN string into a SPP string
   call f77upk (card, Memc[cp], SZ_LINE)
  
# Open the user area string for appending.  If the user area is not
# empty the last character must be the newline record delimiter,
# else the new record we add will be invalid.

   max_lenuserarea = (LEN_IMDES + IM_LENHDRMEM(im) - IMU + 1) * SZ_STRUCT
   ua = IM_USERAREA(im)
  
   for (rp=ua;  Memc[rp] != EOS;  rp=rp+1)
              ;
   if (rp - ua + IDB_RECLEN + 1 >= max_lenuserarea)
   call syserrs (SYS_IDBOVFL, card)
  
   if (rp > ua && Memc[rp-1] != '\n') {
              Memc[rp] = '\n'
              rp = rp + 1
          }

# Append the supplied card.
    for (op = rp; op <= rp + IDB_RECLEN; op = op + 1) {
        if (Memc[cp] == EOS || Memc[cp] == '\n')
            break
        Memc[op] = Memc[cp]
        cp = cp + 1
    }

# Pad with blanks, if necessary.
    for (; op <= rp+IDB_RECLEN; op = op + 1)
        Memc[op] = ' '

# Terminate the card.
    Memc[op] = '\n'
    Memc[op+1] = EOS

    IM_UPDATE(im) = YES
  
    call sfree (sp)
end

