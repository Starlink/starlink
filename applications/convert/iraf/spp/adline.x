include <syserr.h>
include <imhdr.h>
include "imfort.h"
  
define	IDB_RECLEN	80
  
# IMCCRD -- Copy an existing FITS format header card to the user area
# Changed name to adline (RAHM)
  
procedure adline (im, card)

#  Name:
#     addline

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
#     {enter_new_authors_here}

#  History:
#     25-SEP-1992 (sjacoby):
#        Original version.
#     28-SEP-1992 (RAHM)
#        Added STARLINK style prologue and a few comments.

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
  
# Append the new record with an uninitialized value field.  Keyword
# value pairs are encoded in FITS format.
  
   do op = rp, rp + IDB_RECLEN             # blank fill card
      Memc[op] = ' '
  
      call amovc (Memc[cp], Memc[rp], IDB_RECLEN)
  
# Terminate the card.
      Memc[rp+IDB_RECLEN] = '\n'
      Memc[rp+IDB_RECLEN+1] = EOS
  
      IM_UPDATE(im) = YES
  
      call sfree (sp)
end



