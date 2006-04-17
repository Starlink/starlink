      SUBROUTINE MSG1_PUTC( CVALUE, STRING, IPOSN, STATUS )
*+
*  Name:
*     MSG1_PUTC

*  Purpose:
*     Put a CHARACTER string into another at a given position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG1_PUTC( CVALUE, STRING, IPOSN, STATUS )

*  Description:
*     The string CVALUE (or as much of it as there is room for) is
*     copied into the part of STRING beginning at position IPOSN+1.
*     IPOSN is updated to indicate the end position of the copy of
*     CVALUE within STRING after this operation. If the resulting
*     string is truncated because STRING is too short, then the string
*     terminated with an ellipsis and STATUS is returned set to 
*     SAI__WARN. The sizes of CVALUE and STRING are based on the 
*     declared Fortran 77 size given by the intrinsic function LEN. 

*  Implementation Notes:
*     The coding of this routine assumes that the need to append an 
*     ellipsis is rare: i.e. it is less efficient when it has to
*     append an ellipsis.

*  Arguments:
*     CVALUE = CHARACTER * ( * ) (Given)
*        The string to be copied.
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string into which CVALUE is to be copied.
*     IPOSN = INTEGER (Given and Returned)
*        The position pointer within STRING.
*     STATUS = INTEGER (Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1983, 1984, 1988, 1989, 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     JRG: Jack Giddings (UCL)
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     2-OCT-1984 (ACD):
*        Documenation improved.
*     13-SEP-1988 (AJC):
*        Documentation improved.
*     24-FEB-1989 (AJC):
*        Check on string sizes.
*     29-JUL-1991 (PCTR):
*        EMS1_PUTC modified from CHR_PUTC.
*     17-JUL-1992 (PCTR):
*        Corrected array subscript handling - it cannot now return IPOSN
*        with a value greater than the declared length of STRING!
*     21-FEB-2001 (AJC):
*        Renamed from EMS1_PUTC
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAI constants

*  Arguments Given:
      CHARACTER CVALUE * ( * )

*  Arguments Given and Returned:
      CHARACTER STRING * ( * )

      INTEGER IPOSN

*  Status
      INTEGER STATUS

*  Local Variables:
      INTEGER ALLOW              ! Allowed length of CVALUE for copying
      INTEGER IDX                ! Character index
      INTEGER SIZE1              ! Declared size of CVALUE
      INTEGER SIZE2              ! Declared size of STRING

*.

*  Initialise the returned status.
      STATUS = SAI__OK

*  Get the size of target string.
      SIZE1 = LEN( CVALUE )
      SIZE2 = LEN( STRING )

*  Check that the pointer is within string.
      IF ( IPOSN .LT. SIZE2 ) THEN

*     Get the length that can be copied.
         ALLOW = MIN( SIZE1, SIZE2 - IPOSN )

*     Copy the string.
         STRING( IPOSN + 1 : ) = CVALUE

*     Check if an ellipsis is required.
         IF ( ALLOW .LT. SIZE1 ) THEN

*        Append an ellipsis.
            IDX = MAX( 1, SIZE2 - 2 )
            STRING( IDX : ) = '...'
            STATUS = SAI__WARN
         END IF

*     Update the pointer value.
         IPOSN = IPOSN + ALLOW
      ELSE

*     The pointer is beyond the declared length of the string, so 
*     append an ellipsis.
         IDX = MAX( 1, SIZE2 - 2 )
         STRING( IDX : ) = '...'
         STATUS = SAI__WARN

*     Update the pointer value.
         IPOSN = SIZE2
      END IF

      END
