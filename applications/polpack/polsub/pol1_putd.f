      SUBROUTINE POL1_PUTD( DVALUE, STRING, IPOSN )
*+
*  Name:
*     POL1_PUTD

*  Purpose:
*     Put a DOUBLE PRECISION value into a string at a given position

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_PUTD( DVALUE, STRING, IPOSN )

*  Description:
*     The DOUBLE PRECISION value is encoded into a concise string which
*     is then copied into the given string beginning at position IPOSN+1.
*     IPOSN is returned updated to indicate the end position of the
*     encoded number within STRING. This is like CHR_PUTD except that
*     exponents use an E instead of a D (TCL does not understand D
*     exponents).

*  Arguments:
*     DVALUE = DOUBLE PRECISION (Given)
*        The value to be encoded into the string.
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string into which DVALUE is to be copied.
*     IPOSN = INTEGER (Given and Returned)
*        The position pointer within STRING.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-JUN-2017 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given
      DOUBLE PRECISION DVALUE

*  Arguments Given and Returned:
      CHARACTER STRING*(*)
      INTEGER IPOSN

*  Local Variables:
      INTEGER I
      INTEGER IOLD
*.

*  Save inital position.
      IOLD = IPOSN

*  Use CHR_PUTD to get the basic string.
      CALL CHR_PUTD( DVALUE, STRING, IPOSN )

*  Convert any D's to E's in the portion of the string just written.
      DO I = IOLD + 1, IPOSN
         IF( STRING( I : I ) .EQ. 'D' ) STRING( I : I ) = 'E'
      END DO

      END
