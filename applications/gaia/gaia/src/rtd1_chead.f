      SUBROUTINE RTD1_CHEAD( IN, NEL, OUT, STATUS )
*+
* Name:
*    RTD1_CHEAD

*  Purpose:
*     Writes an output FITS header from an existing one.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RTD1_CHEAD( IN, NEL, OUT, STATUS )

*  Description:
*     This routine copies a 1D character array of a given size into
*     another (which must be at least as big). The contents are assumed
*     to be FITS cards, which have been created by GAIA. A check is made
*     to ensure that the special NDFID card is not written out.

*  Arguments:
*     IN( NEL ) = CHARACTER * ( * ) (Given)
*        The input FITS block (from GAIA).
*     NEL = INTEGER (Given)
*        The number of elements (cards) in IN.
*     OUT( NEL ) = CHARACTER * ( * ) (Returned)
*        The output FITS block.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     15-DEC-1997 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NEL
      CHARACTER * ( * ) IN( NEL )

*  Arguments Returned:
      CHARACTER * ( * ) OUT( NEL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the whole of the input character array to the output one. If a
*  card that starts with "NDFID" is found it is replaced with a blank 
*  card.
      DO 1 I = 1, NEL
         IF ( IN( I )( 1: 5 ) .NE. 'NDFID' ) THEN 
            OUT( I ) = IN( I )
         ELSE
            OUT( I ) = ' '
         END IF
 1    CONTINUE
      END
