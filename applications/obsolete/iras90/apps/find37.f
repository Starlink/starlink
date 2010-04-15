      SUBROUTINE FIND37( NOELEM, POSN, VALUER, ARRAYR, STATUS )
*+
*  Name:
*     FIND37

*  Purpose:
*     To put the real value VALUER in the real array
*     element ARRAYR(POSN)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND37( NOELEM, POSN, VALUER, ARRAYR, STATUS )

*  Description:
*     To put the real value VALUER in the real array
*     element ARRAYR(POSN)

*  Arguments:
*     NOELEM = INTEGER (Given)
*        Number of elements in ARRAYR
*     POSN = INTEGER (Given)
*        Position in array in which value is to be put
*     VALUER = REAL (Given)
*        Value to be put in ARRAYR( POSN )
*     ARRAYR( NOELEM ) = REAL (Given and Returned)
*        Array in which value is to be placed
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     None

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     2-APR-1992 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NOELEM
      INTEGER POSN
      REAL VALUER

*  Arguments Given and Returned:
      REAL ARRAYR( NOELEM )

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Put the value VALUER in the element ARRAYR( POSN )
      ARRAYR( POSN ) = VALUER

      END
