      SUBROUTINE FIND34( NOELEM, POSN, VALUED, ARRAYD, STATUS )
*+
*  Name:
*     FIND34

*  Purpose:
*     To put the double value VALUED in the double array
*     element ARRAYD(POSN)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND34( NOELEM, POSN, VALUED, ARRAYD, STATUS )

*  Description:
*     To put the double value VALUED in the double array
*     element ARRAYD(POSN)

*  Arguments:
*     NOELEM = INTEGER (Given)
*        Number of elements in ARRAYD
*     POSN = INTEGER (Given)
*        Position in array in which value is to be put
*     VALUED = DOUBLE PRECISION (Given)
*        Value to be put in ARRAYD( POSN )
*     ARRAYD( NOELEM ) = DOUBLE PRECISION (Given and Returned)
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
      DOUBLE PRECISION VALUED

*  Arguments Given and Returned:
      DOUBLE PRECISION ARRAYD( NOELEM )

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Put the value VALUED in the element ARRAYD( POSN )
      ARRAYD( POSN ) = VALUED

      END
