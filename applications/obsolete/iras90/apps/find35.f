      SUBROUTINE FIND35( NOELEM, POSN, VALUEI, ARRAYI, STATUS )
*+
*  Name:
*     FIND35

*  Purpose:
*     To put the integer value VALUEI in the integer array
*     element ARRAYI(POSN)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND35( NOELEM, POSN, VALUEI, ARRAYI, STATUS )

*  Description:
*     To put the integer value VALUEI in the integer array
*     element ARRAYI(POSN)

*  Arguments:
*     NOELEM = INTEGER (Given)
*        Number of elements in ARRAYI
*     POSN = INTEGER (Given)
*        Position in array in which value is to be put
*     VALUEI = INTEGER (Given)
*        Value to be put in ARRAYI( POSN )
*     ARRAYI( NOELEM ) = INTEGER (Given and Returned)
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
      INTEGER VALUEI

*  Arguments Given and Returned:
      INTEGER ARRAYI( NOELEM )

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Put the value VALUEI in the element ARRAYI( POSN )
      ARRAYI( POSN ) = VALUEI

      END
