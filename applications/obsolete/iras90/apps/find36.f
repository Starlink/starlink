      SUBROUTINE FIND36( NOELEM, POSN, VALUEL, ARRAYL, STATUS )
*+
*  Name:
*     FIND36

*  Purpose:
*     To put the logical value VALUEL in the logical array
*     element ARRAYL(POSN)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND36( NOELEM, POSN, VALUEL, ARRAYL, STATUS )

*  Description:
*     To put the logical value VALUEL in the logical array
*     element ARRAYL(POSN)

*  Arguments:
*     NOELEM = INTEGER (Given)
*        Number of elements in ARRAYL
*     POSN = INTEGER (Given)
*        Position in array in which value is to be put
*     VALUEL = LOGICAL (Given)
*        Value to be put in ARRAYL( POSN )
*     ARRAYL( NOELEM ) = LOGICAL (Given and Returned)
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
      LOGICAL VALUEL

*  Arguments Given and Returned:
      LOGICAL ARRAYL( NOELEM )

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Put the value VALUEL in the element ARRAYL( POSN )
      ARRAYL( POSN ) = VALUEL

      END
