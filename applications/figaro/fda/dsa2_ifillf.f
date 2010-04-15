      SUBROUTINE DSA2_IFILLF( EL, LBND, ARRAY, STATUS )
*+
*  Name:
*     DSA2_IFILLF

*  Purpose:
*     Fills an NDF axis-centre array with pixel indices.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA2_IFILLF( EL, LBND, ARRAY, STATUS )

*  Description:
*     This routine fills the given real array with numbers that start
*     at LBND and increase by 1.0 with each index increment.

*  Arguments:
*     EL = INTEGER (Given)
*        The size of the array.
*     LBND = INTEGER (Given)
*        The lower bound of the NDF.
*     ARRAY( EL ) = REAL (Returned)
*        The array to fill.
*     STATUS = INTEGER (Given)
*        The global status.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 July 9 (MJC):
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
      INTEGER EL
      INTEGER LBND

*  Arguments Returned:
      REAL ARRAY( EL )

*  Local Variables:
      INTEGER I

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 10 I = 1, EL
         ARRAY( I ) = REAL( I + LBND - 1 )
   10 CONTINUE

      END
