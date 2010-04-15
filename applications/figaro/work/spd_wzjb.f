      SUBROUTINE SPD_WZJB( TNPAR, PARATY, STATUS )
*+
*  Name:
*     SPD_WZJB

*  Purpose:
*     Set up parameter types for ARCLOCAT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZJB( NCOMP, TNPAR, COMPTY, PARATY, STATUS )

*  Description:
*     The routine performs some preparatory work for ARCLOCAT. It sets
*     up the output parameter types.

*  Arguments:
*     TNPAR = INTEGER (Given)
*        The total number of parameters in the results structure. This
*        must be an even number.
*     PARATY( TNPAR ) = CHARACTER * ( * ) (Returned)
*        The parameter types.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     07 Jun 1993 (hme):
*        Original version.
*     25 Nov 1994 (hme):
*        Renamed from SPADT.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER TNPAR

*  Arguments Returned:
      CHARACTER * ( * ) PARATY( TNPAR )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Fill PARATY.
      DO 1 I = 2, TNPAR, 2
         PARATY(I-1) = 'centre'
         PARATY(I) = 'peak'
 1    CONTINUE

      END
