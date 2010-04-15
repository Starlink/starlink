      SUBROUTINE SPD_WZKJ( NCOMP, TNPAR, COMPTY, PARATY, STATUS )
*+
*  Name:
*     SPD_WZKJ

*  Purpose:
*     Set up component and parameter types for ARCIDENT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZKJ( NCOMP, TNPAR, COMPTY, PARATY, STATUS )

*  Description:
*     The routine performs some preparatory work for ARCIDENT. It sets
*     up the output component types and parameter types.

*  Arguments:
*     NCOMP = INTEGER (Given)
*        The number of components in the results structure.
*     TNPAR = INTEGER (Given)
*        The total number of parameters in the results structure.
*        This must be at least 2 times NCOMP.
*     COMPTY( NCOMP ) = CHARACTER * ( * ) (Returned)
*        The component types.
*     PARATY( TNPAR ) = CHARACTER * ( * ) (Returned)
*        The parameter types.
*     STATUS = INTEGER (Given and Returned)
*        The global status. The status is set and an error report made
*        if TNPAR is less than twice NCOMP.

*  Notes:
*     This routine assumes that each component has been allocated
*     exactly two parameters.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 Jun 1993 (hme):
*        Original version.
*     25 Jan 1995 (hme):
*        Renamed from SPADQ.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NCOMP
      INTEGER TNPAR

*  Arguments Returned:
      CHARACTER * ( * ) COMPTY( NCOMP )
      CHARACTER * ( * ) PARATY( TNPAR )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that PARATY long enough.
      IF ( TNPAR .LT. 2 * NCOMP ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCIDENT_E15', 'ARCIDENT: Error ' //
     :      'initialising output results: Total number of' //
     :      'parameters must be twice the number of components.',
     :      STATUS )
         RETURN
      END IF

*  Fill COMPTY.
      CALL SPD_UAAFC( 1, NCOMP, COMPTY, 'arc feature', STATUS )

*  Fill PARATY.
      DO 1 I = 1, TNPAR, 2
         PARATY(I) = 'centre'
         PARATY(I+1) = 'laboratory value'
 1    CONTINUE

      END
