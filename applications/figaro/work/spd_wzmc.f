      SUBROUTINE SPD_WZMC( NCOMP, TNPAR, NPARA, COMPTY, PARATY,
     :   STATUS )
*+
*  Name:
*     SPD_WZMC

*  Purpose:
*     Check results structure is suitable for ARCDISP.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZMC( NCOMP, TNPAR, NPARA, COMPTY, PARATY, STATUS )

*  Description:
*     This routine checks an existing, accessed results structure. It
*     finds out if the structure conforms to what ARCDISP needs.
*     The criteria are:
*     -  Each component has two parameters.
*     -  The parameters of each component have types 'centre' and
*        'laboratory value'.
*     -  Each component is of type 'arc feature'.

*  Arguments:
*     NCOMP = INTEGER (Given)
*        The number of components, size of NPARA and COMPTY arrays.
*     TNPAR = INTEGER (Given)
*        The total number of parameters, size of the PARATY array.
*     NPARA( NCOMP ) = INTEGER (Given)
*        The array telling how many parameters each component has.
*     COMPTY( NCOMP ) = CHARACTER * ( * ) (Given)
*        The array of component types.
*     PARATY( TNPAR ) = CHARACTER * ( * ) (Given)
*        The array of parameter types.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set and an error reported if the
*        given arrays fail the check as decribed above.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     17 Jun 1993 (hme):
*        Original version.
*     25 Jan 1995 (hme):
*        Renamed from SPADY.
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
      INTEGER NPARA( NCOMP )
      CHARACTER * ( * ) COMPTY( NCOMP )
      CHARACTER * ( * ) PARATY( TNPAR )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index

*  Internal References:
      LOGICAL CHR_SIMLR          ! True if strings are similar

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check TNPAR.
      IF ( TNPAR .LT. 2 * NCOMP ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCDISP_E07', 'ARCDISP: Error accessing ' //
     :      'input results structure. Too few parameters in total.',
     :      STATUS )
         GO TO 500
      END IF

*  Loop through the components.
      DO 1 I = 1, NCOMP

*     Check NPARA.
         IF ( NPARA(I) .NE. 2 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ARCDISP_E08', 'ARCDISP: Error ' //
     :         'accessing input results structure. Not all ' //
     :         'components have two parameters.', STATUS )
            GO TO 500
         END IF

*     Check all COMPTY.
         IF ( .NOT. CHR_SIMLR( COMPTY(I), 'arc feature' ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ARCDISP_E09', 'ARCDISP: Error ' //
     :         'accessing input results structure. Not all ' //
     :         'components are arc features.', STATUS )
            GO TO 500
         END IF

*     Check PARATY.
         IF ( .NOT. CHR_SIMLR( PARATY(2*I-1), 'centre' ) .OR.
     :        .NOT. CHR_SIMLR( PARATY(2*I), 'laboratory value' ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ARCDISP_E10', 'ARCDISP: Error ' //
     :         'accessing input results structure. Not all ' //
     :         'components have "centre" and "laboratory value".',
     :         STATUS )
            GO TO 500
         END IF
 1    CONTINUE

*  Return.
 500  CONTINUE
      END
