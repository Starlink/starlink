      SUBROUTINE SPD_WZJA( MODE, NCOMP, TNPAR, NPARA, COMPTY, PARATY,
     :   STATUS )
*+
*  Name:
*     SPD_WZJA

*  Purpose:
*     Check results structure is suitable for ARCLOCAT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZJA( MODE, NCOMP, TNPAR, NPARA, COMPTY, PARATY, STATUS )

*  Description:
*     This routine checks an existing, accessed results structure. It
*     finds out if the structure conforms to what ARCLOCAT needs, or
*     would have created. The criteria are:
*     -  Each component has two parameters.
*     -  The parameters of each component have types 'centre' and
*        'peak'.
*     -  Each component is of type 'Gauss feature' (if MODE='G') or
*        'triangle feature' (if MODE='T').

*  Arguments:
*     MODE = CHARACTER * ( * ) (Given)
*        Must be 'G' or 'T', case-sensistive.
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
*     09 Jun 1993 (hme):
*        Original version.
*     25 Nov 1994 (hme):
*        Renamed from SPADV.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) MODE
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
         CALL ERR_REP( 'ARCLOCAT_E09', 'ARCLOCAT: Error accessing ' //
     :      'input results structure. Too few parameters in total.',
     :      STATUS )
         GO TO 500
      END IF

*  Loop through the components.
      DO 1 I = 1, NCOMP

*     Check NPARA.
         IF ( NPARA(I) .NE. 2 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ARCLOCAT_E10', 'ARCLOCAT: Error ' //
     :         'accessing input results structure. Not all ' //
     :         'components have two parameters.', STATUS )
            GO TO 500
         END IF

*     Check PARATY.
         IF ( .NOT. CHR_SIMLR( PARATY(2*I-1), 'centre' ) .OR.
     :        .NOT. CHR_SIMLR( PARATY(2*I),   'peak'   ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ARCLOCAT_E11', 'ARCLOCAT: Error ' //
     :         'accessing input results structure. Not all ' //
     :         'components have "centre" and "peak".', STATUS )
            GO TO 500
         END IF
 1    CONTINUE

*  Check all COMPTY.
      IF ( MODE .EQ. 'G' ) THEN
         DO 2 I = 1, NCOMP
            IF ( .NOT. CHR_SIMLR( COMPTY(I), 'Gauss feature' ) ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'ARCLOCAT_E12', 'ARCLOCAT: Error ' //
     :            'accessing input results structure. Not all ' //
     :            'components are Gauss features.', STATUS )
               GO TO 500
            END IF
 2       CONTINUE
      ELSE IF ( MODE .EQ. 'T' ) THEN
         DO 3 I = 1, NCOMP
            IF ( .NOT. CHR_SIMLR( COMPTY(I), 'triangle feature' ) ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'ARCLOCAT_E13', 'ARCLOCAT: Error ' //
     :            'accessing input results structure. Not all ' //
     :            'components are triangle features.', STATUS )
               GO TO 500
            END IF
 3       CONTINUE
      END IF

*  Return.
 500  CONTINUE
      END
