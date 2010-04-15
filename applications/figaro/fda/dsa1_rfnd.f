      SUBROUTINE DSA1_RFND( DSAREF, SLOT, STATUS )
*+
*  Name:
*     DSA1_RFND

*  Purpose:
*     Look up the reference slot number for a reference name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA1_RFND( DSAREF, SLOT, STATUS )

*  Description:
*     Look up the reference slot number for a reference name.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     SLOT = INTEGER (Returned)
*        The reference slot number allocated to this reference name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     26 Nov 1995 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF

*  Arguments Returned:
      INTEGER SLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 16 ) REFUC   ! Upper case version of reference

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Fold reference name to upper case, as it would be stored in the
*  common block.
      REFUC = DSAREF
      CALL CHR_UCASE( REFUC )

*  Check each slot in turn, first match bails out of search.
*  If the loop finishes, the reference could not be resolved.
      DO 1 SLOT = 1, DSA__MAXREF
         IF ( DSA__REFUSD(SLOT) .AND. REFUC .EQ. DSA__REFNAM(SLOT) )
     :      GO TO 2
 1    CONTINUE
         SLOT = 1
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T001', REFUC )
         CALL ERR_REP( 'FDA_E008', 'DSA1_RFND: ' //
     :      'No slot in use for reference ^FDA_T001.', STATUS )
 2    CONTINUE

*  Return.
      END
