      SUBROUTINE DSA_DELETE_QUALITY( DSAREF, STATUS )
*+
*  Name:
*     DSA_DELETE_QUALITY

*  Purpose:
*     Delete the quality component in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_DELETE_QUALITY( DSAREF, STATUS )

*  Description:
*     This routine removes the quality component from an NDF. If the NDF
*     does not contain a quality component, then this routine does
*     nothing. The quality array should not be mapped when this routine
*     is called, and nor should the main data array. Note that this
*     routine does no processing of the data - it merely deletes the
*     structure.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     12 Feb 1995 (ks):
*        Original version.
*     21 Feb 1996 (hme):
*        FDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! The reference slot

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up reference slot.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )

*  Delete the quality component.
      CALL NDF_RESET( DSA__REFID1(SLOT), 'QUALITY', STATUS )

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END
