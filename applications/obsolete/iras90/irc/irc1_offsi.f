      SUBROUTINE IRC1_OFFSI( IDC, SAMP1, DETIN1, DETIN2, DIST, SAMP2,
     :                       STATUS )
*+
*  Name:
*     IRC1_OFFSI

*  Purpose:
*     Find a sample which is a given in-scan distance away from a given
*     sample, with no argument verification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_OFFSI( IDC, SAMP1, DETIN1, DETIN2, DIST, SAMP2, STATUS )

*  Description:
*     This routine provides the functionality for IRC_OFFST, without
*     argument verification.

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the CRDD file.
*     SAMP1 = REAL (Given)
*        A fractional sample number which gives the starting point of
*        the offset operation.
*     DETIN1 = INTEGER (Given)
*        The detector index to which SAMP1 refers.
*     DETIN2 = INTEGER (Given)
*        The detector index to which SAMP2 should refer.
*     DIST = REAL (Given)
*        The in-scan distance to offset away from SAMP1, in radians.
*     SAMP2 = REAL (Returned)
*        The fractional sample number from detector DETIN2 which is the
*        required distance away from sample SAMP1 from detector DETIN1.
*        The displacement from SAMP1 to SAMP2 is in the same direction
*        as the focal plane Y axis if DIST is positive.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-FEB-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC errors.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_TYPE( IRC__MAX ) = CHARACTER (Read)
*           HDS type of the DETAILS component.

*  Arguments Given:
      INTEGER IDC
      REAL    SAMP1
      INTEGER DETIN1
      INTEGER DETIN2
      REAL    DIST

*  Arguments Returned:
      REAL    SAMP2

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call an appropriate routine for each CRDD type.
      IF( CCM_TYPE( IDC ) .EQ. 'SURVEY_BSIGHT' ) THEN
         CALL IRC1_OFFSB( IDC, SAMP1, DETIN1, DETIN2, DIST, SAMP2,
     :                    STATUS )

*  If the CRDD type is unrecognised, give an error report.
      ELSE
         STATUS = IRC__BADTY
         CALL MSG_SETC( 'T', CCM_TYPE( IDC ) )
         CALL ERR_REP( 'IRC1_OFFSI_ERR1',
     :                 'IRC1_OFFSI does not yet support ^T data',
     :                 STATUS )
      END IF

      END
