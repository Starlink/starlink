      SUBROUTINE IRC1_DISTI( IDC, SAMP1, DETIN1, SAMP2, DETIN2, DIST,
     :                       STATUS )
*+
*  Name:
*     IRC1_DISTI

*  Purpose:
*     Find the arc-distance between two given samples without argument
*     verification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_DISTI( IDC, SAMP1, DETIN1, SAMP2, DETIN2, DIST, STATUS )

*  Description:
*     This routine provides the functionality for IRC_DIST without the
*     overheads of argument verification.

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the CRDD file.
*     SAMP1 = REAL (Given)
*        The first fractional sample number.
*     DETIN1 = INTEGER (Given)
*        The detector index to which the SAMP1 refers.
*     SAMP2 = REAL (Given)
*        The second fractional sample number.
*     DETIN2 = INTEGER (Given)
*        The detector index to which the SAMP2 refers.
*     DIST = REAL (Given)
*        The arc-length of the detector track joining SAMP1 and SAMP2,
*        in radians. Positive if the displacement from SAMP1 to SAMP2 is
*        in the same direction as the focal plane Y axis.
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
      REAL    SAMP2
      INTEGER DETIN2

*  Arguments Returned:
      REAL    DIST

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call an appropriate routine for each CRDD type.
      IF( CCM_TYPE( IDC ) .EQ. 'SURVEY_BSIGHT' ) THEN
         CALL IRC1_DSTSB( IDC, SAMP1, DETIN1, SAMP2, DETIN2, DIST,
     :                    STATUS )

*  If the CRDD type is unrecognised, give an error report.
      ELSE
         STATUS = IRC__BADTY
         CALL MSG_SETC( 'T', CCM_TYPE( IDC ) )
         CALL ERR_REP( 'IRC1_DISTI_ERR1',
     :                 'IRC1_DISTI does not yet support ^T data',
     :                 STATUS )
      END IF


      END
