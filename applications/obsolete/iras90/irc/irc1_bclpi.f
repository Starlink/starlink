      SUBROUTINE IRC1_BCLPI( IDC, DETIND, RA, DEC, CLSAMP, CLZFP,
     :                       STATUS )
*+
*  Name:
*     IRC1_BCLPI

*  Purpose:
*     Find the position of closest approach of the boresight track to a
*     given sky position, without argument verification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_BCLPI( IDC, DETIND, RA, DEC, CLSAMP, CLZFP, STATUS )

*  Description:
*     This routine provides the functionality for IRC_BCLAP, without
*     argument verification.

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the CRDD file.
*     DETIND = INTEGER (Given)
*        The detector index to use.
*     RA = DOUBLE PRECISION (Given)
*        The Right Ascension (FK4 B1950) of the given sky position.
*     DEC = DOUBLE PRECISION (Given)
*        The Declination (FK4 B1950) of the given sky position.
*     CLSAMP = REAL (Returned)
*        The fractional sample number at which the point of closest
*        approach of the boresight track to the given sky position is
*        reached.
*     CLZFP = REAL (Returned)
*        The focal plane Z coordinate of the given position at the point
*        of closest approach. In radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-FEB-1991 (DSB):
*        Original version.
*     9-MAY-1991 (DSB):
*        Updated for IRA version 2.
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
      INCLUDE 'IRC_ERR'          ! IRC error values.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_TYPE( IRC__MAX ) = CHARACTER (Read)
*           HDS type of the DETAILS component.

*  Arguments Given:
      INTEGER IDC
      INTEGER DETIND
      DOUBLE PRECISION RA
      DOUBLE PRECISION DEC

*  Arguments Returned:
      REAL    CLSAMP
      REAL    CLZFP

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call an appropriate routine for each CRDD type.
      IF( CCM_TYPE( IDC ) .EQ. 'SURVEY_BSIGHT' ) THEN
         CALL IRC1_BCLSB( IDC, DETIND, RA, DEC, CLSAMP, CLZFP, STATUS )

*  If the CRDD type is unrecognised, give an error report.
      ELSE
         STATUS = IRC__BADTY
         CALL MSG_SETC( 'T', CCM_TYPE( IDC ) )
         CALL ERR_REP( 'IRC1_BCLPI_ERR1',
     :                 'IRC1_BCLPI does not yet support ^T data',
     :                 STATUS )
      END IF

      END
