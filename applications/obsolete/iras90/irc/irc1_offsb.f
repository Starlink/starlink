      SUBROUTINE IRC1_OFFSB( IDC, SAMP1, DETIN1, DETIN2, DIST, SAMP2,
     :                       STATUS )
*+
*  Name:
*     IRC1_OFFSB

*  Purpose:
*     Find a sample which is a given in-scan distance away from a given
*     sample, assuming SURVEY_BSIGHT data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_OFFSB( IDC, SAMP1, DETIN1, DETIN2, DIST, SAMP2, STATUS )

*  Description:
*     The sample is found by assuming constant scan speed
*     (= CCM_NOMSP), and simultaneous detector samples in each column
*     of the DATA array. No verification of argument values is done.

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
*        The arc-distance to offset away from SAMP1, in radians.
*     SAMP2 = REAL (Returned)
*        The fractional sample number from detector DETIN2 which is the
*        required distance away from sample SAMP1 from DETIN1. The
*        displacement from SAMP1 to SAMP2 is in the same direction as
*        the focal plane Y axis if DIST is positive.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-FEB-1991 (DSB):
*        Original version.
*     11-FEB-1992 (DSB):
*        Modified to use CCM_DETOR.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_BAND( IRC__MAX ) = INTEGER (Read)
*           IRAS band number (NOT wavelength).
*        CCM_NOMSP( IRC__MAX ) = REAL (Read)
*           Nominal scan speed. Radians per second.
*        CCM_DETNO( IRC__MAXD2S, IRC__MAX ) = INTEGER (Read)
*           The detector number corresponding to each detector index.
*        CCM_DETOR( IRC__MAX ) = INTEGER (Read)
*           The index within CCM_DETNO corresponding to row zero of the
*           NDF.

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

*  Local Variables:
      REAL TOTAL                 ! Total in-scan distance which the
      REAL Y1                    ! Focal plane Y coordinate of centre
                                 ! of detector with index DETIN1 (in
                                 ! arc-mins).
      REAL Y2                    ! Focal plane Y coordinate of centre
                                 ! of detector with index DETIN2 (in
                                 ! arc-mins).
                                 ! boresight moves.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the focal plane Y coordinate at the centre of the two detectors.
      Y1 = I90__DETY( CCM_DETNO( DETIN1 + CCM_DETOR( IDC ), IDC ) )
      Y2 = I90__DETY( CCM_DETNO( DETIN2 + CCM_DETOR( IDC ), IDC ) )

*  Remove the in-scan distance between the two detectors from the
*  required distance. This gives the total distance which the boresight
*  moves, in radians.
      TOTAL = DIST - ( Y2 - Y1 )*IRA__DTOR/60.0

*  Calculate the sample number which is the required total in-scan
*  distance away from SAMP1 (in the positive Y, or anti-scan,
*  direction), assuming constant scan speed.
      SAMP2 = SAMP1 - REAL( I90__SRATE( CCM_BAND( IDC ) ) )
     :               *TOTAL/CCM_NOMSP( IDC )

      END
