      SUBROUTINE IRC1_DSTSB( IDC, SAMP1, DETIN1, SAMP2, DETIN2, DIST,
     :                       STATUS )
*+
*  Name:
*     IRC1_DSTSB

*  Purpose:
*     Find the in-scan distance between two samples from the same
*     detector, assuming SURVEY_BSIGHT data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_DSTSB( IDC, SAMP1, DETIN1, SAMP2, DETIN2, DIST, STATUS )

*  Description:
*     The distance is found by assuming constant scan speed and
*     simultaneous detector samples in each column of the NDF DATA
*     array. No verification of argument values is done.

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the CRDD file.
*     SAMP1 = REAL (Given)
*        The first fractional sample position.
*     DETIN1 = INTEGER (Given)
*        The detector index to which SAMP1 refers.
*     SAMP2 = REAL (Given)
*        The second fractional sample position.
*     DETIN2 = INTEGER (Given)
*        The detector index to which SAMP2 refers.
*     DIST = REAL (Given)
*        The arc-distance between SAMP1 and SAMP2, in radians.
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
*        CCM_SLOW( IRC__MAX ) = INTEGER (Read)
*           Lowest sample number in the DATA array.
*        CCM_SHIGH( IRC__MAX ) = INTEGER (Read)
*           Highest sample number in the DATA array.
*        CCM_DETNO( IRC__MAXD2S, IRC__MAX ) = INTEGER (Read)
*           The detector number corresponding to each detector index.
*        CCM_DETOR( IRC__MAX ) = INTEGER (Read)
*           The index within CCM_DETNO corresponding to row zero of the
*           NDF.

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

*  Local Variables:
      REAL Y1                    ! Focal plane Y coordinate of centre
                                 ! of detector with index DETIN1 (in
                                 ! arc-mins).
      REAL Y2                    ! Focal plane Y coordinate of centre
                                 ! of detector with index DETIN2 (in
                                 ! arc-mins).
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the distance moved by the boresight in the positive Y
*  direction (i.e. anti scan direction) assuming constant scan
*  speed and simultaneous detectors.
      DIST = CCM_NOMSP( IDC )*( SAMP1 - SAMP2 )
     :      /REAL( I90__SRATE( CCM_BAND( IDC ) ) )

*  Get the focal plane Y coordinate at the centre of the two detectors.
      Y1 = I90__DETY( CCM_DETNO( DETIN1 + CCM_DETOR( IDC ), IDC ) )
      Y2 = I90__DETY( CCM_DETNO( DETIN2 + CCM_DETOR( IDC ), IDC ) )

*  Add on to this the in-scan distance between the two detectors.
      DIST = DIST + ( Y2 - Y1 )*IRA__DTOR/60.0

      END
