      SUBROUTINE IRC1_DCLSB( IDC, DETIND, RA, DEC, CLSAMP, CLZFP,
     :                       STATUS )
*+
*  Name:
*     IRC1_DCLSB

*  Purpose:
*     Find the position of closest approach of a detector track to a
*     given sky position, assuming SURVEY_BSIGHT data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_DCLSB( IDC, DETIND, RA, DEC, CLSAMP, CLZFP, STATUS )

*  Description:
*     The closest approach of the boresight to the given point is found
*     by calling IRC1_BCLSB. The sample number thus found is incremented
*     sufficiently to bring the specified detector to the same in-scan
*     position that the boresight had at its closest approach. This
*     assumes simultaneous detector samples and constant scan speed.

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
*        approach of the detector track to the given sky position is
*        reached. Accurate to about 0.1 samples.
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
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC error values.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_BAND( IRC__MAX ) = INTEGER (Read)
*           IRAS band number (NOT wavelength).
*        CCM_NOMSP( IRC__MAX ) = REAL (Read)
*           Nominal scan speed.
*        CCM_DLOW( IRC__MAX ) = INTEGER (Read)
*           Lowest detector index in the DATA array.
*        CCM_DETNO( IRC__MXD2S, IRC__MAX ) = INTEGER (Read)
*           The detector number corresponding to each detector index.
*        CCM_DETOR( IRC__MAX ) = INTEGER (Read)
*           The index within CCM_DETNO corresponding to row zero of the
*           NDF.

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

*  Local Variables:
      REAL    BSAMP              ! Sample number at which the boresight
                                 ! reaches its closest approach to the
                                 ! given point.
      INTEGER DETNO              ! The detector number of the detector
                                 ! with the given index.
      REAL    DYFP               ! The focal plane Y coordinate of the
                                 ! detector centre, in radians.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the sample number at which the boresight reaches its closest
*  approach to the given point.

      CALL IRC1_BCLSB( IDC, DETIND, RA, DEC, BSAMP, CLZFP, STATUS )

*  Get the detector number with the given detector index, and the focal
*  plane Y coordinate of the detector, in radians.
      DETNO = CCM_DETNO( DETIND + CCM_DETOR( IDC ), IDC )
      DYFP = I90__DETY( DETNO )*IRA__DTOR/60.0

*  Find the number of samples which are taken while the detector centre
*  moves from its position at sample BSAMP, to the in-scan position of
*  the boresight at sample BSAMP. Add this value onto BSAMP to get the
*  sample at which the detector reaches its closest approach to the
*  given position.
      CLSAMP = BSAMP + DYFP*I90__SRATE( CCM_BAND( IDC ) )/
     :                 CCM_NOMSP( IDC )

 999  CONTINUE

      END

