      SUBROUTINE IRC1_BCLSB( IDC, DETIND, RA, DEC, CLSAMP, CLZFP,
     :                       STATUS )
*+
*  Name:
*     IRC1_BCLSB

*  Purpose:
*     Find the position of closest approach of the boresight track to a
*     given sky position, assuming SURVEY_BSIGHT data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_BCLSB( IDC, DETIND, RA, DEC, CLSAMP, CLZFP, STATUS )

*  Description:
*     The algorithm works in satellite coordinates, PSI and THETA (see
*     IRAS Explanatory Supplement chapter III). The closest approach
*     of the boresight to the given RA/DEC position occurs when the
*     satellite clock angle (PSI) matches the equivalent clock angle
*     of the given point. However, the equivalent clock angle of the
*     given point varies slowly with time, as the solar longitude
*     varies. An iterative scheme is used in which a first guess at the
*     clock angle is made based on the solar longitude reached at the
*     start of the scan. The UTC at which this clock angle would
*     actually be reached in then found, and the corresponding solar
*     longitude is found. A new clock angle is found based on this new
*     solar longitude. The iterative scheme is continued until the
*     change in PSI between succesive iterations is less than 0.1
*     samples. This usually happens after only one iteration.

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
*        CCM_CAGRD( IRC__MAX ) = REAL (Read)
*           The rate of change of clock angle (PSI) with time, in
*           radians per second (assumed constant).
*        CCM_CAZER( IRC__MAX ) = REAL (Read)
*           The clock angle (PSI) at sample number 1, in radians.
*        CCM_THETA( IRC__MAX ) = REAL (Read)
*           The cone angle at which the scan was taken, in radians
*           (assumed constant).

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
      REAL OLDPSI                ! Last times value of PSI
      REAL PSI                   ! Clock angle, radians.
      REAL PSIERR                ! Max. allowed error in PSI, radians.
      REAL SLA_RANGE             ! SLALIB function giving angle in range
                                 ! +/- PI.
      REAL SMPFRQ                ! Detector sampling frequency, Hz.
      REAL THETA                 ! Cone angle, radians.
      REAL UTCOFF                ! Time since data sample no. 1,
                                 ! seconds.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the satellite coordinates corresponding to the given
*  position assuming the solar longitude is constant at the value
*  reached at data sample number 1.
      CALL IRC1_CLSAT( IDC, RA, DEC, 0.0, THETA, PSI, STATUS )

*  The sample position should be good to a tength of a sample.
*  Set up the maximum allowed error in PSI. Note, the rate of change of
*  clock angle (PSI) with repect to time is held in CCM_CAGRD, which is
*  set up in a call to IRC1_PNTSB called from within IRC1_CLSAT.
      SMPFRQ = I90__SRATE( CCM_BAND( IDC ) )
      PSIERR = ABS( CCM_CAGRD( IDC )*0.1/SMPFRQ )

*  The solar longitude is not in fact constant. Iterate until the
*  clock angle and solar longitude values are consistent (i.e. are
*  achieved at the same moment in time to within a tenth of a sample).
 10   CONTINUE

*  Save last times value of PSI.
      OLDPSI = PSI

*  Find the UTC offset from data sample number 1 at which this
*  clock angle is achieved. This assumes a linear relation between UTC
*  offset and clock angle. The fit is set up by IRC1_PNTSB, called from
*  within IRC1_CLSAT.
      UTCOFF = ( OLDPSI - CCM_CAZER( IDC ) )/CCM_CAGRD( IDC )

*  Now calculate the satellite coordinates corresponding to the given
*  position assuming the solar longitude is constant at the value
*  reached at this UTC offset.
      CALL IRC1_CLSAT( IDC, RA, DEC, UTCOFF, THETA, PSI, STATUS )

*  If the clock angle PSI changed by more than the allowed error,
*  go round for another iteration.
      IF( ABS( SLA_RANGE( PSI - OLDPSI ) ) .GT. PSIERR) GO TO 10

*  Find the sample number to which this UTC offset corresponds.
      CLSAMP = UTCOFF*SMPFRQ + 1.0

*  Find the focal plane Z coordinate of the given position at this time.
      CLZFP = CCM_THETA( IDC ) - THETA

 999  CONTINUE

      END
