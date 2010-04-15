      SUBROUTINE IRC1_SCOSB( IDC, SAMPLE, PSI, THETA, SOLONG, UTCS,
     :                       STATUS )
*+
*  Name:
*     IRC1_SCOSB

*  Purpose:
*     Returns boresight positions at a set of detector samples,
*     assuming SURVEY_BSIGHT data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_SCOSB( IDC, SAMPLE, PSI, THETA, SOLONG, UTCS, STATUS )

*  Description:
*     Straight lines are fitted to the solar longitude and the clock
*     angle (PSI) samples stored in the DETAILS structure contained in
*     the CRDD file. These fits are evaluated at the times of the
*     required samples. Cone angle (THETA) is assumed constant
*     throughout the scan. See figure III.B.7 in the IRAS Catalogues
*     and Atlases Explanatory Supplement for definitions of THETA and
*     PSI. (Note, this routine uses PSI, not PHI, as the clock angle.
*     PSI = 2.PI - PHI ).
*
*     If this is the first time pointing information has been requested
*     from the given CRDD file, then the linear fits are found and the
*     gradients and intercepts stored in common. Also, the matrix which
*     rotates 3-vectors representing Cartesian equatorial (1950)
*     positions to Cartesian ecliptic (of date) positions is calculated
*     and stored in common. Later calls to this routine use the stored
*     information rather than calculating it all again.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     SAMPLE = REAL (Given)
*        A fractional sample number. If BAD, then the corresponding
*        elements of the output arrays are set BAD.
*     PSI = REAL (Returned)
*        The cone angle at the specified sample (radians).
*     THETA = REAL (Returned)
*        The clock angle at the specified sample (radians).
*     SOLONG = REAL (Returned)
*        The solar longitude at the specified sample (radians), refered
*        to the mean equator and equinox at the time given by UTCS.
*     UTCS = DOUBLE PRECISION (Returned)
*        The absolute UTCS at the specified sample (seconds).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-AUG-1993 (DSB):
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
      INCLUDE 'PRM_PAR'          ! Starlink BAD values.
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_POINT( IRC__MAX ) = LOGICAL (Read)
*           True if pointing information has been generated.
*        CCM_BAND( IRC__MAX ) = INTEGER (Read)
*           IRAS band number (NOT wavelength).
*        CCM_CAGRD( IRC__MAX ) = REAL (Read and Write)
*           The rate of change of clock angle (PSI) with time, in
*           radians per second (assumed constant).
*        CCM_CAZER( IRC__MAX ) = REAL (Read and Write)
*           The clock angle (PSI) at sample number 1, in radians.
*        CCM_SLGRD( IRC__MAX ) = REAL (Read and Write)
*           The rate of change of solar longitude with time, in radians
*           per second (assumed constant).
*        CCM_SLZER( IRC__MAX ) = REAL (Read and Write)
*           The solar longitude at sample number 1, in radians.
*        CCM_THETA( IRC__MAX ) = REAL (Read and Write)
*           The cone angle at which the scan was taken, in radians
*           (assumed constant).
*        CCM_UTCS1( IRC__MAX ) = DOUBLE PRECISION (Read and Write)
*           UTCS at sample number 1.0

*  Arguments Given:
      INTEGER IDC
      REAL    SAMPLE

*  Arguments Returned:
      REAL PSI
      REAL THETA
      REAL SOLONG
      DOUBLE PRECISION UTCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL UTCOFF                ! Difference in UTC between sample
                                 ! number 1 and the current sample.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If pointing information has not yet been set up...
      IF( .NOT. CCM_POINT( IDC ) ) THEN

*  Initialise the pointing information for this CRDD file stored in
*  common.
         CALL IRC1_PNTSB( IDC, STATUS )
         IF( STATUS .NE. SAI__OK ) GO TO 999

      END IF

*  If the sample number is BAD, set all the output values BAD.
      IF( SAMPLE .EQ. VAL__BADR ) THEN
         PSI = VAL__BADR
         THETA = VAL__BADR
         SOLONG = VAL__BADR
         UTCS = VAL__BADD

*  Calculate the UTC offset from sample number 1, assuming UTC of a
*  given sample number is the same for all detectors (true for survey
*  CRDD). Return the absolute UTCS.
      ELSE
         UTCOFF = ( SAMPLE - 1.0 )/REAL( I90__SRATE( CCM_BAND( IDC ) ) )
         UTCS = DBLE( UTCOFF ) + CCM_UTCS1( IDC )

*  Calculate the solar longitude and clock angle (PSI) at this time,
*  using the linear fits created by IRC1_PNTSB. Cone angle (THETA) is
*  assumed to be constant throughout the scan.
         SOLONG = CCM_SLGRD( IDC )*UTCOFF + CCM_SLZER( IDC )
         PSI = CCM_CAGRD( IDC )*UTCOFF + CCM_CAZER( IDC )
         THETA = CCM_THETA( IDC )

      END IF

 999  CONTINUE

      END
