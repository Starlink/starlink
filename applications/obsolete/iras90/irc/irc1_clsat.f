      SUBROUTINE IRC1_CLSAT( IDC, RA, DEC, UTCOFF, THETA, PSI, STATUS )
*+
*  Name:
*     IRC1_CLSAT

*  Purpose:
*     Calculate satellite coordinates for a given RA and DEC.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_CLSAT( IDC, RA, DEC, UTCOFF, THETA, PSI, STATUS )

*  Description:
*     IRC1_PNTSB is called to ensure that pointing information is
*     available for this CRDD file. The given position is then
*     converted to ecliptic coordinates (mean of date) using the
*     rotation matrix set up by IRC1_PNTSB. The solar longitude (mean
*     of date) at the given UTC offset is calculated using a linear fit
*     set up by IRC1_PNTSB.This is then subtracted from the ecliptic
*     longitude of the given position, to obtain coordinates relative
*     to the sun. The clock and cone angles are then calculated
*     directly. No checks are made that the cone angle generated could
*     actually have been achieved in practice, given the restrictions
*     described in the IRAS Explanatory Supplement chapter III.


*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     RA = DOUBLE PRECISION (Given)
*        The RA (B1950, FK4) of the position for which satellite
*        coordinates are required. Radians.
*     DEC = DOUBLE PRECISION (Given)
*        The DEC (B1950, FK4) of the position for which satellite
*        coordinates are required. Radians.
*     UTCOFF = REAL (Given)
*        Difference in UTC between the time to which the PSI and THETA
*        values returned relate, and the time of data sample number
*        1 in the CRDD file. Seconds.
*     THETA = REAL (Returned)
*        The cone angle, i.e. the angle between the satellite boresight
*        vector and the sun vector. Radians.
*     PSI = REAL (Returned)
*        The clock angle, i.e. the angle between the satellite boresight
*        vector and the ecliptic north pole, measure in the plane
*        perpendicular to the sun vector. PSI increases anticlockwise
*        as seen from the sun (which means PSI decreases with time).
*        Radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-MAR-1991 (DSB):
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
      INCLUDE 'I90_DAT'          ! IRAS90 constants
      INCLUDE 'IRC_PAR'          ! IRC constants

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_SLGRD( IRC__MAX ) = REAL (Read)
*           The rate of change of solar longitude with time, in radians
*           per second (assumed constant).
*        CCM_SLZER( IRC__MAX ) = REAL (Read)
*           The solar longitude at sample number 1, in radians.
*        CCM_RMAT( 9, IRC__MAX ) = DOUBLE PRECISION (Read)
*           Rotation matrices which rotate equatorial (1950) coordinates
*           expressed in Cartesian form, to ecliptic coordinates (of
*           date), also expressed in Cartesian form.

*  Arguments Given:
      INTEGER IDC
      DOUBLE PRECISION RA
      DOUBLE PRECISION DEC
      REAL UTCOFF

*  Arguments Returned:
      REAL THETA
      REAL PSI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION COSSOL    ! Cosine of SOLONG.
      DOUBLE PRECISION P(3)      ! Cartesian coordinates of given point.
      DOUBLE PRECISION SINSOL    ! Sine of SOLONG.
      REAL             SLA_RANGE ! SLALIB function.
      REAL             SLA_RANORM! SLALIB function.
      DOUBLE PRECISION SOLONG    ! Solar longitude (mean of date,
                                 ! radians).
      DOUBLE PRECISION TEMP      ! Temporary storage.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure pointing information is available for this CRDD file.
      CALL IRC1_PNTSB( IDC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Convert the given position from equatorial coordinates (B1950) to a
*  Cartesian representation of ecliptic coordinates (mena of date).
*  This uses the rotation matrix set up by IRC1_PNTSB (stored in
*  common).  X is towards the equinox, Y is 90 degrees east of X in the
*  plane of the ecliptic and Z is towards the north ecliptic pole.
      CALL SLA_DCS2C( RA, DEC, P )
      CALL SLA_DMXV( CCM_RMAT( 1, IDC ), P, P )

*  Find the solar longitude (of date) at the given UTC offset from data
*  sample number 1.
      SOLONG = DBLE( CCM_SLGRD( IDC )*UTCOFF + CCM_SLZER( IDC ) )

*  Rotate the XY plane of the Cartesian coordinate system so that X is
*  pointing towards the sun.
      COSSOL = COS( SOLONG )
      SINSOL = SIN( SOLONG )

      TEMP = P(1)*COSSOL + P(2)*SINSOL
      P(2) = - P(1)*SINSOL + P(2)*COSSOL
      P(1) = TEMP

*  Calculate the clock angle PSI, and the cone angle THETA.
      PSI = SLA_RANORM( REAL( -REAL( ATAN2( P(2), P(3) ) ) ) )
      THETA = SLA_RANGE( REAL( ACOS( P(1) ) ) )

 999  CONTINUE

      END
