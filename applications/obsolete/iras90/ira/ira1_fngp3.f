      SUBROUTINE IRA1_FNGP3( SCS, NSTGAP, STGAPS, NSAGAP, SAGAPS,
     :                       NUDGAP, UDGAPS, LONINT, LATINT, STATUS )
*+
*  Name:
*     IRA1_FNGP3

*  Purpose:
*     Find nearest fine gap values for longitude and latitude interval.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_FNGP3( SCS, NSTGAP, STGAPS, NSAGAP, SAGAPS,
*                      NUDGAP, UDGAPS, LONINT, LATINT, STATUS )

*  Description:
*     This find the nearest 'fine gap' to the given longitude and
*     latitude interval. When the sky coordinate is equatorial the fine
*     gap longitude will be in seconds of time, otherwise the gap is in
*     seconds of degree. The latitude gaps are always in seconds of
*     degree. For galactic and ecliptic coordinates, both longitude and
*     latitude gaps will be in micro-degrees.

*  Arguments:
*     SCS = CHARACTER*( * ) (Given)
*        The name of the sky coordinate system.
*     NSTGAP = INTEGER (Given)
*        The number of the fine gaps in seconds of time.
*     STGAPS( NSTGAP ) = REAL (Given)
*        The fine gaps in seconds of time.
*     NSAGAP = INTEGER (Given)
*        The number of the fine gaps in seconds of arc.
*     SAGAPS( NSAGAP ) = REAL (Given)
*        The fine gaps in seconds of arc.
*     NUDGAP = INTEGER (Given)
*        The number of the fine gaps in micro-degrees.
*     UDGAPS( NUDGAP ) = REAL (Given)
*        The fine gaps in micro-degrees.
*     LONINT = DOUBLE PRECISION (Given and Returned)
*        On entry, it is the initial longitude interval. On exit, it is
*        the found nearest fine gap.
*     LATINT = DOUBLE PRECISION (Given and Returned)
*        On entry, it is the initial latitude interval. On exit, it is
*        the found nearest fine gap.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-FEB-1992 (DSB):
*        Original version based on IRA1_FINGP.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA_ package constants

*  Arguments Given:
      CHARACTER*( * ) SCS
      INTEGER NSTGAP
      REAL STGAPS( NSTGAP )
      INTEGER NSAGAP
      REAL SAGAPS( NSAGAP )
      INTEGER NUDGAP
      REAL UDGAPS( NUDGAP )

*  Arguments Given and Returned:
      DOUBLE PRECISION LONINT
      DOUBLE PRECISION LATINT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BJ               ! B or J
      DOUBLE PRECISION EQU       ! Epoch of the reference equinox
      REAL FINT                  ! Fine interval
      CHARACTER*( IRA__SZSCS ) FULSCS
                                 ! The full sky coordinate name
      DOUBLE PRECISION INTUD     ! Interval in micro degrees
      DOUBLE PRECISION INTST     ! Interval in seconds of time
      DOUBLE PRECISION INTSA     ! Interval in seconds of time
      CHARACTER*( IRA__SZSCS ) SCSEQU
                                 ! The sky coordinate name with equinox

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the sky coordinate is Equatorial, the units of the longitude
*  will be HHMM.
      SCSEQU = SCS
      CALL IRA_GETEQ( SCSEQU, EQU, BJ, FULSCS, STATUS )
      IF ( FULSCS( : 10 ) .EQ. 'EQUATORIAL' ) THEN

*  Convert the longitude gap into the seconds of time.
         INTST = LONINT * IRA__R2TS

*  Find the nearest "fine gap".
         CALL IRA1_NRVAL( REAL( INTST ), NSTGAP, STGAPS, FINT, STATUS )

*  Convert the found gap into radians.
         LONINT = DBLE( FINT ) * IRA__TS2R

*  Ecliptic and galactic longitudes are in micro degrees.
      ELSE IF( FULSCS( : 8 ) .EQ. 'ECLIPTIC' .OR.
     :         FULSCS( : 8 ) .EQ. 'GALACTIC' ) THEN

*  Convert the longitude gap into micro degrees.
         INTUD = LONINT * IRA__RTOD * 1.0D6

*  Find the nearest "fine gap".
         CALL IRA1_NRVAL( REAL( INTUD ), NUDGAP, UDGAPS, FINT, STATUS )

*  Convert the found gap into radians.
         LONINT = DBLE( FINT ) * IRA__DTOR * 1.0D-6

*  For all other sky coordinates the units of longitude is DDMM, find
*  the nearest "fine gap".
      ELSE
         INTSA = LONINT * IRA__R2AS
         CALL IRA1_NRVAL( REAL( INTSA ), NSAGAP, SAGAPS, FINT, STATUS )
         LONINT = DBLE( FINT ) * IRA__AS2R
      END IF

*  Ecliptic and galactic latitudes are in micro degrees.
      IF( FULSCS( : 8 ) .EQ. 'ECLIPTIC' .OR.
     :    FULSCS( : 8 ) .EQ. 'GALACTIC' ) THEN

*  Convert the latitude gap into micro degrees.
         INTUD = LATINT * IRA__RTOD * 1.0D6

*  Find the nearest "fine gap".
         CALL IRA1_NRVAL( REAL( INTUD ), NUDGAP, UDGAPS, FINT, STATUS )

*  Convert the found gap into radians.
         LATINT = DBLE( FINT ) * IRA__DTOR * 1.0D-6

*  For all other coordinate systems, latitudes are express in DDMM,
*  find the nearest "fine gap".
      ELSE
         INTSA = LATINT * IRA__R2AS
         CALL IRA1_NRVAL( REAL( INTSA ), NSAGAP, SAGAPS, FINT, STATUS )
         LATINT = DBLE( FINT ) * IRA__AS2R

      END IF

      END
