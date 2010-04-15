      SUBROUTINE IRC1_BPSSB( IDC, NVAL, SAMPLE, DETIND, SCS, RA, DEC,
     :                       ANGLE, SPEED, STATUS )
*+
*  Name:
*     IRC1_BPSSB

*  Purpose:
*     Returns boresight positions at a set of detector samples,
*     assuming SURVEY_BSIGHT data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_BPSSB( IDC, NVAL, SAMPLE, DETIND, SCS, RA, DEC, ANGLE,
*                      SPEED, STATUS )

*  Description:
*     Boresight positions are found by first fitting straight lines
*     to the solar longitude and the clock angle (PSI) samples stored in
*     the DETAILS structure contained in the CRDD file. These fits are
*     evaluated at the times of the required samples. Cone angle (THETA)
*     is assumed constant throughout the scan. See figure III.B.7
*     in the IRAS Catalogues and Atlases Explanatory Supplement for
*     definitions of THETA and PSI. (Note, this routine uses PSI, not
*     PHI, as the clock angle. PSI = 2.PI - PHI ). The clock angle, cone
*     angle and solar longitude values are then converted to the
*     corresponding RA and DEC values (B1950, FK4).
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
*     NVAL = INTEGER (Given)
*        The number of samples in the input and output lists.
*     SAMPLE( NVAL ) = REAL (Given)
*        A list of fractional sample numbers. If any sample numebr is
*        BAD, then the corresponding elements of the output arrays are
*        set BAD.
*     DETIND( NVAL ) = INTEGER (Given)
*        A list of detector indices.
*     SCS = CHARACTER * ( * ) (Returned)
*        The Sky Coordinate System in which the boresight positions and
*        angles are returned. See the IRA_ documentation (ID/1) for more
*        information about Sky Coordinate Systems. The variable supplied
*        for argument SCS should have a declared length equal to the
*        symbolic constant IRA__SZSCS. This routine returns
*        SCS = EQUATORIAL(B1950).
*     RA( NVAL ) = DOUBLE PRECISION (Returned)
*        An array holding the RA value  (B1950, FK4) of the
*        boresight at the moment each sample specified in the input
*        lists was taken (radians).
*     DEC( NVAL ) = DOUBLE PRECISION (Returned)
*        An array holding the DEC value (B1950, FK4) of the
*        boresight at the moment each sample specified in the input
*        lists was taken (radians).
*     ANGLE( NVAL ) = DOUBLE PRECISION (Returned)
*        The scan angle at the boresight. This is measured from
*        equatorial north to the positive focal plane Y axis.  The
*        angle is in radians and is measured positive in the same sense
*        as rotation from north to east.
*     SPEED( NVAL ) = REAL (Returned)
*        The scan speed in radians per second. Positive values imply
*        that sources move in the positive focal plane Y direction (i.e
*        in the "with-survey" direction). Negative values imply that
*        sources move in the negative Y direction (i.e. "anti-survey").
*        This routine returns the value stored in the CRDD_INFO
*        structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     DCP: Diana Parsons (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     15-MAR-1991 (DSB):
*        Original version.
*     9-MAY-1991 (DSB):
*        Updated for IRA version 2.
*     25-AUG-1993 (DSB):
*        Changed to use IRC1_SCOSB rather than calculating PSI, THETA
*        and SOLONG itself.
*     23-JUN-1993 (DCP):
*        Add error check after IRC1_SCOSB, the program otherwise goes into
*        SLA routines which are not error checked and may produce
*        misleading messages.
*     {enter_further_changes_here}

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
*        CCM_NOMSP( IRC__MAX ) = REAL (Read)
*           Nominal scan speed.
*        CCM_CAGRD( IRC__MAX ) = REAL (Read)
*           The rate of change of clock angle (PSI) with time, in
*           radians per second (assumed constant).
*        CCM_RMAT( 9, IRC__MAX ) = DOUBLE PRECISION (Read)
*           Rotation matrices which rotate equatorial (1950) coordinates
*           expressed in Cartesian form, to ecliptic coordinates (of
*           date), also expressed in Cartesian form.

*  Arguments Given:
      INTEGER IDC
      INTEGER NVAL
      REAL    SAMPLE( NVAL )
      INTEGER DETIND( NVAL )

*  Arguments Returned:
      CHARACTER SCS*(*)
      DOUBLE PRECISION RA( NVAL )
      DOUBLE PRECISION DEC( NVAL )
      DOUBLE PRECISION ANGLE( NVAL )
      REAL    SPEED( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION BLAT      ! Latitude of boresight using an axis
                                 ! pointing at the sun.
      DOUBLE PRECISION BLONG     ! Longitude of boresight about an axis
                                 ! pointing at the sun.
      DOUBLE PRECISION COSSOL    ! Cos of solar longitude.
      DOUBLE PRECISION DECDOT    ! Rate of change of DEC, in radians per
                                 ! second.
      INTEGER IVAL               ! Sample counter.
      DOUBLE PRECISION NEWX      ! Rotated X value.
      DOUBLE PRECISION NEWY      ! Rotated Y value.
      DOUBLE PRECISION POSVEL(6) ! Position and velocity of boresight in
                                 ! Cartesian coordinates.
      REAL PSI                   ! Clock angle at the time of the
                                 ! sample, in radians.
      DOUBLE PRECISION RADIUS    ! "Radius" of celestial sphere. Should
                                 ! be unity.
      DOUBLE PRECISION RADOT     ! Rate of change of RA, in radians per
                                 ! second.
      DOUBLE PRECISION RDSDOT    ! Rate of change of "radius" of
                                 ! celestial sphere. Should be zero.
      DOUBLE PRECISION SINSOL    ! Sin of solar longitude.
      REAL SOLONG                ! Solar longitude at the time of the
                                 ! sample (in radians), in ecliptic
                                 ! coordinates of date.
      DOUBLE PRECISION TEMP      ! Temporary storage.
      REAL THETA                 ! Cone angle at the time of the
                                 ! sample, in radians.
      DOUBLE PRECISION UTCS      ! UTCS at the current sample.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the value of returned argument SCS.
      SCS = 'EQUATORIAL(B1950)'

*  Loop round each required position.
      DO IVAL = 1, NVAL

*  If the sample number is BAD, set all the output values BAD, and pass
*  on to the next position.
         IF( SAMPLE( IVAL ) .EQ. VAL__BADR ) THEN
            RA( IVAL ) = VAL__BADD
            DEC( IVAL ) = VAL__BADD
            ANGLE( IVAL ) = VAL__BADD
            SPEED( IVAL ) = VAL__BADR

*  Get the satellite coordinates and solar longitude at this sample.
         ELSE
            CALL IRC1_SCOSB( IDC, SAMPLE( IVAL ), PSI, THETA, SOLONG,
     :                       UTCS, STATUS )

*  Check for error, as SLA routines do not have error checking and may
*  give misleading error message
            IF ( STATUS .NE. SAI__OK) GO TO 999

*  Clock angle and cone angle can be treated like a longitude/latitude
*  system in which the "north pole" is in the direction of the sun.
*  Generate these longitude and latitude values (NB, the clock angle
*  used here, PSI, *decreases* with time)
            BLONG = IRA__PIBY2 + DBLE( PSI )
            BLAT = IRA__PIBY2 - DBLE( THETA )

*  Convert the boresight position and velocity from sherical coordinates
*  to cartesian coordinates in which X is 90 degrees east of the sun in
*  the plane of ecliptic, Y is towards the north ecliptic pole, and Z is
*  towards the sun. This assumes that the sun always lies exactly in the
*  mean ecliptic (i.e. solar latitude is exactly zero). The boresight
*  velocity is needed to calculate the scan direction, and in spherical
*  coordinates is given by the rate of change of longitude and latitude
*  with respect to time, i.e. CCM_CAGRD and zero respectively.
*  POSVEL(1) to POSVEL(3) hold X,Y and Z components of the boresight
*  position, POSVEL(4) to POSVEL(6) hold X,Y and Z  components of the
*  boresight velocity.
            CALL SLA_DS2C6( BLONG, BLAT, 1.0D0,
     :                      DBLE( CCM_CAGRD( IDC ) ), 0.0D0, 0.0D0,
     :                      POSVEL )

*  Swap the Cartesian axes around so that X is towards the sun, Y is 90
*  degrees east of the sun in the ecliptic, and Z is towards the north
*  ecliptic pole.
            TEMP = POSVEL(1)
            POSVEL(1) = POSVEL(3)
            POSVEL(3) = POSVEL(2)
            POSVEL(2) = TEMP

            TEMP = POSVEL(4)
            POSVEL(4) = POSVEL(6)
            POSVEL(6) = POSVEL(5)
            POSVEL(5) = TEMP

*  Rotate the XY plane (the ecliptic) so that the X axis is in the
*  direction of the mean equinox of date. This gives the standard
*  Cartesian coordinate system for representing ecliptic coordinates.
            COSSOL = COS( DBLE( SOLONG ) )
            SINSOL = SIN( DBLE( SOLONG ) )

            NEWX = POSVEL(1)*COSSOL - POSVEL(2)*SINSOL
            NEWY = POSVEL(1)*SINSOL + POSVEL(2)*COSSOL
            POSVEL(1) = NEWX
            POSVEL(2) = NEWY

            NEWX = POSVEL(4)*COSSOL - POSVEL(5)*SINSOL
            NEWY = POSVEL(4)*SINSOL + POSVEL(5)*COSSOL
            POSVEL(4) = NEWX
            POSVEL(5) = NEWY

*  Use the matrix stored in common by IRC1_PNTSB to rotate these vectors
*  from a Cartesian coordinate system based on the ecliptic and equinox
*  of date, to a Cartesian coordinate system based on the equator and
*  equinox of Besselian epoch 1950. The FK4 equatorial system is used.
            CALL SLA_DIMXV( CCM_RMAT( 1, IDC ), POSVEL(1), POSVEL(1) )
            CALL SLA_DIMXV( CCM_RMAT( 1, IDC ), POSVEL(4), POSVEL(4) )

*  Convert the Cartesian coordinates back to spherical coordinates. This
*  gives the required RA and DEC, and rate of change of RA and DEC with
*  respect to time.
            CALL SLA_DC62S( POSVEL, RA( IVAL ), DEC( IVAL ), RADIUS,
     :                      RADOT, DECDOT, RDSDOT )

*  Calculate the scan angle and store it.
            ANGLE( IVAL ) = ATAN2( -RADOT*COS( DEC( IVAL ) ), -DECDOT )

*  Return the nominal scan speed read from the CRDD file. In the case
*  of survey boresight data this "norminal" speed is in fact universally
*  applicable.
            SPEED( IVAL ) = CCM_NOMSP( IDC )

         END IF

*  Do the next required position.
      END DO

 999  CONTINUE

      END
