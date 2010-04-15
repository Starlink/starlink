      SUBROUTINE IRA1_LIMIT( PROJ, NP, P, ACEN, BCEN, XAXIS, SIGN,
     :                       DTARG, DTOL, ZZTOL, ZZ, STATUS )
*+
*  Name:
*     IRA1_LIMIT

*  Purpose:
*     Find image coordinates which are a given arc-distance from a
*     given point, along either the X or Y image axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_LIMIT( PROJ, NP, P, ACEN, BCEN, XAXIS, SIGN, DTARG,
*                      DTOL, ZZTOL, ZZ, STATUS )

*  Description:
*     This routine finds pixels along either the X or Y axes, which are
*     a given arc-distance from the supplied point. Pixels vary in
*     size. Therefore it is not good enough to divide the required
*     arc-distance by the nominal pixel size to get the corresponding
*     number of pixels. Instead, a "binary chop" method is used in
*     which an interval containing the required coordinate is found,
*     and repreatedly bisected. This process continues until the
*     interval is less than the size supplied in argument ZZTOL. The
*     process also terminates if, by chance, one of the end points of
*     the interval corresponds to an arc-distance which is within DTOL
*     of the required arc-distance.

*  Arguments:
*     PROJ = CHARACTER * ( * ) (Given)
*        The full projection name. Abbreviations should not be given.
*     NP = INTEGER (Given)
*        The number of values supplied in argument P.
*     P( NP ) = DOUBLE PRECISION (Given)
*        Projection parameters.
*     ACEN = DOUBLE PRECISION (Given)
*        The longitude of the supplied point.
*     BCEN = DOUBLE PRECISION (Given)
*        The latitude of the supplied point.
*     XAXIS = LOGICAL (Given)
*        If true, then the displacement from the supplied point is
*        made  parallel to the X image axis. Otherwise, the displacement
*        is parallel to the Y image axis.
*     SIGN = INTEGER (Given)
*        Indicates if the displacement away from the supplied point is
*        to be in the positive or negative axis direction. SIGN should
*        be given the value +1 for positive displacements and -1 for
*        negative displacements.
*     DTARG = DOUBLE PRECISION (Given)
*        The required arc-length of the displacement away from the
*        supplied point, in radians.
*     DTOL = DOUBLE PRECISION (Given)
*        An immediate return is made if a point is found which is within
*        DTOL of the required arc-length of the displacement. DTOL
*        should be given in radians.
*     ZZTOL = DOUBLE PRECISION (Given)
*        The accuracy to which the image coordinate value corresponding
*        to the required displacement is required. In units of pixels.
*     ZZ = DOUBLE PRECISION (Returned)
*        If XAXIS is true, then the image coordinates (ZZ,0.0) is the
*        required arc-distance away from the supplied point.  If XAXIS
*        is false, then the image coordinates (0.0,ZZ) is the required
*        arc-distance away from the supplied point.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-MAY-1991 (DSB):
*        Original version.
*     24-FEB-1993 (DSB):
*        Argument ACEN and BCEN added.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! BAD values.
      INCLUDE 'IRA_ERR'          ! IRA error values.

*  Arguments Given:
      CHARACTER        PROJ*(*)
      INTEGER          NP
      DOUBLE PRECISION P( NP )
      DOUBLE PRECISION ACEN
      DOUBLE PRECISION BCEN
      LOGICAL          XAXIS
      INTEGER          SIGN
      DOUBLE PRECISION DTARG
      DOUBLE PRECISION DTOL
      DOUBLE PRECISION ZZTOL

*  Arguments Returned:
      DOUBLE PRECISION ZZ

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION
     :        A,                 ! Longitude of current position.
     :        A0,                ! Longitude of supplied point.
     :        B,                 ! Latitude of current position.
     :        COSB0,             ! COS of latitude of supplied point.
     :        D,                 ! Distance from current position to
     :                           ! supplied point.
     :        DLAST,             ! Last times value of D.
     :        SINB0,             ! SIN of latitude of supplied point.
     :        X0,                ! X coordinate at supplied point.
     :        Y0,                ! Y coordinate at supplied point.
     :        ZZHI,              ! Current upper bound on ZZ.
     :        ZZLO               ! Current lower bound on ZZ.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the pixel dimensions are not zero.
      IF( P(5)*P(6) .EQ. 0.0D0 ) THEN
         STATUS = IRA__SING
         CALL ERR_REP( 'IRA1_LIMIT_ERR1',
     :  'IRA1_LIMIT: Astrometry information specifies zero pixel size.',
     :                 STATUS )
         GO TO 999
      END IF

*  Store commonly used values.
      A0 = ACEN
      COSB0 = COS( BCEN )
      SINB0 = SIN( BCEN )

*  Find the image coordinates corresponding to the supplied point.
      CALL IRA1_IPRJ( 1, ACEN, BCEN, .FALSE., PROJ, NP, P, X0, Y0,
     :                STATUS )

*  Set initial guess assuming constant pixel size. ZZ is the distance
*  from the referrence pixel ( image coordinates (X0,Y0) ) to the
*  current axis position. ZZ is always positive. Argument SIGN
*  indicates which side of the supplied pixel the current axis
*  position is on, and argument XAXIS indicates if the X or Y axis is
*  to be used.
      IF( XAXIS ) THEN
         ZZ = DTARG/P(5)
      ELSE
         ZZ = DTARG/P(6)
      END IF

*  Set the lower limit on ZZ to zero.
      ZZLO = 0.0

*  Indicate that no upper limit yet exists by setting ZZHI negative.
      ZZHI = -1.0

*  Loop round until an upper limit is found.
      DLAST = -1.0

      DO WHILE( ZZHI .LT. 0 .AND. STATUS .EQ. SAI__OK )

*  Find the sky coordinates corresponding to the current position on the
*  X or Y axis.
         IF( XAXIS ) THEN
            CALL IRA1_IPRJ( 1, SIGN*ZZ + X0, Y0, .TRUE., PROJ, NP, P,
     :                      A, B, STATUS )

         ELSE
            CALL IRA1_IPRJ( 1, X0, SIGN*ZZ + Y0, .TRUE., PROJ, NP, P,
     :                      A, B, STATUS )

         END IF

         IF( STATUS .NE. SAI__OK ) GO TO 999

*  If the current axis position does not correspond to any point on
*  the sky, use the current ZZ value as the upper limit, and use half
*  the current ZZ value as the next guess.
         IF( A .EQ. VAL__BADD ) THEN
            ZZHI = ZZ
            ZZ = 0.5*ZZ

*  Otherwise...
         ELSE

*  Find the arc distance between the supplied point and the
*  point corresponding to the current position on the axis.
            D = ACOS( MAX( -1.0D0, MIN( 1.0D0,
     :                COSB0*COS( B )*COS( A - A0 ) + SINB0*SIN( B ) )
     :               ) )

*  If the last change in ZZ causes D to increase by less than 10%, the
*  process is not converging. If so, abort.
            IF( D .LT. 1.1*DLAST .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = IRA__NOCNV
               CALL ERR_REP( 'IRA1_LIMIT_ERR2',
     :           'IRA1_LIMIT: Requested image size is too big', STATUS )
               GO TO 999
            END IF

*  Save the current value of D for use in the next iteration.
            DLAST = D

*  If it so happens that the current axis position is at the correct
*  arc-distance from the supplied position (to within the given
*  tolerance), return immediately with this value.
            IF( ABS( D - DTARG ) .LE. DTOL ) THEN
               GO TO 998

*  Otherwise...
            ELSE

*  If the current axis position is beyond the required distance, use
*  it as the upper limit,and set the next guess to half the current ZZ
*  value.
               IF( D .GT. DTARG ) THEN
                  ZZHI = ZZ
                  ZZ = 0.5*ZZ

*  Otherwise, use the current position as the lower limit, and use twice
*  the current ZZ value for the next guess.
               ELSE
                  ZZLO = ZZ
                  ZZ = 2.0*ZZ

               END IF

            END IF

         END IF

      END DO

*  Now loop round refining the upper and lower limits, until the gap
*  between them is less than the given tolerance.
      DO WHILE( ZZHI - ZZLO .GT. ZZTOL .AND. STATUS .EQ. SAI__OK )

*  Find the sky coordinates corresponding to the current position on the
*  X or Y axis.
         IF( XAXIS ) THEN
            CALL IRA1_IPRJ( 1, SIGN*ZZ + X0, Y0, .TRUE., PROJ, NP, P,
     :                      A, B, STATUS )

         ELSE
            CALL IRA1_IPRJ( 1, X0, SIGN*ZZ + Y0, .TRUE., PROJ, NP, P,
     :                      A, B, STATUS )

         END IF

         IF( STATUS .NE. SAI__OK ) GO TO 999

*  If the current axis position does not correspond to any point on the
*  sky, reduce the upper limit to the current ZZ value, and use the
*  mean of the current ZZ value and the current lower limit as the next
*  guess.
         IF( A .EQ. VAL__BADD ) THEN
            ZZHI = ZZ
            ZZ = 0.5*( ZZ + ZZLO )

*  Otherwise...
         ELSE

*  Find the arc distance between the supplied point and the
*  point corresponding to the current position on the axis.
            D = ACOS( MAX( -1.0D0, MIN( 1.0D0,
     :                COSB0*COS( B )*COS( A - A0 ) + SINB0*SIN( B ) )
     :               ) )

*  If it so happens that the current axis position is at the correct
*  arc-distance from the supplied position (to within the given
*  tolerance), return immediately with this value.
            IF( ABS( D - DTARG ) .LE. DTOL ) THEN
               GO TO 998

*  Otherwise...
            ELSE

*  If the current axis position is beyond the required distance, reduce
*  the upper limit to the current ZZ value, and use the mean of the
*  current ZZ value and the current lower limit as the next guess.
               IF( D .GT. DTARG ) THEN
                  ZZHI = ZZ
                  ZZ = 0.5*( ZZ + ZZLO )

*  Otherwise, increase the lower limit to the current ZZ value, and use
*  the mean of the current ZZ value and the current upper limit as the
*  next guess.
               ELSE
                  ZZLO = ZZ
                  ZZ = 0.5*( ZZ + ZZHI )

               END IF

            END IF

         END IF

      END DO

*  Convert ZZ to an actual image coordinate value, rather than just an
*  offset from the supplied point.
 998  CONTINUE

      IF( XAXIS ) THEN
         ZZ = ZZ*SIGN + X0

      ELSE
         ZZ = ZZ*SIGN + Y0

      END IF

 999  CONTINUE

      END
