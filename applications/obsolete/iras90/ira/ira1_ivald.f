      SUBROUTINE IRA1_IVALD( NVAL, FORWRD, TPROJ, NP, P, IN1, IN2, OK,
     :                       STATUS )
*+
*  Name:
*     IRA1_IVALD

*  Purpose:
*     Check for valid coordinate data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_IVALD( NVAL, FORWRD, TPROJ, NP, P, IN1, IN2, OK,
*                      STATUS )

*  Description:
*     Does the work for IRA1_VALID if the projection is known. The
*     transformation equations transform between "local" coordinates
*     and (U,V) coordinates. The "local" coordinate system is a
*     longitude/latitude spherical coordinate system similar to the sky
*     coordinate systems, but positioned so that the requested
*     reference point ( as given by projection parameters P(1) and P(2))
*     has local longitude and latitude values of zero. The local
*     coordinate system is also arranged so that local "north" (i.e.
*     increasing local latitude) is rotated by the angle specified by
*     p(8) from sky coordinate north. The (U,V) system is a cartesian
*     coordinate system on the projection surface similar to the image
*     (X,Y) coordinate system. The differences are that the U and V axes
*     are always parallel to local north and west, the (U,V) origin is
*     placed at the reference point, and U and V are measured in radians
*     rather than pixels.

*  Arguments:
*     NVAL = INTEGER (Given)
*        The number of coordinate points to be transformed.
*     FORWRD = LOGICAL (Given)
*        If true then the forward mapping of the projection is used
*        from image coordinate to sky coordinate. Otherwise, the
*        inverse mapping from sky coordinate to image coordinates is
*        used.
*     TPROJ = CHARACTER * ( * ) (Given)
*        The full name of the projection to use.
*     NP = INTEGER (Given)
*        The size of the array P.
*     P( NP ) = DOUBLE PRECISION (Given)
*        The values to use for the projection parameters. Assumed to be
*        good (i.e. not equal to VAL__BADD). The absolute values of P(5)
*        and P(6) are used.
*     IN1( NVAL ) = DOUBLE PRECISION (Given and Returned)
*        If FORWRD is true, then IN1 holds values of the first image
*        coordinate (X) on entry and the corresponding U coordinates on
*        exit. Otherwise IN1 holds values of the sky longitude on entry
*        and the corresponding local longitude on exit.
*     IN2( NVAL ) = DOUBLE PRECISION (Given and returned)
*        If FORWRD is true, then IN2 holds values of the second image
*        coordinate (Y) on entry and the corresponding V coordinates on
*        exit. Otherwise IN2 holds values of the sky latitude on entry
*        and the corresponding local latitude on exit.
*     OK( NVAL ) = LOGICAL (Returned)
*        Each element of OK is true if the result of transforming the
*        corresponding input coordinates would result in valid
*        transformed coordinates.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*

*  History:
*     20-FEB-1992 (DSB):
*        Original version.
*     11-SEP-1992 (DSB):
*        P(8) added.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA error values.

*  Arguments Given:
      INTEGER          NVAL
      LOGICAL          FORWRD
      CHARACTER        TPROJ*(*)
      INTEGER          NP
      DOUBLE PRECISION P( NP )

*  Arguments Given and Returned:
      DOUBLE PRECISION IN1( NVAL )
      DOUBLE PRECISION IN2( NVAL )

*  Arguments Returned:
      LOGICAL OK( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER          I         ! Loop count.
      DOUBLE PRECISION COSROT    ! Cos of the rotation from (U,V) to
                                 ! image coordinates.
      DOUBLE PRECISION RMAT(3,3) ! Matrix which rotates sky coordinates
                                 ! to local coordinates.
      DOUBLE PRECISION SINROT    ! Sin of the rotation from (U,V) to
                                 ! image coordinates.
      DOUBLE PRECISION VLOC(3)   ! A 3-vector giving the Cartesain
                                 ! form of a local coordinate position.
      DOUBLE PRECISION VSKY(3)   ! A 3-vector giving the Cartesain
                                 ! form of a sky coordinate position.
      DOUBLE PRECISION XNORM     ! Normalised X image coordinate value.
      DOUBLE PRECISION YNORM     ! Normalised Y image coordinate value.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the requested pixel area is not zero.
      IF( P(5)*P(6) .EQ. 0.0D0 ) THEN
         STATUS = IRA__SING
         CALL MSG_SETC( 'PR', TPROJ )
         CALL ERR_REP( 'IRA1_IVALD_ERR1',
     :               'IRA1_IVALD: Requested ^PR projection is singular',
     :                 STATUS )
         GO TO 999
      END IF

*  Set up a rotation matrix which will convert the supplied sky
*  coordinates to a longitude/latitude system in which the requested
*  reference point has a longitude and latitude of zero. This
*  coordinate system is referred to here as the "local" coordinate
*  system. The matrix also includes the required rotation (about a
*  radius through the reference point) of the celestial sphere so that
*  "north" in the local coordinate system is at the position angle
*  specified by projection parameter P(8).
      CALL SLA_DEULER( 'ZYX', P(1), -P(2), -P(8), RMAT )

*  If performing an inverse projection (from sky to image coordinates),
*  convert all the supplied sky coordinates, to local coordinates. This
*  is done by converting the input coordinates to Cartesian form,
*  applying the rotation matrix just evaluated, and then converting
*  back to spherical coordinates.
      IF( .NOT. FORWRD ) THEN

         DO I = 1, NVAL

            IF( IN1( I ) .NE. VAL__BADD .AND.
     :          IN2( I ) .NE. VAL__BADD ) THEN

               CALL SLA_DCS2C( IN1( I ), IN2( I ), VSKY )
               CALL SLA_DMXV( RMAT, VSKY, VLOC )
               CALL SLA_DCC2S( VLOC, IN1( I ), IN2( I ) )

            END IF

         END DO

*  Otherwise, convert the input image coordinates to (U,V) coordinates.
*  (U,V) coordinates are like image (X,Y) coordinates, except that they
*  are in units of radians rather than pixels, they are relative to
*  an origin at the reference point, and they are rotated by an angle
*  which puts the Y axis at the position angle specified by p(7). The
*  rotation of the celestial sphere specified by p(8) will result in the
*  V axis being at a position angle of p(8) in sky coordinates,
*  therefore, rotating the image coordinates by an angle of P(7)-P(8)
*  with respect to (U,V) will put the Y axis at a position angle of
*  P(7). U is stored in OUT1 and V is stored in OUT2.
      ELSE

*  Calculate COS and SIN of the rotation to be applied to the image
*  co-ordinates to convert them to (U,V) coordinates.
         COSROT = COS( P( 7 ) - P( 8 ) )
         SINROT = SIN( P( 7 ) - P( 8 ) )

*  Loop round each pair of good image coordinates.
         DO I = 1, NVAL
            IF( IN1( I ) .NE. VAL__BADD .AND.
     :          IN2( I ) .NE. VAL__BADD ) THEN

*  Calculate the normalised image coordinates (origin at the reference
*  point and measured in radians instead of pixels).
               XNORM = ABS( P(5) )*( IN1( I ) - P(3) )
               YNORM = ABS( P(6) )*( IN2( I ) - P(4) )

*  Apply the rotation to get (U,V) coordinates.
               IN1( I ) = XNORM*COSROT - YNORM*SINROT
               IN2( I ) = YNORM*COSROT + XNORM*SINROT

            END IF
         END DO

      END IF

*  Call the appropriate routine to check the transformation between
*  local coordinates and (U,V) coordinates.
      IF( TPROJ .EQ. 'GNOMONIC' ) THEN
         CALL IRA1_VGNOM( FORWRD, NVAL, IN1, IN2, OK, STATUS )

      ELSE IF( TPROJ .EQ. 'LAMBERT' ) THEN
         CALL IRA1_VLAMB( FORWRD, NVAL, IN1, IN2, OK, STATUS )

      ELSE IF( TPROJ .EQ. 'AITOFF' ) THEN
         CALL IRA1_VAITO( FORWRD, NVAL, IN1, IN2, OK, STATUS )

      ELSE IF( TPROJ .EQ. 'ORTHOGRAPHIC' ) THEN
         CALL IRA1_VORTH( FORWRD, NVAL, IN1, IN2, OK, STATUS )

*  If the projection is not supported, give a message.
      ELSE
         STATUS = IRA__BADPR
         CALL MSG_SETC( 'PROJ', TPROJ )
         CALL ERR_REP( 'IRA1_IVALD_ERR2',
     :                 'IRA1_IVALD: Projection ^PROJ not yet supported',
     :                  STATUS )

      END IF

 999  CONTINUE

      END
