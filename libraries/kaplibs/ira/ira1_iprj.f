      SUBROUTINE IRA1_IPRJ( NVAL, IN1, IN2, FORWRD, TPROJ, NP, P,
     :                        OUT1, OUT2, STATUS )
*+
*  Name:
*     IRA1_IPRJ

*  Purpose:
*     Apply a specified projection to supplied coordinate data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_IPRJ( NVAL, IN1, IN2, FORWRD, TPROJ, NP, P, OUT1, OUT2,
*                    STATUS )

*  Description:
*     Does the work for IRA_PROJ without argument verification. The
*     transformation equations transform between "local" coordinates and
*     (U,V) coordinates. The "local" coordinate system is a
*     longitude/latitude spherical coordinate system similar to the sky
*     coordinate systems, but positioned so that the requested reference
*     point ( as given by projection parameters P(1) and P(2) ) has
*     local longitude and latitude values of zero. The local coordinate
*     system is also arranged so that local "north" (i.e. increasing
*     local latitude) is rotated by the angle specified by p(8) from
*     sky coordinate north. The (U,V) system is a cartesian coordinate
*     system on the projection surface similar to the image (X,Y)
*     coordinate system. The differences are that the U and V axes are
*     always parallel to local north and west, the (U,V) origin is
*     placed at the reference point, and U and V are measured in radians
*     rather than pixels.

*  Arguments:
*     NVAL = INTEGER (Given)
*        The number of coordinate points to be transformed.
*     IN1( NVAL ) = DOUBLE PRECISION (Given)
*        If FORWRD is true, then IN1 holds values of the first image
*        coordinate (X), otherwise IN1 holds values of the sky
*        longitude.
*     IN2( NVAL ) = DOUBLE PRECISION (Given)
*        If FORWRD is true, then IN2 holds values of the second image
*        coordinate (Y), otherwise IN2 holds values of the sky
*        latitude.
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
*     OUT1( NVAL ) = DOUBLE PRECISION (Returned)
*        If FORWRD is true, then OUT1 holds values of the sky longitude
*        corresponding to the image coordinates given in arrays IN1 and
*        IN2. Otherwise, OUT1 holds values of the first image
*        coordinate (X) corresponding to the input sky coordinates. If
*        either the corresponding IN1 or IN2 value is equal to the
*        Starlink "BAD" value (VAL__BADD), then OUT1 is set bad.
*     OUT2( NVAL ) = DOUBLE PRECISION (Returned)
*        If FORWRD is true, then OUT2 holds values of the sky latitude
*        corresponding to the image coordinates given in arrays IN1 and
*        IN2. Otherwise, OUT2 holds values of the second image
*        coordinate corresponding to the input sky coordinates. If
*        either the corresponding IN1 or IN2 value is equal to the
*        Starlink "BAD" value (VAL__BADD), then OUT2 is set bad.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*

*  History:
*     7-MAY-1991 (DSB):
*        Original version.
*     10-FEB-1992 (DSB):
*        Use absolute pixel sizes.
*     11-SEP-1992 (DSB):
*        p(8) added.
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
      DOUBLE PRECISION IN1( NVAL )
      DOUBLE PRECISION IN2( NVAL )
      LOGICAL          FORWRD
      CHARACTER        TPROJ*(*)
      INTEGER          NP
      DOUBLE PRECISION P( NP )

*  Arguments Returned:
      DOUBLE PRECISION OUT1( NVAL )
      DOUBLE PRECISION OUT2( NVAL )

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
         CALL ERR_REP( 'IRA1_IPRJ_ERR1',
     :                'IRA1_IPRJ: Requested ^PR projection is singular',
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
               CALL SLA_DCC2S( VLOC, OUT1( I ), OUT2( I ) )

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
               OUT1( I ) = XNORM*COSROT - YNORM*SINROT
               OUT2( I ) = YNORM*COSROT + XNORM*SINROT

            END IF
         END DO

      END IF

*  Call the appropriate routine to transform between local coordinates
*  and (U,V) coordinates.
      IF( TPROJ .EQ. 'GNOMONIC' ) THEN
         CALL IRA1_GNOM( FORWRD, NVAL, OUT1, OUT2, STATUS )

      ELSE IF( TPROJ .EQ. 'LAMBERT' ) THEN
         CALL IRA1_LAMB( FORWRD, NVAL, OUT1, OUT2, STATUS )

      ELSE IF( TPROJ .EQ. 'AITOFF' ) THEN
         CALL IRA1_AITO( FORWRD, NVAL, OUT1, OUT2, STATUS )

      ELSE IF( TPROJ .EQ. 'ORTHOGRAPHIC' ) THEN
         CALL IRA1_ORTH( FORWRD, NVAL, OUT1, OUT2, STATUS )

*  If the projection is not supported, give a message.
      ELSE
         STATUS = IRA__BADPR
         CALL MSG_SETC( 'PROJ', TPROJ )
         CALL ERR_REP( 'IRA1_IPRJ_ERR2',
     :                 'IRA1_IPRJ: Projection ^PROJ not yet supported',
     :                  STATUS )

      END IF

*  If an error was reported, abort.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If performing an inverse projection (from sky to image coordinates),
*  convert all the (U,V) coordinates just calculated, to image (X,Y)
*  coordinates.
      IF( .NOT. FORWRD ) THEN

*  Calculate COS and SIN of the rotation applied to image co-ordinates
*  which converts them to (U,V) coordinates.
         COSROT = COS( P( 7 ) - P( 8 ) )
         SINROT = SIN( P( 7 ) - P( 8 ) )

*  Loop round each pair of good (U,V) coordinates.
         DO I = 1, NVAL
            IF( OUT1( I ) .NE. VAL__BADD ) THEN

*  Rotate the (U,V) coordinates to get normalised image coordinates
*  (origin at the reference point and measured in radians instead of
*  pixels).
               XNORM = OUT1( I )*COSROT + OUT2( I )*SINROT
               YNORM = OUT2( I )*COSROT - OUT1( I )*SINROT

*  Scale and shift the normalised image coordinates to get true image
*  coordinates.
               OUT1( I ) =  XNORM/ABS( P(5) ) + P(3)
               OUT2( I ) =  YNORM/ABS( P(6) ) + P(4)

            END IF
         END DO

*  Otherwise, convert the local coordinates just calculated, to sky
*  coordinates (using the inverse of the rotation matrix).
      ELSE

         DO I = 1, NVAL

            IF( OUT1( I ) .NE. VAL__BADD ) THEN

               CALL SLA_DCS2C( OUT1( I ), OUT2( I ), VLOC )
               CALL SLA_DIMXV( RMAT, VLOC, VSKY )
               CALL SLA_DCC2S( VSKY, OUT1( I ), OUT2( I ) )

            END IF

         END DO

      END IF

 999  CONTINUE

      END
