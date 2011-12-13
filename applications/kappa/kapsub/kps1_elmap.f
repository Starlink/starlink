      SUBROUTINE KPS1_ELMAP( SWAP, FRM, XIN, YIN, AXRIN, SIGIN, ANGIN,
     :                       MAP, XOUT, YOUT, AXROUT, SIGOUT, ANGOUT,
     :                       STATUS )
*+
*  Name:
*     KPS1_ELMAP

*  Purpose:
*     Transform an ellipse using a given Mapping.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_ELMAP( SWAP, FRM, XIN, YIN, AXRIN, SIGIN, ANGIN, MAP,
*                      XOUT, YOUT, AXROUT, SIGOUT, ANGOUT, STATUS )

*  Description:
*     The parameters of an ellipse are provided (centre, axis ratio,
*     minor axis length and major axis inclination), and the corresponding
*     properties of the ellipse are found after transformation using the
*     supplied Mapping. It is assumed that the Mapping is linear, and has
*     2 input axes and 2 output axes.

*  Arguments:
*     SWAP = LOGICAL (Given)
*        Is FRM a SkyFrame with swapped axes?
*     FRM = INTEGER (Given)
*        The Frame to which the outputs produced by MAP refer.
*     XIN = DOUBLE PRECISION (Given)
*        The supplied X position of the ellipse centre.
*     YIN = DOUBLE PRECISION (Given)
*        The supplied Y position of the ellipse centre.
*     AXRIN = REAL (Given)
*        The supplied axis ratio of the ellipse.
*     SIGIN = REAL (Given)
*        The supplied width of the minor axis of the ellipse.
*     ANGIN = REAL (Returned)
*        The supplied inclination of the major axis to the X axis in radians
*        (x through y positive).
*     MAP = INTEGER (Given)
*        The AST Mapping.
*     XOUT = DOUBLE PRECISION (Given)
*        The returned X position of the ellipse centre.
*     YOUT = DOUBLE PRECISION (Given)
*        The returned Y position of the ellipse centre.
*     AXROUT = REAL (Given)
*        The returned axis ratio of the ellipse.
*     SIGOUT = DOUBLE PRECISION (Given)
*        The returned width of the minor axis of the ellipse.
*     ANGOUT = REAL (Returned)
*        The orientation of the ellipse. For SkyFrames, this will be the
*        position angle of the major axis. For other Frames it is the
*        inclination of the major axis to the X axis in radians (x
*        through y positive). Returned in the range 0 - PI.
*     STATUS = INTEGER (Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      LOGICAL SWAP
      INTEGER FRM
      DOUBLE PRECISION XIN
      DOUBLE PRECISION YIN
      REAL AXRIN
      REAL SIGIN
      REAL ANGIN
      INTEGER MAP

*  Arguments Returned:
      DOUBLE PRECISION XOUT
      DOUBLE PRECISION YOUT
      REAL AXROUT
      DOUBLE PRECISION SIGOUT
      REAL ANGOUT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION SLA_DBEAR ! Bearing of one position from another

*  Local Variables:
      DOUBLE PRECISION PI
      PARAMETER ( PI = 3.1415926535898 )

*  Local Variables:
      DOUBLE PRECISION COSIN     ! Cosine of input angle
      DOUBLE PRECISION DX        ! Increment along first axis
      DOUBLE PRECISION DY        ! Increment along second axis
      DOUBLE PRECISION INPOS( 3, 2 ) ! Input positions
      DOUBLE PRECISION MAJAXS    ! Length of major axis
      DOUBLE PRECISION P1( 2 )   ! First point
      DOUBLE PRECISION P2( 2 )   ! Second point
      DOUBLE PRECISION OUTPOS( 3, 2 )! Mapped positions
      DOUBLE PRECISION SININ     ! Sine of input angle
      DOUBLE PRECISION T         ! Temporary storage for swapping
      INTEGER A                  ! Index of longitude axis
      INTEGER B                  ! Index of latitude axis
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the length of the major axis on input.
      IF( AXRIN .GT. 1.0 ) THEN
         MAJAXS = DBLE( SIGIN*AXRIN )

      ELSE IF( AXRIN .GT. 0.0 ) THEN
         MAJAXS = DBLE( SIGIN/AXRIN )

      ELSE
         MAJAXS = 0.0D0
      END IF

*  Store SIN and COS of the input major axis angle.
      SININ = SIN( DBLE( ANGIN ) )
      COSIN = COS( DBLE( ANGIN ) )

*  Store three input positions; 1) the centre, 2) one end of the minor
*  axis, 3) one end of the major axis.
      INPOS( 1, 1 ) = XIN
      INPOS( 1, 2 ) = YIN
      INPOS( 2, 1 ) = XIN - SININ*DBLE( SIGIN )
      INPOS( 2, 2 ) = YIN + COSIN*DBLE( SIGIN )
      INPOS( 3, 1 ) = XIN + COSIN*MAJAXS
      INPOS( 3, 2 ) = YIN + SININ*MAJAXS

*  Transform these using the supplied Mapping.
      CALL AST_TRANN( MAP, 3, 2, 3, INPOS, .TRUE., 2, 3, OUTPOS,
     :                STATUS )

*  Check all the mapped values are good. If any are bad, return bad values.
      IF( OUTPOS( 1, 1 ) .EQ. AST__BAD .OR.
     :    OUTPOS( 1, 2 ) .EQ. AST__BAD .OR.
     :    OUTPOS( 2, 1 ) .EQ. AST__BAD .OR.
     :    OUTPOS( 2, 2 ) .EQ. AST__BAD .OR.
     :    OUTPOS( 3, 1 ) .EQ. AST__BAD .OR.
     :    OUTPOS( 3, 2 ) .EQ. AST__BAD ) THEN
         XOUT = AST__BAD
         YOUT = AST__BAD
         AXROUT = VAL__BADR
         SIGOUT = AST__BAD
         ANGOUT = VAL__BADR

      ELSE

*  Return the mapped centre.
         XOUT = OUTPOS( 1, 1 )
         YOUT = OUTPOS( 1, 2 )

*  Find the arc-length of the mapped minor axis.
         P1( 1 ) = XOUT
         P1( 2 ) = YOUT
         P2( 1 ) = OUTPOS( 2, 1 )
         P2( 2 ) = OUTPOS( 2, 2 )
         SIGOUT = AST_DISTANCE( FRM, P1, P2, STATUS )

*  Find the arc-length of the mapped major axis.
         P2( 1 ) = OUTPOS( 3, 1 )
         P2( 2 ) = OUTPOS( 3, 2 )
         MAJAXS = AST_DISTANCE( FRM, P1, P2, STATUS )

*  The Mapping may have resulted in the mapped "minor" axis being longer
*  than the mapped "major" axis. In this case we swap the roles of the
*  major and minor axes.
         IF( SIGOUT .GT. MAJAXS ) THEN
            T = SIGOUT
            SIGOUT = MAJAXS
            MAJAXS = T

            OUTPOS( 3, 1 ) = OUTPOS( 2, 1 )
            OUTPOS( 3, 2 ) = OUTPOS( 2, 2 )
            OUTPOS( 2, 1 ) = P2( 1 )
            OUTPOS( 2, 2 ) = P2( 2 )
         END IF

*  Find the axis ratio.
         IF( SIGOUT .NE. 0.0 ) THEN
            AXROUT = REAL( MAJAXS/SIGOUT )
         ELSE
            AXROUT = 0.0
         END IF

*  To find the bearing of the major axis, we should really have an AST
*  function "AST_BEAR", similar to SLALIB function SLA_BEAR, which
*  takes account of the geometry of the Frame. Until then we use
*  techniques for the two common cases of simple Eucleidean and
*  spherical geometry. First deal wil spherical geometry.
         IF( AST_ISASKYFRAME( FRM, STATUS ) ) THEN

*  A default SkyFrame has axis 1=longitude, axis 2= latitude. Reverse
*  these if the axis order is swapped in the SkyFrame.
            IF( .NOT. SWAP ) THEN
               A = 1
               B = 2
            ELSE
               A = 2
               B = 1
            END IF

*  Find the position angle of the end of the major axis as seen from the
*  centre position.
            ANGOUT = REAL( SLA_DBEAR( OUTPOS( 1, A ), OUTPOS( 1, B ),
     :                                OUTPOS( 3, A ), OUTPOS( 3, B ) ) )

*  If this is not a skyframe, just use euclidean geometry.
         ELSE
            DY = OUTPOS( 3, 2 ) - YOUT
            DX = OUTPOS( 3, 1 ) - XOUT
            IF( DX .NE. 0.0 .OR. DY .NE. 0.0 ) THEN
               ANGOUT = REAL( ATAN2( DY, DX ) )
            ELSE
               ANGOUT = 0.0
            END IF
         END IF

*  Return ANGOUT in the range 0 - PI.
         IF( ANGOUT .LT. 0 ) THEN
            ANGOUT = ANGOUT + PI

         ELSE IF( ANGOUT .GT. PI ) THEN
            ANGOUT = ANGOUT - PI

         END IF

      END IF

      END
