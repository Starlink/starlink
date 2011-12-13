      SUBROUTINE KPG1_LINTD( N, XA, YA, XB, YB, IFIT, C, MAXERR, RMSERR,
     :                       STATUS )
*+
*  Name:
*     KPG1_LINTD

*  Purpose:
*     Obtains a linear transformation between two sets of x,y positions
*     with least squared error.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_LINTD( N, XA, YA, XB, YB, IFIT, C, MAXERR, RMSERR,
*                      STATUS )

*  Description:
*     The co-efficients of a linear transformation are returned which
*     maps the (XA,YA) positions to the corresponding (XB,YB) positions
*     with least squares error. The type of fit can be specified as:
*
*       o  A shift of origin only (IFIT 1)
*
*       o  A shift and rotation (IFIT = 2)
*
*       o  A shift, rotation and magnification (IFIT = 3)
*
*       o  A shift, rotation, magnification and shear (IFIT = 4)
*
*     If the value of IFIT is too high for the supplied data, a lower
*     value will be used and returned in IFIT. The returned coefficients
*     are such that:
*
*       Fitted XB position = C(1) + C(2)*XA + C(3)*YA
*
*       Fitted YB position = C(4) + C(5)*XA + C(6)*YA

*  Arguments:
*     N = INTEGER (Given)
*        The number of supplied positions.
*     XA( N ) = DOUBLE PRECISION (Given)
*        The X  co-ordinates at the first set of positions.
*     YA( N ) = DOUBLE PRECISION (Given)
*        The Y  co-ordinates at the first set of positions.
*     XB( N ) = DOUBLE PRECISION (Given)
*        The X  co-ordinates at the second set of positions.
*     YB( N ) = DOUBLE PRECISION (Given)
*        The Y  co-ordinates at the second set of positions.
*     IFIT = INTEGER (Given and Returned)
*        The type of fit required. A lower value will be used (and
*        returned) if a fit of the specified type could not be
*        obtained.
*     C( 6 ) = DOUBLE PRECISION (Returned)
*        The coefficients of the linear fit.
*     MAXERR = DOUBLE PRECISION (Returned)
*        The maximum error between the supplied (XB,YB) positions and
*        the fitted (XB,YB) positions, in pixels.
*     RMSERR = DOUBLE PRECISION (Returned)
*        The RMS error between the supplied (XB,YB) positions and
*        the fitted (XB,YB) positions, in pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1998 (DSB):
*        Original version, based on the LINTDN subroutine from the EDRS
*        package, written by R.F. Warren-Smith.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION XA( N )
      DOUBLE PRECISION YA( N )
      DOUBLE PRECISION XB( N )
      DOUBLE PRECISION YB( N )

*  Arguments Given and Returned:
      INTEGER IFIT

*  Arguments Returned:
      DOUBLE PRECISION C( 6 )
      DOUBLE PRECISION MAXERR
      DOUBLE PRECISION RMSERR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION
     :        A(4,4),
     :        B(4,2),
     :        WK1(4)

      INTEGER
     :        I,
     :        IFAIL,
     :        WK2(4)

      LOGICAL
     :        AGAIN

      DOUBLE PRECISION
     :        SW,
     :        SWX,
     :        SWY,
     :        SWXY,
     :        SWX2,
     :        SWY2,
     :        SWXD,
     :        SWYD,
     :        SWXXD,
     :        SWYYD,
     :        SWXYD,
     :        SWYXD,
     :        W,
     :        WX,
     :        WY,
     :        XD0

      DOUBLE PRECISION
     :        YD0,
     :        X0,
     :        Y0,
     :        SWYXD0,
     :        SWXYD0,
     :        SWXXD0,
     :        SWYYD0,
     :        TOP,
     :        BOT,
     :        THETA,
     :        MAXSQR,
     :        SSQRES,
     :        XXA,
     :        YYA

      DOUBLE PRECISION
     :        XXB,
     :        YYB,
     :        XBFIT,
     :        YBFIT,
     :        SQRES

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check validity of arguments
      IF( N .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_LINTD_ERR1', 'KPG1_LINTD: No positions '//
     :                 'supplied (programming error)', STATUS )
         GO TO 999
      END IF

*  Find the total number of valid positions.
      SW = 0.0
      DO I = 1, N

         IF( XA( I ) .NE. AST__BAD .AND. YA( I ) .NE. AST__BAD .AND.
     :       XB( I ) .NE. AST__BAD .AND. YB( I ) .NE. AST__BAD ) THEN
            SW = SW + 1.0
         END IF

      END DO

*  Report an error if all positions are invalid.
*  Check validity of arguments
      IF( SW .LE. 0.0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_LINTD_ERR2', 'KPG1_LINTD: No valid '//
     :                 'positions supplied', STATUS )
         GO TO 999
      END IF

*  Ok...set type of fit required between 1 and 4
      IFIT = MIN( MAX( 1, IFIT ), 4 )

*  Check that the fit does not have too many degrees of freedom for the
*  number of data points available.
      IF( NINT( SW ) .LE. 2 ) IFIT = MIN( IFIT, 3 )
      IF( NINT( SW ) .LE. 1 ) IFIT = 1

*  Initiallise sums for normal equations
      SWX = 0.0
      SWY = 0.0
      SWXY = 0.0
      SWX2 = 0.0
      SWY2 = 0.0
      SWXD = 0.0
      SWYD = 0.0
      SWXXD = 0.0
      SWYYD = 0.0
      SWXYD = 0.0
      SWYXD = 0.0

*  Form sums, setting weight to zero for invalid positions
      DO I = 1, N

         IF( XA( I ) .NE. AST__BAD .AND. YA( I ) .NE. AST__BAD .AND.
     :       XB( I ) .NE. AST__BAD .AND. YB( I ) .NE. AST__BAD ) THEN
            W = 1.0

         ELSE
            W = 0.0
         END IF

         WX = W*XA( I )
         WY = W*YA( I )
         SWX = SWX + WX
         SWY = SWY + WY
         SWXD = SWXD + W*XB( I )
         SWYD = SWYD + W*YB( I )


*  If fit only requires a shift of origin, further sums are not
*  required
         IF( IFIT .NE. 1 ) THEN
            SWXY = SWXY + WX*YA( I )
            SWX2 = SWX2 + WX*XA( I )
            SWY2 = SWY2 + WY*YA( I )
            SWXXD = SWXXD + WX*XB( I )
            SWXYD = SWXYD + WX*YB( I )
            SWYXD = SWYXD + WY*XB( I )
            SWYYD = SWYYD + WY*YB( I )
         END IF

      END DO

*  Iterate up to 4 times, reducing IFIT by 1 each time
      IFIT = IFIT + 1
      IFAIL = 0
      AGAIN = .TRUE.

      DO WHILE( AGAIN )
         IFIT = IFIT - 1

*  Shift of origin only: equations simply solved
*  ---------------------------------------------
         IF( IFIT .EQ. 1 ) THEN
            C( 1 ) = ( SWXD - SWX )/SW
            C( 2 ) = 1.0
            C( 3 ) = 0.0
            C( 4 ) = ( SWYD - SWY )/SW
            C( 5 ) = 0.0
            C( 6 ) = 1.0
            AGAIN = .FALSE.

*  Shift of origin and rotation
*  ----------------------------
         ELSE IF( IFIT .EQ. 2 ) THEN

*  Calculate the centroids of each set of positions
            XD0 = SWXD/SW
            YD0 = SWYD/SW
            X0 = SWX/SW
            Y0 = SWY/SW

*  Initiallise storage for new sums
            SWYXD0 = 0.0
            SWXYD0 = 0.0
            SWXXD0 = 0.0
            SWYYD0 = 0.0

*  Form new sums, using the deviations from the centroids
            DO I = 1, N

               IF( XA( I ) .NE. AST__BAD .AND.
     :             YA( I ) .NE. AST__BAD .AND.
     :             XB( I ) .NE. AST__BAD .AND.
     :             YB( I ) .NE. AST__BAD ) THEN

                  SWYXD0 = SWYXD0 + (YA( I ) - Y0 )*(XB( I ) - XD0 )
                  SWXYD0 = SWXYD0 + (XA( I ) - X0 )*(YB( I ) - YD0 )
                  SWXXD0 = SWXXD0 + (XA( I ) - X0 )*(XB( I ) - XD0 )
                  SWYYD0 = SWYYD0 + (YA( I ) - Y0 )*(YB( I ) - YD0 )

               END IF

            END DO

*  If the rotation angle is defined...
            TOP = SWYXD0 - SWXYD0
            BOT = SWYYD0 + SWXXD0
            IF( TOP .NE. 0.0 .OR. BOT .NE. 0.0 ) THEN

*  ...calculate the rotation angle about the centroids and assign the
*  results to the transform coefficients
               THETA = ATAN2( TOP, BOT )
               C( 1 ) = XD0 - (X0*COS( THETA ) + Y0*SIN( THETA ) )
               C( 2 ) = COS( THETA )
               C( 3 ) = SIN( THETA )
               C( 4 ) = YD0 - ( - X0*SIN( THETA ) + Y0*COS( THETA ) )
               C( 5 ) =  - SIN( THETA )
               C( 6 ) = COS( THETA )
               AGAIN = .FALSE.

            END IF

*  Shift, rotation and magnification: set up normal equations
*  ----------------------------------------------------------
         ELSE IF( IFIT .EQ. 3 ) THEN

            A( 1, 1 ) = SW
            A( 1, 2 ) = SWX
            A( 1, 3 ) = SWY
            A( 1, 4 ) = 0.0D0
            A( 2, 1 ) = SWX
            A( 2, 2 ) = SWX2 + SWY2
            A( 2, 3 ) = 0.0D0
            A( 2, 4 ) = SWY
            A( 3, 1 ) = SWY
            A( 3, 2 ) = 0.0D0
            A( 3, 3 ) = SWX2 + SWY2
            A( 3, 4 ) =  - SWX
            A( 4, 1 ) = 0.0D0
            A( 4, 2 ) = SWY
            A( 4, 3 ) =  - SWX
            A( 4, 4 ) = SW
            B( 1, 1 ) = SWXD
            B( 2, 1 ) = SWXXD + SWYYD
            B( 3, 1 ) = SWYXD - SWXYD
            B( 4, 1 ) = SWYD

*  Solve the linear normal equations
            CALL PDA_DGEFS( A, 4, 4, B( 1, 1 ), 1, IFAIL, WK1, WK2,
     :                      STATUS )

*  If successful, assign result to the transformation coefficients
            IF( STATUS .EQ. SAI__OK ) THEN
               C( 1 ) = B( 1, 1 )
               C( 2 ) = B( 2, 1 )
               C( 3 ) = B( 3, 1 )
               C( 4 ) = B( 4, 1 )
               C( 5 ) =  -B( 3, 1 )
               C( 6 ) = B( 2, 1 )
               AGAIN = .FALSE.

            ELSE
               CALL ERR_ANNUL( STATUS )
            END IF

*  Full fit required: set up normal equations
*  ------------------------------------------
         ELSE IF( IFIT .EQ. 4 ) THEN
            A( 1, 1 ) = SW
            A( 1, 2 ) = SWX
            A( 1, 3 ) = SWY
            A( 2, 1 ) = SWX
            A( 2, 2 ) = SWX2
            A( 2, 3 ) = SWXY
            A( 3, 1 ) = SWY
            A( 3, 2 ) = SWXY
            A( 3, 3 ) = SWY2
            B( 1, 1 ) = SWXD
            B( 2, 1 ) = SWXXD
            B( 3, 1 ) = SWYXD
            B( 1, 2 ) = SWYD
            B( 2, 2 ) = SWXYD
            B( 3, 2 ) = SWYYD

*  Solve linear normal equations
            CALL PDA_DGEFS( A, 4, 3, B( 1, 1 ), 1, IFAIL, WK1, WK2,
     :                      STATUS )
            CALL PDA_DGEFS( A, 4, 3, B( 1, 2 ), 2, IFAIL, WK1, WK2,
     :                      STATUS )

*  If successful,  assign results to transformation coefficients
            IF( STATUS .EQ. SAI__OK ) THEN
               C( 1 ) = B( 1, 1 )
               C( 2 ) = B( 2, 1 )
               C( 3 ) = B( 3, 1 )
               C( 4 ) = B( 1, 2 )
               C( 5 ) = B( 2, 2 )
               C( 6 ) = B( 3, 2 )
               AGAIN = .FALSE.

            ELSE
               CALL ERR_ANNUL( STATUS )
            END IF

         END IF

*  If a fit was successfully obtained this time,  exit from iteration
*  loop. otherwise try again with IFIT reduced by 1.
      END DO

*  Find the sum of the squared residuals and the maximum residual
*  between the supplied and estimated (XB,YB) positions.
      MAXSQR = VAL__MINR
      SSQRES = 0.0

      DO I = 1, N
         XXA = XA( I )
         YYA = YA( I )
         XXB = XB( I )
         YYB = YB( I )

         IF( XXA .NE. AST__BAD .AND. YYA .NE. AST__BAD .AND.
     :       XXB .NE. AST__BAD .AND. YYB .NE. AST__BAD ) THEN

            XBFIT = C( 1 ) + C( 2 )*XXA + C( 3 )*YYA
            YBFIT = C( 4 ) + C( 5 )*XXA + C( 6 )*YYA

            SQRES = ( XXB - XBFIT )**2 + (YYB - YBFIT )**2

            SSQRES = SSQRES + SQRES
            MAXSQR = MAX( MAXSQR, SQRES )

         END IF

      END DO

*  Calculate the returned statistics.
      MAXERR = SQRT( MAXSQR )
      RMSERR = SQRT( SSQRES/SW )

 999  CONTINUE

      END
