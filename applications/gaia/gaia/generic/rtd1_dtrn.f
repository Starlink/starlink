      SUBROUTINE RTD1_DTRN( X1, Y1, X2, Y2, NVAL, IFIT, TR, RESID,
     :                      STATUS )
*+
*  Name:
*     RTD1_DTRN

*  Purpose:
*     Determines a linear transformation between two sets of positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RTD1_DTRN( X1, Y1, X2, Y2, NVAL, IFIT, TR, RESID, STATUS )

*  Description:
*     This routines sets up the sums for the normal equations suitable
*     for each type of fit. Then dependent on the either solves the
*     system trivially or calls the PDA library routine DGEFS to solve
*     the equations. If the fit is not succesfull then the number of
*     degrees of freedom are reduced and the fit is attempted again.

*  Arguments:
*     X1( NVAL ) = DOUBLE PRECISION (Given)
*        Set of X positions whose transformation is to be determined.
*     Y1( NVAL ) = DOUBLE PRECISION (Given)
*        Set of Y positions whose transformation is to be determined.
*     X2( NVAL ) = DOUBLE PRECISION (Given)
*        Set of reference X positions.
*     Y2( NVAL ) = DOUBLE PRECISION (Given)
*        Set of reference Y positions.
*     NVAL = INTEGER (Given)
*        Size of input arrays.
*     IFIT = INTEGER (Given and Returned)
*        A integer value which indicates which type of fit is required.
*        It's values represent transformations:
*          1 = shift of origin only
*          2 = shift of origin and rotation
*          3 = shift of origin and magnification
*          4 = shift of origin, rotation and magnification (solid body)
*          5 = full six parameter fit
*          6 = as 4, but no reflections
*        This value may be modified on return.
*     TR( 6 ) = DOUBLE PRECISION (Returned)
*        The six parameters which define the fit.
*     RESID = DOUBLE PRECISION (Returned)
*        Root mean square residual distance of the fit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The transformation used by this routine is
*          x1 = tr(1) + tr(2)*x2 + tr(3)*y2
*          y1 = tr(4) + tr(5)*x2 + tr(6)*y2
*
*     The equations are solved using the least squares form of these
*     when appropriate -- short cuts (when any of the tr's are known
*     and when it is necessary to restrict the fit) are taken as
*     necessary.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  Authors:
*     RFWS: Rodney Warren-Smith (Durham Polarimetry Group)
*     PWD: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     - (RFWS):
*        Original version.
*     10-JUL-1992 (PWD):
*        Changed to ADAM style, added new option (ifit=3).
*     17-SEP-1996 (PWD):
*        Changed to use PDA_DGEFS instead of NAG routine F04AEF.
*     03-JUL-1997 (PWD):
*        The PDA_DGEFS routine was not working for IFIT=3. Problem
*        recoded to work around this.
*     20-OCT-1997 (PWD):
*        Converted from CCD1_DTRN to RTD1_DTRN for use in GAIA.
*     13-JAN-1998 (PWD):
*        Converted to try a flip when fit type is 4.
*     21-JAN-1998 (PWD):
*        Added fit type 6 to be like old 4.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PRM_PAR'         ! Primitive data constants

*  Arguments Given:
      INTEGER NVAL
      DOUBLE PRECISION X1( NVAL )
      DOUBLE PRECISION Y1( NVAL )
      DOUBLE PRECISION X2( NVAL )
      DOUBLE PRECISION Y2( NVAL )

*  Arguments Given and Returned:
      INTEGER IFIT

*  Arguments Returned:
      DOUBLE PRECISION TR( 6 )
      DOUBLE PRECISION RESID

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER NSIG
      PARAMETER ( NSIG = 15 )   ! Number of significant figures in
                                ! linear equation solutions

*  Local Variables:
      DOUBLE PRECISION A( 4, 4 ) ! Workspace for sums and right
                                 ! and left-hand sides of normal
                                 ! equations.
      DOUBLE PRECISION B1( 4 )
      DOUBLE PRECISION B2( 4 )
      DOUBLE PRECISION BOT
      DOUBLE PRECISION OLDTR( 6 )
      DOUBLE PRECISION SGN
      DOUBLE PRECISION SIG( 2 )
      DOUBLE PRECISION SW
      DOUBLE PRECISION SWX
      DOUBLE PRECISION SWX2
      DOUBLE PRECISION SWXD
      DOUBLE PRECISION SWXXD
      DOUBLE PRECISION SWXXD0
      DOUBLE PRECISION SWXY
      DOUBLE PRECISION SWXYD
      DOUBLE PRECISION SWXYD0
      DOUBLE PRECISION SWY
      DOUBLE PRECISION SWY2
      DOUBLE PRECISION SWYD
      DOUBLE PRECISION SWYXD
      DOUBLE PRECISION SWYXD0
      DOUBLE PRECISION SWYYD
      DOUBLE PRECISION SWYYD0
      DOUBLE PRECISION THETA
      DOUBLE PRECISION TOP
      DOUBLE PRECISION W
      DOUBLE PRECISION WORK( 4 * (4 + 1 ) ) ! Workspace for PDA_DGEFS/DGEIR
      DOUBLE PRECISION WX
      DOUBLE PRECISION WY
      DOUBLE PRECISION X0
      DOUBLE PRECISION XD0
      DOUBLE PRECISION XMEAN
      DOUBLE PRECISION Y0
      DOUBLE PRECISION YD0
      DOUBLE PRECISION YMEAN
      INTEGER I
      INTEGER IND               ! Number of significant figures
      INTEGER ITASK             ! Whether to solve or reuse A
      INTEGER ITRY
      INTEGER IWORK( 4 )        ! Workspace for PDA_DGEFS
      INTEGER J

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the number of data points.
      SW = DBLE( NVAL )

*  Set type of fit required between 1 and 6.
      IFIT = MIN( MAX( 1, IFIT ), 6 )

*  Set default value for root mean square residual.
      RESID = 0.0D0

*  Check that the fit does not have too many degrees of freedom for
*  the number of data points available,
      IF ( NVAL .LE. 2 .AND. IFIT .EQ. 5 ) IFIT = 4
      IF ( NVAL .LE. 1 ) IFIT = 1

*  Initiallise sums for normal equations.
      SWX = 0.0D0
      SWY = 0.0D0
      SWXY = 0.0D0
      SWX2 = 0.0D0
      SWY2 = 0.0D0
      SWXD = 0.0D0
      SWYD = 0.0D0
      SWXXD = 0.0D0
      SWYYD = 0.0D0
      SWXYD = 0.0D0
      SWYXD = 0.0D0

*  Form sums.
      DO 20 I = 1, NVAL
         W = 1.0D0
         WX = W * X1( I )
         WY = W * Y1( I )
         SWX = SWX + WX
         SWY = SWY + WY
         SWXD = SWXD + W * X2( I )
         SWYD = SWYD + W * Y2( I )

*  If fit only requires a shift of origin, further sums are not
*  required.
         IF ( IFIT .NE. 1 ) THEN
            SWXY = SWXY + WX * Y1( I )
            SWX2 = SWX2 + WX * X1( I )
            SWY2 = SWY2 + WY * Y1( I )
            SWXXD = SWXXD + WX * X2( I )
            SWXYD = SWXYD + WX * Y2( I )
            SWYXD = SWYXD + WY * X2( I )
            SWYYD = SWYYD + WY * Y2( I )
         END IF
 20   CONTINUE

*  Iterate up to 5 times, reducing ifit by 1 each time.
      IFIT = IFIT + 1
      IND = NSIG
      DO 60 ITRY = 1, 5
         IFIT = IFIT - 1

*  Shift of origin only: equations simply solved
*  --------------------
         IF ( IFIT .EQ. 1 ) THEN
            TR( 1 ) = ( SWXD - SWX ) / SW
            TR( 2 ) = 1.0D0
            TR( 3 ) = 0.0D0
            TR( 4 ) = ( SWYD - SWY ) / SW
            TR( 5 ) = 0.0D0
            TR( 6 ) = 1.0D0

*  Shift of origin and rotation
*  ----------------------------
*  The method used to solve this type is to zero the relative axes
*  positions (using the x and y centroids) then from the transformation
*  equation.
*          (  cos  sin )
*          ( -sin  cos )
*  and
*          tan = sin / cos
*  together with the reduced normal equations, we derive an angle.

         ELSE IF ( IFIT .EQ. 2 ) THEN
*  Calculate the centroids of each set of positions.
            XD0 = SWXD / SW
            YD0 = SWYD / SW
            X0 = SWX / SW
            Y0 = SWY / SW

*  Initiallise storage for new sums.
            SWYXD0 = 0.0D0
            SWXYD0 = 0.0D0
            SWXXD0 = 0.0D0
            SWYYD0 = 0.0D0

*  Form new sums, using the deviations from the centroids.
            DO 146 I = 1, NVAL
               SWYXD0 = SWYXD0 + ( Y1( I ) - Y0 ) * ( X2( I ) - XD0 )
               SWXYD0 = SWXYD0 + ( X1( I ) - X0 ) * ( Y2( I ) - YD0 )
               SWXXD0 = SWXXD0 + ( X1( I ) - X0 ) * ( X2( I ) - XD0 )
               SWYYD0 = SWYYD0 + ( Y1( I ) - Y0 ) * ( Y2( I ) - YD0 )
 146        CONTINUE

*  If the rotation angle is not defined then indicate an error.
            TOP = SWYXD0 - SWXYD0
            BOT = SWYYD0 + SWXXD0
            IF ( TOP .EQ. 0.0D0 .AND. BOT .EQ. 0.0D0 ) THEN
               IND = -1
            ELSE

*  Otherwise calculate the rotation angle about the centroids
*  and assign the results to the transform coefficients.
               THETA = ATAN2( TOP, BOT )
               TR( 1 ) = XD0 - ( X0 * COS( THETA ) + Y0 * SIN( THETA ) )
               TR( 2 ) = COS( THETA )
               TR( 3 ) = SIN( THETA )
               TR( 4 ) = YD0 - ( -X0 * SIN( THETA ) +
     :                            Y0 * COS( THETA ) )
               TR( 5 ) = -SIN( THETA )
               TR( 6 ) = COS( THETA )
            END IF

*  Shift of origin and magnification.
*  ----------------------------------
*  Normal equations are: (rearranged to look like nxn)
*           a.n  + b.Sx                     = Sx'
*           a.Sx + b.Sx**2                  = Sx'.x
*                            f.Sy    + d.n  = Sy'
*                            f.Sy**2 + d.Sy = Sy'.y
*
*  For independent scaling along each axis, no cross terms (c=e=0)
*
         ELSE IF ( IFIT .EQ. 3 ) THEN
            A( 1, 1 ) = SW
            A( 1, 2 ) = SWX
            A( 1, 3 ) = 0.0D0
            A( 1, 4 ) = 0.0D0

            A( 2, 1 ) = SWX
            A( 2, 2 ) = SWX2
            A( 2, 3 ) = 0.0D0
            A( 2, 4 ) = 0.0D0

            A( 3, 1 ) = 0.0D0
            A( 3, 2 ) = 0.0D0
            A( 3, 3 ) = SWY
            A( 3, 4 ) = SW

            A( 4, 1 ) = 0.0D0
            A( 4, 2 ) = 0.0D0
            A( 4, 3 ) = SWY2
            A( 4, 4 ) = SWY

            B1( 1 ) = SWXD
            B1( 2 ) = SWXXD
            B1( 3 ) = SWYD
            B1( 4 ) = SWYYD

*  Solve linear normal equations.
            ITASK = 1
            IND = NSIG
            CALL PDA_DGEFS( A, 4, 4, B1, ITASK, IND, WORK, IWORK,
     :                      STATUS )

*  If successful, assign results to transformation coefficients.
            IF( IND .GT. 0 ) THEN
               TR( 1 ) = B1( 1 )
               TR( 2 ) = B1( 2 )
               TR( 3 ) = 0.0D0
               TR( 4 ) = B1( 4 )
               TR( 5 ) = 0.0D0
               TR( 6 ) = B1( 3 )
            END IF

*  Shift, rotation and magnification: set up normal equations.
*  ---------------------------------
*  We have b=-f and e=-c. Put this into full normal equations combine
*  the two sets and derive. Introduce a flip into X to allow a
*  reflection term on a second pass. The pass with the best fit is then
*  chosen.
         ELSE IF ( IFIT .EQ. 4 .OR. IFIT .EQ. 6 ) THEN
            DO 2 I = 1, 2
               SIG( I ) = VAL__MAXD
               IF ( I .EQ. 1 .OR. IFIT .EQ. 6 ) THEN
                  SGN = 1.0D0
               ELSE
                  SGN = -1.0D0
               END IF
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
               A( 3, 4 ) = -SWX
               A( 4, 1 ) = 0.0D0
               A( 4, 2 ) = SWY
               A( 4, 3 ) = -SWX
               A( 4 ,4 ) = SW
               B1( 1 ) = SGN * SWXD
               B1( 2 ) = SGN * SWXXD + SWYYD
               B1( 3 ) = SGN * SWYXD - SWXYD
               B1( 4 ) = SWYD

*  Solve linear normal equations.
               ITASK = 1
               IND = NSIG
               CALL PDA_DGEFS( A, 4, 4, B1, ITASK, IND, WORK, IWORK,
     :                         STATUS )

*  If successful, assign result to the transformation coefficients.
               IF ( IND .GT. 0 ) THEN
                  TR( 1 ) = SGN * B1( 1 )
                  TR( 2 ) = SGN * B1( 2 )
                  TR( 3 ) = B1( 3 )
                  TR( 4 ) = B1( 4 )
                  TR( 5 ) = SGN * ( -B1( 3 ) )
                  TR( 6 ) = B1( 2 )
                  IF ( I .EQ. 1 ) THEN
                     OLDTR( 1 ) = TR( 1 )
                     OLDTR( 2 ) = TR( 2 )
                     OLDTR( 3 ) = TR( 3 )
                     OLDTR( 4 ) = TR( 4 )
                     OLDTR( 5 ) = TR( 5 )
                     OLDTR( 6 ) = TR( 6 )
                  ENDIF
                  RESID = 0.0D0
                  DO 3 J = 1, NVAL
                     X0 = TR(1) + X1(J) * TR(2) + Y1(J) * TR(3)
                     Y0 = TR(4) + X1(J) * TR(5) + Y1(J) * TR(6)
                     XMEAN = X2( J ) - X0
                     YMEAN = Y2( J ) - Y0
                     RESID = RESID + XMEAN * XMEAN + YMEAN * YMEAN
 3                CONTINUE
                  SIG( I ) = SQRT( RESID / DBLE( NVAL ) )
               END IF
 2          CONTINUE
            IF ( SIG( I ) .NE. VAL__MAXD .OR.
     :           SIG( I ) .NE. VAL__MAXD ) THEN
               IF ( SIG( 1 ) .LT. SIG( 2 ) .OR. NVAL .EQ. 2 ) THEN
                  TR( 1 ) = OLDTR( 1 )
                  TR( 2 ) = OLDTR( 2 )
                  TR( 3 ) = OLDTR( 3 )
                  TR( 4 ) = OLDTR( 4 )
                  TR( 5 ) = OLDTR( 5 )
                  TR( 6 ) = OLDTR( 6 )
                  IF ( IND .EQ. 0 ) IND = 1
               END IF
            ELSE
               IND = 0
            END IF

*  Full fit required: set up normal equations.
*  -----------------
*  Normal equations are:
*
*        a.n  + b.Sx    + c.Sy    = Sx'
*        a.Sx + b.Sx**2 + c.Sxy   = Sx'.x
*        a.Sy + b.Sx.y  + c.SY**2 = Sx'.y
*        d.n  + e.Sx    + f.Sy    = Sy'
*        d.Sx + e.Sx**2 + f.Sx.y  = Sy'.x
*        d.Sy + e.Sx.y  + f.Sy**2 = Sy'.y

         ELSE IF ( IFIT .EQ. 5 ) THEN
            A( 1, 1 ) = SW
            A( 1, 2 ) = SWX
            A( 1, 3 ) = SWY
            A( 2, 1 ) = SWX
            A( 2, 2 ) = SWX2
            A( 2, 3 ) = SWXY
            A( 3, 1 ) = SWY
            A( 3, 2 ) = SWXY
            A( 3, 3 ) = SWY2
            B1( 1 ) = SWXD
            B1( 2 ) = SWXXD
            B1( 3 ) = SWYXD
            B2( 1 ) = SWYD
            B2( 2 ) = SWXYD
            B2( 3 ) = SWYYD

*  Solve linear normal equations.
            ITASK = 1
            IND = NSIG
            CALL PDA_DGEFS( A, 4, 3, B1, ITASK, IND, WORK, IWORK,
     :                      STATUS )
            IF ( IND .GT. 0 ) THEN
               ITASK = 2        ! Solve using existing factorisation of A
               IND = NSIG
               CALL PDA_DGEFS( A, 4, 3, B2, ITASK, IND, WORK, IWORK,
     :                         STATUS )

*  If successful, assign results to transformation coefficients.
               IF( IND .GT. 0 ) THEN
                  TR( 1 ) = B1( 1 )
                  TR( 2 ) = B1( 2 )
                  TR( 3 ) = B1( 3 )
                  TR( 4 ) = B2( 1 )
                  TR( 5 ) = B2( 2 )
                  TR( 6 ) = B2( 3 )
               END IF
            END IF
         END IF

*  If a fit was successfully obtained this time, exit from iteration
*  loop. otherwise try again with ifit reduced by 1.
         IF ( IND .GT. 0 ) GO TO 99
 60   CONTINUE

*  Exit here when have a fit or error
 99   CONTINUE
      IF ( STATUS .EQ. SAI__OK .AND. IND .GT. 0 ) THEN

*  Work out the fitting error (the root mean square distance).
         RESID = 0.0
         DO 1 I = 1, NVAL
            X0 = TR( 1 ) + X1( I ) * TR( 2 ) + Y1( I ) * TR( 3 )
            Y0 = TR( 4 ) + X1( I ) * TR( 5 ) + Y1( I ) * TR( 6 )
            WX = X2( I ) - X0
            WY = Y2( I ) - Y0
            RESID = RESID + WX * WX + WY * WY
 1       CONTINUE
         RESID = SQRT( RESID / DBLE( NVAL ) )
      END IF

      END
* $Id$
