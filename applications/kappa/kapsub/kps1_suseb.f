      SUBROUTINE KPS1_SUSEB( X, Y, Z, NBIN, NXKNOT, NYKNOT, XKNOT,
     :                       YKNOT, NCOEF, COEFF, SCALE, FIT, RESID,
     :                       RMS, STATUS )

*+
*  Name:
*     KPS1_SUSEB

*  Purpose:
*     Evaluates a bi-cubic spline for a set of x-y positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_SUSEB( X, Y, Z, NBIN, NXKNOT, NYKNOT, XKNOT, YKNOT,
*                      NCOEF, COEFF, SCALE, FIT, RESID, RMS, STATUS )

*  Description:
*     This routine evaluates a bi-cubic spline (B-spline represention)
*     given vectors of x-y co-ordinates, and returns the fit and the
*     residuals.  The rms difference between the fitted and the original
*     values is also calculated.

*  Arguments:
*     X( * ) = REAL (Given)
*        The x co-ordinates of the points to be evaluated.
*     Y( * ) = REAL (Given)
*        The y co-ordinates of the points to be evaluated.
*     Z( * ) = REAL (Given)
*        The values at the given x-y positions before fitting.
*     NBIN = INTEGER (Given)
*        The number of points at which the spline is to be
*        evaluated.
*     NXKNOT = INTEGER (Given)
*        The number of interior knots in the x direction.
*     NYKNOT = INTEGER (Given)
*        The number of interior knots in the y direction.
*     XKNOT( NXKNOT+8 ) = REAL (Given)
*        The x positions of complete set of knots associated with x.
*     YKNOT( NYKNOT+8 ) = REAL (Given)
*        The y positions of complete set of knots associated with y.
*     NCOEF = INTEGER (Given)
*        The number of bi-cubic coefficients.  Must equal (%NXKNOT+4) *
*        (%NYKNOT+4).
*     COEFF( NCOEF ) = REAL (Given)
*        The bi-cubic B-spline coefficients, defined at the knots in the
*        order increasing x knot, then increasing y.  Thus coefficient
*        Cij in the standard convention is %COEFF((i-1)*(%NYKNOT+8)+j).
*     SCALE = REAL (Given)
*        The scale factor applied to the data values before calculating
*        the spline. The inverse of this value is used to correctly
*        scale the fitted values. If it is negative there will be no
*        rescaling.
*     FIT( * ) = REAL (Returned)
*        The fitted array.
*     RESID( * ) = REAL (Returned)
*        The residuals of the fit.
*     RMS = REAL (Returned)
*        The rms difference between the raw array and the
*        fitted array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses the magic-value method for bad or undefined pixels.

*  Algorithm:
*     -  Calculate the number of panels produced by the knots.
*     -  Initialise the rms sums.
*     -  Scan through the pixels.
*     -  Evaluate the spline surface at the pixel and scale if
*        requested.
*     -  Form residuals and sums for the rms error of the fit.
*     -  At the end of the loop calculate the rms error.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 January 30 (MJC):
*        Original version.
*     1996 October 14 (MJC):
*        Replaced remaining NAG so changed the d.p. types to real, and
*        no longer needed POINT and NPOINT arguments.  Renamed from
*        SPL2EB.  Converted to modern style.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad-pixel definitions

*  Arguments Given:
      REAL X( * )                ! X co-ordinates of the data
      REAL Y( * )                ! Y co-ordinates of the data
      REAL Z( * )                ! Data values
      INTEGER NBIN               ! Number of points for evaluation
      INTEGER NXKNOT             ! Total number of interior knots in x
      INTEGER NYKNOT             ! Total number of interior knots in y
      REAL XKNOT( NXKNOT + 8 )   ! Positions of the knots in x
      REAL YKNOT( NYKNOT + 8 )   ! Positions of the knots in y
      INTEGER NCOEF              ! Number of spline coefficients
      REAL COEFF( NCOEF )        ! B-spline coefficients
      REAL SCALE                 ! Data scale factor

*  Arguments Returned:
      REAL FIT( * )              ! Fitted data
      REAL RESID( * )            ! Residuals
      REAL RMS                   ! RMS difference between the fitted and
                                 ! unfitted arrays

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter
      INTEGER IFAIL              ! PDA error status
      INTEGER IWORK( 2 )         ! Work array
      INTEGER NPT                ! Number of points used to calculate
                                 ! the rms
      REAL SUMSQ                 ! Sum of the square of differences
      LOGICAL SCALED             ! Data are scaled
      REAL WORK( 8 )             ! Work array

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      SCALED = SCALE .GT. 0.0

*  Initialise sums to form the rms error of the fit.
      SUMSQ = 0.0
      NPT = 0

*  Scan through the pixels.
      DO I = 1, NBIN

*  Evaluate the bi-cubic spline at the binned pixel centre.  Since
*  the number of points is one, the dimensions of the work arrays are
*  known and can be declared explicitly in this routine.
         CALL PDA_BISPEV( XKNOT, NXKNOT + 8, YKNOT, NYKNOT + 8, COEFF,
     :                    3, 3, X( I ), 1, Y( I ), 1, FIT( I ), WORK, 8,
     :                    IWORK, 2, IFAIL )

*  Test for an error.
         IF ( IFAIL .NE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'IFAIL', IFAIL )
            CALL ERR_REP( 'KPS1_SUSEB_PDA',
     :        'KPS1_SUSEB: Error ^IFAIL returned by PDA_BISPEV '/
     :        /'evaluation.', STATUS )
            GO TO 999
         END IF

*  Form residuals and sums for the rms error of the fit.
         IF ( Z( I ) .NE. VAL__BADR ) THEN
            RESID( I ) = FIT( I ) - Z( I )
            SUMSQ = SUMSQ + RESID( I ) * RESID( I )
            NPT = NPT + 1
            IF ( SCALED ) THEN
               FIT( I ) = FIT( I ) / SCALE
               RESID( I ) = RESID( I ) / SCALE
            END IF
         ELSE
            FIT( I ) = VAL__BADR
            RESID( I ) = VAL__BADR
         END IF

*  End of the loop through the pixels.
      END DO

*  Calculate the rms error of the fit.
      IF ( NPT .GE. 1 ) THEN
         IF ( SCALED ) THEN
            RMS = SQRT( SUMSQ / REAL( NPT ) ) / SCALE
         ELSE
            RMS = SQRT( SUMSQ / REAL( NPT ) )
         END IF
      ELSE
         RMS = VAL__BADR
      END IF

  999 CONTINUE

      END
