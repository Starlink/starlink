      SUBROUTINE KPS1_SUSF( NXKNOT, NYKNOT, XMIN, XMAX, YMIN, YMAX,
     :                      NKNOT, FIRST, NWS, NLWS, NIWS, MAXBIN,
     :                      X, Y, Z, W, BINW, NBIN, XKNOT, YKNOT,
     :                      WS, LWS, IWS, COEFF, NCOEF, SCALE, STATUS )

*+
*  Name:
*     KPS1_SUSF

*  Purpose:
*     Fits a bi-cubic spline surface by least squares to an input
*     two-dimensional array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_SUSF( NXKNOT, NYKNOT, XMIN, XMAX, YMIN, YMAX, NKNOT,
*                     FIRST, NWS, NLWS, NIWS, MAXBIN, X, Y, Z, W, BINW,
*                     NBIN, XKNOT, YKNOT, WS, LWS, IWS, COEFF, NCOEF,
*                     SCALE, STATUS )

*  Description:
*     This routine fits a bi-cubic spline surface for the given
*     vectors of values and weights at the given x-y co-ordinates, and
*     returns the coefficients of the fit.

*  Arguments:
*     NXKNOT = INTEGER (Given)
*        The number of interior knots in the x direction.
*     NYKNOT = INTEGER (Given)
*        The number of interior knots in the y direction.
*     XMIN = REAL (Given)
*        Minimum x co-ordinate of the data bins (left edge, not the
*        centroid).
*     YMIN = REAL (Given)
*        Minimum y co-ordinate of the data bins (bottom edge, not the
*        centroid).
*     XMAX = REAL (Given)
*        Maximum x co-ordinate of the data bins (right edge, not the
*        centroid).
*     YMAX = REAL (Given)
*        Maximum y co-ordinate of the data bins (top edge, not the
*        centroid).
*     NKNOT = INTEGER (Given)
*        The maximum number of knots along either axis.
*     FIRST = LOGICAL (Given)
*        If true the data values will be scaled.
*     NWS = INTEGER*8 (Given)
*        The dimension of the WS work space which must be at least
*        U*V*(2+B1+B2)+2*(U+V+4*(NBIN+NE)+NE-6)+B2+1  where
*        U = NXKNOT + 4, V = NYKNOT + 4, NE = MAX( NXKNOT, NYKNOT ) +
*        8.  Defining BX = 3*V+4, BY = 3*U+4, if (BX>=BY) then
*        B1 = BY and B2 = B1+V-3, otherwise B1 = BX and B2 = B1+U-3.
*     NLWS = INTEGER*8 (Given)
*        The dimension of the LWS work space which must be at least
*        U*V*(B2+1)+B2 where the variables are the same as those
*        defined in the description of NWS.
*     NIWS = INTEGER*8 (Given)
*        The dimension of the IWS work space which must be at least
*        NBIN + ( NXKNOT + 1 ) * ( NYKNOT + 1 ).
*     MAXBIN = INTEGER (Given)
*        The maximum dimension of the data, weight and co-ordinate
*        vectors.
*     X( MAXBIN ) = REAL (Given)
*        The x co-ordinates of the points to be evaluated.
*     Y( MAXBIN ) = REAL (Given)
*        The y co-ordinates of the points to be evaluated.
*     Z( MAXBIN ) = REAL (Given)
*        The values at the given x-y positions.
*     W( MAXBIN ) = REAL (Given and returned)
*        The weights at the given x-y positions.
*     BINW( MAXBIN ) = REAL (Given and returned)
*        Workspace.
*     NBIN = INTEGER*8 (Given and Returned)
*        The number of data points to be fitted by least-squares.
*     XKNOT( NKNOT ) = REAL (Returned)
*        The x positions of complete set of knots associated with x.
*     YKNOT( NKNOT ) = REAL (Returned)
*        The y positions of complete set of knots associated with y.
*     WS( NWS ) = REAL (Returned)
*        Work space.
*     LWS( NLWS ) = REAL (Returned)
*        Work space.
*     IWS( NIWS ) = INTEGER (Returned)
*        Work space.
*     COEFF( * ) = REAL (Returned)
*        The bi-cubic B-spline coefficients, defined at the knots in the
*        order increasing x knot, then increasing y.  Thus coefficient
*        Cij in the standard convention is %COEFF((i-1)*(%NYKNOT+8)+j).
*     NCOEF = INTEGER (Returned)
*        The number of bi-cubic coefficients.
*     SCALE = REAL (Returned)
*        The scale factor applied to the data values before calculating
*        the spline.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses the magic-value method for bad or undefined pixels.

*  Algorithm:
*     -  Calculate the number of panels produced by the knots and the
*        total number of spline coefficients.
*     -  Sort the x and y positions.
*     -  Find the maximum and minmum data values.
*     -  Scale the data by the range so that they lie between -1 and +1
*        to improve the performance of the fitting routines.
*     -  Add 2 more bins with zero weight at the bottom-left and
*        top-right corners of the image, to ensure that the spline fit
*        is valid over the whole image area.
*     -  Obtain the spline coefficients of the least-squares fit.
*     -  Should the fit be not unique then warn the user.

*  Copyright:
*     Copyright (C) 1990-1991 Science & Engineering Research Council.
*     Copyright (C) 1996-1997 Central Laboratory of the Research
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council.  All Rights Reserved.

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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 January 31 (MJC):
*        Original version based on some EDRS code.
*     1991 July 5 (MJC):
*        Passed NWS and WS as arguments.  Validates input number of
*        panels in case the maximum number of knots is greater in the
*        calling routine than allowed here, and added commentary on this
*        point.
*     1996 January 31 (MJC):
*        Replaced NAG sorting calls.
*     1996 October 14 (MJC):
*        Replaced remaining NAG so changed the d.p. types to real, and
*        passed different work arrays.  Renamed from SPL2D.
*     1997 July 30 (MJC):
*        Added LWS and NWS arguments.
*     2006 April 12 (MJC):
*        Remove unused variables.
*     20-FEB-2020 (DSB):
*        Support huge arrays.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad-pixel definitions
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Arguments Given:
      INTEGER NXKNOT             ! Number of interior knots in x dirn
      INTEGER NYKNOT             ! Number of interior knots in y dirn
      REAL XMIN, XMAX            ! X bounds of the fit
      REAL YMIN, YMAX            ! Y bounds of the fit
      INTEGER NKNOT              ! Dimension of XKNOT, YKNOT
      LOGICAL FIRST              ! Data values are to be scaled
      INTEGER*8 NWS              ! Dimension of workspace
      INTEGER*8 NLWS             ! Dimension of workspace
      INTEGER*8 NIWS             ! Dimension of workspace
      INTEGER MAXBIN             ! Dimension of the data vectors
      REAL X( MAXBIN )           ! X co-ordinates of the data
      REAL Y( MAXBIN )           ! Y co-ordinates of the data
      REAL Z( MAXBIN )           ! Data values

*  Arguments Given and Returned:
      REAL W( MAXBIN )           ! Data weights
      INTEGER*8 NBIN             ! Number of points for evaluation

*  Arguments Returned:
      REAL BINW( MAXBIN )        ! Workspace
      REAL XKNOT( NKNOT )        ! Positions of the knots in x
      REAL YKNOT( NKNOT )        ! Positions of the knots in y
      REAL WS( NWS )             ! Workspace
      REAL LWS( NIWS )           ! Workspace
      INTEGER IWS( NIWS )        ! Workspace
      REAL COEFF( * )            ! B-spline coefficients
      INTEGER NCOEF              ! Number of spline coefficients
      REAL SCALE                 ! Data scale factor

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL EXR( 2 )              ! Effective x limits
      REAL EYR( 2 )              ! Effective y limits
      INTEGER*8 I                ! Loop counter
      INTEGER IFAIL              ! PDA error status
      REAL MAXV                  ! Maximum data value
      REAL MINV                  ! Minimum data value
      REAL SIGMA                 ! Weighted sum of squares of residuals

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If there were insufficient bins containing valid data report th
*  error and exit.
      IF ( NBIN .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETK( 'NBIN', NBIN )
         CALL ERR_REP( 'KPS1_SUSF_INSFD',
     :     'KPS1_SUSF: Insufficient data --- ^NBIN bins.', STATUS )
         GO TO 999
       END IF

*  Copy the x positions into spare workspace and sort into ascending
*  order.
      DO I = 1, NBIN
         BINW( I ) = X( I )
      END DO

      CALL PDA8_QSAR( NBIN, BINW )

*  Set the interior x knots an equal number of data points apart in the
*  x direction.
      IF ( NXKNOT .GE. 1 )
     :  CALL KPS1_SUSKR( NBIN, BINW, NXKNOT, XKNOT( 5 ), STATUS )

*  Now repeat the above for the interior y knots.
      DO I = 1, NBIN
         BINW( I ) = Y( I )
      END DO

      CALL PDA8_QSAR( NBIN, BINW )

      IF ( NYKNOT .GE. 1 )
     :   CALL KPS1_SUSKR( NBIN, BINW, NYKNOT, YKNOT( 5 ), STATUS )

*  Exit in case something has gone wrong before we attempt to use
*  PDA_SURFIT.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

      IF ( FIRST ) THEN

*  Find maximum and minimum data values.
         MAXV = VAL__MINR
         MINV = VAL__MAXR
         DO I = 1, NBIN
            MAXV = MAX( Z( I ), MAXV )
            MINV = MIN( Z( I ), MINV )
         END DO

*  Derive the scale factor.  Watch for pathological case.  Setting the
*  scale factor to be negative will prevent evaluation routines from
*  re-scaling.
         IF ( MAXV - MINV .LT. 1.E-6 * ABS( MINV ) ) THEN
            SCALE = -1.0
         ELSE
            SCALE = 1. / REAL( MAXV - MINV )

*  Scale data values to lie in the range -1 to +1 to improve
*  performance of the fitting routines.
             DO I = 1, NBIN
               Z( I ) = Z( I ) * SCALE
            END DO
         END IF
      END IF

*  Add 2 more bins, with zero weight, at opposite corners of the image,
*  to ensure that the spline fit is valid over the entire image area.
      X( NBIN + 1 ) = XMIN - 1.0
      Y( NBIN + 1 ) = YMIN - 1.0
      Z( NBIN + 1 ) = 0.0
      W( NBIN + 1 ) = VAL__EPSR * W( NBIN )
      X( NBIN + 2 ) = XMAX + 1.0
      Y( NBIN + 2 ) = YMAX + 1.0
      Z( NBIN + 2 ) = 0.0
      W( NBIN + 2 ) = VAL__EPSR * W( NBIN )
      NBIN = NBIN + 2

*  The limits are slightly expanded.
      EXR( 1 ) = XMIN - 1.0
      EXR( 2 ) = XMAX + 1.0
      EYR( 1 ) = YMIN - 1.0
      EYR( 2 ) = YMAX + 1.0

*  Obtain the bi-cubic spline coefficients of the least-squares fit.
*  The -1 means that we supply the interior knots and a least-squares
*  fit is performed.
      CALL PDA8_SURFIT( -1, NBIN, X, Y, Z, W, EXR( 1 ), EXR( 2 ),
     :                  EYR( 1 ), EYR( 2 ), 3, 3, 0.0, NXKNOT + 8,
     :                  NYKNOT + 8, NKNOT, 4.* VAL__EPSR, NXKNOT + 8,
     :                  XKNOT, NYKNOT + 8, YKNOT, COEFF, SIGMA, WS,
     :                  NWS, LWS, NLWS, IWS, NIWS, IFAIL )

*  Check for an error.
      IF ( IFAIL .GT. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'IFAIL', IFAIL )
         CALL ERR_REP( 'KPS1_SUSF_PDA',
     :        'KPS1_SUSF: Error ^IFAIL returned by PDA_SURFIT fitting '/
     :        /'the spline surface.', STATUS )
         GO TO 999
      END IF

*  See if the fit was not unique.
      IF ( IFAIL .LT. -2 ) THEN
         CALL MSG_OUTIF( MSG__NORM, 'KPS1_SUSF_NUFIT1',
     :     'The bi-cubic spline surface is not uniquely defined '/
     :     /'by the data.', STATUS )
         CALL MSG_OUTIF( MSG__NORM,'KPS1_SUSF_NUFIT2',
     :     'Examine the output image to see whether or '/
     :     /'not this gives a reasonable solution.', STATUS )
      END IF

      NCOEF = ( NXKNOT + 4 ) * ( NYKNOT + 4 )

  999 CONTINUE

      END
