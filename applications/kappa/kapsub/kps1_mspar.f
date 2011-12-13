      SUBROUTINE KPS1_MSPAR( DIM1, DIM2, XAXIS, XMIN, XMAX, YAXIS,
     :                         YMIN, YMAX, NXPAR, NYPAR, MCHOEF, CHCOEF,
     :                         WORK, OUTARR, STATUS )
*+
*  Name:
*     KPS1_MSPAx

*  Purpose:
*     Evaluates a bivariate Chebyshev polynomial series for all elements
*     of a 2-dimensional data array, using x and y axis co-ordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL KPS1_MSPAx( DIM1, DIM2, XAXIS, XMIN, XMAX, YAXIS, YMIN,
*                       YMAX, NXPAR, NYPAR, MCHOEF, CHCOEF, WORK,
*                       OUTARR, STATUS )

*  Arguments:
*     DIM1 = INTEGER (Given)
*        The first dimension of the data array.
*     DIM2 = INTEGER (Given)
*        The second dimension of the data array.
*     XAXIS( DIM1 ) = DOUBLE PRECISION (Given)
*        X axis array, giving the x co-ordinate of the centre of each
*        column in the array.  Any columns in the array which are
*        outside the x-axis range XMIN to XMAX will be returned as
*        bad values.
*     XMIN = DOUBLE PRECISION (Given)
*        Lower end of the x range of the fit.
*     XMAX = DOUBLE PRECISION (Given)
*        Upper end of the x range of the fit.
*     YAXIS( DIM1 ) = DOUBLE PRECISION (Given)
*        Y axis array, giving the y co-ordinate of the centre of each
*        column in the array.  Any rows in the array which are outside
*        the y-axis range YMIN to YMAX will be returned as bad values.
*     YMIN = DOUBLE PRECISION (Given)
*        Lower end of the y range of the fit.
*     YMAX = DOUBLE PRECISION (Given)
*        Lower end of the y range of the fit.
*     NXPAR = INTEGER (Given)
*        The number of parameters of the FIT in the x direction, i.e.
*        the degree of the polynomial plus one.
*     NYPAR = INTEGER (Given)
*        The number of parameters of the FIT in the y direction, i.e.
*        the degree of the polynomial plus one.
*     MCHOEF = INTEGER (Given)
*        The dimension of the array of Chebyshev coefficients.
*     CHCOEF( MCHOEF ) = DOUBLE PRECISION (Given)
*        The Chebyshev polynomial coefficients, in the order increasing
*        x power for each increasing y power.  Thus coefficient Aij in
*        the standard convention is %CHCOEF(i*(%NYPAR)+j+1).  The array
*        may be rectangular, i.e. the highest x and y orders do not
*        have to be the same.
*     WORK( DIM1 ) = DOUBLE PRECISION (Returned)
*        Workspace for intermediate results.
*     OUTARR( DIM1, DIM2 ) = ? (Returned)
*        The fitted array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B or UB as appropriate.  The
*     OUTARR argument supplied to the routine must have the data type
*     specified.
*     -  Uses the magic-value method for bad or undefined pixels.

*  Algorithm:
*     -  Scan through the bins a line at a time.
*     -  If the Y co-ordinate is within the valid range then
*           Evaluate the Chebyshev surface for the line of pixels.
*        else
*           Fill the line with bad values
*        end if

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     Copyright (C) 1995-1997 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     SMB: Steven M. Beard (ROE)
*     {enter_new_authors_here}

*  History:
*     1990 Jan 30 (MJC):
*        Original version as PLY2EV.
*     1993 May 10 (SMB):
*        Converted to PLY2EA to allow the x-y co-ordinates of each
*        pixel to be different from its column and row number.  The
*        calculation of the RMS has been removed, because this routine
*        does not have an input array to compare.
*     1993 May 11 (SMB):
*        Fill any elements whose x or y co-ordinates are outside the
*        range where the polynomial is valid with bad values.
*     1995 August 3 (MJC):
*        Made generic.  Renamed from PLY2EA.  Used a modern-style
*        prologue and coding.
*     1996 October 8 (MJC):
*        Replaced NAG call.  Although this could have been made truly
*        generic and losing the WORK array, it would have generated
*        major changes to the calling application code and other
*        subroutines it calls.
*     1997 May 10 (MJC):
*        Renamed from KPS1_FSPAx.  Reordered code to improve efficiency.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad-pixel definitions

*  Arguments Given:
      INTEGER DIM1               ! First dimension of the data array
      INTEGER DIM2               ! Second dimension of the data array
      DOUBLE PRECISION XAXIS( DIM1 ) ! X-axis array
      DOUBLE PRECISION XMIN, XMAX ! X bounds of the polynomial fit
      DOUBLE PRECISION YAXIS( DIM1 ) ! Y-axis array
      DOUBLE PRECISION YMIN, YMAX ! Y bounds of the polynomial fit
      INTEGER NXPAR              ! X degree of the polynomial plus 1
      INTEGER NYPAR              ! Y degree of the polynomial plus 1
      INTEGER MCHOEF             ! Dimension of Chebyshev coeff. array
      DOUBLE PRECISION CHCOEF( MCHOEF ) ! Chebyshev coefficients

*  Arguments Returned:
      DOUBLE PRECISION WORK( DIM1 ) ! Workspace to store intermediate
                                 ! fitted values
      REAL OUTARR( DIM1, DIM2 ) ! Fitted data

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXPAR              ! Maximum number of parameters which
                                 ! can be handled in each direction
      PARAMETER ( MXPAR = 15 )

*  Local Variables:
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter
      INTEGER MFIRST             ! Lower limit of of valid columns
      INTEGER MLAST              ! Upper limit of valid columns
      DOUBLE PRECISION PX( MXPAR ) ! Work array

*  Internal References:
      REAL VAL_DTOR         ! Conversion from double precision

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the range of x-axis values which are within the valid
*  range.  It is assumed there is one contiguous run of good x-axis
*  values, with any out of range values occurring at the beginning or
*  end of the XAXIS array.  MFIRST and MLAST will denote the first and
*  last x values within the good run.
      I = 1
      DO WHILE ( ( ( XAXIS( I ) .LT. XMIN ) .OR.
     :             ( XAXIS( I ) .GT. XMAX ) ) .AND. ( I .LE. DIM1 ) )
         I = I + 1
      END DO
      MFIRST = I

      I = DIM1
      DO WHILE ( ( ( XAXIS( I ) .LT. XMIN ) .OR.
     :             ( XAXIS( I ) .GT. XMAX ) ) .AND. ( I .GE. 1 ) )
         I = I - 1
      END DO
      MLAST = I

*  Check there is a least one good value.
      IF ( MLAST .GE. MFIRST ) THEN

*  Scan through the bins a line at a time. Note for efficiency reasons
*  the fitting for a whole line is made in a single call.
         DO J = 1, DIM2

*  Check that the Y co-ordinate for this line is within the valid range.
            IF ( ( YAXIS( J ) .GE. YMIN ) .AND.
     :           ( YAXIS( J ) .LE. YMAX ) ) THEN

*  Evaluate the fitted surface for all pixels in the line between
*  columns MFIRST and MLAST.
               CALL KPG1_CHE2D( MLAST - MFIRST + 1, XMIN, XMAX,
     :                          XAXIS( MFIRST ), YMIN, YMAX, YAXIS( J ),
     :                          NXPAR - 1, NYPAR - 1, MCHOEF, CHCOEF,
     :                          MXPAR, PX, WORK, STATUS )

*  Return the polynomial values, setting the regions outside the run of
*  good pixels to bad values.  (Note that the first and third loops may
*  execute zero times if there are no bad pixels).
               DO I = 1, MFIRST - 1
                  OUTARR( I, J ) = VAL__BADR
               END DO

               DO I = MFIRST, MLAST
                  OUTARR( I, J ) = VAL_DTOR( .TRUE., WORK( I ),
     :                                         STATUS )
               END DO

               DO I = MLAST + 1, DIM1
                  OUTARR( I, J ) = VAL__BADR
               END DO
            ELSE

*  All x co-ordinates lie outside the range.  Fill the line with bad
*  values.
               DO I = 1, DIM1
                  OUTARR( I, J ) = VAL__BADR
               END DO
            END IF

*  End of the loops through the line of pixels.
         END DO

      ELSE

*  Y co-ordinate is the outside range.  Fill the line with bad values.
         DO J = 1, DIM2
            DO I = 1, DIM1
               OUTARR( I, J ) = VAL__BADR
            END DO
         END DO
      END IF

  999 CONTINUE

      END
