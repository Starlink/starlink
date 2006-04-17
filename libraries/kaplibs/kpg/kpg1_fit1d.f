      SUBROUTINE KPG1_FIT1D( LBND, UBND, Y, X, M, C, RMS, STATUS )
*+
*  Name:
*     KPG1_FIT1D

*  Purpose:
*     Fit a least squares straight line to supplied data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_FIT1D( LBND, UBND, Y, X, M, C, RMS, STATUS )

*  Description:
*     A straight line is fitted to the data supplied in X and Y, using
*     the least squares criterion. The returned values of M and C are
*     the gradient and intercept of the fit, so that y = M.x + C. The
*     RMS residual of the Y data from the fit is returned in RMS.
*
*     An error is reported if there are less than two good data values
*     in Y, or if the X values cover a range of zero.

*  Arguments:
*     LBND = INTEGER (Given)
*        The lower bound of the X and Y arrays.
*     UBND = INTEGER (Given)
*        The upper bound of the X and Y arrays.
*     Y( LBND : UBND ) = DOUBLE PRECISION (Given)
*        The Y data values. Any bad values are ignored.
*     X( LBND : UBND ) = DOUBLE PRECISION (Given)
*        The X positions corresponding to each Y data value.
*     M = DOUBLE PRECISION (Returned)
*        The gradient.
*     C = DOUBLE PRECISION (Returned)
*        The intercept.
*     RMS = DOUBLE PRECISION (Returned)
*        The RMS residual between the Y values and the fit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-OCT-1992 (DSB):
*        Original version (IRM_FIT1D).
*     7-DEC-1998 (DSB):
*        Brought into KAPPA from IRAS90.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants

*  Arguments Given:
      INTEGER LBND
      INTEGER UBND
      DOUBLE PRECISION Y( LBND : UBND )
      DOUBLE PRECISION X( LBND : UBND )

*  Arguments Returned:
      DOUBLE PRECISION M
      DOUBLE PRECISION C
      DOUBLE PRECISION RMS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION DENOM     ! Denominator
      DOUBLE PRECISION SX        ! Sum of X values.      
      DOUBLE PRECISION SXX       ! Sum of X squared values.      
      DOUBLE PRECISION SXY       ! Sum of X*Y values.      
      DOUBLE PRECISION SY        ! Sum of Y values.      
      DOUBLE PRECISION SYY       ! Sum of Y squared values.      
      DOUBLE PRECISION XVAL      ! X value
      DOUBLE PRECISION YVAL      ! Y value
      INTEGER I                  ! Loop count.
      INTEGER N                  ! No. of points in sums.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the running sums to zero.
      SY = 0.0D0
      SX = 0.0D0
      SXY = 0.0D0
      SXX = 0.0D0
      SYY = 0.0D0
      N = 0

*  Loop round finding the sums of all the necessary terms.
      DO I = LBND, UBND
         XVAL = X( I )
         YVAL = Y( I )

         IF( XVAL .NE. VAL__BADD .AND. YVAL .NE. VAL__BADD ) THEN
            SY = SY +  YVAL
            SX = SX +  XVAL
            SXY = SXY +  XVAL * YVAL
            SXX = SXX +  XVAL**2
            SYY = SYY +  YVAL**2
            N = N + 1
         END IF

      END DO

*  Report an error if there are less than 2 good data values.
      IF( N .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_FIT1D_ERR1',
     :                 'KPG1_FIT1D: No good data values suupplied',
     :                 STATUS )
         GO TO 999

      ELSE IF( N .EQ. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_FIT1D_ERR2',
     :                 'KPG1_FIT1D: Only 1 good data value found',
     :                 STATUS )
         GO TO 999

      END IF

*  Form the denominator used to calculate the returned values.
      DENOM =  N*SXX - SX*SX

*  Report an error if the denominator is zero.
      IF( DENOM .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_FIT1D_ERR3',
     :                 'KPG1_FIT1D: All supplied X values are equal',
     :                 STATUS )

      END IF

*  Form the gradient.
      M =  ( N*SXY - SX*SY )/DENOM 

*  Form the intercept.
      C =  ( SXX*SY - SX*SXY ) /DENOM 

*  Form the RMS residual.
      RMS =  SQRT( MAX( 0.0D0, ( SYY +
     :       ( 2.0*SX*SY*SXY - SY*SY*SXX - SXY*SXY*N )/DENOM)/N ) ) 

 999  CONTINUE

      END
