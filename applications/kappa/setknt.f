      SUBROUTINE SETKNT( X, NX, NXKNOT, XKNOT, STATUS )
 
*+
*  Name:
*     SETKNT

*  Purpose:
*     To place interior knots so as to maintain an even distribution
*     of data points between the knot positions when fitting spline
*     functions to data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SETKNT( X, NX, NXKNOT, XKNOT, STATUS )

*  Arguments:
*     X( NX ) = DOUBLE PRECISION (Given)
*        The co-ordinates of the points.
*     NX = INTEGER (Given)
*        The number of data points.
*     NXKNOT = INTEGER (Given)
*        The number of interior knots to be defined.
*     XKNOT( NXKNOT ) = DOUBLE PRECISION (Returned)
*        The x positions of the interior knots defined.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses the magic-value method for bad or undefined pixels.

*  Algorithm:
*     - Calculate the mean number of data points per knot interval.
*     - Place the knots this number of data points apart, interpolating
*       linearly between the data positions to obtain the knot
*       positions.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 Jan 31 (MJC):
*        Original version based on the EDRS code of the same name.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER
     :  NX,                      ! Number of data points
     :  NXKNOT                   ! Number of interior knots

      DOUBLE PRECISION
     :  X( NX )                  ! Co-ordinates of the data

*  Arguments Returned:
      DOUBLE PRECISION
     :  XKNOT( NXKNOT )          ! Positions of the interior knots

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  I,                       ! Loop counter
     :  NDAT1,                   ! Index to the lower data point used in
                                 ! the interpolation
     :  NDAT2                    ! Index to the upper data point used in
                                 ! the interpolation

      REAL
     :  DATAPT,                  ! Number of data points just less than
                                 ! the required knot value
     :  FRAC,                    ! Interpolation fraction
     :  XINTVL                   ! Number of data points per knot
                                 ! interval
 
*.

*    Check inherited global status.
    
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find the mean number of data points per knot interval.
*
      XINTVL = REAL( NX - 1 ) / ( REAL( NXKNOT ) + 1.0 )

*    Count through each knot, calculating the number of data points
*    less than the required knot value.

      DO  I = 1, NXKNOT
         DATAPT = 1.0 + XINTVL * REAL ( I )

*       The result is not usually an integer, so interpolate linearly
*       between the ordered data positions.

         NDAT1 = INT( DATAPT )
         NDAT2 = MIN( NX, NDAT1 + 1 )
         FRAC = DATAPT - REAL( NDAT1 )
         XKNOT( I ) = X( NDAT1 ) * ( 1.0 - FRAC ) + X( NDAT2 ) * FRAC
      END DO
 
      END
