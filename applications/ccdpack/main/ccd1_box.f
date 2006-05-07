      SUBROUTINE CCD1_BOX( XS, YS, XOUT, YOUT, OVER, STATUS )
      
*+
*  Name:
*     CCD1_BOX

*  Purpose:
*     Calculate the area common to input clockwise polygon X(n), Y(n)
*     with square (XS, YS) to (XS+1, YS+1)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_BOX( XS, YS, XOUT, YOUT, OVER, STATUS )

*  Arguments:
*     XS = INTEGER (Given)
*       X co-ordinate of affected ouptut pixel
*     YS = INTEGER (Given)
*       Y co-ordinate of affected ouptut pixel
*     XOUT ( 4 ) = INTEGER (Given)
*       X co-ordinates of corners of input pixel in output grid
*     YOUT ( 4 ) = INTEGER (Given)
*       Y co-ordinates of corners of input pixel in output grid
*     OVER = DOUBLE PRECISION (Returned)
*       Returned overlap
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     AA: Alasdair Allan (STARLINK, Keele University)
*     {enter_new_authors_here}

*  History:
*     19-JUL-1999 (AA):
*        Original version based on Bill Sparks BOXER algorithim
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      
*  Arguments Given:
      INTEGER XS, YS
      DOUBLE PRECISION XOUT( 4 ), YOUT( 4 )

*  Arguments Returned:
      DOUBLE PRECISION OVER

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      DOUBLE PRECISION PX( 4 ), PY( 4 )
      DOUBLE PRECISION SUM, AREA

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up co-ordinates relative to unit square at origin
      DO I  = 1, 4   
         PX(I) = XOUT(I) - DBLE(XS) +0.5D0
         PY(I) = YOUT(I) - DBLE(YS) +0.5D0
      END DO   
      
*  For each line in the polygon calulate the area common to the unit square,
*  Allow negative area for subsequent `vector' addition of sub-areas. 
      SUM = 0.0D0
      DO I = 1, 3
         CALL CCD1_SGARE( PX(I), PY(I), PX(I+1), PY(I+1), 
     :                     AREA, STATUS )
         SUM = SUM + AREA
      END DO
         
      CALL CCD1_SGARE( PX(4), PY(4), PX(1), PY(1), AREA, STATUS )      
      SUM = SUM + AREA
            
      OVER = SUM
  
*  Time at the bar please...      
999   END


