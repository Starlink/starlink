      SUBROUTINE KPG1_DWSOD( LBND, UBND, AXIS, SCALE, OFFSET, STATUS )
*+
*  Name:
*     KPG1_DWSOx
 
*  Purpose:
*     Obtains for the scale and offset for a linear transformation from
*     world to data co-ordinates.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_DWSOx( LBND, UBND, AXIS, SCALE, OFFSET, STATUS )
 
*  Description:
*     This routine determines a linear transformation between world
*     (pixel) co-ordinates and data co-ordinates obtained from the axis
*     structure.  A linear equation is solved using the two boundary
*     conditions for the lower and upper bounds in world and data
*     co-ordinates.
*
*  Arguments:
*     LBND = INTEGER (Given)
*        The lower bound of the axis array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis array.
*     AXIS( LBND:UBND ) = ? (Given)
*        The axis array.
*     SCALE = ? (Returned)
*        The scale factor of the linear axis to transform from world
*        co-ordinates to data co-ordinates.
*     OFFSET = ? (Returned)
*        The offset of the linear axis at pixel 0 to transform from
*        world co-ordinates to data co-ordinates.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for the data types real or double precision:
*     replace "x" in the routine name by R or D respectively, as
*     appropriate.  The axis array, and the returned scale and offset
*     should have this data type as well.
 
*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1991 June 21 (MJC):
*        Original version.
*     {enter_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
 
*  Arguments Given:
      INTEGER
     :  LBND,
     :  UBND
 
      DOUBLE PRECISION
     :  AXIS( LBND:UBND )
 
*  Arguments Returned:
      DOUBLE PRECISION
     :  SCALE,
     :  OFFSET
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      DOUBLE PRECISION
     :  DB( 2 )                  ! Bounds of the data co-ordinates
 
*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions
 
*.
 
*    Check the inherited global status.
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*    Find the extreme data co-ordinates for finding the scales and
*    offsets.
 
      DB( 1 ) = AXIS( LBND )
      DB( 2 ) = AXIS( UBND )
 
*    Solve a linear equation using the two boundary conditions in
*    world and data co-ordinates.  Protect against divide by zero.
 
      IF ( UBND - LBND .GT. 0 ) THEN
         SCALE = ( DB( 2 ) - DB( 1 ) ) / NUM_ITOD( UBND - LBND )
         OFFSET = DB( 1 ) - SCALE * ( NUM_ITOD( LBND ) - 0.5 )
      ELSE
         SCALE = 1.0D0
         OFFSET = 0.0D0
      END IF
 
      END
