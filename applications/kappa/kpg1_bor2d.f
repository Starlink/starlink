      SUBROUTINE KPG1_BOR2D( VALUE, BORWID, DIM1, DIM2, ARRAY, STATUS )
*+
*  Name:
*     KPG1_BOR2x
 
*  Purpose:
*     Places a border of constant values at the edges of a 2-d array.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_BOR2x( VALUE, BORWID, DIM1, DIM2, ARRAY, STATUS )
 
*  Description:
*     This routine assigns a constant value to the edge pixels of a
*     2-d array.  The width of the edge is adjustable along each
*     axis, but it is the same for the leading and trailing edges along
*     a given dimension.  If the border is wider than the array
*     a SAI__ERROR status is returned.
 
*  Arguments:
*     VALUE = ? (Given)
*        Value to be assigned to the borders of the array.
*     BORWID( 2 ) = INTEGER (Given)
*        The width in pixels of the borders in each dimension, x then y.
*        Each must be less than its corresponding dimension.
*     DIM1 = INTEGER (Given)
*        The first dimension of the array to have constant-valued
*        peripheries.
*     DIM2 = INTEGER (Given)
*        The second dimension of the array to have constant-valued
*        peripheries.
*     ARRAY( DIM1, DIM2 ) = ? (Given and Returned)
*        The array to have its borders set to a constant.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B or UB as appropriate. The
*     array and value to be substituted that are supplied to the routine
*     must have the data type specified.
 
*  Implementation Deficiencies:
*     -  The routine should be made n-D.
 
*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1991 July 22 (MJC):
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
      DOUBLE PRECISION VALUE
      INTEGER BORWID( 2 )
      INTEGER DIM1
      INTEGER DIM2
 
*  Arguments Given and Returned:
      DOUBLE PRECISION ARRAY( DIM1, DIM2 )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER
     :  I, J                     ! Loop counters
 
*.
 
*    Check the inherited global status.
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*    Validate border widths.
 
      IF ( BORWID( 1 ) .GE. DIM1 .OR. BORWID( 2 ) .GE. DIM2 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'BOR1', BORWID( 1 ) )
         CALL MSG_SETI( 'BOR2', BORWID( 2 ) )
         CALL MSG_SETI( 'DIM1', DIM1 )
         CALL MSG_SETI( 'DIM2', DIM2 )
         CALL ERR_REP( 'KPG1_BOR2x_TOOWIDE',
     :     'The border (^BOR1, ^BOR2) is wider than the array '/
     :     /'(^DIM1, ^DIM2).', STATUS )
         GOTO 999
      END IF
 
*    Fill the lower border.
 
      DO J = 1, BORWID( 2 )
         DO I = 1, DIM1
            ARRAY( I, J ) = VALUE
         END DO
      END DO
 
*    Fill the left and right borders.
 
      DO J = BORWID( 2 ) + 1, DIM2 - BORWID( 2 )
         DO I = 1, BORWID( 1 )
            ARRAY( I, J ) = VALUE
         END DO
         DO I = DIM1 - BORWID( 1 ) + 1, DIM1
            ARRAY( I, J ) = VALUE
         END DO
      END DO
 
*    Fill the upper border.
 
      DO J = DIM2 - BORWID( 2 ) + 1, DIM2
         DO I = 1, DIM1
            ARRAY( I, J ) = VALUE
         END DO
      END DO
 
  999 CONTINUE
 
      END
