      SUBROUTINE LD2AR( NX, NY, SX, SY, NBIN, X, Y, Z, ARRAY, STATUS )
*+
*  Name:
*     LD2AR

*  Purpose:
*     Converts a sparse form of a 2-D array stored in DOUBLE PRECISION
*     into its REAL 2-D counterpart.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LD2AR( NX, NY, SX, SY, NBIN, X, Y, Z, ARRAY, STATUS )

*  Description:
*     A list of x-y positions and values are converted to a complete
*     2-d array.  Missing elements take the bad value.

*  Arguments:
*     NX = INTEGER (Given)
*        The first dimension of the 2-d array.
*     NY = INTEGER (Given)
*        The second dimension of the 2-d array.
*     SX = REAL (Given)
*        The co-ordinate scale factor (i.e. the length of a pixel) in
*        the x direction.  It is used to determine which pixel a given
*        x-y position lies.  Normally, it will have the value 1.
*     SY = REAL (Given)
*        The co-ordinate scale factor (i.e. the length of a pixel) in
*        the y direction.  It is used to determine which pixel a given
*        x-y position lies.  Normally, it will have the value 1.
*     NBIN = INTEGER (Given)
*        The number of bins in the pixel list.
*     X( NBIN ) = DOUBLE PRECISION (Given)
*        The x position of the pixel in the list.
*     Y( NBIN ) = DOUBLE PRECISION (Given)
*        The y position of the pixel in the list.
*     Z( NBIN ) = DOUBLE PRECISION (Given)
*        The value of the pixel in the list.
*     ARRAY( NX, NY ) = REAL (Returned)
*        The expanded 2-d array formed from the list.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     {routine_notes}...

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 Feb 22 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Magic-value definitions.

*  Arguments Given:
      INTEGER
     :  NX, NY,                  ! Dimensions of the output array
     :  NBIN                     ! Number of bins in the x-y list.

      REAL
     :  SX, SY                   ! Pixel sizes

      DOUBLE PRECISION
     :  X( NBIN ),               ! x positions of the pixel list
     :  Y( NBIN ),               ! y positions of the pixel list
     :  Z( NBIN )                ! Values of the pixels in the list

*  Arguments Returned:
      REAL
     :  ARRAY( NX, NY )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  I, J,                    ! 2-d array indices
     :  L                        ! Loop counter

*.

*    Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    First initialise the array to the magic value.

      DO  J = 1, NY
         DO  I = 1, NX
            ARRAY( I, J ) = VAL__BADR
         END DO
      END DO

*    Fill the array with the list of values, by computing which bin a
*    given x-y position is situated.

      DO  L = 1, NBIN
         I = MIN( INT( REAL( X( L ) ) / SX ) + 1, NX )
         J = MIN( INT( REAL( Y( L ) ) / SY ) + 1, NY )
         ARRAY( I, J ) = REAL( Z( L ) )
      END DO

      END
