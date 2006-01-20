      SUBROUTINE KPS1_LMKPC( NDIM, DIM, EL, PIXCEN, STATUS )
*+
*  Name:
*     KPS1_LMKPC

*  Purpose:
*     Store GRID Frame pixel centres in an array for LISTMAKE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LMKPC( NDIM, DIM, EL, PIXCEN, STATUS )

*  Description:
*     This routine stores the GRID Frame co-ordinates of every pixel
*     in the supplied array.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of axes.
*     DIM( NDIM ) = INTEGER (Given)
*        The dimension of each axis.
*     EL = INTEGER (Given)
*        The total number of pixels.
*     PIXCEN( EL, NDIM ) = DOUBLE PRECISION (Returned)
*        The returned pixel centre positions.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER NDIM
      INTEGER DIM( NDIM )
      INTEGER EL

*  Arguments Returned:
      DOUBLE PRECISION PIXCEN( EL, NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Position count
      INTEGER J( NDF__MXDIM )    ! pixel indices
      INTEGER K                  ! Axis count
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the indices of the current pixel.
      DO K = 1, NDIM
         J( K ) = 1
      END DO

*  Do each pixel.
      DO I = 1, EL

*  Store the required GRID co-ordinates.
         DO K = 1, NDIM
            PIXCEN( I, K ) = DBLE( J( K ) ) 
         END DO

*  Increment the pixel indices.   
         J( 1 ) = J( 1 ) + 1

         K = 1
         DO WHILE( K .LE. NDIM .AND. J( K ) .GT. DIM( K ) ) 
            J( K ) = 1
            K = K + 1
            J( K ) = J( K ) + 1
         END DO

      END DO

      END
