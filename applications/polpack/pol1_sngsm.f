      SUBROUTINE POL1_SNGSM( NPIX, NROW, NP, DATA, WORK, STATUS )
*+
*  Name:
*     POL1_SNGSM

*  Purpose:
*     Applies a 2D boxfilter to all planes of a 3D cube.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SNGSM( NPIX, NROW, NP, DATA, WORK, STATUS )

*  Description:
*     This routine smoothes each plane of the supplied cube using a 2D 
*     mean box filter. The size of the box is hardwired to the value
*     of parameter HW.

*  Arguments:
*     NPIX = INTEGER (Given)
*        The number of pixels per row in each plane.
*     NROW = INTEGER (Given)
*        The number of rows in each plane.
*     NP = INTEGER (Given)
*        The number of planes.
*     DATA( NPIX, NROW, NP ) = REAL (Given and Returned)
*        The array to be smoothed.
*     WORK( NPIX, NROW ) = REAL (Given and Returned)
*        A work array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-FEB-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NPIX
      INTEGER NROW
      INTEGER NP

*  Arguments Given and Returned:
      REAL DATA( NPIX, NROW, NP )
      REAL WORK( NPIX, NROW )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER HW                 ! Half width of the box size
      PARAMETER ( HW = 2 )

*  Local Variables:
      INTEGER I                  ! Plane index
      INTEGER IPW1               ! Pointer to real workspace
      INTEGER IPW2               ! Pointer to integer workspace
      INTEGER J                  ! Row index
      INTEGER K                  ! Pixel index
      LOGICAL BADOUT             ! Any bad output piels?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Allocate work space.
      CALL PSX_CALLOC( NPIX, '_REAL', IPW1, STATUS )
      CALL PSX_CALLOC( NPIX, '_INTEGER', IPW2, STATUS )

*  Loop round each plane.
      DO I = 1, NP
         CALL POL1_BLOCR( .TRUE., .TRUE., .FALSE., NPIX, NROW, 
     :                    DATA( 1, 1, I ), HW, HW, 1, WORK, BADOUT, 
     :                    %VAL( IPW1 ), %VAL( IPW2 ), STATUS )

*  Copy the smoothed data back to the input array.
         DO J = 1, NROW
            DO K = 1, NPIX
               DATA( K, J, I ) = WORK( K, J )
            END DO
         END DO

      END DO

*  Free the work space.
      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPW2, STATUS )

      END
