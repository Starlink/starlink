      SUBROUTINE KPS1_GLIDD( LBND1, LBND2, UBND1, UBND2, DIN, NPOS, 
     :                       PIXPOS, STATUS ) 
*+
*  Name:
*     KPS1_GLIDD

*  Purpose:
*     Store bad pixels positions in a supplied array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_GLIDD( LBND1, LBND2, UBND1, UBND2, DIN, NPOS, PIXPOS, STATUS ) 

*  Description:
*     This routine stores the pixel co-ordinates at the centre of all the 
*     bad pixels in the supplied array.

*  Arguments:
*     LBND1 = INTEGER (Given)
*        Lower pixel index on axis 1.
*     LBND2 = INTEGER (Given)
*        Lower pixel index on axis 2.
*     UBND1 = INTEGER (Given)
*        Upper pixel index on axis 1.
*     UBND2 = INTEGER (Given)
*        Upper pixel index on axis 2.
*     DIN( LBND1:UBND1, LBND2:UBND2 ) = DOUBLE PRECISION (Given)
*        The input data array.
*     NPOS = INTEGER (Returned)
*        The number of returned positions. Returned equal to zero if
*        there are no bad pixels in the array,
*     PIXPOS( NPOS, 2 ) = DOUBLE PRECISION (Returned)
*        The array in which to store the first NPOS bad pixel positions
*        found in the supplied data array.
*     STATUS = INTEGER (Given and Returned)
*        The inherited status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-MAR-2000 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed

*  Global Constants: 
      INCLUDE 'SAE_PAR'        ! Global SSE parameters 
      INCLUDE 'PRM_PAR'        ! VAL__ constants

*  Arguments Given:
      INTEGER LBND1
      INTEGER LBND2
      INTEGER UBND1
      INTEGER UBND2
      DOUBLE PRECISION DIN( LBND1:UBND1, LBND2:UBND2 )
      INTEGER NPOS

*  Arguments Returned:
      DOUBLE PRECISION PIXPOS( NPOS, 2 )

*  Global Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER COUNT,I,J      
*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

* Initialise the number of bad pixels found so far.
      COUNT = 0

*  Loop round the data array.
      DO J = LBND2, UBND2
         DO I = LBND1, UBND1

*  See if this pixel is bad.
            IF( DIN( I, J ) .EQ. VAL__BADD ) THEN

*  If so, increase the count of bad pixels.
               COUNT = COUNT + 1

*  If the array is not full, store the pixel co-ordinates at the centre
*  of this pixel.
               IF( COUNT .LE. NPOS ) THEN
                  PIXPOS( COUNT, 1 ) = DBLE( I ) - 0.5D0
                  PIXPOS( COUNT, 2 ) = DBLE( J ) - 0.5D0
               END IF

            END IF

         END DO

      END DO

      END
