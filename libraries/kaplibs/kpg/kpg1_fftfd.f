      SUBROUTINE KPG1_FFTFD( M, N, IN, WORK, OUT, STATUS )
*+
*  Name:
*     KPG1_FFTFD

*  Purpose:
*     Takes the forward FFT of a real image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_FFTFD( M, N, IN, WORK, OUT, STATUS )

*  Description:
*     The Fourier transform of the input (purely real) image is taken 
*     and returned in OUT.  The returned FT is stored in Hermitian format,
*     in which the real and imaginary parts of the FT are combined into
*     a single array. The FT can be inverted using KPG1_FFTBD, and two
*     Hermitian FTs can be multipled together using routine KPG1_HMLTD.

*  Arguments:
*     M = INTEGER (Given)
*        Number of columns in the input image.
*     N = INTEGER (Given)
*        Number of rows in the input image.
*     IN( M, N ) = DOUBLE PRECISION (Given)
*        The input image.
*     WORK( * ) = DOUBLE PRECISION (Given)
*         Work space.  This must be at least ( 3*MAX( M, N ) + 15 )
*         elements long.
*    OUT( M, N ) = DOUBLE PRECISION (Returned)
*       The FFT in Hermitian form.  Note, the same array can be used
*       for both input and output, in which case the supplied values
*       will be over-written.
*    STATUS = INTEGER (Given and Returned)
*       The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-FEB-1995 (DSB):
*        Original version. Written to replace KPG1_RLFFT which
*        used NAG routines.
*     1995 March 27 (MJC):
*        Removed long lines and minor documentation revisions.  Used
*        modern-style variable declarations.
*     1995 September 7 (MJC):
*        Used PDA_ prefix for FFTPACK routines.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER M
      INTEGER N
      DOUBLE PRECISION IN( M, N )
      DOUBLE PRECISION WORK( * )

*  Arguments Returned:
      DOUBLE PRECISION OUT( M, N )

*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Column counter
      INTEGER IW                 ! Index into work array
      INTEGER J                  ! Row counter

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the input array to the output array.
      DO J = 1, N
         DO I = 1, M
            OUT( I, J ) = IN( I, J )
         END DO
      END DO

*  Initialise an array holding trig. functions used to form the FFT
*  of the input image rows.
      CALL PDA_DRFFTI( M, WORK )

*  Transform each row of the output array, and convert each array into
*  the equivalent NAG format.
      DO J = 1, N
         CALL PDA_DRFFTF( M, OUT( 1, J ), WORK )
         CALL PDA_DR2NAG( M, OUT( 1, J ) )
      END DO

*  Re-initialise the work array to hold trig. functions used to form
*  the FFT of the image columns.
      CALL PDA_DRFFTI( N, WORK )

*  Store the index of the last-used element in the work array.
      IW = 2 * N + 15

*  Transform each column of the current output array.
      DO I = 1, M

*  Copy this column to the end of the work array, beyond the part used
*  to store trig. functions.
         DO  J = 1, N
            WORK( IW + J ) = OUT( I, J )
         END DO

*  Transform the copy of this column.
         CALL PDA_DRFFTF( N, WORK( IW + 1 ), WORK )
         CALL PDA_DR2NAG( N, WORK( IW + 1 ) )

*  Copy the transformed column back to the output array.
         DO  J = 1, N
            OUT( I, J ) = WORK( IW + J ) 
         END DO

      END DO

      END
