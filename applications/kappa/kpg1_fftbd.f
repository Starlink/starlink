      SUBROUTINE KPG1_FFTBD( M, N, IN, WORK, OUT, STATUS )
*+
*  Name:
*     KPG1_FFTBD

*  Purpose:
*     Takes the inverse (Backward) FFT of a real image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_FFTBD( M, N, IN, WORK, OUT, STATUS )

*  Description:
*     The input array should hold a Fourier transform of a purely real 
*     image, in Hermitian format as produced by KPG1_FFTFD, or
*     KPG1_HMLTD.  The inverse FFT of this image is taken and returned
*     in OUT.

*  Arguments:
*     M = INTEGER (Given)
*        Number of columns in the input array.
*     N = INTEGER (Given)
*        Number of rows in the input array.
*     IN( M, N ) = DOUBLE PRECISION (Given)
*        The input array (a Fourier transform of a real image, stored in
*        Hermitian format).
*     WORK( * ) = DOUBLE PRECISION (Given)
*        Work space.  This must be at least ( 3*MAX( M, N ) + 15 )
*        elements long.
*     OUT( M, N ) = DOUBLE PRECISION (Returned)
*        The inverse FFT of the input array (a purely real image).
*        Note, the same array can be specified for both input and
*        output, in which case the supplied values are overwitten.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-FEB-1995 (DSB):
*        Original version.  Written to replace KPG1_HMFFT which
*        used NAG routines.
*     1995 March 27 (MJC):
*        Removed long lines and minor documentation revisions.  Used
*        modern-style variable declarations.
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

*  Initialise an array holding trig. functions used to form the inverse
*  FFT of the input image columns.
      CALL PDA_DRFFTI( N, WORK )

*  Store the index of the last-used element in the work array.
      IW = 2 * N + 15

*  Transform each column of the current output array.
      DO I = 1, M

*  Copy this column to the end of the work array, beyond the part used to
*  store trig. functions.
         DO  J = 1, N
            WORK( IW + J ) = OUT( I, J )
         END DO

*  Convert this column from NAG's Hermitian format to FFTPACK format,
*  and then transform the copy of this column using an FFTPACK routine.
         CALL PDA_DNAG2R( N, WORK( IW + 1 ) )
         CALL PDA_DRFFTB( N, WORK( IW + 1 ), WORK )

*  Copy the transformed column back to the output array.
         DO  J = 1, N
            OUT( I, J ) = WORK( IW + J ) 
         END DO

      END DO

*  Initialise an array holding trig. functions used to form the inverse
*  FFT of the input image rows.
      CALL PDA_DRFFTI( M, WORK )

*  Transform each row of the output array. 
      DO J = 1, N
         CALL PDA_DNAG2R( M, OUT( 1, J ) )
         CALL PDA_DRFFTB( M, OUT( 1, J ), WORK )
      END DO

      END
