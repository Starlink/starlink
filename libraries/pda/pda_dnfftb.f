      SUBROUTINE PDA_DNFFTB( NDIM, DIM, X, Y, WORK, ISTAT )
*+
*  Name:
*     PDA_DNFFTB

*  Purpose:
*     Take the backward FFT of an N-dimensional complex array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PDA_DNFFTB( NDIM, DIM, X, Y, WORK, ISTAT )

*  Description:
*     The supplied Fourier co-efficients in X and Y are replaced by the
*     corresponding spatial data obtained by doing an inverse Fourier
*     transform. See the forward FFT routine PDA_DNFFTF for more details.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of dimensions. This should be no more than 20.
*     DIM( NDIM ) = INTEGER (Given)
*        The size of each dimension.
*     X( * ) = DOUBLE PRECISION (Given and Returned)
*        Supplied holding the real parts of the Fourier co-efficients.
*        Returned holding the real parts of the spatial data. The array
*        should have the number of elements implied by NDIM and DIM.
*     Y( * ) = DOUBLE PRECISION (Given and Returned)
*        Supplied holding the imaginary parts of the Fourier co-efficients.
*        Returned holding the imaginary parts of the spatial data. The array
*        should have the number of elements implied by NDIM and DIM.
*     WORK( * ) = DOUBLE PRECISION (Given and Returned)
*        A work array. This should have at least ( 6*DimMax + 15 )
*        elements where DimMax is the maximum of the values supplied in
*        DIM.
*     ISTAT = INTEGER (Returned)
*        If the value of NDIM is greater than 20 or less than 1, then
*        ISTAT is returned equal to 1, and the values in X and Y are
*        left unchanged. Otherwise, ISTAT is returned equal to 0.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-FEB-1995 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER NDIM
      INTEGER DIM( NDIM )

*  Arguments Given and Returned:
      DOUBLE PRECISION X( * )
      DOUBLE PRECISION Y( * )
      DOUBLE PRECISION WORK( * )

*  Arguments Returned:
      INTEGER ISTAT

*  Local Constants:
      INTEGER MXDIM              ! Max number of dimensions
      PARAMETER( MXDIM = 20 )

*  Local Variables:
      INTEGER
     :     CART( MXDIM + 1 ),    ! Current Cartesian pixel indices
     :     I,                    ! Index of current dimension
     :     INC,                  ! Vector increment to next row element
     :     IW,                   ! Index into the work array
     :     IWN,                  ! Index of first free work array element
     :     J,                    ! Row counter
     :     K,                    ! Pixel index on current axis
     :     M,                    ! Size of current dimension
     :     N,                    ! Total no. of pixels
     :     STEP,                 ! Vector step to start of next row
     :     V,                    ! Vector address
     :     V0                    ! Vector address of start of current row

      DOUBLE PRECISION
     :     FAC                   ! Normalisation factor

*.

*  Check that the supplied number of dimensions is not too high, and
*  not too low. Return 1 for the status variable and abort otherwise.
      IF( NDIM .GT. MXDIM .OR. NDIM .LE. 0 ) THEN
         ISTAT = 1

*  If the number of dimensions is ok, return 0 for the status value and
*  continue.
      ELSE
         ISTAT = 0

*  Find the total number of pixels.
         N = 1
         DO I = 1, NDIM
            N = N*DIM( I )
         END DO

*  The first dimension can be processed using a faster algorithm
*  because the elements to be processed occupy adjacent elements in the
*  supplied array. Set up the step (in vector address) between the
*  start of each row, and initialise the vector address of the start of
*  the first row.
         M = DIM( 1 )
         V0 = 1

*  Initialise the FFT work array for the current dimension. Save the
*  index of the next un-used element of the work array.
         CALL PDA_DCFFTI( M, WORK )
         IWN = 4*M + 16

*  Store the factor which will normalise the Fourier co-efficients
*  returned by this routine (i.e. so that a call to PDA_DNFFTB followed by a
*  call to PDA_DNFFTB will result in no change to the data).
         FAC = 1.0D0/SQRT( DBLE( N ) )

*  Loop round copying each row.
         DO J = 1, N/M

*  Copy this row into the unused part of the work array.
            IW = IWN
            V = V0
            DO K = 1, M
               WORK( IW ) = X( V )
               WORK( IW + 1 ) = Y( V )
               IW = IW + 2
               V = V + 1
            END DO

*  Take the FFT of it.
            CALL PDA_DCFFTB( M, WORK( IWN ), WORK )

*  Copy it back to the supplied arrays, normalising it in the process.
            IW = IWN
            V = V0
            DO K = 1, M
               X( V ) = WORK( IW )*FAC
               Y( V ) = WORK( IW + 1 )*FAC
               IW = IW + 2
               V = V + 1
            END DO


*  Increment the vector address of the start of the next row.
            V0 = V0 + M

         END DO

*  Now set up the increment between adjacent elements of "rows" parallel
*  to the second dimension.
         INC = DIM( 1 )

*  Process the remaining dimensions. Store the durrent dimensions.
         DO I = 2, NDIM
            M = DIM( I )

*  Initialise the co-ordinates (vector and Cartesian) of the first
*  element of the first row.
            V0 = 1

            DO J = 1, NDIM
               CART( J ) = 1
            END DO

*  Initialise the FFT work array for this dimension, and save the index
*  of the next un-used element in the work array.
            CALL PDA_DCFFTI( M, WORK )
            IWN = 4*M + 16

*  Store the step (in vector address) between the end of one "row" and
*  the start of the next.
            STEP = INC*( M - 1 )

*  Loop round each "row" parallel to the current dimensions.
            DO J = 1, N/M

*  Copy the current "row" into the work space.
               V = V0
               IW = IWN

               DO K = 1, M
                  WORK( IW ) = X( V )
                  WORK( IW + 1 ) = Y( V )
                  V = V + INC
                  IW = IW + 2
               END DO

*  Take the FFT of the current "row".
               CALL PDA_DCFFTB( M, WORK( IWN ), WORK )

*  Copy the FFT of the current "row" back into the supplied array.
               V = V0
               IW = IWN

               DO K = 1, M
                  X( V ) = WORK( IW )
                  Y( V ) = WORK( IW + 1 )
                  V = V + INC
                  IW = IW + 2
               END DO

*  Increment the co-ordinates of the start of the current "row".
               V0 = V0 + 1
               K = 1
               CART( 1 ) = CART( 1 ) + 1

*  If the upper pixel index bound for the current dimension has been
*  exceeded, reset the pixel index to 1 and increment the next
*  dimension. If the next dimension is the dimension currently being
*  transformed, skip over it so that it stays at 1 (but increment the
*  vector address to account for the skip).
               DO WHILE( CART( K ) .GT. DIM( K ) )
                  CART( K ) = 1
                  K = K + 1

                  IF( K .EQ. I ) THEN
                     K = K + 1
                     V0 = V0 + STEP
                  END IF

                  CART( K ) = CART( K ) + 1

               END DO

            END DO

*  Store the increment in vector address between adjacent elements of
*  the next "row".
            INC = INC*M

         END DO

      END IF

      END
