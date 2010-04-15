      PROGRAM FFTTEST
*+
*  Name:
*     FFTTEST

*  Purpose:
*     Compare FFTPACK and NAG Fourier Transform routines.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Fortran PROGRAM

*  Invocation:
*     FFTTEST

*  Description:
*     Two arbitrary 1D arrays (of length 200) are convolved together by
*     multiplication of their Fourier transforms. This is done using both
*     NAG routines and FFTPACK routines. Statistics on the absolute and
*     relative differences between the two results are displayed. Two
*     thousand such convolutions are done and the mean elapsed time for one
*     convolution using both libraries is displayed.
*
*     The test is done using purely real functions, and then done again
*     using complex functions.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-FEB-1995 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  External References:
      EXTERNAL ETIME
      REAL ETIME

*  Local Constants:
      INTEGER N                  ! No. of points in each array
      PARAMETER ( N = 200 )

      INTEGER RPT                ! No. of repeats to perform
      PARAMETER ( RPT = 2000 )

      DOUBLE PRECISION PI        ! Pi
      PARAMETER ( PI = 3.1415927D0 )

*  Local Variables:
      INTEGER I, J, K, IFAIL
      DOUBLE PRECISION A, B, C, D, X1( N ), Y1( N ), X2( N ), Y2( N ),
     :                 R1( N ), R2( N ), RF( N ), RN( N ),
     :                 WSAVE( 4*N+15 ), FAC, C1( 2*N ),
     :                 C2( 2*N ), CF( 2*N ), XX1( N ), YY1( N ),
     :                 XX2( N ), YY2( N ), XN( N ), YN( N )
      REAL FTIMER, NTIMER, TARRY( 2 ), T0, FTIMEC, NTIMEC

*.

*  Set up the input data (NAG-style). These are just samples from two
*  arbitrary functions which will be convolved together during the test.
      DO I = 1, N
         A = 2.0*PI*DBLE( I - 1 )/DBLE( N - 1 )

         X1( I ) = A*SIN( A*A )
         Y1( I ) = -A*SIN( A )

         X2( I ) = Y1( I )**2
         Y2( I ) = X1( I )*Y1( I )

      END DO

*  Store the current time
      T0 = ETIME( TARRY )

*  Say what is happening.
      WRITE(*,*)
      WRITE(*,*) 'Doing comparison between real sequence routines...'

*  Repeat the convolution a number of times to get better timing values.
      DO J = 1, RPT

*  Copy the real parts of these arrays to two arrays which will be
*  passed to the FFTPACK real sequence routines.
         DO I = 1, N
            R1( I ) = X1( I )
            R2( I ) = X2( I )
         END DO

*  Use FFTPACK to take the FFT of the real part of each function.
         CALL PDA_DRFFTI( N, WSAVE )
         CALL PDA_DRFFTF( N, R1, WSAVE )
         CALL PDA_DRFFTF( N, R2, WSAVE )

*  Multiply the 2 transforms together. First multiply the zeroth term
*  for which all imaginary parts are zero.
         RF( 1 ) = R1( 1 ) * R2( 1 )

*  Now do the remaining terms. Real and imaginary terms are stored in
*  adjacent elements of the arrays.
         DO I = 2, N - 1, 2

            A = R1( I )
            B = R1( I + 1 )
            C = R2( I )
            D = R2( I + 1 )

            RF( I ) = A*C - B*D
            RF( I + 1 ) = B*C + A*D

         END DO

*  If there are an even number of elements, do the last term, for which
*  the imaginary parts are again zero.
         IF( MOD( N, 2 ) .EQ. 0 ) RF( N ) = R1( N ) * R2( N )

*  Now take the inverse FFT.
         CALL PDA_DRFFTB( N, RF, WSAVE )

*  Divide the results by N*SQRT( N ) to take account of the different
*  normalisation of the FFTPACK results.
         FAC = 1.0D0/( DBLE( N ) * SQRT( DBLE( N ) ) )

         DO I = 1, N
            RF( I ) = RF( I )*FAC
         END DO

      END DO

*  Store the mean time (in seconds) taken to do one loop.
      FTIMER = ( ETIME( TARRY ) - T0 )/REAL( RPT )
      WRITE(*,*) '   FFTPACK done.'

*  Store the current time.
      T0 = ETIME( TARRY )

*  Repeat the convolution a number of times to get better timing values.
      DO J = 1, RPT

*  Copy the real parts of the two input arrays to two arrays which will
*  be passed to the NAG real sequence routines.
         DO I = 1, N
            R1( I ) = X1( I )
            R2( I ) = X2( I )
         END DO

*  Use NAG to take the FFT of the real part of each function.
         IFAIL = 0
         CALL C06FAF( R1, N, WSAVE, IFAIL )
         CALL C06FAF( R2, N, WSAVE, IFAIL )

*  Multiply the 2 transforms together. First multiply the zeroth term
*  for which all imaginary parts are zero.
         RN( 1 ) = R1( 1 ) * R2( 1 )

*  Now do the remaining terms. Real and imaginary terms are stored at
*  opposite ends of the arrays (with imaginary at the end, in reverse
*  order).
         DO I = 1, (N - 1)/2

            A = R1( I + 1 )
            B = R1( N - I + 1 )
            C = R2( I + 1 )
            D = R2( N - I + 1 )

            RN( I + 1) = A*C - B*D
            RN( N - I + 1 ) = B*C + A*D

         END DO

*  If there are an even number of elements, do the last term, for which
*  the imaginary parts are again zero.
         IF( MOD( N, 2 ) .EQ. 0 ) THEN
            RN( 1 + N/2 ) = R1( 1 + N/2 ) * R2( 1 + N/2 )
         END IF

*  Now take the inverse FFT. First take the complex conjugate of the FFT
*  array, and then call the Hermitian FFT routine.
         CALL C06GBF( RN, N, IFAIL )
         CALL C06FBF( RN, N, WSAVE, IFAIL )

      END DO

*  Store the mean time (in seconds) taken to do one loop.
      NTIMER = ( ETIME( TARRY ) - T0 )/REAL( RPT )
      WRITE(*,*) '   NAG done.'

*  Display the timings.
      WRITE(*,*)
      WRITE(*,*) '  Elapsed time for a NAG convolution     : ',NTIMER,
     :           ' s'
      WRITE(*,*) '  Elapsed time for an FFTPACK convolution: ',FTIMER,
     :           ' s'

*  Get statistics on the differences between the NAG and FFTPACK
*  results, and display them.
      CALL STATFN( N, RF, RN )

*---------------------------------------------------------------------
*  Now do it all again using the complex sequence routines instead of
*  the real sequence routines.
*
*  Say what is happening.
      WRITE(*,*)
      WRITE(*,*) 'Doing comparison between complex sequence routines...'

*  Store the current time
      T0 = ETIME( TARRY )

*  Repeat the convolution a number of times to get better timing values.
      DO J = 1, RPT

*  Store the real and imagainary parts of the input arrays in arrays
*  which can be passed to the FFTPACK complex sequence routines.
         DO I = 1, N
            K = 2*I
            C1( K - 1 ) = X1( I )
            C1( K ) = Y1( I )
            C2( K - 1 ) = X2( I )
            C2( K ) = Y2( I )
         END DO

*  Use FFTPACK to take the FFT of each complex function.
         CALL PDA_DCFFTI( N, WSAVE )
         CALL PDA_DCFFTF( N, C1, WSAVE )
         CALL PDA_DCFFTF( N, C2, WSAVE )

*  Multiply the 2 transforms together.
         DO I = 1, 2*N - 1, 2
            A = C1( I )
            B = C1( I + 1 )
            C = C2( I )
            D = C2( I + 1 )

            CF( I ) = A*C - B*D
            CF( I + 1 ) = B*C + A*D

         END DO

*  Now take the inverse FFT.
         CALL PDA_DCFFTB( N, CF, WSAVE )

*  Divide the results by N*SQRT( N ) to take account of the different
*  normalisation of the FFTPACK results.
         FAC = 1.0D0/( DBLE( N ) * SQRT( DBLE( N ) ) )

         DO I = 1, 2*N
            CF( I ) = CF( I )*FAC
         END DO

      END DO

*  Store the mean time (in seconds) taken to do one loop.
      FTIMEC = ( ETIME( TARRY ) - T0 )/REAL( RPT )
      WRITE( *, * ) '   FFTPACK done.'

*  Now do it again using NAG routines. Store the current time
      T0 = ETIME( TARRY )

*  Repeat the convolution a number of times to get better timing values.
      DO J = 1, RPT

*  Copy the input arrays to other arrays so that the original input data
*  wont be over-written by the NAG routines.
         DO I = 1, N
            XX1( I ) = X1( I )
            YY1( I ) = Y1( I )
            XX2( I ) = X2( I )
            YY2( I ) = Y2( I )
         END DO

*  Use NAG to take the FFT of each complex function.
         CALL C06FCF( XX1, YY1, N, WSAVE, IFAIL )
         CALL C06FCF( XX2, YY2, N, WSAVE, IFAIL )

*  Multiply the 2 transforms together.
         DO I = 1, N
            A = XX1( I )
            B = YY1( I )
            C = XX2( I )
            D = YY2( I )

            XN( I ) = A*C - B*D
            YN( I ) = B*C + A*D

         END DO

*  Now take the inverse FFT.
         CALL C06GCF( YN, N, IFAIL )
         CALL C06FCF( XN, YN, N, WSAVE, IFAIL )
         CALL C06GCF( YN, N, IFAIL )

      END DO

*  Store the mean time (in seconds) taken to do one loop.
      NTIMEC = ( ETIME( TARRY ) - T0 )/REAL( RPT )
      WRITE( *, * ) '   NAG done.'

*  Display the timings.
      WRITE(*,*)
      WRITE(*,*) 'Results of comparison between complex sequence '//
     :           'routines:'
      WRITE(*,*)
      WRITE(*,*) '  Elapsed time for a NAG convolution     : ',NTIMEC,
     :           ' s'
      WRITE(*,*) '  Elapsed time for an FFTPACK convolution: ',FTIMEC,
     :           ' s'

*  Convert the FFTPACK results into NAG format (i.e. put the real and
*  imaginary parts into two separate arrays).
      DO I = 1, N
         K = 2*I
         X1( I ) = CF( K - 1 )
         Y1( I ) = CF( K )
      END DO

*  Get statistics on the differences between the real parts of the NAG
*  and FFTPACK results, and display them.
      WRITE(*,*)
      WRITE(*,*) 'Comparison of real parts...'
      CALL STATFN( N, X1, XN )

*  Get statistics on the differences between the imaginary parts of the NAG
*  and FFTPACK results, and display them.
      WRITE(*,*)
      WRITE(*,*) 'Comparison of imaginary parts...'
      CALL STATFN( N, Y1, YN )

*  Display the relative speeds of the real and complex routines.
      WRITE(*,*)
      WRITE(*,*) 'Relative speed of real to complex routines:'
      WRITE(*,*) '   NAG    : ',NTIMEC/NTIMER
      WRITE(*,*) '   FFTPACK: ',FTIMEC/FTIMER
      WRITE(*,*)

      END



      SUBROUTINE STATFN( N, RF, RN )
      IMPLICIT NONE
      INTEGER N, I
      DOUBLE PRECISION RF( N ), RN( N ), MX, SUM1, SUM2, RES


*  Get statistics on the absolute differences.
      MX = ABS( RF( 1 ) - RN( 1 ) )
      SUM1 = 0.0
      SUM2 = 0.0

      DO I = 1, N
         RES = ( RF( I ) - RN( I ) )
         MX = MAX( MX, ABS( RES ) )
         SUM1 = SUM1 + RES
         SUM2 = SUM2 + RES**2
      END DO

*  Display the results.
      WRITE(*,*)
      WRITE(*,*) '  Mean absolute difference: ', REAL( SUM1/N )
      WRITE(*,*) '  RMS absolute difference : ', REAL( SQRT( SUM2/N ) )
      WRITE(*,*) '  Max. absolute difference: ', REAL( MX )
      WRITE(*,*)

*  Get statistics on the relative differences.
      MX = 0.5*ABS( ( RF( 1 ) - RN( 1 ) ) / ( RF( 1 ) + RN( 1 ) ) )
      SUM1 = 0.0
      SUM2 = 0.0

      DO I = 1, N
         RES = 0.5*( ( RF( I ) - RN( I ) ) / ABS( RF( I ) + RN( I ) ) )
         MX = MAX( MX, ABS( RES ) )
         SUM1 = SUM1 + RES
         SUM2 = SUM2 + RES**2
      END DO

*  Display the results.
      WRITE(*,*) '  Mean relative difference: ', REAL( SUM1/N )
      WRITE(*,*) '  RMS relative difference : ', REAL( SQRT( SUM2/N ) )
      WRITE(*,*) '  Max. relative difference: ', REAL( MX )
      WRITE(*,*)

      END
