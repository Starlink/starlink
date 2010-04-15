      PROGRAM RANDTEST
*+
*  Name:
*     RANDTEST

*  Purpose:
*     Tests the PDA random-number generators

*  Language:
*     Starlink Fortran 77

*  Description:
*     This routine tests the PDA random-number generators for
*     10000-element samples, and using different parameter settings.
*     The mean, standard deviation, skewness, and kurtosis are
*     calculated; these and the expected values are output.  Also output
*     are the first 100 sample elements.

*  Usage:
*     randtest

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 November 21 (MJC):
*        Original version based upon the test routine provided with the
*        TOMS599 package.
*     1997 February 26 (DSB):
*        Converted from an A-TASK into a stand-alone function. Option for
*        initialising the seed using the current time removed. A fixed seed
*        is now always used.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  External References:
      EXTERNAL PDA_RAND
      REAL PDA_RAND           ! Random number, linear [0,1]

      EXTERNAL PDA_RNEXP
      REAL PDA_RNEXP          ! Random number, exponential dist.

      EXTERNAL PDA_RNGAM
      REAL PDA_RNGAM          ! Random number, gamma distribution

      EXTERNAL PDA_RNNOR
      REAL PDA_RNNOR          ! Random number, Normal distribution

      EXTERNAL PDA_RNPOI
      INTEGER PDA_RNPOI          ! Random number, Poisson distribution

*  Local Constants:
      INTEGER NDIST              ! Number of distributions
      PARAMETER ( NDIST = 5 )

      INTEGER NRAND              ! Number of random numbers generated
      PARAMETER ( NRAND = 10000 )

*  Local Variables:
      REAL A                     ! Gamma-function A value
      CHARACTER * ( 75 ) BUFFER  ! Buffer for creating some output
      REAL E1                    ! Sample mean
      REAL E2                    ! Sample standard deviation
      REAL E3                    ! Sample skewness
      REAL E4                    ! Sample kurtosis
      INTEGER I                  ! Loop counter
      INTEGER II( NDIST )        ! Number of runs per distribution
      INTEGER IDIS               ! Distribution loop counter
      INTEGER IPAR               ! Run number loop counter
      REAL MU                    ! Mean value
      INTEGER NRUN               ! Number of runs
      LOGICAL RANDOM             ! Use non-reproducable seed?
      REAL SAMPLE( NRAND )       ! Deviates
      REAL S1                    ! Summed first moment
      REAL S2                    ! Summed second moment
      REAL S3                    ! Summed third moment
      REAL S4                    ! Summed fourth moment
      INTEGER SEED               ! Seed
      REAL T1                    ! Mean of supplied distribution
      REAL T2                    ! Std. dev. of supplied distribution
      REAL T3                    ! Skewness of supplied distribution
      REAL T4                    ! Kurtosis of supplied distribution
      INTEGER TICKS              ! Time ticks
      REAL VPAR4( 22 )           ! Parameters for Gamma distribution
      REAL VPAR5( 24 )           ! Parameters for Poisson distribution
      REAL X1                    ! First moment
      REAL X2                    ! Second moment
      REAL X3                    ! Third moment
      REAL X4                    ! Fourth moment

*  Local Data:
      DATA II / 1, 1, 1, 22, 24 /

      DATA VPAR4 / 0.0001, 0.25, 0.5, 0.75, 0.9999, 1.0, 1.5, 2.0, 3.0,
     :             4.0, 5.0, 7.0, 10.0, 15.0, 20.0, 30.0, 50.0, 100.0,
     :             1000.0, 10000.0, 100000.0, 1000000.0 /

      DATA VPAR5 / 0.0001, 1.0, 2.0, 5.0, 9.99, 10.0, 12.0, 15.0, 20.0,
     :             25.0, 30.0, 40.0, 50.0, 75.0, 100.0, 150.0, 200.0,
     :             500.0, 1000.0, 2000.0, 5000.0, 1.E4, 1.E5, 1.E6 /

*.

* Set the fixed seed.
      SEED = 1111
      SEED = ( SEED / 4 ) * 4 + 1
      IF ( SEED .LT. 0 ) SEED = 1

*  Write the main heading.
      WRITE(*,*)
     :  'Listing of trial runs for PDA-random-number routines based '/
     :  /'code by Ahrens/Dieter/Kohrt using seed ',SEED

*  Trial runs for 5 different cases:
*
*    IDIS = 1:  0,1 -uniform distribution
*    IDIS = 2:  (standard-) exponential distribution
*    IDIS = 3:  (standard-) Normal distribution
*    IDIS = 5:  Poisson-(mu) distribution
      DO IDIS = 1, NDIST
         NRUN = II( IDIS )

*  Write the case heading.
         WRITE(*,*)
         WRITE(*,*)
         IF ( IDIS .EQ. 1 ) THEN
            WRITE(*,*)
     :        '1)   0,1 -Uniform distribution:'
            WRITE(*,*)
     :        '*******************************'

         ELSE IF ( IDIS .EQ. 2 ) THEN
            WRITE(*,*)
     :        '2)  (Standard-) exponential distribution:'
            WRITE(*,*)
     :        '*****************************************'

         ELSE IF ( IDIS .EQ. 3 ) THEN
            WRITE(*,*)
     :        '3)  (Standard-) Normal distribution:'
            WRITE(*,*)
     :        '************************************'

         ELSE IF ( IDIS .EQ. 4 ) THEN
            WRITE(*,*)
     :        '4)  (standard-) gamma-(a) distribution:'
            WRITE(*,*)
     :        '***************************************'

         ELSE IF ( IDIS .EQ. 5 ) THEN
            WRITE(*,*)
     :        '5)  Poisson-(mu) distribution (integer samples are '/
     :        /'displayed as reals):'
            WRITE(*,*)
     :        '***************************************************'/
     :        /'********************'

         END IF

*  Loop for every parameter value.
         DO IPAR = 1, NRUN

*  Case 4 and 5: set parameter values according to the data vector.
            IF ( IDIS .EQ. 4 ) A = VPAR4( IPAR )
            IF ( IDIS .EQ. 5 ) MU = VPAR5( IPAR )

*  Set the seed for each run and distribution.
            CALL PDA_RNSED( SEED )

*  Sample the random numbers, dealing with each case separately.

*  0,1 -uniform distribution.
            IF ( IDIS .EQ. 1 ) THEN
               DO I = 1, NRAND
                  SAMPLE( I ) = PDA_RAND( 0.0 )
               END DO
               T1 = 0.5
               T2 = 1.0 / SQRT( 12.0 )
               T3 = 0.0
               T4 = -1.2

*  (Standard-) exponential distribution.
            ELSE IF ( IDIS .EQ. 2 ) THEN
               DO I = 1, NRAND
                  SAMPLE( I ) = PDA_RNEXP( 1.0 )
               END DO
               T1 = 1.0
               T2 = 1.0
               T3 = 2.0
               T4 = 6.0

*  (Standard-) Normal distribution
            ELSE IF ( IDIS .EQ. 3 ) THEN
               DO I = 1, NRAND
                   SAMPLE( I ) = PDA_RNNOR( 0.0, 1.0 )
               END DO
               T1 = 0.0
               T2 = 1.0
               T3 = 0.0
               T4 = 0.0

*  (Standard-) gamma-(a) distribution
            ELSE IF ( IDIS .EQ. 4 ) THEN
               DO I = 1, NRAND
                  SAMPLE( I ) = PDA_RNGAM( A )
               END DO
               T1 = A
               T2 = SQRT( A )
               T3 = 2.0 / T2
               T4 = 6.0 / A

*  Write the underlined parameter values.
               IF ( IPAR .NE. 1 ) THEN
                  WRITE(*,*)
                  WRITE(*,*)
               END IF
               BUFFER = ' '
               BUFFER( 50: ) = 'Gamma-(A) = '
               WRITE(*,*) BUFFER, A
               BUFFER( 50: ) = '-------------------------'
               WRITE(*,*) BUFFER


*  Poisson-(mu) distribution
            ELSE IF ( IDIS .EQ. 5 ) THEN
               DO I = 1, NRAND
                  SAMPLE( I ) = REAL( PDA_RNPOI( MU ) )
               END DO
               T1 = MU
               T2 = SQRT( MU )
               T3 = 1.0 / T2
               T4 = 1.0 / MU

*  Write the underlined parameter values.
               IF ( IPAR .NE. 1 ) THEN
                  WRITE(*,*)
                  WRITE(*,*)
               END IF
               BUFFER = ' '
               BUFFER( 47: ) = 'Poisson-(MU) = '
               WRITE(*,*) BUFFER, MU
               BUFFER( 47: ) = '----------------------------'
               WRITE(*,*) BUFFER

            END IF

*  Output the first 100 random deviates for each run.  Integer samples
*  are displayed as reals.
            IF ( IDIS .LE. 3 ) THEN
               WRITE(*,*)
               WRITE(*,*)
            END IF

            WRITE(*,*) '  First 100 samples:'
            WRITE(*,*) '  ..................'
            WRITE(*,*)

            DO I = 1, 100, 5
               WRITE(*,*) SAMPLE( I ), SAMPLE( I + 1 ),
     :                    SAMPLE( I + 2 ), SAMPLE( I + 3 ),
     :                    SAMPLE( I + 4 )
            END DO

*  Evaluate the sample mean:  E1 - (1 / N) * SUM( SAMPLE( I ) )
            S1 = 0.0
            DO I = 1, NRAND
               S1 = S1 + SAMPLE( I )
            END DO
            E1 = S1 / REAL( NRAND )

*  Evaluate further sample statistics.
*
*  Sk  - (1 / N) * SUM( (SAMPLE( I ) - E1) ** k )
*        Sample central moments (k = 2,3,4) with respect to sample mean.
*
*  E2  - SQRT(S2)
*        Sample standard deviation (= SQRT(SAMPLE variance)).
*
*  E3  - S3 / S2**(3 / 2)
*        Sample skewness
*
*  E4  - S4 / S2**2-3
*        Sample kurtosis
            S2 = 0.0
            S3 = 0.0
            S4 = 0.0

            DO I = 1, NRAND
               X1 = SAMPLE( I ) - E1
               X2 = X1 * X1
               X3 = X2 * X1
               X4 = X2 * X2

               S2 = S2 + X2
               S3 = S3 + X3
               S4 = S4 + X4
            END DO

            S2 = S2 / 10000.0
            S3 = S3 / 10000.0
            S4 = S4 / 10000.0

            E2 = SQRT( S2 )
            IF( S2 .GT. 0.0 ) THEN
               E3 = S3 / SQRT( S2 * S2 * S2 )
               E4 = S4 / ( S2 * S2 ) - 3.0
            ELSE
               E3 = 0.0
               E4 = 0.0
            END IF

*  Write the test data heading.
            WRITE(*,*)
            WRITE(*,*)
            WRITE(*,*)
     :        '  Test Data:     (based on ',NRAND,' samples)'
            WRITE(*,*) '  ..........'

*  Write the statistics' heading.
            WRITE(*,*)
            BUFFER = ' '
            BUFFER( 22: ) = 'Mean         Std.dev.       Skewness '/
     :                      /'      Kurtosis'
            WRITE(*,*) BUFFER
            WRITE(*,*)

*  Write the statistics.
            WRITE( BUFFER, '(''  True values:'',4E15.6)') T1, T2, T3,
     :        T4
            WRITE(*,*) BUFFER

            WRITE( BUFFER, '(''  Sample data:'',4E15.6)') E1, E2, E3,
     :        E4
            WRITE(*,*) BUFFER

            WRITE(*,*)

*  End of parameter loop.
         END DO

*  End of distribution loop.
      END DO

      END
