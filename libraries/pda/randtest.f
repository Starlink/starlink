      SUBROUTINE RANDTEST( STATUS )
*+
*  Name:
*     RANDTEST

*  Purpose:
*     Tests the PDA random-number generators

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL RANDTEST( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine tests the PDA random-number generators for
*     10000-element samples, and using different parameter settings.
*     The mean, standard deviation, skewness, and kurtosis are
*     calculated; these and the expected values are output.  Also output
*     are the first 100 sample eelements.

*  Usage:
*     randtest

*  ADAM Parameters:
*     RANDOM = _LOGICAL (Read)
*        If TRUE, the system clock is used to generate the initial seed.
*        If FALSE, the value of parameter SEED is used. [FALSE]
*     SEED = INTEGER (Read)
*        The initial seed.  This should be 4 * K + 1, where K is a
*        non-negative integer.  Values not satisying this are set to the
*        next lower value that does.  Negative values are treated as 0.
*        SEED is ignored when RANDOM is TRUE. [0]

*  [examples]
*  [optional_A_task_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 November 21 (MJC):
*        Original version based upon the test routine provided with the
*        TOMS599 package.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

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

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine whether a random seed is required.
      RANDOM = .FALSE.
      CALL PAR_GET0L( 'RANDOM', RANDOM, STATUS )
      IF ( RANDOM ) THEN

*  Obtain the computer's time ticks to generate the seed.
         CALL PSX_TIME( TICKS, STATUS )
         SEED = ( TICKS / 4 ) * 4 + 1

*  Obtain the seed value, and validate it.
      ELSE
         CALL PAR_GET0I( 'SEED', SEED, STATUS )
         SEED = ( SEED / 4 ) * 4 + 1
         IF ( SEED .LT. 0 ) SEED = 1
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 999
      
*  Write the main heading.
      CALL MSG_SETI( 'S', SEED )
      CALL MSG_OUT( 'HEADING',
     :  'Listing of trial runs for PDA-random-number routines based '/
     :  /'code by Ahrens/Dieter/Kohrt using seed ^S', STATUS )

*  Trial runs for 5 different cases:
*
*    IDIS = 1:  0,1 -uniform distribution
*    IDIS = 2:  (standard-) exponential distribution
*    IDIS = 3:  (standard-) Normal distribution
*    IDIS = 5:  Poisson-(mu) distribution
      DO IDIS = 1, NDIST
         NRUN = II( IDIS )

*  Write the case heading.
         CALL MSG_BLANK( STATUS )
         CALL MSG_BLANK( STATUS )
         IF ( IDIS .EQ. 1 ) THEN
            CALL MSG_OUT( 'Case1',
     :        '1)   0,1 -Uniform distribution:', STATUS )
            CALL MSG_OUT( 'Case1u', 
     :        '*******************************', STATUS )

         ELSE IF ( IDIS .EQ. 2 ) THEN
            CALL MSG_OUT( 'Case2', 
     :        '2)  (Standard-) exponential distribution:', STATUS )
            CALL MSG_OUT( 'Case2u', 
     :        '*****************************************', STATUS )

         ELSE IF ( IDIS .EQ. 3 ) THEN
            CALL MSG_OUT( 'Case3', 
     :        '3)  (Standard-) Normal distribution:', STATUS )
            CALL MSG_OUT( 'Case3u', 
     :        '************************************', STATUS )

         ELSE IF ( IDIS .EQ. 4 ) THEN
            CALL MSG_OUT( 'Case4', 
     :        '4)  (standard-) gamma-(a) distribution:', STATUS )
            CALL MSG_OUT( 'Case4u', 
     :        '***************************************', STATUS )

         ELSE IF ( IDIS .EQ. 5 ) THEN
            CALL MSG_OUT( 'Case5', 
     :        '5)  Poisson-(mu) distribution (integer samples are '/
     :        /'displayed as reals):', STATUS )
            CALL MSG_OUT( 'Case5u', 
     :        '***************************************************'/
     :        /'********************', STATUS )

         END IF

*  Loop for every parameter value.
         DO IPAR = 1, NRUN

*  Case 4 and 5: set parameter values according to the data vector.
            IF ( IDIS .EQ. 4 ) A = VPAR4( IPAR )
            IF ( IDIS .EQ. 5 ) MU = VPAR5( IPAR )

*  Set the seed for each run and distribution.
            CALL PDA_RNSED( SEED, STATUS )

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
                  SAMPLE( I ) = PDA_RNGAM( A, STATUS )
               END DO
               T1 = A
               T2 = SQRT( A )
               T3 = 2.0 / T2
               T4 = 6.0 / A

*  Write the underlined parameter values.
               IF ( IPAR .NE. 1 ) THEN
                  CALL MSG_BLANK( STATUS )
                  CALL MSG_BLANK( STATUS )
               END IF
               BUFFER = ' '
               BUFFER( 50: ) = 'Gamma-(A) = '
               CALL MSG_SETC( 'BUF', BUFFER )
               CALL MSG_FMTR( 'A', 'E13.6', A )
               CALL MSG_OUT( 'GammaA', '^BUF ^A', STATUS )
               BUFFER( 50: ) = '-------------------------'
               CALL MSG_SETC( 'BUF', BUFFER )
               CALL MSG_OUT( 'GammaAu', '^BUF', STATUS )


*  Poisson-(mu) distribution
            ELSE IF ( IDIS .EQ. 5 ) THEN
               DO I = 1, NRAND
                  SAMPLE( I ) = REAL( PDA_RNPOI( MU, STATUS ) )
               END DO
               T1 = MU
               T2 = SQRT( MU )
               T3 = 1.0 / T2
               T4 = 1.0 / MU

*  Write the underlined parameter values.
               IF ( IPAR .NE. 1 ) THEN
                  CALL MSG_BLANK( STATUS )
                  CALL MSG_BLANK( STATUS )
               END IF
               BUFFER = ' '
               BUFFER( 47: ) = 'Poisson-(MU) = '
               CALL MSG_SETC( 'BUF', BUFFER )
               CALL MSG_FMTR( 'MU', 'E13.6', MU )
               CALL MSG_OUT( 'PoissonMU', '^BUF ^MU', STATUS )
               BUFFER( 47: ) = '----------------------------'
               CALL MSG_SETC( 'BUF', BUFFER )
               CALL MSG_OUT( 'PoissonMUu', '^BUF', STATUS )

            END IF

*  Output the first 100 random deviates for each run.  Integer samples
*  are displayed as reals.
            IF ( IDIS .LE. 3 ) THEN
               CALL MSG_BLANK( STATUS )
               CALL MSG_BLANK( STATUS )
            END IF

            CALL MSG_OUT( 'SHEADING', '  First 100 samples:', STATUS )
            CALL MSG_OUT( 'SHEADINGu', '  ..................', STATUS )
            CALL MSG_BLANK( STATUS )

            DO I = 1, 100, 5
               CALL MSG_FMTR( 'S1', 'E14.6', SAMPLE( I ) )
               CALL MSG_FMTR( 'S2', 'E14.6', SAMPLE( I + 1 ) )
               CALL MSG_FMTR( 'S3', 'E14.6', SAMPLE( I + 2 ) )
               CALL MSG_FMTR( 'S4', 'E14.6', SAMPLE( I + 3 ) )
               CALL MSG_FMTR( 'S5', 'E14.6', SAMPLE( I + 4 ) )
               CALL MSG_OUT( 'VALUES',
     :           '^S1 ^S2 ^S3 ^S4 ^S5', STATUS )
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
            E3 = S3 / SQRT( S2 * S2 * S2 )
            E4 = S4 / ( S2 * S2 ) - 3.0

*  Write the test data heading.
            CALL MSG_BLANK( STATUS )
            CALL MSG_BLANK( STATUS )
            CALL MSG_SETI( 'NRAND', NRAND )
            CALL MSG_OUT( 'TDHEADING',
     :        '  Test Data:     (based on ^NRAND samples)', STATUS )
            CALL MSG_OUT( 'TDHEADINGu', '  ..........', STATUS )

*  Write the statistics' heading.
            CALL MSG_BLANK( STATUS )
            BUFFER = ' '
            BUFFER( 22: ) = 'Mean         Std.dev.       Skewness '/
     :                      /'      Kurtosis'
            CALL MSG_SETC( 'BUF', BUFFER )
            CALL MSG_OUT( 'STATHEADING', '^BUF', STATUS )
            CALL MSG_BLANK( STATUS )

*  Write the statistics.
            WRITE( BUFFER, '(''  True values:'',4E15.6)') T1, T2, T3,
     :        T4
            CALL MSG_SETC( 'BUF', BUFFER )
            CALL MSG_OUT( 'TRUESTAT', '^BUF', STATUS )

            WRITE( BUFFER, '(''  Sample data:'',4E15.6)') E1, E2, E3,
     :        E4
            CALL MSG_SETC( 'BUF', BUFFER )
            CALL MSG_OUT( 'SAMPLESTAT', '^BUF', STATUS )

            CALL MSG_BLANK( STATUS )

*  End of parameter loop.
         END DO

*  End of distribution loop.
      END DO

*  If an error occurred, then report a contextual message.
  999 CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'RANDTEST_ERR',
     :     'RANDTEST: Error testing the PDA random-number routines.',
     :     STATUS )
      END IF

      END
