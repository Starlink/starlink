*+  CRFRSB - fills a 2-d array with different types of test data

      SUBROUTINE CRFRSB( DIM1, DIM2, TYPED, MEAN, HIGH, LOW, DIRN,
     :                   SIGMA, ARRAY, STATUS )
*
*    Description :
*
*     This routine generates artificial data in a 2-d DATA_ARRAY for
*     A-task test purposes. Several types of data are available,
*     including random, Poissonian- and Gaussian-noise, flat, and
*     ramping-type data.
*
*    Invocation :
*
*     CALL CRFRSB( DIM1, DIM2, TYPED, MEAN, HIGH, LOW, DIRN, SIGMA,
*    :             ARRAY, STATUS )
*
*    Arguments :
*
*     DIM1 = INTEGER( READ )
*         The first dimension of the generated 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the generated 2-d array.
*     TYPED  =  CHAR*2( READ )
*         Type of data generated: RR - random between 0 and 1;
*         RL - random between limits; RP - Poissonian noise about
*         mean; RA - ramp between limits; FL - flat; BL - zeroes;
*         GN - Gaussian noise about mean.
*     MEAN  =  REAL( READ )
*         Mean pixel value in data
*     HIGH  =  REAL( READ )
*         High value in data to define limits
*     LOW  =  REAL( READ )
*         Low value in data to define limits
*     DIRN  =  INTEGER( READ )
*         Direction of ramping: 1 - left to right; 2 - right to left;
*         3 - bottom to top; 4 - top to bottom.
*     SIGMA =  REAL( READ )
*         Standard deviation to be used in normal distribution
*     ARRAY ( DIM1, DIM2 )  =  REAL( WRITE )
*         Generated array of test data
*     STATUS  =  INTEGER( READ )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Generate seed for random numbers
*     Check to see which kind of data is to be produced
*        For all lines of output image
*           For all pixels in current line
*              Put required data into pixel
*           Endfor
*        Endfor
*     Endif
*     End
*
*    Deficiencies :
*
*     Poissonian noise is artificial. No checks are made that the ramps
*     or random limits will not generate bad pixels.
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     29-07-1985 : First implemenation as aid in debugging A-tasks
*                : (REVA::MJM)
*     25-10-1985 : Modified to take blank frame option (REVA::MJM)
*     10-12-1985 : Modified to use POISSON subroutine (UKTH::MARK)
*     1986 Aug 13: Renamed from CREFRAMESUB, completed the prologue,
*                  POISSON call revised and nearly conformed to
*                  Starlink programming standards (RL.STAR::CUR).
*     1986 Sep 4 : Renamed parameters section in prologue to arguments,
*                  completed prologue and tidied (RL.STAR::CUR).
*     1988 May 23: Added normal noise option (RL.STAR::CUR).
*     1988 Jul 27: Reordered do loops for RA mode L-R and R-L
*                  (RL.STAR::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*     1992 Mar 17: Used portable random-number generation (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT  NONE            ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'        ! global SSE definitions

*    Import :

      INTEGER
     :    DIM1, DIM2,
     :    DIRN

      REAL
     :    LOW,
     :    HIGH,
     :    SIGMA,
     :    MEAN

      CHARACTER*2
     :    TYPED

*    Export :

      REAL
     :    ARRAY( DIM1, DIM2 )

*    Status :

      INTEGER  STATUS

*    External References:
      REAL SLA_RANDOM           ! Random-number generator
      REAL KPG1_SEED            ! Random-number seed initialisation

*    Local variables :

      REAL
     :    DATA,                ! Dummy variable for temporary data
                               ! storage
     :    INTER,               ! Intensity interval per line/col in ramp
     :    SEED,                ! Random number generator seed
     :    VALUE                ! Value given by random subroutine

      INTEGER
     :    I, J                 ! General array counter variables

*-
*    Error check on entry - return if not ok

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise the random-number generator seed.  It is taken as input
*    by SLA_RANDOM, which returns a pseudo-random number between 0 and
*    1, and is updated on return. Use SLA_RANDOM once here to fully
*    randomize numbers generated for arrays below.

      SEED = KPG1_SEED( STATUS )
      VALUE = SLA_RANDOM( SEED )

*    Check for TYPED and fill array accordingly

*    Random between 0 and 1

      IF( TYPED .EQ. 'RR' ) THEN            

         DO  J  =  1, DIM2
            DO  I  =  1, DIM1
               VALUE = SLA_RANDOM( SEED )
               ARRAY( I, J ) = VALUE
            END DO
         END DO

*    Random between limits

      ELSE IF( TYPED .EQ. 'RL' ) THEN       

         DO  J  =  1, DIM2
            DO  I  =  1, DIM1
               VALUE = SLA_RANDOM( SEED )
               VALUE  =  ( VALUE * ( HIGH - LOW ) ) + LOW
               ARRAY( I, J ) = VALUE
            END DO
         END DO

*    Poissionian noise on mean - artificial at present - to be revised

      ELSE IF ( TYPED .EQ. 'RP' ) THEN 

         DO  J  =  1, DIM2
            DO  I  =  1, DIM1
               DATA = MEAN
               CALL KPG1_POISR( 1, DATA, SEED, STATUS )
               ARRAY( I, J ) = DATA
            END DO
         END DO

*    Ramp between limits

      ELSE IF ( TYPED .EQ. 'RA' ) THEN     

*       Ramp L - R

         IF ( DIRN .EQ. 1 ) THEN
            INTER  = ( HIGH - LOW ) / ( DIM1 - 1 )
            DO  J  =  1, DIM2
               DO  I  =  1, DIM1
                  ARRAY( I, J ) = LOW + ( INTER *  ( I - 1 ) )
               END DO
            END DO

*       Ramp R - L

         ELSE IF ( DIRN .EQ. 2 ) THEN
            INTER = ( HIGH - LOW ) / ( DIM1 - 1 )
            DO  J  =  1, DIM2
               DO  I  =  1, DIM1
                  ARRAY( I, J ) = HIGH - ( INTER * ( I - 1 ) )
               END DO
            END DO

*       Ramp B - T

         ELSE IF ( DIRN .EQ. 3 ) THEN
            INTER = ( HIGH - LOW ) / ( DIM2 - 1 )
            DO  J  =  1, DIM2
               DO  I  =  1, DIM1
                  ARRAY( I, J ) = LOW + ( INTER * ( J - 1 ) )
               END DO
            END DO

*       Ramp T - B

         ELSE IF ( DIRN .EQ. 4 ) THEN
            INTER = ( HIGH - LOW ) / ( DIM2 - 1 )
            DO  J  =  1, DIM2
               DO  I  =  1, DIM1
                  ARRAY( I, J ) = HIGH - ( INTER * ( J - 1 ) )
               END DO
            END DO

         END IF

*    Flat over whole array

      ELSE IF ( TYPED .EQ. 'FL' ) THEN     

         DO  J  =  1, DIM2
            DO  I  =  1, DIM1
               ARRAY( I, J ) = MEAN
            END DO
         END DO

*    Blank array - all zeroes

      ELSE IF ( TYPED .EQ. 'BL' ) THEN

         DO  J  =  1, DIM2
            DO  I  =  1, DIM1
               ARRAY( I, J ) = 0.0
            END DO
         END DO

*    Gaussian noise around a fixed mean

      ELSE IF ( TYPED .EQ. 'GN' ) THEN

         DO  J  =  1, DIM2
            DO  I  =  1, DIM1
               CALL NORMAL( MEAN, SIGMA, SEED, DATA, STATUS )
               ARRAY( I, J ) = DATA
            END DO
         END DO

      END IF

*    End and return

      END
