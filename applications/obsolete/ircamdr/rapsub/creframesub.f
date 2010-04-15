
*+  CREFRAMESUB - fills an array with different types of test data

      SUBROUTINE CREFRAMESUB( DIMS1, DIMS2, ARRAY, TYPED, MEAN, SIGMA,
     :                        HIGH, LOW, DIRN, STATUS )

*    Description :
*
*     This routine generates artificial data in a DATA_ARRAY for
*     A-task test purposes. Several types of data are available,
*     including random, Poisson, flat, and ramping type data.
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Check to see which kind of data is to be produced
*     For all rows of output image
*        For all pixels in current row
*           Put required data into pixel
*        Endfor
*     Endfor
*     Return
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     29-07-1985 : First implemenation as aid in debugging A-tasks (REVA::MJM)
*     25-10-1985 : Modified to take blank frame option (REVA::MJM)
*     10-12-1985 : Modified to use POISSON subroutine (UKTH::MARK)
*     04-04-1987 : Modified to provide noise of a defined sigma about
*                : a defined mean (UKTH::MJM)
*     20-Jul-1994  Changed VAX-specific RAN function to NAG G05 routines
*                  and removed SEED from arguments of calls   (SKL@JACH)
*     20-Jul-1994  Changed arguments so that DIMS input separately
*                  so that routine will still compile (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT  NONE            ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'        ! global SSE definitions

*    Import :

      INTEGER
     :    DIMS1,            ! dimensions of input array
     :    DIMS2,            ! dimensions of input array
     :    DIRN                  ! direction of ramping

      REAL
     :    LOW,                  ! low intensity value in data
     :    HIGH,                 ! high intensity value in data
     :    MEAN,                 ! mean intensity value in data
     :    SIGMA                 ! sigma to be used in normal distribution

      CHARACTER*2
     :    TYPED                 ! type of data to be generated

*    Export :

      REAL
     :    ARRAY( DIMS1, DIMS2 )    ! data generated

*    Status :

      INTEGER  STATUS           ! global status variable

*    External functions

      REAL PDA_RAND
      EXTERNAL PDA_RAND

*    Local variables :

      REAL
     :    X,                    ! dummy argument for NAG routine
     :    INTER,                ! intensity interval per row/col in ramp
     :    VALUE,                ! value given by RANDOM subroutine
     :    DATA                  ! dummy variable for temporary data storage

      INTEGER
     :    TICKS, SEED,         ! Random number seed
     :    I, J                 ! counters

*-
*    check status on entry - return if not ok
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*    initialise the random number generator seed using system clock
      CALL PSX_TIME( TICKS, STATUS )
      SEED = ( TICKS / 4 ) * 4 + 1
      CALL PDA_RNSED( SEED )

*    check for TYPED and fill array accordingly
      IF( TYPED .EQ. 'RR' ) THEN
                                          ! random between 1 and 0
         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1
               VALUE = PDA_RAND( X )
               ARRAY( I, J ) = VALUE
            END DO
         END DO

      ELSE IF( TYPED .EQ. 'RL' ) THEN
                                          ! random between limits
         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1
               VALUE = PDA_RAND( X )
               VALUE  =  ( VALUE * ( HIGH - LOW ) ) + LOW
               ARRAY( I, J ) = VALUE
            END DO
         END DO

      ELSE IF( TYPED .EQ. 'NM' ) THEN
                                          ! noise of a defined sigma about
                                          ! a defined mean
         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1
               CALL NORMAL( MEAN, DATA, SIGMA, STATUS )
               ARRAY( I, J )  =  DATA
            END DO
         END DO

      ELSE IF( TYPED .EQ. 'RP' ) THEN
                                          ! Poisson noise on mean
         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1
               CALL POISSON( MEAN, DATA, STATUS )
               ARRAY( I, J ) = DATA
            END DO
         END DO

      ELSE IF( TYPED .EQ. 'RA' ) THEN
                                           ! ramp between limits

         IF( DIRN .EQ. 1 ) THEN            ! ramp L - R
            INTER  = ( HIGH - LOW ) / ( DIMS1  - 1 )
            DO  I  =  1, DIMS1
               DO  J  =  1, DIMS2
                  ARRAY( I, J ) = LOW + ( INTER *  ( I - 1 ) )
               END DO
            END DO

         ELSE IF( DIRN .EQ. 2 ) THEN       ! ramp R - L
            INTER = ( HIGH - LOW ) / ( DIMS1 - 1 )
            DO  I  =  1, DIMS1
               DO  J  =  1, DIMS2
                  ARRAY( I, J ) = HIGH - ( INTER * ( I - 1 ) )
               END DO
            END DO

         ELSE IF( DIRN .EQ. 3 ) THEN       ! ramp B - T
            INTER = ( HIGH - LOW ) / ( DIMS2  - 1 )
            DO  J  =  1, DIMS2
               DO  I  =  1, DIMS1
                  ARRAY( I, J ) = LOW + ( INTER * ( J - 1 ) )
               END DO
            END DO

         ELSE IF( DIRN .EQ. 4 ) THEN       ! ramp T - B
            INTER = ( HIGH - LOW ) / ( DIMS2  - 1 )
            DO  J  =  1, DIMS2
               DO  I  =  1, DIMS1
                  ARRAY( I, J ) = HIGH - ( INTER * ( J - 1 ) )
               END DO
            END DO

         END IF

      ELSE IF( TYPED .EQ. 'FL' ) THEN
                                           ! flat all over array
         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1
               ARRAY( I, J ) = MEAN
            END DO
         END DO

      ELSE IF( TYPED .EQ. 'BL' ) THEN
                                           ! blank array - all zeroes
         DO  J  =  1, DIMS2
            DO  I  =  1, DIMS1
               ARRAY( I, J ) = 0.0
            END DO
         END DO

      END IF


*    end and return
      END
