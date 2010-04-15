
*+  POISSON - takes a value and returns a Poisson noisy version

      SUBROUTINE POISSON ( INVALUE, OUTVALUE, STATUS )

*    Description :
*
*     This routine takes as input a number and returns a number
*     which is the input number plus or minus a random amount
*     of Poisson type (shot) noise. Uses a Box-Mueller algorithm
*     to generate a fairly good normal distribution.
*
*    Invocation :
*
*     CALL POISSON( INVALUE, OUTVALUE, SEED, STATUS )
*
*    Method :
*
*     If error on entry
*        OUTVALUE = INVALUE
*        Return
*     Endif
*     Initialise finished flag to false
*     Do while not finished
*       Get random numbers from the seed
*       If valid numbers generated
*          Modify the input value according to the random
*           numbers, Box Mueller transform, and Poisson noise law
*          Return this value in OUTVALUE
*          Set finished flag to true
*       Endif
*     Enddo
*     Return
*
*    Deficiencies :
*
*     Uses Vax specific routine RAN, and has to overcome the bug that
*     odd and even random numbers generated are correlated.
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     09-12-1985 : First implementation (UKTH::MARK)
*     14-02-1986 : Much better normal distribution algorithm used (REVA::MJM)
*     20-Jul-1994 Changed VAX-specific RAN function to NAG routine,
*                 changed arguments as SEED no longer required (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      REAL
     :     INVALUE                ! input value

*    Import-Export :

*    Export :

      REAL
     :     OUTVALUE               ! output value

*    Status :

      INTEGER  STATUS             ! global status parameter
      INTEGER TICKS, SEED         ! For random seed

*    External function

      REAL PDA_RAND
      EXTERNAL PDA_RAND

*    Local variables :

      REAL
     :     X, Y,                  ! dummy arguments for NAG function
     :     RA, RB, RR, R          ! random numbers used by algorithm

      LOGICAL
     :     FINISHED               ! flag variable used to check that a
                                  ! valid random number has been generated

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         OUTVALUE  =  INVALUE
         RETURN
      ENDIF

*    initialise the random number generator seed using system clock

      CALL PSX_TIME( TICKS, STATUS )
      SEED = ( TICKS / 4 ) * 4 + 1
      CALL PDA_RNSED( SEED )

*    initialise logical flag
      FINISHED  =  .FALSE.

*    loop until a valid random number has been generated
      DO WHILE( .NOT. FINISHED )

*       generate two uncorrelated random numbers between -1 and 1
          X  = PDA_RAND( X )
         RA  =  -1.0 + 2.0*X
          X  = PDA_RAND( X )
         RA  =  -1.0 + 2.0*X
          Y  = PDA_RAND( Y )
         RB  =  -1.0 + 2.0*Y
          Y  = PDA_RAND( Y )
         RB  =  -1.0 + 2.0*Y

*       get another from these that will lie between 0 and 2
         RR  =  RA**2 + RB**2

*       accept only those that lie between 0 and 1 - otherwise start again
         IF( RR .LT. 1.0 ) THEN

*          generate the Box-Mueller transform
            R  =  SQRT( -2.0*ALOG( RR ) / RR )
            OUTVALUE  =  ( SQRT( ABS( INVALUE ) ) * RA * R ) + INVALUE

*          set the finished flag to true - exit now
            FINISHED  =  .TRUE.

         END IF

      END DO


*    return and end

      END
