*+  NORMAL - takes a value and returns a normally distributed noisy
*            version

      SUBROUTINE NORMAL ( INVAL, SIGMA, SEED, OUTVAL, STATUS )
*
*    Description :
*
*     This routine takes as input a number and returns a number
*     which is the input number plus or minus a random amount
*     of normally distributed noise. Uses a Box-Mueller algorithm
*     to generate a fairly good normal distribution.
*
*    Invocation :
*
*     CALL NORMAL( INVAL, SIGMA, SEED, OUTVAL, STATUS )
*
*    Arguments :
*
*     INVAL  =  REAL( READ )
*         Input value to which noise is to be added
*     SIGMA  =  REAL ( READ )
*         Standard deviation of  normal distribution
*     SEED  =  REAL ( READ, WRITE )
*         Seed for random number generator.
*     OUTVAL  =  REAL ( WRITE )
*         Output value which has random noise added
*     STATUS = INTEGER ( READ )
*         Global status value
*
*    Method :
*
*     If error on entry 
*        OUTVAL = INVAL
*     Else
*        Initialise finished flag to false
*        Do while not finished
*          Get random numbers from the seed
*          If valid numbers generated
*             Modify the input value according to the random
*              numbers and the Box-Mueller transform
*             Return this value in OUTVAL
*             Set finished flag to true
*          Endif
*        Enddo
*     Endif
*     Return
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm Currie RAL ( UK.AC.RL.STAR::CUR )
*
*    History :
*
*     14-02-1986 : First implementation (REVA::MJM)  
*     1986 Aug 12: Completed prologue and nearly conformed to 
*                  Starlink standards (RL.STAR::CUR).
*     1986 Sep 2 : Renamed parameter section arguments and tidied
*                  (RL.STAR::CUR).
*     1992 Mar 17: Used portable random-number generation (RAL::CUR).
*
*    Type definitions :

      IMPLICIT  NONE            ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'        ! SSE global definitions

*    Import :

      REAL
     :     INVAL,
     :     SIGMA

*    Import-Export :

      REAL
     :     SEED

*    Export :

      REAL
     :     OUTVAL

*    Status :

      INTEGER  STATUS

*    External References:
      REAL SLA_RANDOM           ! Random-number generator

*    Local variables :

      REAL
     :     RA, RB, RR, R        ! random numbers used by algorithm

      LOGICAL
     :     FINSHD               ! flag variable used to check that a
                                ! valid random number has been generated

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN
         OUTVAL  =  INVAL
      ELSE

*       initialise logical flag

         FINSHD  =  .FALSE.

*       loop until a valid random number has been generated

         DO WHILE ( .NOT. FINSHD )

*          generate two uncorrelated random numbers between -1 and 1.
*          SLA_RANDOM returns values in the range 0 to 1.

            RA  =  -1.0 + 2.0 * SLA_RANDOM( SEED )
            RA  =  -1.0 + 2.0 * SLA_RANDOM( SEED )
            RB  =  -1.0 + 2.0 * SLA_RANDOM( SEED )
            RB  =  -1.0 + 2.0 * SLA_RANDOM( SEED )

*          get another from these that will lie between 0 and 2

            RR  =  RA*RA + RB*RB

*          accept only those that lie between 0 and 1 - otherwise start
*          again

            IF ( RR .LT. 1.0 ) THEN

*             generate the Box-Mueller transform

               R  =  SQRT( -2.0 * ALOG( RR ) / RR )
               OUTVAL  =  ( ABS( SIGMA ) * RA * R ) + INVAL

*             set the finished flag to true - exit now

               FINSHD  =  .TRUE.

            END IF

         END DO

      END IF

*    return and end

      END

