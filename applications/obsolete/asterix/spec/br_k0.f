*  Functions needed by Gould's Bremsstrahlung formulation
*+ BR_K0 - Modified Bessel function of the second kind
      DOUBLE PRECISION FUNCTION BR_K0( A, STATUS )
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Andy Pollock (BHVAD::AMTP)
*     Richard Beard (Birmingham)
*
*    History :
*
*     20 Aug 85 : Original
*     13 Jan 93 : Remembers last value, and converted to D.P. (DJA)
*     23 Jun 97 : Replace NAG with ASTPDA (RB)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      DOUBLE PRECISION A
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      DOUBLE PRECISION PDA_MBESK0
*
*    Local variables :
*
      DOUBLE PRECISION LAST_A		! Last argument
      DOUBLE PRECISION LAST_K0         	! Last K0 in this routine
      INTEGER FAIL                   	! PDA failure code
*
*    Local data :
*
      DATA    LAST_A/-1.0D0/		! Undefined for this value
*
*    Preserve across calls :
*
      SAVE    LAST_A,LAST_K0
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) THEN
        BR_K0 = 0.0D0
        RETURN
      END IF

*    Different from last time?
      IF ( A .NE. LAST_A ) THEN

        LAST_A = A
        FAIL=0
        LAST_K0 = PDA_MBESK0(A,FAIL)
        IF ( FAIL .NE. 0 ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ',
     :         'IFAIL non-zero on exit from PDA routine PDA_MBESK0',
     :         STATUS )
        END IF
      END IF

      BR_K0 = LAST_K0

      END
