*+  BR_K2 - Modified Bessel function of the second kind
      DOUBLE PRECISION FUNCTION BR_K2(A,STATUS)
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Andy Pollock (BHVAD::AMTP)
*    History :
*
*     20 Aug 85 : Original
*     13 Jan 93 : Remembers last value, converted to D.P. (DJA)
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
      DOUBLE PRECISION BR_K0,BR_K1
*
*    Local variables :
*
      DOUBLE PRECISION LAST_A,LAST_K2   ! Last A and K2 in this routine
*
*    Local data :
*
      DATA    LAST_A/-1.0D0/		! Undefined for this value
*
*    Preserve across calls :
*
      SAVE    LAST_A,LAST_K2
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Different from last time?
      IF ( A .NE. LAST_A ) THEN
        LAST_A = A
        LAST_K2 = BR_K0(A,STATUS) + BR_K1(A,STATUS)*2.0D0/A
      END IF

      BR_K2 = LAST_K2

      END
