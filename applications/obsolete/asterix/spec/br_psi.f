*+  BR_PSI - Gould's bremmstrahlung electron-electron correction
      DOUBLE PRECISION FUNCTION BR_PSI(A,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Andy Pollock (BHVAD::AMTP)
*    History :
*     20 Aug 85 : Original
*     14 Jul 88 : Amended to cope with high values of E/kT (BHVAD::RDJ)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
*
*    Import :
*
      DOUBLE PRECISION A
*
*    Status :
*
      INTEGER STATUS
*
*    Local constants :
*
      REAL        C2
        PARAMETER (C2=14.630)
*
*    Function declarations :
*
      DOUBLE PRECISION BR_K0,BR_K1   ! modified Bessel functions of the second kind
      DOUBLE PRECISION BR_DEXP       ! Simple exponential
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( A .LT. 75.0 ) THEN
        BR_PSI = 3.0D0/(4.0D0*A)+BR_K1(A,STATUS)/BR_K0(A,STATUS)
     :                + C2*BR_DEXP(-A)/(16.0D0*A*BR_K0(A,STATUS))

*    Approximation for large E/kT
      ELSE
        BR_PSI=1.+(C2/16.)*SQRT(2/(MATH__PI*A))+5./(4.*A)

      END IF

      END
