*+  BR_F - Gould's bremmstrahlung electron-ion correction
      DOUBLE PRECISION FUNCTION BR_F( A, STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Andy Pollock (BHVAD::AMTP)
*    History :
*     20 Aug 85 : Original
*     14 Jul 88 : Amended to cope with high values of E/kT (BHVAD::RDJ)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      DOUBLE PRECISION A
*    Status :
      INTEGER STATUS
*    Function declarations :
      DOUBLE PRECISION BR_K0,BR_K1,BR_K2     ! modified Bessel functions of the second kind
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF(A.LT.75.)THEN
        BR_F = 1.0D0/(4.0D0*A)+3.0D0*BR_K1(A,STATUS)/BR_K0(A,STATUS)
     :           +A*(1.D0-BR_K2(A,STATUS)/BR_K0(A,STATUS))

*    Approximation if E/2kT becomes to large
      ELSE
        BR_F=1.D0+(3.D0/(4.D0*A))-(1/(8.D0*A*A))

      END IF

      END
