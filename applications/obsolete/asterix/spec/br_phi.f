*+  BR_PHI - Gould's bremmstrahlung Born approximation correction
      DOUBLE PRECISION FUNCTION BR_PHI( A, STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Andy Pollock (BHVAD::AMTP)
*    History :
*
*     20 Aug 85 : Original
*     14 Jul 88 : Amended to cope with high values of E/kT (BHVAD::RDJ)
*     13 Jan 93 : Converted to D.P. (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
*    Import :
      DOUBLE PRECISION A
*    Status :
      INTEGER STATUS
*
*    Function declarations :
*
      DOUBLE PRECISION BR_DEXP
      DOUBLE PRECISION BR_K0,BR_K1	! modified Bessel functions of the second kind
      DOUBLE PRECISION S15ADF		! NAG complementary error function
*    Local variables
      INTEGER FAIL                ! NAG failure code
      DOUBLE PRECISION EMA,ERFC       !
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      FAIL=0
      ERFC = S15ADF( SQRT(2.0D0*A), FAIL )
      IF(FAIL.EQ.0)THEN
        IF(A.LT.40.)THEN
           EMA = BR_DEXP(-A)
           BR_PHI=MATH__PI*((1.+SQRT(MATH__PI*A))*ERFC-
     :             EMA*EMA/SQRT(2.))/
     :             (2.*A*EMA*(BR_K1(A,STATUS)-BR_K0(A,STATUS)))

*      Approximate expression used at high A values
        ELSE
          BR_PHI = 1.0D0-(SQRT(MATH__PI/A)/4.)+(0.125/A)
        END IF

      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP('NAG_ERROR',
     :                'IFAIL non-zero on exit from S15ADF',
     :                STATUS)
      END IF

      END
