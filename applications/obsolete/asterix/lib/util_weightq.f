*+  UTIL_WEIGHTQ - Weight data with quality taken into account
      SUBROUTINE UTIL_WEIGHTQ(ALG,N,A,Q,S,B,STATUS)
*    Description :
*     The N data points in the REAL array A are weighted according to
*     the contents of the array S(N) according to the algorithm ALG:
*
*          ALG=1 implies B(I)=A(I)/S(I) (Inverse variance)
*
*    Invocation :
*     CALL UTIL_WEIGHTQ(ALG,N,A,Q,S,B,STATUS)
*    Parameters :
*     ALG=INTEGER(R)
*           algorithm to be used
*     N=INTEGER(R)
*           no. of data points
*     A(N)=REAL(R)
*           source data array
*     Q(N)=LOGICAL(R)
*           source data quality
*     S(N)=REAL(R)
*           array of weights
*     B(N)=REAL(W)
*           destination data array
*     STATUS=INTEGER(U)
*    Method :
*     Select algorithm
*     For I = 1 to N
*        apply algorithm to A(I),S(I),B(I) if Q(I) = TRUE
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jim Peden (BHVAD::JCMP)
*    History :
*     27 Jan 86 : Original (BHVAD::JCMP)
*     17 Feb 86 : Bug fix - divide by zero (BHVAD::JCMP)
*     24 Aug 89 : Quality version ( BHVAD::DJA )
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
	INTEGER ALG		! algorithm number
	INTEGER N		! # data values
	REAL A(N)		! input data
	REAL S(N)		! weights data
        LOGICAL Q(N)            ! quality data
*    Import-Export :
*    Export :
	REAL B(N)		! output data
*    Status :
	INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
	INTEGER I
*    Internal References :
*    Local data :
*-

* Status check
	IF ( STATUS .NE. SAI__OK ) RETURN

* Select algorithm
	IF(ALG.EQ.1) THEN
*	  Algorithm 1: inverse variance
	   DO I=1,N
              IF ( Q(I) ) THEN
	         IF( S(I) .GT. 0.0 ) THEN
	            B(I)=A(I)/S(I)
	         ELSE
	            B(I)=A(I)
	         END IF
              END IF
	   END DO
	ELSE
*	  Unknown algorithm
	   STATUS=SAI__ERROR
	   CALL ERR_REP('BAD_ALG','Unknown algorithm',STATUS)
	ENDIF

9000	IF(STATUS.NE.SAI__OK) THEN
	   CALL ERR_REP('S','From UTIL_WEIGHTQ',STATUS)
	ENDIF

	END
