*+  UTIL_WEIGHT - Weight data
      SUBROUTINE UTIL_WEIGHT(ALG,N,A,S,B,STATUS)
*    Description :
*     The N data points in the REAL array A are weighted according to
*     the contents of the array S(N) according to the algorithm ALG:
*
*          ALG=1 implies B(I)=A(I)/S(I) (Inverse variance)
*
*    Invocation :
*     CALL UTIL_WEIGHT(ALG,N,A,S,B)
*    Parameters :
*     ALG=INTEGER(R)
*           algorithm to be used
*     N=INTEGER(R)
*           no. of data points
*     A(N)=REAL(R)
*           source data array
*     S(N)=REAL(R)
*           array of weights
*     B(N)=REAL(W)
*           destination data array
*     STATUS=INTEGER(U)
*    Method :
*     Select algorithm
*     For I = 1 to N
*        apply algorithm to A(I),S(I),B(I)
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jim Peden (BHVAD::JCMP)
*    History :
*     27 Jan 86:  original (BHVAD::JCMP)
*     17 Feb 86:  bug fix - divide by zero (BHVAD::JCMP)
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
	IF(STATUS.NE.SAI__OK) GOTO 9000

* Select algorithm
	IF(ALG.EQ.1) THEN
*	  Algorithm 1: inverse variance
	   DO I=1,N
	      IF(S(I).GT.0.) THEN
	         B(I)=A(I)/S(I)
	      ELSE
	         B(I)=A(I)
	      ENDIF
	   ENDDO
	ELSE
*	  Unknown algorithm
	   STATUS=SAI__ERROR
	   CALL ERR_REP('BAD_ALG','Unknown algorithm',STATUS)
	ENDIF

9000	IF(STATUS.NE.SAI__OK) THEN
	   CALL ERR_REP('S','From UTIL_WEIGHT',STATUS)
	ENDIF

	END
