*+  ARR_REG1<T> - Fills a <TYPE> array with regularly spaced values
      SUBROUTINE ARR_REG1<T>(BASE,INCR,NDAT,ARR,STATUS)
*    Description :
*     Fills <TYPE> array ARR with regularly spaced values BASE+(i-1)*INC.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*     31 Mar 87
*    Type Definitions :
	IMPLICIT NONE
*    Global constants :
        INCLUDE 'SAE_PAR'
*    Import :
      <TYPE> BASE		! Base value
      <TYPE> INCR		! Increment
	INTEGER NDAT		! No of data values
*    Export :
      <TYPE> ARR(NDAT)		! Resulting array
*    Status :
      INTEGER STATUS
*    Local variables :
	INTEGER I
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, NDAT
	ARR(I) = BASE + (I-1)*INCR
      END DO

      END
