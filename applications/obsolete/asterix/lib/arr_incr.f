*+  ARR_INCR - Test whether a _REAL array is increasing
      SUBROUTINE ARR_INCR(N,ARRAY,INCR)
*    Description :
*     The _REAL array ARRAY with N values is tested and the _LOGICAL
*     variable INCR is returned .TRUE. if ARRAY(I+1).GE.ARRAY(I) for all
*     I:1.LE.I.LT.N
*    Invocation :
*     CALL ARR_INCR(N,ARRAY,INCR)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jim Peden (BHVAD::JCMP)
*    History :
*     26 Sep 84:  original (BHVAD::JCMP)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
	INTEGER N			! size of array
	REAL ARRAY(N)			! array to be tested
*    Import-Export :
*    Export :
	LOGICAL INCR			! array values increasing
*    Status :
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
	INTEGER I
*    Internal References :
*    Local data :
*-

	IF(N.LE.1) THEN
	   INCR=.TRUE.
	ELSE
	   DO I=1,N-1
	      INCR=ARRAY(I+1).GE.ARRAY(I)
	      IF(.NOT.INCR) GOTO 99
	   ENDDO
	ENDIF

99	END
