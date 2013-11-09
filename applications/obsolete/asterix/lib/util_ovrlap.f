*+  UTIL_OVRLAP - Duration of overlap between two series of intervals
      SUBROUTINE UTIL_OVRLAP(N1,INT1,N2,INT2,OVRLAP)
*    Description :
*     Two series of disjoint intervals are described by the increasing
*     REAL arrays INT1(N1) and INT2(N2). N1 and N2 are even INTEGERS.
*     This subroutine calculates the total duration of the intersection
*     of the two sets of intervals.
*    Invocation :
*     CALL UTIL_OVRLAP(N1,INT1,N2,INT2,OVRLAP)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jim Peden (BHVAD::JCMP)
*    History :
*     1 Nov 84:  original (BHVAD::JCMP)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
	INTEGER N1			! size of interval array 1
	REAL INT1(N1)			! interval array 1
	INTEGER N2			! size of interval array 2
	REAL INT2(N2)			! interval array 2
*    Import-Export :
*    Export :
	REAL OVRLAP			! total overlap
*    Status :
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
	INTEGER I,J

	REAL TI,TIP1			! ith interval
	REAL TJ,TJP1			! jth interval
*    Internal References :
*    Local data :
*-

	I=1
	J=1
	OVRLAP=0.
10	TI=INT1(I)
	TIP1=INT1(I+1)
	TJ=INT2(J)
	TJP1=INT2(J+1)
	IF(TIP1.LE.TJP1) THEN
	   IF(TI.GE.TJ) THEN
*	     INT1 < INT2
	      OVRLAP=OVRLAP+TIP1-TI
	   ELSEIF(TIP1.GT.TJ) THEN
*	     non-empty intersection
	      OVRLAP=OVRLAP+TIP1-TJ
	   ENDIF
*	  Next I
	   I=I+2
	   IF(I.LE.N1) THEN
	      GOTO 10
	   ELSE
	      GOTO 90
	   ENDIF
	ELSE
	   IF(TI.LT.TJ) THEN
*	     INT2 < INT1
	      OVRLAP=OVRLAP+TJP1-TJ
	   ELSEIF(TI.LT.TJP1) THEN
*	     non-empty intersection
	      OVRLAP=OVRLAP+TJP1-TI
	   ENDIF
*	  Next J
	   J=J+2
	   IF(J.LE.N2) THEN
	      GOTO 10
	   ELSE
	      GOTO 90
	   ENDIF
	ENDIF

* End of one or other interval array
90	CONTINUE

	END
