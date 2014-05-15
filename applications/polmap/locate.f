      SUBROUTINE LOCATE(XA,N,X,I)
C+
C
C Subroutine:
C
C     L O C A T E
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C XA (<), N (<), X (<), I (>)
C
C History:
C
C   May 1994 Created
C
C
C Finds the index I of the array XA so that xa(I) < x < xa(I+1)
C Bisection algorithm
C
C
C
C
C
C-
      IMPLICIT NONE
      INTEGER N,I,JLOWER,JUPPER,JMID
      REAL XA(*)
      REAL X

      JLOWER = 0
      JUPPER = N+1

      DO WHILE(JUPPER-JLOWER.GT.1)
        JMID = (JUPPER+JLOWER)/2
C
        IF((XA(N).GT.XA(1)).EQV.(X.GT.XA(JMID)))THEN
          JLOWER = JMID
        ELSE
          JUPPER = JMID
        ENDIF
C
      ENDDO
      I = JLOWER
      END
