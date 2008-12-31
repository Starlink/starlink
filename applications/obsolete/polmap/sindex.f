      SUBROUTINE SINDEX(N,INARRAY,IX)
C+
C
C Subroutine: 
C
C  S I N D E X
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C N (<), INARRAY (<), IX (>)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C  Indexed sorting routine
C
C
C
C-

      IMPLICIT NONE
      INTEGER N
      REAL INARRAY(*)
      INTEGER IX(*)
      INTEGER I,KN,IN,LN,JN
      REAL Q
C
      DO KN=1,N
        IX(KN)=KN
      ENDDO
C
      LN=N/2+1
      IN=N
C
        DO WHILE(.TRUE.)

        IF(LN.GT.1)THEN
          LN=LN-1
          JN=IX(LN)
          Q=INARRAY(JN)
        ELSE
          JN=IX(IN)
          Q=INARRAY(JN)
          IX(IN)=IX(1)
          IN=IN-1
C
          IF (IN .EQ. 1) THEN
            IX(1)=JN
            GOTO 666 ! End
          ENDIF
C
        ENDIF
C
        I=LN
        KN=2*LN
C
        DO WHILE(KN.LE.IN)
C
          IF(KN.LT.IN)THEN
            IF(INARRAY(IX(KN)) .LT. INARRAY(IX(KN+1)))KN=KN+1
          ENDIF
C
          IF(Q.LT.INARRAY(IX(KN)))THEN
            IX(I)=IX(KN)
            I=KN
            KN=KN+KN
          ELSE
            KN=IN+1
          ENDIF
C
        ENDDO
        IX(I)=JN
      ENDDO
 666  CONTINUE
      END



