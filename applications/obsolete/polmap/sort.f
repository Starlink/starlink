      SUBROUTINE SORT(NPTS,ARRAY)
C+
C
C Subroutine: 
C
C    S O R T
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C NPTS (<), ARRAY (><)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C
C Heapsort algorithm 
C
C
C
C
C
C-
      IMPLICIT NONE
      REAL ARRAY(*),SWAP
      INTEGER A,NPTS,M,I,J
      A = NPTS/2+1
      M = NPTS
      DO WHILE(.TRUE.)
C
        IF(A.GT.1)THEN
          A = A-1
          SWAP = ARRAY(A)
        ELSE
          SWAP = ARRAY(M)
          ARRAY(M) = ARRAY(1)
          M = M-1
          IF(M.EQ.1)THEN
            ARRAY(1) = SWAP
            GOTO 999  ! Finished...
          ENDIF
        ENDIF
C
        I = A
        J = A+A
        DO WHILE (J.LE.M)
          IF(J.LT.M)THEN
            IF(ARRAY(J) .LT. ARRAY(J+1)) J = J+1
          ENDIF
          IF(SWAP.LT.ARRAY(J))THEN
            ARRAY(I) = ARRAY(J)
            I = J
            J = J+J
          ELSE
            J = M+1
          ENDIF
        END DO
        ARRAY(I) = SWAP
      ENDDO
 999  CONTINUE
      END
