C+
      SUBROUTINE ECH_ORINIT(NINT,FIRST,LAST,ORDERS)
C
C     E C H _ O R I N I T
C
C     Utility routine that initializes ORDERS array for PAR_RDARY
C     call.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NINT,FIRST,LAST
      REAL ORDERS(NINT)
C
C     Local variables
C
      INTEGER I,NY,DELTA
C
      NY=IABS(LAST-FIRST)+1
      IF (FIRST.LT.LAST) THEN
         ORDERS(1)=FIRST+NY/10
         ORDERS(NINT)=LAST-NY/10
         DELTA=(ORDERS(NINT)-ORDERS(1))/(NINT-1)
         DO I=1,NINT-2,1
            ORDERS(I+1)=FIRST+I*DELTA
         END DO
      ELSE
         ORDERS(1)=FIRST-NY/10
         ORDERS(NINT)=LAST+NY/10
         DELTA=(ORDERS(NINT)-ORDERS(1))/(NINT-1)
         DO I=1,NINT-2,1
            ORDERS(I+1)=FIRST+I*DELTA
         END DO
      END IF
C
C     That's all folks ...
C
      RETURN
      END
