C*GRINIT -- initialize GRPCKG
C+
      SUBROUTINE GRINIT
C
C Initialize GRPCKG and read font file. Called by GROPEN, but may be 
C called explicitly if needed.
C--
C 29-Apr-1996 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER   I
      LOGICAL   INIT
      SAVE      INIT
      DATA      INIT / .TRUE. /
C
      IF (INIT) THEN
         DO 10 I=1,GRIMAX
            GRSTAT(I) = 0
 10      CONTINUE
         CALL GRSY00
         INIT = .FALSE.
      END IF
      RETURN
      END
