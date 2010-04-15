*-----------------------------------------------------------------------

      SUBROUTINE SPIKE (NSR, ISR, DATA, NLOW, NHIGH, SRTOL, NBPTS, BAD)

C    Removes up to 16 spikes on points interval (NLOW,NHIGH)
C    Channels with spikes are set to bad channel value.

      IMPLICIT  NONE

C     Formal parameters

      INTEGER   NSR               ! Returns # of spikes removed
      INTEGER   ISR(*)            ! Returns channel #s of removed spikes
      REAL      DATA(*)
      INTEGER   NLOW, NHIGH       ! Points interval
      REAL      SRTOL             ! Spike tolerance
      INTEGER   NBPTS             ! Length of DATA array
      REAL      BAD               ! Bad channel value

C     Local variables

      INTEGER   J
      LOGICAL   LSP, RSP
      LOGICAL   LOK, ROK

C  Ok, go...


      NSR = 0

      DO 20 J = NLOW, NHIGH
        IF (DATA(J).NE.BAD) THEN

          LOK = J.GE.2       .AND. DATA(J-1).NE.BAD
          LSP = ABS(DATA(J)-DATA(J-1)).GT.SRTOL .AND. LOK
          ROK = J.LE.NBPTS-1 .AND. DATA(J+1).NE.BAD
          RSP = ABS(DATA(J)-DATA(J+1)).GT.SRTOL .AND. ROK

          IF (LSP .AND. RSP) THEN
            IF (DATA(J).LT.AMAX1(DATA(J-1),DATA(J+1))
     &          .AND. DATA(J).GT.AMIN1(DATA(J-1),DATA(J+1))) GO TO 20
            NSR = NSR + 1
            ISR(NSR) = J
            DATA(J) = BAD
            IF (NSR.EQ.16) GO TO 50
          ELSE IF (LSP .AND..NOT.ROK) THEN
            DATA(J)  = BAD
            NSR      = NSR+1
            ISR(NSR) = J
            IF (NSR.EQ.16) GO TO 50
          ELSE IF (RSP .AND..NOT.LOK) THEN
            DATA(J)  = BAD
            NSR      = NSR+1
            ISR(NSR) = J
            IF (NSR.EQ.16) GO TO 50
          END IF
        END IF

20    CONTINUE

50    RETURN
      END


