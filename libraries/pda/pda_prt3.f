
      SUBROUTINE PDA_PRT3(MAX,N,XP,X,FP,F)

      DOUBLE PRECISION  XP(*), X(*), FP, F
      INTEGER  N
      LOGICAL  MAX

      WRITE(*,'(''  '')')
      CALL PDA_PRTVEC(X,N,'CURRENT X')
      IF (MAX) THEN
         WRITE(*,'(''  CURRENT F: '',G25.18)') F
      ELSE
         WRITE(*,'(''  CURRENT F: '',G25.18)') -F
      END IF
      CALL PDA_PRTVEC(XP,N,'TRIAL X')
      WRITE(*,'(''  POINT REJECTED SINCE OUT OF BOUNDS'')')

      RETURN
      END
