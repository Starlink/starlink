
      SUBROUTINE PDA_PRT4(MAX,N,XP,X,FP,F)

      DOUBLE PRECISION  XP(*), X(*), FP, F
      INTEGER  N
      LOGICAL  MAX

      WRITE(*,'(''  '')')
      CALL PDA_PRTVEC(X,N,'CURRENT X')
      IF (MAX) THEN
         WRITE(*,'(''  CURRENT F: '',G25.18)') F
         CALL PDA_PRTVEC(XP,N,'TRIAL X')
         WRITE(*,'(''  RESULTING F: '',G25.18)') FP
      ELSE
         WRITE(*,'(''  CURRENT F: '',G25.18)') -F
         CALL PDA_PRTVEC(XP,N,'TRIAL X')
         WRITE(*,'(''  RESULTING F: '',G25.18)') -FP
      END IF

      RETURN
      END
