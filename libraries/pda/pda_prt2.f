
      SUBROUTINE PDA_PRT2(MAX,N,X,F)

      DOUBLE PRECISION  X(*), F
      INTEGER  N
      LOGICAL  MAX

      WRITE(*,'(''  '')')
      CALL PDA_PRTVEC(X,N,'INITIAL X')
      IF (MAX) THEN
         WRITE(*,'(''  INITIAL F: '',/, G25.18)') F
      ELSE
         WRITE(*,'(''  INITIAL F: '',/, G25.18)') -F
      END IF

      RETURN
      END
