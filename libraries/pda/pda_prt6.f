
      SUBROUTINE PDA_PRT6(MAX)

      LOGICAL  MAX

      IF (MAX) THEN
         WRITE(*,'(''  THOUGH LOWER, POINT ACCEPTED'')')
      ELSE
         WRITE(*,'(''  THOUGH HIGHER, POINT ACCEPTED'')')
      END IF

      RETURN
      END
