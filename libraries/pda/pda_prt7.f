
      SUBROUTINE PDA_PRT7(MAX)

      LOGICAL  MAX

      IF (MAX) THEN
         WRITE(*,'(''  LOWER POINT REJECTED'')')
      ELSE
         WRITE(*,'(''  HIGHER POINT REJECTED'')')
      END IF

      RETURN
      END
