      PROGRAM SGSPAR_TEST
      INTEGER STATUS
      STATUS = 0

*  Simple test to see that the SGSPAR routine are available for linking

      PRINT *,'This test program only links against the SGSPAR routines'
      PRINT *,'It does not actually call them.'
     ://' not needed.'
      IF( STATUS .NE. -12345 ) GOTO 999
      CALL SGS_ANNUL
      CALL SGS_ASSOC
      CALL SGS_CANCL
      CALL SGS_DEACT

  999 CONTINUE
      print *,'ending'
      END
