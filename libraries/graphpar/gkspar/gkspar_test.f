      PROGRAM GKSPAR_TEST
      INTEGER STATUS
      STATUS = 0

*  Simple test to see that the GKS routine are available to be linked against.

      PRINT *,'This test program only links against the GKSPAR routines'
      PRINT *,'It does not actually call them.'
     ://' not needed.'
      IF( STATUS .NE. -12345 ) GOTO 999
      CALL GKS_ANNUL
      CALL GKS_ASSOC
      CALL GKS_DEACT
      CALL GKS_CANCL
      CALL GKS_GSTAT
      CALL GKS_RESET
  999 CONTINUE
      print *,'ending'
      END
