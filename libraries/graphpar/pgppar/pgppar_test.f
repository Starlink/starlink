      PROGRAM PGPPAR_TEST
      INTEGER STATUS
      STATUS = 0

*  Simple test to see that the PGPPAR routine are available for linking

      PRINT *,'This test program only links against the PGPPAR routines'
      PRINT *,'It does not actually call them.'
     ://' not needed.'
      IF( STATUS .NE. -12345 ) GOTO 999
      CALL PGP_ANNUL
      CALL PGP_ASSOC
      CALL PGP_CANCL
      CALL PGP_DEACT

  999 CONTINUE
      print *,'ending'
      END
