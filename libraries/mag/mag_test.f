      PROGRAM MAG_TEST
      INTEGER STATUS
      STATUS = 0

*  Simple test to see that the MAG routine are available to be linked against.

      PRINT *,'This test program only links against the MAG routines.'
      PRINT *,'It does not actually call them, so a magnetic tape is'
     ://' not needed.'
      IF( STATUS .NE. -12345 ) GOTO 999
      CALL MAG_ALOC
      CALL MAG_ANNUL
      CALL MAG_ASSOC
      CALL MAG_CANCL
      CALL MAG_CLOSE
      CALL MAG_DEACT
      CALL MAG_DEAL
      CALL MAG_DISM
      CALL MAG_JEOV
      CALL MAG_JUMP
      CALL MAG_MOUNT
      CALL MAG_MOVE
      CALL MAG_POS
      CALL MAG_READ
      CALL MAG_REW
      CALL MAG_SET
      CALL MAG_SKIP
      CALL MAG_WRITE
      CALL MAG_WTM
  999 CONTINUE
      print *,'ending'
      END
