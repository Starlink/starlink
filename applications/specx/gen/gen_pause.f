
*-----------------------------------------------------------------------
C------------------------------------------------------------------------

      SUBROUTINE GEN_PAUSE(IFAIL)

C  Routine to wait for input from the terminal to continue
C  ^C to abandon (safely), <RET> or y to continue, anything else to
C  unwind the command stack one level

      LOGICAL  CONTINUE
      INTEGER  STACK_POINTER

      IFAIL = 0
      CALL GEN_YESNO ('Continue?',.TRUE.,CONTINUE,ISTAT)
      IF (.NOT.CONTINUE) THEN
        IF (STACK_POINTER().NE.0) THEN
          CALL GEN_UNWIND(1)
        ELSE
          IFAIL = 45
        END IF
      END IF

      RETURN
      END
