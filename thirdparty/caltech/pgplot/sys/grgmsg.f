
C*GRGMSG -- print system message (Sun/Convex-UNIX)
C+
      SUBROUTINE GRGMSG (STATUS)
      INTEGER STATUS
C
C This routine obtains the text of the VMS system message corresponding
C to code STATUS, and displays it using routine GRWARN. On non-VMS
C systems, it just displays the error number.
C
C Argument:
C  STATUS (input): 32-bit system message code.
C--
C 18-Feb-1988
C-----------------------------------------------------------------------
      CHARACTER*10 BUFFER
C
      WRITE (BUFFER, '(I10)') STATUS
      CALL GRWARN('system message number: '//BUFFER)
      END
