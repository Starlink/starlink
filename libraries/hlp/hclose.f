      SUBROUTINE hlp_HCLOSE (J)
*+
*  - - - - - - -
*   H C L O S E
*  - - - - - - -
*
*  Close the help library.
*
*  Specified in INCLUDE:
*     LHBUF     i           length of help buffer
*
*  Given (in COMMON):
*     JHELP     i           state of HLP system: 1 or 2 = open
*     LUHL      i           unit number for help library file
*     IHBUF     i           address of HBUF within help library file
*     HBUF      c*(LHBUF)   help buffer
*
*  Returned (in COMMON):
*     JHELP     i           state of HLP system: -1=closed
*     HLOPEN    c*()        name of open help library (' ')
*     LOFFNU    i           level number for next help library (0)
*
*  Returned (argument)
*     J         i           status:  0 = OK
*                                   -1 = HLP system in illegal state
*                                   -3 = write error
*                                   -5 = close error
*
*  P.T.Wallace   Starlink   13 June 1995
*-

      IMPLICIT NONE

      INTEGER J

      INCLUDE 'helpic'



*  What state is the HLP system in?
      IF (JHELP.EQ.1) THEN

*  Open for writing: flush.
         IF (IHBUF.GE.0) WRITE (UNIT=LUHL,FMT='(A)',
     :                          REC=1+IHBUF/LHBUF,ERR=9000) HBUF

      ELSE IF (JHELP.NE.2) THEN

*  Wrong state: set status and exit.
         GO TO 9010

      END IF

*  Close the file.
      CLOSE (UNIT=LUHL,ERR=9020)

*  Resets.
      JHELP=-1
      HLOPEN=' '
      LOFFNU=0

*  Set status and exit.
      J=0
      GO TO 9999

*  Write error
 9000 CONTINUE
      J=-3
      GO TO 9999

*  HLP system in the wrong state
 9010 CONTINUE
      J=-1
      GO TO 9999

*  Error on close
 9020 CONTINUE
      J=-5

*  Exit.
 9999 CONTINUE

      END
