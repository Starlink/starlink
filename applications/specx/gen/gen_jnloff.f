*  History:
*     16 Nov 1993 (hme):
*        Replace LIB${GET|FREE}_LUN with FIO_{G|P}UNIT.
*     09 Jan 1994 (rp):
*        Replace FIO_ routines with UGET/UFREE
*-----------------------------------------------------------------------

      SUBROUTINE GEN_JNLOFF (IERR)

*  Routine to disable journalling of command line input

      IMPLICIT  NONE

*  Formal parameters:

      INTEGER*4 IERR

*  Common blocks

      LOGICAL*4 JON
      INTEGER*4 JLUN
      COMMON /GEN_JOURNAL/ JON, JLUN

*  Other variables

      INTEGER*4 ISTAT

      IF (.NOT.JON) RETURN

      BACKSPACE (JLUN)             ! To delete the J* command
      WRITE     (JLUN,*) ' '

      CLOSE (JLUN,IOSTAT=ISTAT)
      IF (ISTAT.NE.0) THEN
        IERR = 18
        RETURN
      END IF
      ISTAT = 0
      CALL UFREELUN (JLUN, ISTAT)

      JON = .FALSE.

      RETURN
      END
