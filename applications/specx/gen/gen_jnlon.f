*  History:
*     16 Nov 1993 (hme):
*        Replace LIB${GET|FREE}_LUN with FIO_{G|P}UNIT.
*     09 Jan 1994 (rp):
*        Replace FIO_ routines with UGET/UFREE
*-----------------------------------------------------------------------

      SUBROUTINE GEN_JNLON (FILENAME, IERR)

*  Routine to enable journalling of all input to the command line
*  processor. Tricky, since immediate mode responses need to be filed
*  as comments - otherwise the file can't be re-run.

      IMPLICIT  NONE

*  Formal parameters:

      CHARACTER FILENAME*(*)
      INTEGER*4 IERR

*  Common blocks

      LOGICAL*4 JON
      INTEGER*4 JLUN
      COMMON /GEN_JOURNAL/ JON, JLUN

*  Other variables

      INTEGER*4 ISTAT

      IF (JON) RETURN


      ISTAT = 0
      CALL UGETLUN (JLUN, ISTAT)
      IF (ISTAT.NE.0) THEN
        IERR = 18
        RETURN
      END IF
      OPEN (JLUN, FILE=FILENAME, STATUS='NEW', ACCESS='SEQUENTIAL',
     &      CARRIAGECONTROL='LIST', IOSTAT=ISTAT)
      IF (ISTAT.NE.0) THEN
        IERR = 18
        RETURN
      END IF

      JON = .TRUE.

      RETURN
      END
