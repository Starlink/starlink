*  History:
*     16 Nov 1993 (hme):
*        Replace LIB${GET|FREE}_LUN with FIO_{G|P}UNIT.
*     09 Jan 1994 (rp):
*        Replace FIO_ routines with UGET/UFREE
*     31 July 2000 (ajc):
*        Remove CARRIAGECONTROL from OPEN
*        Open file STATUS='UNKNOWN' not 'NEW'
*        Set error 10 (Error opening file) not 18 (unknown error)
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
        IERR = 10
        RETURN
      END IF
      OPEN (JLUN, FILE=FILENAME, STATUS='UNKNOWN', ACCESS='SEQUENTIAL',
     &      IOSTAT=ISTAT)
      IF (ISTAT.NE.0) THEN
        IERR = 10
        RETURN
      END IF

      JON = .TRUE.

      RETURN
      END
