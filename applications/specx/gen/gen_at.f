*  History:
*     17 Nov 1993 (hme):
*        Replace LIB${GET|FREE}_LUN with FIO_{G|P}UNIT.
*        Replace CALL CLOSE with CLOSE.
*     09 Jan 1994 (rp):
*        Replace FIO_ routines with UGET/UFREE
*-----------------------------------------------------------------------

      SUBROUTINE GEN_AT(FILENAM,IERR)

*     Indirect command file processor for SPECX

      CHARACTER FILENAM*(*)
      INTEGER*4 GEN_ILEN

      INCLUDE 'CLI_STACK.INC'

*     Local variables

      INTEGER STATUS

*     OK? Go..

      STATUS = 0
      CALL UGETLUN (LUN, STATUS)
      IF (STATUS.NE.0) GO TO 100

      OPEN (LUN, FILE=FILENAM, STATUS='OLD', READONLY,
     &     ACCESS='SEQUENTIAL', FORM='FORMATTED',
     &     CARRIAGECONTROL='LIST', IOSTAT=IERR, ERR=100)

  100 IF(IERR.NE.0)   THEN
        TYPE *,'Trouble opening @ file ',FILENAM
        CALL GEN_ERMSG    (IERR)
        CLOSE             (LUN)
        CALL UFREELUN (LUN, STATUS)
        RETURN
      END IF

      ISP          = ISP + 1       ! Increment stack pointer

      ICLI (1,ISP) = 1             ! Reset position in string
      ICLI (3,ISP) = LUN           ! Save logical unit number on stack
      ICLI (4,ISP) = 0             ! Initialize line number to zero

      CALL SET_LUN_IN (LUN)

D     type *,'-------------------'
D     type *,'Stack pointer incremented ',ISP
D     type *,'..new input unit ',LUN,' new string pos''n',ICLI(1,ISP)
D     type *,'..new length of CLI ',ICLI(2,ISP)
D     type *,'-------------------'

      RETURN
      END
