*  History:
*     17 Nov 1993 (hme):
*        Replace LIB${GET|FREE}_LUN with FIO_{G|P}UNIT.
*        Replace CALL CLOSE with CLOSE.
*     09 Jan 1994 (rp):
*        Replace FIO_ routines with UGET/UFREE
*     28 July 2000 (ajc):
*        Remove CARRIAGECONTROL and READONLY from OPEN
*        Change TYPE * to PRINT *
*        Unused GEN_ILEN
*-----------------------------------------------------------------------

      SUBROUTINE GEN_AT(FILENAM,IERR)

*     Indirect command file processor for SPECX

      CHARACTER FILENAM*(*)

      INCLUDE 'CLI_STACK.INC'

*     Local variables

      INTEGER STATUS

*     OK? Go..

      STATUS = 0
      CALL UGETLUN (LUN, STATUS)
      IF (STATUS.NE.0) GO TO 100

      OPEN (LUN, FILE=FILENAM, STATUS='OLD',
     &     ACCESS='SEQUENTIAL', FORM='FORMATTED',
     &     IOSTAT=IERR, ERR=100)

  100 IF(IERR.NE.0)   THEN
        PRINT *,'Trouble opening @ file ',FILENAM
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

CD    print *,'-------------------'
CD    print *,'Stack pointer incremented ',ISP
CD    print *,'..new input unit ',LUN,' new string pos''n',ICLI(1,ISP)
CD    print *,'..new length of CLI ',ICLI(2,ISP)
CD    print *,'-------------------'

      RETURN
      END
