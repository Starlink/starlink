*  History:
*     20 July 2000 (ajc):
*        Missing commas in FORMAT
*        Don't split strings across lines
*        Unused GEN_ILEN
C-----------------------------------------------------------------------

      SUBROUTINE DISPST (NSHOW, STACK_DATA, BUF)

C  Routine to display the format and contents of the data stack.
C  Shows only the top NSHOW spectra or the maximum number in the stack
C    -- whichever is the lesser. Only shows stack format if STACK_DATA = .T.

      IMPLICIT  NONE

C     Formal parameters:
      INTEGER   NSHOW
      LOGICAL   STACK_DATA
      REAL      BUF(1)

C     Local variables:
      INTEGER   J, J1
      CHARACTER NAME(3)
      CHARACTER POS*2
      CHARACTER SCAN*4
      CHARACTER TITLE*26

C     Include files:
      INCLUDE 'STACKCOMM'
      INCLUDE 'SPECX_PARS'
      INCLUDE 'STAKPAR'

      DATA NAME/'X','Y','Z'/

C  Ok, go...

      IF (STACK_DATA) THEN
        WRITE (6,*)
        WRITE (6,'('' ('',I2,'' stack positions, length '','//
     &             'I4'' points)'')') JSTK,LSTK-LHEAD
        WRITE (6,'('' (Y-register data starts at STACK(''I4''))''/)')
     &             IDAT2
        WRITE (6,*)
        WRITE (6,'(15X,''Data stack contents'')')
      END IF

      WRITE (6,*)
      WRITE (6,1000)

      J1 = JTOP
      DO J = 1,JSTK
        TITLE = ' '
        SCAN  = ' '
        IF (J.LE.J1.AND.LSCAN.GE.0)   THEN
          TITLE = ITITLE
          WRITE (SCAN,1002) LSCAN
        ENDIF
        WRITE (POS,1003) J
        IF (J.LE.3)      POS = ' '//NAME(J)
        IF (J.EQ.JSTK)   POS = ' T'
        IF (J.LE.NSHOW)  WRITE (6,1004) POS,SCAN,TITLE
        CALL ROLL (BUF)
      END DO

      JTOP = J1
      RETURN

 1000 FORMAT(/' Stack posn',4X,'Scan no',4X,'Title')
 1002 FORMAT(I4)
 1003 FORMAT(I2)
 1004 FORMAT(4X,A2,9X,A4,6X,A26)

      END

C-----------------------------------------------------------------------

