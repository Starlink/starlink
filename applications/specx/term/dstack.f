*  History:
*     20 July 2000 (ajc):
*        Missing commas in FORMAT
*        Don't split strings across lines
C-----------------------------------------------------------------------

      SUBROUTINE DSTACK

C  Program to set parameters of stack used for spectrum manipulation

      INCLUDE  'SPECX_PARS'
      INCLUDE  'STAKPAR'

      CHARACTER PROMPT*32

      INTEGER   GEN_ILEN

C  Ok, go...

      ICHNGE = 0                ! Need to clear stack?
      LDAT   = LSTK-LHEAD       ! Maximum scan length

      PROMPT=' '
      WRITE(PROMPT,'(''['',I4,''] ''''"'')') LDAT
    5 CALL GEN_GETI4('"'' Enter new stack parameters''//'//
     &           '''$Maximum number of data points? '//
     &           PROMPT(:GEN_ILEN(PROMPT)),LDAT,' ',LSTK2,JDEF)
      IF (LSTK2.NE.LDAT)   ICHNGE=1
      LSTK = LSTK2 + LHEAD

      JSTK1 = JSTK
      CALL GEN_GETI4('No of levels in stack?',JSTK1,'I2',JSTK,JDEF)
      IF (JSTK.NE.JSTK1)   ICHNGE=ICHNGE+2

      IF (JSTK*LSTK.GT.8848)  THEN
        JSTK = 9216/LSTK
        WRITE(6,30) JSTK
        GO TO 5

      ELSE
        IDAT2 = LSTK+LHEAD
        IF(ICHNGE.EQ.1)   JTOP=1
        IF(ICHNGE.EQ.2)   JTOP=MIN0(JTOP,JSTK)
        IF(ICHNGE.EQ.3)   JTOP=1
      END IF

      RETURN

   30 FORMAT(' *** Insufficient room - max levels = ',I3, '***')

      END

