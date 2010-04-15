      SUBROUTINE GK2DCS(ICODE,X,Y)

      INCLUDE '../../include/check.inc'

*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkio.par'

*  Read cursor position
*
      INTEGER ICODE
      REAL X,Y
*
      INTEGER NOUT,I,J,NLEFT,IX,IY
      CHARACTER*20 REPORT
*
*  Note that it is not possible to request cursor position
*  without displaying crosshairs
*
      CALL GKIOCO(KIOPB,'R(P(I))',NLEFT)
      CALL GKIOCO(KIOSN,' ',NLEFT)
      CALL GKIOCI(KIONN,0,' ',REPORT,NOUT)
      IX=0
      IY=0
      IF (NOUT.EQ.0) THEN
         ICODE=13
         CALL GKIOCI(KIONN,0,' ',REPORT(2:),NOUT)
      ELSE
         ICODE=ICHAR(REPORT(1:1))
      ENDIF
      DO 10 I=3,NOUT-1
         IF (REPORT(I:I).EQ.',') GOTO 20
         IX=(IX*10)+(ICHAR(REPORT(I:I))-48)
   10 CONTINUE
   20 DO 30 J=I+1,NOUT-1
         IY=(IY*10)+(ICHAR(REPORT(J:J))-48)
   30 CONTINUE
*
      X=FLOAT(IX)
      Y=FLOAT(IY)
*
      RETURN
      END

