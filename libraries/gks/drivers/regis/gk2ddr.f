      SUBROUTINE GK2DDR(IXR,IYR,IMODE)
*
*      If IMODE=0 then move to (IX,IY)
*      If IMODE=1 then line to (IX,IY)

      INCLUDE '../../include/check.inc'

*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
      INTEGER NLEFT, LEN, IMODE, IXR, IYR
      CHARACTER*10 BUFFER
*
*  Offsets in KWKDAT workspace
*
      INTEGER MODE, CHARHI,CHARWI,ICX,ICY, ICPEN
      PARAMETER (MODE = 1, CHARHI = 2, CHARWI = 3, ICX = 4, ICY = 5,
     :           ICPEN = 6)
      INTEGER ICHFLG, ICHSTA, ICHNUM
      PARAMETER (ICHFLG=KMXWKI, ICHSTA=KMXWKI-1, ICHNUM=KMXWKI-2)
      INTEGER ILCFLG, ILCSTA, ILCX, ILCY
      PARAMETER (ILCFLG=KMXWKI-3, ILCSTA=KMXWKI-4)
      PARAMETER (ILCX=KMXWKR, ILCY=KMXWKR-1)
*
      IF (IMODE.EQ.0) THEN
         WRITE(BUFFER,'(A)')'P['
      ELSE
         WRITE(BUFFER,'(A)')'V['
      ENDIF
*
      IF (IXR.EQ.KWKDAT(ICX,KWKIX)) THEN
	 IF (IYR.EQ.KWKDAT(ICY,KWKIX)) THEN
            IF(IMODE.EQ.1) THEN
            WRITE(BUFFER,'(A)')']'
            LEN=3
            CALL GKIOCO(KIOPB,BUFFER(1:LEN),NLEFT)
            ENDIF
	    RETURN
	 ELSE
	    WRITE(BUFFER(3:),100)IYR
  100	    FORMAT(',',I3.3,']')
	    LEN=7
	 ENDIF
      ELSE
	 IF (IYR.EQ.KWKDAT(ICY,KWKIX)) THEN
	    WRITE(BUFFER(3:),101)IXR
  101	    FORMAT(I3.3,']')
	    LEN=6
	 ELSE
	    WRITE(BUFFER(3:),102)IXR,IYR
  102	    FORMAT(I3.3,',',I3.3,']')
	    LEN=10
	 ENDIF
      ENDIF
      CALL GKIOCO(KIOPB,BUFFER(1:LEN),NLEFT)
      KWKDAT(ICX,KWKIX)=IXR
      KWKDAT(ICY,KWKIX)=IYR
*
      RETURN
      END

