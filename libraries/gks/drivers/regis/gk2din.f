      SUBROUTINE GK2DIN(TYPE)
*
*     Routine - GK2DIN
*
*     Purpose device initialisation
*
*     Paramters -
*
*      TYPE	INTEGER the required VT24x type
*			=1720 for VT240 Monochrome
*			=1721 for VT241 Colour
*
*     Author -  G.G.Tolton  Leicester University
*            Computer Laboratory
*
*     Date -  February 1987
*
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
      INTEGER NLEFT, TYPE
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

      CHARACTER*1 BSLASH, ESC
*      PARAMETER ( ESC = CHAR(27), BSLASH = CHAR(92) )
      ESC = CHAR(27)
      BSLASH = CHAR(92)
*
*
*	Initialise the I/O buffer
*
      CALL GKIOCO(KIOIT,' ',NLEFT)
*
*	Set start of buffer to 'ENTER REGIS'
*	and end of buffer to 'EXIT REGIS'
*
      CALL GKIOCO(KIOBB,ESC//'Pp',NLEFT)
      CALL GKIOCO(KIOEB,ESC//BSLASH, NLEFT)
*
*	Define coordinate system as [0,0] in bottom left
*
      CALL GKIOCO(KIOPB,'S(A[0,479][799,0])',NLEFT)
*
*	and set the CP to [0,0]
*
      KWKDAT(ICX,KWKIX)=-99
      KWKDAT(ICY,KWKIX)=-99
      CALL GK2DDR(0,0,0)
*
      IF(TYPE.EQ.1720) THEN
        KWKDAT(MODE,KWKIX)=0
      ELSE
        KWKDAT(MODE,KWKIX)=1
      ENDIF
*
      KWKDAT(CHARHI,KWKIX)=20
      KWKDAT(CHARWI,KWKIX)=9
*
*      Set up colour table:
*	    Colour VT241	Mono VT240
*           0 - Black		0 - Black
*           1 - White		1 - White
*           2 - Red		2 - Dim Grey
*           3 - Green		3 - Light Grey
*
      IF (KWKDAT(MODE,KWKIX).EQ.0) THEN
	 CALL GKIOCO(KIOPB,'S(M0(L0)1(L100)2(L33)3(L67))',NLEFT)
      ELSE
	 CALL GKIOCO(KIOPB,'S(M0(AD)1(AW)2(AR)3(AG))',NLEFT)
      ENDIF
*
*	Set background, foreground and replace mode
*
      CALL GKIOCO(KIOPB,'S(I0)W(I1)W(R)',NLEFT)
*
      KWKDAT(ICPEN,KWKIX) = 1
*
      RETURN
      END
