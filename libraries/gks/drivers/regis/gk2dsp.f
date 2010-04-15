      SUBROUTINE GK2DSP(INDEC)
*
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Select logical colour associated with colour index INDEC
*
*  MAINTENANCE LOG
*  ---------------
*     08/01/85  GGT  Original version stabilized
*     10/01/86  GGT  Adapted for ACW
*     18/02/87  GGT  Adapted for DEC VT24x

      INCLUDE '../../include/check.inc'
*
*  ARGUMENTS
*  ---------
*     INP INDEC    Colour index
*
      INTEGER INDEC
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
      INTEGER NLEFT
      CHARACTER*5 BUFFER
*
*      Set the graphics foreground colour
*
      IF (KWKDAT(ICPEN,KWKIX).NE.INDEC) THEN
         WRITE(BUFFER,100)INDEC
  100    FORMAT('W(I',I1.1,')')
*
         CALL GKIOCO(KIOPB,BUFFER,NLEFT)
*
         KWKDAT(ICPEN,KWKIX) = INDEC
      ENDIF
*
*
      RETURN
      END
