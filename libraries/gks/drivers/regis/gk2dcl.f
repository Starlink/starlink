      SUBROUTINE GK2DCL

*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Clears screen
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwkd.cmn'
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
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
*
*  LOCALS
*  ------
      INTEGER NLEFT
*---------------------------------------------------------------------
*
      CALL GKIOCO(KIOPB,'S(E)',NLEFT)
      CALL GKIOCO(KIOSN,' ',NLEFT)
*
      RETURN
      END

