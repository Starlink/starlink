      SUBROUTINE GK2DXF(IFID,RHT,RMAXWD,RBOT,RTOP,RWD)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Supply Font Details
*
*  MAINTENANCE LOG
*  ---------------
*     07/03/84  MGC  Original version stabilized (CC81)
*     08/01/85  GGT  Adapt for use with Tek 4010
*     18/02/87  GGT  Adapt for use with DEC VT24x
*
*  ARGUMENTS
*  ---------
*     INP IFID     Font identifier (ignored)
*     OUT RHT      Height from base to cap line
*     OUT RMAXWD   Width of widest character
*     OUT RBOT     Distance from base to bottom line
*     OUT RTOP     Distance from cap to top line
*     OUT RWD      Character widths array
*
      INTEGER IFID
      REAL RHT,RMAXWD,RBOT,RTOP,RWD(*)
*
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
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
      INTEGER II
*
*
*  COMMENTS
*  --------
*
*       See GK2DXC for comments on text height and width
*
*
* --------------------------------------------------------------------

      RHT=FLOAT(KWKDAT(CHARHI,KWKIX))
      RMAXWD=FLOAT(KWKDAT(CHARWI,KWKIX))
      RBOT=0.0
      RTOP=0.0
*     widths for chars [32..126]
      DO 10 II=1,95
        RWD(II)=RMAXWD
 10   CONTINUE
      RETURN
      END

