C# IL>=a, OL>=0
      SUBROUTINE GKXPAL(XTX,YTX,RNYADJ,RNBASE,RNCAP,RX,RY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     locate point on pseudo text extent for alignment
*
*  MAINTENANCE LOG
*  ---------------
*     14/11/83    FY  Original version stabilized
*     19/01/84    PB  font details obtained as parameters
*
*  ARGUMENTS
*  ---------
*     INP RNYADJ   distance between halfline & mid. point of char. body
*     INP RNBASE   distance bottom to base
*     INP RNCAP    distance cap to top
*     INP XTX(1),YTX(1)   bottom left
*     INP XTX(2),YTX(2)   bottom right
*     INP XTX(3),YTX(3)   top right
*     INP XTX(4),YTX(4)   top left
*     OUT RX     x component of point for alignment
*     OUT RY     y component of point for alignment
*
      REAL XTX(4),YTX(4),RX,RY,RNYADJ,RNBASE,RNCAP
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkxfd.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*---------------------------------------------------------------------



* it is assumed, if user specified alignment is NORMAL, it will be
* resolved prior calling this routine.


      IF (KWHTXA(KWKIX).EQ.GALEFT) THEN
        RX = XTX(1)
      ELSEIF (KWHTXA(KWKIX).EQ.GACENT) THEN
        RX = (XTX(2) - XTX(1)) / 2.0 + XTX(1)
      ELSEIF (KWHTXA(KWKIX).EQ.GARITE) THEN
        RX = XTX(2)
      ENDIF

      IF (KWVTXA(KWKIX).EQ.GATOP) THEN
        RY = YTX(3)
      ELSEIF (KWVTXA(KWKIX).EQ.GACAP) THEN
        RY = YTX(3) - RNCAP
      ELSEIF (KWVTXA(KWKIX).EQ.GAHALF) THEN
        RY = (YTX(3)+YTX(1)-RNCAP+RNBASE)/2.0 + RNYADJ
      ELSEIF (KWVTXA(KWKIX).EQ.GABASE) THEN
        RY = YTX(1) + RNBASE
      ELSEIF (KWVTXA(KWKIX).EQ.GABOTT) THEN
        RY = YTX(1)
      ENDIF

      END
