

*----------------------------------------------------------------------
      SUBROUTINE GK0BDR
*
* (C) COPYRIGHT ICL & SERC  1985
*

*----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Part of Workstation Driver
*  Author:             DRJF
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Draws border around new frame.
*
*  MAINTENANCE LOG
*  ---------------
*     01/11/85  DRJF  Original version stabilized
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
      INTEGER    KFRX,   KFRY,   KORDER,   KXACFR,   KCPEN
      PARAMETER (KFRX=1, KFRY=2, KORDER=5, KXACFR=7, KCPEN=11)
      INTEGER     KXACR
      PARAMETER  (KXACR=1)
      REAL BX(5),BY(5)
      DATA BX/0.0,0.0,0.0,0.0,0.0/, BY/0.0,0.0,0.0,0.0,0.0/
*
*----------------------------------------------------------------------


*
*     Set coordinates of vertices of new frame
*
      IF (KWKDAT(KXACFR,KWKIX).EQ.KXACR) THEN
*
*       Xacross
*
        BY(2)=FLOAT(KWKDAT(KFRX,KWKIX))
        BX(3)=FLOAT(KWKDAT(KFRY,KWKIX))
        BY(3)=FLOAT(KWKDAT(KFRX,KWKIX))
        BX(4)=FLOAT(KWKDAT(KFRY,KWKIX))
      ELSE
*
*       Yacross
*
        BY(2)=FLOAT(KWKDAT(KFRY,KWKIX))
        BX(3)=FLOAT(KWKDAT(KFRX,KWKIX))
        BY(3)=FLOAT(KWKDAT(KFRY,KWKIX))
        BX(4)=FLOAT(KWKDAT(KFRX,KWKIX))
      END IF
      IF (KWKDAT(KCPEN,KWKIX).NE.0) THEN
*
*       Current pen not black
*
        KWKDAT(KCPEN,KWKIX)=0
        KWKDAT(KORDER,KWKIX)=0
      END IF
      CALL GK0BLN(5,BX,BY)
      RETURN
*
      END
