

*----------------------------------------------------------------------
      SUBROUTINE GK0BNF
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
*  PURPOSE OF ROUTINE
*  ------------------
*     Calculates displacements for the new frame
*
*  MAINTENANCE LOG
*  ---------------
*   01/11/85  DRJF  Original Version stabilised.
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*
      INTEGER    KFRX,   KFRY,   KFRORX,   KFRORY,   KORDER,   KMAXX
      PARAMETER (KFRX=1, KFRY=2, KFRORX=3, KFRORY=4, KORDER=5, KMAXX=6)
      INTEGER    KXACFR,   KADJFR,   IXPEN,   IYPEN
      PARAMETER (KXACFR=7, KADJFR=8, IXPEN=9, IYPEN=10)
      INTEGER    KGAP,     KPLWID,       QBCON
      PARAMETER (KGAP=200, KPLWID=17800, QBCON=20000.0)
      INTEGER    KADJST,   KXACR
      PARAMETER (KADJST=1, KXACR=1)
      INTEGER KRX,KRY
      INTEGER IFRSZ,NLEFT
*
*     FF82 - Advance frame
*
      INTEGER IFF82(4)
      LOGICAL NEWROW
      DATA IFF82/255,130,0,0/
*
*----------------------------------------------------------------------


*
*     Set requested frame size in local variables
*
      IF (KWKDAT(KADJFR,KWKIX).EQ.KADJST) THEN
*
*       Adjust
*
        IF (KWKDAT(KXACFR,KWKIX).EQ.KXACR) THEN
*
*         Xacross
*
          KRX=NINT(QCWVYT(KWKIX)*QBCON)
          KRY=NINT(QCWVXR(KWKIX)*QBCON)
        ELSE
*
*         Yacross
*
          KRX=NINT(QCWVXR(KWKIX)*QBCON)
          KRY=NINT(QCWVYT(KWKIX)*QBCON)
        END IF
      ELSE
*
*       Fixed mode
*
        IF (KWKDAT(KXACFR,KWKIX).EQ.KXACR) THEN
*
*         Xacross
*
          KRX=KDSRY(KWKIX)
          KRY=KDSRX(KWKIX)
        ELSE
*
*         Yacross
*
          KRX=KDSRX(KWKIX)
          KRY=KDSRY(KWKIX)
        END IF
      END IF
      IF (KWKDAT(KXACFR,KWKIX).EQ.KXACR) THEN
*
*       Xacross
*
*       Calculate origin of new frame
*
        KWKDAT(KFRORY,KWKIX)=KWKDAT(KFRORY,KWKIX)+
     :                       KWKDAT(KFRY,KWKIX)+KGAP
        KWKDAT(KFRORX,KWKIX)=KWKDAT(KFRORX,KWKIX)-
     :                       KWKDAT(KFRX,KWKIX)+KRX
        NEWROW=.FALSE.
        IF(KWKDAT(KFRORY,KWKIX)+KRY.GT.KPLWID)THEN
*
*         New frame width extends over edge of paper. Start a
*         new row of frames
*
          NEWROW=.TRUE.
          KWKDAT(KFRORY,KWKIX)=0
          KWKDAT(KFRORX,KWKIX)=KRX
        END IF
      ELSE
*
*       Yacross
*
*       Calculate origin of new frame
*
        KWKDAT(KFRORY,KWKIX)=KWKDAT(KFRORY,KWKIX)-
     :                       KGAP-KRY
        NEWROW=.FALSE.
        IF (KWKDAT(KFRORY,KWKIX).LT.0) THEN
*
*         New origin is off the edge of the paper. Start a
*         new row of frames
*
          NEWROW=.TRUE.
          KWKDAT(KFRORY,KWKIX)=KPLWID-KRY
          KWKDAT(KFRORX,KWKIX)=0
        END IF
      END IF
*
      IF (NEWROW) THEN
        CALL GKIOFO(KIOQS,1,KDAT,NLEFT)
        IF (NLEFT.LT.4) CALL GKIOFO(KIOER,1,KDAT,NLEFT)
*
*       Set up FF82 order
*
        IFRSZ=INT((KWKDAT(KMAXX,KWKIX)+KGAP)/20)
        IFF82(3)=IFRSZ/256
        IFF82(4)=MOD(IFRSZ,256)
        CALL GKIOFO(KIOPB,4,IFF82,NLEFT)
        KWKDAT(KORDER,KWKIX)=0
*
*       Initialise pen position
*
        KWKDAT(IXPEN,KWKIX)=0
        KWKDAT(IYPEN,KWKIX)=0
        KWKDAT(KMAXX,KWKIX)=KRX
      ELSE
        KWKDAT(KMAXX,KWKIX)=MAX(KWKDAT(KMAXX,KWKIX),KRX)
      END IF
*
*     Save previous frame sizes
*
      KWKDAT(KFRY,KWKIX)=KRY
      KWKDAT(KFRX,KWKIX)=KRX
*
      CALL GK0BDR
      RETURN
*
      END
