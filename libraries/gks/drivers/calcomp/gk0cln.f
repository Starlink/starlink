

*-----------------------------------------------------------------
      SUBROUTINE GK0CLN(N,X,Y)
*-----------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CC81 Outputs polyline to buffer
*
*  MAINTENANCE LOG
*  ---------------
*     00/99/83  MGC  Original version stabilized
*     19/03/84  MGC  Minimise pen movement
*
*  ARGUMENTS
*  ---------
*     INP N   - number of points
*     INP X,Y - coordinates of points
*
      INTEGER N
      REAL X(N),Y(N)
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
      INTEGER NLEFT
      INTEGER IXPEN, IYPEN
      PARAMETER (IXPEN=2, IYPEN=3)
      INTEGER II,IFR,ITO,INC
      INTEGER IX,IY,IXN,IYN,IDX,IDY,IDXN,IDYN

*  ALGORITHM
*  ---------
*     .PEN UP                  : H         ) only if
*     .MOVE PEN                : <X>/<Y>K  ) necessary
*     .PEN DOWN                : I
*     .DRAW POLYLINE LINES -   : <X>/<Y>K
*                          -   : <X>/   K
*                          -   :    /<Y>K
*
*  Drawing order can be [1..N] or [N..1]
*
* --------------------------------------------------------------------

      IX=IFIX(X(1))
      IY=IFIX(Y(1))
      IXN=IFIX(X(N))
      IYN=IFIX(Y(N))
*     current position unknown
      IF(KWKDAT(IXPEN,KWKIX).EQ.KNIL .OR. KWKDAT(IYPEN,KWKIX).EQ.KNIL)
     :THEN
*       pen up
        CALL GK0CPB(72)
        CALL GK0CPP(IX,IY)
        IFR=2
        ITO=N
        INC=1
*     current position known
      ELSE
        IDX=IABS(KWKDAT(IXPEN,KWKIX)-IX)
        IDY=IABS(KWKDAT(IYPEN,KWKIX)-IY)
        IDXN=IABS(KWKDAT(IXPEN,KWKIX)-IXN)
        IDYN=IABS(KWKDAT(IYPEN,KWKIX)-IYN)
*       polyline initial point at current position
        IF(IDX.EQ.0 .AND. IDY.EQ.0) THEN
          IFR=2
          ITO=N
          INC=1
*       polyline final point at current position
        ELSEIF(IDXN.EQ.0 .AND. IDYN.EQ.0) THEN
          IFR=N-1
          ITO=1
          INC=-1
*       polyline initial point closest to current position
        ELSEIF(IDX*IDX+IDY*IDY .LT. IDXN*IDXN+IDYN*IDYN)
     :  THEN
*         pen up
          CALL GK0CPB(72)
          CALL GK0CPP(IX,IY)
          IFR=2
          ITO=N
          INC=1
*       polyline final point closest to current position
        ELSE
*         pen up
          CALL GK0CPB(72)
          CALL GK0CPP(IXN,IYN)
          IFR=N-1
          ITO=1
          INC=-1
        ENDIF
      ENDIF
*     pen down
      CALL GK0CPB(73)
*     draw polyline
      IF(N.GT.1) THEN
        DO 100 II=IFR,ITO,INC
          IX=IFIX(X(II))
          IY=IFIX(Y(II))
          CALL GKIOBO(KIOQS,1,KDAT,NLEFT)
          IF(NLEFT.LT.12) CALL GK0CPA(KIOSN,1,KDAT)
          IF(IX.NE.KWKDAT(IXPEN,KWKIX)) THEN
            CALL GK0CPN(IX)
            CALL GK0CPB(47)
            IF(IY.NE.KWKDAT(IYPEN,KWKIX)) CALL GK0CPN(IY)
            CALL GK0CPB(75)
          ELSEIF (IY.NE.KWKDAT(IYPEN,KWKIX)) THEN
            CALL GK0CPB(47)
            CALL GK0CPN(IY)
            CALL GK0CPB(75)
          ENDIF
          KWKDAT(IXPEN,KWKIX)=IX
          KWKDAT(IYPEN,KWKIX)=IY
 100    CONTINUE
      ENDIF
      END
