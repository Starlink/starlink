

*----------------------------------------------------------------------
      SUBROUTINE GK0BLN(N,X,Y)
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
*     BENSON Outputs polyline to buffer
*
*  MAINTENANCE LOG
*  ---------------
*     01/11/85  DRJF  Original version stabilized
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
      INTEGER    KFRORX,   KFRORY,   KORDER,   KXACFR,   IXPEN
      PARAMETER (KFRORX=3, KFRORY=4, KORDER=5, KXACFR=7, IXPEN=9)
      INTEGER    IYPEN,    KCPEN
      PARAMETER (IYPEN=10, KCPEN=11)
      INTEGER    KVCTR
      PARAMETER (KVCTR=1)
      INTEGER    KXACR
      PARAMETER (KXACR=1)
      INTEGER    PENDWN,   PENUP
      PARAMETER (PENDWN=2, PENUP=0)
      INTEGER    MAXLEN
      PARAMETER (MAXLEN=16383)
      INTEGER    IBYTES
      PARAMETER (IBYTES=4)
      INTEGER KHEAD(IBYTES)
      INTEGER II,IFR,ITO,INC
      INTEGER IX,IY,IXN,IYN,IDX,IDY,IDXN,IDYN
      INTEGER IABX,IABY,IDXREM,IDYREM
      INTEGER KSPEN,NLEFT
      LOGICAL REMLIN
      DATA KHEAD/255,2,0,0/
*
*----------------------------------------------------------------------


*
*     Set up VECTOR order
*
      KHEAD(3)=KWKDAT(KCPEN,KWKIX)
*
*     Put in a VECTOR order if necessary
*
*     Find out how many bytes are left in the current record
*
      CALL GKIOFO(KIOQS,1,KDAT,NLEFT)
*
*     If the current order in the record is a VECTOR
*
      IF (KWKDAT(KORDER,KWKIX).EQ.KVCTR) THEN
*
*       If there are not enough bytes left in the current record
*       to put a piece of data in, end the current record and
*       start a new one, and add a vector header
*
        IF (NLEFT.LT.4) THEN
          CALL GKIOFO(KIOER,1,KDAT,NLEFT)
          CALL GKIOFO(KIOPB,4,KHEAD,NLEFT)
        END IF
      ELSE
*
*       Current order is not a VECTOR
*
*       If there are not enough bytes left in the current record
*       for a VECTOR order and a piece of data, end the current
*       record and start a new one
*
        IF (NLEFT.LT.8) CALL GKIOFO(KIOER,1,KDAT,NLEFT)
        CALL GKIOFO(KIOPB,4,KHEAD,NLEFT)
        KWKDAT(KORDER,KWKIX)=KVCTR
      END IF
*
*     Initialise
*
      IABX=KWKDAT(IXPEN,KWKIX)
      IABY=KWKDAT(IYPEN,KWKIX)
*
*     Calculate the relative X, Y coordinates of the first and
*     last points from the current position of the BENSON pen
*     in paper coordinates
*
      IF (KWKDAT(KXACFR,KWKIX).EQ.KXACR) THEN
*
*       Xacross
*
        IX=-NINT(Y(1))+KWKDAT(KFRORX,KWKIX)
        IY=NINT(X(1))+KWKDAT(KFRORY,KWKIX)
        IXN=-NINT(Y(N))+KWKDAT(KFRORX,KWKIX)
        IYN=NINT(X(N))+KWKDAT(KFRORY,KWKIX)
      ELSE
*
*       Yacross
*
        IX=NINT(X(1))+KWKDAT(KFRORX,KWKIX)
        IY=NINT(Y(1))+KWKDAT(KFRORY,KWKIX)
        IXN=NINT(X(N))+KWKDAT(KFRORX,KWKIX)
        IYN=NINT(Y(N))+KWKDAT(KFRORY,KWKIX)
      END IF
      IDX=ABS(IX-IABX)
      IDY=ABS(IY-IABY)
      IDXN=ABS(IXN-IABX)
      IDYN=ABS(IYN-IABY)

*
*     Find out whether the first or last point is closest to
*     the current position of the BENSON pen
*
      IF (IDX.EQ.0 .AND. IDY.EQ.0) THEN
*
*       Polyline first point at current position
*
        IFR=1
        ITO=N
        INC=1
      ELSE IF (IDXN.EQ.0 .AND. IDYN.EQ.0) THEN
*
*       Polyline last point at current position
*
        IFR=N
        ITO=1
        INC=-1
      ELSE IF (IDX*IDX+IDY*IDY .LT. IDXN*IDXN+IDYN*IDYN) THEN
*
*       Polyline first point closest to current position
*
        IFR=1
        ITO=N
        INC=1
      ELSE
*
*       Polyline last point closest to current position
*
        IFR=N
        ITO=1
        INC=-1
      END IF
*
*     Draw polyline
*
      IDXREM=0
      IDYREM=0
*
*     Set pen state to UP so that when the pen is moved to
*     its initial position no line is drawn
*
      KSPEN=PENUP
      REMLIN=.FALSE.
*
*     Loop for all line segments
*
      DO 100 II=IFR,ITO,INC
*
*       Transform raster coordinates to paper coordinates
*
        IF (KWKDAT(KXACFR,KWKIX).EQ.KXACR) THEN
*
*         Xacross
*
          IX=-NINT(Y(II))+KWKDAT(KFRORX,KWKIX)
          IY=NINT(X(II))+KWKDAT(KFRORY,KWKIX)
        ELSE
*
*         Yacross
*
          IX=NINT(X(II))+KWKDAT(KFRORX,KWKIX)
          IY=NINT(Y(II))+KWKDAT(KFRORY,KWKIX)
        END IF
        IDX=IX-IABX
        IDY=IY-IABY
*
*       Does line segment need spliting. Algorithm not 100
*       per cent safe, but allowable since line length is
*       very large
*
   10   CONTINUE
        IF (ABS(IDX).GT.MAXLEN.OR.ABS(IDY).GT.MAXLEN)THEN
*
*         Line too long so divide into fraction and remainder
*
          REMLIN=.TRUE.
          IDXREM=IDXREM-IDX/2+IDX
          IDYREM=IDYREM-IDY/2+IDY
          IDX=IDX/2
          IDY=IDY/2
          GOTO 10
        ELSE
*
*         If there are not enough bytes left in the current
*         record to put a piece of data in, end the current
*         record and start a new one
*
          IF (NLEFT.LT.4) THEN
            CALL GKIOFO(KIOER,1,KDAT,NLEFT)
            CALL GKIOFO(KIOPB,4,KHEAD,NLEFT)
          END IF
*
*         With line in range
*
          CALL GK0BXY(IDX,IDY,KSPEN,NLEFT)
          IF (REMLIN) THEN
*
*           Deal with remainder
*
            IDX=IDXREM
            IDY=IDYREM
            IDXREM=0
            IDYREM=0
            REMLIN=.FALSE.
            GOTO 10
          END IF
*
*         Save last point
*
          IABX=IX
          IABY=IY
        END IF
*
*       Set pen state to DOWN so that POLYLINE can be drawn
*
        KSPEN=PENDWN
 100  CONTINUE
      KWKDAT(IXPEN,KWKIX)=IABX
      KWKDAT(IYPEN,KWKIX)=IABY
      RETURN
*
      END
