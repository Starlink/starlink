C# IL>=a, OL>=1
      SUBROUTINE GKPWCX(N,PX,PY,SCAN,X,Y,TMIN,TMAX)
*
* (C) COPYRIGHT ICL & SERC  1985
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Pick utility
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set or update total primitive extent and tell if scan required.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC   Original version stabilised
*
*  ARGUMENTS
*  ---------
*     INP N     - number of points
*     INP PX    - x-coordinates of points
*     INP PY    - y-coordinates of points
*     OUT SCAN  - .TRUE. if scan required
*     OUT X,Y   - Pick point
*     OUT TMIN,TMAX - Hit tolerances
*
      INTEGER N
      REAL    PX(N),PY(N)
      REAL    X,Y,TMIN,TMAX
      LOGICAL SCAN
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read     /CCA/    -  Pick point (QSS1..2)
*     Ignore   /CCA/    -  Tolerances (QSS3..4)
*     Update   /CCA/    -  Extent     (QSS5..8)
*     Read     /CCA/    -  Distance   (QSS9)
*
      INCLUDE '../include/gkcca.cmn'
*
*  LOCALS
*  ------
*     I        Loop count
*     LX,RX    X-extent of primitive
*     BY,TY    Y-extent of primitive
*
      INTEGER  I
      REAL     LX,RX,BY,TY
*
*  EXTERNALS
*  ---------
*
*  STACK USAGE
*  -----------
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

* Initialise at initial point
      LX=PX(1)
      RX=PX(1)
      BY=PY(1)
      TY=PY(1)

* Find extent for this component of the primitive
      IF(N.GT.1) THEN
        DO 100 I=2,N
          IF(PX(I).LT.LX) THEN
            LX=PX(I)
          ELSEIF(PX(I).GT.RX) THEN
            RX=PX(I)
          ENDIF
          IF(PY(I).LT.BY) THEN
            BY=PY(I)
          ELSEIF(PY(I).GT.TY) THEN
            TY=PY(I)
          ENDIF
 100    CONTINUE
      ENDIF

      IF(LX.GT.QSS4) LX=LX-QSS4
      RX=RX+QSS4
      IF(BY.GT.QSS4) BY=BY-QSS4
      TY=TY+QSS4


* Set or update total extent as appropriate
      IF(QSS5.LT.0.0) THEN
*       initialise because not yet set
        QSS5=LX
        QSS6=RX
        QSS7=BY
        QSS8=TY
      ELSE
*       update according to direction of expansion
        IF(LX.LT.QSS5) QSS5=LX
        IF(RX.GT.QSS6) QSS6=RX
        IF(BY.LT.QSS7) QSS7=BY
        IF(TY.GT.QSS8) QSS8=TY
      ENDIF


* Deduce whether a scan is relevant
      SCAN=(QSS1.GE.0.0 .AND. QSS9.GE.0.0 .AND.
     :      (.NOT. (QSS1.LT.QSS5 .OR. QSS1.GT.QSS6 .OR.
     :              QSS2.LT.QSS7 .OR. QSS2.GT.QSS8)))

* Supply pick point and tolerances regardless
      X=QSS1
      Y=QSS2
      TMIN=QSS3
      TMAX=QSS4

      END
