C# IL>=a, OL>=1
      SUBROUTINE GKPWHL(N,PX,PY)
*
* (C) CORYRIGHT ICL & SERC  1985
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
*     Derive polyline extent and (option) scan for a hit.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC   Original version stabilised
*
*  ARGUMENTS
*  ---------
*     INP N  - number of polyline points
*     INP PX - x-coordinates of points
*     INP PY - y-coordinates of points
*
      INTEGER N
      REAL PX(N),PY(N)
*
*  COMMON BLOCK USAGE
*  ------------------
*
*  LOCALS
*  ------
*     SMALL    Value for detecting vertical and horizontal lines
*     SCAN     .TRUE. if scan required
*     I        Loop count
*     X,Y      Pick point
*     TMIN     Hit tolerance minimum
*     TMAX     Hit tolerance maximum
*     LX,RX,BY,TY  Line segment extent
*     DX,DY    Line segment vector
*     ADX,ADY  Line segment distances
*     SX,SY    Angled line derived point
*     DIST     Hit distance
*
      REAL       SMALL
      PARAMETER (SMALL=0.001)
      LOGICAL  SCAN
      INTEGER  I
      REAL     X,Y,TMIN,TMAX
      REAL     LX,RX,BY,TY
      REAL     DX,DY,ADX,ADY,SX,SY,DIST
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

* Derive polyline extent and exit unless scan required
      IF(N.GT.1) THEN
        CALL GKPWCX(N,PX,PY,SCAN,X,Y,TMIN,TMAX)
        IF( SCAN ) THEN

          DIST=1.0
          DO 100 I=1,N-1

            IF(PX(I).LT.PX(I+1)) THEN
              LX=PX(I)-TMAX
              RX=PX(I+1)+TMAX
            ELSE
              LX=PX(I+1)-TMAX
              RX=PX(I)+TMAX
            ENDIF
            IF(PY(I).LT.PY(I+1)) THEN
              BY=PY(I)-TMAX
              TY=PY(I+1)+TMAX
            ELSE
              BY=PY(I+1)-TMAX
              TY=PY(I)+TMAX
            ENDIF

            IF(.NOT. (X.LT.LX .OR. X.GT.RX .OR.
     :                Y.LT.BY .OR. Y.GT.TY)) THEN
              DX=PX(I+1)-PX(I)
              DY=PY(I+1)-PY(I)
              ADX=ABS(DX)
              ADY=ABS(DY)
              IF(ADX.GT.SMALL .AND. ADY.GT.SMALL) THEN
                SX=((DX/DY) * (Y-PY(I))) + PX(I)
                SY=((DY/DX) * (X-PX(I))) + PY(I)
                DIST=(AMIN1(ABS(X-SX),ABS(Y-SY))-TMIN) / (TMAX-TMIN)
              ELSEIF(ADX.LT.SMALL .AND. ADY.LT.SMALL) THEN
                DIST=(AMAX1(ABS(X-PX(I)),ABS(Y-PY(I)))-TMIN) /
     :                (TMAX-TMIN)
              ELSEIF(ADX.LT.SMALL) THEN
                DIST=(ABS(X-PX(I))-TMIN) / (TMAX-TMIN)
              ELSE
                DIST=(ABS(Y-PY(I))-TMIN) / (TMAX-TMIN)
              ENDIF
            ENDIF
*           stop on close or exact hit
            IF(DIST.LT.1.0) THEN
              CALL GKPWCD(DIST)
              GOTO 999
            ENDIF
 100      CONTINUE
        ENDIF
      ENDIF

 999  CONTINUE

      END
