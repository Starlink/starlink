C# IL>=a, OL>=1
      SUBROUTINE GKPWHP(N,PX,PY)
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
*     Derive polygon extent and (option) scan for a hit.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC   Original version stabilised
*     06/08/90  KEVP  Unused local variable romoved (S342).
*
*  ARGUMENTS
*  ---------
*     INP N  - number of polygon points
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
*     SMALL    Value for detecting vertical and horizontal edges
*     SCAN     .TRUE. if scan required
*     I        Loop count
*     IC       Intersect count
*     X,Y      Pick point
*     TMIN     Hit tolerance minimum
*     TMAX     Hit tolerance maximum
*     LX,RX,BY,TY  Edge extent
*     X0,Y0    First point of edge
*     DX,DY    Edge vector
*     ADX,ADY  Edge distances
*     SX       Angled edge derived x-position
*
      REAL       SMALL
      PARAMETER (SMALL=0.001)
      LOGICAL  SCAN
      INTEGER  I,IC
      REAL     X,Y,TMIN,TMAX
      REAL     LX,RX,BY,TY
      REAL     X0,Y0,DX,DY,ADX,ADY,SX
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
*    Pick point is inside if line from (X,Y) to (0,Y) intersects
*    polygon an odd number of times.
*
*---------------------------------------------------------------------

* Check suitable number of points


      IF(N.GT.2) THEN
*       obtain polygon extent
        CALL GKPWCX(N,PX,PY,SCAN,X,Y,TMIN,TMAX)
        IF ( SCAN ) THEN

          IC=0
          X0=PX(N)
          Y0=PY(N)

          DO 100 I=1,N

            IF(X0.LT.PX(I)) THEN
              LX=X0-TMAX
              RX=PX(I)+TMAX
            ELSE
              LX=PX(I)-TMAX
              RX=X0+TMAX
            ENDIF
            IF(Y0.LT.PY(I)) THEN
              BY=Y0-TMAX
              TY=PY(I)+TMAX
            ELSE
              BY=PY(I)-TMAX
              TY=Y0+TMAX
            ENDIF

            IF(.NOT. (X.LT.LX .OR. Y.LE.BY .OR. Y.GT.TY)) THEN

              IF(X.GT.RX) THEN
                IC=IC+1
              ELSE
                DX=PX(I)-X0
                DY=PY(I)-Y0
                ADX=ABS(DX)
                ADY=ABS(DY)

                IF(ADX.GT.SMALL .AND. ADY.GT.SMALL) THEN
                  SX=((DX/DY) * (Y-Y0)) + X0
                  IF(X.GT.SX) IC=IC+1
                ELSEIF(ADY.GT.SMALL) THEN
                  IF(X.GT.PX(I)) IC=IC+1
                ENDIF
              ENDIF

            ENDIF

            X0=PX(I)
            Y0=PY(I)
 100      CONTINUE

*         record exact hit if inside
          IF(MOD(IC,2).NE.0) CALL GKPWCD(-1.0)
        ENDIF
      ENDIF

      END
