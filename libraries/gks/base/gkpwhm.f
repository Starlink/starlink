C# IL>=a, OL>=1
      SUBROUTINE GKPWHM(N,PX,PY)
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
*     Derive polymarker extent and (option) scan for a hit.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC   Original version stabilised
*
*  ARGUMENTS
*  ---------
*     INP N  - number of polymarker points
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
*     I        Loop count
*     SCAN     .TRUE. if scan required
*     X,Y      Pick point
*     TMIN     Hit tolerance minimum
*     TMAX     Hit tolerance maximum
*     DIST     Hit distance
*
      INTEGER  I
      REAL     X,Y,TMIN,TMAX,DIST
      LOGICAL  SCAN
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

* Derive polmarker extent and exit unless scan required
      IF(N.GT.0) THEN
        CALL GKPWCX(N,PX,PY,SCAN,X,Y,TMIN,TMAX)
        IF( SCAN ) THEN
          DO 100 I=1,N
            DIST=(AMAX1(ABS(X-PX(I)),ABS(Y-PY(I)))-TMIN) / (TMAX-TMIN)
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
