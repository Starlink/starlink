C# IL>=a, OL>=1
      SUBROUTINE GKPPPR(ITYPE,XP,YP,NR,RX,RY,BOXDEF,SQTOL,SQPKAP,
     :                  SQD,INSIDE)
*
*---------------------------------------------------------------------
*
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Primitive Pick scan utility
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/88  KEVP  Stabilised
*     12/10/88  KEVP  Made sure the GDP Arc is put forward for pick,
*                     if it is within the pick aperture of the pick point
*                     and that distance is set to zero for pick point on
*                     the boundary of a fill area polygon.
*     05/01/89  KEVP  Changed Name from GKLPPR to GKPPPR
*     16/05/89  KEVP  Changed PICKIT to INSIDE, I/O SQDIST to OUT SQD
*                     INP SQFABT to INP SQTOL
*     16/01/89  KEVP  Count pick point as being inside any primitive
*                     If closer than SQRT(SQTOL), so to end search.
*
*
*  ARGUMENTS
*  ---------
*     INP  ITYPE   Type of primitive
*     INP  XP,YP   The Pick Point
*     INP  NR      Number of points
*     INP  RX,RY   The points (DC, except for GDP then CSS-WC)
*     INP  BOXDEF  True, if bounding box needs defining (GDP use only)
*     INP  SQTOL   The squared tolerence for direct hit on edges (sq DC)
*     INP  SQPKAP  The squared pick aperture in Square DC
*     OUT  SQD     Squared distance between Pick pt and Primitive's
*                  edge in Square DC
*     OUT  INSIDE  Pick point inside primitive - interior picked
*
      INTEGER ITYPE, NR
      REAL XP,YP, RX(NR),RY(NR), SQTOL, SQPKAP, SQD
      LOGICAL BOXDEF, INSIDE
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /CCA/    Individual variables from CSS return in this
*                     CSS Communication Area.
*     Modify /ERR/    KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkwdt.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkcca.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  EXTERNALS
*  ---------
*     GKPPAL  Scan polyline for pick (pick pt & dists thr CSS Comm Area)
*     GKPPAF  Scan fill-area for pick (pick pt & dists thr CSS Comm Area)
*     GKPWHD  Dummy raster output routine
*
      EXTERNAL GKPPAL, GKPPAF, GKPWHD
*
*  LOCALS
*  ------
*     EXPMK  An expansion of the marker area
*            to make small markers easier to pick.
*     INSAVE Used to save Fill-Area style, while temporily changed
*            to GHOLLO.
*     RMSIZE Realised size of marker
*     SQBD   Squared distance of pick point from primitive's bounding box
*
*
      INTEGER INSAVE
      REAL    SQBD, EXPMK, RMSIZE
      PARAMETER (EXPMK=2.0)

*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*---------------------------------------------------------------------
*
*     Initialise to not pick and current distance outside pick aperture
      INSIDE = .FALSE.
      SQD = SQPKAP + 1.0
*     Find distance of pick point from primitive's bounding box
*     if far then primitive need not be scanned
      IF(ITYPE .NE. KGDP) CALL GKPPBP (XP,YP,NR,RX,RY,SQBD)

*     CASE ITYPE of

      GOTO (120,130,999,150,999,170) ITYPE - KPL + 1


*     Otherwise
      GOTO 999
*
*     -------------
*     Polyline  KPL
*     -------------
  120 CONTINUE
*     Data received and used:
*     NR       :  Number of points in polyline
*     RX,RY    :  Polyline Coordinates in DC
*
*     If not too far,
*     find distance between pick pt and polyline in DC
      IF(SQBD .LE. SQPKAP)THEN
        CALL GKPPPL (XP,YP,NR,RX,RY,SQD)
        GOTO 800
      ELSE
        GOTO 999
      ENDIF

*     --------------
*     Polymarker KPM
*     --------------
  130 CONTINUE
*     Data received and used:
*     NR       :  Number of points in polymarker
*     RX,RY    :  Polymarker Coordinates in DC
*
*     Find Realized Markersize (as for utility GKMTYP)
      CALL GKPPMS (KWMKTY(KWKIX),QNMMKS(KWKIX),QWMKSZ(KWKIX),RMSIZE)
*     If not too far,
*     find distance between pick pt and polymarker in DC
      IF(SQBD .LE. EXPMK*SQPKAP + RMSIZE*RMSIZE)THEN
        CALL GKPPPM (XP,YP,NR,RX,RY,
     :               QWCLXL(KWKIX),QWCLXR(KWKIX),
     :               QWCLYB(KWKIX),QWCLYT(KWKIX),SQD)
*       Modify distance, to take account of markersize
        SQD = SQD - RMSIZE*RMSIZE - (EXPMK-1.0)*SQPKAP
        IF(SQD .LT. 0.0)SQD = 0.0
        GOTO 800
      ELSE
        GOTO 999
      ENDIF

*     -------------
*     Fill Area KFA
*     -------------
  150 CONTINUE
*     Data received and used:
*     NR       :  Number of points in fill-area
*     RX,RY    :  Fill-Area Coordinates in DC
*
*     If not too far,
*     find distance between pick pt and fill-area boundary in DC.
      IF(SQBD .LE. SQPKAP)THEN
        CALL GKPPPL (XP,YP,NR,RX,RY,SQD)
        IF(SQD .GE. SQTOL)THEN
*         Find out whether pick point is inside fill-area.
          CALL GKPPFA (XP,YP,NR,RX,RY,INSIDE)
        ELSE
*         Count pick point as being inside fill-area.
          INSIDE = .TRUE.
        ENDIF
      ENDIF

      GOTO 800

*     ----------------------------------
*     Generalised Drawing Primitive KGDP
*     ----------------------------------
  170 CONTINUE

*     Data received and used:
*     NR     : Number of points
*     RX,RY  : Coordinates in CSS WC (not DC)
*     KSS1   : GDP identifier
*     QSS1   : X coordinate of point P
*     QSS2   : X coordinate of point Q
*     QSS3   : X coordinate of point R
*     QSS4   : Y coordinate of point P
*     QSS5   : Y coordinate of point Q
*     QSS6   : Y coordinate of point R

*     Transfer Registration points to WCA
      QWR1 = QSS1
      QWR2 = QSS2
      QWR3 = QSS3
      QWR4 = QSS4
      QWR5 = QSS5
      QWR6 = QSS6

*     Put pick point into CSS communcation area to be
*     used by GKPPAL or GKPPAF
      QSS1 = XP
      QSS2 = YP
*     Put distance as something greater than the pick aperture
      QSS3 = SQPKAP + 1.0

*     Scan GDP for pick by using GKCRCS
      IF(KSS1 .EQ. -1)THEN
*        Simple arc unfilled
         CALL GKCRCS (-1,NR,RX,RY,0.0,.FALSE.,0,GKPPAL,GKPWHD)
         SQD =QSS3
      ELSE
*        Fillable item
         QSS4 = SQTOL
         INSAVE = KWFAIS(KWKIX)
         KWFAIS(KWKIX) = GHOLLO
         CALL GKCRCS (KSS1,NR,RX,RY,0.0,.FALSE.,0,GKPPAF,GKPWHD)
         KWFAIS(KWKIX) = INSAVE
         IF(KERROR .NE. 0)GOTO 999
         SQD = QSS3
         INSIDE = (KSS2 .EQ. 1)
      ENDIF
*     Expand bounding box to accomodate GDP, if being defined
      IF(BOXDEF) CALL GKPPBC (KSS1,NR,RX,RY)

      GOTO 800

  800 CONTINUE
*     End of primitive,
*     Count primitive as inside if distance is very small, so that
*     further segments need not be searched.
      IF(SQD .LT. SQTOL)INSIDE = .TRUE.
      GOTO 999


  999 CONTINUE
      END
