C# IL>=a, OL>=0
      SUBROUTINE GKPPBC(IOPT,NRD,RX,RY)
*
* (C) COPYRIGHT ICL & SERC  1988
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  UTILITY
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Expand bounding box to accomodate GDP
*     of type ARC, CHORD, PIE or CIRCLE
*     For any other GDP option, nothing is done.
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/88  KEVP  Stabilised
*     09/01/89  KEVP  Changed name from GKLPBC to GKPPBC
*     16/01/89  KEVP  Changed Bounding Boxes from WC to DC
*
*  ARGUMENTS
*  ---------
*     INP   IOPT    Curve option
*                     -1: arc unstyled
*                     -2: arc (chord) with interior styled
*                     -3: arc (pie) with interior styled
*                     -4: circle with interior styled
*     INP   NRD     Number of vertices
*     INP   RX }    Co-ordinates of three points on arc in CSS-WC.
*     INP   RY }
*
      INTEGER  IOPT,NRD
      REAL     RX(3),RY(3)
*
*  COMMON BLOCK USAGE
*  ------------------
*
*     Read   /GKYWCA/   Wkstn Comumication area (registration pts)
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*   Values of IOPT
*     JARC   = -1 Plain Arc
*     JCHORD = -2 Chord  (fillable)
*     JPIE   = -3 Pie    (fillable)
*     JCIRC  = -4 Circle (fillable)
*
*   Values of ISTAT
*     JENDS  = 0  Arc with end points
*     JFULL  = 1  Full Circle
*     JLIN   = 2  Line
*
      INTEGER    JARC,    JCHORD,    JPIE,    JCIRC
      PARAMETER (JARC=-1, JCHORD=-2, JPIE=-3, JCIRC=-4)

      INTEGER    JENDS,   JFULL,   JLIN
      PARAMETER (JENDS=0, JFULL=1, JLIN=2)

*     AXFM       normalisation transform
*     BXFM       inverse AXFM (backtransform to WC)
*     DECTH      decisive turning angle (anything less is small)
*     DXA,DYA    vector from First point of arc to Centre
*     ISTAT      status of arc 0=with ends, 1=full, 2=line
*     NPOLY      number of vertices in bounding polygon
*     RADIUS     radius in WC
*     RATIO      a geometric ratio
*     THETA      arc's turning angle (negative if clockwise)
*     TWOPI      Two pi
*     TQCOPY     copy of w/s total transform (QWTOTT)
*     XCEN,YCEN  centre of arc in WC
*     XPOLY,YPOLY   bounding polygon
*     XPTS,YPTS  copy holding corrected points (back tranformed)
*
      REAL TWOPI, DECTH
      PARAMETER (TWOPI = 6.283185072, DECTH = 2.0)
      REAL DXA,DYA, RATIO
      REAL XCEN,YCEN,RADIUS,THETA
      INTEGER I,ISTAT, NPOLY
      REAL    XPTS(3),YPTS(3), AXFM(3,2),BXFM(3,2),TQCOPY(6)
      REAL    XPOLY(4),YPOLY(4)
*
*  ALGORITHM
*  ---------
*     1: Obtain details of transform. Change world coords to
*        take account of the registration points.
*
*     2: Calculate the centre and turning angle in WC
*
*     3: Generate a bounding polygon of 3 or 4 vectices
*        according to the following strategy.
*
*        If ARC or CHORD and the turning angle is small,
*           the bounding polygon is the triangle with
*           the line between the two endpoints
*           and the tangents of the endpoints.
*        If PIE and the turning angle is small,
*           the bounding polygon is the kite with
*           the two straight lines of the pie
*           and the tangents of the arc endpoints.
*        Else,
*           the bounding polygon is the minimum square
*           containing the whole circle.
*
*        The turning angle is considered small if it is less than
*        some fixed value, which is in turn less than 180 degrees.
*
*     4: Convert bouding polygon back to CSS-WC and expand bounding box
*        to accomodate its vertices.
*
*---------------------------------------------------------------------

*     Check that number of points is correct for Curve Option.
      IF (.NOT.((NRD.EQ.3 .AND. IOPT.NE.JCIRC) .OR.
     :          (NRD.EQ.2 .AND. IOPT.EQ.JCIRC))
     :   )THEN
         GOTO 9999
      ENDIF
*
*     Save original total segment transformation (ie CSS-WC to DC)
      DO 10 I=1,6
         TQCOPY(I) = QWTOTT(I,KWKIX)
   10 CONTINUE
*
*     Change world coords to take account of registration points.
*     (The original WC are restored at the end of the routine.)
      CALL GKMTDV(QWRA(1),QWRA(4),AXFM)
      CALL GKMTIV(AXFM,BXFM)
      CALL GKMTXF(BXFM,NRD,RX,RY,XPTS,YPTS)
      CALL GKMTML(AXFM,QWTOTT(1,KWKIX),QWTOTT(1,KWKIX))


*     -- handle circle parameters --
      IF (IOPT.EQ.JCIRC) THEN
         XPTS(3) = XPTS(1)
         YPTS(3) = YPTS(1)
         DXA    = XPTS(2) - XPTS(1)
         DYA    = YPTS(2) - YPTS(1)
         XCEN   = XPTS(2)
         YCEN   = YPTS(2)
         RADIUS = SQRT(DXA*DXA + DYA*DYA)
         THETA  = TWOPI
         ISTAT  = JFULL
      ELSE
         CALL GKCRCE (XPTS,YPTS,XCEN,YCEN,RADIUS,THETA,ISTAT)
         THETA = ABS(THETA)
      ENDIF
*
*     Generate bounding polygon.
      IF(ISTAT .EQ. JLIN)THEN
*        pts collinear
         NPOLY = 2
         XPOLY(1) = XPTS(1)
         YPOLY(1) = YPTS(1)
         XPOLY(2) = XPTS(3)
         YPOLY(2) = YPTS(3)
      ELSEIF(THETA .LT. DECTH)THEN
*        small turning angle
         XPOLY(1) = XPTS(1)
         YPOLY(1) = YPTS(1)
         XPOLY(3) = XPTS(3)
         YPOLY(3) = YPTS(3)
         RATIO = 0.5/COS(THETA/2.0)
         XPOLY(2) = RATIO*(XPTS(1)+XPTS(3)) + (1.0-RATIO)*XCEN
         YPOLY(2) = RATIO*(YPTS(1)+YPTS(3)) + (1.0-RATIO)*YCEN
         IF(IOPT .EQ. JPIE)THEN
            NPOLY = 4
            XPOLY(4) = XCEN
            YPOLY(4) = YCEN
         ELSE
            NPOLY = 3
         ENDIF
      ELSE
*        large turning angle (including angles above 180 degrees)
         NPOLY = 4
         XPOLY(1) = XCEN - RADIUS
         YPOLY(1) = YCEN - RADIUS
         XPOLY(2) = XCEN + RADIUS
         YPOLY(2) = YCEN - RADIUS
         XPOLY(3) = XCEN + RADIUS
         YPOLY(3) = YCEN + RADIUS
         XPOLY(4) = XCEN - RADIUS
         YPOLY(4) = YCEN + RADIUS
      ENDIF
*     Accomodate vertices in bounding box
      CALL GKPPBB(NPOLY,XPOLY,YPOLY)

 9999 CONTINUE
      END
