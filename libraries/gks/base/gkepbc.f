C# IL>=a, OL>=0
      SUBROUTINE GKEPBC(IOPT,NRD,RX,RY,NBP,BPX,BPY)
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
*     Output a bounding polygon for GDP
*     of type ARC, CHORD, PIE or CIRCLE
*     For any other GDP option, zero vertex polygon is returned.
*
*  MAINTENANCE LOG
*  ---------------
*     22/06/89  KEVP  Stabilised
*
*  ARGUMENTS
*  ---------
*     INP   IOPT     Curve option
*                       -1: arc unstyled
*                       -2: arc (chord) with interior styled
*                       -3: arc (pie) with interior styled
*                       -4: circle with interior styled
*     INP   NRD      Number of vertices
*     INP   RX,RY    Co-ordinates of three points on arc in WC
*     OUT   NBP      Number of vertices in bounding polygon
*     OUT   BPX,BPY  Bounding Polygon (Vertex Coords in WC)
*
      INTEGER  IOPT,NRD, NBP
      REAL     RX(3),RY(3), BPX(4),BPY(4)
*
*  COMMON BLOCK USAGE
*  ------------------
*
*     Read   /GKYWCA/   Wkstn Communication area (registration pts)
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
*     RADIUS     radius in WC
*     RATIO      a geometric ratio
*     THETA      arc's turning angle (negative if clockwise)
*     TWOPI      Two pi
*     TQCOPY     copy of w/s total transform (QWTOTT)
*     XCEN,YCEN  centre of arc in WC
*     XPTS,YPTS  copy holding corrected points (back tranformed)
*
      REAL TWOPI, DECTH
      PARAMETER (TWOPI = 6.283185072, DECTH = 2.0)
      REAL DXA,DYA, RATIO
      REAL XCEN,YCEN,RADIUS,THETA
      INTEGER I,ISTAT
      REAL    XPTS(3),YPTS(3), AXFM(3,2),BXFM(3,2),TQCOPY(6)
*
*  ALGORITHM
*  ---------
*     1: Obtain details of transform. Change world coords to
*        take account of the registration points.
*
*     2: Calculate the centre and turning angle in WC
*
*     3: Generate the bounding polygon of 3 or 4 vectices
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
*---------------------------------------------------------------------

      NBP = 0
*     Check that number of points is correct for Curve Option.
      IF (.NOT.((NRD.EQ.3 .AND. IOPT.NE.JCIRC) .OR.
     :          (NRD.EQ.2 .AND. IOPT.EQ.JCIRC))
     :   )THEN
         GOTO 9999
      ENDIF
*
*     Save original total segment transformation (ie WC to DC)
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
         NBP = 2
         BPX(1) = XPTS(1)
         BPY(1) = YPTS(1)
         BPX(2) = XPTS(3)
         BPY(2) = YPTS(3)
      ELSEIF(THETA .LT. DECTH)THEN
*        small turning angle
         BPX(1) = XPTS(1)
         BPY(1) = YPTS(1)
         BPX(3) = XPTS(3)
         BPY(3) = YPTS(3)
         RATIO = 0.5/COS(THETA/2.0)
         BPX(2) = RATIO*(XPTS(1)+XPTS(3)) + (1.0-RATIO)*XCEN
         BPY(2) = RATIO*(YPTS(1)+YPTS(3)) + (1.0-RATIO)*YCEN
         IF(IOPT .EQ. JPIE)THEN
            NBP = 4
            BPX(4) = XCEN
            BPY(4) = YCEN
         ELSE
            NBP = 3
         ENDIF
      ELSE
*        large turning angle (including angles above 180 degrees)
         NBP = 4
         BPX(1) = XCEN - RADIUS
         BPY(1) = YCEN - RADIUS
         BPX(2) = XCEN + RADIUS
         BPY(2) = YCEN - RADIUS
         BPX(3) = XCEN + RADIUS
         BPY(3) = YCEN + RADIUS
         BPX(4) = XCEN - RADIUS
         BPY(4) = YCEN + RADIUS
      ENDIF

*     Restore original total segment transformation (ie WC to DC)
      DO 20 I=1,6
         QWTOTT(I,KWKIX) = TQCOPY(I)
   20 CONTINUE

 9999 CONTINUE
      END
