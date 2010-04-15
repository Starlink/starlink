C# IL>=a, OL>=0
      SUBROUTINE GKPSCI (RECT, NP,PX,PY, NCROSS,INDEX,ICLIPS, ICLPS1)
*
* (C) COPYRIGHT ICL & SERC  1989
*

*-------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Find the index of each vertex of a polygon, whose
*     preceding edge crosses the edge of the clipping rectangle
*     and the clip status of each such crossing.
*
*  MAINTENANCE LOG
*  ---------------
*     23/02/89  KEVP  Original Version Stabilised
*     26/04/89  KEVP  Changed name from GKFCXI.
*     22/07/90  PLP   Commenting brought in line with standard format.
*
*  ARGUMENTS
*  ---------
*     INP  RECT   The clipping rectangle (xmin,xmax,ymin,ymax)
*     INP  NP     Number of vertices in the polygon
*     INP  PX,PY  The polygon
*     INP  NCROSS The maximum number of crossings of the
*                 clipping rectangle edges by the polygon edges
*     OUT  INDEX  The indices of the edges crossed by the rectangle
*     OUT  ICLIPS The clip status of each such crossing
*     OUT  ICLPS1 The clip status of the first vertex of the polygon
*
      INTEGER NP, NCROSS, INDEX(NCROSS),ICLIPS(NCROSS), ICLPS1
      REAL    RECT(4), PX(NP),PY(NP)

*  COMMON BLOCK USAGE
*  ------------------
*     None
*
*  LOCALS
*  ------
*     CROSS   True, if crossing occurs
*     ICLPSA  Clip status of first point in pair
*     ICLPSB  Clip status of second point in pair
*     ICLD    Clip status difference
*     ICLS    Clip status sum
*     ICROSS  Index in (1:NCROSS)
*     IPA,IPB Indices in (1:NP)
*
      INTEGER ICROSS, ICLPSA, ICLPSB, ICLD, ICLS, IPA,IPB
      LOGICAL CROSS
*---------------------------------------------------------------------
      ICROSS = 1
*     Clip status of first point
      IF(PX(1) .LT. RECT(1))THEN
         ICLPSA = -1
      ELSEIF(PX(1) .GT. RECT(2))THEN
         ICLPSA = 1
      ELSE
         ICLPSA = 0
      ENDIF
      IF(PY(1) .LT. RECT(3))THEN
         ICLPSA = ICLPSA - 10
      ELSEIF(PY(1) .GT. RECT(4))THEN
         ICLPSA = ICLPSA + 10
      ENDIF
*     Set clip status of first vertex of polygon
      ICLPS1 = ICLPSA
*
      DO 300 IPA=1,NP
*       Clip status of second point in pair
        IF(IPA .LT. NP)THEN
*         First point in pair not last point of polygon
          IPB = IPA + 1
          IF(PX(IPB) .LT. RECT(1))THEN
             ICLPSB = -1
          ELSEIF(PX(IPB) .GT. RECT(2))THEN
             ICLPSB = 1
          ELSE
             ICLPSB = 0
          ENDIF
          IF(PY(IPB) .LT. RECT(3))THEN
             ICLPSB = ICLPSB - 10
          ELSEIF(PY(IPB) .GT. RECT(4))THEN
             ICLPSB = ICLPSB + 10
          ENDIF
        ELSE
*         Last point joined to first
          IPB = 1
          ICLPSB = ICLPS1
        ENDIF
*       If there is a change in clip status
        IF(ICLPSA .NE. ICLPSB)THEN
           IF((ICLPSA .EQ. 0) .OR. (ICLPSB .EQ. 0))THEN
*          One point outside
              INDEX(ICROSS) = IPA + 1
              ICLIPS(ICROSS) = ICLPSA + ICLPSB
              ICROSS = ICROSS+1
           ELSE
*          Both points outside (but line may cross the rectangle)
              ICLD = IABS(ICLPSB - ICLPSA)
              ICLS = ICLPSB + ICLPSA
              IF((ICLD .GT. 2).AND.(ICLD .NE. 10*(ICLD/10)))THEN
*                Does the line cross the clipping rectangle?
                 CALL GKLNCR(RECT,PX(IPA),PY(IPA),
     :                       PX(IPB),PY(IPB),CROSS)
              ELSE
*                Does the line cross opposite sides of the rectangle?
                 CROSS = (ICLS .EQ. 0)
              ENDIF
              IF(CROSS)THEN
                 INDEX(ICROSS) = IPA+1
                 INDEX(ICROSS+1) = IPA+1
                 ICLIPS(ICROSS) = ICLPSA
                 ICLIPS(ICROSS+1) = ICLPSB
                 ICROSS = ICROSS+2
              ENDIF
           ENDIF
        ENDIF
*       Move to next two points
        ICLPSA = ICLPSB
  300 CONTINUE

      END
