C# IL>=a, OL>=0
      SUBROUTINE GKPPFA(XP,YP,NRD,RX,RY,INSIDE)
*
*---------------------------------------------------------------------
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
*     Determines whether a particular point (the pick point)
*     is inside a fill area primitive or not
*     The pick point is assumed not to be on the boundary.
*
*
*  MAINTENANCE LOG
*  ---------------
*     02/02/88  KEVP  Stabilised
*     09/01/89  KEVP  Changed name from GKLPFA to GKPPFA
*
*  ARGUMENTS
*  ---------
*     INP XP,YP  The Pick Point
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates (in same coord system as pick pt)
*     OUT INSIDE True, if pick point is inside fill-area
*
      INTEGER NRD
      LOGICAL INSIDE
      REAL    XP,YP, RX(NRD),RY(NRD)
*
*  LOCALS
*  ------
*     AX,AY   Former end point of line-segment minus pick pt (A)
*     BX,BY   Latter end point of line-segment minus pick pt (B)
*     IA,IB   Indices for the end points of line-segment
*             When IA=NRD, IB=1: ie, fill area is closed.
*     NCROSS  Number of crossings above pick point counted.
*     TRI2    Twice the area of triangle formed by
*             pick point and line-segment
*             Negative, if line-segment passes pick point anti-clockwise

      REAL    AX,AY,  BX,BY, TRI2
      INTEGER IA, IB, NCROSS
*
*  ALGORITHM
*  ---------
*     Counts the number of times the primitive boundary
*     crosses the ray extending directly upwards from the
*     pick point.
*     If this is odd then, the pick point is inside
*     otherwise it is outside
*
*     In testing whether the two consecutive points are at
*     different sides of the pick point, it would be simpler
*     to use (AX*BX .LT. 0), but this could cause a crossing
*     not to be counted of be counted twice, if either AX or
*     BX is 0.
*
*-------------------------------------------------------------

      NCROSS = 0

*     First point
      AX = RX(1) - XP

*     Loop over lines joining consecutive points
      DO 10 IA=1,NRD

*        Latter point
         IB = IA+1-NRD*(IA/NRD)
         BX = RX(IB) - XP

*        If the two points are at either side of the pick point,
*        check whether the line joining them goes above.
*        If so increment the crossing count.
         IF((AX .GT. 0) .NEQV. (BX .GT. 0))THEN
           AY = RY(IA) - YP
           BY = RY(IB) - YP
           IF((AY .GT. 0) .AND. (BY .GT. 0))THEN
*          Both points above
              NCROSS = NCROSS + 1
           ELSEIF((AY .LE. 0) .AND. (BY .LE. 0))THEN
*          Neither point above
           ELSE
*          One point above and one below
              TRI2 = AY*BX - AX*BY
              IF((BX .GT. 0) .AND. (TRI2 .GT. 0))THEN
                 NCROSS = NCROSS + 1
              ELSEIF((AX .GT. 0) .AND. (TRI2 .LT. 0))THEN
                 NCROSS = NCROSS + 1
              ENDIF
           ENDIF
         ENDIF
*        Make latter pt former for next line-segment
         AX = BX
   10 CONTINUE

*     Give result
      INSIDE = (NCROSS .NE. 2*(NCROSS/2))

      RETURN
*
      END
