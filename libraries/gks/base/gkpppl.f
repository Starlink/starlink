C# IL>=a, OL>=0
      SUBROUTINE GKPPPL(XP,YP,NRD,RX,RY,SQDIST)
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
*     Determines the distance
*     that a particular point (the pick point)
*     is from a polyline primitive.
*
*
*  MAINTENANCE LOG
*  ---------------
*     22/02/88  KEVP  Stabilised
*     09/01/89  KEVP  Changed name from GKLPPL to GKPPPL
*
*  ARGUMENTS
*  ---------
*     INP XP,YP  The Pick Point DC
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates DC
*     OUT SQDIST The squared distance of the pick pt from the primitive
*                (DC units squared)
*
      INTEGER NRD
      REAL    XP,YP, RX(NRD),RY(NRD), SQDIST
*
*  LOCALS
*  ------
*     AX,AY   Former end point of line-segment minus pick pt (A)
*     BX,BY   Latter end point of line-segment minus pick pt (B)
*     ABX,ABY Line-segment vector (A-B)
*     IA,IB   Indices for the end points of line-segment
*     P2DIST  Squared distance between pick pt and line-segment
*     SQDAB   Squared distance between A and B
*             = length of line-segment squared
*     TRI2    Twice the area of triangle formed by
*             pick point and line-segment
*
*      All real locals are in DC units
*

      REAL    AX,AY,  BX,BY, SQDAB, TRI2
      REAL    ABX,ABY, P2DIST
      INTEGER IA, IB
*
*  ALGORITHM
*  ---------
*     For each line-segment of polyline, the distance
*     between it and the pick point is calculated
*     and the minimum is taken.
*
*     Squared distances are used to eliminate the need for
*     square root calculations.
*
*-------------------------------------------------------------

*     First point
      AX = RX(1) - XP
      AY = RY(1) - YP
      SQDIST = AX*AX + AY*AY

*     Loop over lines joining consecutive points
      DO 10 IA=1,NRD-1

*        Latter point
         IB = IA+1
         BX = RX(IB) - XP
         BY = RY(IB) - YP
*        Examine triangle formed by pick pt
*        and the line-segment to see
*        if either of the angles with the line-segment
*        are obtuse
*        (for simplicity the triangle is shifted so the pick pt)
*        (is at the origin: ie, triangle AOB is used.          )
         ABX = AX - BX
         ABY = AY - BY
         IF(AX*ABX + AY*ABY .LE. 0.0)THEN
*          Angle OAB obtuse (or right-angle)
           P2DIST = AX*AX + AY*AY
         ELSEIF(BX*ABX + BY*ABY .GE. 0.0)THEN
*          Angle OBA obtuse (or right-angle)
           P2DIST = BX*BX * BY*BY
         ELSE
*        Both angles acute
*             In this case the distance is
*             twice the area of the triangle over the length of AB
*             The squared distance is calculated.
              TRI2 = ABS(AY*BX - AX*BY)
              SQDAB = ABX*ABX + ABY*ABY
              P2DIST = TRI2*TRI2/SQDAB
         ENDIF
*        Make latter pt former for next line-segment
         AX = BX
         AY = BY
*        Reduce distance to minimum
         IF(P2DIST .LT. SQDIST) SQDIST=P2DIST
   10 CONTINUE

      RETURN
      END
