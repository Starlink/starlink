C# IL>=a, OL>=0
      SUBROUTINE GKPPPM(XP,YP,NRD,RX,RY,XMIN,XMAX,YMIN,YMAX,SQDIST)
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
*     is from a polymarker primitive.
*
*
*  MAINTENANCE LOG
*  ---------------
*     22/02/88  KEVP  Stabilised
*     09/01/88  KEVP  Changed name from GKLPPM to GKPPPM
*
*  ARGUMENTS
*  ---------
*     INP XP,YP  The Pick Point in DC
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates in DC
*     INP XMIN,XMAX,YMIN,YMAX  Clipping Rectangle in DC
*     OUT SQDIST Squared distance of pick point from the polymarker
*                in squared DC units
*
      INTEGER NRD
      REAL    XP,YP, RX(NRD),RY(NRD), SQDIST
      REAL    XMIN,XMAX,YMIN,YMAX
*
*  LOCALS
*  ------
*     AX,AY   Vector of marker pt wrt pick pt
*     P1DIST  Squared distance between pick pt and marker
*     WIDTH,HEIGHT  Dimensions of clipping rectangle
*     I       Index over polymarker

      REAL    AX,AY, P1DIST, HEIGHT,WIDTH
      INTEGER I
*
*  ALGORITHM
*  ---------
*     Simply take the minimum distance that
*     the pick point is from each marker point
*     that is within the clipping rectangle
*
*-------------------------------------------------------------


      HEIGHT = YMAX - YMIN
      WIDTH  = XMAX - XMIN
      SQDIST = WIDTH*WIDTH + HEIGHT*HEIGHT
      DO 10 I=1,NRD
         AX = RX(I) - XP
         AY = RY(I) - YP
         IF((RX(I) .LT. XMIN) .OR. (XMAX .LT. RX(I))) GOTO 10
         IF((RY(I) .LT. YMIN) .OR. (YMAX .LT. RY(I))) GOTO 10
         P1DIST = AX*AX + AY*AY
         IF(P1DIST .LT. SQDIST) SQDIST = P1DIST
   10 CONTINUE

      RETURN
      END
