C# IL>=a, OL>=0
      SUBROUTINE GKPPBP(XP,YP, NRD,RX,RY, SQDIST)
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
*     Determines the squared distance between the pick point
*     and the minimum box with horizontal and vertical sides,
*     which contains a given set of points.
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/88  KEVP  Created
*     09/01/89  KEVP  Changed name from GKLPBP to GKPPBP
*
*  ARGUMENTS
*  ---------
*     INP XP,YP  The Pick Point
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates (in same coord system as pick pt)
*     OUT SQDIST Squared distance of pick point from box
*
      INTEGER NRD
      REAL    XP,YP, RX(NRD),RY(NRD), SQDIST
*
*  LOCALS
*  ------
*     XLEFT,XRIGHT
*     YBASE,YTOP   Sides of the box
*     I            Index over set of vertices
*     OUT          How far pick point is out from a side of the box
*
      REAL XLEFT,XRIGHT, YBASE,YTOP, OUT(4)
      INTEGER I
*-------------------------------------------------------------
*
*     Initialise squared distance as zero.
      SQDIST = 0.0

*     Initialise box at first point
      XLEFT  = RX(1)
      XRIGHT = RX(1)
      YBASE  = RY(1)
      YTOP   = RY(1)

*     Test whether pick point is in box
*     If not,
*        expand box to accomodate next point
*        and repeat,
*     Else, quit leaving squared distance as zero.
*
      DO 10 I=2,NRD
         IF((XLEFT .LE. XP) .AND. (XP .LE. XRIGHT))THEN
           IF((YBASE .LE. YP) .AND. (YP .LE. YTOP))THEN
              GOTO 9999
           ENDIF
         ENDIF
         IF(RX(I) .LT. XLEFT)  XLEFT  = RX(I)
         IF(RX(I) .GT. XRIGHT) XRIGHT = RX(I)
         IF(RY(I) .LT. YBASE)  YBASE  = RY(I)
         IF(RY(I) .GT. YTOP)   YTOP   = RY(I)
   10 CONTINUE
*
*     Here determine squared distance of pick pt from complete box
      OUT(1) = XLEFT - XP
      OUT(2) = XP - XRIGHT
      OUT(3) = YBASE - YP
      OUT(4) = YP - YTOP
      DO 20 I=1,4
         IF(OUT(I) .GT. 0.0) SQDIST = SQDIST + OUT(I)*OUT(I)
   20 CONTINUE


 9999 CONTINUE
      END
