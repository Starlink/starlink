      SUBROUTINE GKCRCV (XFLC,YFLC,THETA,NRD,RX,RY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*------------------------------------------------------------------

*     Type of routine: Utility
*              Author: KEVP
*
      INCLUDE '../include/check.inc'

*  PURPOSE
*  -------
*     Generate a sequence of points that form a circular arc
*
*  MAINTENANCE LOG
*  ---------------
*     08/02/88  KEVP  Created  (based on part of old GKCRCS)
*     08/04/88  KEVP  Removed unused RADIUS argument.
*
*  ARGUMENTS
*  ---------
*     INP  XFLC,YFLC     Array of three points,first point of arc
*                        centre of arc's circle and last point of arc
*     INP  THETA         Its turning angle (negative if clockwise)
*     INP  NRD           Number of points on curve (must be at least 2)
*     OUT  RX,RY         The points of the curve
*
      INTEGER  NRD
      REAL XFLC(3),YFLC(3), THETA, RX(NRD),RY(NRD)

*  LOCALS
*  ------
*     AX,AY   First point of arc
*     BX,BY   Last point of arc
*     FI      Turning angle for one vector (negative if clockwise)
*     FICOS   Cosine of FI
*     FISIN   Sine of FI
*     IPT     Index of current point
*     XCEN,YCEN   Centre of arc's circle
*     XCURR,YCURR Current point
*     XTEMP       Temporary value of X-coord for moving to next point

      INTEGER IPT
      REAL AX,AY, BX,BY, FI,FICOS,FISIN,
     :     XCEN,YCEN, XCURR,YCURR, XTEMP
*
*  COMMENTS
*  --------
*     Any one system of coordinates can be used for which the arc is
*     circular. If NRD is supplied by GKCRCN world coordinates should
*     be used.
*
*  ALGORITHM
*  ---------
*     1: If there are just two points, they are given
*
*     Otherwise:
*
*     2: The turning angle THETA is divided by the number of
*     vectors (number of pts - 1) and the SIN & COS are taken
*     to form the rotation of one point to the next.
*
*     3: The first point is put in and the next,
*     which is obtained by the above rotation is put in,
*     until the penultimate point is put in.
*
*     4: The last point (from input) is put in.
*
*----------------------------------------------------------------------

*     Get points from input array
      AX   = XFLC(1)
      AY   = YFLC(1)
      XCEN = XFLC(2)
      YCEN = YFLC(2)
      BX   = XFLC(3)
      BY   = YFLC(3)

*                 2D rotate thru 'FI'
*
*     --         [ cos(FI) ,-sin(FI) ]                             --
*     --         [ sin(FI) , cos(FI) ]                             --

      IF (NRD.GT.2) THEN
         FI    = THETA / (NRD-1)
         FISIN = SIN(FI)
         FICOS = SQRT(1 - FISIN*FISIN)
         XCURR = AX - XCEN
         YCURR = AY - YCEN

         DO 100 IPT = 1,NRD-1
            RX(IPT) = XCURR + XCEN
            RY(IPT) = YCURR + YCEN
            XTEMP   = FICOS*XCURR - FISIN*YCURR
            YCURR   = FISIN*XCURR + FICOS*YCURR
            XCURR   = XTEMP
  100    CONTINUE
         RX(NRD) = BX
         RY(NRD) = BY

      ELSEIF(NRD .EQ. 2)THEN
         RX(1) = AX
         RY(1) = AY
         RX(2) = BX
         RY(2) = BY
      ENDIF

      END
