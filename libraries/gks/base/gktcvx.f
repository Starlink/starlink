C# IL>=a, OL>=0
      SUBROUTINE GKTCVX (NP,PX,PY,ICOVX)
*
* (C) COPYRIGHT ICL & SERC  1988
*

*--------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:  KEVP
*
      INCLUDE '../include/check.inc'

*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Determine whether a simple polygon is convex (or not)
*     and its direction (clockwise or anti-clockwise) if convex.
*
*
*  MAINTENANCE LOG
*  ---------------
*     01/07/88  KEVP  Original version stabilized.
*     22/07/90  PLP   Removed unused local varable CONVEX,
*                     brought comments in line with standard format.
*     12/03/91  PLP   Declared PX & PY as arrays.
*     10/12/91  KEVP  Initialized IBCY & IABY (C91).
*
*  ARGUMENTS
*  ---------
*     INP   NP      Number of vertices
*     INP   PX,PY   Coordinates of vertices (raster coordinates)
*     OUT   ICOVX   -1 = Convex and Clockwise
*                    0 = Not convex
*                    1 = Convex and Anticlockwise
*
      INTEGER NP, ICOVX
      REAL    PX(*),PY(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
*
*  LOCALS
*  ------
*
      INTEGER    I, IAX,IAY, IBX,IBY, ICX,ICY
      INTEGER    IABX,IABY, IBCX,IBCY, IABC
      INTEGER    LTURN, NTURN

*------------------------------------------------------------------------

*     Initialise variables
      ICOVX = 0
      IAX = INT(PX(NP-1))
      IAY = INT(PY(NP-1))
      IBX = INT(PX(NP))
      IBY = INT(PY(NP))
      LTURN = 0
*
*     Loop over consecutive triples
      DO 10 I=1,NP
         ICX = INT(PX(I))
         ICY = INT(PY(I))
         IABX = IBX - IAX
         IABY = IBY - IAY
         IBCX = ICX - IBX
         IBCY = ICY - IBY
*     +  The cross product IABC (when not zero) +
*     +  is of constant sign                    +
*     +  if and only if the polygon is convex.  +
         IABC = (IABX*IBCY - IABY*IBCY)
*     +  Its sign (if constant) is +ve anticlockwise +
*     +                            -ve clockwise     +
         IF(IABC .GT. 0)THEN
            IF(LTURN .LT. 0) GOTO 99
            NTURN = 1
         ELSEIF(IABC .LT. 0)THEN
            IF(LTURN .GT. 0) GOTO 99
            NTURN = -1
         ELSE
            NTURN = LTURN
         ENDIF
*
*        Set variables for next consecutive triple
         IAX = IBX
         IAY = IBY
         IBX = ICX
         IBY = ICY
         LTURN = NTURN
   10 CONTINUE

      ICOVX  =  NTURN

   99 CONTINUE
      RETURN
      END
