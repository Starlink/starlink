      SUBROUTINE GKPPBB (NP,PX,PY)
*--------------------------------------------------------------------
*
*     Type of routine: Utility
*              Author: KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE
*  -------
*     Either creates a new bounding box, that accomodates
*            the set of points supplied
*         or expands an existing box to accomodate the
*            points supplied.
*
*  MAINTENACE LOG
*  --------------
*     24/02/88  KEVP  Created
*     09/01/89  KEVP  Changed name from GKLPBB to GKPPBB
*
*  ARGUMENTS
*  ---------
*     As for the device polyline routine
*     and therefore can be used as an argument in its place.
*
*     INP  NP       Number of points
*     INP  PX,PY    The points
*
      INTEGER NP
      REAL    PX(NP),PY(NP)

*  COMMOM BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*     I      Loop index for points
*     BOUND  Bounding Box   (XMIN,XMAX,YMIN,YMAX)
*     INAME  Not used
*     PRI    Not used
*
*  Indices of BOUND
*     JLEFT  Minimum X
*     JRIGHT Maximum X
*     JBASE  Minimum Y
*     JTOP   Maximum Y

      INTEGER I, JLEFT,JRIGHT,JBASE,JTOP
      REAL    BOUND(4), PRI
      INTEGER INAME
      PARAMETER (JLEFT=1,JRIGHT=2,JBASE=3,JTOP=4)
*
*  ALGORITHM
*  ---------
*     If the left and right hand sides of the box supplied
*     are in the wrong order (ie, box undefined),
*        a new box is created to accomodate the points
*        (min & max of the x and y coords)
*     else (ie, box defined)
*        the box supplied is expanded to accomodate the points
*        (min & max of the x and y coords and the box)
*
*  COMMENTS
*  --------
*     The bounding box can be initialised by executing
*     the utility GKPPIB at the CREATE SEGMENT entry point.
*
*     The utility GKLCPK takes bounding boxes to be in DC uneffected
*     by the segment transformation = CSS World Coords from GKCSRD.
*--------------------------------------------------------------------

*     Get bounding box
      CALL GKSLGE(KSSGPT(KWKIX),KCURR,INAME,PRI,BOUND)
      IF(KERROR .NE. 0)GOTO 999
      IF(INAME .LT. 0)GOTO 999


*     If the box is undefined, create new box around first pt.
      IF (BOUND(JLEFT) .GT. BOUND(JRIGHT)) THEN
         BOUND(JLEFT)  = PX(1)
         BOUND(JRIGHT) = PX(1)
         BOUND(JBASE)  = PY(1)
         BOUND(JTOP)   = PY(1)
      ENDIF

*     Expand bounding box to accomodate points
      DO 10 I=1,NP
         IF(PX(I) .LT. BOUND(JLEFT))  BOUND(JLEFT ) = PX(I)
         IF(PX(I) .GT. BOUND(JRIGHT)) BOUND(JRIGHT) = PX(I)
         IF(PY(I) .LT. BOUND(JBASE))  BOUND(JBASE ) = PY(I)
         IF(PY(I) .GT. BOUND(JTOP))   BOUND(JTOP  ) = PY(I)
   10 CONTINUE

*     Update bounding box
      CALL GKSLBB (KSSGPT(KWKIX),KCURR,BOUND)

  999 CONTINUE
      RETURN
      END
