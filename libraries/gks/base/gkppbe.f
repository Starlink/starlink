      SUBROUTINE GKPPBE
*--------------------------------------------------------------------
*
*     Type of routine: Utility
*              Author: KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE
*  -------
*     Convert an undefined bounding box
*     to an empty box (ie one with no points in it)
*
*  MAINTENACE LOG
*  --------------
*     24/02/88  KEVP  Created
*     09/01/89  KEVP  Changed name from GKLPBE to GKPPBE
*     16/11/89  RMK   Removed unused local variable.
*
*  ARGUMENTS
*  ---------
*     None: segment specification is KCURR

*  COMMOM BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*     BOUND  Bounding Box   (XMIN,XMAX,YMIN,YMAX)
*
*  Indices of BOUND
*     JLEFT  Minimum X
*     JRIGHT Maximum X
*     JBASE  Minimum Y
*     JTOP   Maximum Y
*
*     INAME  segment name
*     PRI    segment priority

      INTEGER JLEFT,JRIGHT,JBASE,JTOP, INAME
      REAL    BOUND(4), PRI
      PARAMETER (JLEFT=1,JRIGHT=2,JBASE=3,JTOP=4)
*
*  ALGORITHM
*  ---------
*     The undefined box is signified by  XMIN greater than XMAX.
*     The empty box is signified by YMIN greater than YMAX.
*
*     If the the box is undefined, it is changed to defined and empty,
*     else nothing is done.
*
*  COMMENTS
*  --------
*     Can CSS store empty segments?
*--------------------------------------------------------------------
*
      CALL GKSLGE (KSSGPT(KWKIX),KCURR,INAME,PRI,BOUND)
      IF(BOUND(JLEFT) .GT. BOUND(JRIGHT))THEN
         BOUND(JRIGHT) = BOUND(JLEFT) + 1.0
         BOUND(JBASE) = 0.0
         BOUND(JTOP) = -1.0
         CALL GKSLBB (KSSGPT(KWKIX),KCURR,BOUND)
      ENDIF
      END
