      SUBROUTINE GKPPBU
*--------------------------------------------------------------------
*
*     Type of routine: Utility
*              Author: KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE
*  -------
*     Set the bounding boxes of all segments as undefined
*
*  MAINTENACE LOG
*  --------------
*     04/05/89  KEVP  Created
*
*  ARGUMENTS
*  ---------
*     none
*
*  COMMOM BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     BOUND  Bounding Box   (XMIN,XMAX,YMIN,YMAX)
*     ISLPTR Segment list pointer
*     N      Segment number (in list)
*     NSEG   Segment name
*
*  Indices of BOUND
*     JLEFT  Minimum X
*     JRIGHT Maximum X

      INTEGER ISLPTR, JLEFT,JRIGHT, N, NSEG
      REAL    BOUND(4)
      PARAMETER (JLEFT=1,JRIGHT=2)
*
*  ALGORITHM
*  ---------
*     For each segment's bounding box,
*     set XMIN to be greater than XMAX to indicate that
*     the box is undefined
*
*  COMMENTS
*  --------
*     If the GKPPBx bounding box utilities are to be used,
*       This should be executed in the SET WORKSTATION VIEWPORT
*       and SET WORKSTATION WINDOW entry points.
*--------------------------------------------------------------------
      ISLPTR = KSSGPT(KWKIX)
      IF(ISLPTR .NE. KNIL)THEN
        BOUND(JLEFT)  =  0.0
        BOUND(JRIGHT) = -1.0
        N = 1
   10   CONTINUE
        CALL GKSLQN (ISLPTR,N,NSEG)
        IF(NSEG .NE. KNIL)THEN
          CALL GKSLBB (ISLPTR,NSEG,BOUND)
          N = N + 1
          IF(KERROR .EQ. 0)GOTO 10
        ENDIF
      ENDIF
      END
