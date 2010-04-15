C# IL>=a, OL>=0
      SUBROUTINE GKTZTP (LPEAK,XCUT1,XCUT2,YCUT,
     :                    NV,VY,VXETT,IYETT,IETB,JETNXT)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Truncate a peak in an edge table down to specified
*     points, placing the truncated edge pair in its correct
*     place in the edge table.
*
*  MAINTENANCE LOG
*  ---------------
*     29/11/91  KEVP  Original Version Stabilised (C69).
*
*  ARGUMENTS
*  ---------
*     INP LPEAK   Index of Edge Preceeding Peak in Edge Table Ordering
*     INP XCUT1   X-value to be truncated to (1st edge)
*     INP XCUT2   X-value to be truncated to (2nd edge)
*     INP YCUT    Y-value to be truncated to (both edges of course)
*     INP NV      Number of Vertices - Edge Table Length
*     I/O VY      Y-coords of vertices (edge may be truncated)
*     I/O VXETT   X-coords of edge tops (edge may be truncated)
*     INP IYETT   edge top vertex indices
*     I/O IETB    edge bottom vertex indices (may be swapped)
*     I/O JETNXT  Edge Table Link array for ordering the edges.
*
      INTEGER LPEAK, NV, IYETT(NV), IETB(NV), JETNXT(0:NV)
      REAL XCUT1,XCUT2,YCUT, VY(NV), VXETT(NV)
*
* Note: The Edge Tables are passed as arguments
*         so that they may be re-indexed from one or zero.
*
*  LOCALS
*  ------
*     IPEAK  Vertex Index of Peak
*     J      Loop Index (never referred to)
*     JPEAK  Array Containing Edge Indices for Peak
*     JPOSS  Index of current edge (in do loop)
*     LPOSS  Index of edge preceeding current edge (in do loop)
*     TOPY   Peak Level (Y-coord)
*
      INTEGER J, JPEAK(2), IPEAK, JPOSS, LPOSS
      REAL    TOPY
*
*
*
*  ALGORITHM
*  ---------
*    Check that we have a peak.
*       If not quit with error -2004.
*
*    If cut is below edge top (peak level),
*       Find a new place for the edge pair (as truncated) in
*       the edge ordering.
*       Set Edge Top coordinates to those of the cut.
*
*    Else,
*       Do nothing.
*
*  NOTE
*  ----
*    The Y-index of the edge tops always remains unchanged.
*---------------------------------------------------------------------
*
*
*     Get Edge Indices
      JPEAK(1) = JETNXT(LPEAK)
      JPEAK(2) = JETNXT(JPEAK(1))
*
*     Do we really have a peak?
      IF((JPEAK(1) .GT. 0) .AND. (JPEAK(2) .GT. 0))THEN
         IF(IYETT(JPEAK(1)) .EQ. IYETT(JPEAK(2)))THEN
            IPEAK = IYETT(JPEAK(1))
            TOPY = VY(IPEAK)
         ELSE
*        Edges do not have common top vertex Y-index
            CALL GKBUG(-2004,'GKTZTP')
            GOTO 999
         ENDIF
      ELSE
*     Invalid edge Indices
         CALL GKBUG(-2004,'GKTZTP')
         GOTO 999
      ENDIF
*
      IF(YCUT .LE. TOPY)THEN
*     Cut is below peak
*
*       Temporarily remove the edges of the peak
        JETNXT(LPEAK) = JETNXT(JPEAK(2))
*
*       Search through edges to find place for truncated peak.
        JPOSS = 0
        DO 20 J=0,NV-1
           LPOSS = JPOSS
           JPOSS = JETNXT(LPOSS)
           IF(JPOSS .EQ. 0)GOTO 21
*          Pass Higher Y,
           IF(VY(IYETT(JPOSS)) .LE. YCUT)THEN
*            ... but not Lower Y
             IF(VY(IYETT(JPOSS)) .LT. YCUT)GOTO 21
*            Else,  Pass Lower Vertex Index
             IF(IYETT(JPOSS) .GE. IPEAK)GOTO 21
           ENDIF
   20   CONTINUE
*       No new place found for edge, put at end.
        LPOSS = JPOSS
        JPOSS = 0
   21   CONTINUE

*       Insert Peak in new place
        JETNXT(LPOSS) = JPEAK(1)
        JETNXT(JPEAK(2)) = JPOSS

*       Truncate Edges of Peak
        VXETT(JPEAK(1)) = XCUT1
        VXETT(JPEAK(2)) = XCUT2
        VY(IPEAK)     = YCUT
*
      ELSE
*  Cut too high - do nothing
      ENDIF
  999 CONTINUE

      END
