C# IL>=a, OL>=0
      SUBROUTINE GKTZRE (JREMOV,NV,VX,VY,VXETT,IYETT,IETB,JETNXT)
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
*     Remove an edge with a peak edge attached to it,
*     while preserving the peak edge.
*
*     Note: This edge to be removed and the edge at the other end
*     of the peak edge must have been truncated down (in Y)
*     to the bottom of the edge to be removed.
*     So that peak edge is correctly positioned
*     after the edge to be removed has been removed.
*
*  MAINTENANCE LOG
*  ---------------
*     10/10/91  KEVP  Original Version Stabilised (C69).
*
*  ARGUMENTS
*  ---------
*     INP JREMOV  Index of Edge to be Removed
*     INP NV      Number of Vertices
*     INP VX      Vertex X-coords
*     I/O VY      Vertex Y-coords
*     I/O VXETT   Edge tops (X coords)
*     INP IYETT   Vertex indices of edge tops (for Y-coords)
*     I/O IETB    Vertex indices of edge bottoms (for both coords)
*     I/O JETNXT  Edge Table Link array for ordering the edges.
*
      INTEGER JREMOV
      INTEGER NV, IYETT(NV),IETB(NV), JETNXT(0:NV)
      REAL VX(NV),VY(NV), VXETT(NV)
*
* Note: The Edge Tables are passed as arguments
*         so that they may be re-indexed from zero or one.
*
*  COMMON BLOCKS
*  -------------
      INCLUDE '../include/gkmc.par'
*                           for QTOL
*  LOCALS
*  ------
*     IBELOW  Vertex Y-index to top of edge below
*     J       Loop Index (never referred to)
*     JBELOW  Edge index of edge below (or possible such edge)
*     LBELOW  Edge index of preceeding edge below (or possible such edge)
*     TROUGH  True, if bottom of edge to be removed is a trough
*
      INTEGER J, IBELOW, JBELOW, LBELOW
      LOGICAL TROUGH
*
*
*
*  ALGORITHM
*  ---------
*     (1) Find the edge, that is below the edge to be removed.
*         If none is found, quit.
*
*     (2) Copy this edge onto the one to be removed (except Y-index).
*
*     (3) By-pass the original of this edge in the edge ordering.
*
*---------------------------------------------------------------------
*
*     Find the edge directly below the edge to be removed.
*     This will replace the edge to be removed.
      IBELOW = IETB (JREMOV)
      TROUGH = .FALSE.
      JBELOW = 0
      DO 100 J=1,NV+1
         LBELOW = JBELOW
         JBELOW = JETNXT(LBELOW)
         IF(JBELOW .EQ. 0) GOTO 102
         IF(IBELOW .EQ. IYETT(JBELOW))GOTO 105
  100 CONTINUE
*     Error, if here
      GOTO 999
  102 CONTINUE
*     No edge found below - must be a trough
      TROUGH = .TRUE.
  105 CONTINUE
*
      IF(TROUGH)THEN
*     Bottom of edge to be removed is a trough.
*        Leave edge to be removed, truncated to a single point.
*        This will be dealt later on by GKTZJP in GKTZVP.
         VXETT(JREMOV) = VX(IETB(JREMOV))
      ELSE
*     Not a trough - there is a replacement edge
*        Remove edge, by overwiting with replacement edge.
         VXETT(JREMOV) = VXETT(JBELOW)
         IETB(JREMOV)  = IETB(JBELOW)
*        and bypassing old copy of replacement edge.
         JETNXT(LBELOW) = JETNXT(JBELOW)
*     The replacement edge is already correctly placed in the edge
*     table ordering, owing to the truncation of the edge to be removed.
      ENDIF
  999 CONTINUE
      END
