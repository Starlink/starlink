C# IL>=a, OL>=0
      SUBROUTINE GKTZHP (NV,VX,VY,VXETT,IYETT,IETB,JETNXT,FOUND,JTZ)
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
*     Get the two sloping edges at the highest peak from the edge table.
*
*  MAINTENANCE LOG
*  ---------------
*     08/11/91  KEVP  Original Version Stabilised (C69).
*
*  ARGUMENTS
*  ---------
*     INP NV      Number of Vertices
*     INP VX      X-coords of Vertices
*     INP VY      Y-coords of Vertices
*     I/O VXETT   X-coords of edge tops
*     INP IYETT   Vertex indices of edge tops (for Y-coords)
*     I/O IETB    Vertex indices of edge bottoms (for both coords)
*     I/O JETNXT  Edge Table Link array for ordering the edges.
*     OUT FOUND   True, if such an edge pair has been found.
*     OUT JTZ     Array containing the edge indices of the peak sides
*
      INTEGER JTZ(0:2)
      INTEGER NV, IYETT(NV),IETB(NV), JETNXT(0:NV)
      REAL VX(NV),VY(NV), VXETT(NV)
      LOGICAL FOUND
*
* Note: The Edge Tables are passed as arguments
*         so that they may be re-indexed from zero or one.
*
*  COMMON BLOCKS
*  -------------
      INCLUDE '../include/gkmc.par'
*                           for QTOL
      INCLUDE '../include/gkerr.cmn'
*                           for KERROR
*  LOCALS
*  ------
*     ISIDE   Do loop index over left/right - Current Side
*     NHRE    Number of horizontal edges removed
*     SLOPEF  True if at least one pair of slopes has been found
*
      INTEGER  ISIDE, NHRE
      LOGICAL  SLOPEF
*
*
*
*  ALGORITHM
*  ---------
*     This routine assumes that the edge table structure supplied
*     does correspond to a polygon.
*     The edges are assumed to be sorted in the order of the Y-values
*     of their tops, downwards and edges of equal Y-value by the vertex
*     index of their tops.
*
*     The edge table does not explicitly include all horizontal edges.
*     Those derived from chopping of peak (refered to as peak edges)
*     are indicated by the presence of two edges with the same vertex
*     Y-index for their tops. To find such a pair easy to find the
*     edge table is sorted to ensure so that they are consecutive.
*
*
*     (1) All horizontal edges level with the top of the edge table
*         are removed.
*
*     (2) If any edges were removed,
*         The edge table is tidied to ensure that all implicit peak
*         edges are correct.
*
*     (3) There should now be a peak at the first two edges in the
*         edge table ordering, if return this peak a report FOUND.
*
*
*---------------------------------------------------------------------
*     Get Edges
      JTZ(0) = 0
      JTZ(1) = JETNXT(0)
      JTZ(2) = JETNXT(JTZ(1))

      FOUND = .FALSE.
*
*     Check for empty edge table
      IF((JTZ(1) .EQ. 0) .OR. (JTZ(2) .EQ. 0)) GOTO 999


*     Remove all horizontal edges from top
      CALL GKTZRH (NV,VX,VY,VXETT,IYETT,IETB,JETNXT,NHRE,SLOPEF)
*
      IF(SLOPEF)THEN
*     Pair of sloping edges found, at top of edge table
*        Tidy the peaks, if any edges were removed
         IF(NHRE .GT. 0)THEN
            CALL GKTZVP (NV,VX,VY,VXETT,IYETT,IETB,JETNXT)
         ENDIF
*
*        A highest peak should now be the first two edges
         JTZ(0) = 0
         JTZ(1) = JETNXT(JTZ(0))
         JTZ(2) = JETNXT(JTZ(1))
         FOUND = (JTZ(1) .NE. 0)
         IF(FOUND) FOUND = (IYETT(JTZ(1)) .EQ. IYETT(JTZ(2)))
      ENDIF

  999 CONTINUE
      END
