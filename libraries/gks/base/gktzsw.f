C# IL>=a, OL>=0
      SUBROUTINE GKTZSW (JEL,JER,
     :                   NV,VX,VY,VXETT,IYETT,IETB,JETNXT)
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
*     To swap edges, if required to keep peak edges valid.
*     Also join two touching peak edges when required.
*
*  MAINTENANCE LOG
*  ---------------
*     17/10/91  KEVP  Original Version Stabilised (C69).
*
*  ARGUMENTS
*  ---------
*     INP JEL     Edge index of left edge (after swapping)
*     INP JER     Edge index of right edge (after swapping)
*     INP NV      Number of Vertices
*     INP VX      Vertex X-coords
*     INP VY      Vertex Y-coords
*     I/O VXETT   Edge tops (X coords)
*     INP IYETT   Vertex indices of edge tops (for Y-coords)
*     I/O IETB    Vertex indices of edge bottoms (for both coords)
*     INP JETNXT  Edge Table Link array for ordering the edges.
*
      INTEGER JEL, JER
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
*     IBOT     Edge bottom vertex index (used for swapping)
*     SWAP     True, if edges need swapping
*     VXBOT    Top of edge (X-coord)    (used for swapping)
*     VXLTOP   Top of Left Edge (X-coord)
*     VXRTOP   Top of Right Edge (X-coord)
*     VYLTOP   Top of Left Edge (Y-coord)
*     VYRTOP   Top of Right Edge (Y-coord)
*
      INTEGER IBOT
      REAL    VXLTOP,VYLTOP, VXRTOP,VYRTOP
      REAL    VXTOP
      LOGICAL SWAP
*
*  ALGORITHM
*  ---------
*     Check that the two edges have the same top Y
*     (ie, they are cut flush).
*
*     If not, report error -2004 and quit.
*
*     Determine whether to swap edges,
*        ie, the edges touch and cross
*        or  the edges overlap
*
*     Swap them if required
*
*
*---------------------------------------------------------------------
*
*     Are the edges cut flush?
      IF((JEL .GT. 0) .AND. (JER .GT. 0))THEN
         VYLTOP = VY(IYETT(JEL))
         VYRTOP = VY(IYETT(JER))
         IF(ABS(VYLTOP-VYRTOP) .LT. QTOL)THEN
*        Determine whether to Swap Edges, Join Peak edges or do nothing.
            VXLTOP = VXETT(JEL)
            VXRTOP = VXETT(JER)
            IF(ABS(VXLTOP-VXRTOP) .LT. QTOL)THEN
*           The Peak edges linked to the two edges just touch
               CALL GKTZSL(JEL,JER,NV,VX,VY,VXETT,IYETT,IETB,SWAP)
            ELSEIF(VXRTOP .LT. VXLTOP)THEN
*           The Peak Edges overlap - swap them
               SWAP = .TRUE.
            ELSE
*           The Peak Edges separate - Do nothing (Shouldn't get here)
               SWAP = .FALSE.
            ENDIF
*
            IF(SWAP)THEN
*             Swap edge top X-coords and Bottoms (not top Y-indices)
              IBOT = IETB(JEL)
              IETB(JEL) = IETB(JER)
              IETB(JER) = IBOT
              VXTOP = VXETT(JEL)
              VXETT(JEL) = VXETT(JER)
              VXETT(JER) = VXTOP
            ENDIF
         ELSE
*        Edges are at different levels.
            CALL GKBUG(-2004,'GKTZSW')
         ENDIF
      ELSE
*     Index of left or right edge is invalid.
         CALL GKBUG(-2004,'GKTZSW')
      ENDIF
*
      END
