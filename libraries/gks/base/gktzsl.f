C# IL>=a, OL>=0
      SUBROUTINE GKTZSL (JEL,JER,NV,VX,VY,VXETT,IYETT,IETB,CROSS)
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
*     Compares the slope of two edges, one from left and one from right,
*     when the top edge is on the other and then determines whether
*     they cross.
*
*  MAINTENANCE LOG
*  ---------------
*     17/10/91  KEVP  Original Version Stabilised (C69)
*
*  ARGUMENTS
*  ---------
*     INP JEL     Edge index of edge from left
*     INP JER     Edge index of edge from right
*     INP NV      Number of Vertices
*     INP VX      Vertex X-coords
*     INP VY      Vertex Y-coords
*     INP VXETT   Edge tops (X coords)
*     INP IYETT   Vertex indices of edge tops (for Y-coords)
*     INP IETB    Vertex indices of edge bottoms (for both coords)
*     OUT CROSS   True if edges cross
*
      INTEGER JEL, JER
      INTEGER NV, IYETT(NV),IETB(NV)
      REAL VX(NV),VY(NV), VXETT(NV)
      LOGICAL CROSS
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
*     HORIZL  True if, Left edge is Horizontal
*     HORIZR  True if, Right edge is Horizontal
*     SLOPEL  Slope of Left side (X per -Y)
*     SLOPER  Slope of Right side (X per -Y)
*     VXLBOT  Bottom of Left edge (X-coord)
*     VXLTOP  Top of Left edge (X-coord)
*     VXRBOT  Bottum of Right edge (X-coord)
*     VXRTOP  Top of Right edge (X-coord)
*     VYLBOT  Bottum of Left edge (Y-coord)
*     VYLTOP  Top of Left edge (Y-coord)
*     VYRBOT  Bottum of Right edge (Y-coord)
*     VYRTOP  Top of Right edge (Y-coords)
*
      LOGICAL HORIZL,         HORIZR
      REAL    SLOPEL,         SLOPER
      REAL    VXLTOP,VYLTOP,  VXRTOP,VYRTOP
      REAL    VXLBOT,VYLBOT,  VXRBOT,VYRBOT
*
*
*
*  ALGORITHM
*  ---------
*     Compares slopes, taking account of the possiblity of a horizontal
*     edge.
*
*  NOTE
*  ----
*     This routine assumes that the top of one edge is on the other edge.
*     It does not matter which edge has its top on the other.
*---------------------------------------------------------------------
*
*     Y coords for left edge and whether horizontal
      VYLTOP = VY(IYETT(JEL))
      VYLBOT = VY(IETB(JEL))
      HORIZL = (ABS(VYLTOP-VYLBOT) .LT. QTOL)
*     Y coords for right edge and whether horizontal
      VYRTOP = VY(IYETT(JER))
      VYRBOT = VY(IETB(JER))
      HORIZR = (ABS(VYRTOP-VYRBOT) .LT. QTOL)
*     X coords for both edges
      VXLTOP = VXETT(JEL)
      VXRTOP = VXETT(JER)
      VXLBOT = VX(IETB(JEL))
      VXRBOT = VX(IETB(JER))
*
      IF(HORIZL)THEN
         IF(HORIZR)THEN
*        Both edges horizontal
            CROSS = (VXRBOT .LT. VXLBOT)
         ELSE
*        Left edge horizontal
            CROSS = (VXLTOP .LT. VXLBOT)
         ENDIF
      ELSE
         IF(HORIZR)THEN
*        Right edge horizontal
            CROSS = (VXRBOT .LT. VXRTOP)
         ELSE
*        Neither edge horizontal - calculate and compare slopes
            SLOPEL = (VXLBOT-VXLTOP)/(VYLTOP-VYLBOT)
            SLOPER = (VXRBOT-VXRTOP)/(VYRTOP-VYRBOT)
            CROSS = (SLOPEL .GT. SLOPER)
         ENDIF
      ENDIF
*
      END
