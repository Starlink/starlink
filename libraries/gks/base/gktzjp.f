C# IL>=a, OL>=0
      SUBROUTINE GKTZJP (JTCHA,JTCHB,JOUTB,JPREB,
     :                   NV,VXETT,IYETT,IETB,JETNXT)
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
*     Joins a pair of contiguous peak edges together,
*     where the touching edges have the same bottom vertex.
*
*  MAINTENANCE LOG
*  ---------------
*     29/10/91  KEVP  Original Version Stabilised (C69).
*
*  ARGUMENTS
*  ---------
*     INP JTCHA   Index of touching edge of Peak A
*     INP JTCHB   Index of touching edge of Peak B
*     INP JOUTB   Index of Outer Edge of Peak B
*     INP JPREB   Index of edge preceeding Peak B
*     INP NV      Number of Vertices
*     I/O VXETT   Vertex X-coords of edge tops
*     INP IYETT   Vertex indices of edge tops (for Y-coords)
*     I/O IETB    Vertex indices of edge bottoms (for both coords)
*     I/O JETNXT  Edge Table Link array for ordering the edges.
*
      INTEGER JTCHA, JTCHB, JOUTB, JPREB
      INTEGER NV, IYETT(NV), IETB(NV), JETNXT(0:NV)
      REAL  VXETT(NV)
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
*     JAFTB  Index of edge just after peak B
*
      INTEGER JAFTB
*
*
*  ALGORITHM
*  ---------
*     Check that the edges of Peak B have the same top vertex Y-index
*
*     Get index of edge after Peak B, checking that the preceeding
*     edge actually does preceed.
*
*     Check that the two touching edges have the same bottom vertex
*
*     If any of the above checks fails, report error -2004 and quit.
*
*     Copy outer edge of peak B onto touching edge of peak A,
*     except for the top Y-index.
*
*     Remove the edges of peak B.
*
*  NOTE
*  ----
*     The joined peak takes the top Y-index of peak A
*---------------------------------------------------------------------
*
*     Check validity of Peak B
      IF(IYETT(JTCHB) .NE. IYETT(JOUTB))THEN
         CALL GKBUG(-2004,'GKTZJP')
         GOTO 999
      ENDIF
*     Get index of edge after Peak B
      IF(JETNXT(JPREB) .EQ. JTCHB) THEN
         JAFTB = JETNXT(JOUTB)
      ELSEIF(JETNXT(JPREB) .EQ. JOUTB)THEN
         JAFTB = JETNXT(JTCHB)
      ELSE
*     Preceeding edge does not preceed as required
         CALL GKBUG(-2004,'GKTZJP')
         GOTO 999
      ENDIF
*
*     The two touching edges must have the same vertex at bottom.
      IF(IETB(JTCHA) .EQ. IETB(JTCHB))THEN
*        Copy Outer Edge of B onto Touching Edge of A (save, top Y-index)
         VXETT(JTCHA) = VXETT(JOUTB)
         IETB (JTCHA) = IETB (JOUTB)
*        Remove Peak B
         JETNXT(JPREB) = JAFTB
      ELSE
*     The two touching edges do not have same bottom vertex
         CALL GKBUG (-2004,'GKTZJP')
      ENDIF
  999 CONTINUE
      END
