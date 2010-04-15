C# IL>=a, OL>=0
      SUBROUTINE GKTZRH (NV,VX,VY,VXETT,IYETT,IETB,JETNXT,NRE,FOUND)
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
*     Remove all horizontal edges from the top of the edge table.
*
*  MAINTENANCE LOG
*  ---------------
*     28/10/91  KEVP  Original Version Stabilised (C69).
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
*     OUT NRE     Number of edges removed
*     OUT FOUND   True, if at least one pair of sloping edges found
*
      INTEGER NV, IYETT(NV),IETB(NV), JETNXT(0:NV), NRE
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
*  LOCALS
*  ------
*     IBOT    Vertex index of edge bottom
*     ITOP    Vertex index of edge top
*     ISIDE   Do loop index over left/right - Current Side
*     J1,J2   Do loop indices (never referred to)
*     JPEAK   Array of edge indices for peak
*     JPOSS   Index of possible edge
*     NPEAK   Number of peaks found
*     NSLOPE  Number of sloping edges found
*     NTROGH  Number of troughs found
*     ETTOP   Level of edge table top
*
      INTEGER ITOP, IBOT, ISIDE, J1,J2, JPOSS, JPEAK(0:2)
      INTEGER NPEAK, NSLOPE, NTROGH
      REAL    ETTOP
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
*     For edges from the start of the edge table in the edge table
*     ordering, until one is reached, whose top is lower than the first,
*        Find a peak and remove all horizontal edges leading from it.
*
*     Count number of peaks found and number of edges removed.
*     Report error if no peaks were found and return number of edges
*     removed.
*
*---------------------------------------------------------------------

      NRE = 0
      NPEAK = 0
      NSLOPE = 0
      NTROGH = 0
      FOUND = .FALSE.

*     Check if edge table is empty
      IF(JETNXT(0) .EQ. 0) GOTO 999
      ETTOP = VY(IYETT(JETNXT(0)))

      JPEAK(1) = 0
      JPEAK(2) = JETNXT(0)
*
      DO 200 J2=1,NV/2
*       Find Next Peak
        JPEAK(0) = JPEAK(1)
        JPEAK(1) = JPEAK(2)
        JPEAK(2) = JETNXT(JPEAK(2))
        IF(JPEAK(2) .EQ. 0)GOTO 201
        IF(ETTOP-VY(IYETT(JPEAK(2))) .GT. 0)THEN
           IF(NSLOPE .GT. 0)THEN
*          Removal of horizontal edges at top complete
              GOTO 201
           ELSE
*          Top must have been removed completely, move to new top
              ETTOP = VY(IYETT(JPEAK(1)))
           ENDIF
        ENDIF
        IF(IYETT(JPEAK(1)) .EQ. IYETT(JPEAK(2)))THEN
*          Peak Found
           NPEAK = NPEAK + 1
           DO 150 ISIDE=1,2
*             Remove all horizontal edges till sloping edge or trough
              JPOSS = JPEAK(ISIDE)
              DO 100 J1=1,NV
                 ITOP = IYETT(JPOSS)
                 IBOT = IETB(JPOSS)
                 IF(VY(ITOP)-VY(IBOT) .LE. QTOL)THEN
*                  Edge horizontal remove it
                   CALL GKTZRE(JPOSS,NV,VX,VY,VXETT,IYETT,IETB,JETNXT)
                   NRE = NRE + 1
                   IF(IETB(JPOSS) .EQ. IBOT)THEN
*                  Trough reached,
                     NTROGH = NTROGH + 1
                     IF(IETB(JPEAK(1)) .EQ. IETB(JPEAK(2)))THEN
*                    Trough connects peak, remove peak completely
                        JETNXT(JPEAK(0)) = JETNXT(JPEAK(2))
                        NRE = NRE + 1
                        NTROGH = NTROGH + 1
*                       Reset edge indices for peak
                        JPEAK(1) = JPEAK(0)
                        JPEAK(2) = JETNXT(JPEAK(1))
                        GOTO 151
                     ENDIF
*                    Quit loop
                     GOTO 101
                   ENDIF
                 ELSE
*                Sloping edge found, quit loop
                   NSLOPE = NSLOPE + 1
                   GOTO 101
                 ENDIF
  100         CONTINUE
  101         CONTINUE
  150      CONTINUE
  151      CONTINUE
        ENDIF
  200 CONTINUE
  201 CONTINUE

      IF(NPEAK .LT. 1)THEN
*     No peaks found - invalid edge table
         CALL GKBUG (-2004,'GKTZRH')
      ELSE
         FOUND = ((NSLOPE .GE. 2) .AND. (NSLOPE .EQ. 2*(NSLOPE/2)))
      ENDIF
*
  999 CONTINUE
      END
