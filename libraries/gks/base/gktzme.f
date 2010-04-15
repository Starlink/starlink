C# IL>=a, OL>=0
      SUBROUTINE GKTZME(NV,VX,VY,NP,IPOLY,IETLOW,
     :                  VXETT,IYETT,IETB,JETNXT)
*
* (C) COPYRIGHT ICL & SERC  1991
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Constructs a sorted Edge Table for the supplied polygon set
*     based on indices.
*
*  MAINTENANCE LOG
*  ---------------
*     29/11/91  KEVP  Original version stabilized (C69).
*
*  ARGUMENTS
*  ---------
*     INP NV      No of vertices
*     INP VX,VY   Vertex coordinates
*     INP NP      Number of Polygons
*     INP IPOLY   Indices of Last Vertex of each Polygon
*     OUT IETLOW  Vertex index of lowest vertex
*     OUT VXETT   Edge top X-coods
*     OUT IYETT   Vertex indices of edge top Y-coords
*     OUT IETB    Vertex indices of edge bottoms
*     OUT JETNXT  Edge index of next edge (in order of tops downwards)
*
      INTEGER NV, NP, IPOLY(NP),
     :        IETLOW, IYETT(NV),IETB(NV), JETNXT(0:NV)
      REAL VX(NV), VY(NV), VXETT(NV)
*
*     Note:  Though on stack, the Edge Table array bases are passed
*     down as parameters so that they may be re-indexed from zero
*     or one.
*
*     Edges are indexed 1 to NV.
*     Index 0 is reserved to index the first (ie, highest edge)
*     in JETNXT and as a value of JETNXT for the last (ie, lowest edge).
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkmc.par'
*                        for QTOL
*
*  LOCALS
*  ------
*     IBOT   Bottom Vertex of edge
*     IEND   End Vertex of current polygon
*     ILAST  Last Vertex
*     IPOSS  Possible Vertex
*     ISTART Starting Vertex of current polygon
*     ITHIS  Last Vertex
*     ITOP   Top Vertex of edge
*     IV     Current Vertex - loop index in (1:NV)
*     J      Loop index for edges (1:JEND+1) never referred to
*     JEND   End of edge table
*     JLPOSS Last Possible Edge
*     JPOSS  Possible Edge
*     LP     Current Polygon - loop index in (1:NP)
*
      INTEGER IV, ILAST,ITHIS, ITOP,IBOT, ISTART,IEND, IPOSS,
     :        J,  JLPOSS,JPOSS, JEND, LP
*
*     Note: I... variables are indices of VERTICES,
*           J... variables are indices of EDGES and
*           L... variables are indices of POLYGONS.
*
*  ALGORITHM
*  ---------
*     Constructs a sorted Edge Table from the supplied polygon set.
*     This consists of
*          X-coords of vertices (for edge bottoms)
*          Y-coords of vertices (for either end of edge)
*          X-coords of edge tops
*          vertex indices of edge tops (for Y-coords)
*          vertex indices of edge bottoms (for both coords)
*          edge index of next edge down
*     All edges are included (including horizontal), when the
*     edge table is made, but if a vertex with two edge tops
*     can be converted into a horizontal edge, which would not
*     be included, but is implicit from the ends having the same
*     vertex index for Y (called a Peak Edge).
*
*     NV contains the length of the vertex arrays, (or at least,
*     the length we are interested in).
*
*     NP contains the length of the polygon array.
*
*     NV  determines the extent of the stack-space arrays acquired
*     for use as the components of the Edge Table: -the actual arguments
*     corresponding to VXETT, IYETT, IETB and JETNXT.
*
*     on Exit: IETLOW indicates the lowest limit of the edges.
*
*     Edges of infinitessimal length are not included in the edge table,
*     but the first vertex of such an edge is used as the first vertex of
*     the next edge.
*
*
*---------------------------------------------------------------------

* start with empty Edge table
      JETNXT(0) = 0
      JEND = 0
      IETLOW = VY(IPOLY(NP))
*
* set index of first vertex
      ISTART = 1

      DO 40 LP=1,NP
* For each polygon ....
* we will be starting with the edge from the last vertex to the first
        IEND = IPOLY(LP)
*       Check length of last edge
        DO 10 IV=IPOLY(LP),ISTART+3,-1
           IF((ABS(VY(IV)-VY(IV-1)) .LE. QTOL) .AND.
     :        (ABS(VX(IV)-VX(IV-1)) .LE. QTOL))THEN
*          Edge length infinitessimal.
*             cut it out and check new last edge.
              IEND = IV-1
           ELSE
              GOTO 11
           ENDIF
   10   CONTINUE
   11   CONTINUE
        IF(IEND-ISTART .LT. 3)THEN
*          Polygon has too few vertices - move to next polygon.
           ISTART = IPOLY(LP) + 1
           GOTO 40
        ENDIF
        ITHIS = IEND

        DO 30 IV=ISTART,IEND
* For each vertex in the polygon ....
           ILAST = ITHIS
           ITHIS = IV

*  update lowest extent
           IF (VY(ITHIS) .LT. VY(IETLOW)) IETLOW = ITHIS
*  check length of edge
           IF((ABS(VY(ITHIS)-VY(ILAST)) .LE. QTOL) .AND.
     :        (ABS(VX(ITHIS)-VX(ILAST)) .LE. QTOL))THEN
               ITHIS = ILAST
               GOTO 30
           ENDIF
*  set edge ends
           IF (VY(ITHIS) .GT. VY(ILAST)) THEN
*          This above last
              ITOP  = ITHIS
              IBOT  = ILAST
           ELSE
*          This below last
              ITOP  = ILAST
              IBOT  = ITHIS
           ENDIF

*  now find a home for it
           JLPOSS = 0
           JPOSS  = JETNXT(JLPOSS)

           DO 20 J=1,JEND+1
*  sort on higher Y
              IF(JPOSS.EQ.0)GOTO 21
              IPOSS = IYETT(JPOSS)
              IF(VY(IPOSS) .GT. VY(ITOP))THEN
                 JLPOSS = JPOSS
                 JPOSS = JETNXT(JLPOSS)
              ELSEIF(VY(IPOSS) .GE. VY(ITOP))THEN
*  sort on lower vertex Y-index (so that equal Y-indices are together)
                 IF(IPOSS .LT. ITOP)THEN
                    JLPOSS = JPOSS
                    JPOSS = JETNXT(JLPOSS)
                 ELSE
                    GOTO 21
                 ENDIF
              ELSE
                 GOTO 21
              ENDIF
   20      CONTINUE
*          should not get here, if JETNXT properly formed.
   21      CONTINUE

*  we need to insert new edge between (JLPOSS) and (JPOSS)

*  allocate new slot in the Edge Index Space (J...)
           JEND = JEND + 1
           VXETT(JEND) = VX(ITOP)
           IYETT(JEND) = ITOP
           IETB(JEND) = IBOT
           JETNXT(JEND) = JPOSS
           JETNXT(JLPOSS) = JEND
*  finished with vertex
  30    CONTINUE
*  set starting vertex for next polygon
        ISTART = IPOLY(LP) + 1
*  finished with polygon
  40  CONTINUE


  999 CONTINUE
      END
