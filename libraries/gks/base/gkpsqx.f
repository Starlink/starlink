C# IL>=a, OL>=0
      SUBROUTINE GKPSQX(NR,RX,RY,NPOLY,IPOLY,LEX,NEX)
*
* (C) COPYRIGHT ICL & SERC  1988
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
*     Count the number of edge instersections in a polygon set.
*
*  MAINTENANCE LOG
*  ---------------
*     11/11/91  KEVP  Original Version Stabilised (C69).
*
*  ARGUMENTS
*  ---------
*     INP NR     Length of Vertex Coordinate Arrays
*     INP RX,RY  Array containing Vertex coordinates
*     INP NPOLY  Number of Polygons
*     INP IPOLY  Array Indicating Ends of Polygons in Vertex Arrays
*     INP LEX    Limit to count of edge-intersections (0 = no limit)
*     OUT NEX    Number of edge-intersections counted
*
      INTEGER NR, NPOLY, IPOLY(NPOLY), LEX, NEX
      REAL    RX(NR),RY(NR)
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
*                        for QTOL
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     COLLIN   True, if edges are collinear
*     CROSS    True, if edges cross
*     DM       Dummy array for unused arguments
*     EIX,EIY  Arrays for First Edge
*     EJX,EJY  Arrays for Second Edge
*     IEND     Index of Last vertex of First Polygon in do loop
*     IP       Index of First Polygon
*     ISTART   Index of First vertex of First Polygon
*     IV1,IV2  Vertex Indices for First Edge
*     JEND     Index of Last vertex of Second Polygon in do loop
*     JP       Index of Second Polygon
*     JSTART   Index of First vertex of Second Polygon
*     JV1,JV2  Vertex Indices for Second Edge
*     NV       Number of Vertices
*     RXMIN,RXMAX,RYMIN,RYMAX   Bounding box of First Edge

      INTEGER IV1,IV2, JV1,JV2, ISTART,IEND, JSTART,JEND, NV, IP, JP
      REAL    RXMIN,RXMAX,RYMIN,RYMAX, EIX(2),EIY(2), EJX(2),EJY(2)
      REAL    DM(2)
      LOGICAL CROSS, COLLIN
*
*  ALGORITHM
*  ---------
*
*---------------------------------------------------------------------
*

*     Check validity of number of vertices (mean per polygon at least 3)
*                                      (at most length of vertex arrays)
      NV = IPOLY(NPOLY)
      IF((NV .LT. 3*NPOLY) .OR. (NV .GT. NR))THEN
        CALL GKBUG (-2004,'GKPSQX')
        GOTO 999
      ENDIF
*     Check validity of number of polygons
      IF(NPOLY .LT. 1)THEN
         CALL GKBUG (-2004,'GKPSQX')
         GOTO 999
      ENDIF

*     Count self-intersections in each polygon
      NEX = 0
      ISTART = 1
      DO 130 IP=1,NPOLY
         IF(IPOLY(IP)-ISTART .GE. 3)THEN
*        Polygon has at least 4 edges
            IEND = IPOLY(IP)
            IF(ABS(RX(ISTART)-RX(IEND)) .LE. QTOL)THEN
               IF(ABS(RY(ISTART)-RY(IEND)) .LE. QTOL)THEN
*                 Ignore last vertex of polygon, if the same as first.
                  IEND = IEND - 1
               ENDIF
            ENDIF
            IV1 = IEND
            DO 120 IV2=ISTART,IEND-2
*             Get bounding box of first edge
              IF(RX(IV1) .LT. RX(IV2))THEN
                 RXMAX = RX(IV2)
                 RXMIN = RX(IV1)
              ELSE
                 RXMAX = RX(IV1)
                 RXMIN = RX(IV2)
              ENDIF
              IF(RY(IV1) .LT. RY(IV2))THEN
                 RYMAX = RY(IV2)
                 RYMIN = RY(IV1)
              ELSE
                 RYMAX = RY(IV1)
                 RYMIN = RY(IV2)
              ENDIF
*             Set up do loop for second edge
              JV1    = IV2 + 1
              JSTART = IV2 + 2
              IF(IV1 .EQ. IEND)THEN
                 JEND = IEND - 1
              ELSE
                 JEND = IEND
              ENDIF
              DO 110 JV2=JSTART,JEND
*                Determine whether the edges intersect
*                First check bounding boxes
                 IF((RX(JV1) .LT. RXMIN) .AND.
     :              (RX(JV2) .LT. RXMIN))THEN
                 ELSEIF((RX(JV1) .GT. RXMAX) .AND.
     :                  (RX(JV2) .GT. RXMAX))THEN
                 ELSEIF((RY(JV1) .LT. RYMIN) .AND.
     :                  (RY(JV2) .LT. RYMIN))THEN
                 ELSEIF((RY(JV1) .GT. RYMAX) .AND.
     :                  (RY(JV2) .GT. RYMAX))THEN
                 ELSE
*                Bounding boxes do intersect
                   EIX(1) = RX(IV1)
                   EIX(2) = RX(IV2)
                   EIY(1) = RY(IV1)
                   EIY(2) = RY(IV2)
                   EJX(1) = RX(JV1)
                   EJX(2) = RX(JV2)
                   EJY(1) = RY(JV1)
                   EJY(2) = RY(JV2)
                   CALL GKQLNI(EIX,EIY,EJX,EJY,CROSS,COLLIN,DM,DM)
                   IF(CROSS .AND. .NOT. COLLIN)THEN
                      NEX = NEX + 1
                      IF(NEX .EQ. LEX)GOTO 999
                   ENDIF
                 ENDIF
                 JV1 = JV2
  110         CONTINUE
              IV1 = IV2
  120       CONTINUE
         ENDIF
         ISTART = IPOLY(IP) + 1
  130 CONTINUE

*     If several polygons, count interpolygon edge intersections
      IF(NPOLY .GE. 2)THEN
         ISTART = 1
         DO 320 IP=1,NPOLY-1
            IV1 = IPOLY(IP)
            DO 310 IV2=ISTART,IPOLY(IP)
               DO 220 JP=IP+1,NPOLY
                  JSTART=IPOLY(JP-1)+1
                  JV1 = IPOLY(JP)
                  DO 210 JV2=JSTART,IPOLY(JP)
                     EIX(1) = RX(IV1)
                     EIX(2) = RX(IV2)
                     EIY(1) = RY(IV1)
                     EIY(2) = RY(IV2)
                     EJX(1) = RX(JV1)
                     EJX(2) = RX(JV2)
                     EJY(1) = RY(JV1)
                     EJY(2) = RY(JV2)
                     CALL GKQLNI(EIX,EIY,EJX,EJY,CROSS,COLLIN,DM,DM)
                     IF(CROSS .AND. .NOT. COLLIN)THEN
                        NEX = NEX + 1
                        IF(NEX .EQ. LEX) GOTO 999
                     ENDIF
                     JV1 = JV2
  210             CONTINUE
  220          CONTINUE
               IV1 = IV2
  310       CONTINUE
            ISTART = IPOLY(IP) + 1
  320   CONTINUE
      ENDIF

  999 CONTINUE

      RETURN
      END
