C# IL>=a, OL>=0
      SUBROUTINE GKTZVP (NV,VX,VY,VXETT,IYETT,IETB,JETNXT)
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
*     Ensure the the Peak edges, implied by the edge table are valid.
*
*  MAINTENANCE LOG
*  ---------------
*     03/12/91  KEVP  Original Version Stabilised (C69).
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
*
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
      INCLUDE '../include/gkerr.cmn'
*                           for KERROR
*  LOCALS
*  ------
*     FINAL  True, if at end of pass this is the final pass
*     IBOT   Vertex index of edge bottom
*     J0,J1,J2    Loop Indices (never referred to)
*     JAL    Array index for left hand side of peak A
*     JAR    Array index for right hand side of peak A
*     JBL    Array index for left hand side of peak B
*     JBR    Array index for right hand side of peak B
*     JEA    Array of edge indices for peak A and preceeding edge
*     JEAJ   Index of edge in peak A to be possibly form join
*     JEAO   Index of outer edge in peak A
*     JEB    Array of edge indices for peak B and preceeding edge
*     JEBJ   Index of edge in peak B to be possibly form join
*     JEBO   Index of outer edge in peak B
*     PKAQ   True, if the X-ends of peak A are unknown
*     PKASV  True, if peak A is a single vertex
*     PKBSV  True, if peak B is a single vertex
*     XPKA   Array of X-ends for Peak A
*     XPKB   Array of X-ends for Peak B
*     YPKA   Y-level of Peak A
*     YPKB   Y-level of Peak B
*
      INTEGER JEA(0:2), JEB(0:2), J0,J1,J2
      INTEGER JAL,JAR, JBL,JBR, IBOT, JEAJ,JEAO, JEBJ,JEBO
      REAL    XPKA(2),YPKA,  XPKB(2),YPKB
      LOGICAL FINAL, PKAQ, PKASV, PKBSV
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
*     Search the edges in the edge order to find a peak edge (A).
*
*     If found,
*          search for another (B) at the same Y-level
*
*          If found,
*             Determine whether they touch, overlap or don't intersect.
*
*             If they touch
*                If the touching edges have the same bottom vertex,
*                   remove them both & join the two peak edges together.
*                Else If the touching edges cross,
*                   swap them.
*
*             else if they overlap
*                Swap edges so that edges with the same vertex Y-index
*                at the top are next to each other.
*
*             else
*                do nothing.
*
*          continue search, till edge at lower level is reached.
*
*     Continue seach till last edge.
*
*     Repeat the above until no changes are made.
*
*---------------------------------------------------------------------
*
      DO 130 J0=1,NV
*     We may have to do several passes of the edge table
*     Assume to be final pass unless, something suggests otherwise.
         FINAL = .TRUE.
         PKASV = .FALSE.
         JEA(1) = 0
         JEA(2) = JETNXT(0)
         IF(JEA(2) .EQ. 0) GOTO 121
*        Look for a Peak Edge
         DO 120 J1=1,NV+1
            JEA(0) = JEA(1)
            JEA(1) = JEA(2)
            JEA(2) = JETNXT(JEA(1))
            JAL=1
            JAR=2
            PKAQ = .TRUE.
            IF(JEA(1) .EQ. 0)GOTO 121
            IF(JEA(2) .EQ. 0)GOTO 121
            IF(IYETT(JEA(1)) .EQ. IYETT(JEA(2)))THEN
*           Peak Edge Found (call it A)
               YPKA = VY(IYETT(JEA(1)))
               JEB(1) = JEA(2)
               JEB(2) = JETNXT(JEB(1))
               IF(JEB(2) .EQ. 0)GOTO 121
*              Look for another Peak Edge at the Same Level
               DO 110 J2=1,NV+1
                  JEB(0) = JEB(1)
                  JEB(1) = JEB(2)
                  JEB(2) = JETNXT(JEB(1))
                  IF(JEB(2) .EQ. 0)GOTO 120
                  IF(IYETT(JEB(1)) .EQ. IYETT(JEB(2)))THEN
*                 Another Peak Edge Found (call it B)
                     YPKB = VY(IYETT(JEB(1)))
                     IF(ABS(YPKA-YPKB) .LE. QTOL)THEN
*                       Peak edges at same level
                        IF(PKAQ)THEN
                           XPKA(1) = VXETT(JEA(1))
                           XPKA(2) = VXETT(JEA(2))
                           IF(XPKA(1) .LE. XPKA(2))THEN
                              JAL = 1
                              JAR = 2
                           ELSE
                              JAL = 2
                              JAR = 1
                           ENDIF
                           PKASV = (XPKA(JAR)-XPKA(JAL) .LE. QTOL)
                           PKAQ = .FALSE.
                        ENDIF
                        XPKB(1) = VXETT(JEB(1))
                        XPKB(2) = VXETT(JEB(2))
                        IF(XPKB(1) .LE. XPKB(2))THEN
                           JBL = 1
                           JBR = 2
                        ELSE
                           JBL = 2
                           JBR = 1
                        ENDIF
                        PKBSV = (XPKB(JBR)-XPKB(JBL) .LE. QTOL)
                        IF(XPKA(JAR)+QTOL .LT. XPKB(JBL)) GOTO 110
                        IF(XPKB(JBR)+QTOL .LT. XPKA(JAL)) GOTO 110
                        IF(XPKA(JAR)-XPKB(JBL) .LE. QTOL)THEN
*                       We have A's right touching B's left
*                         First find out if the peak edges need joining
                          JEAJ = JEA(JAR)
                          JEBJ = JEB(JBL)
                          JEBO = JEB(JBR)
                          IF(PKASV)THEN
*                         Peak A is a single point - check its left edge
                             IF(IETB(JEA(JAL)) .EQ. IETB(JEBJ))THEN
                               JEAJ = JEA(JAL)
                             ELSEIF(PKBSV)THEN
*                            Both peaks are single points
*                               check outer edges
                                IF(IETB(JEA(JAL)).EQ.IETB(JEBO))THEN
                                   JEAJ = JEA(JAL)
                                   JEBJ = JEB(JBR)
                                   JEBO = JEB(JBL)
                                ENDIF
                             ENDIF
                          ENDIF
                          IF(PKBSV)THEN
*                         Peak B is a single point - check its right edge
                             IF(IETB(JEA(JAR)).EQ.IETB(JEB(JBR)))THEN
                               JEBJ = JEB(JBR)
                               JEBO = JEB(JBL)
                             ENDIF
                          ENDIF
                          IF(IETB(JEAJ) .EQ. IETB(JEBJ))THEN
*                         The Peak Edges need joining
*                            Join the Peak Edges
                             CALL GKTZJP(JEAJ,JEBJ,JEBO,JEB(0),
     :                                  NV,VXETT,IYETT,IETB,JETNXT)
                             FINAL = .FALSE.
                             GOTO 130
                          ELSE
*                            Swap edges, if necessary
                             IBOT = IETB(JEA(JAR))
                             CALL GKTZSW(JEA(JAR),JEB(JBL),
     :                               NV,VX,VY,VXETT,IYETT,IETB,JETNXT)
                             FINAL = (IBOT .EQ. IETB(JEA(JAR)))
                          ENDIF
                        ELSEIF(XPKB(JBR)-XPKA(JAL) .LE. QTOL)THEN
*                       We have B's right touching A's left
                          JEBJ = JEB(JBR)
                          JEAJ = JEA(JAL)
                          JEAO = JEA(JAR)
                          IF(PKASV)THEN
*                         Peak A is a single point - check its right edge
                             IF(IETB(JEBJ) .EQ. IETB(JEA(JAR)))THEN
                                JEAJ = JEA(JAR)
                                JEAO = JEA(JAL)
                             ENDIF
                          ELSEIF(PKBSV)THEN
*                         Peak B is a single point - check its left edge
                             IF(IETB(JEB(JBL)) .EQ. IETB(JEAJ))THEN
                                JEBJ = JEB(JBL)
                             ENDIF
                          ENDIF
                          IF(IETB(JEBJ) .EQ. IETB(JEAJ))THEN
*                         The peak edges need joining.
*                            Join the Peak Edges.
                             CALL GKTZJP(JEBJ,JEAJ,JEAO,JEA(0),
     :                                   NV,VXETT,IYETT,IETB,JETNXT)
                             FINAL = .FALSE.
                             GOTO 130
                          ELSE
*                         Swap edges, if necessary
                            IBOT=IETB(JEB(JBR))
                            CALL GKTZSW(JEB(JBR),JEA(JAL),
     :                               NV,VX,VY,VXETT,IYETT,IETB,JETNXT)
                            FINAL = (IBOT .EQ. IETB(JEB(JBR)))
                          ENDIF
                        ELSE
*                          Here - The two peak edges Overlap
                           IF(XPKA(JAL) .LT. XPKB(JBL)) THEN
                             IF(XPKA(JAR) .LT. XPKB(JBR))THEN
*                            We have (LeftA-LeftB),(RightA-RightB)
                                CALL GKTZSW(JEA(JAR),JEB(JBL),
     :                                NV,VX,VY,VXETT,IYETT,IETB,JETNXT)
                             ELSE
*                            We have (LeftA-LeftB),(RightB-RightA)
                                CALL GKTZSW(JEA(JAR),JEB(JBL),
     :                             NV,VX,VY,VXETT,IYETT,IETB,JETNXT)
                             ENDIF
                           ELSE
                             IF(XPKB(JBR) .LT. XPKA(JAR))THEN
*                            We have (LeftB-LeftA),(RightB-RightA)
                                CALL GKTZSW(JEB(JBR),JEA(JAL),
     :                               NV,VX,VY,VXETT,IYETT,IETB,JETNXT)
                             ELSE
*                            We have (LeftB-LeftA),(RightA-RightB)
                                CALL GKTZSW(JEB(JBR),JEA(JAL),
     :                               NV,VX,VY,VXETT,IYETT,IETB,JETNXT)
                             ENDIF
                           ENDIF
                           FINAL = .FALSE.
                        ENDIF
                     ELSE
*                    2nd Peak edge lower
                        GOTO 120
                     ENDIF
                  ENDIF
*                 Escape, if error.
                  IF(KERROR .NE. 0)GOTO 999
  110          CONTINUE
            ENDIF
  120    CONTINUE
  121    CONTINUE
         IF(FINAL)GOTO 131
  130 CONTINUE
  131 CONTINUE

*     Remove top peak if both its edges are identical.
      DO 200 J0=1,NV/2
         JEA(1) = JETNXT(0)
         IF(JEA(1) .EQ. 0)GOTO 201
         JEA(2) = JETNXT(JEA(1))
         IF(JEA(2) .EQ. 0)GOTO 201
         IF(IETB(JEA(1)) .EQ. IETB(JEA(2)))THEN
            IF(ABS(VXETT(JEA(1)) - VXETT(JEA(2))) .LE. QTOL)THEN
               JETNXT(0) = JETNXT(JEA(2))
            ENDIF
         ENDIF
  200 CONTINUE
  201 CONTINUE

  999 CONTINUE

         END
