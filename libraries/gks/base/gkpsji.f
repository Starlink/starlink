C# IL>=a, OL>=0
      SUBROUTINE GKPSJI (NW,WSX,WSY,IWR,MS,IWS,NPOLY,MPOLY,ISP)
*
* (C) COPYRIGHT ICL & SERC  1989
*

*-------------------------------------------------------------------
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
*     Process a polygon section list so as to join intersecting
*     polygons at their intersections.
*
*  MAINTENANCE LOG
*  ---------------
*    23/02/89  KEVP  Stabilised
*    04/04/89  KEVP  Got do loops to execute for reverse sections
*    26/04/89  KEVP  Removed debug WRITE statements.
*    22/07/90  PLP   Commenting brought in line with standard format.
*    22/08/90  KEVP  Corrected stack error GOTO statements (Bug C23).
*
*
*  ARGUMENTS
*  ---------
*     INP  NW       Number of points in Vertex Workspace
*     I/O  WSX,WSY  Workspace, where the vertices are stored
*     I/O  IWR      Workspace Write Index
*     INP  MS       Maximum number of sections in section list
*     I/O  IWS      Section list
*     I/O  NPOLY    Number of polygons
*     INP  MPOLY    Maximum number of Polygons
*     I/O  ISP      List of sections, where each polygon begins

      INTEGER NW, IWR, MS,IWS(2,MS), MPOLY, NPOLY, ISP(MPOLY)
      REAL    WSX(NW),WSY(NW)
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     IP1    1st Polygon
*     IP2    2nd Polygon
*     IPMIN  Earliest polygon that needs processing
*
*     IW1    Current Vertex Index in 1st polygon
*     IWL1   Last Vertex Index in 1st polygon
*     IS1    Current Section in 1st polygon
*     IR1    Current Section in 1st polygon (Reversed=1, Foreward=0)
*     IW2    Current Vertex Index in 2nd polygon
*     IWL2   Last Vertex Index in 2nd polygon
*     IS2    Current Section in 2nd polygon
*     IR2    Current Section in 2nd polygon (Reversed=1, Foreward=0)
*     NS1    Number of vertices in first polygon
*     NS2    Number of vertices in second polygon
*     NSO    Number of vertices in joined polygon
*     NJN    Joining count (precaution against time consuming bug)
*     XL1,YL1  XL2,YL2  edges tested for crossing
*     XC,YC  Array containing crossing point
*     CROSS  True, if edges cross
*     COLLIN True, if edges cross and are collinear (not used)
*
*     ISFROM Old section index
*     ISTO   New section index
*     IPFROM Old polygon index
*     IPTO   New polygon index
*
*     IPJOIN   Stack pointer to joined polygon
*
      INTEGER IP1,IP2, IW1,IWL1, IS1,IR1, IW2,IWL2, IS2,IR2
      INTEGER NS1, NS2, NSO, NJN,  IPJOIN, IPMIN
      REAL    XL1(2),YL1(2), XL2(2),YL2(2), XC(2), YC(2)
      INTEGER ISFROM,ISTO, IPFROM,IPTO
      LOGICAL CROSS, COLLIN
*
*  STACK USAGE
*  -----------
*     1 integer allocation of length equal to twice the total number
*     of sections in two intersecting polygons plus 4 (pointer IPJOIN)
*     joined polygon.
*
*  ALGORITHM
*  ---------
*    Find two polygons that intersect and join them at an intersection.
*      The unjoined pair are deleted from the section list
*      in which all subsequent polygons are moved back to fill
*      the gaps. The joined polygon is then appended.
*    This process is repeated until all the intersections are gone.
*
*  ERRORS
*  ------
*    -2004 Polygon has a non-existaent section.
*
*  WARNING
*  -------
*   If IWS is not suffienty long, ie MS is not sufficiently large
*   this routine will write over the end no check is made in the
*   innermost loop.
*   The number of joins required, should first be counted by calling
*   GKPSNI, then MS made at least the original number of sections
*   plus 4 times the number of joins needed.
*
*-----------------------------------------------------------------------

*     Quit if less than 2 polygons
      IF(NPOLY .LT. 2)GOTO 999
      IPJOIN = KNIL
      NJN    = 1
      IPMIN  = 1

*     Join two polygons and repeat till all are joined
   40 CONTINUE

*     Check start of earliest polygon
      IS1 = ISP(IPMIN)
      IF((IS1 .LT. 1) .OR. (IS1 .GT. MS))THEN
         CALL GKBUG (-2004,'GKPSJI')
         GOTO 999
      ENDIF

      DO 160 IP1 = IPMIN,NPOLY-1
*        Find last vertex of 1st polygon
         IS1 = ISP(IP1+1)-1
         IF((IS1 .LT. 1) .OR. (IS1 .GT. MS))THEN
            CALL GKBUG (-2004,'GKPSJI')
            GOTO 999
         ENDIF
         IR1 = 0
         IF(IWS(1,IS1) .GT. IWS(2,IS1)) IR1 = 1
         IWL1 = IWS(2,IS1)-1+IR1
         XL1(1) = WSX(IWL1)
         YL1(1) = WSY(IWL1)
         DO 150 IP2 = IP1+1,NPOLY
*           Find last vertex of 2nd polygon
            IS2 = ISP(IP2+1)-1
            IF((IS2 .LT. 1) .OR. (IS2 .GT. MS))THEN
               CALL GKBUG (-2004,'GKPSJI')
               GOTO 999
            ENDIF
            IR2 = 0
            IF(IWS(1,IS2) .GT. IWS(2,IS2)) IR2 = 1
            IWL2 = IWS(2,IS2)-1+IR2
            XL2(1) = WSX(IWL2)
            YL2(1) = WSY(IWL2)
            DO 140 IS1 = ISP(IP1),ISP(IP1+1)-1
*              Section of 1st polygon
               IR1 = 0
               IF(IWS(1,IS1) .GT. IWS(2,IS1)) IR1 = 1
               DO 130 IW1 = IWS(1,IS1)-IR1,IWS(2,IS1)-1+IR1,
     :                                            1-IR1-IR1
*                 Vertex of 1st polygon
                  XL1(2) = WSX(IW1)
                  YL1(2) = WSY(IW1)
                  DO 120 IS2 = ISP(IP2),ISP(IP2+1)-1
*                    Section of 2nd Polygon
                     IR2 = 0
                     IF (IWS(1,IS2) .GT. IWS(2,IS2)) IR2 = 1
                     DO 110 IW2 = IWS(1,IS2)-IR2,IWS(2,IS2)-1+IR2,
     :                                                  1-IR2-IR2
*                       Vertex of 2nd Polygon
                        XL2(2) = WSX(IW2)
                        YL2(2) = WSY(IW2)
*                       Test if edges cross
                        CALL GKQLNI(XL1,YL1,XL2,YL2,CROSS,COLLIN,
     :                              XC,YC)
                        IF(CROSS)THEN
*                         Quit if too many joins (causable by bug)
                          IF(NJN .GE. MPOLY)GOTO 900
*                         Get stack for joined polygon
                          NS1 = ISP(IP1+1) - ISP(IP1)
                          NS2 = ISP(IP2+1) - ISP(IP2)
                          NSO = NS1 + NS2 + 4
*                         Set IPMIN to last polygon in case of error
                          IPMIN = NPOLY
                          CALL GKSTAL(KINTGS,NSO*2,IPJOIN)
                          IF(KERROR .NE. 0)GOTO 999
*                         Join the polygons
                          CALL GKPSJN (IWR,
     :              IS1-ISP(IP1)+1,IW1,NS1,IWS(1,ISP(IP1)),
     :              IS2-ISP(IP2)+1,IW2,NS2,IWS(1,ISP(IP2)),
     :                                 NSO,KSTACK(IPJOIN))
                          IF(KERROR .NE. 0)GOTO 95
                          WSX(IWR) = XC(1)
                          WSY(IWR) = YC(1)
                          IWR = IWR + 1
*                         Delete original two polygons and
*                         move others to fill the gaps
                          ISTO = ISP(IP1)
                          DO 50 ISFROM = ISP(IP1+1),ISP(IP2)-1
                             IWS(1,ISTO) = IWS(1,ISFROM)
                             IWS(2,ISTO) = IWS(2,ISFROM)
                             ISTO = ISTO + 1
   50                     CONTINUE
                          DO 60 ISFROM = ISP(IP2+1),ISP(NPOLY+1)-1
                             IWS(1,ISTO) = IWS(1,ISFROM)
                             IWS(2,ISTO) = IWS(2,ISFROM)
                             ISTO = ISTO + 1
   60                     CONTINUE
                          IPTO = IP1
                          DO 70 IPFROM = IP1+1,IP2-1
                             ISP(IPTO) = ISP(IPFROM) - NS1
                             IPTO = IPTO + 1
   70                     CONTINUE
                          DO 80 IPFROM = IP2+1,NPOLY+1
                             ISP(IPTO) = ISP(IPFROM) - NS1 - NS2
                             IPTO = IPTO + 1
   80                     CONTINUE
*                         Append Joined polygon
                          DO 90 ISFROM = 1,NSO
                             IWS(1,ISTO) = KSTACK(IPJOIN+2*ISFROM-2)
                             IWS(2,ISTO) = KSTACK(IPJOIN+2*ISFROM-1)
                             ISTO = ISTO + 1
                             IF(ISTO .GT. MS)THEN
                                GOTO 900
                             ENDIF
   90                     CONTINUE
                          ISP(IPTO) = ISTO
                          NPOLY = NPOLY - 1
                          IPMIN = IP1
   95                     CALL GKSTDA(KINTGS,IPJOIN)
                          IF(KERROR .NE. 0)GOTO 999
                          IPJOIN = KNIL
                          NJN = NJN + 1
                          IF(IWR .GT. NW)GOTO 999
                          GOTO 40
                        ENDIF
                        XL2(1) = XL2(2)
                        YL2(1) = YL2(2)
  110                CONTINUE
  120             CONTINUE
                  XL1(1) = XL1(2)
                  YL1(1) = YL1(2)
  130          CONTINUE
  140       CONTINUE
  150    CONTINUE
  160 CONTINUE


  900 CALL GKSTDA (KINTGS,IPJOIN)
  999 CONTINUE
      END
