C# IL>=a, OL>=0
      SUBROUTINE GKPSNI (NW,WSX,WSY,NS,IWS,NPOLY,ISP,NMAX,NIN)
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
*     Count the number of polygons that intersect other polygons
*     in a polygon set, yet to be spliced.
*
*  MAINTENANCE LOG
*  ---------------
*     23/02/89  KEVP Stabilised
*     04/04/89  KEVP Got do loops to execute for reverse sections
*     25/04/89  KEVP Corrected search algorithm of polygon pairs
*     26/04/89  KEVP Removed debug WRITE statements.
*     22/07/90  PLP   Commenting brought in line with standard format.
*
*  ARGUMENTS
*  ---------
*     INP  NW       Number of points in Vertex Workspace
*     INP  WSX,WSY  Workspace, where the vertices are stored
*     INP  NS       Number of sections to be spliced
*     INP  IWS      The section list
*     INP  NPOLY    Number of polygons
*     INP  ISP      List of sections, where each polygon begins
*     INP  NMAX     Maximum value of count before abandoning (if +ve)
*     OUT  NIN      Number of polygons that intersect with at least
*                   one other polygon
*
      INTEGER  NW, NS, IWS(2,NS), NPOLY, ISP(NPOLY+1), NMAX, NIN
      REAL     WSX(NW),WSY(NW)
*
*  COMMON BLOCK USAGE
*  ------------------
*     None
*
*  LOCALS
*  ------
*     IP1    1st Polygon
*     IP2    2nd Polygon
*
*     IW1    Current Vertex Index in 1st polygon
*     IWL1   Last Vertex Index in 1st polygon
*     IS1    Current Section in 1st polygon
*     IR1    Current Section in 1st polygon (Reversed=1, Foreward=0)
*     IW2    Current Vertex Index in 2nd polygon
*     IWL2   Last Vertex Index in 2nd polygon
*     IS2    Current Section in 2nd polygon
*     IR2    Current Section in 2nd polygon (Reversed=1, Foreward=0)
*
*     CROSS  True, if edges cross
*     COLLIN True, if edges cross and are collinear (not used)
*
      INTEGER IP1,IS1,IW1,IWL1,IR1, IP2,IS2,IW2,IWL2,IR2, ICOUNT
      REAL    XL1(2),YL1(2), XL2(2),YL2(2), XC(2),YC(2)
      LOGICAL CROSS, COLLIN

*  ERRORS
*  ------
*    -2004 Polygon has a non-existent section.
*
*  COMMENT
*  -------
*    Only polygon edge crossings are counted,
*    not polygons that are entirely inside another polygon.
*--------------------------------------------------------------------
*
*     Initialise count
      NIN = 0
*     Quit if less than 2 polygons
      IF(NPOLY .LT. 2)GOTO 999

*     Check start of first polygon
      IS1 = ISP(1)
      IF((IS1 .LT. 1) .OR. (IS1 .GT. NS))THEN
         CALL GKBUG (-2004,'GKPSNI')
         GOTO 999
      ENDIF

      ICOUNT = 0
      DO 60 IP1 = 1,NPOLY-1
*        Find last vertex of 1st polygon
         IS1 = ISP(IP1+1)-1
         IF((IS1 .LT. 1) .OR. (IS1 .GT. NS))THEN
            CALL GKBUG (-2004,'GKPSNI')
            GOTO 999
         ENDIF
         IR1 = 0
         IF(IWS(1,IS1) .GT. IWS(2,IS1)) IR1 = 1
         IWL1 = IWS(2,IS1)-1+IR1
         XL1(1) = WSX(IWL1)
         YL1(1) = WSY(IWL1)
         DO 50 IP2 = IP1+1,NPOLY
*           Find last vertex of 2nd polygon
            IS2 = ISP(IP2+1)-1
            IF((IS2 .LT. 1) .OR. (IS2 .GT. NS))THEN
               CALL GKBUG (-2004,'GKPSNI')
               GOTO 999
            ENDIF
            IR2 = 0
            IF(IWS(1,IS2) .GT. IWS(2,IS2)) IR2 = 1
            IWL2 = IWS(2,IS2)-1+IR2
            XL2(1) = WSX(IWL2)
            YL2(1) = WSY(IWL2)
            DO 40 IS1 = ISP(IP1),ISP(IP1+1)-1
*              Section of 1st polygon
               IR1 = 0
               IF(IWS(1,IS1) .GT. IWS(2,IS1)) IR1 = 1
               DO 30 IW1 = IWS(1,IS1)-IR1,IWS(2,IS1)-1+IR1,
     :                                           1-IR1-IR1
*                 Vertex of 1st polygon
                  XL1(2) = WSX(IW1)
                  YL1(2) = WSY(IW1)
                  DO 20 IS2 = ISP(IP2),ISP(IP2+1)-1
*                    Section of 2nd Polygon
                     IR2 = 0
                     IF (IWS(1,IS2) .GT. IWS(2,IS2)) IR2 = 1
                     DO 10 IW2 = IWS(1,IS2)-IR2,IWS(2,IS2)-1+IR2,
     :                                                 1-IR2-IR2
*                       Vertex of 2nd Polygon
                        XL2(2) = WSX(IW2)
                        YL2(2) = WSY(IW2)
*                       Test if edges cross
                        CALL GKQLNI(XL1,YL1,XL2,YL2,CROSS,COLLIN,
     :                              XC,YC)
                        IF(CROSS)THEN
*                          Increment count and 1st polygon
                           NIN = NIN + 1
*                          Quit if Max reached (Only +ve max counts)
                           IF(NMAX .GT. 0)THEN
                             IF(NIN .GE. NMAX) GOTO 999
                           ENDIF
                           GOTO 50
                        ENDIF
                        XL2(1) = XL2(2)
                        YL2(1) = YL2(2)
*                       Prevent too many runs at loop
                        ICOUNT = ICOUNT + 1
                        IF(ICOUNT .GT. 1000000)THEN
                           CALL GKBUG (-1016,'GKPSNI')
                           GOTO 999
                        ENDIF
   10                CONTINUE
   20             CONTINUE
                  XL1(1) = XL1(2)
                  YL1(1) = YL1(2)
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
   60 CONTINUE

  999 CONTINUE
      END
