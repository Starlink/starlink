C# IL>=a, OL>=0
      SUBROUTINE GKEPBB(ITYPE, NID,IDAT, NRD,RX,RY,
     :                  RMARG, LNSUB,XFSUB)
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
*     Draws a bounding box around the given primitive.
*     May be used for pick echoplay, especially for
*     monochrome workstations.
*     Line attributes for box must bw set in advance.
*
*  MAINTENANCE LOG
*  ---------------
*     20/06/89  KEVP  Original version stabilised
*     22/06/89  KEVP  Improved GDP by calling GKEPBC.
*     22/06/89  KEVP  Clipped box with GKFILH rather than in GKLCLP,
*                     removing inapplicable arguments.
*     06/07/90  KEVP  Suspended echoplay of string and character
*                     precision text as an emergency fix of bug S381.
*     06/07/90  KEVP  Removed debug WRITE statements.
*     25/03/91  KEVP  Reinstated echoplay for string and character
*                     precision text, using the character base vector
*                     instead of the character width vector (C20).
*
*  ARGUMENTS
*  ---------
*     INP ITYPE  Primitive Type (KPL,KPM,KTX,KFA,KCA,KGDP)
*     INP NID    Number of integers in IDAT
*     INP IDAT   Integer data
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates (WC)
*     INP RMARG  Margin (NDC units)
*     INP LNSUB  Device polyline output routine
*     INP XFSUB  Device text font details routine
*
      INTEGER ITYPE, NRD, NID, IDAT(NID)
      REAL    RX(NRD),RY(NRD), RMARG
      EXTERNAL  LNSUB, XFSUB
*
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     BOXX,BOXY            Box to be drawn
*     PX,PY                Polygon with NP Vertices (for GDP)
*     RECT                 Clipping Rectangle (xmin,xmax,ymin,ymax)
*     NW                   Size of Workspace
*     WSX,WSY              Workspace for Clipping
*     XMIN,XMAX,YMIN,YMAX  Box in min/max form
*     XMARG,YMARG          Margin in WC
*     UP                   Absolute value of character up-vector
*     NPOLY                Number of polygons after clipping
*     IPOLY                Limits of Polygons after clipping
*     NEED                 Quantity of workspace needed
*
      INTEGER I, NP, NW, IPOLY(2), NPOLY, NEED
      PARAMETER (NW = 12)
      REAL BOXX(5),BOXY(5), XMIN,XMAX,YMIN,YMAX, UP, XMARG,YMARG
      REAL PX(4),PY(4), RECT(4), WSX(NW),WSY(NW)
*-------------------------------------------------------------

      GOTO (120,120,140,120,160,170) ITYPE - KPL + 1
      GOTO 9999
*
*     ----------------------------------
*     polyline, polymarker and fill-area
*     ----------------------------------
  120 CONTINUE
      IF(NRD .LT. 1)GOTO 9999
*     Derive box in WC
      XMAX = RX(1)
      XMIN = RX(1)
      YMAX = RY(1)
      YMIN = RY(1)
      DO 125 I=2,NRD
         IF(XMAX .LT. RX(I))XMAX = RX(I)
         IF(XMIN .GT. RX(I))XMIN = RX(I)
         IF(YMAX .LT. RY(I))YMAX = RY(I)
         IF(YMIN .GT. RY(I))YMIN = RY(I)
  125 CONTINUE
      GOTO 7777

*     ----
*     text
*     ----
  140 CONTINUE
      IF(NID .LT. 1)GOTO 9999
*     Data in WCA
*     QWR1,QWR2  Text position
*     calculate height and width vectors
*     QWR3:wd(x), QWR4:wd(y), QWR5:ht(x), QWR6:ht(y)
      UP = SQRT(QCCHUX*QCCHUX + QCCHUY*QCCHUY)
      QWR3 = (QCCHUY/UP)*QCCHH
      QWR5 = (QCCHUX/UP)*QCCHH
      QWR4 = -QWR5
      QWR6 = QWR3
*     QWCHRX(KWKIX),QWCHRY(KWKIX) : baseline vector
*
*     stroke precision
      IF (KWTXPR(KWKIX) .EQ. GSTRKP) THEN
        CALL GKXQXO(NID,IDAT,BOXX,BOXY)
*     string and char precision
      ELSE
*       baseline vector from ws Set text attributes entry
        CALL GKXQXC (NID,IDAT,QWCHRX(KWKIX),QWCHRY(KWKIX),
     :                  BOXX,BOXY,XFSUB)

      ENDIF
      IF(KERROR .NE. 0) GOTO 9999
      GOTO 8888
*
*     ----------
*     cell array
*     ----------
  160 CONTINUE
*     Data received and used:
*     NID    : Number of cells
*     QWR1   : X coordinate of point P
*     QWR2   : Y coordinate of point P
*     QWR3   : X coordinate of point Q
*     QWR4   : Y coordinate of point Q
*     QWR5   : X coordinate of point R
*     QWR6   : Y coordinate of point R
      IF(NID .LT. 1)GOTO 9999

*     Define bounding box
      XMAX = QWR1
      XMIN = QWR1
      YMAX = QWR2
      YMIN = QWR2
      IF(XMAX .LT. QWR3)XMAX = QWR3
      IF(XMIN .GT. QWR3)XMIN = QWR3
      IF(YMAX .LT. QWR4)YMAX = QWR4
      IF(YMIN .GT. QWR4)YMIN = QWR4
      IF(XMAX .LT. QWR5)XMAX = QWR5
      IF(XMIN .GT. QWR5)XMIN = QWR5
      IF(YMAX .LT. QWR6)YMAX = QWR6
      IF(YMIN .GT. QWR6)YMIN = QWR6
      GOTO 7777

*     ---
*     GDP
*     ---
  170 CONTINUE
      CALL GKEPBC (KWI1,NRD,RX,RY,NP,PX,PY)
      IF(NP .LT. 1) GOTO 9999
      XMAX = PX(1)
      XMIN = PX(1)
      YMAX = PY(1)
      YMIN = PY(1)
      DO 175 I=2,NP
         IF(XMAX .LT. PX(I))XMAX = PX(I)
         IF(XMIN .GT. PX(I))XMIN = PX(I)
         IF(YMAX .LT. PY(I))YMAX = PY(I)
         IF(YMIN .GT. PY(I))YMIN = PY(I)
  175 CONTINUE
      GOTO 7777

 7777 CONTINUE
*     Add Margin
      CALL GKTNWV (RMARG,RMARG,XMARG,YMARG)
      BOXX(1) = XMIN - XMARG
      BOXX(2) = XMAX + XMARG
      BOXX(3) = XMAX + XMARG
      BOXX(4) = XMIN - XMARG
      BOXY(1) = YMIN - YMARG
      BOXY(2) = YMIN - YMARG
      BOXY(3) = YMAX + YMARG
      BOXY(4) = YMAX + YMARG

 8888 CONTINUE
*     Convert Coords
      CALL GKTWD (4,BOXX,BOXY,WSX,WSY)
*     Clip
      RECT(1) = QWCLXL(KWKIX)
      RECT(2) = QWCLXR(KWKIX)
      RECT(3) = QWCLYB(KWKIX)
      RECT(4) = QWCLYT(KWKIX)
      CALL GKFHCL(RECT,4,NW,WSX,WSY,1,IPOLY,NPOLY,NEED)
      IF(NEED .GT. NW)GOTO 9999
      IF(NPOLY .GT. 1)GOTO 9999
*     Close polygon
      WSX(IPOLY(2)) = WSX(1)
      WSY(IPOLY(2)) = WSY(1)
*     Draw
      CALL LNSUB(IPOLY(2),WSX,WSY)

 9999 CONTINUE
      END
