C# IL>=a, OL>=0
      SUBROUTINE GKPSCL(RECT,NCROSS,NW,WSX,WSY,IWR,MS,IWS,MP,ISP,NPO)
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
*     Produce a structure that can be made into a clipped polygon.
*
*     This structure consists of the polygon followed by the
*     clip induced edges and an array giving
*     how to splice the polygon and clip induced edges,
*     to form one or more clipped polygons.
*
*  MAINTENANCE LOG
*  ---------------
*     23/02/89  KEVP  Original version stabilised
*     26/04/89  KEVP  Revised names of utilities: GKFCPS to GKPSCL;
*                     GKFCXI to GKPSCI; GKFCXO to GKPSCO;
*                     GKFCPB to GKPSCB; GKLPFA to GKPPFA.
*     26/04/89  KEVP  Removed debug WRITE statements.
*     16/11/89  RMK   Removed unused local variables.
*     22/07/90  PLP   Removed debug WRITE statements and brought
*                     commenting in line with standard format.
*     02/08/90  KEVP  Corrected stack error GOTO statements (Bug C23)
*
*  ARGUMENTS
*  ---------
*     INP  RECT    Clipping rectangle (xmin,xmax,ymin,ymax)
*     INP  NCROSS  Number of times polygon crosses clipping rectangle
*     INP  NW      Length of workspace
*     I/O  WSX,WSY The workspace
*                  INP  The polygon
*                  OUT  The polygon + The clip induced edges
*     OUT  IWR     Write-Position in workspace
*     INP  MS      Maximum number of sections
*     I/O  IWS     The list of workspace sections forming the
*                  clipped polygon(s) - The Section List
*                  INP   IWS(1,1)  - start of polygon = 1
*                        IWS(2,1)  - end of polygon = NP
*                  OUT   IWS(1,N)  - start of section N
*                        IWS(2,N)  - end of section N
*     INP  MP      The maximum number of polygons required
*     OUT  ISP     The position in IWS of the first section of each
*                  polygon (KNIL end of polygon list passed)
*     OUT  NPO     Number of polygons output (0 if polygon rejected)
*
*     Note: NW must be at least NP+NCROSS+4.
*
      INTEGER NW, IWR, MS, MP, IWS(2,MS),
     :        NCROSS, ISP(MP+1), NPO
      REAL    RECT(4), WSX(NW),WSY(NW)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     Stack pointers for integer stack
*     JINDEX  Crossing indices (where on polygon crossings occur)
*     JCSTAT  Clip status of each crossing
*     JORDER  Order of crossings on clip rectangle
*             (anticlockwise from upper right-hand corner)
*     JPLACE  Ordinal placings of crossings on clip rectangle
*
*     Stack pointers for real stack
*     JCOORD  Significant coord of each crossing point
*
      INTEGER JINDEX, JCSTAT, JORDER, JPLACE, JCOORD

*     ICLPS1  Clip status of first vertex in polygon
*     ICLPSA  Clip status of first point in pair
*     ICLPSB  Clip status of second point in pair
*     ICLIPS  Clip status of current crossing
*     ICLABS  Absolute value of ICLIPS
*     ICROSS  Crossing identifier = its stack offset {0:NCROSS-1}
*     IECL    1 if clip induced section is anti-clockwise (-1 clockwise)
*     IELS    End of last section
*     IEDGE   Edge of clipping rectangle (1=Base,2=Left,3=Right,4=Top)
*     INEDGE  Edge of clipping rectangle for next crossing
*     INSIDE  True, if inside polygon
*     IPA,IPB Indices in polygon (1:NP)
*     IPAIR   Do loop index for pairs of crossings
*     IPASS   Indicates which side of corner crossing is (-1 or 1)
*     IODIR   (IODIR .EQ. 1) true, if first crossing is outwards
*     IPDIR   (IPDIR .EQ. 1) true, if upper right corner is inside
*     IPLACE  Place of crossing in ordering around clip rectangle
*     IPOLST  Start in KSTACK(JINDEX) of current polygon
*     IS      Index in the section list IWS
*     IWA     Workspace Index of first vertex of section
*     IWB     Workspace Index of first vertex of section
*     NP      Number of vertices in initial polygon
*     NPOLY   Number of polygons
*     OUTW    True, if outward crossing
*     TX,TY   Point on top edge selected for determining insideness
*     TXA,TXB X-coords sometimes used to determine TX.
*     NTOP    Number of crossing of polygon over the top of the rectangle
*     NTOFF   Number of crossings between the upper right corner
*             and (TX,TY).
*     XA,YA   First point in pair
*     XB,YB   Second point in pair
*     XC,YC   The relevant corner
*     XAC,YAC The corner relative to first point
*     XCB,YCB The second point relative to corner
*
      INTEGER  ICROSS, ICLPS1, ICLIPS, ICLABS
      INTEGER  IPA,IPB, NP, IPASS, IEDGE, NTOP, INEDGE
      INTEGER  NTOFF, NPOLY, IPLACE, IWA,IWB
      INTEGER  IODIR, IPDIR, IS, IELS, IECL, IPAIR, IPOLST
      REAL     XA,YA, XB,YB, XC,YC, XAC,YAC, XCB,YCB
      REAL     TX,TY, TXA, TXB
      LOGICAL  OUTW, INSIDE

*
*  STACK USAGE
*  -----------
*     Integer stack for
*       Index of crossings in polygon
*       Clip status of each crossing
*       Order of crossings around clipping rectangle
*       Ordinal placing of each crossing around clipping rectangle
*     1 allocation length 4*NCROSS.
*
*     Real stack for
*       Crossing points (significant coord):
*     1 allocation length NCROSS.
*
*  ERRORS
*  ------
*     -2004  On Input IWS(1,1) not equal to 1 or
*            IWS(2,1) the number of vertices is either less than 3
*            or greater than NW - NCROSS - 4
*
*
*  ALGORITHM
*  ---------
*     Calculate all the crossing points
*
*     Determine their order around the clipping rectangle
*     and their ordinal placing around the clipping rectangle
*
*     Find a point on the top edge of the clipping rectangle
*     that is not a crossing point and determine,
*     whether it is inside the polygon or not.
*
*     Then work out how the crossing points, pair
*     to form the ends of the clip induced edge sections.
*
*     Determine the order of the sections in
*     each clipped polygon.
*       A section is a clip induced edge section or
*       a maximal unclipped contiguous part of the polygon.
*     Each begins and ends at a crossing point,
*     but crossing points are only included for
*     clip induced edge sections.
*
*     As this is done the clip induced edge sections are put
*     into the workspace inclusive of the end (crossing) points
*
*  COMMENTS
*  --------
*     The CLIP STATUS of a point is
*     its relationship to the clip rectangle as below.
*
*                                                           *
*                  |                   |                    *
*                                                           *
*          9       |        10         |     11             *
*                                                           *
*      _ _ _ _ _ _ |___________________|_ _ _ _ _ _ _       *
*                  |                   |                    *
*                  |     CLIPPING      |                    *
*                  |                   |                    *
*         -1       |         0         |      1             *
*                  |                   |                    *
*                  |     RECTANGLE     |                    *
*      _ _ _ _ _ _ |___________________| _ _ _ _ _ _ _      *
*                                                           *
*                  |                   |                    *
*                                                           *
*        -11       |       -10         |     -9             *
*                                                           *
*                  |                   |                    *
*
*      The clip status of a crossing (of the clipping rectangle edge)
*      is intially the clip status of the outside point of the crossing
*      and is later the clip status of the area at the crossing
*      immediately outside the rectangle.
*
*  STATEMENT FUNCTION
*  -----------------
      INTEGER IDIR, N
      IDIR(N) = 1 - 2*(N-2*(N/2))
*     1 if N is even, -1 if odd. (ie, (-1)**N )
*------------------------------------------------------------------------

*     Check IWS and determine number of vertices in initial polygon
      NPO = 0
      IF(IWS(1,1) .NE. 1)THEN
         CALL GKBUG(-2004,'GKPSCL')
         GOTO 999
      ENDIF
      NP = IWS(2,1) - 1
      IF((NP .LT. 3) .OR. (NP+NCROSS+4 .GT. NW))THEN
         CALL GKBUG(-2004,'GKPSCL')
         GOTO 999
      ENDIF
*
*     Initialise
      ISP(1) = 1

*     Allocate Stack
      CALL GKSTAL (KINTGS,NCROSS*4,JINDEX)
      IF(KERROR .NE. 0)GOTO 999
      JCSTAT = JINDEX + NCROSS
      JORDER = JCSTAT + NCROSS
      JPLACE = JORDER + NCROSS
      CALL GKSTAL (KREALS,NCROSS,JCOORD)
      IF(KERROR .NE. 0)GOTO 998
*
***** Determine the index of each crossing *****
***** and its clip status                  *****
      CALL GKPSCI (RECT,NP,WSX,WSY,
     :             NCROSS,KSTACK(JINDEX),KSTACK(JCSTAT),ICLPS1)
*
***** Calculate the crossing points *****
*
      OUTW = (ICLPS1 .EQ. 0)
*     First crossing will be outward, if first point is inside
      DO 450 ICROSS=0,NCROSS-1
         IPA = KSTACK(JINDEX+ICROSS) - 1
         IF(IPA .EQ. 0)IPA = NP
         IPB = KSTACK(JINDEX+ICROSS)
         XA = WSX(IPA)
         YA = WSY(IPA)
         XB = WSX(IPB)
         YB = WSY(IPB)
         ICLIPS = KSTACK(JCSTAT+ICROSS)
         ICLABS = IABS(ICLIPS)
         IF((ICLABS .GT. 1).AND.(ICLABS .NE. 10))THEN
*        Need to determine which edge of rectangle crossing occurs
*        This is done by determining which side of the
*        appropiate corner it passes.
*
            IF(ICLIPS .EQ. -11)THEN
*             lower left-hand corner
              XC = RECT(1)
              YC = RECT(3)
            ELSEIF(ICLIPS .EQ. -9)THEN
*             lower right-hand corner
              XC = RECT(2)
              YC = RECT(3)
            ELSEIF(ICLIPS .EQ. 11)THEN
*             upper right-hand corner
              XC = RECT(2)
              YC = RECT(4)
            ELSE
*             upper left-hand corner
              XC = RECT(1)
              YC = RECT(4)
            ENDIF
*           does the polygon edge pass to the left or right (travelling
*           outwards) of the corner.
*           Cross product used
            XAC = XC-XA
            YAC = YC-YA
            XCB = XB-XC
            YCB = YB-YC
            IF(XAC*YCB - XCB*YAC .GT. 0)THEN
*             passes to the left
              IPASS = 1
            ELSEIF(XAC*YCB - XCB*YAC .LT. 0)THEN
*             passes to the right
              IPASS = -1
            ELSE
*             passes through
              IPASS = 1
*             deemed to pass through vertical edge.
              GOTO 410
            ENDIF
*           Switch IPASS if inwards
            IF(.NOT. OUTW) IPASS = -IPASS
*           Switch IPASS if upper right or opposite corner
            IF(ICLABS .EQ. 9)IPASS = -IPASS
  410       CONTINUE
*
*           Now IPASS is -1 for LEFT or RIGHT edge
*                        +1 for TOP  or BASE.
*
*           Now we know which edge has the crossing
            IF(IPASS .EQ. -1)THEN
               ICLIPS = ICLIPS - 10*NINT(FLOAT(ICLIPS)/10.0)
            ELSE
               ICLIPS = 10*NINT(FLOAT(ICLIPS)/10.0)
            ENDIF
            KSTACK(JCSTAT+ICROSS) = ICLIPS
         ENDIF
*
*        Now we know which edge is crossed,
*            the crossing points can be obtained
         IEDGE = (ICLIPS+18)/6
*        This gives IEDGE; 1=Base, 2=Left, 3=Right & 4=Top.
         IF(IEDGE .EQ. 1)THEN
*          must cross base
           QSTACK(JCOORD+ICROSS) =
     :           ((RECT(3)-YA)*XB + (YB-RECT(3))*XA)/(YB-YA)
         ELSEIF(IEDGE .EQ. 2)THEN
*          must cross left-hand side
           QSTACK(JCOORD+ICROSS) =
     :           ((RECT(1)-XA)*YB + (XB-RECT(1))*YA)/(XB-XA)
         ELSEIF(IEDGE .EQ. 3)THEN
*          must cross right-hand side
           QSTACK(JCOORD+ICROSS) =
     :           ((RECT(2)-XA)*YB + (XB-RECT(2))*YA)/(XB-XA)
         ELSEIF(IEDGE .EQ. 4)THEN
*          must cross top
           QSTACK(JCOORD+ICROSS) =
     :           ((RECT(4)-YA)*XB + (YB-RECT(4))*XA)/(YB-YA)
         ELSE
           QSTACK(JCOORD+ICROSS) = 240.0
         ENDIF

*        Next crossing is out if present one is in and vice versa
         OUTW = .NOT. OUTW
  450 CONTINUE

*
***** Find order of crossing points around the clip rectangle *****
*
      CALL GKPSCO(NCROSS,KSTACK(JCSTAT),QSTACK(JCOORD),
     :                   KSTACK(JORDER),KSTACK(JPLACE),NTOP)
*
*     Select a point on the top edge of the rectangle
*     and determine whether it is inside the polygon or not.
*
      TY = RECT(4)
      NTOFF = 0
      IF(NTOP .EQ. 0)THEN
*       If no top crossings select centre of top edge.
        TX = (RECT(1)+RECT(2))/2.0
      ELSE
        TXB = RECT(2)
*  ---  Loop in case crossing points are too close ---
  610   CONTINUE
        TXA = QSTACK(JCOORD+KSTACK(JORDER+NTOFF)-1)
  611   CONTINUE
*       If the two points are not too close,
        IF(TXB-TXA .GT. QTOL*(RECT(2)-RECT(1)))THEN
*          select the point halfway between.
           TX = (TXA+TXB)/2.0
        ELSE
*          Move onto next crossing
           NTOFF = NTOFF + 1
           TXB = TXA
           IF(NTOFF .LT. NTOP)GOTO 610


           TXA = RECT(1)
           IF(NTOFF .LE. NTOP)GOTO 611
* ---      End of loop  ---
*          If here, either error or the number of crossings
*          exceeded 1/QTOL, which is unrealistically high.
           GOTO 900
        ENDIF
      ENDIF
      CALL GKPPFA (TX,TY,NP,WSX,WSY,INSIDE)
      IPDIR = 0
      IF(INSIDE)IPDIR = 1
*     work back to upper right-hand corner
*     (only parity matters, therefore add rather than subtract)
      IPDIR = IPDIR+NTOFF
      IPDIR = IPDIR - 2*(IPDIR/2)
*     IPDIR is 1 if and only if, the
*     upper right-hand corner of the rectangle is inside the polygon
*     ie every even crossing (in clip rectangle order) joins
*     to the next one down.
*

*     Order the sections in IWS adding the clip induced
*     edges onto the end of the workspace
      IODIR = 0
      OUTW = (ICLPS1 .EQ. 0)
      IF(OUTW)IODIR = 1
      ICROSS = 0
      CALL GKPSCB(NCROSS,KSTACK(JINDEX),KSTACK(JORDER),KSTACK(JPLACE),
     :            NP,IODIR,IPDIR,ICROSS)
      NPOLY  = 0
      IELS = KNIL
      IS = 1
      IWS(1,IS) = KSTACK(JINDEX+ICROSS)
      IWR   = NP+1
*     Loop over pairs of crossings
      DO 750 IPAIR = 1,NCROSS,2
*
*        +++ Section of polygon inside clipping rectangle +++
*
         IWA = KSTACK(JINDEX+ICROSS)
         KSTACK(JINDEX+ICROSS) = KNIL
         ICROSS = ICROSS + IDIR(ICROSS+IODIR)
         IF(ICROSS .EQ. NCROSS)THEN
*          end of crossings list reached
           ICROSS = 0
           IF(IWA .NE. NP+1)THEN
*          If section is of non-zero length
              IF(IS .GT. MS) GOTO 757
              IWS(1,IS) = IWA
              IWS(2,IS) = NP+1
              IS = IS+1
           ENDIF
           IWA = 1
         ELSEIF(ICROSS .EQ. -1)THEN
*          start of crossings list reached
           ICROSS = NCROSS - 1
           IF(IWA .NE. 1)THEN
*          If section is of non-zero length
              IF(IS .GT. MS) GOTO 757
              IWS(1,IS) = IWA
              IWS(2,IS) = 1
              IS = IS+1
           ENDIF
           IWA = NP+1
         ENDIF
         IWB = KSTACK(JINDEX+ICROSS)
         KSTACK(JINDEX+ICROSS) = KNIL
         IF(IWA .NE. IWB)THEN
*        If section is of non-zero length
            IF(IS .GT. MS) GOTO 757
            IWS(1,IS) = IWA
            IWS(2,IS) = IWB
            IS = IS+1
*           Put in start of next clip induced edge section
            IF(IS .GT. MS) GOTO 717
            IWS(1,IS) = IWR
         ELSEIF(IWR .EQ. IELS)THEN
*        Else preceding section can be extended (if it exists)
            IS = IS - 1
         ELSE
            IF(IS .GT. MS)GOTO 717
*           Put in start of next clip induced edge section
            IWS(1,IS) = IWR
         ENDIF
*
*  +++   Clip-induced Edge section +++

*        Put crossing point into workspace.
         IEDGE = (KSTACK(JCSTAT+ICROSS)+18)/6
         WSX(IWR) = QSTACK(JCOORD+ICROSS)
         WSY(IWR) = QSTACK(JCOORD+ICROSS)
         IF(IEDGE .EQ. 1)THEN
            WSY(IWR) = RECT(3)
         ELSEIF(IEDGE .EQ. 2)THEN
            WSX(IWR) = RECT(1)
         ELSEIF(IEDGE .EQ. 3)THEN
            WSX(IWR) = RECT(2)
         ELSE
            WSY(IWR) = RECT(4)
         ENDIF
         IWR = IWR + 1
*        Find crossing at other end of section
         IPLACE = KSTACK(JPLACE+ICROSS)
         IPLACE = IPLACE - IDIR(IPLACE+IPDIR)
         IF(IPLACE .LT. 1)THEN
            IPLACE = NCROSS
         ELSEIF(IPLACE .GT. NCROSS)THEN
            IPLACE = 1
         ENDIF
         ICROSS = KSTACK(JORDER+IPLACE-1) - 1
*        Is it round a corner?
         INEDGE = (KSTACK(JCSTAT+ICROSS)+18)/6
         IF(INEDGE .NE. IEDGE)THEN
*           Put in corner(s)
            IF(IDIR(IPLACE+IPDIR) .EQ.  1)THEN
*             Anticlockwise section
              IECL = 0
            ELSE
*             Clockwise section
              IECL = 1
            ENDIF

            GOTO (711,714,712,713, 711,712,714,713) IEDGE + 4*IECL
            GOTO 900
  711       CONTINUE
*             Lower Right/Left
              WSX(IWR) = RECT(2-IECL)
              WSY(IWR) = RECT(3)
              IWR = IWR + 1
            IF(INEDGE .EQ. 3-IECL)GOTO 715
  712       CONTINUE
*             Upper Right/Left
              WSX(IWR) = RECT(2-IECL)
              WSY(IWR) = RECT(4)
              IWR = IWR + 1
            IF(INEDGE .EQ. 4)GOTO 715
  713       CONTINUE
*             Upper Left/Right
              WSX(IWR) = RECT(1+IECL)
              WSY(IWR) = RECT(4)
              IWR = IWR + 1
            IF(INEDGE .EQ. 2+IECL)GOTO 715
  714       CONTINUE
*             Lower Left/Right
              WSX(IWR) = RECT(1+IECL)
              WSY(IWR) = RECT(3)
              IWR = IWR + 1
            IF(INEDGE .NE. 1)GOTO 711
  715       CONTINUE
         ENDIF
         IEDGE = INEDGE
*        Put the other crossing point in
         WSX(IWR) = QSTACK(JCOORD+ICROSS)
         WSY(IWR) = QSTACK(JCOORD+ICROSS)
         IF(IEDGE .EQ. 1)THEN
            WSY(IWR) = RECT(3)
         ELSEIF(IEDGE .EQ. 2)THEN
            WSX(IWR) = RECT(1)
         ELSEIF(IEDGE .EQ. 3)THEN
            WSX(IWR) = RECT(2)
         ELSE
            WSY(IWR) = RECT(4)
         ENDIF
         IWR = IWR + 1
*        Put end of section workspace-index into IWS
         IF(IS .GT. MS)GOTO 757
         IWS(2,IS) = IWR
         IELS = IWR
*        +++ end of section +++
         IS = IS + 1
*
  717    CONTINUE
         IF(KSTACK(JINDEX+ICROSS) .EQ. KNIL)THEN
*          Polygon finished
            IPOLST = ICROSS
*           Put polygon data in
            NPOLY=NPOLY+1
            ISP(NPOLY+1) = IS
*           If there are more possible vertices
            IF(NPOLY .GT. MP) THEN
               GOTO 760
            ENDIF
            IF(IPAIR + 1 .LT. NCROSS)THEN
*             find another polygon
              DO 720 ICROSS = 0,NCROSS-1
                 IF(KSTACK(JINDEX+ICROSS) .NE. KNIL)GOTO 730
  720         CONTINUE
              GOTO 757
            ELSE
              GOTO 757
            ENDIF
  730       CONTINUE
*           Another Polygon is found
            IELS = KNIL
            CALL GKPSCB(NCROSS,KSTACK(JINDEX),KSTACK(JORDER),
     :                  KSTACK(JPLACE),NP,IODIR,IPDIR,ICROSS)
            IF(ICROSS .EQ. KNIL)GOTO 760
         ENDIF
  750 CONTINUE
*     End of Do loop over pairs of crossings

  757 CONTINUE
      IF(NPOLY .LE. MP) NPO = NPOLY
*
  760 CONTINUE
*
***** Release Stack *****
*
  900 CONTINUE

      CALL GKSTDA (KREALS,JCOORD)
  998 CALL GKSTDA (KREALS,JINDEX)

  999 CONTINUE
      END
