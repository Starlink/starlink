C# IL>=a, OL>=0
      SUBROUTINE GKFHCL(RECT,NP,NW,WSX,WSY,MPOLY,IPOLY,NPOLY,NEED)
*
* (C) COPYRIGHT ICL & SERC  1989
*

*----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             KEVP
*
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Clip a fill area polygon
*
*  MAINTENANCE LOG
*  ---------------
*     23/02/89  KEVP Original Version Stabilised
*     03/04/89  KEVP Eliminated unnecessary expansion of stack-space for
*                    additional sections arising from joining of
*                    overlapping polygons by GKPSJI.
*     26/04/89  KEVP Revised names of utilities: GKFACL to GKFHCL;
*                    GKFACS to GKPSFH; GKFCPS to GKPSCL.
*     27/04/89  KEVP Changed name of GKLPFA to GKPPFA.
*     22/05/89  KEVP Tidied up comments, removed debug WRITE statements.
*     23/05/89  KEVP Made more effective and efficient use of stackspace
*                    of section list for non-hollow filstyles.
*     08/06/89  KEVP Corrected formulation of IPOLY after GKPSFH.
*     12/06/89  KEVP Tidied up estimation of number of polygons needed.
*     22/07/90  PLP  Commenting brought in line with standard format;
*                    also removed unused local IS and the debug WRITE
*                    statements.
*     22/08/90  KEVP Corrected stack error GOTO statement. (Bug C23).
*     12/11/91  KEVP Stopped counting of polygons of two or less
*                    vertices (C69).
*
*  ARGUMENTS
*  ---------
*     INP  RECT    Clipping rectangle (xmin,xmax,ymin,ymax)
*     INP  NP      Number of vertices in the polygon
*     INP  NW      Number of points in the workspace (min NP+1)
*     I/O  WSX,WSY The workspace
*                  INP The polygon (1:NP)
*                  OUT The clipped polygon(s) (IPOLY(n):IPOLY(n+1))
*     INP  MPOLY   The maximum number of polygons to be output
*     OUT  IPOLY   The position of polygons the the workspace
*     OUT  NPOLY   The actual number of polygons output
*     OUT  NEED    Amount of workspace needed if NW is not enough
*
      INTEGER NP,MPOLY,NW,NPOLY,IPOLY(MPOLY+1), NEED
      REAL    RECT(4), WSX(NW),WSY(NW)
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwkd.cmn'

*  LOCALS
*  ------
*     CROSS   True, if line crosses clipping rectangle
*     ICLPSA  Clip status of first point in pair
*     ICLPSB  Clip status of second point in pair
*     ICLD    Clip status difference
*     ICLS    Clip status sum
*     INSIDE  True, if inside polygon
*     IP      Index of polygon vertex
*     ISL     Stack pointer for section list
*     IWR     Workspace Write Index
*     JP      Polygon index
*     MPO     Maximum Number of polygons (may be less than MPOLY)
*     NCROSS  Number of crossings of clipping boundary by polygon
*     MSECT   Maximum number of sections expected
*     NSECT   Actual Number of sections
*     NSMIN   Minimum number of sections required for stack
*     NSWANT  Desired number of sections for stack
*     NPOLCR  Number of intersecting polygons
*
      INTEGER NCROSS, ISL, IP, MSECT, MPO, JP, IWR
      INTEGER ICLPSA, ICLPSB, ICLD, ICLS, NPOLCR, NSECT
      INTEGER NSMIN, NSWANT, ILOOP
      LOGICAL CROSS, INSIDE


*  STACK USAGE
*  -----------
*     Integer stack for
*       Section list for clipped polygon
*     1 allocation length at least 2*NSMIN (NSMIN <= NCROSS+1)
*     GKLUMP used to allocate stack if not HOLLOW.
*
*     GKPSCL uses stack space totalling 5*NCROSS
*
*     GKPSFH uses stack space equal to the total number of
*            vertices moved in the workspace.
*
*  ALGORITHM
*  ---------
*     Determine whether all the vertices are in the clipping rectangle.
*
*     If so,
*           output original polygon and quit.
*     Otherwise,
*           count number of crossings
*
*     If no crossings,
*           determine whether clip rectangle is inside polygon.
*           If inside,
*                     output clipping rectangle and quit.
*           otherwise,
*                     output no polygons and quit.
*     Otherwise,
*           check whether there is enough workspace.
*
*     If not,
*          quit, so that more workspace can be allocated
*     Otherwise,
*          put clip induced edges after polygon in workspace
*          and how workspace is to be spliced to produce
*          the clipped polygons.
*
*          Splice the workspace to produce the clipped polygon(s).
*
*          Output polygon parameters
*
*          Release stack
*
*  COMMENTS
*  --------
*     Clip status is as defined in the COMMENTS section
*     of GKPSCL
*
*     If there is not enough workspace to accomodate
*     the polygon, the crossings and the four corners
*     of the clipping rectangle,
*        the routine terminates outputing the required space in NEED
*        and setting NPOLY to half the number of crossings,
*        which is an upper bound to the number of polygons
*        after clipping.
*
*------------------------------------------------------------------------
*     Initialise Output Arguments
      NEED = NP + 1
      NPOLY = 1
      NSMIN = 1
      NCROSS = 0
      IPOLY(1) = 1


***** Is clipping necessary? *****
      DO 100 IP = 1,NP
        IF(WSX(IP) .LT. RECT(1))GOTO 110
        IF(WSX(IP) .GT. RECT(2))GOTO 110
        IF(WSY(IP) .LT. RECT(3))GOTO 110
        IF(WSY(IP) .GT. RECT(4))GOTO 110
  100 CONTINUE

*     Here - no clipping required - output polygon
      IPOLY(2) = 1 + NP
      GOTO 999
*
  110 CONTINUE
*
***** Count the crossings of the clip rectangle boundary  *****
***** by the polygon and the number of sections they define ***
*
*     Clip status of first point
      IF(WSX(1) .LT. RECT(1))THEN
         ICLPSA = -1
      ELSEIF(WSX(1) .GT. RECT(2))THEN
         ICLPSA = 1
      ELSE
         ICLPSA = 0
*        Section split by end of polygon vertex list
         NSMIN = NSMIN+1
      ENDIF
      IF(WSY(1) .LT. RECT(3))THEN
         ICLPSA = ICLPSA - 10
      ELSEIF(WSY(1) .GT. RECT(4))THEN
         ICLPSA = ICLPSA + 10
      ENDIF

*     Is there enough workspace?
      IF(NW .LT. NEED)GOTO 999

*     Close polygon in workspace
      WSX(1+NP) = WSX(1)
      WSY(1+NP) = WSY(1)
*     loop round the closed polygon - workspace (1:NP+1)
      DO 200 IP=1,NP
*       Clip status of second point in pair
        IF(WSX(1+IP) .LT. RECT(1))THEN
           ICLPSB = -1
        ELSEIF(WSX(1+IP) .GT. RECT(2))THEN
           ICLPSB = 1
        ELSE
           ICLPSB = 0
        ENDIF
        IF(WSY(1+IP) .LT. RECT(3))THEN
           ICLPSB = ICLPSB - 10
        ELSEIF(WSY(1+IP) .GT. RECT(4))THEN
           ICLPSB = ICLPSB + 10
        ENDIF
*       If there is a change in clip status
        IF(ICLPSA .NE. ICLPSB)THEN
           IF(ICLPSA .EQ. 0)THEN
*          First point inside second out
              NCROSS = NCROSS + 1
              NSMIN  = NSMIN  + 1
           ELSEIF(ICLPSB .EQ. 0)THEN
*          First point outside second in
              NCROSS = NCROSS + 1
              NSMIN = NSMIN + 1
           ELSE
*          Both points outside
              ICLD = ABS(ICLPSB - ICLPSA)
              ICLS = ICLPSB + ICLPSA
              IF((ICLD .GT. 2).AND.(ICLD .NE. 10*(ICLD/10)))THEN
*                 Did the line cross the clipping rectangle ?
                  CALL GKLNCR(RECT,WSX(IP),WSY(IP),
     :                             WSX(IP+1),WSY(IP+1),CROSS)
                  IF(CROSS)THEN
                     NCROSS = NCROSS + 2
                  ENDIF
              ELSEIF(ICLS .EQ. 0)THEN
*                 Line crosses opposite sides of clipping rectangle
                  NCROSS = NCROSS + 2
              ENDIF
           ENDIF
        ENDIF
*       Move to next two points
        ICLPSA = ICLPSB
  200 CONTINUE
*     Counting of crossings complete

*
***** If no crossings *****
*
      IF(NCROSS .EQ. 0)THEN
*        Is centre of clipping rectangle in polygon?
         CALL GKPPFA((RECT(1)+RECT(2))/2.0,(RECT(3)+RECT(4))/2.0,
     :                NP,WSX,WSY,INSIDE)
         IF(INSIDE)THEN
*        Output clipping rectangle
           IPOLY(2) = 5
           WSX(1) = RECT(1)
           WSX(2) = RECT(2)
           WSX(3) = RECT(2)
           WSX(4) = RECT(1)
           WSY(1) = RECT(3)
           WSY(2) = RECT(3)
           WSY(3) = RECT(4)
           WSY(4) = RECT(4)
         ENDIF
*        Else - Output no polygons
         NPOLY = 0
         GOTO 999
      ENDIF

*     Is there enough workspace
      NEED = NP + NCROSS + 4
      IF(NW .LT. NEED)GOTO 999

*****  Get stack space *****
      ILOOP = 0
  250 CONTINUE
      IF(NSMIN .LT. 2)NSMIN = 2
      IF((KWFAIS(KWKIX) .EQ. GHOLLO) .OR. (ILOOP .GE. 1))THEN
         MSECT = NSMIN
         CALL GKSTAL (KINTGS,2*MSECT,ISL)
      ELSE
         NSWANT = NSMIN + 4*MPOLY - 4
         CALL GKLUMP (KINTGS,2,NSWANT,NSMIN,4,MSECT,ISL)
      ENDIF
      IF(KERROR .NE. 0)GOTO 999

***** Get Unspliced Clipped Polygon and Section list *****
      KSTACK(ISL) = 1
      KSTACK(ISL+1) = NP+1
      CALL GKPSCL (RECT,NCROSS,NEED,WSX,WSY,IWR,MSECT,KSTACK(ISL),
     :             MPOLY,IPOLY,NPOLY)
      IF(KERROR .NE. 0)GOTO 989
      IF(NPOLY .EQ. 0)GOTO 989
      NEED = IWR - 1
*     Is there enough provision for polygons
      IF(NPOLY .GT. MPOLY)GOTO 989

*
*     If not hollow and several polygons, need to take care of
*     case were some of the polygons overlap.
      IF((NPOLY .GT. 1) .AND. (KWFAIS(KWKIX) .NE. GHOLLO))THEN
         CALL GKPSNI (NW,WSX,WSY, MSECT,KSTACK(ISL),
     :                NPOLY,IPOLY, 0, NPOLCR)
         IF(KERROR .NE. 0)GOTO 989
         NEED = NEED + NPOLCR
         IF(NEED .GT. NW)GOTO 989
         IF(NPOLCR .GT. 0)THEN
           NSECT = IPOLY(NPOLY+1) - 1
           IF(NSECT + 4*NPOLCR .GT. MSECT) THEN
*             Here section list is not long enough to
*             accomodate the joining of the polygons
*             need to realocate space for sections.
              NSMIN = NSECT + 4*NPOLCR
              CALL GKSTDA (KINTGS,ISL)
              ILOOP = ILOOP + 1
*             ILOOP prevents us being caught in loop if bug.
              IF(ILOOP .LE. 1)GOTO 250
              GOTO 999
           ENDIF

*          Process section list to join up polygons at intersections
           MPO = NPOLY
           CALL GKPSJI(NW,WSX,WSY,IWR,MSECT+4*NPOLCR,KSTACK(ISL),
     :                 NPOLY,MPO,IPOLY)
           IF(KERROR .NE. 0)GOTO 900
           MSECT = IPOLY(NPOLY+1) - 1
           NEED = IWR - 1
         ENDIF
      ENDIF

***** Splice polygons together on the workspace *****
      CALL GKPSFH (NPOLY,IPOLY,NEED,WSX,WSY,MSECT,KSTACK(ISL))
      IF(KERROR .NE. 0)GOTO 900

***** Set IPOLY to point to workspace instead of KSTACK(ISL)
      DO 300 JP = NPOLY+1,2,-1
        IPOLY(JP) = KSTACK(ISL + 2*IPOLY(JP-1) - 1)
        IF(IPOLY(JP) .GT. NW+1)THEN
          CALL GKBUG( -1016,'GKFHCL')
          GOTO 989
        ENDIF
  300 CONTINUE

***** Re-adjust number of polygons
      DO 350 JP=NPOLY,1,-1
             IF(IPOLY(JP+1)-IPOLY(JP) .GE. 2)GOTO 351
  350 CONTINUE
  351 CONTINUE
      NPOLY = JP
*
***** Release Stack
  900 CONTINUE
  989 CALL GKSTDA (KINTGS,ISL)

  999 CONTINUE
      END
