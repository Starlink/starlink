      SUBROUTINE GKFILH(NRD,RX,RY,FILSUB)
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
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Fill Area Utility fills supplied polygon with filling
*     as supplied by FILSUB, clipping it.
*
*  MAINTENANCE LOG
*  ---------------
*     21/04/88  KEVP  Created from part of GKFILS
*     18/05/88  KEVP  Converted arguments from DC to WC
*     23/02/89  KEVP  Removed Debug WRITE statements
*     29/03/89  KEVP  Raised MCROSS from 8 to 16, introduced NXTRA
*     26/04/89  KEVP  Changed name of GKFACL to GKFHCL (Hardware/Hollow)
*                     Removed debug WRITE statements.
*     23/05/89  KEVP  Made more effective use of stackspace by allocating
*                     with GKLUMP. MCROSS put back to 8 (max for convex).
*                     Raised NXTRA to 20.
*     12/06/89  KEVP  Allowed GKFHCL to be called a 3rd time,
*                     in case there is not enough space for polygons.
*      7/12/89  NMH   Removed debug write statement
*     30/07/90  KEVP  Corrected stack error GOTO statements (Bug C23).
*
*
*  ARGUMENTS
*  ---------
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates in WC
*     INP FILSUB Device driver polyline or fill-area routine
*
      INTEGER  NRD
      REAL     RX(NRD),RY(NRD)
      EXTERNAL FILSUB
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     IWSX   Stack base for clipping workspace X-coords
*     IWSY   Stack base for clipping workspace Y-coords
*     IPP    Stack base of clipped polygon list
*
*     RECT   Clipping rectangle (xmin,xmax,ymin,ymax)
*     XSHR,YSHR  Shrinking factor for clip rectangle
*
*     MCROSS The maximum number of crossings of the clipping rectangle
*            boundary by the polygon, initially allowed
*     MPOLY  The maximum number of clipped polygons
*     NPMIN  The miminum number of clipped polygons required for stack
*     NPWANT The desired number of clipped polygons required for stack
*     NWMIN  The minumum length of workspace acceptable
*     NWWANT The desired length of workspace
*     NXTRA  Number of extra points added to workspace to allow for
*            intersecting polygons after HOLLOW clipping, if HARDWARE
*            Clipping is to be done.
*     NWS    The number of points available in the workspace
*
*     ILOOP  Number of times in loop
*     NPOLY  Actual number of polygons
*     NEED   Actual worspace needed
*
*     JP     Current Clipped Polygon
*     LP     Length of clipped polygon (in vertices)
*     IFIRST Position in workspace of first point of clipped polygon
*     ILAST  Position in workspace of last point of clipped polygon
*
*     RLINEX Endpoint X-Coords for 2-point polyline (hollow only)
*     RLINEY Endpoint Y-Coords for 2-point polyline (hollow only)

      INTEGER  IWSX,IWSY, IPP
      INTEGER  MCROSS, MPOLY, NWS, JP, LP, IFIRST,ILAST, NXTRA
      INTEGER  ILOOP, NPOLY, NEED
      INTEGER  NWMIN, NWWANT, NPMIN, NPWANT
      REAL RLINEX(2), RLINEY(2), RECT(4), XSHR,YSHR

      PARAMETER (MCROSS = 8, NXTRA = 20)
*
*  STACK USAGE
*  -----------
*     -for Fill-Area CLip algorithm:-  (GKLUMP used)
*
*       REAL space of minimum size (NRD+MCROSS+4)*2
*                     target size  (3*NRD + 4   )*2 HOLLOW
*                                  (10*NRD/3 + 4)*2 other fillstyles
*                           subject to limit of 1/4 total stack
*            at IWSX
*
*       INTEGER space of minumum size MCROSS/2 + 1
*                        target size  NRD/3
*                           subject to limit of 1/20 total stack
*            at IPP
*
*     -should this not be enough, the stack is released and reallocated-
*     -so the actual number of crossings instead of MCROSS.            -
*
*       Total INT+REAL requirement, including workspace acquired
*        by GKFHCL and its subroutines, amounts to
*                   NRD*2 + NCROSS*9 + MPOLY + 11,
*        where NCROSS is the number of crossings and if it is large)
*        If the fill-style is not HOLLOW this goes up to,
*                   NRD*2 + NCROSS*9 + MPOLY*2 + 12
*
*
*        At a later stage the total INT+REAL requirement no greater
*        than
*                   NRD*4 + NCROSS*5 + MPOLY + 8.
*        for HOLLOW else its no greater than
*                   NRD*4 + NCROSS*5 + MPOLY*3 + 10
*
*      For polygons with a large number of vertices, stack requirement
*      is less than that needed by GKPCLP,
*      if the number times the polygon crosses the clipping rectangle
*      crosses the clipping rectangle per vertex is less than 10/9.
*
*  ALGORITHM
*  ---------
*     (1) Workspace is acquired, sufficient for the maximum possible
*     final no of (clip-induced) vertices, and fragmented polygons.
*
*     (2) The polygon vertex coordinates are converted to DC and put
*         into the workspace
*
*     (3) GKFHCL is invoked to clip the polygon.
*
*     (4) If the workspace is insuffucient for GKFHCL,
*            the workspace is released and
*            re-acquired in the amount needed.
*            Steps (2) and (3) are then repeated
*            with the larger workspace.
*
*     (5) FILSUB is invoked to display the clipped polygon(s).
*     If the fill-area style is hollow, then the last point
*     is joined to the first.
*
*     (6) At the end of the day all work-space acquisitions are released.
*
*  ERRORS
*  ------
*     301  Not enough stack available
*
*  COMMENTS
*  --------
*     If the fill area style is hollow,
*        FILSUB can be the device polyline routine.
*     Otherwise, it should supply the appropriate filling
*        and close the filled area itself.
*
*     For fill-style hollow the clip rectangle is shrunk slightly
*     to ensure that all clip induced edges are visible.
*
*     For hardware fill-area the utility does not yet work corectly,
*     if several polygons are formed by clipping
*     and at least two of them overlap.
*
*---------------------------------------------------------------------

* Extract total clip rectangle using W/S ID from Comms Area:
* if fill area style is hollow, shrink it slightly
      IF(KWFAIS(KWKIX) .EQ. GHOLLO)THEN
         XSHR = QTOL*(QWCLXR(KWKIX)-QWCLXL(KWKIX))
         YSHR = QTOL*(QWCLYT(KWKIX)-QWCLYB(KWKIX))
      ELSE
         XSHR = 0.0
         YSHR = 0.0
      ENDIF
      RECT(1)=QWCLXL(KWKIX) + XSHR
      RECT(2)=QWCLXR(KWKIX) - XSHR
      RECT(3)=QWCLYB(KWKIX) + YSHR
      RECT(4)=QWCLYT(KWKIX) - YSHR

*  Acquire stack space for clipping workspace and polygon pointers
*     Initially as a mimimum,
*         allow up to MCROSS crossings and MCROSS/2 polygons.
*     but if possible,
*         allow up to 2*NRD crossings and NRD/3 polygons.
*         but without taking more than quarter of the remaining
*         stackspace for workspace or 1/20 for polygons.
      NPMIN = MCROSS/2
      NPWANT = NRD/3
      IF(NPMIN .GT. NPWANT)NPMIN = NPWANT
      NWMIN = NRD + MCROSS + 4
      IF(KWFAIS(KWKIX) .EQ. GHOLLO)THEN
        NWWANT = 3*NRD + 4
      ELSE
        NWWANT = (10*NRD)/3 + 4
      ENDIF
      IF(NWMIN .GT. NWWANT)NWMIN = NWWANT
* --- Loop in case of inadequate space ---
      ILOOP = 1
  450 CONTINUE
      CALL GKLUMP(KREALS,2,NWWANT,NWMIN,4,NWS,IWSX)
      IF (KERROR .NE. 0) GOTO 999
      IWSY = IWSX + NWS
      CALL GKLUMP(KINTGS,1,NPWANT+1,NPMIN+1,20,MPOLY,IPP)
      IF(KERROR .NE. 0) GOTO 989
      MPOLY = MPOLY - 1

*     Convert coordinates to DC and put them into the workspace
      CALL GKTWD (NRD,RX,RY,QSTACK(IWSX),QSTACK(IWSY))

*     Clip polygon, if space is sufficient
      CALL GKFHCL(RECT,NRD,NWS,QSTACK(IWSX),QSTACK(IWSY),
     :            MPOLY,KSTACK(IPP),NPOLY,NEED)
      IF(KERROR .NE. 0) GOTO 987
*** Note: KERROR is set, if insufficient workspace for crossings ***
      IF (KERROR.NE.0) GOTO 987
*     Polygon is clipped only if there is enough space for it.
      IF((NEED .GT. NWS) .OR. (NPOLY .GT. MPOLY))THEN
*        The supplied space is not sufficient.
*        Release stack and repeat with stack of required size.
         CALL GKSTDA (KINTGS,IPP)
         CALL GKSTDA (KREALS,IWSX)
         IF(KERROR .NE. 0)GOTO 999
         IF(KWFAIS(KWKIX) .EQ. GHOLLO)THEN
            NWMIN = NEED
         ELSE
*          Allow for up upto NXTRA polygon intersections
           NWMIN = NEED + NXTRA
         ENDIF
         NPMIN = NPOLY
         IF(ILOOP .LE. 3)THEN
           ILOOP = ILOOP + 1
           GOTO 450
*          End of loop
         ELSE
*          Excessive repeat of loop - This should not happen.
           GOTO 999
         ENDIF
*   ---  End of loop (second repeat should not occur) ---
      ENDIF

      IF (NPOLY .GT. 0) THEN
        IFIRST=KSTACK(IPP)
*       Use FILSUB and join up ends, if hollow.
        DO 20 JP=1,NPOLY
            ILAST=KSTACK(IPP+JP)-1
            LP=ILAST-IFIRST+1
            IF((LP .GT. NWS) .OR. (LP .LT. 1))THEN
                CALL GKBUG (-1016,'GKFILH')
                GOTO 987
            ENDIF
            CALL FILSUB(LP,
     :           QSTACK(IWSX+IFIRST-1),QSTACK(IWSY+IFIRST-1))
* now join first to last, if hollow.
            IF(KWFAIS(KWKIX) .EQ. GHOLLO)THEN
               RLINEX(1)=QSTACK(IWSX+ILAST-1)
               RLINEY(1)=QSTACK(IWSY+ILAST-1)
               RLINEX(2)=QSTACK(IWSX+IFIRST-1)
               RLINEY(2)=QSTACK(IWSY+IFIRST-1)
               CALL FILSUB(2,RLINEX,RLINEY)
            ENDIF
            IFIRST=ILAST+1
   20   CONTINUE
      ENDIF

* Release Workspace
  987 CALL GKSTDA(KINTGS,IPP)
  989 CALL GKSTDA(KREALS,IWSX)
  999 CONTINUE
      END
