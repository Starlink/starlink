*---------------------------------------------------------------------
      SUBROUTINE GK1AFL(NRD, XW, YW)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Send (solid & pattern) fill area to the external PostScript file.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*  ARGUMENTS
*  ---------
*      INP NRD     - Integer number boundary points
*      INP XW      - Array of real x-coordinates of boundary points (WC)
*      INP YW      - Array of real y-coordinates of boundary points (WC)
*
      INTEGER NRD
      REAL XW(*), YW(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
*
*   EXTERNALS
*   ---------
*  GK1ATF  - Device Trapezoid Output Routine
*
      EXTERNAL GK1ATF
*
*  LOCALS
*  ------
*  CHANGE  - Logical flag.
*  DUMMY   - Dummy character, required by the buffering routine.
*  FC      - Format for the coordinates
*  I,J     - Temporary count variables.
*  IALG    - Choice of Algorithm
*  ICHUNK  - Size of stack, used for splitting FA boundary points.
*  IFASTY  - Workspace offset for current fill area style
*  IOFF    - Offset for stack.
*  IPOLY   - Dummy argument to GKPSQX, indicating just one polygon.
*  IREM    - Dummy integer, required by the buffering routine.
*  LC      - Length of the coordinates when formatted
*  LFC     - Length of the coordinates format
*  MAXARB  - Maximum number of vertices for Arbitrary polygon (one path)
*  MAXSIM  - Maximum number of vertices for Simple polygon (one path)
*  N       - Count for stack.
*  NCROSS  - Number of edge crossings counted
*  NTZ     - Number of trapezoids that can be sent in one path
*  REALA   - Real array to hold the directory data.
*  RTMP1   - Temporary real to hold device coordinates.
*  RTMP2   - Temporary real to hold device coordinates.
*  S       - Character variable, via which chunks of PostScript are sent for
*            buffering.
*

*     Offsets in KWKDAT
      INTEGER    IFASTY
      PARAMETER (IFASTY=4)

*     Chunksize,  Path Limit, Number of trapezoids
      INTEGER    ICHUNK,    NTZ,   MAXARB,   MAXSIM
      PARAMETER (ICHUNK=100,NTZ=49,MAXARB=13,MAXSIM=300)

*     small real
      REAL       SMALL
      PARAMETER (SMALL=1.0E-4)

*
      LOGICAL CHANGE
      CHARACTER S*100, DUMMY, FC*7
      INTEGER  I,J, IOFF, IREM, N, LC, LFC
      INTEGER IPOLY(1), IALG, NCROSS
      REAL RTMP1, RTMP2
*
*  ALGORITHM
*  ---------
*     Current fill area attributes are compared to those held locally.
*     If there's been a change new attributes are stored and written out.
*     If the area to be filled is deemed to complicated for Postscript,
*       it is divided into trapezoids. The 'tz' procedure is invoked to
*       add each such trapezoid as a subpath of the current path till
*       a limit is reached, then the path is closed and filled with
*       'fasoldo' or 'fapdo', before continuing or at end.
*     else
*       all the points are put into a single path (in chunks) and this
*       is closed and filled with 'fasoldo' or 'fapdo'.
*     'fasoldo' is used for solid fill and 'fapdo' for patterned fill.
*
*     The format for the coordinates in each chunk is selected so that
*     they are not too long.
*
*---------------------------------------------------------------------
*

*     Initialise the flag
      CHANGE = .FALSE.

*
*     See if locally stored copy of the attributes needs updating.
*
*     Fill Area Style Index:
      IF(KWKDAT(IFASTY,KWKIX).NE.KWFASI(KWKIX)) THEN
         KWKDAT(IFASTY,KWKIX) = KWFASI(KWKIX)
         CHANGE = .TRUE.
      ENDIF

*
*     Start from a new line in the external file
*
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*     Set up the fill area attributes (style index),
*     if change has occurred.
*
      IF (CHANGE) THEN
         WRITE(S, 50) KWKDAT(IFASTY,KWKIX)
   50    FORMAT( I3, ' fastat')
         CALL GKFOCO(KIOPB, S(1:10), IREM)
         CALL GKFOCO(KIOSN,DUMMY,IREM)
      ENDIF

*     Determinine whether to let postscript process the whole Fill Area
*     in one path or to decompose it into trapezoids.
      IALG = 1
      IF(NRD .GT. MAXSIM)THEN
         IALG = 2
      ELSEIF(NRD .GT. MAXARB)THEN
         IPOLY(1) = NRD
*        Find out whether the polygon crosses itself
         CALL GKPSQX(NRD,XW,YW,1,IPOLY,1,NCROSS)
         IF(NCROSS .GE. 1)THEN
            IALG = 2
         ENDIF
      ENDIF

*     Take a memory snapshot, so as not to clutter the memory with
*     temporaries.
      CALL GKFOCO(KIOPB,'save',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

      IF(IALG .EQ. 1)THEN
*       Send Whole Polygon to Postscript as one path
*
*       Prepare and send the fill area bounding coordinates.
*

*       Get the stack for coordinates chunking.
        CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
        IF (KERROR.EQ.0) THEN
*         Do a gsave here - matching grestore supplied later.
          CALL GKFOCO(KIOPB,'gsave',IREM)
          CALL GKFOCO(KIOSN,DUMMY,IREM)
*         There must only be ONE moveto per area because PostScript
*         takes each moveto to be the beginning of a subpath. Before
*         filling, all subpaths are closed by connecting their last
*         point to their first. For large, chunked, fill areas this
*         means many wrong sub-fills instead of a single correct one.
*         So, we move to the first point of the fill area and then
*         chunk as normal, but use lineto all the time (yes, this
*         does mean that boundary points of a chunk get visited
*         twice, but it's OK).
          CALL GKTWD(1,XW(1),YW(1), RTMP1, RTMP2)
          WRITE(S,70) RTMP1, RTMP2
   70     FORMAT(2F10.2, ' moveto')
          CALL GKFOCO(KIOSN,DUMMY,IREM)
          CALL GKFOCO(KIOPB,S(1:27),IREM)
*         Output in manageable sections
          N = ICHUNK
          DO 152 I=1,NRD,ICHUNK-1
            IF (NRD-I.LT.ICHUNK-1) N = NRD-I + 1
            IF (N.EQ.1) GOTO 152
*           Convert to Postscript Points
            CALL GKTWD(N,XW(I),YW(I),QSTACK(IOFF),
     :                               QSTACK(IOFF+ICHUNK))
*           Select Coordinate Formats and then Send Coords to File
            CALL GK1ASF(N,QSTACK(IOFF),LC,LFC,FC)
            CALL GKFOCO(KIOPB, '[', IREM)
            DO 100 J=0, N-1
               WRITE(S,FC(1:LFC)) QSTACK(IOFF+J)
               CALL GKFOCO(KIOPB, S(1:LC), IREM)
  100       CONTINUE
            CALL GK1ASF(N,QSTACK(IOFF+ICHUNK),LC,LFC,FC)
            CALL GKFOCO(KIOPB, '][', IREM)
            DO 120 J=0, N-1
               WRITE(S,FC(1:LFC)) QSTACK(IOFF+ICHUNK+J)
               CALL GKFOCO(KIOPB, S(1:LC), IREM)
  120       CONTINUE
*           Now call the fagen procedure to form the PostScript path.
            CALL GKFOCO(KIOPB, ']fagen', IREM)
  152     CONTINUE
*         Deallocate the stack
          CALL GKSTDA(KREALS, IOFF)
          IF(KWFAIS(KWKIX).EQ.GSOLID)THEN
*         Call the fasoldo procedure to do solid fill.
             CALL GKFOCO(KIOPB,' fasoldo', IREM)
             CALL GKFOCO(KIOSN,DUMMY,IREM)
          ELSE
*         Deal with pattern here.
            CALL GK1APA
          ENDIF
          CALL GKFOCO(KIOSN,DUMMY,IREM)
          CALL GKFOCO(KIOPB,'grestore',IREM)
        ELSE
          GOTO 990
        ENDIF
*     End of one path polygon case
      ELSEIF(IALG .EQ. 2)THEN
*     Polygon too complex - decompose into trapezoids
*                           send each as a subpath
*                           send several paths if long enough
*       Get stackspace for whole polygon
        IF(KERROR .EQ. 0)THEN
*         Call Trapezoid decomposition Fill Area Utility
*         filling trapezoids with GK1ATF.
          CALL GKFATZ(NRD,XW,YW,NTZ,0.1,GK1ATF)
        ELSE
          GOTO 990
        ENDIF
      ENDIF

*     Restore
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'restore',IREM)

  990 CONTINUE

      END
