C# IL>=a, OL>=0
      SUBROUTINE GKFATZ(NRD,RX,RY,NTZ,YTOL,TRASUB)
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
*     Fill Area using TRAPEZOID decomposition. The device
*     trapezoid output routine draws a sets of trapezoids with the
*     current fill area attributes.
*
*
*  MAINTENANCE LOG
*  ---------------
*     08/11/91  KEVP  Original Version Stabilised (C69).
*
*  ARGUMENTS
*  ---------
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates in WC
*     INP NTZ    Number of trapezoids that can be stored
*     INP YTOL   Tolerance on trapezoid Y-coords
*     INP TRASUB Device driver trapezoid output routine
*
      INTEGER NRD, NTZ
      REAL    RX(NRD),RY(NRD), YTOL
      EXTERNAL TRASUB
*
*  COMMON BLOCK USAGE
*  ------------------
*
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
*     IETVXB Stack base for Vertex X-coords
*     IETVYB Stack base for Vertex Y-coords
*     IPOLYB Stack base for polygon end vertex indices
*     IETTXB Stack base for Edge Top X-coords
*     IETTYB Stack base for Edge Top Vertex Y-Indices
*     IETBB  Stack base for Edge Bottom Vertex Indices
*     IETNXB Stack base for Next Edge Down
*     ITZB   Stack base of Trapezoids
*
*     I      Do loop index (never referred to)
*     IBOT   Stack index for bottom of trapeziod
*     IETLOW Index of lowest Vertex
*     IFAIS  Current Fill Area Style (for saving it)
*     IPOLY  Array of polygon end vertices (length 1 for one polygon)
*     ISTART Starting Vertex of First Polygon
*     ITOP   Stack index for top of trapeziod
*     ITZ    Number of Trapezoids extracted but not yet sent.
*     MAXDO  Maximum number of times that GKTZEX may be called
*     MPOLY  Maximum number of polygons after clipping
*     NEED   Number of vertices needed by the clipping utility
*     NPOLY  Number of polygons after clipping
*     NV     Number of vertices after clipping
*     RECT   Clipping rectangle
*     RYTOL  Tolerances per coordinate unit (ie, reciprocal of YTOL)
*     YLTZ   Top of last trapezoid
*     FOUND  True, if trapezoid has been found
*     LTZ    True, if last trapezoid exists
*     TOLUSE True, if the Y-tolerance is in use
*
      INTEGER IETVXB, IETVYB, IPOLYB
      INTEGER IETTXB, IETTYB, IETBB, IETNXB, ITZB
      INTEGER MPOLY, NPOLY, NEED, NV, IFAIS, ISTART, MAXDO
      INTEGER ITZ,IETLOW,I, ITOP,IBOT
      REAL RECT(4), RYTOL, YLTZ
      LOGICAL FOUND, LTZ, TOLUSE
*
*  EXTERNALS
*  ---------
*
*  STACK USAGE
*  -----------
*     -for Edge Table
*       REAL workspace of size   NRD starting at IETVXB
*       REAL workspace of size   NRD starting at IETVYB
*       REAL workspace of size   NRD starting at IETTXB
*       INT  workspace of size   NRD starting at IETTYB
*       INT  workspace of size   NRD starting at IETBB
*       INT  workspace of size NRD+1 starting at IETNXB
*
*
*  ALGORITHM
*  ---------
*     Sufficient blocks of workspace are acquired to accommodate
*     the Edge Table arrays for the supplied polygon and the
*     specified number of trapezoids.
*
*     GKTZME constructs the Edge Table based on vertex indices
*
*     GKTZEX extracts each trapezoid from this edge table
*
*     This trapezoid is put in the trapezoid stack allocation,
*     which is sent to the workstation by TRASUB when full or the
*     last trapezoid has been extracted.
*
*     At the end of the day all work-space acquisitions are released.
*
*  ERRORS
*  ------
*     301  Not enough stack available
*
*  COMMENTS
*  --------
*     This routine does not clip the fill area, but trapezoids
*     entirely outside the clipping rectangle are not sent.
*---------------------------------------------------------------------
*

* Extract total clip rectangle using W/S ID from Comms Area:
      RECT(1)=QWCLXL(KWKIX)
      RECT(2)=QWCLXR(KWKIX)
      RECT(3)=QWCLYB(KWKIX)
      RECT(4)=QWCLYT(KWKIX)

*   acquire space for Polygon Set
      CALL GKSTAL(KREALS,NRD+1,IETVXB)
      IF (KERROR.NE.0) GOTO 999
      CALL GKSTAL(KREALS,NRD+1,IETVYB)
      IF (KERROR.NE.0) GOTO 988
      MPOLY = NRD/3
      CALL GKSTAL(KINTGS,MPOLY+1,IPOLYB)
      IF(KERROR .NE. 0) GOTO 977

*   convert from WC
      CALL GKTWD (NRD,RX,RY,QSTACK(IETVXB),QSTACK(IETVYB))
*
*   clip polygon - as though hollow (ie, overlapping polygons not joined)
      IFAIS = KWFAIS(KWKIX)
      KWFAIS(KWKIX) = GHOLLO
      CALL GKFHCL(RECT,NRD,NRD+1,QSTACK(IETVXB),QSTACK(IETVYB),
     :            MPOLY,KSTACK(IPOLYB),NPOLY,NEED)
      IF(KERROR .NE. 0)GOTO 966
      IF(NEED .GT. NRD+1)THEN
*     Need more space for clip induced vertices
         NV = NEED
         CALL GKSTDA(KINTGS,IPOLYB)
         CALL GKSTDA(KREALS,IETVYB)
         CALL GKSTDA(KREALS,IETVXB)
         CALL GKSTAL(KREALS,NV,IETVXB)
         IF(KERROR .NE. 0) GOTO 999
         CALL GKSTAL(KREALS,NEED,IETVYB)
         IF(KERROR .NE. 0) GOTO 988
         CALL GKSTAL(KINTGS,MPOLY,IPOLYB)
         IF(KERROR .NE. 0) GOTO 977
         CALL GKTWD(NRD,RX,RY,QSTACK(IETVXB),QSTACK(IETVYB))
         CALL GKFHCL(RECT,NRD,NV,QSTACK(IETVXB),QSTACK(IETVYB),
     :               MPOLY,KSTACK(IPOLYB),NPOLY,NEED)
         IF(KERROR .NE. 0)GOTO 966
         NV = KSTACK(IPOLYB+NPOLY) - KSTACK(IPOLYB)
         IF(NV .GT. NEED) NV = NEED
      ELSE
*     Enough Space
         NV = NRD
      ENDIF
      KWFAIS(KWKIX) = IFAIS


*     Get stack space for rest of edge table
      CALL GKSTAL(KREALS,NV,IETTXB)
      IF (KERROR.NE.0) GOTO 966
      CALL GKSTAL(KINTGS,NV,IETTYB)
      IF (KERROR.NE.0) GOTO 955
      CALL GKSTAL(KINTGS,NV,IETBB)
      IF (KERROR.NE.0) GOTO 944
      CALL GKSTAL(KINTGS,NV+1,IETNXB)
      IF (KERROR.NE.0) GOTO 933

*     Convert Polygon Ends for use by GKTZME
      ISTART = KSTACK(IPOLYB)
      DO 20 I=0,NPOLY
         KSTACK(IPOLYB+I) = KSTACK(IPOLYB+I)-ISTART
  20  CONTINUE

*     Make Edge Table
      CALL GKTZME(NV,QSTACK(IETVXB+ISTART-1),QSTACK(IETVYB+ISTART-1),
     :            NPOLY,KSTACK(IPOLYB+1),
     :            IETLOW,QSTACK(IETTXB),KSTACK(IETTYB),
     :            KSTACK(IETBB),KSTACK(IETNXB))

*     Acquire space for the trapezoids
      CALL GKSTAL(KREALS,6*NTZ,ITZB)
      IF(KERROR.NE.0) GOTO 922

*     Set variables prior to do loop
      ITZ = 0
      TOLUSE = (YTOL .GT. QTOL)
      IF(TOLUSE)THEN
         RYTOL = 1.0/YTOL
      ELSE
         RYTOL = QNIL
      ENDIF
*
      YLTZ = QNIL
      LTZ = .FALSE.
      MAXDO = NV*NV
* @@@ Limit to Protect CPU @@@
      IF(MAXDO .GT. 250000)MAXDO = 250000
      DO 40 I=1,MAXDO
*        Extract a Trapezoid and put into stack
         CALL GKTZEX(NV,QSTACK(IETVXB),QSTACK(IETVYB),
     :               QSTACK(IETTXB),KSTACK(IETTYB),KSTACK(IETBB),
     :               KSTACK(IETNXB),RECT(3),
     :               QSTACK(ITZB+6*ITZ),FOUND)
         IF(KERROR .NE. 0)GOTO 41
         IF(FOUND)THEN
            IF(LTZ .AND. (QSTACK(ITZB+6*ITZ+4) .GT. YLTZ))THEN
*           Trapezoid higher than last - bug: quit
               GOTO 41
            ELSE
               LTZ = .TRUE.
               YLTZ = QSTACK(ITZB+6*ITZ+4)
            ENDIF
         ENDIF

*
         IF(FOUND)THEN
*          A trapezoid was found in the edge table.
           IF(TOLUSE)THEN
*             Round Y-coords to units of tolerance
              ITOP = ITZB + 6*ITZ + 4
              IBOT = ITZB + 6*ITZ + 5
              QSTACK(ITOP) = YTOL*INT(RYTOL*QSTACK(ITOP))
              QSTACK(IBOT) = YTOL*INT(RYTOL*QSTACK(IBOT))
*             Keep trapezoid, if non-zero depth
              IF(QSTACK(ITOP) .GT. QSTACK(IBOT))THEN
                 ITZ = ITZ + 1
              ENDIF
           ELSE
*             Keep trapezoid
              ITZ = ITZ + 1
           ENDIF
           IF(ITZ .EQ. NTZ) THEN
*             Send stack of trapezoids if full
              CALL TRASUB (NTZ,NTZ,QSTACK(ITZB))
              ITZ = 0
           ENDIF
         ELSE
*          Trapezoid not found - edge table empty or bug - quit loop
           GOTO 41
         ENDIF
   40 CONTINUE
*  Only get here if bug
   41 CONTINUE
*     Send any remaining trapezoids
      IF(ITZ .NE. 0) CALL TRASUB (ITZ,NTZ,QSTACK(ITZB))

* Release Workspace
  900 CALL GKSTDA(KREALS,ITZB)
  922 CALL GKSTDA(KINTGS,IETNXB)
  933 CALL GKSTDA(KINTGS,IETBB)
  944 CALL GKSTDA(KINTGS,IETTYB)
  955 CALL GKSTDA(KREALS,IETTXB)
  966 CALL GKSTDA(KINTGS,IPOLYB)
  977 CALL GKSTDA(KREALS,IETVYB)
  988 CALL GKSTDA(KREALS,IETVXB)
  999 CONTINUE

      END
