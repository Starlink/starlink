C# IL>=a, OL>=0
      SUBROUTINE GKFILA(NRD,RX,RY,IFASC,IFILSC,LINSUB,ROSUB,FILSUB)
*
* (C) COPYRIGHT ICL & SERC  1988
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             NGB/KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Fill Area Utility fills supplied polygon with appropriate
*     filling, using hardware or software fill area facilities
*     according to the Fill Area Strategy Code
*
*  MAINTENANCE LOG
*  ---------------
*      3/03/83  NGB   Original version stabilized
*      7/03/83  NGB   Use KCLIPS to see if device can clip
*     29/04/83  AS    Change subroutine name from GKFILL
*      4/07/83  PGLS  KERROR changes
*     28/07/83  AS    Use heap for patterns, other consistency changes
*     29/11/83  NGB   Remove redundant arguments from call to GKSCAN
*     02/03/84  MGC   Replace -1 with KNIL
*     13/11/85  DRJF  PATTERN, HATCH and SOLID FILL AREA styles use
*                     SCAN LINES for there creation. This is
*                     inappropiate for the BENSON and other devices,
*                     hence the introduction of a scale factor which
*                     will ensure a more suitable spacing of the lines
*                     in HATCH and PATTERN styles.
*     04/02/86  RMK   Shortened line in GKSCAN call to avoid problems
*                     with VAX version.
*     19/01/87  RMK   IS conversion. To allow negative hatch styles,
*                     added local variable to convert hatch style index
*                     to list element.
*     21/04/88  KEVP  Split the main functions between the new utilities
*                     GKFILH, GKFILP and GKFILC according to the
*                     fill-area style.
*     29/04/88  KEVP  Introduced Fill Area Strategy Code to select
*                     hardware or software fill area for each style.
*                     Changed name from GKFILS
*
*  ARGUMENTS
*  ---------
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates
*     INP IFASC  Fill Area strategy code (See Comments)
*     INP IFILSC FILL AREA scale factor
*     INP LINSUB Device driver polyline output routine
*     INP ROSUB  Device driver raster output routine
*     INP FILSUB Device driver fill-area output routine
*
      INTEGER NRD, IFASC, IFILSC
      REAL    RX(NRD),RY(NRD)
      EXTERNAL LINSUB, ROSUB, FILSUB
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     HWFILL True, if hardware filling is to be used
*     IDCX   Stack bases of transformed vertex coords
*     IDCY   Stack bases of transformed vertex coords
*
      INTEGER IDCX,IDCY
      LOGICAL HWFILL
*
*  STACK USAGE
*  -----------
*     -for transformation to DC:
*       REAL workspace of size NRD starting at IDCX
*       REAL workspace of size NRD starting at IDCY
*
*  COMMENTS (Fill Area Strategy Code)
*  ----------------------------------
*      Fill Area     Software(s/w) or Hardware(H/W) Fill Used
*      Strategy
*        Code         SOLID    HATCHED     PATTERNED
*          0           s/w       s/w          s/w
*          1           H/W       s/w          s/w
*          2           s/w       H/W          s/w
*          3           H/W       H/W          s/w
*          4           s/w       s/w          H/W
*          5           H/W       s/w          H/W
*          6           s/w       H/W          H/W
*          7           H/W       H/W          H/W
*
*  ALGORITHM
*  ---------
*     The polygon vertices are first transformed into DC
*
*     For Fill-style HOLLOW, call GKFILH with LINSUB.
*
*     For other fill area styles,
*     the appropiate bit is extracted from the fill area
*     strategy code, which sets the hardware fill flag.
*
*     If the hardware fill flag is true,
*                            call GKFILH with FILSUB
*     else
*       for fill-style
*                     SOLID, call GKFILP with LINSUB
*                   PATTERN, call GKFILP with ROSUB
*                     HATCH, call GKFILC with LINSUB.
*
*     Finally the stack for DC is released.
*
*  ERRORS
*  ------
*     301  Not enough stack available
*
*---------------------------------------------------------------------
*   transform vertices to DC
         CALL GKSTAL(KREALS,NRD,IDCX)
         IF (KERROR.NE.0) GOTO 99
         CALL GKSTAL(KREALS,NRD,IDCY)
         IF (KERROR.NE.0) GOTO 99
         CALL GKTWD (NRD,RX,RY,QSTACK(IDCX),QSTACK(IDCY))

      IF (KWFAIS(KWKIX) .EQ. GHOLLO) THEN
* (fillstyle hollow)
         CALL GKFILH (NRD,QSTACK(IDCX),QSTACK(IDCY),LINSUB)
      ELSE
* (non-hollow fillstyle)
*
*        Set hardware fill flag
         HWFILL = .FALSE.
         IF((IFASC .GT. 0) .AND. (IFASC .LT. 8))THEN
            IF(KWFAIS(KWKIX) .EQ. GSOLID)THEN
               HWFILL = (IFASC .NE. 2*(IFASC/2))
            ELSEIF(KWFAIS(KWKIX) .EQ. GHATCH)THEN
               HWFILL = (IFASC/2 .NE. 2*(IFASC/4))
            ELSE
               HWFILL = (IFASC .GE. 4)
            ENDIF
         ENDIF

         IF(HWFILL)THEN
*        Hardware Fill
            CALL GKFILH(NRD,QSTACK(IDCX),QSTACK(IDCY),FILSUB)
         ELSEIF(KWFAIS(KWKIX) .EQ. GHATCH)THEN
*        Software Hatch Fill
            CALL GKFILC(NRD,QSTACK(IDCX),QSTACK(IDCY),IFILSC,LINSUB)
         ELSE
*        Software solid or pattrned fill
            CALL GKFILP(NRD,QSTACK(IDCX),QSTACK(IDCY),IFILSC,
     :                  LINSUB,ROSUB)
         ENDIF
      ENDIF

*     Release stack
      CALL GKSTDA(KREALS,IDCY)
      CALL GKSTDA(KREALS,IDCX)

   99 CONTINUE
      END
