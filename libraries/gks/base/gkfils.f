C# IL>=a, OL>=0
      SUBROUTINE GKFILS(NRD,RX,RY,IFILSC,LINSUB,ROSUB)
*
* (C) COPYRIGHT ICL & SERC  1988
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             NGB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Fill Area Utility fills supplied polygon with appropriate
*     filling, without using any hardware fill area facility.
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
*     18/05/88  KEVP  GKFILH, GKFILC & GKFILP changed to take WC
*
*  ARGUMENTS
*  ---------
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates (WC)
*     INP IFILSC FILL AREA scale factor
*     INP LINSUB Device driver polyline output routine
*     INP ROSUB  Device driver raster output routine
*
      INTEGER NRD, IFILSC
      REAL    RX(NRD),RY(NRD)
      EXTERNAL LINSUB, ROSUB
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
*     none
*
*  ALGORITHM
*  ---------
*     Call fill area utility according to fill-style:
*
*     HOLLOW     GKFILH with LINSUB
*     SOLID      GKFILP with LINSUB
*     PATTERN    GKFILP with ROSUB
*     HACHED     GKFILC with LINSUB
*
*---------------------------------------------------------------------
*
      IF(KWFAIS(KWKIX) .EQ. GHOLLO)THEN
*     Hollow Fill
         CALL GKFILH(NRD,RX,RY,LINSUB)
      ELSEIF(KWFAIS(KWKIX) .EQ. GHATCH)THEN
*     Software Hatch Fill
         CALL GKFILC(NRD,RX,RY,IFILSC,LINSUB)
      ELSE
*     Solid or pattrned fill
         CALL GKFILP(NRD,RX,RY,IFILSC,LINSUB,ROSUB)
      ENDIF
      RETURN
      END
