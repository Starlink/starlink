C# IL>=a, OL>=0
      SUBROUTINE GKFILP(NRD,RX,RY,IFILSC,LINSUB,ROSUB)
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
*     Fill Area in SOLID or PATTERN using the raster output
*     routine in scanlines.
*
*  MAINTENANCE LOG
*  ---------------
*     21/04/88  KEVP  Created from part of GKFILS (Author: NGB)
*     18/05/88  KEVP  Changed arguments from DC to WC
*     22/08/90  KEVP  Unused variables found to be deleted.
*     22/08/90  KEVP  Put in correct GOTO statements for stack errors,
*                     (Bug C23).
*
*  ARGUMENTS
*  ---------
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates in WC
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
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     RETLOY DC Y-range covered by Area
*     RETHIY DC Y-range covered by Area
*     ILOY   Scanline Integer equivalent DC Y-range covered by Area
*     IHIY   Scanline Integer equivalent DC Y-range covered by Area
*     IETI   Edge Table Control Variables
*     IETMAX Edge Table Control Variables
*     IDCX   Stack base for DC X-coordinates
*     IDCY   Stack base for DC Y-coordinates
*     IETXB  Stack bases of Edge Table arrays
*     IETYB  Stack bases of Edge Table arrays
*     IETDXB Stack bases of Edge Table arrays
*     IETEYB Stack bases of Edge Table arrays
*     IETNXB Stack bases of Edge Table arrays
*     XMIN   Clipping rectangle
*     XMAX   Clipping rectangle
*     YMIN   Clipping rectangle
*     YMAX   Clipping rectangle
*
      INTEGER ILOY, IHIY, IETI, IDCX,IDCY,
     :        IETMAX, IETXB, IETYB, IETDXB, IETEYB, IETNXB, IY
      REAL RETLOY, RETHIY, XMIN, XMAX, YMIN, YMAX
*
*  EXTERNALS
*  ---------
*
      EXTERNAL GKXLIN,GKPMAP

*  HEAP USAGE
*  ----------
*     Pattern details
*
*  STACK USAGE
*  -----------
*     -for conversion from WC to DC
*       REAL workspace of size NRD+1 starting at IDCX
*       REAL workspace of size NRD+1 starting at IDCY
*
*     -for scan-conversion Edge Table:
*       REAL workspace of size NRD+1 starting at IETXB
*       REAL workspace of size NRD+1 starting at IETYB
*       REAL workspace of size NRD+1 starting at IETDXB
*       REAL workspace of size NRD+1 starting at IETEYB
*       INT  workspace of size NRD+1 starting at IETNXB
*
*
*  ALGORITHM
*  ---------
*     Sufficient blocks of workspace are acquired to accommodate
*     the Edge Table arrays for the supplied polygon.
*
*     GKMET constructs the Edge Table
*
*     GKSCAN delivers slices to either
*         GKXLIN, for Fill-style SOLID
*     or  GKPMAP, for Fill-style PATTERN
*
*     At the end of the day all work-space acquisitions are released.
*
*  ERRORS
*  ------
*     301  Not enough stack available
*
*  COMMENTS
*  --------
*     If the fill-area style is neither solid or patterned,
*     this utility will do nothing but waste time.
*
*     This utility assumes DC to be Raster Coordinates.
*---------------------------------------------------------------------
*
*   transform vertices to DC
      CALL GKSTAL(KREALS,NRD,IDCX)
      IF (KERROR.NE.0) GOTO 999
      CALL GKSTAL(KREALS,NRD,IDCY)
      IF (KERROR.NE.0) GOTO 988
      CALL GKTWD (NRD,RX,RY,QSTACK(IDCX),QSTACK(IDCY))

* Extract total clip rectangle using W/S ID from Comms Area:
      XMIN=QWCLXL(KWKIX)
      XMAX=QWCLXR(KWKIX)
      YMIN=QWCLYB(KWKIX)
      YMAX=QWCLYT(KWKIX)

*   acquire space for Edge Table
      CALL GKSTAL(KREALS,NRD+1,IETXB)
      IF (KERROR.NE.0) GOTO 977
      CALL GKSTAL(KREALS,NRD+1,IETYB)
      IF (KERROR.NE.0) GOTO 966
      CALL GKSTAL(KREALS,NRD+1,IETDXB)
      IF (KERROR.NE.0) GOTO 955
      CALL GKSTAL(KREALS,NRD+1,IETEYB)
      IF (KERROR.NE.0) GOTO 944
      CALL GKSTAL(KINTGS,NRD+1,IETNXB)
      IF (KERROR.NE.0) GOTO 933

      CALL GKMET(NRD,QSTACK(IDCX),QSTACK(IDCY),RETLOY,RETHIY,IETMAX,
     :           QSTACK(IETXB),QSTACK(IETYB),
     :           QSTACK(IETDXB),QSTACK(IETEYB),KSTACK(IETNXB))

      ILOY=IFIX(RETLOY)
      IHIY=IFIX(RETHIY)
      IETI=0
      IY=IHIY
*
*     Begin Loop
*
   40 CONTINUE
      IF (KWFAIS(KWKIX) .EQ. GSOLID) THEN
* call SCAN with instructions to call XLIN for each output span
          CALL GKSCAN(IY+IFILSC,IY,IETI,IETMAX,QSTACK(IETXB),
     :                QSTACK(IETYB),QSTACK(IETDXB),QSTACK(IETEYB),
     :                KSTACK(IETNXB),XMIN,XMAX,YMIN,YMAX,
     :                GKXLIN,LINSUB)

      ELSEIF(KWFAIS(KWKIX) .EQ. GPATTR) THEN
* call SCAN with instructions to call PMAP for each output span
                  CALL GKSCAN(IY+IFILSC,IY,IETI,IETMAX,QSTACK(IETXB),
     :                    QSTACK(IETYB),QSTACK(IETDXB),QSTACK(IETEYB),
     :                    KSTACK(IETNXB),XMIN,XMAX,YMIN,YMAX,
     :                    GKPMAP,ROSUB)
* (uses derived Pattern details out of Pattern COMMON Data)

      ENDIF
      IY=IY-IFILSC
      IF (IY.GE.ILOY)  GOTO 40
*
*     End Loop
*

* Release Workspace
  900 CALL GKSTDA(KINTGS,IETNXB)
  933 CALL GKSTDA(KREALS,IETEYB)
  944 CALL GKSTDA(KREALS,IETDXB)
  955 CALL GKSTDA(KREALS,IETYB)
  966 CALL GKSTDA(KREALS,IETXB)
  977 CALL GKSTDA(KREALS,IDCY)
  988 CALL GKSTDA(KREALS,IDCX)
  999 CONTINUE

      END
