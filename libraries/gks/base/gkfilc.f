C# IL>=a, OL>=0
      SUBROUTINE GKFILC(NRD,RX,RY,IFILSC,LINSUB)
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
*     Fill hatched area
*
*  MAINTENANCE LOG
*  ---------------
*     21/04/88  KEVP  Created from part of GKFILS (Author: NGB)
*     18/05/88  KEVP  Changed Arguments from DC to WC
*     22/07/90  PLP   Removed unused locals N, ITXB, ITYB.
*     22/08/90  KEVP  Put in correct GOTO statements for stack errors,
*                     (Bug C23).
*
*  ARGUMENTS
*  ---------
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates in WC
*     INP IFILSC FILL AREA scale factor
*     INP LINSUB Device driver polyline output routine
*
      INTEGER  NRD, IFILSC
      REAL     RX(NRD),RY(NRD)
      EXTERNAL LINSUB
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
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
*     IRXB   Stack bases of rotated vertex coords
*     IRYB   Stack bases of rotated vertex coords
*     XMIN   Clipping rectangle
*     XMAX   Clipping rectangle
*     YMIN   Clipping rectangle
*     YMAX   Clipping rectangle
*     MXSTYL Maximum number of hatch styles
*     MXFAM  Maximum number of hatch families per style
*     IHAFAM Number of line families for each hatch style
*     IHASP  Hatch fill spacing
*     IHAISP Hatch fill initial spacing
*     IHASTY List element of hatch styles
*     HAANG  Hatch angle
*
      INTEGER    MXSTYL,    MXFAM
      PARAMETER (MXSTYL=10, MXFAM=4)
      INTEGER ILOY, IHIY, IETI, IDCX,IDCY,
     :        IETMAX, IETXB, IETYB, IETDXB, IETEYB, IETNXB,
     :        IRXB, IRYB, I, IY,
     :        IHAFAM(1:MXSTYL), IHASP(1:MXFAM,1:MXSTYL),
     :        IHAISP(1:MXFAM,1:MXSTYL), IHASTY
      REAL RETLOY, RETHIY, XMIN, XMAX, YMIN, YMAX, HAANG(MXFAM,MXSTYL)
*
*  EXTERNALS
*  ---------
*
      EXTERNAL GKTILT,GKXLIN,GKPMAP

      DATA IHAFAM/1,1,1,1,2,2,3,4,4,3/
      DATA IHASP/8,0,0,0, 8,0,0,0, 8,0,0,0, 8,0,0,0, 8,8,0,0, 8,8,0,0,
     :           13,13,13,0, 20,20,20,20, 20,20,20,20, 14,14,14,0/
      DATA IHAISP/0,0,0,0, 0,0,0,0, 0,0,0,0, 5,0,0,0, 0,0,0,0,
     :            0,5,0,0, 0,0,0,0, 0,5,0,5, 0,5,0,5, 7,7,7,0/
      DATA HAANG/0.0,0.0,0.0,0.0, 1.570796,0.0,0.0,0.0,
     :           0.785398,0.0,0.0,0.0, 2.356194,0.0,0.0,0.0,
     :           0.0,1.570796,0.0,0.0, 0.785398,2.356194,0.0,0.0,
     :           0.0,1.047198,2.094395,0.0, 0.0,0.0,1.570796,1.570796,
     :           0.785398,0.785398,2.356194,2.356194,
     :           0.0,1.047198,2.094395,0.0/
*
*  STACK USAGE
*  -----------
*     -for conversion from WC to DC
*       REAL workspace of size NRD starting at IDCX
*       REAL workspace of size NRD starting at IDCY
*
*     -for scan-conversion Edge Table:
*       REAL workspace of size NRD+1 starting at IETXB
*       REAL workspace of size NRD+1 starting at IETYB
*       REAL workspace of size NRD+1 starting at IETDXB
*       REAL workspace of size NRD+1 starting at IETEYB
*       INT  workspace of size NRD+1 starting at IETNXB
*
*     -for HATCH vertex rotation:
*       REAL workspace of size NRD starting at IRXB
*       REAL workspace of size NRD starting at IRYB
*
*
*  ALGORITHM
*  ---------
*     Sufficient blocks of workspace are acquired to accommodate
*     the Edge Table arrays for the supplied polygon.
*
*     Additional workspace is acquired, into which the vertex
*     coordinates may be rotated.
*
*     The Hatch details appropriate to the current Interior
*     style, are derived:
*
*     For each Hatch angle:
*
*         The supplied vertices are rotated
*
*         GKMET is invoked to construct the Edge Table at
*         this angle
*
*         GKSCAN is invoked with instructions to call
*         GKROTV with each output line. GKROTV, in turn,
*         is passed the supplied LINSUB with which to output
*         the final, unrotated result.
*
*     At the end of the day all work-space acquisitions are released.
*
*  ERRORS
*  ------
*     301  Not enough stack available
*
*  COMMENTS
*  --------
*     Fill area style is ignored and assumed to be hatched.
*
*---------------------------------------------------------------------
*   transform vertices to DC
      CALL GKSTAL(KREALS,NRD,IDCX)
      IF (KERROR.NE.0) GOTO 999
      CALL GKSTAL(KREALS,NRD,IDCY)
      IF (KERROR.NE.0) GOTO 998
      CALL GKTWD (NRD,RX,RY,QSTACK(IDCX),QSTACK(IDCY))


* Extract total clip rectangle using W/S ID from Comms Area:
      XMIN=QWCLXL(KWKIX)
      XMAX=QWCLXR(KWKIX)
      YMIN=QWCLYB(KWKIX)
      YMAX=QWCLYT(KWKIX)
*
*   acquire space for Edge Table
      CALL GKSTAL(KREALS,NRD+1,IETXB)
      IF (KERROR.NE.0) GOTO 997
      CALL GKSTAL(KREALS,NRD+1,IETYB)
      IF (KERROR.NE.0) GOTO 989
      CALL GKSTAL(KREALS,NRD+1,IETDXB)
      IF (KERROR.NE.0) GOTO 988
      CALL GKSTAL(KREALS,NRD+1,IETEYB)
      IF (KERROR.NE.0) GOTO 983
      CALL GKSTAL(KINTGS,NRD+1,IETNXB)
      IF (KERROR.NE.0) GOTO 982

*     acquire Workspace for rotated vertices
      CALL GKSTAL(KREALS,NRD,IRXB)
      IF (KERROR.NE.0) GOTO 981
      CALL GKSTAL(KREALS,NRD,IRYB)
      IF (KERROR.NE.0) GOTO 979

      IHASTY = -KWFASI(KWKIX)
      DO 30 I=1,IHAFAM(IHASTY)
         CALL GKROTV(NRD,QSTACK(IDCX),QSTACK(IDCY),HAANG(I,IHASTY),
     :               QSTACK(IRXB),QSTACK(IRYB))
*  (leaves rotation coefficients set for GKTILT in GKYGCA QWR7 & 8)
*
         CALL GKMET(NRD,QSTACK(IRXB),QSTACK(IRYB),RETLOY,
     :                 RETHIY,IETMAX,QSTACK(IETXB),QSTACK(IETYB),
     :                 QSTACK(IETDXB),QSTACK(IETEYB),KSTACK(IETNXB))

         ILOY=IFIX(RETLOY)
         IHIY=IFIX(RETHIY)
         IETI=0
*  now derive the correctly registered initial scanline Y
         IY=IHIY-MOD((IHIY-IHAISP(I,IHASTY)*IFILSC),
     :               IHASP(I,IHASTY)*IFILSC)
   20    CONTINUE
*  call SCAN with instructions to call TILT for each output span
            CALL GKSCAN(IY+IHASP(I,IHASTY)*IFILSC,IY,IETI,
     :           IETMAX,QSTACK(IETXB),QSTACK(IETYB),
     :           QSTACK(IETDXB),QSTACK(IETEYB),KSTACK(IETNXB),
     :           XMIN,XMAX,YMIN,YMAX,GKTILT,LINSUB)
*  (uses Rotation components out of GKYGCA QWR7 & 8)

            IY=IY-IHASP(I,IHASTY)*IFILSC
         IF (IY.GE.ILOY)  GOTO 20
*        End Repeat
   30 CONTINUE

*  Release Workspace
  978 CALL GKSTDA(KREALS,IRYB)
  979 CALL GKSTDA(KREALS,IRXB)
  981 CALL GKSTDA(KINTGS,IETNXB)
  982 CALL GKSTDA(KREALS,IETEYB)
  983 CALL GKSTDA(KREALS,IETDXB)
  988 CALL GKSTDA(KREALS,IETYB)
  989 CALL GKSTDA(KREALS,IETXB)
  997 CALL GKSTDA(KREALS,IDCY)
  998 CALL GKSTDA(KREALS,IDCX)
  999 CONTINUE

      END
