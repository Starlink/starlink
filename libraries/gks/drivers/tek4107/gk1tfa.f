

      SUBROUTINE GK1TFA(NRD,RX,RY,ICOL,LINSUB)
*
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             NGB
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Fill Area Utility fills supplies polygon with solid fill
*     on Tek4107
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
*     21/02/85  GGT   GKFILL modified to provide solid fill on
*                     Tek 4107
*
*  ARGUMENTS
*  ---------
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates
*     INP ICOL   Fill area colour index
*     INP LINSUB Device driver polyline output routine
*
      INTEGER NRD,ICOL
      REAL    RX(NRD),RY(NRD)
      EXTERNAL LINSUB
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     IXB    Stack base of clipped polygon vertices
*     IYB    Stack base of clipped polygon vertices
*     IPB    Stack base of clipped polygon list
*     IP     Polygon clipping controls
*     IPOLY  Polygon clipping controls
*     IFIRST Polygon clipping controls
*     ILAST  Polygon clipping controls
*     ITXB   Stack bases of transformed vertex coords
*     ITYB   Stack bases of transformed vertex coords
*     RLINEX Endpoint Coords for 2-point polyline
*     RLINEY Endpoint Coords for 2-point polyline
*     XMIN   Clipping rectangle
*     XMAX   Clipping rectangle
*     YMIN   Clipping rectangle
*     YMAX   Clipping rectangle

      INTEGER IXB, IYB, IPB, IP, IPOLY, IFIRST, ILAST, ITXB, ITYB,
     :        N
      INTEGER NB,NLEFT,SETC(5),BEGIN(9),END(3)
      INTEGER IX,IY,HIX,HIY,LOX,LOY,IEB,I,J,JCOL
      REAL RLINEX(2), RLINEY(2), XMIN, XMAX, YMIN, YMAX
      REAL PTORX,PTORY
      PARAMETER (PTORX=6.0,PTORY=6.0)
      DATA SETC  /27,77,80,0,0/
      DATA BEGIN /27,76,80,0,0,0,0,0,48/
      DATA END   /27,76,69/
*
*  EXTERNALS
*  ---------
*
*
*
*  STACK USAGE
*  -----------
*     -for transformation to DC:
*       REAL workspace of size NRD starting at ITXB
*       REAL workspace of size NRD starting at ITYB
*
*     -for PolyClip algorithm:
*       REAL workspace of size NRD*2 starting at IXB
*       REAL workspace of size NRD*2 starting at IYB
*       INT  workspace of size NRD/2 starting at IPB
*       (total INT+REAL requirement, including workspace acquired
*        by GKCLIP, amounts to NRD*12 )
*
*
*  ALGORITHM
*  ---------
*     The polygon vertices are first transformed into DC
*
*     For Fill-style SOLID:
*
*         Workspace is acquired, sufficient for the maximum possible
*         final no of (clip-induced) vertices, and fragmented polygons.
*
*         GPCLP is invoked to clip the polygon.
*
*         For each resultant polygon, the supplied LINSUB polyline
*         output routine is passed the appropriate section of the
*         clipped vertex-list. It is called again, with the first
*         and last points, to close the polygon.
*
*
*     At the end of the day all possible work-space acquisitions are
*     released, whether used or not
*
*  ERRORS
*  ------
*     301  Not enough stack available
*
*---------------------------------------------------------------------



* Extract total clip rectangle using W/S ID from Comms Area:
      XMIN=QWCLXL(KWKIX)
      XMAX=QWCLXR(KWKIX)
      YMIN=QWCLYB(KWKIX)
      YMAX=QWCLYT(KWKIX)

* pre-set stack pointers for tidy collapse
      ITXB   = KNIL
      ITYB   = KNIL
      IXB    = KNIL
      IYB    = KNIL
      IPB    = KNIL


* Transform vertices
      CALL GKSTAL(KREALS,NRD,ITXB)
      IF (KERROR.NE.0) GOTO 99
      CALL GKSTAL(KREALS,NRD,ITYB)
      IF (KERROR.NE.0) GOTO 99

      CALL GKTWD (NRD,RX,RY,QSTACK(ITXB),QSTACK(ITYB))


      IF (KWFAIS(KWKIX) .EQ. GSOLID) THEN
* Fill-Style Solid: acquire stack for clipped polygon arrays

         CALL GKSTAL(KREALS,NRD*2,IXB)
         IF (KERROR.NE.0) GOTO 99
         CALL GKSTAL(KREALS,NRD*2,IYB)
         IF (KERROR.NE.0) GOTO 99
         CALL GKSTAL(KINTGS,NRD/2,IPB)
         IF (KERROR.NE.0) GOTO 99

         IPOLY=1
         KSTACK(IPB)=NRD
* invoke clipping utility giving it a single polygon to work on
         CALL GKPCLP(IPOLY,NRD,KSTACK(IPB),QSTACK(ITXB),QSTACK(ITYB),
     :                XMIN,XMAX,YMIN,YMAX,QSTACK(IXB),QSTACK(IYB))
* -returns IPOLY polygons, with vertices (for polygon IP=1,IPOLY)
*    running from KSTACK(IPB+IP-2)+1 to KSTACK(IPB+IP-1)
* (except for IP=1, which has vertices 1 to KSTACK(IPB)  )
*** Note: CLIP may set KERROR if insufficient Workspace ***
         IF (KERROR.NE.0) GOTO 99

         IF (IPOLY .GT. 0) THEN
*        Set colour index
          JCOL=-ICOL
          NB=4
          CALL GK1TTI(JCOL,SETC,NB)
          CALL GKIOBO(KIOPB,NB-1,SETC,NLEFT)
            IFIRST=1
            DO 10 IP=1,IPOLY
*           Set up the panel fill command
             IX=INT(QSTACK(IXB+IFIRST-1)*PTORX+0.5)
             IY=INT(QSTACK(IYB+IFIRST-1)*PTORY+0.5)
             HIX=IX/128
             HIY=IY/128
             LOX=IX-HIX*128
             LOY=IY-HIY*128
             I=LOX
             J=LOY
             LOX=LOX/4
             LOY=LOY/4
             IEB=4*(J-LOY*4)+(I-LOX*4)
             BEGIN(4)=HIY + 32
             BEGIN(5)=IEB + 96
             BEGIN(6)=LOY + 96
             BEGIN(7)=HIX + 32
             BEGIN(8)=LOX + 64
             CALL GKIOBO(KIOPB,9,BEGIN,NLEFT)
*
               ILAST=KSTACK(IPB+IP-1)
               N=ILAST-IFIRST+1
             CALL LINSUB(N,QSTACK(IXB+IFIRST-1),QSTACK(IYB+IFIRST-1))
*              now join first to last
             RLINEX(1)=QSTACK(IXB+ILAST-1)
             RLINEY(1)=QSTACK(IYB+ILAST-1)
             RLINEX(2)=QSTACK(IXB+IFIRST-1)
             RLINEY(2)=QSTACK(IYB+IFIRST-1)
             CALL LINSUB(2,RLINEX,RLINEY)

               IFIRST=ILAST+1
   10       CONTINUE
*           End panel
          CALL GKIOBO(KIOPB,3,END,NLEFT)
         ENDIF
*     ...non-degenerate polygons available
      ENDIF

* Release Workspace
   99 CONTINUE
      CALL GKSTDA(KINTGS,IPB)
      CALL GKSTDA(KREALS,IYB)
      CALL GKSTDA(KREALS,IXB)
      CALL GKSTDA(KREALS,ITYB)
      CALL GKSTDA(KREALS,ITXB)
      END
