C# IL>=a, OL>=1
      SUBROUTINE GKPWCN(XDC,YDC,STRAN,XWKTOL,XWKHFD,
     :                     DIST,SGBOX,IDPICK,IDPRIM,IDTYPE)
*
* (C) COPYRIGHT ICL & SERC  1985
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Workstation utility
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To scan current segment contents for a hit.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC   Original version stabilised.
*     21/01/87  ARG   IS conversion. Error numbers changed. Debugging
*                     comments removed.
*
*  ARGUMENTS
*  ---------
*     INP  XDC     - Pick point X
*     INP  YDC     - Pick point Y
*     INP  STRAN   - Segment transformation
*     INP  XWKTOL  - Routine to obtain hit tolerances for primitives
*     INP  XWKHFD  - Routine to obtain hardware text details
*     OUT  DIST    - Hit distance
*     OUT  SGBOX   - Segment extent
*     OUT  IDPICK  - Pick identifier
*     OUT  IDPRIM  - Primitive identifier
*     OUT  IDTYPE  - Primitive type
*
      REAL     XDC,YDC
      REAL     STRAN(6)
      EXTERNAL XWKTOL,XWKHFD
      REAL     DIST,SGBOX(4)
      INTEGER  IDPICK,IDPRIM,IDTYPE
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /STK/    Arrays from CSS return on the stack. This routine
*                     deallocates it.
*     Read   /CCA/    Individual variables from CSS return in this
*                     CSS Communication Area.
*     Read   /SL/     Clipping indicator and current viewports
*     Modify /WCA/    Individual variables from CSS are put here.
*     Modify /ERR/    KERROR
*
      INCLUDE '../include/gaspct.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkpca.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkcca.cmn'
      INCLUDE '../include/gkfab.cmn'
      INCLUDE '../include/gkpca.cmn'
      INCLUDE '../include/gkplb.cmn'
      INCLUDE '../include/gkpmb.cmn'
      INCLUDE '../include/gktxb.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     ICHUNK  Size of stack chunk
*     ICASE   Derived from ITEM; used for computed GOTO.
*     IHOLD   Local copy of KERROR.
*     ITEM    Type of item returned from CSS
*     NR      Number of REALs returned from CSS
*     NI      Number of INTEGERs returned from CSS
*     NC      Number of CHARACTER*80's returned from CSS
*     IPR     Stack pointers for array of reals (x & y)
*     IPI     Stack pointer for integers
*     IPC     Stack pointer for CHARACTER*80 information
*     MORE    Indicates whether there is more of the current item
*             not yet returned by CSS (values GMORE and GNMORE)
*     I,J     Loop counters
*     N       Number of points in chunk
*     IOFF    Stack pointer for primitive data
*     SLICE   .TRUE. if primitive is sliced
*     TMIN    Hit tolerance minimum
*     TMAX    Hit tolerance maximum
*     ICPICK  Pick identifier - current
*     ICPRIM  Primitive identifier - current
*     RCDIST  Hit distance for current primitive
*
      INTEGER    ICHUNK
      PARAMETER (ICHUNK=200)
      INTEGER ICASE,IHOLD,ITEM,NR,NI,NC,IPR,IPI,IPC,MORE
      INTEGER I,J,N,IOFF,ICPICK,ICPRIM
      REAL    TMIN,TMAX,RCDIST
      LOGICAL SLICE
*
*  EXTERNALS
*  ---------
*     GKPWHL  Scan polyline
*     GKPWHP  Scan polygon
*     GKPWHD  Scan raster (stub)
*
      EXTERNAL GKPWHL,GKPWHP,GKPWHD
*
*  STACK USAGE
*  -----------
*     2*NR     REAL     - used by CSS to return (x & y)
*     1*NI     INTEGER  - used by CSS to return 1 array of INTEGER info
*     1*NC     CHARACTER*80  - unused - no CHARACTER stack yet
*     2*8      REAL     - text extent and cell array points
*     2*ICHUNK REAL     - polyline and polymarker points
*
*  ERRORS
*  ------
*       301  Storage overflow in segment storage
*     -2004  Bug in parameters of internal routine
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

* -------------------------------------------------------------------
* Initialise data
* -------------------------------------------------------------------
      IDPICK=0
      IDPRIM=0
      DIST=1.0
      SGBOX(1)=-1.0
      ICPICK=0
      ICPRIM=0
      SLICE=.FALSE.

* -------------------------------------------------------------------
* Start of loop
* -------------------------------------------------------------------
 100  CONTINUE

* Reset local stack data pointer
      IOFF=KNIL

* Read next or remainder of current item from CSS
      CALL GKCSRD(ITEM, NR,NI,NC, IPR,IPI,IPC, MORE)
      IF(KERROR.NE.0) GOTO 888

* Primitives can be sliced over calls
      IF(MORE.EQ.GMORE .AND.
     :   .NOT. (ITEM.EQ.KPL .OR. ITEM.EQ.KPM .OR.
     :          ITEM.EQ.KCA)) THEN
        KERROR = 301
        GOTO 888
      ENDIF

* Branch in groups
      IF(KPL.LE.ITEM .AND. ITEM.LE.KGDP) THEN
*       primitives
        IF(.NOT. SLICE) THEN
          ICPRIM=ICPRIM+1
          SLICE=MORE.EQ.GMORE
        ENDIF
        ICASE=ITEM-KPL+1
*            PL  PM  TX  FA  CA  GDP
        GOTO(210,220,230,240,250,260) ICASE
      ELSEIF(KSPLA.LE.ITEM .AND. ITEM.LE.KSFAA) THEN
*       attributes
        ICASE=ITEM-KSPLA+1
*            PLA PMA TXA FAA
        GOTO(310,320,330,340) ICASE
      ELSEIF(ITEM.EQ.KSPKID) THEN
*       set pick identifier
        GOTO 410
      ELSEIF(ITEM.EQ.KNT) THEN
*       normalisation transformation
        GOTO 510
*     end of segment
      ELSEIF(ITEM.EQ.KENSG) THEN
        GOTO 888
      ENDIF
      GOTO 970

* -------------------------------------------------------------------
* Polyline
* -------------------------------------------------------------------
 210  CONTINUE
*     Data expected:
*     NR     : Number of points
*     IPR    : Coordinates stack pointer

      IF(NR.LT.2) GOTO 970
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF(KERROR.NE.0) GOTO 888
      CALL XWKTOL(KPL,TMIN,TMAX)
      CALL GKPWCR(XDC,YDC,TMIN,TMAX)
      N = ICHUNK
      DO 212 I=1,NR,ICHUNK-1
        IF(NR-I.LT.ICHUNK-1) N = NR-I + 1
        IF (N.EQ.1) GOTO 212
        CALL GKTWD(N,QSTACK(IPR),QSTACK(IPR+NR),
     :                QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
        CALL GKPWHL(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
 212  CONTINUE
      GOTO 777

* -------------------------------------------------------------------
* Polymarker
* -------------------------------------------------------------------
 220  CONTINUE
*     Data expected:
*     NR     : Number of points
*     IPR    : Coordinates stack pointer

      IF(NR.LT.1) GOTO 970
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF(KERROR.NE.0) GOTO 888
      CALL XWKTOL(KPM,TMIN,TMAX)
      CALL GKPWCR(XDC,YDC,TMIN,TMAX)
      N = ICHUNK
      DO 222 I=1,NR,ICHUNK-1
        IF(NR-I.LT.ICHUNK-1) N = NR-I + 1
        CALL GKTWD(N,QSTACK(IPR),QSTACK(IPR+NR),
     :                QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
        CALL GKPWHM(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
 222  CONTINUE
 224  CONTINUE
      GOTO 777

* -------------------------------------------------------------------
* Text
* -------------------------------------------------------------------
 230  CONTINUE
*     Data expected:
*     NI    : Number of characters
*     IPI   : ASCII integer text stack pointer
*     QSS1  : X coordinate of text
*     QSS2  : Y coordinate of text

      IF(NI.LT.1) GOTO 970
      CALL GKSTAL(KREALS,16,IOFF)
      IF(KERROR.NE.0) GOTO 888

* Text position
      QWR1=QSS1
      QWR2=QSS2
* Width vector
      QWR3=QSCHWX
      QWR4=QSCHWY
* Height vector
      QWR5=QSCHHX
      QWR6=QSCHHY


* Get tolerances from workstation and store
      CALL XWKTOL(KTX,TMIN,TMAX)
      CALL GKPWCR(XDC,YDC,TMIN,TMAX)

* Invoke utility appropriate for text precision
      IF(KWTXPR(KWKIX) .EQ. GSTRKP) THEN
        CALL GKXQXO(NI,KSTACK(IPI),QSTACK(IOFF),QSTACK(IOFF+4))
      ELSE
*       baseline vector from ws set text attributes entry
        CALL GKXQXC(NI,KSTACK(IPI),QWCHRX(KWKIX),QWCHRY(KWKIX),
     :                 QSTACK(IOFF),QSTACK(IOFF+4),XWKHFD)
      ENDIF

* Transform text extent
      CALL GKTWD(4,QSTACK(IOFF),QSTACK(IOFF+4),
     :              QSTACK(IOFF+8),QSTACK(IOFF+12))
      CALL GKPWHP(4,QSTACK(IOFF+8),QSTACK(IOFF+12))
      GOTO 777

* -------------------------------------------------------------------
* Fill area
* -------------------------------------------------------------------
 240  CONTINUE
*     Data expected:
*     NR     : Number of points
*     IPR    : Coordinates stack pointer

      IF(NR.LT.3) GOTO 970
* Get tolerances from workstation and store
      CALL XWKTOL(KFA,TMIN,TMAX)
      CALL GKPWCR(XDC,YDC,TMIN,TMAX)

* Use fill utility to generate hollow polygons
      KWFAIS(KWKIX)=GHOLLO
      CALL GKFILL(NR,QSTACK(IPR),QSTACK(IPR+NR),
     :               GKPWHP,GKPWHD)
      GOTO 777

* -------------------------------------------------------------------
* Cell array
* -------------------------------------------------------------------
 250  CONTINUE
*     Data expected:
*     NI     : Number of cells
*     IPI    : Array of colour indexes stack pointer
*     KSS1   : size of colour index array (DX)
*     KSS2   : size of colour index array (DY)
*     KSS3   : dimension of colour index array (IDIMX)
*     QSS1   : X coordinate of point P
*     QSS2   : Y coordinate of point P
*     QSS3   : X coordinate of point Q
*     QSS4   : Y coordinate of point Q
*     QSS5   : X coordinate of point R
*     QSS6   : Y coordinate of point R

      IF(NI.LT.1) GOTO 970
      CALL GKSTAL(KREALS,16,IOFF)
      IF(KERROR.NE.0) GOTO 888
      QSTACK(IOFF)=QSS1
      QSTACK(IOFF+1)=QSS3
      QSTACK(IOFF+2)=QSS5
      QSTACK(IOFF+3)=QSS1+QSS3-QSS5
      QSTACK(IOFF+4)=QSS2
      QSTACK(IOFF+5)=QSS4
      QSTACK(IOFF+6)=QSS6
      QSTACK(IOFF+7)=QSS2+QSS4-QSS6

* Get tolerances from workstation and store
      CALL XWKTOL(KTX,TMIN,TMAX)
      CALL GKPWCR(XDC,YDC,TMIN,TMAX)

* Transform cell corners
      CALL GKTWD(4,QSTACK(IOFF),QSTACK(IOFF+4),
     :              QSTACK(IOFF+8),QSTACK(IOFF+12))
      CALL GKPWHP(4,QSTACK(IOFF+8),QSTACK(IOFF+12))
      GOTO 777


* -------------------------------------------------------------------
* GDP
* -------------------------------------------------------------------
 260  CONTINUE
*     Data expected:
*     NR     : Number of points
*     IPR    : Coordinates stack pointer
*     NC     : Number of characters
*     IPC    : CHaracter data record stack pointer
*     KSS1   : GDP identifier
*     QSS1   : X coordinate of point P
*     QSS2   : X coordinate of point Q
*     QSS3   : X coordinate of point R
*     QSS4   : Y coordinate of point P
*     QSS5   : Y coordinate of point Q
*     QSS6   : Y coordinate of point R

      IF( NR.LT.1 ) GOTO 970
      QWR1=QSS1
      QWR2=QSS2
      QWR3=QSS3
      QWR4=QSS4
      QWR5=QSS5
      QWR6=QSS6
      KWI1=KSS1

* Get tolerances from workstation and store
      CALL XWKTOL(KGDP,TMIN,TMAX)
      CALL GKPWCR(XDC,YDC,TMIN,TMAX)

      CALL GKPWHG(NR,NI,NC,IPR,IPI,IPC)
      GOTO 777


* -------------------------------------------------------------------
* Polyline attributes
* -------------------------------------------------------------------
 310  CONTINUE
* Find polyline bundle for this index & workstation
      DO 312 I=1,KMXPLB
        IF(KPLI(I,KWKIX) .EQ. KSPLI) GOTO 316
        IF(KPLI(I,KWKIX) .EQ. -1) GOTO 314
 312  CONTINUE
*     Not found: use bundle 1
 314  CONTINUE
      I = 1
 316  CONTINUE

* Fetch from bundle or communication area depending on aspect flag
      IF (KSPLAF(1) .EQ. GBUNDL) THEN
         KWLNTY(KWKIX) = KLNTY(I,KWKIX)
      ELSE
         KWLNTY(KWKIX) = KSLNTY
      ENDIF
      IF (KSPLAF(2) .EQ. GBUNDL) THEN
         QWLNWD(KWKIX) = QLNWD(I,KWKIX)
      ELSE
         QWLNWD(KWKIX) = QSLNWD
      ENDIF
      GOTO 888

* -------------------------------------------------------------------
* Polymarker attributes
* -------------------------------------------------------------------
 320  CONTINUE
* Find polymarker bundle for this index & workstation
      DO 322 I=1, KMXPMB
        IF(KPMI(I,KWKIX) .EQ. KSPMI) GOTO 326
        IF(KPMI(I,KWKIX) .EQ. -1) GOTO 324
 322  CONTINUE
*     Not found: use bundle 1
 324  CONTINUE
      I = 1
 326  CONTINUE

* Fetch from bundle or communication area depending on aspect flag
      IF(KSPMAF(1) .EQ. GBUNDL) THEN
         KWMKTY(KWKIX) = KMKTY(I,KWKIX)
      ELSE
         KWMKTY(KWKIX) = KSMKTY
      ENDIF
      IF(KSPMAF(2) .EQ. GBUNDL) THEN
         QWMKSZ(KWKIX) = QMKSZ(I,KWKIX)
      ELSE
         QWMKSZ(KWKIX) = QSMKSZ
      ENDIF
      GOTO 888

* -------------------------------------------------------------------
* Text attributes
* -------------------------------------------------------------------
 330  CONTINUE

* Find text bundle for this index and workstation
      DO 332 I=1,KMXTXB
        IF(KTXI(I,KWKIX) .EQ. KSTXI) GOTO 336
        IF(KTXI(I,KWKIX) .EQ. KNIL) GOTO 334
 332  CONTINUE
* Not found: use bundle 1
 334  CONTINUE
      I = 1
 336  CONTINUE

* Fetch from bundle or communication area depending on aspect flag
      IF(KSTXAF(KTXFNA) .EQ. GBUNDL) THEN
        KWTXFN(KWKIX) = KTXFN(I,KWKIX)
        KWTXPR(KWKIX) = KTXPR(I,KWKIX)
      ELSE
        KWTXFN(KWKIX) = KSTXFN
        KWTXPR(KWKIX) = KSTXPR
      ENDIF

* Fetch character expansion factor
      IF(KSTXAF(KCHXPA) .EQ. GBUNDL) THEN
        QWCHXP(KWKIX) = QCHXP(I,KWKIX)
      ELSE
        QWCHXP(KWKIX) = QSCHXP
      ENDIF

* Similarly for character spacing
      IF (KSTXAF(KCHSPA) .EQ. GBUNDL) THEN
        QWCHSP(KWKIX) = QCHSP(I,KWKIX)
      ELSE
        QWCHSP(KWKIX) = QSCHSP
      ENDIF

* Transfer text path
      KWTXP(KWKIX) = KSTXP

* Transfer text alignment and resolve 'normal'
      KWHTXA(KWKIX) = KSHTXA
      KWVTXA(KWKIX) = KSVTXA
      IF (KSHTXA .EQ. GAHNOR) THEN
        IF(KWTXP(KWKIX).EQ.GRIGHT) THEN
          KWHTXA(KWKIX) = GALEFT
        ELSEIF(KWTXP(KWKIX).EQ.GLEFT) THEN
          KWHTXA(KWKIX) = GARITE
        ELSE
          KWHTXA(KWKIX) = GACENT
        ENDIF
      ENDIF
      IF(KSVTXA .EQ. GAVNOR) THEN
        IF(KWTXP(KWKIX).EQ.GDOWN) THEN
          KWVTXA(KWKIX) = GATOP
        ELSE
          KWVTXA(KWKIX) = GABASE
        ENDIF
      ENDIF

* Transform width and height vectors
      CALL GKTWDV(QSCHWX,QSCHWY,QWCHWX(KWKIX),QWCHWY(KWKIX))
      CALL GKTWDV(QSCHHX,QSCHHY,QWCHHX(KWKIX),QWCHHY(KWKIX))

* Set rotation vector from width vector
      QWCHRX(KWKIX)=QWCHWX(KWKIX)
      QWCHRY(KWKIX)=QWCHWY(KWKIX)

* Let font index be the font number, workstation can reset it.
      KWTXFI(KWKIX) = KWTXFN(KWKIX)
      GOTO 888

* -------------------------------------------------------------------
* Fill area attributes
* -------------------------------------------------------------------
 340  CONTINUE
      GOTO 888

* -------------------------------------------------------------------
* Set pick identifier
* -------------------------------------------------------------------
 410  CONTINUE
      ICPICK=KSS1
      GOTO 888

* -------------------------------------------------------------------
* Normalisation transformation
* -------------------------------------------------------------------
 510  CONTINUE

* Clipping rectangle
      QWR7=QSS7
      QWR8=QSS8
      QWR9=QSS9
      QWR10=QSS10

* TransformationC2
      QWR1=1.0
      QWR2=0.0
      QWR3=0.0
      QWR4=0.0
      QWR5=1.0
      QWR6=0.0

* Segment transformation
      DO 512 J=1,6
        QWRA(10+J)=STRAN(J)
 512  CONTINUE

* Update total transformation on workstation
      CALL GKWKC4
      GOTO 888

* -------------------------------------------------------------------
* End primitive
* -------------------------------------------------------------------
 777  CONTINUE
* Get hit distance and update segment bounding box
      CALL GKPWCG(RCDIST,SGBOX)

* Record details if exact or closer than current hit
      IF(DIST.GT.0.0 .AND. RCDIST.LT.1.0 .AND. RCDIST.LT.DIST) THEN

        DIST=RCDIST
        IDPICK=ICPICK
        IDPRIM=ICPRIM
        IDTYPE=ITEM
      ENDIF
      GOTO 888

* -------------------------------------------------------------------
* End of loop
* -------------------------------------------------------------------
 888  CONTINUE
* Here to deallocate stack. We pass through here whether there was
* was error or not.
      IHOLD=KERROR
      CALL GKSTDA(KREALS,IOFF)
* Deallocate stack in reverse order to the parameters returning
* from GKCSRD.
      CALL GKSTDA(KCHARS,IPC)
      CALL GKSTDA(KINTGS,IPI)
      CALL GKSTDA(KREALS,IPR)
      IF(IHOLD.NE.0) KERROR=IHOLD
* Unlock segment and finish if error or exact hit
      IF(KERROR.NE.0 .OR.
     :   (KPKECO.EQ.KPSCAN .AND. DIST.LT.0.0)) GOTO 960
* Finish if end of segment (deselect already done by CSS)
      IF(ITEM.EQ.KENSG) GOTO 999
* Return to loop
      GOTO 100


* -------------------------------------------------------------------
* Deselect segment if error before end
* -------------------------------------------------------------------
 960  CONTINUE
      IHOLD=KERROR
      CALL GKCSRL
      KERROR=IHOLD
      GOTO 999

* -------------------------------------------------------------------
* Bug in data returned by GKCSRD
* -------------------------------------------------------------------
  970 CALL GKBUG (-2004,'GKPWCN')

*
  999 CONTINUE
      END
