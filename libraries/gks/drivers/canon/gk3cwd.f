      SUBROUTINE GK3CWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             DLT
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Workstation driver for CANON LBP-8 II and A2 laser printer
*
*  MAINTENANCE LOG
*  ---------------
*
*  23 JUL 1986  DLT  Corrected spelling of CANON
*                    Revised reset logic to avoid blank pages
*  05 SEP 1986  PTW  Fixed stripes in cell array
*  04 NOV 1986  DLT  Removed input related code
*  28 JUN 1987  DLT  Fixed colour table inquiry bugs
*  30 JUN 1987  DLT  Fixed set colour representation
*  01 JUL 1987  DLT  Set printer to portrait mode explicitly
*  04 AUG 1987  DLT  Fix text facilities inquiry
*  04 AUG 1987  DLT  Use landscape mode
*  21 AUG 1987  PTW  New greyscale mask
*  04 MAR 1988  DLT  Reverted to using portrait mode because of A2 bugs
*  11 AUG 1989  DLT  Convert to IS and general tidy up
*                    Merge with TeX insertion driver
*
*  ARGUMENTS
*  ---------
*     INP IENT  - Entrypoint code
*     INP NID   - Size of array IDAT
*     I/0 IDAT  - Integer data passed to or from workstation
*     INP NRD   - Size of arrays RX and RY
*     I/O RX    - Real X-coordinate data passed to or from workstation
*     I/O RY    - Real Y-coordinate data passed to or from workstation
*     INP NCD   - Size of character array
*     I/O STR   - Character array
*
      INTEGER IENT, NID, IDAT(NID), NRD, NCD
      REAL RX(NRD), RY(NRD)
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwke.par'
      INCLUDE '../../include/gwksgl.par'
      INCLUDE '../../include/gkinp.par'
      INCLUDE '../../include/gkfls.par'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkcon.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkfls.cmn'
      INCLUDE '../../include/gkxfd.cmn'
      INCLUDE '../../include/gkerr.cmn'

*
*  EXTERNALS
*  ---------
*
      EXTERNAL GK3CLN, GK3CRO
*
*  LOCALS
*  ------
*     IPREC       Precision array for Text;
*                 ***** Note the local precision array will
*                 ***** disappear when there is a decent utility to
*                 ***** handle font names and precisions.
*     ICHUNK      Size of stack used for splitting polylines
*     IOFF        Offset for stack
*     NLEFT       Amount of buffer left
*     I           Temporary integer
*     N           Count (temporary)
*     INTA        Local integer array with multiple uses:
*                   - receiving WDT for Inq Text Facil (size 19)
*     R,G,B       R,G,B in GKS form (range 0.0 to 1.0)
*     REALA       Local real array with multiple uses:
*                   - receiving WDT for Inq Text Facil (size 12)
*     SCALE       Converts from metres to device units
*     TEMP        Temporary real for rotating transformation
*     C           Temporary character
*
      INTEGER IPREC(KFNTMX)
      INTEGER    ICHUNK
      PARAMETER (ICHUNK=100)
      INTEGER IOFF, NLEFT, I, N
      CHARACTER*1 C
      INTEGER INTA(19)
      REAL TEMP
      REAL REALA(12)
      REAL SCALE
      PARAMETER (SCALE=11811.0)
*  Offsets into KWKDAT workspace
      INTEGER CT, ROTATE, TEX
      PARAMETER (CT=1, ROTATE=2, TEX=3)
*  End of offsets
      CHARACTER*1 ESC, VDM, IS2
*      PARAMETER (ESC=CHAR(27), IS2=CHAR(30), VDM=CHAR(125))
       ESC=CHAR(27)
       IS2=CHAR(30)
       VDM=CHAR(125)

*
*
*  STACK USAGE
*  -----------
*     POLYLINE and POLYMARKER for transformations
*
*  ERRORS
*  ------
*      32   Specified workstation is not of category MO
*      34   Specified workstation is not of category MI
*      93   Colour index is invalid
*     102   GDP identifier is invalid
*     180   Specified escape function is not supported
*   -1009   Font file not available
*    2002   List element not available
*   -2010   This driver called with wrong workstation type
*
*  COMMENTS
*  --------
*    This driver supports four workstation type; two for generating files
*    that can be set straight to the printer, one landscape and the
*    other portrait orientation and two that generate files that can be
*    inserted into TeX documents with DVICAN. The only difference
*    between the two sorts of file is that the device reset and setup
*    codes are omitted from the TeX files.
*
*    Landscape orientation is produced by rotating with the
*    transformation matrix rather then putting the printer into
*    landscape mode because the model A2 has bugs in in its landscape
*    mode that can put the printer into an infinte loop.
*
*    The landscape TeX workstation produces file for inserting into
*    portrait orientation documents not for inserting into landscape
*    documents.
*
*---------------------------------------------------------------------


* Conditional GOTO on entrypoint code

      GOTO (       10,  20,  30,  40,  50,  60,  70,  80,9999,
     :      9999, 110, 120, 130, 140, 150, 160, 170, 180, 190,
     :       200, 210,9999, 230, 240, 250, 260, 270, 280,9999,
     :      9999, 310, 320, 330,9999,9999,9999,9999,9999,9999,
     :      9999, 410, 410, 410, 410, 410, 410, 410, 410, 410,
     :       410, 410,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999,7777,7777,7777,7777,7777,7777,7777,7777,7777,
     :      7777,7777,7777,7777,7777,7777,7777,7777,7777,7777,
     :      7777,7777,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 910, 920, 930) IENT

      GOTO (1111,1111,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1111,1111,1330,1111,1111,1111,1370,1111,1111,
     :      1111,1410,1420,1111,1440,1111,1460,7777,7777,7777,
     :      7777,7777,7777,9999,9999,9999,9999,9999,9999,9999,
     :      9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1700,1111,1720,1111,1111,1750,1111,1770,1111,1790,
     :      1111,1111,1111,1111,1111,1850,1111,1870,1880,1111,
     :      1111,1111,7777,7777,7777,7777,7777,7777,7777) IENT-119

      GOTO 9999


* Open workstation
   10 CONTINUE

* Set up workstation state list and workstation description table
      CALL GKIWSL(KWKIX,KWKTYP)
      IF (KERROR.NE.0) GOTO 9999
      KCID(KWKIX) = KWI1
      KDFM(KWKIX) = GBNIG
      KWIO(KWKIX) = GNO
      KIMRGM(KWKIX) = GSUPPD

      IF (KWKTYP.EQ.2600) THEN
*   landscape mode normal file
         KWKDAT(ROTATE,KWKIX) = 1
         KWKDAT(TEX,KWKIX) = 0
      ELSEIF (KWKTYP.EQ.2601) THEN
*   portrait mode normal file
         KWKDAT(ROTATE,KWKIX) = 0
         KWKDAT(TEX,KWKIX) = 0
      ELSEIF (KWKTYP.EQ.2610) THEN
*   landscape mode TeX file
         KWKDAT(ROTATE,KWKIX) = 1
         KWKDAT(TEX,KWKIX) = 1
      ELSEIF (KWKTYP.EQ.2611) THEN
*   portrait mode TeX file
         KWKDAT(ROTATE,KWKIX) = 0
         KWKDAT(TEX,KWKIX) = 1
      ELSE
*   wrong workstation type
         KERROR = -2010
         GO TO 9999
      ENDIF

*   Allocate heap space for colour table
      CALL GKHPAL(KPCI(KWKIX),KINTGS,KWKDAT(CT,KWKIX))
      IF (KERROR.NE.0) GO TO 9999

*   create integer representation of colour table
      DO 14,I=0,KPCI(KWKIX)-1
        QWR1 = QHP(KHPXR(KCTBPT(1,KWKIX))+I)
        QWR2 = QHP(KHPXR(KCTBPT(2,KWKIX))+I)
        QWR3 = QHP(KHPXR(KCTBPT(3,KWKIX))+I)
        KHP(KHPXI(KWKDAT(CT,KWKIX))+I) =
     :   NINT( (0.30*QWR1 + 0.59*QWR2 + 0.11*QWR3) * 255.0)
   14 CONTINUE

*   Ask operating system to make a connection
      CALL GKIOOP(KFWKFW,KCID(KWKIX),KWCID(KWKIX))
      IF (KERROR.NE.0) GOTO 9999

*  Reset workstation viewport and clipping rectangle.
      QRWVXR(KWKIX) = QDSDX(KWKIX)
      QRWVYT(KWKIX) = QDSDY(KWKIX)
      QCWVXR(KWKIX) = QDSDX(KWKIX)
      QCWVYT(KWKIX) = QDSDY(KWKIX)

*  initialize buffering
      CALL GKFOCO(KIOIT,' ',NLEFT)
*  initialise device
      CALL GK3CID
      KWI1 = GOUTPT
      GOTO 9999

* Close workstation
   20 CONTINUE
      IF (KWI1.EQ.1) THEN
        KWDONE = KRFUSE
      ELSE
        IF (KDSMT(KWKIX).EQ.GNEMPT) CALL GK3CCL
*  Reset printer if not TeX
        IF (KWKDAT(TEX,KWKIX).EQ.0) CALL GKFOCO(KIOPB,ESC//'=',NLEFT)
*  Flush commands to file
        CALL GKFOCO(KIOSN,' ',NLEFT)
*  Deallocate heap
        CALL GKHPDA(KWKDAT(CT,KWKIX),KINTGS)
*  Close file and disconnect workstation
        CALL GKIOCL(KWKTYP,KCID(KWKIX),KWCID(KWKIX))
        CALL GKCWSL(KWKIX)
        CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      GOTO 9999



* Clear workstation
   30 CONTINUE
      IF( KWI1.EQ.2 ) THEN
        KWDONE=KRFUSE
        CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      GOTO 9999



* Redraw all segments on workstation
   40 CONTINUE
      KWDONE = KRFUSE
      GOTO 9999



* Update workstation
   50 CONTINUE
      KWDONE = KRFUSE
      GOTO 9999



* Set deferral state
   60 CONTINUE
      KDFM(KWKIX) = KWI1
      KIMRGM(KWKIX) = KWI2
      IF (KWI1.EQ.GASAP) THEN
        KWIO(KWKIX) = GYES
      ELSE
        KWIO(KWKIX) = GNO
      ENDIF
      IF (KIMRGM(KWKIX).EQ.GALLOW .AND. KNFAUP(KWKIX).EQ.GYES .AND.
     :    KDSMT(KWKIX).EQ.GNEMPT) THEN
        KWRGN(KWKIX) = .TRUE.
        KRGN = .TRUE.
      ENDIF
      GOTO 9999



* Do deferred output actions
   70 CONTINUE
      KWI1 = KNFAUP(KWKIX)
      GOTO 9999



* Clear display surface
   80 CONTINUE
      IF (KWI1.EQ.GALWAY .OR. KDSMT(KWKIX).EQ.GNEMPT) THEN
         CALL GK3CCL
         IF (KWKDAT(TEX,KWKIX).EQ.1) THEN
            CALL GKFOCO(KIOSN,' ',NLEFT)
            CALL GKIOCL(KWKTYP,KCID(KWKIX),KWCID(KWKIX))
            CALL GKIOOP(KFWKFW,KCID(KWKIX),KWCID(KWKIX))
         END IF
         CALL GK3CID
      ENDIF
      CALL GKWCLD
      GOTO 9999



* Escape
  110 CONTINUE
      KERROR = 180
      GOTO 9999



* Polyline
  120 CONTINUE
      IF ( KCVIS.EQ.GINVIS) GO TO 9999
      IF (KWPLCI(KWKIX).EQ.0) THEN
         CALL GKFOCO(KIOPB, VDM//'G2'//IS2,NLEFT)
      ELSE
         CALL GKFOCO(KIOPB, VDM//'G0'//IS2,NLEFT)
      END IF
*  Set line type and width
      IF (KWLNTY(KWKIX).GT.1) THEN
        C = CHAR(KWLNTY(KWKIX)+47)
        CALL GKFOCO(KIOPB,'E1'//C//IS2,NLEFT)
      END IF
      IF (NINT(QWLNWD(KWKIX)).GT.1) THEN
        C = CHAR(NINT(QWLNWD(KWKIX))+32)
        CALL GKFOCO(KIOPB,'F1'//C//IS2,NLEFT)
      END IF

      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
        N = ICHUNK
        DO 122 I=1,NRD,ICHUNK-1
          IF (NRD-I.LT.ICHUNK-1) N = NRD-I + 1
          IF (N.EQ.1) GOTO 122
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
          CALL GKLCLP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :                   .FALSE.,45.0,
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK3CLN)
  122   CONTINUE
*   Restore line type and width
      IF (KWLNTY(KWKIX).GT.1) CALL GKFOCO(KIOPB, 'E10'//IS2,NLEFT)
      IF (NINT(QWLNWD(KWKIX)).GT.1)
     :                         CALL GKFOCO(KIOPB, 'F1!'//IS2,NLEFT)

        CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888



* Polymarker
  130 CONTINUE
      IF ( KCVIS.EQ.GINVIS) GO TO 9999
      IF (KWPMCI(KWKIX).EQ.0) THEN
         CALL GKFOCO(KIOPB, VDM//'G2'//IS2,NLEFT)
      ELSE
         CALL GKFOCO(KIOPB, VDM//'G0'//IS2,NLEFT)
      END IF
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
        N = ICHUNK
        DO 132 I=1,NRD,ICHUNK
          IF (NRD-I.LT.ICHUNK) N = NRD-I + 1
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
          CALL GKMTYP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :                   KWMKTY(KWKIX),QNMMKS(KWKIX),QWMKSZ(KWKIX),
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK3CLN)
  132   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888



* Text
  140 CONTINUE
      IF ( KCVIS.EQ.GINVIS) GO TO 9999
      IF (KWTXCI(KWKIX).EQ.0) THEN
         CALL GKFOCO(KIOPB, VDM//'G2'//IS2,NLEFT)
      ELSE
         CALL GKFOCO(KIOPB, VDM//'G0'//IS2,NLEFT)
      END IF
      CALL GKXDWO(NID,IDAT,GK3CLN)
      GOTO 8888



* Fill area
  150 CONTINUE
      IF ( KCVIS.EQ.GINVIS) GO TO 9999
      IF (KWFACI(KWKIX).EQ.0) THEN
         CALL GKFOCO(KIOPB, VDM//'G2'//IS2,NLEFT)
      ELSE
         CALL GKFOCO(KIOPB, VDM//'G0'//IS2,NLEFT)
      END IF
      CALL GKFILS(NRD,RX,RY,1,GK3CLN,GK3CRO)
      GOTO 8888



* Cell array
  160 CONTINUE
      IF ( KCVIS.EQ.GINVIS) GO TO 9999
      CALL GKCELL(NID,IDAT,GK3CRO)
      GOTO 8888



* GDP
  170 CONTINUE
      IF ( KCVIS.EQ.GINVIS) GO TO 9999

* First, check GDP identifier
      IF (KWI1.EQ.0) THEN
         KERROR = 102
         GOTO 9999
      ELSE IF (KWI1.LT.-4 .OR. KWI1.GT.-1) THEN
         KERROR = 104
         GOTO 9999
      ELSE IF (KWI1.EQ.-1) THEN
* Arc
         IF (KWPLCI(KWKIX).EQ.0) THEN
            CALL GKFOCO(KIOPB, VDM//'G2'//IS2,NLEFT)
         ELSE
            CALL GKFOCO(KIOPB, VDM//'G0'//IS2,NLEFT)
         END IF
*     Set line type and width
         IF (KWLNTY(KWKIX).GT.1) THEN
           C = CHAR(KWLNTY(KWKIX)+47)
           CALL GKFOCO(KIOPB,'E1'//C//IS2,NLEFT)
         END IF
         IF (NINT(QWLNWD(KWKIX)).GT.1) THEN
           C = CHAR(NINT(QWLNWD(KWKIX))+32)
           CALL GKFOCO(KIOPB,'F1'//C//IS2,NLEFT)
         END IF
         CALL GKCRCS(KWI1,NRD,RX,RY,1,.FALSE.,1.0,GK3CLN,GK3CRO)
*      Restore line type and width
         IF (KWLNTY(KWKIX).GT.1) CALL GKFOCO(KIOPB, 'E10'//IS2,NLEFT)
         IF (NINT(QWLNWD(KWKIX)).GT.1)
     :                            CALL GKFOCO(KIOPB, 'F1!'//IS2,NLEFT)
      ELSE
* Filled chord, pie, circle
         IF (KWFACI(KWKIX).EQ.0) THEN
            CALL GKFOCO(KIOPB, VDM//'G2'//IS2,NLEFT)
         ELSE
            CALL GKFOCO(KIOPB, VDM//'G0'//IS2,NLEFT)
         END IF
         CALL GKCRCS(KWI1,NRD,RX,RY,1,.FALSE.,10.0,GK3CLN,GK3CRO)
      ENDIF
      GOTO 8888


* Set polyline attributes
  180 CONTINUE
      CALL GKDPLB
* Need to check because individual settings won't have been checked.
      IF (KWLNTY(KWKIX).LT.1 .OR. KWLNTY(KWKIX).GT.5) KWLNTY(KWKIX) = 1
      IF (KWPLCI(KWKIX).GE.KPCI(KWKIX)) KWPLCI(KWKIX) = 1
      QWLNWD(KWKIX) = ANINT(MAX(MIN(QWLNWD(KWKIX),4.0),1.0))
      GOTO 9999



* Set polymarker attributes
  190 CONTINUE
      CALL GKDPMB
* Need to check because individual settings won't have been checked.
      IF (KWMKTY(KWKIX).LT.1 .OR. KWMKTY(KWKIX).GT.5) KWMKTY(KWKIX) = 3
      IF (KWPMCI(KWKIX).GE.KPCI(KWKIX)) KWPMCI(KWKIX) = 1
      GOTO 9999



* Set text attributes
  200 CONTINUE
      CALL GKDTXB
* Need to check because individual settings won't have been checked.
      IF (KWTXCI(KWKIX).GE.KPCI(KWKIX)) KWTXCI(KWKIX) = 1
      GOTO 9999



* Set fill area attributes
  210 CONTINUE
      CALL GKDFAB
* Need to check because individual settings won't have been checked.
      IF (KWFAIS(KWKIX).EQ.GHATCH .AND. (KWFASI(KWKIX).LT.-10 .OR.
     :    KWFASI(KWKIX).GT.-1)) KWFASI(KWKIX) = -1
      IF (KWFAIS(KWKIX).EQ.GPATTR .AND. KWFASI(KWKIX).GT.3)
     :    KWFASI(KWKIX) = 1
      IF (KWFACI(KWKIX).GE.KPCI(KWKIX)) KWFACI(KWKIX) = 1
      GOTO 9999



* Set polyline representation
  230 CONTINUE
      INTA(1) = 5
      CALL GKSRPL(1,INTA,.TRUE.)
      GOTO 9999



* Set polymarker representation
  240 CONTINUE
      CALL GKSRPM(0,INTA,.TRUE.)
      GOTO 9999



* Set text representation
  250 CONTINUE
*     Data expected:
*     KWI1   : Text Index
*     KWI2   : Font
*     KWI3   : Precision  ( STRING, CHAR, STROKE )
*     QWR1   : Character Expansion Factor
*     QWR2   : Character Spacing Factor
*     KWI4   : Text Colour Index
*     Data returned:
*     KERROR : error indicator
*       Make sure that fonts are available
      IF( KWI3.EQ.GSTRKP ) THEN

*       Stroke Precision
        IF( KDBFLS.EQ.KFLNA ) THEN
          KERROR=-1009
          GOTO 9999
        ENDIF
        IF( KDBFLS.EQ.KFLCL ) CALL GKXON
        IF( KERROR.NE.0 ) GOTO 9999
        DO 255 I=1,KFNTMX
          IPREC(I) = GSTRKP
255     CONTINUE
        CALL GKSRTX(KFNTMX,KHFONT,IPREC,.FALSE.)
      ELSE

*       String or Char precision
        IPREC(1)=KWI3
        INTA(1)=1
        CALL GKSRTX(1,INTA,IPREC,.FALSE.)
      ENDIF
      GOTO 9999



* Set fill area representation
  260 CONTINUE
      CALL GKSRFA(.TRUE.)
      GOTO 9999



* Set pattern representation
  270 CONTINUE
      CALL GKSRPA(NID,IDAT)
      GOTO 9999



* Set colour representation
  280 CONTINUE
* See if colour index valid
      IF (KWI1.GE.0 .AND. KWI1.LT.KPCI(KWKIX)) THEN
        QHP(KHPXR(KCTBPT(1,KWKIX))+KWI1) = QWR1
        QHP(KHPXR(KCTBPT(2,KWKIX))+KWI1) = QWR2
        QHP(KHPXR(KCTBPT(3,KWKIX))+KWI1) = QWR3
        IF(KWI1.GT.0) THEN
            KHP(KHPXI(KWKDAT(CT,KWKIX))+KWI1) =
     :          NINT( (0.30*QWR1 + 0.59*QWR2 + 0.11*QWR3) * 255.0)
        END IF
      ELSE
        KERROR = 93
      ENDIF
      GOTO 9999



* Normalisation transformation
  310 CONTINUE
      CALL GKWKC4

* Convert from metres to raster coordnates
      DO 311 I = 1,6
         QWTOTT(I,KWKIX) = QWTOTT(I,KWKIX) * SCALE
  311 CONTINUE
      QWCLXL(KWKIX) = QWCLXL(KWKIX) * SCALE
      QWCLYB(KWKIX) = QWCLYB(KWKIX) * SCALE
      QWCLXR(KWKIX) = QWCLXR(KWKIX) * SCALE
      QWCLYT(KWKIX) = QWCLYT(KWKIX) * SCALE

* Rotate if required
      IF (KWKDAT(ROTATE,KWKIX).EQ.1) THEN
         TEMP = QWTOTT(1,KWKIX)
         QWTOTT(1,KWKIX) = -QWTOTT(4,KWKIX)
         QWTOTT(4,KWKIX) = TEMP

         TEMP = QWTOTT(5,KWKIX)
         QWTOTT(5,KWKIX) = QWTOTT(2,KWKIX)
         QWTOTT(2,KWKIX) = -TEMP

         TEMP = QWTOTT(3,KWKIX)
         QWTOTT(3,KWKIX) = -QWTOTT(6,KWKIX)
         QWTOTT(6,KWKIX) = TEMP

         TEMP = QWCLXL(KWKIX)
         QWCLXL(KWKIX) = -QWCLYT(KWKIX)
         QWCLYT(KWKIX) = QWCLXR(KWKIX)
         QWCLXR(KWKIX) = -QWCLYB(KWKIX)
         QWCLYB(KWKIX) = TEMP
      END IF

* For the TeX workstations the picture has to be shifted so that the top
* of the workstation viewport is at the graphics origin. For the non-TeX
* workstations the shift is done by repositioning the graphics origin on
* the paper instead.
      IF (KWKDAT(TEX,KWKIX).EQ.1) THEN
         QWTOTT(6,KWKIX) = QWTOTT(6,KWKIX) - QCWVYT(KWKIX) * SCALE
         QWCLYB(KWKIX) = QWCLYB(KWKIX) - QCWVYT(KWKIX) * SCALE
         QWCLYT(KWKIX) = QWCLYT(KWKIX) - QCWVYT(KWKIX) * SCALE
         IF (KWKDAT(ROTATE,KWKIX).EQ.1) THEN
            QWTOTT(3,KWKIX) = QWTOTT(3,KWKIX) + QCWVXR(KWKIX) * SCALE
            QWCLXL(KWKIX) = QWCLXL(KWKIX) + QCWVXR(KWKIX) * SCALE
            QWCLXR(KWKIX) = QWCLXR(KWKIX) + QCWVXR(KWKIX) * SCALE
         END IF
      END IF
      GOTO 9999



* Set workstation window
  320 CONTINUE
      CALL GKSWKW
      GOTO 9999



* Set workstation viewport
  330 CONTINUE
      CALL GKSWKV
      GOTO 9999



* Segment entrypoints *
  410 CONTINUE
      CALL GKSGWK(IENT,.FALSE.)
      GOTO 9999


* Write item to GKSM
  910 CONTINUE
      KERROR = 32
      GOTO 9999



* Get item type from GKSM
  920 CONTINUE
      KERROR = 34
      GOTO 9999



* Read item from GKSM
  930 CONTINUE
      KERROR = 34
      GOTO 9999



* Inquire everything *
 1111 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999



* Inquire polyline representation
 1330 CONTINUE
      IF (KWI2.EQ.GSET) THEN
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
        QWR1 = ANINT(MAX(MIN(QWR1,4.0),1.0))
      ENDIF
      GOTO 9999



* Inquire text representation
 1370 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999



* Inquire list element of pattern indices
 1410 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999



* Inquire pattern representation
 1420 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999



* Inquire colour representation
 1440 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      IF (KWI2.EQ.GREALI) THEN
        QWR1 = REAL(KHP(KHPXI(KWKDAT(CT,KWKIX))+KWI1))/255.0
        QWR2 = QWR1
        QWR3 = QWR1
      ENDIF
      GOTO 9999



* ---------------------------------------------------------------------
* Inquire Set Members of Segment Names on Workstation
* ---------------------------------------------------------------------
 1460 CONTINUE
*     KWI1    : list element requested
*
*     Data returned:
*     KERROR : error indicator
*     KWI1   : no of segment names
*     KWI2   : Nth member of set of stored segments for this
*                workstation
*
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KSGRQ = KSGRQ + 1
      GOTO 9999



* Inquire workstation catagory
 1700 CONTINUE
      KWI1 = GOUTPT
      GOTO 9999

* Inquire maximum display surface
 1720 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KWI1 = GMETRE
      GOTO 9999

* Inquire polyline facilities
 1750 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      QWR1 = QWR1/SCALE
      QWR2 = QWR2/SCALE
      QWR3 = QWR3/SCALE
      GOTO 9999

* Inquire polymarker facilities
 1770 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      QWR1 = QWR1/SCALE
      QWR2 = QWR2/SCALE
      QWR3 = QWR3/SCALE
      GOTO 9999

* Inquire text facilities ... on entry KWI1 specifies list element requested
* Allow for string and char precision font (number 1) explicitly
 1790 CONTINUE
      IF( KWI1.GT.KFNTMX+2 ) THEN
        KERROR=2002
        KWI1=KFNTMX+2
        GOTO 9999
      ENDIF
      IF( KWI1.GT.KFNTMX ) THEN

*       String or Char precision font
          IF( KWI1.EQ.KFNTMX+1 ) KWI3 = GSTRP
          IF( KWI1.EQ.KFNTMX+2 ) KWI3 = GCHARP
          KWI2 = 1
      ELSE

*       Stroke precision font
*       Make sure that fonts are available
          IF( KDBFLS.EQ.KFLNA ) THEN
            KERROR=-1009
            GOTO 9999
          ENDIF
          IF( KDBFLS.EQ.KFLCL ) CALL GKXON
          IF( KERROR.NE.0 ) GOTO 9999
          KWI2 = KHFONT(KWI1)
          KWI3 = 2
      ENDIF
      KWI1 = KFNTMX+2
      IF (KWKIX.NE.KNIL) THEN
        KWI4 = KCHH(KWKIX)
        KWI5 = KCHXPF(KWKIX)
        KWI6 = KPTXI(KWKIX)
        QWR1 = QMNCHH(KWKIX)
        QWR2 = QMXCHH(KWKIX)
        QWR3 = QMNCHX(KWKIX)
        QWR4 = QMXCHX(KWKIX)
      ELSE
        CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI4 = INTA(5)
          KWI5 = INTA(6)
          KWI6 = INTA(10)
          QWR1 = REALA(9)/SCALE
          QWR2 = REALA(10)/SCALE
          QWR3 = REALA(11)
          QWR4 = REALA(12)
        ENDIF
      END IF
      GOTO 9999



* Inquire colour facilities
 1850 CONTINUE
*     Data returned:
*     KERROR : error indicator
*     KWI1   : number of available colours
*     KWI2   : colour available
*     KWI3   : number of predefined colour indices
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KWI1 = 33
      GOTO 9999



* Inquire List Element of Available Generalised Drawing Primitives
 1870 CONTINUE
*     Input data :
*     KWI1    : list element requested
*
*     Data returned:
*     KERROR : error indicator
*     KWI1   : no of available Generalised Drawing Primitives
*     KWI2   : Nth element of list of available GDP's
      IF (KWI1.GE.1 .AND. KWI1.LE.4) THEN
        KWI2=-KWI1
      ELSE
        KERROR=2002
        KWI2=KNIL
      ENDIF
      KWI1 = 4
      GOTO 9999

* Inquire Generalised Drawing Primitive
 1880 CONTINUE
*     Input data :
*     KWI1    : GDP identifier
*
*     Data returned:
*     KERROR : error indicator
*     IDAT   : number of sets of attributes used

      KNIR = 0
      IDAT(1) = KNIL
      IDAT(2) = KNIL
      IDAT(3) = KNIL
      IDAT(4) = KNIL
      IF (KWI1.LE.-1 .AND. KWI1.GE.-4) THEN
         KNIR = 1
         IDAT(1) = GFAATT
         IF (KWI1.EQ.-1)  IDAT(1) = GPLATT
      ELSE
         KERROR = 41
      ENDIF
      GOTO 9999


*   All input related functions. None supported
 7777 CONTINUE
      KERROR = 38
      GOTO 9999


*   Here after all output primitives to sort out buffering
 8888 CONTINUE
      KDSMT(KWKIX) = GNEMPT

 9999 CONTINUE

      END
