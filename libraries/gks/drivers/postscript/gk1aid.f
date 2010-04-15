*---------------------------------------------------------------
      SUBROUTINE GK1AID
*---------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Send orders to initialise device to the external PostScript file.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
*     None
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*
*  DUMMY   - Dummy character variable, used to initialise buffering.
*  FNAMES  - Character array, used to store all possible font-names.
*  I       - Temporary count variable.
*  IFONTS  - Font System used by Workstation Type (0 as 2700, 1 as 2701)
*  IREM    - Dummy integer, required by the buffering routine.
*  ISIZE   - Integer to hold the size of heap allocation.
*  MFxxxx  - Arrays which map each workstation's WDT's fonts ordering to
*            internal font ordering.
*

*     Integer workspace offset parameters
      INTEGER    ILNTYP,   IMKTYP,   IFASTY,   ICLIND
      PARAMETER (ILNTYP=1, IMKTYP=2, IFASTY=4, ICLIND=5)
      INTEGER    ICHWFT,   IPAGES,   IFTINT,   IFTNAM,   IFTPSE
      PARAMETER (ICHWFT=6, IPAGES=7, IFTINT=8, IFTNAM=9, IFTPSE=10)
      INTEGER    IFTMAP,   IFTUSD,   IFTHDT,   IORIEN,   IWIDTH
      PARAMETER (IFTMAP=11,IFTUSD=12,IFTHDT=13,IORIEN=14,IWIDTH=15)
      INTEGER    IFORMT,   ICOLR
      PARAMETER (IFORMT=16,ICOLR=17)
*     Real  workspace offset parameters (NOTE:ISCALE no longer used)
      INTEGER    ILNWID,   IMKSZ,   ICCHHT,   ICCHAN,   ISCALE
      PARAMETER (ILNWID=1, IMKSZ=2, ICCHHT=3, ICCHAN=4, ISCALE=5)
      INTEGER    ICLPXL,   ICLPYB,   ICLPXR,   ICLPYT,  IMARGN
      PARAMETER (ICLPXL=6, ICLPYB=7, ICLPXR=8, ICLPYT=9,IMARGN=10)
      INTEGER    ICCHXF
      PARAMETER (ICCHXF=11)
*     Display area's left/right margins (in points)
      REAL       RM2700
      PARAMETER (RM2700 = 8.0)
*     Page orientation indicators (NOTE: if you change these values, you must
*     change the init procedure in GK1APS too)
      INTEGER    LPORT,     LLAND
      PARAMETER (LPORT = 0, LLAND = 1)
*     EPSF indicators (NOTE: if you change these values, you must change the
*     init procedure in GK1APS and value for IIEPSF in GK1AHD too)
      INTEGER NOEPSF, IIEPSF
      PARAMETER (NOEPSF = 0, IIEPSF = 1)
*     Parameter to indicate whole file (not just a page)
      INTEGER IWHOLE
      PARAMETER (IWHOLE=0)
*     Length of the FNAMES variable
      INTEGER LNFNMS
      PARAMETER (LNFNMS=169)
*     Scale factor for converting Metres to Points
      REAL    SCFACT
      PARAMETER (SCFACT=72.0/0.0254)
*     Number of internally known fonts
      INTEGER NUMFNT
      PARAMETER (NUMFNT=13)
*     Number of hardware fonts in WDT for each workstation(NOTE: if this is
*     changed, corresponding change must be made in GK1AWD)
      INTEGER    IH2700,    IH2701
      PARAMETER (IH2700=13, IH2701=13)
*     Mapping arrays
      INTEGER MF2700(IH2700), MF2701(IH2701)
*     Pointers to individual font's start positions in FNAMES
      INTEGER IFSE(NUMFNT+1)
*
      INTEGER IREM, I, ISIZE, IFONTS
*     Due to some compilers complaining about parameterised lengths,
*     FNAMES must be declared like this:
      CHARACTER*(LNFNMS) FNAMES
      CHARACTER DUMMY*1
*
*     Font ordering mapping arrays for each workstation type:
*
*     LaserWriter Portrait:
      DATA MF2700/1,2,3,4,5,6,7,8,9,10,11,12,13/
*     LaserWriter Landscape:
      DATA MF2701/1,2,3,4,5,6,7,8,9,10,11,12,13/
*
*     Array containing font names
*
      DATA FNAMES(1:34)/'CourierCourier-BoldCourier-Oblique'/
      DATA FNAMES(35:74)/'Courier-BoldObliqueTimes-RomanTimes-Bold'/
      DATA FNAMES(75:111)/'Times-ItalicTimes-BoldItalicHelvetica'/
      DATA FNAMES(112:142)/'Helvetica-BoldHelvetica-Oblique'/
      DATA FNAMES(143:169)/'Helvetica-BoldObliqueSymbol'/
      DATA IFSE/1,8,20,35,54,65,75,87,103,112,126,143,164,170/
*
*  ALGORITHM
*  ---------
*     Initialise integer and real workspace, initialise the output
*     buffering. Then for each device set these values:
*         a) Number of HW fonts in a given PostScript device.
*         b) Display area's left and right margins (top and bottom in a
*            Landscape workstation) - these are needed to ensure that the
*            whole of the device coordinate space falls into visible portion
*            of device's imaging area. The value is arrived at empirically.
*         c) Page orientation (Portrait or Landscape)
*         d) Display area's width as given in WDT, converted to points.
*         e) Encapsulated PostScript Format file indicator.
*     Read in all recognised font names and store them onto the Heap.
*     Store the mapping arrays (they map workstation's hardware
*     font's name to an internal identifier) on Heap as well.
*     Send header and prologue.
* --------------------------------------------------------------
*

*
*     Initialise the KWKDAT and QWKDAT variables thus forcing
*     the output primitives to set the values first time round.
*
      DO 20 I=1,KMXWKI
         KWKDAT(I,KWKIX)=KNIL
   20 CONTINUE
*
      DO 30 I=1,KMXWKR
         QWKDAT(I,KWKIX)=QNIL
   30 CONTINUE

*
*     Initialise output buffering
*
      CALL GKFOCO(KIOIT,DUMMY,IREM)

*
*     Depending on workstation type, set display area's left/right margin
*     and orientation indicator.
*
      IF (KWKTYP.EQ.2700)THEN
*        A4 Portrait Monochrome
         IFONTS = 0
         KWKDAT(IFTHDT,KWKIX) = IH2700
         QWKDAT(IMARGN,KWKIX) = RM2700
         KWKDAT(IORIEN,KWKIX) = LPORT
         KWKDAT(IWIDTH,KWKIX) = NINT(SCFACT*QDSDX(KWKIX))
         KWKDAT(IFORMT,KWKIX) = NOEPSF
         KWKDAT(ICOLR ,KWKIX) = GMONOC
C        KWKDAT(ICOLR ,KWKIX) = GCOLOR
      ELSEIF(KWKTYP.EQ.2701)THEN
*        A4 Landscape Monochrome
         IFONTS = 1
         KWKDAT(IFTHDT,KWKIX) = IH2701
         QWKDAT(IMARGN,KWKIX) = RM2700
         KWKDAT(IORIEN,KWKIX) = LLAND
         KWKDAT(IWIDTH,KWKIX) = NINT(SCFACT*QDSDY(KWKIX))
         KWKDAT(IFORMT,KWKIX) = NOEPSF
         KWKDAT(ICOLR ,KWKIX) = GMONOC
      ELSEIF (KWKTYP.EQ.2702)THEN
*        A4 Portrait Monochrome EPSF
         IFONTS = 0
         KWKDAT(IFTHDT,KWKIX) = IH2700
         QWKDAT(IMARGN,KWKIX) = 0.0
         KWKDAT(IORIEN,KWKIX) = LPORT
         KWKDAT(IWIDTH,KWKIX) = NINT(SCFACT*QDSDX(KWKIX))
         KWKDAT(IFORMT,KWKIX) = IIEPSF
         KWKDAT(ICOLR ,KWKIX) = GMONOC
      ELSEIF(KWKTYP.EQ.2703)THEN
*        A4 Landscape Monochrome EPSF
         IFONTS = 1
         KWKDAT(IFTHDT,KWKIX) = IH2701
         QWKDAT(IMARGN,KWKIX) = 0.0
         KWKDAT(IORIEN,KWKIX) = LLAND
         KWKDAT(IWIDTH,KWKIX) = NINT(SCFACT*QDSDY(KWKIX))
         KWKDAT(IFORMT,KWKIX) = IIEPSF
         KWKDAT(ICOLR ,KWKIX) = GMONOC
      ELSEIF (KWKTYP.EQ.2720)THEN
*        A4 Portrait Colour
         IFONTS = 0
         KWKDAT(IFTHDT,KWKIX) = IH2700
         QWKDAT(IMARGN,KWKIX) = RM2700
         KWKDAT(IORIEN,KWKIX) = LPORT
         KWKDAT(IWIDTH,KWKIX) = NINT(SCFACT*QDSDX(KWKIX))
         KWKDAT(IFORMT,KWKIX) = NOEPSF
         KWKDAT(ICOLR ,KWKIX) = GCOLOR
      ELSEIF(KWKTYP.EQ.2721)THEN
*        A4 Landscape Colour
         IFONTS = 1
         KWKDAT(IFTHDT,KWKIX) = IH2701
         QWKDAT(IMARGN,KWKIX) = RM2700
         KWKDAT(IORIEN,KWKIX) = LLAND
         KWKDAT(IWIDTH,KWKIX) = NINT(SCFACT*QDSDY(KWKIX))
         KWKDAT(IFORMT,KWKIX) = NOEPSF
         KWKDAT(ICOLR ,KWKIX) = GCOLOR
      ELSEIF (KWKTYP.EQ.2722)THEN
*        A4 Portrait Colour EPSF
         IFONTS = 0
         KWKDAT(IFTHDT,KWKIX) = IH2700
         QWKDAT(IMARGN,KWKIX) = 0.0
         KWKDAT(IORIEN,KWKIX) = LPORT
         KWKDAT(IWIDTH,KWKIX) = NINT(SCFACT*QDSDX(KWKIX))
         KWKDAT(IFORMT,KWKIX) = IIEPSF
         KWKDAT(ICOLR ,KWKIX) = GCOLOR
      ELSEIF(KWKTYP.EQ.2723)THEN
*        A4 Landscape Colour EPSF
         IFONTS = 1
         KWKDAT(IFTHDT,KWKIX) = IH2701
         QWKDAT(IMARGN,KWKIX) = 0.0
         KWKDAT(IORIEN,KWKIX) = LLAND
         KWKDAT(IWIDTH,KWKIX) = NINT(SCFACT*QDSDY(KWKIX))
         KWKDAT(IFORMT,KWKIX) = IIEPSF
         KWKDAT(ICOLR ,KWKIX) = GCOLOR
      ELSE
         KERROR = -2010
         RETURN
      ENDIF

*
*     initialise the page counter
*
      KWKDAT(IPAGES,KWKIX) = 0

*
*     initialise number of internally known fonts
*
      KWKDAT(IFTINT,KWKIX) = NUMFNT

*
*     Allocate Character Heap Space to store the font names
*
      ISIZE=LNFNMS
      CALL GKHPAL(ISIZE,KCHARS,KWKDAT(IFTNAM,KWKIX))
      IF(KERROR.EQ.0)THEN
*        Store the font names
         DO 100 I=1,LNFNMS
            CHP(KHPXC(KWKDAT(IFTNAM,KWKIX))+I-1) = FNAMES(I:I)
  100    CONTINUE
      ELSE
         RETURN
      ENDIF

*
*     Allocate Integer Heap space to store individual fonts starting positions
*
      CALL GKHPAL(NUMFNT+1,KINTGS,KWKDAT(IFTPSE,KWKIX))
      IF(KERROR.EQ.0)THEN
*        Store the pointers
         DO 200 I=1,NUMFNT+1
            KHP(KHPXI(KWKDAT(IFTPSE,KWKIX))+I-1) = IFSE(I)
  200    CONTINUE
      ELSE
         RETURN
      ENDIF

*
*     Allocate Integer Heap space to store the current workstation's font
*     order mapping array
*
      CALL GKHPAL(KWKDAT(IFTHDT,KWKIX),KINTGS,KWKDAT(IFTMAP,KWKIX))
      IF(KERROR.EQ.0) THEN
         IF(IFONTS .EQ. 0)THEN
            DO 300 I=1,KWKDAT(IFTHDT,KWKIX)
               KHP(KHPXI(KWKDAT(IFTMAP,KWKIX))+I-1) = MF2700(I)
  300       CONTINUE
         ELSEIF(IFONTS .EQ. 1)THEN
            DO 305 I=1,KWKDAT(IFTHDT,KWKIX)
               KHP(KHPXI(KWKDAT(IFTMAP,KWKIX))+I-1) = MF2701(I)
  305       CONTINUE
         ENDIF
      ELSE
         RETURN
      ENDIF

*
*     Allocate Integer Heap space to record the hardware font usage
*
      CALL GKHPAL(NUMFNT,KINTGS,KWKDAT(IFTUSD,KWKIX))
      IF(KERROR.NE.0) RETURN



*
*     Send PostScript Header and then PostScript Prologue to the device
*

      CALL GK1AHD (IWHOLE)
      CALL GK1APS

      END
