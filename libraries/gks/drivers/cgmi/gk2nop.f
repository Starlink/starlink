      SUBROUTINE GK2NOP(ITYPE)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (Part of) Workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  This routine returns the GKS itemtype from the CGM
*  (after converting from the CGM opcodes)
*  Also reads CGM items into internal Buffer CMBUFF
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver GK2NWD
*
*  ARGUMENTS
*  ---------
*     out ITYPE Integer value of GKS Item Type
*
      INTEGER ITYPE
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/    Workstation index
*     Modify /GKZIO/     CMBUFF
*     Modify /GKYWKD/    Derive workstation I/O channel i.d.
*                        KWKDAT usage is described in GK2NWD.
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkerr.cmn'
*
*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgn.cmn'

*   GKS Errors
*   ----------

*      162  No item is left in GKS metafile input (Abort file)
*      163  Metafile item is invalid
*      164  Item type is not a valid GKS item

*  LOCALS
*  ------
*    TEMP    : Temporary integer
*    ICODE   : Next Opcode
*    OPCODE  : Current Opcode
*    N       : No of cols for Cell Array
*    M       : No of rows for Cell Array
*    STFLAG  : Flag to mark if in character string
*    FIRSTM  : Flag to mark first time through
*    BYTE1   : Flag to indicate if first byte of real number
*    CELLS   : Flag to mark cell array after size
*    EXPON   : Flag to mark if Exponent follows

      INTEGER TEMP,ICODE,OPCODE,N,M
      LOGICAL STFLAG,FIRSTM,BYTE1,CELLS,EXPON

      INTEGER REALPT, VDC, VDCX, VDCY
      PARAMETER (REALPT=1,VDC=2,VDCX=3,VDCY=4)

*  Set up Hexadecimal parameters

      INTEGER X20, X30, X40, X58, X5C, X60
      INTEGER ESC, X312C, X3021
      INTEGER X3020, X3120, X3220, X3320, X3420, X3520, X3620, X3720
      INTEGER X3530, X3630
      PARAMETER ( X20 = 32, X30 = 48, X40 = 64, X60 = 96 )
      PARAMETER ( X312C = 12588, X3021 = 12321 )
      PARAMETER ( X3020 = 12320, X3120 = 12576, X3220 = 12832 )
      PARAMETER ( X3320 = 13088, X3420 = 13344, X3520 = 13600 )
      PARAMETER ( X3530 = 13616, X3620 = 13856, X3630 = 13872 )
      PARAMETER ( X3720 = 14112 )
      PARAMETER ( ESC = 27, X58 = 88, X5C = 92 )

*------------------------------------------------------------------

  10  CONTINUE
      KCDPTR(KWKIX) = 1
      KCDREC(KWKIX) = 1
      BYTE1 = .TRUE.
      CELLS = .FALSE.
      EXPON = .FALSE.
      KXNUM=0
      KXCHA=0
      FIRSTM = .TRUE.
      STFLAG = .FALSE.

      IF (FIRSTP(KWKIX)) THEN
         FIRSTP(KWKIX)=.FALSE.
         KCHANI(KWKIX)=KLENRC+1

*   Set the appropriate pointers (metafile defaults replacement
*   or directly from the the metafile )
      ELSEIF(DOREP(KWKIX)) THEN
         KDFPTR(KWKIX)=KDFPTR(KWKIX)-1
         IF (KDFPTR(KWKIX).EQ.0) KDFPTR(KWKIX)=KLENRC
      ELSE
         KCHANI(KWKIX)=KCHANI(KWKIX)-1
         IF (KCHANI(KWKIX).EQ.0) KCHANI(KWKIX)=KLENRC
      ENDIF

  20  CONTINUE

*   Get the next code
      CALL GK2NBU(ICODE)

*   If it is Open Character String set STFLAG true; If it is String
*   Terminator, set STFLAG false.
      IF (ICODE.EQ.ESC) THEN
         CALL GK2NBU(ICODE)
         IF (ICODE.EQ.X58) THEN
            STFLAG = .TRUE.
         ELSEIF (ICODE.EQ.X5C) THEN
            STFLAG = .FALSE.
         ENDIF
         GOTO 20

*   If middle of character string, increment character counter
*   and get next opcode
      ELSEIF(STFLAG) THEN
         KXCHA=KXCHA+1
         GOTO 20
      ENDIF

*   If not an opcode
      IF ( ICODE.GE.X40 .OR. ICODE.LT.X20 ) THEN

*   If past size of Cell Array ignore
         IF (CELLS) GOTO 20

*   If VDC exponent allowed and present then set flag
         IF (BYTE1) THEN
            BYTE1 = .FALSE.
            IF ( KVDCTY(KWKIX).EQ.1
     :           .AND. KVXALL(KWKIX).EQ.0
     :           .AND. MOD(ICODE/8,2).EQ.1 ) EXPON = .TRUE.
         ENDIF

*   Check if its the last byte of a number- if so increment the counter
         IF( ICODE.LT.X60 ) THEN
            IF ( .NOT. EXPON ) THEN
               KXNUM = KXNUM + 1
               BYTE1 = .TRUE.
            ENDIF
            EXPON = .FALSE.

*   If past the 1st 3 points on a cell array find size of array
*   And ignore further elements
            IF( OPCODE.EQ.40 .AND. KXNUM.GE.6 )THEN
               CALL GK2NHI(N,.FALSE.)
               CALL GK2NHI(M,.FALSE.)
               KXNUM = N*M
               CELLS=.TRUE.
            ENDIF
         ENDIF
         GOTO 20
      ENDIF

*   Code is obviously an opcode..
*   If this is the first time we've been here, keep going...otherwise
*   go & suss out the equivalent GKS Item type straight away.
      IF (FIRSTM) THEN
         KCDREC(KWKIX)=1
         KCDPTR(KWKIX)=2

*   If code is an extended opcode, get next byte.
         IF ( ICODE.GE.X30.AND.ICODE.LT.X40 ) THEN
            ICODE=ICODE*256
            CALL GK2NBU(TEMP)

            ICODE=ICODE+TEMP
            KCDPTR(KWKIX)=3
         ENDIF
         FIRSTM = .FALSE.
         OPCODE = ICODE
         KWKDAT(1,KWKIX) = OPCODE
*   Terminate metafile defaults replacement
         IF ( DOREP(KWKIX).AND.OPCODE.EQ.X312C )THEN
            DOREP(KWKIX)=.FALSE.
            KCHANI(KWKIX)=KCHANI(KWKIX)-1
            IF (KCHANI(KWKIX).EQ.0) KCHANI(KWKIX)=KLENRC
         ENDIF

*   Get next code if not End Metafile
         IF(OPCODE.NE.X3021) GOTO 20
      ENDIF

*   Reset character & record pointers to 1st character after opcode
      IF (OPCODE.LT.43) THEN
         KCDPTR(KWKIX)=2
      ELSE
         KCDPTR(KWKIX)=3
      ENDIF
      KCDREC(KWKIX)=1

*   Test to see if last opcode was escape line - if so, back up the
*   pointer by 1
      IF (ICODE.EQ.X20) THEN
         DO 50 TEMP=1,KNSUBS(KWKIX)
            IF(KSUBAR(TEMP,KWKIX).EQ.X60)THEN
               KCHANI(KWKIX)=KCHANI(KWKIX)-1
               IF (KCHANI(KWKIX).EQ.0) KCHANI(KWKIX)=KLENRC
               GOTO 60
            ENDIF
 50      CONTINUE
      ENDIF
 60   CONTINUE

*   Make sure no code is repeated in the metafile defaults replacement
      IF (METDEF(KWKIX))THEN
         KDFPTR(KWKIX)=KDFPTR(KWKIX)-1
      ENDIF

*  Metafile Delimiters
      IF ( OPCODE.GE.X3020.AND.OPCODE.LE.X3020+4 ) THEN
         GOTO (100, 200 , 300 , 400, 500) OPCODE - X3020 + 1
*  Metafile Descriptor elements
      ELSEIF ( OPCODE.GE.X3120.AND.OPCODE.LE.X3120+16 ) THEN
         GOTO (1050,1100,1150,1200,1250,1300,1350,1400,1450,1500,
     :         1550,1600,1650,1700,1750,1800) OPCODE - X3120 + 1
*  Picture Descriptor elements
      ELSEIF ( OPCODE.GE.X3220.AND.OPCODE.LE.X3220+6 ) THEN
         GOTO (2050,2100,2150,2200,2250,2300,2350) OPCODE - X3220 + 1
*  Control elements
      ELSEIF ( OPCODE.GE.X3320.AND.OPCODE.LE.X3320+5 ) THEN
         GOTO (3050,3100,3150,3200,3250,3300) OPCODE - X3320 + 1
*  Drawing Primitives
      ELSEIF ( OPCODE.GE.X20.AND.OPCODE.LE.X20+12 ) THEN
         GOTO (4050,4100,4150,4200,4250,4300,4350,4400,4450,
     :  4500,4550) OPCODE - X20 + 1
*  Circles etc
      ELSEIF ( OPCODE.GE.X3420.AND.OPCODE.LE.X3420+7 ) THEN
         GOTO 5000
* Attributes
      ELSEIF ( OPCODE.GE.X3520.AND.OPCODE.LE.X3520+7 ) THEN
         GOTO 6000
* More Attributes
      ELSEIF ( OPCODE.GE.X3530.AND.OPCODE.LE.X3530+9 ) THEN
         GOTO 6100
* More Attributes
      ELSEIF ( OPCODE.GE.X3620.AND.OPCODE.LE.X3620+12 ) THEN
         GOTO (7050,7100,7150,7200,7250,7300,7350,7400,7450,
     :         7500,7550,7600,7650) OPCODE - X3620 + 1
*  Colour table and ASF
      ELSEIF ( OPCODE.GE.X3630.AND.OPCODE.LE.X3630+1 ) THEN
         GOTO (7700,7750) OPCODE - X3630 + 1
*  External elments
      ELSEIF ( OPCODE.GE.X3720.AND.OPCODE.LE.X3720+2 ) THEN
         GOTO 7760
      ELSE
         GOTO 10
      ENDIF
*---------------------------------------------------------------------*
* Begin Metafile
*----------------------
  100 CONTINUE

*   Set the next picture flag & do metafile replacement flag to false
      BEGMET(KWKIX)=.FALSE.
      GOTO 10

*----------------------
*   End Metafile
*----------------------
  200 CONTINUE
      ITYPE=0
      GOTO 9999

*----------------------
*   Begin Picture
*----------------------
  300 CONTINUE
      ITYPE=1
      CALL GK2NRV
* Set the flag to read the metafile defaults replacements
      IF ((KDFREC(KWKIX).GT.1).OR.(KDFPTR(KWKIX).GT.1)) THEN
         KDFREC(KWKIX)=1
         KDFPTR(KWKIX)=2
         DOREP(KWKIX)=.TRUE.
      ENDIF

      GOTO 9999

*----------------------
*   Begin Picture Body
*----------------------
  400 CONTINUE
      GOTO 10

*----------------------
*   End Picture
*----------------------
  500 CONTINUE
      ITYPE=3
      GOTO 9999

*----------------------
*   Metafile Version
*----------------------
 1050 CONTINUE
      CALL GK2NHI(TEMP,.TRUE.)
*  Error code is wrong but at present only way to stop interpretation
      IF ( TEMP .NE. 1 ) KERROR = 162
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Metafile Description - Ignored
*----------------------
 1100 CONTINUE
      GOTO 10

*----------------------
*   VDC Type
*----------------------
 1150 CONTINUE
      CALL GK2NHI(KVDCTY(KWKIX),.TRUE.)

*  If not Real or integer Set Error flag
      IF (KVDCTY(KWKIX).NE.0 .AND. KVDCTY(KWKIX).NE.1 ) KERROR=-2006
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Integer Precision
*----------------------
 1200 CONTINUE
      CALL GK2NHI(KINTPR(KWKIX),.TRUE.)
*   Should set an error on this....but what?
      IF(KINTPR(KWKIX).GT.32) KERROR = 163
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Real Precision
*----------------------
 1250 CONTINUE
      CALL GK2NHI(KMXRPR(KWKIX),.TRUE.)
      CALL GK2NHI(KMNRPR(KWKIX),.TRUE.)
      CALL GK2NHI(KDEFEX(KWKIX),.TRUE.)
      CALL GK2NHI(KEXPAL(KWKIX),.TRUE.)
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Index Precision
*----------------------
 1300 CONTINUE
      CALL GK2NHI(KIXPR(KWKIX),.TRUE.)
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Colour Precision
*----------------------
 1350 CONTINUE
      CALL GK2NHI(KCOLPR(KWKIX),.TRUE.)
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Colour Index Precision
*----------------------
 1400 CONTINUE
      CALL GK2NHI(KCIXPR(KWKIX),.TRUE.)
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Maximum Colour Index
*----------------------
 1450 CONTINUE
      CALL GK2NHI(KMXCIX(KWKIX),.TRUE.)
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Colour Value Extent
*----------------------
 1500 CONTINUE

*   Get max & min colour values (note: FALSE means that these values
*   are not yet set)
      CALL GK2NRG(QMNCOL,.FALSE.)
      CALL GK2NRG(QMXCOL,.FALSE.)
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Metafile Element List - Ignore at present
*----------------------
 1550 CONTINUE
      GOTO 10

*----------------------
*   Begin Metafile Defaults Replacement
*----------------------
 1600 CONTINUE
      METDEF(KWKIX)=.TRUE.
      KDFREC(KWKIX)=1
      KDFPTR(KWKIX)=1
      GOTO 10

*----------------------
*   End Metafile Defaults Replacement
*----------------------
 1650 CONTINUE
      METDEF(KWKIX)=.FALSE.
      GOTO 10

*----------------------
*   Font List - Currently ignored
*----------------------
 1700 CONTINUE
      GOTO 10

*----------------------
*   Character Set List
*----------------------
 1750 CONTINUE
      CALL GK2NHI(TEMP,.TRUE.)

*   If not 96 character G set, then Set error flag
      IF (TEMP.NE.1) KERROR=164
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Character Coding Announcer
*----------------------
 1800 CONTINUE
      CALL GK2NHI(TEMP,.TRUE.)

*   If not basic seven bit then Set error flag
      IF (TEMP.NE.0) KERROR=163
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Scaling Mode
*----------------------
 2050 CONTINUE
      CALL GK2NHI(KSCAMD(KWKIX),.TRUE.)

*  Check for valid Enumerated types
      IF ( KSCAMD(KWKIX).NE.0 .AND. KSCAMD(KWKIX) .NE.1 ) KERROR=-2006
*   Doesn't handle metric at the moment, Ignore setting
*     IF (KSCAMD(KWKIX).EQ.1) KERROR=164
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Colour Selection Mode
*----------------------
 2100 CONTINUE
      CALL GK2NHI(KCSLMD(KWKIX),.TRUE.)

*  If not indexed or direct colour then set error flag
      IF ( KCSLMD(KWKIX).NE.0 .AND. KCSLMD(KWKIX) .NE.1 ) KERROR=-2006
*  If direct colour then Set error flag
      IF ( KCSLMD(KWKIX).EQ.1 ) KERROR=164
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Line Width Specification Mode
*----------------------
 2150 CONTINUE
      CALL GK2NHI(KLWSMD(KWKIX),.TRUE.)

*  If not Scaled or absolute then set error flag
      IF ( KLWSMD(KWKIX).NE.0 .AND. KLWSMD(KWKIX) .NE.1 ) KERROR=-2006
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Marker Size Specification Mode
*----------------------
 2200 CONTINUE
      CALL GK2NHI(KMSSMD(KWKIX),.TRUE.)

*  If not Scaled or absolute then set error flag
      IF ( KMSSMD(KWKIX).NE.0 .AND. KMSSMD(KWKIX) .NE.1 ) KERROR=-2006
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Edge Width Specification Mode
*----------------------
 2250 CONTINUE
      CALL GK2NHI(KEWSMD(KWKIX),.TRUE.)

*  If not Scaled or absolute then set error flag
      IF ( KEWSMD(KWKIX).NE.0 .AND. KEWSMD(KWKIX) .NE.1 ) KERROR=-2006
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------------
*   VDC Extent
*   Map to Workstation window
*----------------------------
 2300 CONTINUE
      ITYPE=71
      GOTO 9999

*----------------------
*   Background Colour
*----------------------
 2350 CONTINUE
      CALL GK2NRG(QBACOL,.TRUE.)
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   VDC Integer Precision
*----------------------
 3050 CONTINUE
      CALL GK2NHI(KVDCIP(KWKIX),.TRUE.)
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   VDC Real Precision
*----------------------
 3100 CONTINUE
      CALL GK2NHI(KVMXRP(KWKIX),.TRUE.)
      CALL GK2NHI(KVMNRP(KWKIX),.TRUE.)
      CALL GK2NHI(KVDEFX(KWKIX),.TRUE.)
      CALL GK2NHI(KVXALL(KWKIX),.TRUE.)
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Auxiliary Colour
*----------------------
 3150 CONTINUE
      CALL GK2NHI(KAUXCL(KWKIX),.TRUE.)

*   If not 0 or 1 then set error flag
      IF (KAUXCL(KWKIX).NE.0.AND.KAUXCL(KWKIX).NE.1) KERROR=-2006
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Transparency
*----------------------
 3200 CONTINUE
      CALL GK2NHI(KTRANS(KWKIX),.TRUE.)

*   If not 0 or 1 then set error flag
      IF (KTRANS(KWKIX).NE.0.AND.KTRANS(KWKIX).NE.1) KERROR=-2006
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Clip Rectangle
*----------------------
 3250 CONTINUE
      ITYPE=61
      GOTO 9999

*----------------------
*   Clip Indicator
*----------------------
 3300 CONTINUE
      CALL GK2NHI(TEMP,.TRUE.)
      IF (KERROR.NE.0) GOTO 9999
      GOTO 10

*----------------------
*   Polyline
*----------------------
 4050 CONTINUE
      ITYPE=11
      GOTO 9999

*----------------------
*   Disjoint Polyline
*----------------------
 4100 CONTINUE
*   No equivalent in GKS - Generate error message
      KERROR = 164
      GOTO 9999

*----------------------
*   Polymarker
*----------------------
 4150 CONTINUE
      ITYPE=12
      GOTO 9999

*----------------------
*   Text
*----------------------
 4200 CONTINUE
      ITYPE=13
      GOTO 9999

*----------------------
*   Restricted Text
*----------------------
 4250 CONTINUE
*   No equivalent in GKS - Treat as normal text
      ITYPE=13
      GOTO 9999

*----------------------
*   Append Text
*----------------------
 4300 CONTINUE
*   No equivalent in GKS - Generate error message
      KERROR = 164
      GOTO 9999

*----------------------
*   Polygon
*----------------------
 4350 CONTINUE
      ITYPE=14
      GOTO 9999

*----------------------
*   Polygon Set
*----------------------
 4400 CONTINUE
*   No equivalent in GKS - Treat as Polygon
*      KERROR = 164
      ITYPE=14
      GOTO 9999

*----------------------
*   Cell Array
*----------------------
 4450 CONTINUE
      IF ( KCSLMD(KWKIX).EQ.1 ) THEN
*  Direct colour cell arrays not handled
         KERROR = 164
      ELSE
         ITYPE=15
      ENDIF
      GOTO 9999

*----------------------
*   GDP
*----------------------
 4500 CONTINUE
      ITYPE=16
      GOTO 9999

*----------------------
*   Rectangle
*----------------------
 4550 CONTINUE
      ITYPE=14
      GOTO 9999

*-------------------------------------
*   CGM Elements not supported by GKS
*-------------------------------------
 5000 CONTINUE

*  Note: type 6, 7 & 8 not easily converted to GKS (Ellipses)
      IF (OPCODE.GT.X3420+4) THEN
         KERROR = 164
         GOTO 9999
      ENDIF
      ITYPE=16
      GOTO 9999

*----------------------
*   Output Attributes
*----------------------
 6000 CONTINUE

* Should give itype of 21 to 28
      ITYPE = OPCODE - X3520 + 21
      GOTO 9999

 6100 CONTINUE

* Should give itype of 29 to 36
      ITYPE = OPCODE - X3530 + 28
      IF (ITYPE.LE.29) THEN
         ITYPE=ITYPE+1
      ELSEIF (ITYPE.GE.35) THEN
         ITYPE=ITYPE-1
      ENDIF

      GOTO 9999

*----------------------
*   Fill Bundle Index
*----------------------
 7050 CONTINUE
      ITYPE=37
      GOTO 9999

*----------------------
*   Interior Style
*----------------------
 7100 CONTINUE
      ITYPE=38
      GOTO 9999

*----------------------
*   Fill Colour
*----------------------
 7150 CONTINUE
      ITYPE=40
      GOTO 9999

*----------------------
*   Hatch index
*----------------------
 7200 CONTINUE
      KHTPAT(KWKIX)=3
      ITYPE=39
      GOTO 9999

*----------------------
*   Pattern Index
*----------------------
 7250 CONTINUE
      KHTPAT(KWKIX)=2
      ITYPE=39
      GOTO 9999

*----------------------
*   Edge Bundle Index
*----------------------
 7300 CONTINUE
*   No equivalent in GKS - Generater error message
      KERROR = 164
      GOTO 9999

*----------------------
*   Edge Type
*----------------------
 7350 CONTINUE
*   No equivalent in GKS - Generate error message
      KERROR = 164
      GOTO 9999

*----------------------
*   Edge width
*----------------------
 7400 CONTINUE
*   No equivalent in GKS - Generate error message
      KERROR = 164
      GOTO 9999

*----------------------
*   Edge Colour
*----------------------
 7450 CONTINUE
*   No equivalent in GKS - Generate error message
      KERROR = 164
      GOTO 9999

*----------------------
*   Edge visibility
*----------------------
 7500 CONTINUE
*   No equivalent in GKS - Generate error message
      KERROR = 164
      GOTO 9999

*----------------------
*   Fill Reference point
*----------------------
 7550 CONTINUE
      ITYPE=42
      GOTO 9999

*----------------------
*   Pattern Table
*----------------------
 7600 CONTINUE
      ITYPE=55
      GOTO 9999

*----------------------
*   Pattern Size
*----------------------
 7650 CONTINUE
      ITYPE=41
      GOTO 9999

*----------------------
*  Colour Table
*----------------------
 7700 CONTINUE
      ITYPE=56
      GOTO 9999

*----------------------
*   Aspect Source Flags
*----------------------
 7750 CONTINUE
      ITYPE=43
      GOTO 9999

*--------------------
*   External elements
*--------------------
 7760 CONTINUE
      IF ( OPCODE .EQ. X3720 ) THEN
*  Escape
         ITYPE=6
      ELSEIF ( OPCODE .EQ. X3720+1 ) THEN
*  Message
         ITYPE=5
      ELSE
*  Applications Data = User item, Get identifier
         CALL GK2NHI(ITYPE,.TRUE.)
         IF ( ITYPE .LE. 100 ) THEN
            IF ( ITYPE .LE. 0 ) THEN
               ITYPE = 101
            ELSE
               ITYPE = 100 + ITYPE
            ENDIF
         ENDIF
      ENDIF
      GOTO 9999

 9999 CONTINUE

      RETURN
      END
