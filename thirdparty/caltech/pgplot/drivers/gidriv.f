C*GIDRIV -- PGPLOT GIF drivers
C+
      SUBROUTINE GIDRIV (IFUNC, RBUF, NBUF, CHR, LCHR, MODE)
      INTEGER IFUNC, NBUF, LCHR, MODE
      REAL    RBUF(*)
      CHARACTER*(*) CHR
*
* PGPLOT driver for Graphics Interchange Format (GIF) files.
*
************************************************************************
*                           CAUTION                                    *
*                                                                      *
* The GIF specification incorporates the Lempel-Zev-Welch (LZW)        *
* compression technology which is the subject of a patent awarded to   *
* Unisys. Use of this technology, and in particular creation of GIF    *
* format files using this PGPLOT device driver, may require a license  *
* from Unisys.                                                         *
************************************************************************
*
* Supported device: GIF87a file format
*
* Device type codes: /GIF or /VGIF
*
* Default device name: pgplot.gif.
*
* If you have more than one image to plot (i.e. use PGPAGE) with this
* device, subsequent pages will be named: pgplot2.gif, pgplot3.gif,
* etc, disrespective of the device name you specified.
* You can however bypass this by specifying a device name including a
* number sign (#), which will henceforth be replaced by the pagenumber.
* Example: page#.gif will produce files page1.gif, page2.gif, ...,
* page234.gif, etc.
*
* Default view surface dimensions are:
* - GIF  : 850 x 680 pixels (translates to 10.0 x  8.0 inch).
* - VGIF : 680 x 850 pixels (translates to  8.0 x 10.0 inch).
* with an assumed scale of 85 pixels/inch.
* Default width and height can be overridden by specifying environment
* variables
* PGPLOT_GIF_WIDTH  (default 850)
* PGPLOT_GIF_HEIGHT (default 680)
*
* Color capability:
* Indices 0 to 255 are supported. Each of these indices can be assigned
* one color. Default colors for indices 0 to 15 are implemented.
*
* Obtaining hardcopy: Use a GIF viewer or converter.
*=
*  1-Aug-1994 - Created by Remko Scharroo
*  9-Aug-1994 - New scheme for line plotting
* 16-Aug-1994 - Provide multi-image plotting.
*  8-Sep-1994 - Add opcode 29 [TJP].
*  5-Nov-1994 - Adjust size of bitmap if necessary [TJP].
* 18-Jan-1995 - Attempt to prevent integer overflow on systems where
*               BYTE is signed [TJP].
* 28-Dec-1995 - prevent concurrent access [TJP].
* 29-Apr-1996 - use GRCTOI to decode environment variables [TJP].
*  2-Sep-1997 - correct a byte overflow problem
*-----------------------------------------------------------------------
      CHARACTER*(*) LTYPE, PTYPE, DEFNAM
      INTEGER DWD, DHT, BX, BY
      PARAMETER (LTYPE=
     1'GIF   (Graphics Interchange Format file, landscape orientation)',
     2 PTYPE=
     3'VGIF  (Graphics Interchange Format file, portrait orientation)')
      PARAMETER (DEFNAM='pgplot.gif')
      PARAMETER (DWD=850, DHT=680)

      REAL XRES, YRES
      PARAMETER (XRES=85., YRES=XRES)
C
      INTEGER UNIT, IC, NPICT, MAXIDX, STATE
      INTEGER CTABLE(3,0:255), CDEFLT(3,0:15)
      INTEGER IER, I, L, LL, IX0, IY0, IX1, IY1, USERW, USERH, JUNK
      INTEGER GRGMEM, GRFMEM, GROFIL, GRCFIL, GRCTOI
      CHARACTER*80 MSG, INSTR, FILENM
C
C Note: for 64-bit operating systems, change the following 
C declaration to INTEGER*8:
C
      INTEGER PIXMAP, WORK
C
      SAVE UNIT, IC, CTABLE, NPICT, MAXIDX, BX, BY, PIXMAP, FILENM
      SAVE CDEFLT, STATE
      DATA CDEFLT /000,000,000, 255,255,255, 255,000,000, 000,255,000,
     1             000,000,255, 000,255,255, 255,000,255, 255,255,000,
     2             255,128,000, 128,255,000, 000,255,128, 000,128,255,
     3             128,000,255, 255,000,128, 085,085,085, 170,170,170/
      DATA STATE /0/
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,240,250,260,270,280,290), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in GIF device driver:'
     1    //MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 IF (MODE.EQ.1) THEN
         CHR = LTYPE
         LCHR = LEN(LTYPE)
      ELSE IF (MODE.EQ.2) THEN
         CHR = PTYPE
         LCHR = LEN(PTYPE)
      ELSE
         CALL GRWARN('Requested MODE not implemented in GIF driver')
      END IF
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C     (Maximum size is set by GIF format to 2**16 pixels)
   20 RBUF(1) = 0
      RBUF(2) = 65536 
      RBUF(3) = 0
      RBUF(4) = 65536
      RBUF(5) = 0
      RBUF(6) = 255
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 RBUF(1) = XRES
      RBUF(2) = YRES 
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Hardcopy, supports rectangle fill, pixel 
C     primitives, and query color rep.)
C
   40 CHR = 'HNNNNRPNYN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CHR = DEFNAM
      LCHR = LEN(DEFNAM)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 RBUF(1) = 0
      RBUF(2) = BX-1 
      RBUF(3) = 0
      RBUF(4) = BY-1
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 RBUF(1) = 1
      NBUF=1
      RETURN
C
C--- IFUNC = 8, Select plot --------------------------------------------
C
   80 CONTINUE
      RETURN
C
C--- IFUNC = 9, Open workstation ---------------------------------------
C
   90 CONTINUE
C     -- check for concurrent access
      IF (STATE.EQ.1) THEN
         CALL GRWARN('a PGPLOT GIF file is already open')
         RBUF(1) = 0
         RBUF(2) = 0
         RETURN
      END IF
C     -- dimensions of plot buffer
      USERW = 0
      USERH = 0
      CALL GRGENV('GIF_WIDTH', INSTR, L)
      LL = 1
      IF (L.GT.0) USERW = GRCTOI(INSTR(:L),LL)
      CALL GRGENV('GIF_HEIGHT', INSTR, L)
      LL = 1
      IF (L.GT.0) USERH = GRCTOI(INSTR(:L),LL)
      IF (MODE.EQ.1) THEN
*     -- Landscape
         BX = DWD
         IF (USERW.GE.8) BX = USERW
         BY = DHT
         IF (USERH.GE.8) BY = USERH
      ELSE
*     -- Portrait
         BX = DHT
         IF (USERH.GE.8) BX = USERH
         BY = DWD
         IF (USERW.GE.8) BY = USERW
      END IF
      NPICT=1
      MAXIDX=0
*     -- Initialize color table
      DO 95 I=0,15
         CTABLE(1,I) = CDEFLT(1,I)
         CTABLE(2,I) = CDEFLT(2,I)
         CTABLE(3,I) = CDEFLT(3,I)
 95   CONTINUE
      DO 96 I=16,255
         CTABLE(1,I) = 128
         CTABLE(2,I) = 128
         CTABLE(3,I) = 128
 96   CONTINUE       
*
      FILENM = CHR(:LCHR)
      CALL GRGI10 (FILENM, NPICT, MSG)
      UNIT = GROFIL (MSG)
      RBUF(1) = UNIT
      IF (UNIT.LT.0) THEN
         CALL GRWARN('Cannot open output file for GIF plot')
         RBUF(2) = 0
      ELSE
         RBUF(2) = 1
         STATE = 1
      END IF
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      STATE = 0
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
      BX = NINT(RBUF(1))+1
      BY = NINT(RBUF(2))+1
      IER = GRGMEM(BX*BY, PIXMAP)
      IF (IER.NE.1) THEN
         CALL GRGMSG(IER)
         CALL GRWARN('Failed to allocate plot buffer.')
         BX = 0
         BY = 0
         PIXMAP = 0
      END IF
C     -- initialize to zero (background color)
      IF (PIXMAP.NE.0) 
     :     CALL GRGI03(1, 1, BX, BY, 0, BX, BY, %VAL(PIXMAP))
      IF (NPICT.GT.1) THEN
         CALL GRGI10 (FILENM, NPICT, MSG)
         UNIT = GROFIL(MSG)
         IF (UNIT.LT.0) THEN
            CALL GRWARN('Cannot open output file for GIF plot')
         END IF
      END IF
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      IX0=NINT(RBUF(1))+1
      IX1=NINT(RBUF(3))+1
      IY0=BY-NINT(RBUF(2))
      IY1=BY-NINT(RBUF(4))
      IF (PIXMAP.NE.0)
     :     CALL GRGI01(IX0, IY0, IX1, IY1, IC, BX, BY, %VAL(PIXMAP))
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      IX0=NINT(RBUF(1))+1
      IY0=BY-NINT(RBUF(2))
      IF (PIXMAP.NE.0)
     :     CALL GRGI01(IX0, IY0, IX0, IY0, IC, BX, BY, %VAL(PIXMAP))
      RETURN
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
      IF (UNIT.GE.0) THEN
         IER = GRGMEM(2*256*4098, WORK)
         IF (IER.NE.1) THEN
            CALL GRGMSG(IER)
            CALL GRWARN('Failed to allocate work array.')
         ELSE
            CALL GRGI06(UNIT, BX, BY, CTABLE, %VAL(PIXMAP), MAXIDX,
     :                  %VAL(WORK))
         END IF
         JUNK = GRCFIL(UNIT)
         IER = GRFMEM(2*256*4098, WORK)
      END IF
      NPICT = NPICT+1
      IER = GRFMEM(BX*BY, PIXMAP)
      IF (IER.NE.1) THEN
         CALL GRGMSG(IER)
         CALL GRWARN('Failed to deallocate plot buffer.')
      END IF
      RETURN
C
C--- IFUNC=15, Select color index --------------------------------------
C
  150 CONTINUE
      IC = RBUF(1)
      MAXIDX = MAX(MAXIDX, IC)
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C    (Not used.)
C
  160 CONTINUE
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C    (Not implemented: should not be called)
C
  170 CONTINUE
      GOTO 900
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C    (Not implemented: no alpha screen)
C
  180 CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C    (Not implemented: should not be called)
C
  190 CONTINUE
      GOTO 900
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C    (Not implemented: should not be called)
C
  200 CONTINUE
      GOTO 900
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      I = RBUF(1)
      CTABLE(1, I) = NINT(RBUF(2)*255)
      CTABLE(2, I) = NINT(RBUF(3)*255)
      CTABLE(3, I) = NINT(RBUF(4)*255)
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C    (Not implemented: should not be called)
C
  220 CONTINUE
      GOTO 900
C
C--- IFUNC=23, Escape --------------------------------------------------
C    (Not implemented: ignored)
C
  230 CONTINUE
      RETURN
C
C--- IFUNC=24, Rectangle fill ------------------------------------------
C
  240 CONTINUE
      IX0=NINT(RBUF(1))+1
      IX1=NINT(RBUF(3))+1
      IY1=BY-NINT(RBUF(2))
      IY0=BY-NINT(RBUF(4))
      IF (PIXMAP.NE.0) 
     :     CALL GRGI03(IX0, IY0, IX1, IY1, IC, BX, BY, %VAL(PIXMAP))
      RETURN
C
C--- IFUNC=25, Not implemented -----------------------------------------
C
  250 CONTINUE
      RETURN
C
C--- IFUNC=26, Line of pixels ------------------------------------------
C
  260 CONTINUE
      CALL GRGI04(NBUF, RBUF, BX, BY, %VAL(PIXMAP), MAXIDX)
      RETURN
C
C--- IFUNC=27, Not implemented -----------------------------------------
C
  270 CONTINUE
      RETURN
C
C--- IFUNC=28, Not implemented -----------------------------------------
C
  280 CONTINUE
      RETURN
C
C--- IFUNC=29, Query color representation. -----------------------------
C
  290 CONTINUE
      I = RBUF(1)
      RBUF(2) = CTABLE(1,I)/255.0
      RBUF(3) = CTABLE(2,I)/255.0
      RBUF(4) = CTABLE(3,I)/255.0
      NBUF = 4
      RETURN
C-----------------------------------------------------------------------
      END

**GRGI01 -- PGPLOT GIF driver, draw line
*+
      SUBROUTINE GRGI01 (IX0, IY0, IX1, IY1, ICOL, BX, BY, PIXMAP)
      INTEGER IX0, IY0, IX1, IY1
      INTEGER ICOL, BX, BY
      BYTE PIXMAP(BX,BY)
*
* Draw a straight-line segment from absolute pixel coordinates
* (IX0, IY0) to (IX1, IY1).
*
* Arguments:
*  ICOL            (input): Color index
*  PIXMAP   (input/output): The image data buffer.
*-----------------------------------------------------------------------
      INTEGER IX, IY, IS
      REAL    D
      BYTE    VAL
*
      IF (ICOL.GT.127) THEN
         VAL = ICOL-256
      ELSE
         VAL = ICOL
      END IF
      IF (IX0.EQ.IX1 .AND. IY0.EQ.IY1) THEN
         PIXMAP(IX0,IY0)=VAL
      ELSE IF (ABS(IY1-IY0).GT.ABS(IX1-IX0)) THEN
         D=(IX1-IX0)/REAL(IY1-IY0)
         IS=1
         IF (IY1.LT.IY0) IS=-1
         DO 10 IY=IY0,IY1,IS
            IX=NINT(IX0+(IY-IY0)*D)
            PIXMAP(IX,IY)=VAL
 10      CONTINUE
      ELSE
         D=(IY1-IY0)/REAL(IX1-IX0)
         IS=1
         IF (IX1.LT.IX0) IS=-1
         DO 20 IX=IX0,IX1,IS
            IY=NINT(IY0+(IX-IX0)*D)
            PIXMAP(IX,IY)=VAL
 20      CONTINUE
      END IF
      END

**GRGI03 -- PGPLOT GIF driver, fill rectangle
*+
      SUBROUTINE GRGI03 (IX0, IY0, IX1, IY1, ICOL, BX, BY, PIXMAP)
      INTEGER IX0, IY0, IX1, IY1
      INTEGER ICOL, BX, BY
      BYTE PIXMAP(BX,BY)
*
* Arguments:
*  IX0, IY0        (input): Lower left corner.
*  IX1, IY1        (input): Upper right corner.
*  ICOL            (input): Color value.
*  BX, BY          (input): dimensions of PIXMAP.
*  PIXMAP   (input/output): The image data buffer.
*-----------------------------------------------------------------------
      INTEGER IX, IY
      BYTE VAL
C
      IF (ICOL.GT.127) THEN
         VAL = ICOL-256
      ELSE
         VAL = ICOL
      END IF
      DO 20 IY=IY0,IY1
         DO 10 IX=IX0,IX1
            PIXMAP(IX,IY) = VAL
 10      CONTINUE
 20   CONTINUE
      END

**GRGI04 -- PGPLOT GIF driver, fill image line
*+
      SUBROUTINE GRGI04(NBUF,RBUF,BX,BY,PIXMAP,MAXIDX)
      INTEGER I,J,NBUF,BX,BY,N,IC,MAXIDX
      REAL RBUF(NBUF)
      BYTE PIXMAP(BX,BY)
*-
      I = NINT(RBUF(1))+1
      J = BY-NINT(RBUF(2))
      DO 10 N=3,NBUF
         IC=RBUF(N)
         MAXIDX=MAX(MAXIDX,IC)
         IF (IC.GT.127) IC = IC-256
         PIXMAP(I+N-3,J)=IC
 10   CONTINUE
      END

**GRGI06 -- PGPLOT GIF driver, write GIF image
*+
      SUBROUTINE GRGI06 (UNIT, BX, BY, CTABLE, PIXMAP, MAXIDX, CODE)
      INTEGER UNIT, BX, BY, MAXIDX
      INTEGER CTABLE(3,0:255)
      BYTE PIXMAP(BX * BY)
      INTEGER*2 CODE(0:4097,0:255)
*
* Write GIF image to UNIT.
*
* Arguments:
* UNIT   (input): Output unit
* BX,BY  (input): `Screen' size
* CTABLE  (input): Color map
* PIXMAP (input): Image data
* MAXIDX (input): maximum color index used.
*--
* 16-Nov-94: fixed bug (BYTE is signed)
*-----------------------------------------------------------------------
      CHARACTER GIF1*6, GIF2*7, GIF3*3, GIF4*10
      CHARACTER*2 GRGI09
      INTEGER BMAX, BMULT, BREST, BOUT
      INTEGER PIXEL, I, J, K, M, CLEAR, EOI, TABLE, IN, TOTAL, PRE, EXT
      INTEGER OLDPRE, BITS
      INTEGER GRWFCH, GRWFIL
      BYTE BLKOUT(0:254)
      COMMON /GRGICO/ BMAX, BMULT, BREST, BOUT, BLKOUT

      BITS = 1
 10   IF (MAXIDX .LT. 2**BITS) GOTO 20
      BITS = BITS + 1
      GOTO 10
 20   CONTINUE
*
* Write Header.
*
      GIF1 = 'GIF87a'
      I = GRWFCH(UNIT, GIF1)
      IF (I.NE.6) CALL GRWARN ('Error writing GIF header')
*
* Write Logical Screen Descriptor (screen width, screen height,
* color data, background color index [0], pixel aspect ratio [0]).
*
      GIF2(1:2) = GRGI09(BX)
      GIF2(3:4) = GRGI09(BY)
      GIF2(5:5) = CHAR(128 + 17 * (BITS - 1))
      GIF2(6:6) = CHAR(0)
      GIF2(7:7) = CHAR(0)
      I = GRWFCH(UNIT, GIF2)
*
* Write Global Color Table.
*
      DO 30 J=0,2**BITS-1
         GIF3(1:1) = CHAR(CTABLE(1,J))
         GIF3(2:2) = CHAR(CTABLE(2,J))
         GIF3(3:3) = CHAR(CTABLE(3,J))
         I = GRWFCH(UNIT, GIF3)
 30   CONTINUE
*
      PIXEL = MAX(BITS, 2)
*
* Write Image Descriptor.
*
      GIF4(1:1) = ','
      GIF4(2:3) = GRGI09(0)
      GIF4(4:5) = GRGI09(0)
      GIF4(6:7) = GRGI09(BX)
      GIF4(8:9) = GRGI09(BY)
      GIF4(10:10) = CHAR(0)
      I = GRWFCH(UNIT, GIF4)
*
* Write Table Based Image Data, in sub-blocks of up to 255 bytes.
*
      I = GRWFCH(UNIT, CHAR(PIXEL))
C
C LZW-compression; initialize counters; define clear code and EOI code.
C Start packing variable-size codes into 8-bit bytes.
C Push a clear code first.
C `Read' first character.
C
      DO 100 M=0,255
         DO 100 K=0,4095
  100       CODE(K,M)=0
      CLEAR=2**PIXEL
      EOI=CLEAR + 1
      BREST=0
      BOUT=0
      BMULT=1
      BMAX=CLEAR*2
      CALL GRGI07(UNIT, CLEAR)
      IN=1
      TOTAL=BX*BY
      PRE=PIXMAP(IN)
      IF (PRE.LT.0) PRE = PRE+256
*
* Start new data stream at line 310:
* 2**n-1  (n+1)-bit codes
* 2*2**n  (n+2)-bit codes
* 4*2**n  (n+3)-bit codes
*    .         .      .
*   1024     11-bit codes
*   2048     12-bit codes (incl. one clear code)
*
  310 TABLE=EOI
      BMAX=CLEAR*2
*
* `Read' next character; check if combination prefix&extension occurred earlier
*
  320 IF (IN.GE.TOTAL) GOTO 350
      IN=IN+1
      EXT=PIXMAP(IN)
      IF (EXT.LT.0) EXT = EXT+256
      OLDPRE=PRE
      PRE=CODE(PRE,EXT)
      IF (PRE.GT.0) GOTO 320
*
* If no earlier occurrence add combination to table
*
      TABLE=TABLE+1
      CALL GRGI07(UNIT, OLDPRE)
      CODE(OLDPRE,EXT)=TABLE
      PRE=EXT
      IF (TABLE.EQ.BMAX) BMAX=BMAX*2
      IF (TABLE.LT.4095) GOTO 320
      CALL GRGI07(UNIT, CLEAR)
      DO 330 M=0,255
         DO 330 K=0,4095
  330       CODE(K,M)=0
      GOTO 310
*
* Last character
*
  350 CALL GRGI07(UNIT, PRE)
      CALL GRGI07(UNIT, EOI)
      IF (BMULT.GT.1) CALL GRGI08(UNIT, BREST)
      IF (BOUT.GT.0) THEN
         IF (BOUT.GT.127) THEN
            BLKOUT(0) = BOUT-256
         ELSE
            BLKOUT(0) = BOUT
         END IF
         I = GRWFIL (UNIT, BOUT+1, BLKOUT(0))
         BOUT = 0
      END IF
      BLKOUT(0) = 0
      I = GRWFIL (UNIT, 1, BLKOUT(0))
*
* Write GIF Trailer.
*
      I = GRWFCH (UNIT, ';')
      END

**GRGI07 -- Compile GIF output code
*
      SUBROUTINE GRGI07(UNIT, INCODE)
      INTEGER UNIT, INCODE
      INTEGER BMAX, BMULT, BREST, BOUT
      BYTE BLKOUT(0:254)
      COMMON /GRGICO/ BMAX, BMULT, BREST, BOUT, BLKOUT
C
      BREST = BREST + BMULT * INCODE
      BMULT = BMULT * BMAX
C
   10 IF (BMULT .LT. 256) RETURN
      CALL GRGI08(UNIT, BREST)
      BREST = BREST / 256
      BMULT = BMULT / 256
      GOTO 10
C
      END

**GRGI08 -- Compile and write GIF output buffer
*
      SUBROUTINE GRGI08(UNIT, INCODE)
      INTEGER UNIT, INCODE, I, J, GRWFIL
      INTEGER BMAX, BMULT, BREST, BOUT
      BYTE BLKOUT(0:254)
      COMMON /GRGICO/ BMAX, BMULT, BREST, BOUT, BLKOUT
C
      BOUT = BOUT + 1
      J = MOD(INCODE,256)
      IF (J.GT.127) J = J-256
      BLKOUT(BOUT) = J
      IF (BOUT .LT. 254) RETURN
C!        changed 1997-Sep-2
      BLKOUT(0) = 254-256
      I = GRWFIL(UNIT, 255, BLKOUT(0))
      BOUT = 0
      END

**GRGI09 -- Encode integer in 2-char string
*
      CHARACTER*2 FUNCTION GRGI09(I)
      INTEGER I
      INTEGER I1, I2
*
      I1 = MOD(I,256)
      I2 = MOD(I/256,256)
      GRGI09(1:1) = CHAR(I1)
      GRGI09(2:2) = CHAR(I2)
      END

**GRGI10 -- Replace # in filename by picture number
*
      SUBROUTINE GRGI10 (NAME1, NP, NAME2)
      CHARACTER*(*) NAME1
      CHARACTER*(*) NAME2
      CHARACTER*80  TMP
      INTEGER GRTRIM
      INTEGER NP, IDX, L, LN

      LN = GRTRIM(NAME1)
      IDX = INDEX(NAME1,'#')
      IF (IDX.GT.0) THEN
C        -- if the supplied name contains a #-character, replace
C           it with the page number
         CALL GRFAO(NAME1, L, TMP, NP, 0, 0, 0)
      ELSE IF (NP.EQ.1) THEN
C        -- if this is the first page, use the supplied name
         NAME2 = NAME1
         RETURN
      ELSE IF (LN+2.LE.LEN(NAME1)) THEN
C        -- append an underscore and the page number to the supplied
C           name
         NAME1(LN+1:LN+2) = '_#'
         CALL GRFAO(NAME1, L, TMP, NP, 0, 0, 0)
      ELSE
C        -- last resort: invent a new name
         CALL GRFAO('pgplot#.gif', L, TMP, NP, 0, 0, 0)
      END IF
      CALL GRWARN ('Writing new GIF image as: '//TMP(:L))
      NAME2 = TMP(:L)
      END
