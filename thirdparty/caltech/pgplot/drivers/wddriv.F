#if HAVE_CONFIG_H
#include "config.h"
#endif

C*WDDRIV -- PGPLOT XWD drivers
C+
      SUBROUTINE WDDRIV (IFUNC, RBUF, NBUF, CHR, LCHR, MODE)
      INTEGER IFUNC, NBUF, LCHR, MODE
      REAL    RBUF(*)
      CHARACTER*(*) CHR
*
* PGPLOT driver for X Window Dump (XWD) files.
*
* Supported device: XWD format
*
* Device type codes: /WD or /VWD
*
* Default device name: pgplot.xwd.
*
* If you have more than one image to plot (i.e. use PGPAGE) with this
* device, subsequent pages will be named: pgplot2.xwd, pgplot3.xwd,
* etc, disrespective of the device name you specified.
* You can however bypass this by specifying a device name including a
* number sign (#), which will henceforth be replaced by the pagenumber.
* Example: page#.xwd will produce files page1.xwd, page2.xwd, ...,
* page234.xwd, etc.
*
* Default view surface dimensions are:
* - WD   : 850 x 680 pixels (translates to 10.0 x  8.0 inch).
* - VWD  : 680 x 850 pixels (translates to  8.0 x 10.0 inch).
* with an assumed scale of 85 pixels/inch.
* Default width and height can be overridden by specifying environment
* variables
* PGPLOT_WD_WIDTH  (default 850)
* PGPLOT_WD_HEIGHT (default 680)
*
* Color capability:
* Indices 0 to 255 are supported. Each of these indices can be assigned
* one color. Default colors for indices 0 to 15 are implemented.
*
* Obtaining hardcopy: Use an XWD viewer (xwud) or converter.
*=
* 23-Jan-1995 - Steal GIDRIV.F code and bash appropriately [SCA].
* 28-Dec-1995 - Prevent concurrent access [TJP].
* 29-Apr-1996 - Use GRCTOI to decode environment variables [TJP].
*-----------------------------------------------------------------------
      CHARACTER*(*) LTYPE, PTYPE, DEFNAM
      INTEGER DWD, DHT, BX, BY
      PARAMETER (LTYPE=
     1'WD    (X Window Dump file, landscape orientation)',
     2 PTYPE=
     3'VWD   (X Window Dump file, portrait orientation)')
      PARAMETER (DEFNAM='pgplot.xwd')
      PARAMETER (DWD=850, DHT=680)

      REAL XRES, YRES
      PARAMETER (XRES=85., YRES=XRES)
C
      INTEGER UNIT, IC, NPICT, MAXIDX, STATE
      INTEGER CTABLE(3,0:255), CDEFLT(3,0:15)
      INTEGER IER, I, L, LL, IX0, IY0, IX1, IY1, USERH, USERW, JUNK
      INTEGER GRGMEM, GRFMEM, GROFIL, GRCFIL, GRCTOI
      CHARACTER*80 MSG, INSTR, FILENM
C
C Note: for 64-bit operating systems, the following will change
C declaration to INTEGER*8:
C
      INTEGER*SIZEOF_VOID_P PIXMAP
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
      CALL GRWARN('Unimplemented function in WD device driver:'
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
         CALL GRWARN('Requested MODE not implemented in WD driver')
      END IF
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C     (Maximum size is set by XWD format to 2**16 - 1 pixels)
   20 RBUF(1) = 0
      RBUF(2) = 65535
      RBUF(3) = 0
      RBUF(4) = 65535
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
         CALL GRWARN('a PGPLOT XWD file is already open')
         RBUF(1) = 0
         RBUF(2) = 0
         RETURN
      END IF
C     -- dimensions of plot buffer
      USERW = 0
      USERH = 0
      CALL GRGENV('WD_WIDTH', INSTR, L)
      LL = 1
      IF (L.GT.0) USERW = GRCTOI(INSTR(:L),LL)
      CALL GRGENV('WD_HEIGHT', INSTR, L)
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
      CALL GRWD05 (FILENM, NPICT, MSG)
      UNIT = GROFIL (MSG)
      RBUF(1) = UNIT
      IF (UNIT.LT.0) THEN
         CALL GRWARN('Cannot open output file for WD plot')
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
     :     CALL GRWD03(1, 1, BX, BY, 0, BX, BY, %VAL(PIXMAP))
      IF (NPICT.GT.1) THEN
         CALL GRWD05 (FILENM, NPICT, MSG)
         UNIT = GROFIL(MSG)
         IF (UNIT.LT.0) THEN
            CALL GRWARN('Cannot open output file for WD plot')
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
     :     CALL GRWD01(IX0, IY0, IX1, IY1, IC, BX, BY, %VAL(PIXMAP))
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      IX0=NINT(RBUF(1))+1
      IY0=BY-NINT(RBUF(2))
      IF (PIXMAP.NE.0)
     :     CALL GRWD01(IX0, IY0, IX0, IY0, IC, BX, BY, %VAL(PIXMAP))
      RETURN
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
      IF (UNIT.GE.0) THEN
         CALL GRWD06(UNIT, BX, BY, CTABLE, %VAL(PIXMAP), MAXIDX)
         JUNK = GRCFIL(UNIT)
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
     :     CALL GRWD03(IX0, IY0, IX1, IY1, IC, BX, BY, %VAL(PIXMAP))
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
      CALL GRWD04(NBUF, RBUF, BX, BY, %VAL(PIXMAP), MAXIDX)
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

**GRWD01 -- PGPLOT WD driver, draw line
*+
      SUBROUTINE GRWD01 (IX0, IY0, IX1, IY1, ICOL, BX, BY, PIXMAP)
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
C
      IF (ICOL .GT. 127) THEN
         VAL = ICOL - 256
      ELSE
         VAL = ICOL
      END IF
C
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

**GRWD02 -- Store unsigned 16-bit integer in host independent format
*+
      SUBROUTINE GRWD02(I, ARR)
      BYTE ARR(2)
      INTEGER I, TMP
*
      TMP = MOD(I/256,256)
      IF (TMP .GT. 127) THEN
         ARR(1) = TMP - 256
      ELSE
         ARR(1) = TMP
      END IF

      TMP = MOD(I,256)
      IF (TMP .GT. 127) THEN
         ARR(2) = TMP - 256
      ELSE
         ARR(2) = TMP
      END IF
      END

**GRWD03 -- PGPLOT WD driver, fill rectangle
*+
      SUBROUTINE GRWD03 (IX0, IY0, IX1, IY1, ICOL, BX, BY, PIXMAP)
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
*
      IF (ICOL .GT. 127) THEN
         VAL = ICOL - 256
      ELSE
         VAL = ICOL
      END IF
      DO 20 IY=IY0,IY1
         DO 10 IX=IX0,IX1
            PIXMAP(IX,IY) = VAL
 10      CONTINUE
 20   CONTINUE
      END

**GRWD04 -- PGPLOT WD driver, fill image line
*+
      SUBROUTINE GRWD04(NBUF,RBUF,BX,BY,PIXMAP,MAXIDX)
      INTEGER I,J,NBUF,BX,BY,N,IC,MAXIDX
      REAL RBUF(NBUF)
      BYTE PIXMAP(BX,BY)
*-
      I = NINT(RBUF(1))+1
      J = BY-NINT(RBUF(2))
      DO 10 N=3,NBUF
         IC=RBUF(N)
         IF (IC .GT. 127) THEN
            PIXMAP(I+N-3,J)=IC - 256
         ELSE
            PIXMAP(I+N-3,J)=IC
         END IF
         MAXIDX=MAX(MAXIDX,IC)
 10   CONTINUE
      END

**GRWD05 -- Replace # in filename by picture number
*+
      SUBROUTINE GRWD05 (NAME1, NP, NAME2)
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
         CALL GRFAO('pgplot#.xwd', L, TMP, NP, 0, 0, 0)
      END IF
      CALL GRWARN ('Writing new XWD image as: '//TMP(:L))
      NAME2 = TMP(:L)
      END

**GRWD06 -- PGPLOT WD driver, write XWD image
*+
      SUBROUTINE GRWD06 (UNIT, BX, BY, CTABLE, PIXMAP, MAXIDX)
      INTEGER UNIT, BX, BY, MAXIDX
      INTEGER CTABLE(3,0:255)
      BYTE PIXMAP(BX * BY)
*
* Write XWD image to UNIT.
*
* Arguments:
* UNIT   (input): Output unit
* BX,BY  (input): Image size
* CTABLE (input): Color map
* PIXMAP (input): Image data
* MAXIDX (input): Maximum color index used.
*--
* 23-Jan-1995 - New routine [SCA]
*-----------------------------------------------------------------------
      BYTE    COLOR(12), HEAD(107)
      INTEGER I, J, IER
      INTEGER GRWFIL
      DATA COLOR /0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0/
      DATA  HEAD / 0,   0,   0, 107,       0,   0,   0,   7,
     1             0,   0,   0,   2,       0,   0,   0,   8,
     2             0,   0,   0,   0,       0,   0,   0,   0,
     3             0,   0,   0,   0,       0,   0,   0,   1,
     4             0,   0,   0,   8,       0,   0,   0,   1,
     5             0,   0,   0,   8,       0,   0,   0,   8,
     6             0,   0,   0,   0,       0,   0,   0,   3,
     7             0,   0,   0,   0,       0,   0,   0,   0,
     8             0,   0,   0,   0,       0,   0,   0,   8,
     9             0,   0,   1,   0,       0,   0,   0,   0,
     A             0,   0,   0,   0,       0,   0,   0,   0,
     B             0,   0,   0,   0,       0,   0,   0,   0,
     C             0,   0,   0,   0,      80,  71,  80,  76,
     D            79,  84,   0/
*
* Write image width into Header.
*
      CALL GRWD02 (BX, HEAD(19))
      CALL GRWD02 (BX, HEAD(51))
      CALL GRWD02 (BX, HEAD(83))
*
* Write image height into Header.
*
      CALL GRWD02 (BY, HEAD(23))
      CALL GRWD02 (BY, HEAD(87))
*
* Write number of colors into Header.
*
      CALL GRWD02 (MAXIDX + 1, HEAD(79))
*
* Write Header.
*
      IER = GRWFIL (UNIT, 107, HEAD)
      IF (IER .NE. 107) CALL GRWARN ('Error writing XWD header')
*
* Write out the color table.
*
      DO J = 0, MAXIDX
         CALL GRWD02 (J, COLOR(3))
         DO I = 1, 3
            IF (CTABLE(I,J) .GT. 127) THEN
               COLOR(3 + I * 2) = CTABLE(I,J) - 256
            ELSE
               COLOR(3 + I * 2) = CTABLE(I,J)
            END IF
            COLOR(4 + I * 2) = COLOR(3 + I * 2)
         END DO
         IER = GRWFIL (UNIT, 12, COLOR)
      END DO
*
* Write out the bitmap.
*
      IER = GRWFIL (UNIT, BX * BY, PIXMAP)
      END
