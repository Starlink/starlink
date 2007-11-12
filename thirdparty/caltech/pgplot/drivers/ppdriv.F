#if HAVE_CONFIG_H
#include "config.h"
#endif

C*PPDRIV -- PGPLOT PPM drivers
C+
      SUBROUTINE PPDRIV (IFUNC, RBUF, NBUF, CHR, LCHR, MODE)
      INTEGER IFUNC, NBUF, LCHR, MODE
      REAL    RBUF(*)
      CHARACTER*(*) CHR
*
* PGPLOT driver for Portable Pixel Map (PPM) files with 'true color'
* capability.
*
* Supported device: PPM (P6) file format
*
* Device type codes: /PPM or /VPPM
*
* Default device name: pgplot.ppm.
*
* If you have more than one image to plot (i.e. use PGPAGE) with this
* device, subsequent pages will be named: pgplot2.ppm, pgplot3.ppm,
* etc, disrespective of the device name you specified.
* You can however bypass this by specifying a device name including a
* number sign (#), which will henceforth be replaced by the pagenumber.
* Example: page#.ppm will produce files page1.ppm, page2.ppm, etc.
*
* Default view surface dimensions are:
* - PPM  : 850 x 680 pixels 
* - VPPM : 680 x 850 pixels 
* Default width and height can be overriden by specifying environment
* variables
* PGPLOT_PPM_WIDTH  (default 850)
* PGPLOT_PPM_HEIGHT (default 680)
* The nominal scale is 85 pixels per inch.
*
* Color capability:
*   Indices 0 to 255 are supported. 
*   Default colors for indices 0 to 15 are implemented.
*   Color representation can be changed with PGSCR; color changes
*   affect subsequently drawn pixels only, not previously drawn
*   pixels. Thus the image is not limited to 256 different colors.
*
* Obtaining hardcopy: Use a PPM viewer or converter.
*=
*  9-Aug-1993 - Created by Remko Scharroo
*  6-Jul-1994 - Adapted to new PGPLOT version 4.9h
*  4-Aug-1994 - Use FASTIO.
*  9-Aug-1994 - New scheme for line plotting
* 16-Aug-1994 - Provide multi-image plotting.
* 16-Nov-1994 - Revised (T. Pearson).
* 28-Dec-1995 - Prevent concurrent access [TJP].
* 29-Apr-1996 - Use GRCTOI to decode environment variables [TJP].
*-----------------------------------------------------------------------
      CHARACTER*(*) LTYPE, PTYPE, DEFNAM
      INTEGER DWD, DHT
      PARAMETER (
     1 LTYPE= 'PPM   (Portable Pixel Map file, landscape orientation)',
     2 PTYPE= 'VPPM  (Portable Pixel Map file, portrait orientation)')
      PARAMETER (DEFNAM='pgplot.ppm')
      PARAMETER (DWD=850, DHT=680)

      REAL XRES, YRES
      PARAMETER (XRES=85., YRES=XRES)
C
      INTEGER UNIT, IC, CVAL, CTABLE(3,0:255), IER, I, L, LL, NPICT
      INTEGER BX, BY, IX0, IY0, IX1, IY1, R, G, B, STATE, USERH, USERW
      INTEGER CDEFLT(3,0:15), JUNK
      INTEGER GRGMEM, GRFMEM, GROFIL, GRCFIL, GRCTOI
      CHARACTER*80 MSG, INSTR, FILENM
C
C Note: for 64-bit operating systems, the following will change
C declaration to INTEGER*8:
C
      INTEGER*SIZEOF_VOID_P PIXMAP
C
      SAVE    UNIT, IC, CVAL, CTABLE, BX, BY, PIXMAP, NPICT, CDEFLT
      SAVE    STATE
      DATA CDEFLT /000,000,000, 255,255,255, 255,000,000, 000,255,000,
     1             000,000,255, 000,255,255, 255,000,255, 255,255,000,
     2             255,128,000, 128,255,000, 000,255,128, 000,128,255,
     3             128,000,255, 255,000,128, 085,085,085, 170,170,170/
      DATA    STATE /0/
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,240,250,260,270,280,290), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in PPM device driver:'
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
         CALL GRWARN('Requested MODE not implemented in PPM driver')
      ENDIF
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = -1 
      RBUF(3) = 0
      RBUF(4) = -1 
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
         CALL GRWARN('a PGPLOT PPM file is already open')
         RBUF(1) = 0
         RBUF(2) = 0
         RETURN
      END IF
C     -- dimensions of plot buffer
      USERW = 0
      USERH = 0
      CALL GRGENV('PPM_WIDTH', INSTR, L)
      LL = 1
      IF (L.GT.0) USERW = GRCTOI(INSTR(:L),LL)
      CALL GRGENV('PPM_HEIGHT', INSTR, L)
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
      CALL GRPP10 (FILENM, NPICT, MSG)
      UNIT = GROFIL (MSG)
      RBUF(1) = UNIT
      IF (UNIT.LT.0) THEN
          CALL GRWARN('Cannot open output file for PPM plot')
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
C     -- allocate buffer with 4 bytes per pixel
      IER = GRGMEM(4*BX*BY, PIXMAP)
      IF (IER.NE.1) THEN
         CALL GRGMSG(IER)
         CALL GRWARN('Failed to allocate plot buffer.')
         BX = 0
         BY = 0
         PIXMAP = 0
      END IF
C     -- initialize to zero (background color)
      IF (PIXMAP.NE.0) 
     :     CALL GRPP03(1, 1, BX, BY, 0, BX, BY, %VAL(PIXMAP))
C     -- open new file if necessary
      IF (NPICT.GT.1) THEN
          CALL GRPP10 (FILENM, NPICT, MSG)
          UNIT = GROFIL (MSG)
          IF (UNIT.LT.0) 
     :        CALL GRWARN('Cannot open output file for PPM plot')
C         -- no way to return error status!
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
     :     CALL GRPP01(IX0, IY0, IX1, IY1, CVAL, BX, BY, %VAL(PIXMAP))
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      IX0=NINT(RBUF(1))+1
      IY0=BY-NINT(RBUF(2))
      IF (PIXMAP.NE.0) 
     :     CALL GRPP01(IX0, IY0, IX0, IY0, CVAL, BX, BY, %VAL(PIXMAP))
      RETURN
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
      IF (PIXMAP.NE.0) THEN
         CALL GRPP02(UNIT, BX, BY, %VAL(PIXMAP))
         IF (UNIT.GE.0) JUNK = GRCFIL(UNIT)
         NPICT = NPICT+1
         IER = GRFMEM(4*BX*BY, PIXMAP)
         IF (IER.NE.1) THEN
            CALL GRGMSG(IER)
            CALL GRWARN('Failed to deallocate plot buffer.')
         END IF
      END IF
      RETURN
C
C--- IFUNC=15, Select color index --------------------------------------
C
  150 CONTINUE
      IC = NINT(RBUF(1))
      IF (IC.LT.0 .OR. IC.GT.255) IC = 1
      R = CTABLE(1,IC)
      G = CTABLE(2,IC)
      B = CTABLE(3,IC)
      CVAL = R + 256*(G + 256*B)
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
      I = NINT(RBUF(1))
      IF (I.GE.0 .AND. I.LE.255) THEN
         CTABLE(1, I) = NINT(RBUF(2)*255)
         CTABLE(2, I) = NINT(RBUF(3)*255)
         CTABLE(3, I) = NINT(RBUF(4)*255)
      END IF
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
     :     CALL GRPP03(IX0, IY0, IX1, IY1, CVAL, BX, BY, %VAL(PIXMAP))
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
      IF (PIXMAP.NE.0)
     :     CALL GRPP04(NBUF, RBUF, BX, BY, %VAL(PIXMAP), CTABLE)
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

**GRPP01 -- PGPLOT PPM driver, draw line
*+
      SUBROUTINE GRPP01 (IX0, IY0, IX1, IY1, ICOL, BX, BY, PIXMAP)
      INTEGER IX0, IY0, IX1, IY1
      INTEGER ICOL, BX, BY, PIXMAP(BX,BY)
*
* Draw a straight-line segment from absolute pixel coordinates
* (IX0, IY0) to (IX1, IY1).
*
* Arguments:
*  IX0, IY0        (input): Starting point of line.
*  IX1, IY1        (input): End point of line.
*  ICOL            (input): Color value.
*  BX, BY          (input): dimensions of PIXMAP.
*  PIXMAP   (input/output): The image data buffer.
*-
*  9-Aug-1994 - Recreated by Remko Scharroo from GRGI01 by Ge van Geldorp
*               Improved algorithm.
*-----------------------------------------------------------------------
      INTEGER IX, IY, IS
      REAL    D
C
      IF (IX0.EQ.IX1 .AND. IY0.EQ.IY1) THEN
         PIXMAP(IX0,IY0)=ICOL
      ELSE IF (ABS(IY1-IY0).GT.ABS(IX1-IX0)) THEN
         D=(IX1-IX0)/REAL(IY1-IY0)
         IS=1
         IF (IY1.LT.IY0) IS=-1
         DO 10 IY=IY0,IY1,IS
            IX=NINT(IX0+(IY-IY0)*D)
            PIXMAP(IX,IY)=ICOL
 10      CONTINUE
      ELSE
         D=(IY1-IY0)/REAL(IX1-IX0)
         IS=1
         IF (IX1.LT.IX0) IS=-1
         DO 20 IX=IX0,IX1,IS
            IY=NINT(IY0+(IX-IX0)*D)
            PIXMAP(IX,IY)=ICOL
 20      CONTINUE
      ENDIF
      END

**GRPP02 -- PGPLOT PPM driver, write PPM image
*+
      SUBROUTINE GRPP02 (UNIT, BX, BY, PIXMAP)
      INTEGER UNIT, BX, BY, PIXMAP(*)
*
* This routine copies the image buffer to an output buffer. A PPM image
* header and the output buffer are written to UNIT.
*
* Arguments:
*  UNIT    (input): Output unit
*  BX, BY  (input): Image size
*  PIXMAP  (input): Image data
*-
* 10-Aug-1993 - Created by Remko Scharroo
* 16-Nov-1994 - Rewritten by T. Pearson
*-----------------------------------------------------------------------
*       BUFSIZ must be a multiple of 3 (and not larger than 500 for
*       portability to AIX)
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=498)
      INTEGER I, N, IER, IBUF, L1, L2
      INTEGER GRTRIM, GRWFCH
      CHARACTER*128 HEAD
      CHARACTER*20 USER
      CHARACTER*20 TODAY
      CHARACTER*(BUFSIZ) BUF
      LOGICAL BAD
*
*     Write the header (magic number = P6)
*
      CALL GRUSER(USER,L1)
      CALL GRDATE(TODAY,L2)
      WRITE (HEAD, 1000) USER(:L1), TODAY(:L2),CHAR(10),
     :                   BX, BY, CHAR(10), 255, CHAR(10)
 1000 FORMAT ('P6 # PGPLOT PPM image ',A,1X,A,A1, I5,1X,I5,A1, I3,A1)
      N = GRTRIM(HEAD)
      IER = GRWFCH(UNIT, HEAD(1:N))
      IF (IER.NE.N) CALL GRWARN('Failed writing PPM header')
*
*     Write the pixel data as R,G,B components
*
      N=BX*BY
      IBUF=0
      BAD = .FALSE.
      DO 10 I=1,N
         BUF(IBUF+1:IBUF+1) = CHAR(MOD(PIXMAP(I),256))
         BUF(IBUF+2:IBUF+2) = CHAR(MOD(PIXMAP(I)/256,256))
         BUF(IBUF+3:IBUF+3) = CHAR(PIXMAP(I)/(256*256))
         IBUF = IBUF+3
         IF (IBUF.GE.BUFSIZ) THEN
            IER = GRWFCH(UNIT, BUF)
            IF (IER.NE.IBUF) BAD = .TRUE.
            IBUF = 0
         END IF
 10   CONTINUE
      IF (IBUF.GT.0) THEN
         IER = GRWFCH(UNIT, BUF(:IBUF))
         IF (IER.NE.IBUF) BAD = .TRUE.
         IBUF = 0
      END IF
      IF (BAD) CALL GRWARN('Failed writing PPM data')
      END

**GRPP03 -- PGPLOT PPM driver, fill rectangle
*+
      SUBROUTINE GRPP03 (IX0, IY0, IX1, IY1, ICOL, BX, BY, PIXMAP)
      INTEGER IX0, IY0, IX1, IY1
      INTEGER ICOL, BX, BY, PIXMAP(BX,BY)
*
* Arguments:
*  IX0, IY0        (input): Lower left corner.
*  IX1, IY1        (input): Upper right corner.
*  ICOL            (input): Color value.
*  BX, BY          (input): dimensions of PIXMAP.
*  PIXMAP   (input/output): The image data buffer.
*-----------------------------------------------------------------------
      INTEGER IX, IY
C
      DO 20 IY=IY0,IY1
         DO 10 IX=IX0,IX1
            PIXMAP(IX,IY) = ICOL
 10      CONTINUE
 20   CONTINUE
      END

**GRPP04 -- PGPLOT PPM driver, fill image line
*+
      SUBROUTINE GRPP04(NBUF,RBUF,BX,BY,PIXMAP,CTABLE)
      INTEGER I,J,NBUF,BX,BY,N,IC
      REAL RBUF(NBUF)
      INTEGER PIXMAP(BX,BY)
      INTEGER CTABLE(3,0:255)
      INTEGER R, G, B
*-
      I = NINT(RBUF(1))+1
      J = BY-NINT(RBUF(2))
      DO 10 N=3,NBUF
         IC=RBUF(N)
         R = CTABLE(1,IC)
         G = CTABLE(2,IC)
         B = CTABLE(3,IC)
         PIXMAP(I+N-3,J) = R + 256*(G + 256*B)
 10   CONTINUE
      END

**GRPP10 -- Replace # in filename by picture number
*
      SUBROUTINE GRPP10 (NAME1, NP, NAME2)
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
         CALL GRFAO('pgplot#.ppm', L, TMP, NP, 0, 0, 0)
      END IF
      CALL GRWARN ('Writing new PPM image as: '//TMP(:L))
      NAME2 = TMP(:L)
      END
