C*LHDRIV -- PGPLOT device driver for MS-DOS machines running Lahey F77
C 32-bit FORTRAN
C+
      SUBROUTINE LHDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      IMPLICIT NONE
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for IBM PC's and clones running Lahey F77 32-bit Fortran v5.0.
C This driver will put the display into graphics mode, and calls to 'close'
C the workstation will set it back into the previous mode (generally erasing
C the display, so don't do it until you are really finished).
C
C This routine must be compiled and linked with the Lahey graphics
C library GRAPH3 supplied with Lahey Fortran v4.0 or greater.
C
C Microsoft FORTRAN versions:
C 1989-Nov-03 - Started work [AFT]
C 1989-Apr-06 - Improved version [AFT]
C 1991-Mar-13 - Added cursor routine [JHT]
C Lahey FORTRAN versions:
C 1991-Dec-28 - derived from Microsoft version [PAH]
C-----------------------------------------------------------------------
C
C Supported device: IBM PC's and compatables
C
C Device type code: /LH
C
C Default device name: None (the device name, if specified, is
C ignored).
C
C Default view surface dimensions: Depends on monitor, typical 7x10 inches
C
C Resolution: Depends on graphics card.  Tested with a 640x480 VGA card.
C    Driver should work with other graphics cards, however, expect to
C    tweak it a bit.
C
C Color capability: Color indices 0-15 are accepted.  This version maps
C    the PGPLOT color indices into the IBM color indices for with the
C    default color most closely corresponds to the PGPLOT default color.
C    Thus, PGPLOT index 2 (red) maps to IBM index 12 (light red).
C
C Input capability: Graphics cursor implemented using Microsoft Mouse
C    or compatible, accessed through DOS calls.
C
C File format: None.
C
C Obtaining hardcopy: Not possible.
C-----------------------------------------------------------------------
      CHARACTER CMSG*10
      REAL A,B
      real       xfac, yfac
      parameter (xfac=11.0/640.0, yfac=8.5/480.0)
      integer    pencolour, pend, colourmap(0:15), ix, iy
      logical    got_mouse
      save       pencolour, pend

      DATA colourmap/ 0,15,12,10, 9,11,13,14, 6, 2, 3, 1, 5, 4, 8, 7/

C---
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,240,250,260) IFUNC
  900 WRITE (CMSG, '(I10)') IFUNC
      CALL GRWARN('Unimplemented function in LAHEY device driver: '/
     :      /CMSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
   10 CHR = 'LH'
      LCHR = 2
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
   20 CONTINUE
      RBUF(1) = 0
      RBUF(2) = 640.0
      RBUF(3) = 0
      RBUF(4) = 480.0
      RBUF(5) = 0
      RBUF(6) = 15.0
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C Divide the number of pixels on screen by a typical screen size in
C inches.
C
   30 continue
      A = 640.0/9.5
      RBUF(1) = A
      B = 480.0/7.5
      RBUF(2) = B
      RBUF(3) = 1.0
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Interactive, No cursor, No dashed lines, No area fill,
C    No thick lines, No rectangle fill, No pixel primitives,)
C
   40 continue
      if (got_mouse()) then
        CHR = 'ICNNNNNNNN'
      else
        CHR = 'INNNNNNNNN'
      endif
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name. ------------------------------
C
   50 CHR = ' '
      LCHR = 1
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot. ------------------
C
   60 CONTINUE
      RBUF(1) = 0
      RBUF(2) = 640.0
      RBUF(3) = 0
      RBUF(4) = 480.0
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults. ----------------------------------
C
   70 RBUF(1) = 1
      NBUF = 1
      RETURN
C
C--- IFUNC = 8, Select plot. -------------------------------------------
C
   80 CONTINUE
      RETURN
C
C--- IFUNC = 9, Open workstation. --------------------------------------
C
   90 CONTINUE
      RBUF(1) = 0
      RBUF(2) = 1
      NBUF = 2
      IF(RBUF(3) .NE. 0.0) THEN
         PEND=1
      ELSE
         PEND=0
      END IF
      CALL PLOTS(0, 0, 18)
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
      CALL PLOT(0, 0, 999)
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
      IF(PEND.EQ.0) THEN
         CALL PLOT(0, 0, -999)
      ENDIF
      PEND=0
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C
  120 CONTINUE
      CALL PLOT(RBUF(1)*xfac, RBUF(2)*yfac, 3)
      CALL PLOT(RBUF(3)*xfac, RBUF(4)*yfac, 2)
      RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
  130 CONTINUE
      CALL SETPIX(RBUF(1)*xfac, RBUF(2)*yfac, pencolour)
      RETURN
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
      IF (RBUF(1) .NE. 0.0) THEN
        CALL PLOT(0.0, 0.0, -999)
      ENDIF
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
  150 CONTINUE
      pencolour=MIN( MAX(0,NINT(RBUF(1))) ,15)
      pencolour=colourmap(pencolour)
      CALL NEWPEN(pencolour)
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C
C
  170 CONTINUE
      ix = nint(rbuf(1))
      iy = 479-nint(rbuf(2))
      call show_mouse
      call put_mouse(ix,iy)
      call cursor_key(ix,iy,chr)
      rbuf(1) = ix
      rbuf(2) = 479-iy
      call hide_mouse
      RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C
  180 CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C
  190 CONTINUE
      RETURN
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C
  200 CONTINUE
      RETURN
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C
  220 CONTINUE
      RETURN
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
  230 CONTINUE
      RETURN
C
C--- IFUNC=24, Rectangle fill. -----------------------------------------
C
  240 CONTINUE
      RETURN
C
C--- IFUNC=25, Set fill pattern. ---------------------------------------
C
  250 CONTINUE
      RETURN
C
C--- IFUNC=26, Line of pixels. -----------------------------------------
C
  260 CONTINUE
      RETURN
C-----------------------------------------------------------------------
      END



      logical function got_mouse()
      implicit none
      integer*2          ntrup
      integer            intary(9), eax
      equivalence        (eax,intary(1))
      eax = 0
      ntrup = 51
      call intrup(intary,ntrup)
      got_mouse = (eax .eq. 65535)
      return
      end

      subroutine show_mouse
      implicit none
      integer*2 ntrup
      integer            intary(9), eax
      equivalence        (eax,intary(1))
      eax = 1
      ntrup = 51
      call intrup(intary,ntrup)
      return
      end

      subroutine hide_mouse
      implicit none
      integer*2 ntrup
      integer            intary(9), eax
      equivalence        (eax,intary(1))
      eax = 2
      ntrup = 51
      call intrup(intary,ntrup)
      return
      end

      subroutine get_mouse(ix, iy, button)
      implicit none
      integer ix, iy, button
      integer*2 ntrup
      integer            intary(9), eax
      integer*2          bx, cx, dx
      equivalence        (eax,intary(1)),
     -                   (bx,intary(2)),
     -                   (cx,intary(3)),
     -                   (dx,intary(4))
      eax = 3
      ntrup = 51
      call intrup(intary,ntrup)
      ix = cx
      iy = dx
      button = bx
      return
      end

      subroutine put_mouse(ix, iy)
      implicit none
      integer ix, iy
      integer*2 ntrup
      integer            intary(9), eax
      integer*2          bx, cx, dx
      equivalence        (eax,intary(1)),
     -                   (bx,intary(2)),
     -                   (cx,intary(3)),
     -                   (dx,intary(4))
      eax = 4
      cx = ix
      dx = iy
      ntrup = 51
      call intrup(intary, ntrup)
      return
      end

      subroutine cursor_key(ix, iy, key)
      implicit none
      character*(*) key
      integer ix, iy, ikey, ib
      integer*2 ixkey
      ikey = ixkey()
      key = char(ikey)
      call get_mouse(ix, iy, ib)
      return
      end
