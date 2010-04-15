CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_GEN.F    General display s/rs for Unix
C       ( These take no actions - just point to device specific s/rs)
C
C
C DS_INIT       Initialise display
C DS_CLOSE      Close display
C DS_PTTIT      Put image title on display
C DS_SCOL       Set overlay line width and colour
C DS_SPEC_SDEF  Device specific defaults
C DS_GTYPE      Get type of display
C
C DS_OVAL       Put oval on display overlay
C DS_CROSS      Put cross on display overlay
C DS_LINE       Put line on overlay display
C
C DS_SCUR       Start the cursor
C DS_GCUR       Get cursor position on button
C DS_GPCUR      Get cursor position
C DS_GTCUR      Get cursor position on button (panel or type out during)
C DS_OVCUS      Pan an oval cursor and get position
C DS_PCUR       Put cursor position
C DS_PSCUR      Put cursor at screen position
C DS_ERASE      Erase display
C DS_ZOOM       Zoom/pan display
C
C DS_BLINK      Blink two images
C DS_ACIM(RS)   Display part of actual (real:int*2) image


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_INIT -- Initialise display
C
C    alan penny                ral              1990-01-31

      subroutine ds_init ( title, kscreen, ierr )

      implicit none
      include 'ST_DS_GEN_INC'

      character*(*) title	!i: Title to put up
      integer	    kscreen     !i: Ask for screen size flag (0=yes; 1=no)
      integer	    ierr	!o: Error flag (0=ok; 1=bad)
C--
Cbegin


      if ( DSTYPE.eq.4 ) call dsx_init ( title, kscreen, ierr )

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_CLOSE -- Close display
C
C    alan penny                ral              1990-01-31

      subroutine ds_close ( ierr )

      implicit none
      include 'ST_DS_GEN_INC'

      integer	   ierr		!o: Error flag (0=ok)
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( DSTYPE.eq.4 ) call dsx_close ( ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_PTTIT -- Put image title on display
C
C    alan penny                ral              1990-01-31

      subroutine ds_pttit ( title )

      implicit none
      include 'ST_DS_GEN_INC'

      character*(*) title	!i: Title to put up
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( DSTYPE.eq.4 ) call dsx_pttit ( title )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_SCOL -- Set overlay line width and colour
C
C    alan penny                ral              1990-01-31

      subroutine ds_scol ( w, kc )

      implicit none
      include 'ST_DS_GEN_INC'

      real     w		!i: Line width
      integer  kc		!i: Colour (1=red;2=green;3=blue;4=yellow;
				!           5=cyan;6=mauve;7=tan;8=pink)
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( DSTYPE.eq.4 ) call dsx_scol ( w, kc )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_SPEC_SDEF -- Device specific defaults
C
C    alan penny                ral              1990-01-31

      subroutine ds_spec_sdef ()

      implicit none
      include 'ST_DS_GEN_INC'

C--
Cbegin


      if ( DSTYPE.eq.2 ) return
      if ( DSTYPE.eq.4 ) call dsx_id_init


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_GTYPE -- Get type of display
C
C    alan penny                ral              1990-01-31

      subroutine ds_gtype ( ierr )

      implicit none
      include 'ST_DS_GEN_INC'

      integer   ierr		!o: Error flag (ok=0;bad=1)
C--
Cbegin


      DSTYPE = 4

      ierr = 0
      DSZOOMMAX = 64


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_OVAL -- Put oval on display overlay
C
C    alan penny                ral              1990-01-31

      subroutine ds_oval (  x, y, rx, angle, elli, kc )

      implicit none
      include 'ST_DS_GEN_INC'

      real     x		!i: X position of oval in image coords
      real     y		!i: Y position of oval in image coords
      real     rx		!i: Oval major radius in image coords
      real     angle		!i: Oval angle to X-axis
      real     elli		!i: Oval ellipticity
      integer  kc		!i: Colour (1=red;2=green;3=blue;4=yellow;
				!           5=cyan;6=mauve;7=tan;8=pink)
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( DSTYPE.eq.4 ) call dsx_oval ( x, y, rx, angle, elli, kc )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_CROSS -- Put cross on display overlay
C
C    alan penny                ral              1990-01-31

      subroutine ds_cross ( x, y, w, kc )

      implicit none
      include 'ST_DS_GEN_INC'

      real     x		!i: X position of cross in image coords
      real     y		!i: Y position of cross in image coords
      real     w		!i: Width of cross in image coords
      integer  kc		!i: Colour (1=red;2=green;3=blue;4=yellow;
				!           5=cyan;6=mauve;7=tan;8=pink)
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( DSTYPE.eq.4 ) call dsx_cross ( x, y, w, kc )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LINE -- Put line on overlay display
C
C    alan penny                ral              1990-01-31

      subroutine ds_line ( xs, ys, xe, ye, kc )

      implicit none
      include 'ST_DS_GEN_INC'

      real     xs		!i: X start of line in image coords
      real     ys		!i: Y start of line in image coords
      real     xe		!i: X end of line in image coords
      real     ye		!i: Y end of line in image coords
      integer  kc		!i: Colour (1=red;2=green;3=blue;4=yellow;
				!           5=cyan;6=mauve;7=tan;8=pink
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( DSTYPE.eq.4 ) call dsx_line ( xs, ys, xe, ye, kc )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_SCUR -- Start the cursor
C
C    alan penny                ral              1990-01-31

      subroutine ds_scur ( )

      implicit none
      include 'ST_DS_GEN_INC'

C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( DSTYPE.eq.4 ) call dsx_scur

      DSSCUR = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_GCUR -- Get cursor position on button
C
C    alan penny                ral              1990-01-31

      subroutine ds_gcur ( monitor, jx, jy, kbut, ierr )

      implicit none
      include 'ST_DS_GEN_INC'

      logical	monitor		!i: Display cursor po, val on terminal/panel?
      integer   jx		!o: X position of cursor
      integer   jy		!o: Y position of cursor
      integer   kbut		!o: Number ofbutton pressed
      integer   ierr		!o: Error flag (ok=0;bad=1)
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( .not.DSSCUR ) call ds_scur					!Start cursor?

      if ( DSTYPE.eq.4 ) call dsx_gcur ( monitor, jx, jy, kbut, ierr )

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_GPCUR -- Get cursor position
C
C    alan penny                ral              1990-01-31

      subroutine ds_gpcur ( kx, ky, ierr )

      implicit none
      include 'ST_DS_GEN_INC'

      integer   kx		!o: X position of cursor
      integer   ky		!o: Y position of cursor
      integer   ierr		!o: Error flag (ok=0;bad=1)
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( .not.DSSCUR ) call ds_scur					!Start cursor?

      if ( DSTYPE.eq.4 ) call dsx_gpcur ( kx, ky, ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_GTCUR -- Get cursor position on button (panel or type out during)
C
C    alan penny                ral              1990-01-31

      subroutine ds_gtcur ( monitor, jx, jy, kbut, ierr )

      implicit none
      include 'ST_DS_GEN_INC'

      logical	monitor		!i: Display cursor po, val on terminal/panel?
      integer   jx		!o: X position of cursor
      integer   jy		!o: Y position of cursor
      integer   kbut		!o: Number ofbutton pressed
      integer   ierr		!o: Error flag (ok=0;bad=1)
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( .not.DSSCUR ) call ds_scur					!Start cursor?

      if ( DSTYPE.eq.4 ) call dsx_gtcur ( monitor, jx, jy, kbut, ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_OVCUS -- Pan an oval cursor and get position
C
C    alan penny                ral              1990-01-31

      subroutine ds_ovcus ( monitor, rx1, kc1, rx2, kc2,
     +                      apang, elli, kt, kx, ky, kbut )

      implicit none
      include 'ST_DS_GEN_INC'

      logical  monitor		!i: Display cursor po, val on terminal/panel?
      real     rx1		!i/o: Image pixel inner X radius (0.0=none)
      integer  kc1		!i: Colour (1=red;2=green;3=blue;4=yellow;
				!           5=cyan;6=mauve;7=tan;8=pink)
      real     rx2(2)		!i/o: Image pixel inner/outer X annulus radii (0.0=none)
      integer  kc2		!i: Colour (1=red;2=green;3=blue;4=yellow;
				!           5=cyan;6=mauve;7=tan;8=pink)
      real     apang		!i: Angle of oval aperture to X-axis in degrees
      real     elli		!i: Ellipticity of oval
      integer  kt		!i: Change radii flag (0=no;1=inner;2=inner
				!           annulus;3=outer annulus)
      integer  kx		!o: Image Y position
      integer  ky		!o: Image X position
      integer  kbut		!o: Button pressed
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( .not.DSSCUR ) call ds_scur					!Start cursor?

      if ( DSTYPE.eq.4 ) call dsx_ovcus ( monitor, rx1, kc1, rx2,
     +                                    kc2, apang, elli,
     +                                    kt, kx, ky, kbut )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_PCUR -- Put cursor position
C
C    alan penny                ral              1990-01-31

      subroutine ds_pcur ( kx, ky )

      implicit none
      include 'ST_DS_GEN_INC'

      integer   kx		!i: X position to put cursor
      integer   ky		!i: Y position to put cursor
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( .not.DSSCUR ) call ds_scur				!Start cursor?

      if ( DSTYPE.eq.4 ) call dsx_pcur ( kx, ky )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_PSCUR -- Put cursor at screen position
C
C    alan penny                ral              1990-01-31

      subroutine ds_pscur ( x, y )

      implicit none
      include 'ST_DS_GEN_INC'

      real	x		!i: X screen position to put cursor
      real	y		!i: Y screen position to put cursor
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( .not.DSSCUR ) call ds_scur				!Start cursor?

      if ( DSTYPE.eq.4 ) call dsx_pscur ( x, y )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_ERASE -- Erase display
C
C    a j penny                    ral         1990 jan

      subroutine ds_erase ()

      implicit none
      include 'ST_DS_GEN_INC'
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( DSTYPE.eq.4 ) call dsx_erase


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_ZOOM -- Zoom/pan display
C
C    alan penny           ral                       1990-02-01

      subroutine ds_zoom ( monitor, kopt, kopta )

      implicit none
      include 'ST_DS_GEN_INC'

      logical	 monitor	!i: Display cursor po, val on terminal/panel?
      integer    kopt		!i: Zoom type (0=mouse control;1=reset to null)
      integer    kopta		!i: Start/stop meesage flag (0=no;1=yes)
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( .not.DSSCUR ) call ds_scur					!Start cursor?

      if ( DSTYPE.eq.4 ) call dsx_zoom ( monitor, kopt, kopta )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_BLINK -- Blink two images
C
C  alan penny           ral                         1990-06-09

      subroutine ds_blink ( frames, period, fraction )

      implicit none

      include 'ST_DS_GEN_INC'

      integer	frames(2)	!i: Which image frames to blink
      real	period		!i: Blinking period
      real	fraction	!i:
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( DSTYPE.eq.4 ) call dsx_blink ( frames, period, fraction )


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_ACIMS -- Display part of actual int*2 image
C
C    alan penny           ral                       1990-02-01

      subroutine ds_acims ( im, mx, my, nxs, nxe, nys, nye, ix, iy,
     +                      wrap )

      implicit none
      include 'ST_DS_GEN_INC'

      integer   mx		!i: X size of image
      integer   my		!i: Y size of image
      integer*2 im(mx,my)	!i: image
      integer   nxs		!i: X start in image
      integer   nxe		!i: X end   in image
      integer   nys		!i: Y start in image
      integer   nye		!i: Y end   in image
      integer   ix		!i: X posn in virtual image of blh
      integer   iy		!i: Y posn in virtual image of blh
      logical   wrap		!i: Flag to wrap values round display limits
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( DSTYPE.eq.4 ) call dsx_acims ( im, mx, my, nxs, nxe, nys,
     +                                     nye, ix, iy, wrap )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_ACIMR -- Display part of actual real image
C
C    alan penny           ral                       1990-02-01

      subroutine ds_acimr ( im, mx, my, nxs, nxe, nys, nye, ix, iy,
     +                       wrap )

      implicit none
      include 'ST_DS_GEN_INC'

      integer   mx		!i: X size of image
      integer   my		!i: Y size of image
      real      im(mx,my)	!i: image
      integer   nxs		!i: X start in image
      integer   nxe		!i: X end   in image
      integer   nys		!i: Y start in image
      integer   nye		!i: Y end   in image
      integer   ix		!i: X posn in virtual image of blh
      integer   iy		!i: Y posn in virtual image of blh
      logical   wrap		!i: Flag to wrap values round display limits
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( DSTYPE.eq.4 ) call dsx_acimr ( im, mx, my, nxs, nxe, nys,
     +                                     nye, ix, iy, wrap )


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_GEN_A.F      General display s/rs (internal to DS - no outside calls)
C
C DS_SDEF         Set defaults
C DS_SETUP        Set null display
C DS_DISP         Set up and display image
C DS_DISP(RS)     Set up and display image  (real:int*2)
C DS_IMGSCL       Get int*2/int/real array value scale for display (unscaled by  BS,BZ)
C DS_IMGSCL(IRS)  Get int*2/int/real array value scale for display (unscaled by  BS,BZ)
C DS_DOFLASH      Display image, scaled to max contrast, titled
C DS_DOFLASH(RS)  Display image, scaled to max contrast, titled  (real:int*2)
C DS_DODISP       Display image, scaled, titled
C DS_DODISP(RS)   Display image, scaled, titled  (real:int*2)
C DS_BOX          Put box on display overlay
C DS_SPOT         Put spot on display overlay
C DS_ACIM         Display part of actual (int*2/real)image
C DS_GTCOMF       Get image compression factor and displayed image size
C
C DS_TIV          Translate image to screen coords
C DS_TVI          Translate virtual to image coords
C DS_VTYPE        Type posn, value



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_SDEF -- Set defaults
C
C    alan penny           ral                       1990-02-01

      subroutine ds_sdef ( npx, npy )

      implicit none
      include 'ST_DS_GEN_INC'
      include 'ST_DS_PANEL_INC'
      include 'lut.inc'

      integer     npx		!i: No of columns in any panel
      integer     npy		!i: No of rows in any panel
C--
Cbegin


      call ds_setup

      call ds_spec_sdef

      DSCOMFX = 1
      DSCOMFY = 1
      DSNXS   = 1
      DSNXE   = 1
      DSNYS   = 1
      DSNYE   = 1
      DSIXS   = 1
      DSIYS   = 1
      DSSNX   = 1
      DSSNY   = 1
      DSCRSL  = 4.0

      DSKVRANGE = 0

      DSVMIN = 0.0
      DSVMAX = 1.0
      DSWRAP = .false.

      DSZM   = 1
      DSZPX  = 1
      DSZPY  = 1

      DSWINDX  = 5.0
      DSWINDY  = 5.0
      DSWINDXM = 0.0

      LUT_NUM    = 1
      LUT_ENDS   = 1
      LUT_FLIPPED = .false.
      LUT_SC     = 1.0
      LUT_ZE     = 0.0
      LUT_STORED = 8

      PNSNX = npx*91 					!Panel setup
      PNSNY = npy*26


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_SETUP -- Set null display
C
C    alan penny           ral                       1990-02-01

      subroutine ds_setup ( )

      implicit none
      include 'ST_DS_GEN_INC'
      include 'ST_DS_PANEL_INC'
C--
Cbegin


      DSTYPE = 1
      DSOPEN = .false.
      PDSOPEN = .false.

      DSSCUR = .false.

      DOPANEL = .false.
      DOHPANEL = .false.
      PDSTYPE = 0
      PDNUML = 0

      PNSNX = 0
      PNSNY = 0


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_DISP -- Set up and display image
C
C  alan penny                  ral         1990 jan

      subroutine ds_disp ( im, mx, my, type, kopt )

      implicit none

      include 'ST_DS_GEN_INC'
      include 'ST_IMAGE_INC'

      integer        mx			!i: Image X size
      integer        my			!i: Image Y size
      integer*2      im(2)		!i: Short Image
      real           imr                !i: Real image
      character*(*)  type		!i: Image type ("REAL', 'SHORT' )
      integer        kopt		!i: 0=Flash,no type; 1=Flash; 2=Display
      integer*2      dummy(2)
      equivalence (imr, dummy)
C--
Cbegin
      dummy(1) = im(1)
      dummy(2) = im(2)

      if ( type.eq.'REAL' ) then
         call ds_dispr ( imr, mx, my, kopt )
      elseif ( type.eq.'SHORT' ) then
         call ds_disps ( im, mx, my, kopt )
      else
         call printo ( ' ERROR: Programmer Error in S/R DS_DISP' )
         call printo ( '        Code needs rewriting: Contact'//
     +                 '  person who wrote the program' )
       endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_DISPR -- Set up and display real image
C
C  alan penny                  ral         1990 jan

      subroutine ds_dispr ( im, mx, my, kopt )

      implicit none

      include 'ST_DS_GEN_INC'
      include 'ST_IMAGE_INC'

      integer   mx 			!i: Image X size
      integer   my			!i: Image Y size
      real      im(mx,my)		!i: Image
      integer   kopt			!i: 0=Flash,no type; 1=Flash; 2=Display
C--
      logical wrap, bval
      real pvlo, pvhi
Cbegin

      if ( .not.DSOPEN ) return

      if ( kopt.eq.1 ) then						!Type display value range
         pvlo = DSVMIN*BS + BZ
         pvhi = DSVMAX*BS + BZ
         call pargr ( pvlo )
         call pargr ( pvhi )
         call printd ( 'Display contrast: low = %f : high = %f' )
      endif

      if ( kopt.eq.2 ) then						!Get display value range
         pvlo = DSVMIN*BS + BZ
         pvhi = DSVMAX*BS + BZ
         call pargr ( pvlo )
         call pargr ( pvhi )
         call printd (
     +    'Suggested Display contrast: low = %f : high = %f' )
         call get2r ( 'VRANGE', pvlo, pvhi, .true., -1.0e10, 1.0e10 )
         DSVMIN = 0.0
         DSVMAX = 1.0
         if ( BS.ne.0.0 ) then
            DSVMIN = (pvlo-BZ)/BS
            DSVMAX = (pvhi-BZ)/BS
         endif
      endif

      if ( DSVMIN.eq.DSVMAX ) then
         DSVMAX = DSVMAX + 1.0
         DSVMIN = DSVMIN - 1.0
      endif

      bval = DSWRAP
      if ( kopt.eq.2 ) call get1b ( 'WRAP', DSWRAP, bval )		!Get display value wrapping

      DSIXS = (real(DSSNX)/2.0) - 					!Get position in display
     +        (((real(DSNXE-DSNXS+1)-1.0)/real(DSCOMFX))/2.0) + 0.6
      DSIYS = (real(DSSNY)/2.0) -
     +        (((real(DSNYE-DSNYS+1)-1.0)/real(DSCOMFY))/2.0) + 0.6
      if ( kopt.eq.2 ) call get2i ( 'IMPOSN', DSIXS, DSIYS, .true.,
     +                              -100000, 100000 )

      wrap = .false.
      if ( kopt.eq.2 ) wrap = DSWRAP
      call ds_acimr ( im, mx, my, DSNXS, DSNXE, DSNYS, DSNYE, DSIXS,	!Display image
     +                DSIYS, wrap )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_DISPS -- Set up and display short image
C
C  alan penny                  ral         1990 jan

      subroutine ds_disps ( im, mx, my, kopt )

      implicit none

      include 'ST_DS_GEN_INC'
      include 'ST_IMAGE_INC'

      integer   mx 			!i: Image X size
      integer   my			!i: Image Y size
      integer*2 im(mx,my)		!i: Image
      integer   kopt			!i: 0=Flash,no type; 1=Flash; 2=Display
C--
      logical wrap, bval
      real pvlo, pvhi
Cbegin


      if ( .not.DSOPEN ) return

      if ( kopt.eq.1 ) then						!Type display value range
         pvlo = DSVMIN*BS + BZ
         pvhi = DSVMAX*BS + BZ
         call pargr ( pvlo )
         call pargr ( pvhi )
         call printd ( 'Display contrast: low = %f : high = %f' )
      endif

      if ( kopt.eq.2 ) then						!Get display value range
         pvlo = DSVMIN*BS + BZ
         pvhi = DSVMAX*BS + BZ
         call pargr ( pvlo )
         call pargr ( pvhi )
         call printd (
     +    'Suggested Display contrast: low = %f : high = %f' )
         call get2r ( 'VRANGE', pvlo, pvhi, .true., -1.0e10, 1.0e10 )
         DSVMIN = 0.0
         DSVMAX = 1.0
         if ( BS.ne.0.0 ) then
            DSVMIN = (pvlo-BZ)/BS
            DSVMAX = (pvhi-BZ)/BS
         endif
      endif

      if ( DSVMIN.eq.DSVMAX ) then
         DSVMAX = DSVMAX - 1.0
         DSVMIN = DSVMIN + 1.0
      endif

      bval = DSWRAP
      if ( kopt.eq.2 ) call get1b ( 'WRAP', DSWRAP, bval )		!Get display value wrapping

      DSIXS = (real(DSSNX)/2.0) - 					!Get position in display
     +        (((real(DSNXE-DSNXS+1)-1.0)/real(DSCOMFX))/2.0) + 0.5
      DSIYS = (real(DSSNY)/2.0) -
     +        (((real(DSNYE-DSNYS+1)-1.0)/real(DSCOMFY))/2.0) + 0.5
      if ( kopt.eq.2 ) call get2i ( 'IMPOSN', DSIXS, DSIYS, .true.,
     +                              -100000, 100000 )

      wrap = .false.
      if ( kopt.eq.2 ) wrap = DSWRAP
      call ds_acims ( im, mx, my, DSNXS, DSNXE, DSNYS, DSNYE, DSIXS,	!Display image
     +                DSIYS, wrap )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_IMGSCL -- Get array value scale for display (unscaled by  BS,BZ)
C CL parameter accessed:-
C    VRANGE             Low and high limits of the pixel value display range
C
C    a j penny                    ral         1990 jan

      subroutine ds_imgscl ( im, mx, my, type, kxs, kxe, kys, kye )

      implicit none

      include 'ST_DS_GEN_INC'
      include 'ST_IMAGE_INC'

      integer	    mx		!i: X image size
      integer	    my		!i: Y image size
      integer*2	    im(*)	!i: Image
      character*(*) type	!i: Image type ('REAL','INT','SHORT')
      integer       kxs		!i: X start of area to look at
      integer	    kxe		!i: X   end of area to look at
      integer       kys		!i: Y start of area to look at
      integer       kye		!i: Y   end of area to look at
C--
Cbegin


      if ( type.eq.'REAL' ) then
         call ds_imgsclr ( im, mx, my, kxs, kxe, kys, kye )
      elseif ( type.eq.'INT' ) then
         call ds_imgscli ( im, mx, my, kxs, kxe, kys, kye )
      elseif ( type.eq.'SHORT' ) then
         call ds_imgscls ( im, mx, my, kxs, kxe, kys, kye )
      else
         call printo ( ' ERROR: Programmer Error in S/R DS_IMGSCL' )
         call printo ( '        Code needs rewriting: Contact'//
     +                 '  person who wrote the program' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_IMGSCLI -- Get integer array value scale for display (unscaled by  BS,BZ)
C CL parameter accessed:-
C    VRANGE             Low and high limits of the pixel value display range
C
C    a j penny                    ral         1990 jan

      subroutine ds_imgscli ( im, mx, my, kxs, kxe, kys, kye )

      implicit none

      include 'ST_DS_GEN_INC'
      include 'ST_IMAGE_INC'

      integer	mx		!i: X image size
      integer	my		!i: Y image size
      integer	im(mx,my)	!i: Image
      integer	kxs		!i: X start of area to look at
      integer	kxe		!i: X   end of area to look at
      integer	kys		!i: Y start of area to look at
      integer	kye		!i: Y   end of area to look at
C--
      integer istat, kxr(2), kyr(2)
      real am, std
Cbegin


      kxr(1) = kxs
      kxr(2) = kxe
      kyr(1) = kys
      kyr(2) = kye
      call rangei ( im, mx, my, kxr, kyr, INVAL, am, std, istat )       !Get display range
      DSVMIN = am + 3.0*std
      DSVMAX = am - 2.0*std
      if ( DSVMIN.eq.DSVMAX ) then
         DSVMAX = DSVMAX - 1.0
         DSVMIN = DSVMIN + 1.0
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_IMGSCLR -- Get real array value scale for display (unscaled by  BS,BZ)
C CL parameter accessed:-
C    VRANGE             Low and high limits of the pixel value display range
C
C    a j penny                    ral         1990 jan
      subroutine ds_imgsclr ( im, mx, my, kxs, kxe, kys, kye )

      implicit none

      include 'ST_DS_GEN_INC'
      include 'ST_IMAGE_INC'

      integer	mx		!i: X image size
      integer	my		!i: Y image size
      real	im(mx,my)	!i: Image
      integer	kxs		!i: X start of area to look at
      integer	kxe		!i: X   end of area to look at
      integer	kys		!i: Y start of area to look at
      integer	kye		!i: Y   end of area to look at
C--
      integer istat, kxr(2), kyr(2)
      real am, std
Cbegin


      kxr(1) = kxs
      kxr(2) = kxe
      kyr(1) = kys
      kyr(2) = kye
      call ranger ( im, mx, my, kxr, kyr, RINVAL, am, std, istat )	!Get display range
      DSVMIN = am + 3.0*std
      DSVMAX = am - 2.0*std
      if ( DSVMIN.eq.DSVMAX ) then
         DSVMAX = DSVMAX - 1.0
         DSVMIN = DSVMIN + 1.0
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_IMGSCLS -- Get int*2 array value scale for display (unscaled by  BS,BZ)
C CL parameter accessed:-
C    VRANGE             Low and high limits of the pixel value display range
C
C    a j penny                    ral         1990 jan

      subroutine ds_imgscls ( im, mx, my, kxs, kxe, kys, kye )

      implicit none

      include 'ST_DS_GEN_INC'
      include 'ST_IMAGE_INC'

      integer	mx		!i: X image size
      integer	my		!i: Y image size
      integer*2 im(mx,my)	!i: Image
      integer	kxs		!i: X start of area to look at
      integer	kxe		!i: X   end of area to look at
      integer	kys		!i: Y start of area to look at
      integer	kye		!i: Y   end of area to look at
C--
      integer istat, kxr(2), kyr(2)
      real am, std
Cbegin


      kxr(1) = kxs
      kxr(2) = kxe
      kyr(1) = kys
      kyr(2) = kye
      call ranges ( im, mx, my, kxr, kyr, INVAL, am, std, istat )	!Get display range
      DSVMIN = am + 3.0*std
      DSVMAX = am - 2.0*std
      if ( DSVMIN.eq.DSVMAX ) then
         DSVMAX = DSVMAX - 1.0
         DSVMIN = DSVMIN + 1.0
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_DOFLASH -- Display image, scaled to max contrast, titled
C
C     a j penny                ral                  1990-05-07

      subroutine ds_doflash ( im, mx, my, type, kvr, title )

      implicit none

      include 'ST_DS_GEN_INC'

      integer	    mx		!i: Image X size
      integer       my		!i: Image Y size
      integer*2     im(*)	!i: Image
      character*(*) type	!i: Image type ('REAL','SHORT')
      integer	    kvr		!o: Gotscale flag (0=no;1=yes)
      character*(*) title	!i: Image title
C--
Cbegin


      if ( type.eq.'REAL' ) then
         call ds_doflashr ( im, mx, my, kvr, title )
      elseif ( type.eq.'SHORT' ) then
         call ds_doflashs ( im, mx, my, kvr, title )
      else
         call printo ( ' ERROR: Programmer Error in S/R DS_DOFLASH' )
         call printo ( '        Code needs rewriting: Contact'//
     +                 '  person who wrote the program' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_DOFLASHR -- Display real image, scaled to max contrast, titled
C
C     a j penny                ral                  1990-05-07

      subroutine ds_doflashr ( im, mx, my, kvr, title )

      implicit none

      include 'ST_DS_GEN_INC'

      integer	mx		!i: Image X size
      integer	my		!i: Image Y size
      real      im(mx,my)	!i: Image
      integer	kvr		!o: Gotscale flag (0=no;1=yes)
      character*(*) title	!i: Image title
C--
Cbegin


      if ( .not.DSOPEN ) return

      call ds_imgsclr ( im, mx, my, DSNXS, DSNXE, DSNYS, DSNYE )
      kvr = 1
      call ds_dispr ( im, mx, my, 1 )
      call ds_pttit ( title )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_DOFLASHS -- Display short image, scaled to max contrast, titled
C
C     a j penny                ral                  1990-05-07

      subroutine ds_doflashs ( im, mx, my, kvr, title )

      implicit none

      include 'ST_DS_GEN_INC'

      integer	mx		!i: Image X size
      integer	my		!i: Image Y size
      integer*2	im(mx,my)	!i: Image
      integer	kvr		!o: Gotscale flag (0=no;1=yes)
      character*(*) title	!i: Image title
C--
Cbegin


      if ( .not.DSOPEN ) return

      call ds_imgscls ( im, mx, my, DSNXS, DSNXE, DSNYS, DSNYE )
      kvr = 1
      call ds_disps ( im, mx, my, 1 )
      call ds_pttit ( title )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_DODISP -- Display image, scaled, titled
C
C     a j penny                ral                  1990-05-07

      subroutine ds_dodisp ( im, mx, my, type, kvr, title )

      implicit none

      include 'ST_DS_GEN_INC'

      integer	    mx		!i: Image X size
      integer	    my		!i: Image Y size
      integer*2     im(*)	!i: Image
      character*(*) type	!i: Image type ('REAL','SHORT')
      integer	    kvr		!i/o: Gotscale flag (0=no;1=yes)
      character*(*) title	!i: Image title
C--
Cbegin



      if ( type.eq.'REAL' ) then
         call ds_dodispr ( im, mx, my, kvr, title )
      elseif ( type.eq.'SHORT' ) then
         call ds_dodisps ( im, mx, my, kvr, title )
      else
         call printo ( ' ERROR: Programmer Error in S/R DS_DODISP' )
         call printo ( '        Code needs rewriting: Contact'//
     +                 '  person who wrote the program' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_DODISPR -- Display real image, scaled, titled
C
C     a j penny                ral                  1990-05-07

      subroutine ds_dodispr ( im, mx, my, kvr, title )

      implicit none

      include 'ST_DS_GEN_INC'

      integer	mx		!i: Image X size
      integer	my		!i: Image Y size
      real      im(mx,my)	!i: Image
      integer	kvr		!i/o: Gotscale flag (0=no;1=yes)
      character*(*) title	!i: Image title
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( kvr.eq.0 ) call ds_imgsclr ( im, mx, my, DSNXS, DSNXE, DSNYS,
     +                                  DSNYE )
      kvr = 1
      call ds_dispr ( im, mx, my, 2 )
      call ds_pttit ( title )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_DODISPS -- Display image, scaled, titled
C
C     a j penny                ral                  1990-05-07

      subroutine ds_dodisps ( im, mx, my, kvr, title )

      implicit none

      include 'ST_DS_GEN_INC'

      integer	mx		!i: Image X size
      integer	my		!i: Image Y size
      integer*2	im(mx,my)	!i: Image
      integer	kvr		!i/o: Gotscale flag (0=no;1=yes)
      character*(*) title	!i: Image title
C--
Cbegin


      if ( .not.DSOPEN ) return

      if ( kvr.eq.0 ) call ds_imgscls ( im, mx, my, DSNXS, DSNXE, DSNYS,
     +                                  DSNYE )
      kvr = 1
      call ds_disps ( im, mx, my, 2 )
      call ds_pttit ( title )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_BOX -- Put box on display overlay
C
C     a j penny                ral                  1990-05-07

      subroutine ds_box ( xs, xe, ys, ye, kc )

      implicit none

      include 'ST_DS_GEN_INC'

      real     xs		!i: X start of box
      real     xe		!i: X end of box
      real     ys		!i: Y start of box
      real     ye		!i: Y end of box
      integer  kc		!i: Colour (1=red;2=green;3=blue;4=yellow;
				!           5=cyan;6=mauve;7=tan;8=pink)
C--
Cbegin


      if ( .not.DSOPEN ) return

      call ds_line ( xs, ys, xe, ys, kc )
      call ds_line ( xe, ys, xe, ye, kc )
      call ds_line ( xs, ye, xe, ye, kc )
      call ds_line ( xs, ys, xs, ye, kc )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_SPOT -- Put spot on display overlay
C
C     a j penny                ral                  1990-05-07

      subroutine ds_spot ( x, y, kc )

      implicit none

      include 'ST_DS_GEN_INC'

      real     x		!i: X position of spot
      real     y		!i: Y position of spot
      integer  kc		!i: Colour (1=red;2=green;3=blue;4=yellow;
				!           5=cyan;6=mauve;7=tan;8=pink)
C--
Cbegin


      if ( .not.DSOPEN ) return

      call ds_line ( x, y, x, y, kc )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_ACIM -- Display part of actual image
C
C    alan penny           ral                       1990-02-01

      subroutine ds_acim ( im, mx, my, type, nxs, nxe, nys, nye,
     +                      ix, iy, wrap )

      implicit none
      include 'ST_DS_GEN_INC'

      integer   mx		!i: X size of image
      integer   my		!i: Y size of image
      integer*2 im(*)		!i: image
      character*(*) type	!i: Image type ('REAL','SHORT')
      integer   nxs		!i: X start in image
      integer   nxe		!i: X end   in image
      integer   nys		!i: Y start in image
      integer   nye		!i: Y end   in image
      integer   ix		!i: X posn in virtual image of blh
      integer   iy		!i: Y posn in virtual image of blh
      logical   wrap		!i: Flag to wrap values round display limits
C--
Cbegin


      if ( type.eq.'REAL' ) then
         call ds_acimr ( im, mx, my, nxs, nxe, nys, nye,
     +                   ix, iy, wrap )
      elseif ( type.eq.'SHORT' ) then
         call ds_acims ( im, mx, my, nxs, nxe, nys, nye,
     +                   ix, iy, wrap )
      else
         call printo ( ' ERROR: Programmer Error in S/R DS_ACIM' )
         call printo ( '        Code needs rewriting: Contact'//
     +                 '  person who wrote the program' )
      endif



      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_GTCOMF -- Get image compression factor and displayed image size
C Needs input of screen size and displayed image start and end coords
C
C  alan penny           ral                         1990-02-01

      subroutine ds_gtcomf ( kopt )

      implicit none

      include 'ST_DS_GEN_INC'

      integer    kopt		!i: Take default (0), or ask if not 1,1 (1),
				!   or ask (2)?
C--
      integer lx, ly
Cbegin


      lx = 1 + iabs(DSNXE-DSNXS+1-1)/DSSNX				!Scale image size to display size
      ly = 1 + iabs(DSNYE-DSNYS+1-1)/DSSNY
      lx = max(lx,ly)
      ly = max(lx,ly)

      if ( kopt.eq.2 .or.
     +     ((kopt.eq.1).and.((lx.ne.1).or.(ly.ne.1))) ) call get2i (
     +                           'IMCOMP', lx, ly, .true., 1, 1000 )

      DSCOMFX = lx
      DSCOMFY = ly


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_TIV -- Translate image coords to virtual screen coords
C
C    alan penny           ral                       1990-02-01

      subroutine ds_tiv ( kxi, kyi, kxo, kyo )

      implicit none
      include 'ST_DS_GEN_INC'

      integer kxi		!i: Actual image X position
      integer kyi		!i: Actual image Y position
      integer kxo		!o: Virtual screen X position
      integer kyo		!o: Virtual screen Y position
C--
Cbegin


      kxo = DSIXS + (kxi-DSNXS)/DSCOMFX
      kyo = DSIYS + (kyi-DSNYS)/DSCOMFY


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_TVI -- Translate virtual coords to actual image coords
C
C    alan penny           ral                       1990-02-01

      subroutine ds_tvi ( kxi, kyi, kxo, kyo )

      implicit none
      include 'ST_DS_GEN_INC'

      integer kxi		!i: Virtual screen X position
      integer kyi		!i: Virtual screen Y position
      integer kxo		!o: Actual image   X position
      integer kyo		!o: Actual image   Y position
C--
Cbegin


      kxo = DSNXS + (kxi-DSIXS)*DSCOMFX
      kyo = DSNYS + (kyi-DSIYS)*DSCOMFY


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_VTYPE -- Type posn, value
C
C  alan penny             ral             1990 Jan

      subroutine ds_vtype ( istat, px, py )

      implicit none

      include 'ST_IMAGE_INC'

      integer	istat		!i: Cursor got error flag (0=ok, in image)
      real	px		!i: Cursor X posn
      real	py		!i: Cursor Y posn

      integer	kxo		! X Last cursor X posn
      integer	kyo		! Y Last cursor Y posn
      common / dsancom / kxo, kyo
C--
      integer   kx, ky, lx, ly, iv, kl, j, k, kpx, kpy
      real      val
      logical   valid
      integer*2 is
      character at*13, texta*25, textb*27, textc*53
Cbegin


      kx = 0
      ky = 0
      val = 0.0
      valid = .true.
      if ( istat.eq.0 ) then
         kpx = px
         kpy = py
         call vt_tsi ( kpx, kpy, lx, ly )
         if ( lx.ge.1 .and. lx.le.NX .and. ly.ge.1 .and. ly.le.NY ) then
            kx = lx
            ky = ly
            if ( IMTYPE.eq.'SHORT' ) then
               call cops1 ( %val(IPIM), NX, NY, kx, ky, is )
               iv = is
               if ( iv.eq.INVAL ) then
                  valid = .false.
                  val = 0.0
               else
                  valid = .true.
                  val = BS*real(iv) + BZ
               endif
            else
               call copr1 ( %val(IPIM), NX, NY, kx, ky, val )
               if ( val.eq.RINVAL ) then
                  valid = .false.
                  val = 0.0
               else
                  valid = .true.
                  val = BS*val + BZ
               endif
            endif
         endif
      endif

      if ( kx.ne.kxo .or. ky.ne.kyo ) then				!Put up new position

         KXO = kx
         KYO = ky

         if ( .not.valid ) then
            at = 'Invalid'
         else
            if ( val.eq.0.0 ) then
               texta = '0.0'
            elseif ( abs(val).gt.1.0e10 .or. abs(val).lt.1.0e-3 ) then
               write ( texta, '(g17.5)' ) val
            else
               write ( texta, '(f20.7)' ) val
               j = 15 + max(0.0,alog10(1000000.0/abs(val)))
               if ( val.lt.10.0 ) j = j - 1
               if ( j.lt.20 ) texta(j:) = ' '
            endif
            call lbgone ( texta )
            call charln ( texta, k )
            kl = min(12,k)
            at = texta(1:kl)
         endif
         write ( textb, '('' X='',i6,'' Y='',i6,'' Value= '')' ) kx, ky
         textc = textb
         textc(28:) = at
         call printo ( textc )

      endif


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LUT.FOR   Main Look Up Table for X display s/rs
C
C Contains:-
C
C DS_LUTCOL    Set up and load LUT
C DS_LUTROT    Rotate LUT
C DS_LUTSCA    Shift and scale LUT
C DS_LUTBAR    Load/clear display of Look-up Table bar
C DS_LUTPUT    Put a LUT into operation for image display
C DS_LUTPAI    Paint colour into LUT
C DS_LUTENDS   Change `Ends' control on LUT


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LUTCOL -- Set up and load LUT
C
C    alan penny           ral                       1990-02-01

      subroutine ds_lutcol ( nlut )

      implicit none
      include 'ST_DS_GEN_INC'
      include 'lut.inc'

      integer     nlut		!i: Number of LUT to load
C--
Cbegin


      LUT_NUM = nlut

      call ds_lutacol

      if ( DSTYPE.eq.4 ) call dsx_lutcol

      call ds_lutput


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LUTROT -- Rotate LUT
C
C    alan penny           ral                       1990-02-01

      subroutine ds_lutrot ( )

      implicit none
      include 'ST_DS_GEN_INC'
C--
Cbegin


      if ( .not.DSSCUR ) call ds_scur					!Start cursor?

      if ( DSTYPE.eq.4 ) call dsx_lutrot


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LUTSCA -- Shift and scale LUT
C
C    alan penny           ral                       1990-02-01

      subroutine ds_lutsca ( )

      implicit none
      include 'ST_DS_GEN_INC'
C--
Cbegin


      if ( .not.DSSCUR ) call ds_scur					!Start cursor?

      if ( DSTYPE.eq.4 ) call dsx_lutsca


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LUTBAR -- Load/clear display of Look-up Table bar
C
C   alan penny                        ral              1990-01-31

      subroutine ds_lutbar ( kopt )

      implicit none
      include 'ST_DS_GEN_INC'

      integer	kopt		!i: Option Flag 1=load; 0= clear
C--
Cbegin


      if ( DSTYPE.eq.4 ) call dsx_lutbar ( kopt )


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LUTPUT -- Put a LUT into operation for image display
C
C   alan penny                        ral              1990-01-31

      subroutine ds_lutput ( )

      implicit none
      include 'ST_DS_GEN_INC'
C--
Cbegin


      if ( DSTYPE.eq.4 ) call dsx_lutput


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LUTPAI -- Paint colour into LUT
C
C   alan penny                        ral              1990-01-31

      subroutine ds_lutpai ( )

      implicit none
      include 'ST_DS_GEN_INC'
C--
Cbegin


      if ( DSTYPE.eq.4 ) call dsx_lutpai


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LUTENDS -- Change `Ends' control on LUT
C
C    alan penny           ral                       1990-02-01

      subroutine ds_lutends ( kopt )

      implicit none
      include 'ST_DS_GEN_INC'
      include 'lut.inc'
C--
      integer   kopt		!i: Ends type (1=black/white;2=wh/bl;
                                !              3=col/col;4=wrap)
Cbegin


      LUT_ENDS = kopt
      call ds_lutput


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LUT_A.FOR
C
C
C Contains:-
C
C DS_LUTACOL   Set up LUT
C DS_LUTREAD   Read a LUT
C DS_LUTWRITE  Write a full LUT
C DS_LUTBARL   Load 'background' of LUT bar
C DS_LUTTRAN   Translate bar posn to LUT posn
C DS_LUTEND_A  Load ends into temp LUT
C DS_LUTFLIP   Flip LUT
C DS_LUTLOAD   Set up and load LUT
C DS_LOADLUT   Load a LUT into a working LUT



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LUTACOL -- Set up LUT
C
C    alan penny           ral                       1990-02-01

      subroutine ds_lutacol ()

      implicit none
      include 'lut.inc'
C--
      integer j, k, nv(3), jp, jn, nlut
      real    rv, rvp, rvn, val, valp, valn
      real lut_now(NLENT,6)
Cbegin


      nlut = LUT_NUM

      if ( nlut.lt.1 .or. nlut.gt.15 ) then
         call printo('ERROR: Programmer error - Forbidden LUT number')
         call printo ( '        Code needs rewriting: Contact'//
     +                 ' person who wrote the program' )
           return
      endif

      call ds_lutload ( lut_now, nlut )

      nv(1) = nint(lut_now(1,1)) + 1
      nv(2) = nint(lut_now(1,3)) + 1
      nv(3) = nint(lut_now(1,5)) + 1

      do k = 1, 3							!Do for each colour
         jp = 2
         jn = 3
         valp = lut_now(jp,2*K)
         valn = lut_now(jn,2*k)
         rvp  = lut_now(jp,2*k-1)
         rvn  = lut_now(jn,2*k-1)
         do j = 1, NUMDCOL
            rv = real(j-1)/real(NUMDCOL-1)
            if ( rv.gt.lut_now(jn,2*k-1) ) then
               jp = min(nv(k),(jp+1))
               jn = min(nv(k),(jn+1))
               valp = lut_now(jp,2*k)
               valn = lut_now(jn,2*k)
               rvp  = lut_now(jp,2*k-1)
               rvn  = lut_now(jn,2*k-1)
            endif
            if ( abs(rvn-rvp).lt.0.000001 ) then
               val = valp
            else
               val = valp + (valn-valp)*(rv-rvp)/(rvn-rvp)
            endif
            LUT_VAL(j,k) = max(0.0,min(1.0,val))
         enddo
      enddo
      LUT_SC = 1.0
      LUT_ZE = 0.0


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LUTREAD -- Read a LUT
C
C    alan penny           ral                       1990-02-01

      subroutine ds_lutread ( kopt )

      implicit none
      include 'ST_DS_GEN_INC'
      include 'lut.inc'

      integer	kopt		!i: 1=full LUT; 2=short lUT
C--
      integer istat, kx, ky, nlut, nv, ip
      character*70 title
Cbegin


      if ( kopt.eq.1 ) then
         call optabr ( 'INLUT', ip, kx, ky, .true., istat )
         if ( kx.ne.8 .or. ky.ne.NUMDCOL ) then
            call printo ( 'ERROR: Not a full LUT' )
         else
            call copfrr ( %val(ip), kx, ky, 6, 1, LUT_VAL(1,1), NUMDCOL)
            call copfrr ( %val(ip), kx, ky, 7, 1, LUT_VAL(1,2), NUMDCOL)
            call copfrr ( %val(ip), kx, ky, 8, 1, LUT_VAL(1,3), NUMDCOL)
            call gtdesr ( 'INLUT', 'LUTSC', LUT_SC, 1.0, istat )
            call gtdesr ( 'INLUT', 'LUTZE', LUT_ZE, 0.0, istat )
            call gtdesc ( 'INLUT', 'TITLE', title, ' ', nv, istat )
            call printo ( ' Title is:- '//title )
            call ds_lutput
         endif
         call canpar ( 'INLUT' )
      endif

      if ( kopt.eq.2 ) then
         call optabr ( 'INLUT', ip, kx, ky, .true., istat )
         if ( kx.lt.8 .or. kx.gt.30 .or. ky.ne.6 ) then
            call printo ( 'ERROR: Not a short LUT' )
         else
            call get1i ( 'NUM_LUT', nlut, LUT_STORED+1, 9, 15 )
            call ds_loadlut ( %val(ip), kx, ky, nlut )
            call gtdesc ( 'INLUT', 'TITLE', title, ' ', nv, istat )
            call printo ( ' Title is:-'//title )
            LUT_STORED = min((LUT_STORED+1),15)
         endif
         call canpar ( 'INLUT' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LUTWRITE -- Write a full LUT
C
C    alan penny           ral                       1990-02-01

      subroutine ds_lutwrite ( )

      implicit none
      include 'lut.inc'
C--
      integer ip, istat
      character*70 title
Cbegin


      call optabw ( 'OUTLUT', ip, 8, NUMDCOL, .true., istat )

      if ( istat.ne.0 ) return

      call coptrr ( LUT_VAL(1,1), NUMDCOL, %val(ip), 8, NUMDCOL, 6, 1 )
      call coptrr ( LUT_VAL(1,2), NUMDCOL, %val(ip), 8, NUMDCOL, 7, 1 )
      call coptrr ( LUT_VAL(1,3), NUMDCOL, %val(ip), 8, NUMDCOL, 8, 1 )
      call ptdesr ( 'OUTLUT', 'LUTSC', LUT_SC )
      call ptdesr ( 'OUTLUT', 'LUTZE', LUT_ZE )
      call pthead ( 'OUTLUT', 1, 'R', istat )
      call pthead ( 'OUTLUT', 2, 'G', istat )
      call pthead ( 'OUTLUT', 3, 'B', istat )
      call get1c ( 'TITLE', title, 'Look-up Table', .true. )
      call ptdesc ( 'OUTLUT', 'TITLE', title )

      call canpar ( 'OUTLUT' )



      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LUTBARL -- Load 'background' of LUT bar
C
C    alan penny           ral                       1990-02-01

      subroutine ds_lutbarl ( im, kx, ky, xdu, xdd )

      implicit none

      integer    kx		!i: X size of bar
      integer    ky		!i: Y size of bar
      byte       im(kx,ky)	!o: Colour bar
      byte       xdd		!i: Down colour code
      byte       xdu		!i: Up colour code
C--
      integer j, ja, jb, k, ka, kb, lx, ly
Cbegin


      lx = max(2,(kx/10))
      ly = max(2,(ky/5))

      call amovkz ( xdd, im, kx*ky )
      do k = 1, ky, 2*ly
         do ka = 1, ly
            kb = k + ka - 1
            if ( kb.le.ky ) then
               do j = 1, kx, 2*lx
                  do ja = 1, lx
                     jb = j + ja - 1
                     if ( jb.le.kx ) im(jb,kb) = xdu
                  enddo
               enddo
               if ( jb.lt.kx ) then
                  do j = jb, kx
                     im(j,kb) = xdu
                  enddo
               endif
            endif
         enddo
      enddo

      if ( kb.lt.ky ) then
         do k = kb, ky
            do j = 1, kx, 2*lx
               do ja = 1, lx
                  jb = j + ja - 1
                  if ( jb.le.kx ) im(jb,k) = xdu
               enddo
            enddo
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LUTTRAN -- Translate bar posn to LUT posn
C
C   alan penny                  ral                        1990-02-03

      subroutine ds_luttran ( rv, col, jm )

      implicit none
      include 'lut.inc'

      real     rv		!i: Posn in bar (0.0-0.999)
      real     col(3)		!o: Colour in LUT
      integer  jm		!o: Posn in LUT
C--
      integer k
      real st(3), en(3)
Cbegin


      if ( LUT_ENDS.eq.1 ) then
         if ( LUT_FLIPPED ) then
            call amovkr ( 1.0, st, 3 )
            call azeror ( en, 3 )
         else
            call azeror ( st, 3 )
            call amovkr ( 1.0, en, 3 )
         endif
      elseif ( LUT_ENDS.eq.2 ) then
         if ( LUT_FLIPPED ) then
            call azeror ( st, 3 )
            call amovkr ( 1.0, en, 3 )
         else
            call amovkr ( 1.0, st, 3 )
            call azeror ( en, 3 )
         endif
      elseif ( LUT_ENDS.eq.3 ) then
         st(1) = LUT_VAL(1,1)
         st(2) = LUT_VAL(1,2)
         st(3) = LUT_VAL(1,3)
         en(1) = LUT_VAL(NUMDCOL,1)
         en(2) = LUT_VAL(NUMDCOL,2)
         en(3) = LUT_VAL(NUMDCOL,3)
      endif

      rv = (rv-LUT_ZE)/LUT_SC
      jm = 1 + int(real(NUMDCOL-1)*rv)

      if ( jm.lt.1 ) then
         if ( LUT_ENDS.eq.4 ) then
            do while ( jm.lt.1 )
               jm = jm + NUMDCOL
            enddo
            do k = 1, 3
               col(k) = LUT_VAL(jm,k)
            enddo
         else
            jm = 1
            call amovr ( st, col, 3 )
         endif
      elseif ( jm.gt.NUMDCOL ) then
         if ( LUT_ENDS.eq.4 ) then
            do while ( jm.gt.NUMDCOL )
               jm = jm - NUMDCOL
            enddo
            do k = 1, 3
               col(k) = LUT_VAL(jm,k)
            enddo
         else
            jm = NUMDCOL
            call amovr ( en, col, 3 )
         endif
      else
          col(1) = LUT_VAL(jm,1)
          col(2) = LUT_VAL(jm,2)
          col(3) = LUT_VAL(jm,3)
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LUTEND_A -- Load ends into temp LUT
C
C   alan penny                  ral                        1991 Oct

      subroutine ds_lutend_a ( tcol )

      implicit none
      include 'lut.inc'

      real      tcol(NUMDCOL,3)		!o: Temp LUT
C--
      real st(3), en(3), rv
      integer jb, k, ja

Cbegin


      if ( LUT_ENDS.eq.1 ) then
         if ( LUT_FLIPPED ) then
            call amovkr ( 1.0, st, 3 )
            call azeror ( en, 3 )
         else
            call azeror ( st, 3 )
            call amovkr ( 1.0, en, 3 )
         endif
      elseif ( LUT_ENDS.eq.2 ) then
         if ( LUT_FLIPPED ) then
            call azeror ( st, 3 )
            call amovkr ( 1.0, en, 3 )
         else
            call amovkr ( 1.0, st, 3 )
            call azeror ( en, 3 )
         endif
      elseif ( LUT_ENDS.eq.3 ) then
         st(1) = LUT_VAL(1,1)
         st(2) = LUT_VAL(1,2)
         st(3) = LUT_VAL(1,3)
         en(1) = LUT_VAL(NUMDCOL,1)
         en(2) = LUT_VAL(NUMDCOL,2)
         en(3) = LUT_VAL(NUMDCOL,3)
      endif

      if ( LUT_ENDS.eq.4 ) then
         do jb = 1, NUMDCOL
            rv = real(jb-1)/real(NUMDCOL-1)
            ja = 1 + (NUMDCOL-1)*(rv-LUT_ZE)/LUT_SC
            do while ( ja.lt.1 )
               ja = ja + NUMDCOL
            enddo
            do while ( ja.gt.NUMDCOL )
               ja = ja - NUMDCOL
            enddo
            do k = 1, 3
               tcol(jb,k) = LUT_VAL(ja,k)
            enddo
         enddo
      else
         do jb = 1, NUMDCOL
            rv = real(jb-1)/real(NUMDCOL-1)
            ja = NUMDCOL*(rv-LUT_ZE)/LUT_SC
            if ( ja.lt.1 ) then
               do k = 1, 3
                  tcol(jb,k) = st(k)
               enddo
            elseif ( ja.gt.NUMDCOL ) then
               do k = 1, 3
                  tcol(jb,k) = en(k)
               enddo
            else
               do k = 1, 3
                  tcol(jb,k) = LUT_VAL(ja,k)
               enddo
            endif
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LUTFLIP -- Flip LUT
C
C    alan penny           ral                       1990-02-01

      subroutine ds_lutflip ( )

      implicit none
      include 'ST_DS_GEN_INC'
      include 'lut.inc'
C--
      integer j, k
      real ta(NUMDCOL)
Cbegin


      do k = 1, 3
         do j = 1, NUMDCOL
            ta(j) = LUT_VAL(NUMDCOL+1-j,k)
         enddo
         call amovr ( ta, LUT_VAL(1,k), NUMDCOL )
      enddo
      LUT_FLIPPED = .not.LUT_FLIPPED

      call ds_lutput


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LUTLOAD -- Set up and load LUT
C

      subroutine ds_lutload ( lut, nlut )

      implicit none

      real      lut(25*6)	!o: Loaded LUT
      integer   nlut		!i: Number of ref LUT to load
C--
      real lut_use (25,6,15)
      integer j, k
      external ds_lut_bl
      common /lutcomd/ lut_use
Cbegin


      if ( nlut.gt.15 ) return

      do k = 1, 25
	 do j = 1, 6
	    lut(k+(j-1)*25) = lut_use(k,j,nlut)
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_LOADLUT -- Load a LUT into LUT tables storage
C
C
      subroutine ds_loadlut ( ip, kx, ky, nlut )

      integer   kx		!i:
      integer   ky		!i:
      real      ip(kx,ky)	!i: LUT to load
      integer   nlut		!i: Number of LUT to put into in tables

      real      ilut(25,6,15)
      common /lutcomd/ ilut
C--
Cbegin


      call coprr ( ip, kx, ky, 6, kx, 1, ky,
     +             ilut(1,1,nlut), 25, 6, 1, 1)


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_PANEL_U.FOR     General s/rs for using Panels for Unix
C
C DS_P_INIT    Initialise window
C DS_P_GTYPE   Get type of panel display
C DS_P_DSET    Set up for image display
C DS_P_CLOSE   Close window
C DS_P_PTTIT   Put image title on display
C DS_P_SCOL    Set line colour
C DS_P_SCUR    Start the cursor
C DS_P_SSCUR   Start the cursor (sections)
C DS_P_GBOX    Get the cursor position
C DS_P_GSBOX   Get the box chosen (sections)
C DS_P_ERASE   Erase display
C DS_P_LOAD    Load a panel
C DS_P_SLOAD   Load a panel (sections)
C DS_P_UNBHELP Relase the button help box
C DS_P_SWITCH  Switch between panel and keyboard for option choice
C DS_P_SWITCHS Switch between panel and keyboard for option choice (sections)

C DS_P_HX_LOAD  Load panel help box with buttons start general help
C DS_P_HX_HLOAD Load panel help box with help for chosen option


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_INIT -- Initialise panel
C
C  alan penny             ral          1990 jan

      subroutine ds_p_init ( kx, ky, title, kdoh, ierr )

      implicit none

      include 'ST_DS_PANEL_INC'

      integer	    kx		!i: Panel X size
      integer	    ky		!i: Panel Y size
      character*(*) title	!i: Title of panel
      integer	    kdoh	!i: Use Help Panel? (0=no;1=yes)
      integer       ierr	!o: Error flag (0=ok;1=bad)
C--
Cbegin


      if ( PDSTYPE.eq.4 ) call dsx_p_init ( kx, ky, title,
     +                                      kdoh, ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_GTYPE -- Get type of panel display
C
C    alan penny                ral              1990-01-31

      subroutine ds_p_gtype ( ierr )

      implicit none
      include 'ST_DS_PANEL_INC'

      integer   ierr		!o: Error flag (ok=0;bad=1)
C--
Cbegin

      PDSTYPE = 4
      ierr = 0


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_DSET -- Set up panel for display
C
C   alan penny                  ral                        1990-02-03

      subroutine ds_p_dset ()

      implicit none

      include 'ST_DS_PANEL_INC'

C--
Cbegin


      if ( .not.PDSOPEN ) return


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_CLOSE -- Close panel window
C
C  alan penny             ral          1990 jan

      subroutine ds_p_close ( ierr )

      implicit none

      include 'ST_DS_PANEL_INC'

      integer       ierr	!o: Error flag (0=ok;1=bad)
C--
      integer kdoh
Cbegin


      if ( .not.PDSOPEN ) return

      if ( DOHPANEL ) kdoh = 1
      if ( PDSTYPE.eq.4 ) call dsx_p_close ( kdoh, ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_PTTIT -- Put image title on display
C
C    alan penny                ral              1990-01-31

      subroutine ds_p_pttit ( title )

      implicit none

      include 'ST_DS_PANEL_INC'

      character*(*) title	!i: Title to put up
C--
Cbegin


      if ( .not.PDSOPEN ) return

      if ( PDSTYPE.eq.4 ) call dsx_p_pttit ( title )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_SCOL -- Set line colour
C
C    alan penny           ral                       1990-02-01

      subroutine ds_p_scol ( w, kc )

      implicit none

      include 'ST_DS_PANEL_INC'

      real     w		!i: Line width
      integer  kc		!i: Colour (1=red;2=green;3=blue;4=yellow;
				!           5=cyan;6=mauve;7=tan;8=pink)
C--
Cbegin


      if ( .not.PDSOPEN ) return

      if ( PDSTYPE.eq.4 ) call dsx_p_scol ( w, kc )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_SCUR -- Start the cursor
C
C   alan penny                  ral                        1990-02-03

      subroutine ds_p_scur ( kopt )

      implicit none

      include 'ST_DS_PANEL_INC'

      integer	kopt		!i: Default option

C--
Cbegin


      if ( .not.PDSOPEN ) return

      if ( PDSTYPE.eq.4 ) return


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_SSCUR -- Start the cursor (sections)
C
C   alan penny                  ral                        1990-02-03

      subroutine ds_p_sscur ( kn )

      implicit none

      include 'ST_DS_PANEL_INC'

      integer	kn		!i: Number of box to place cursor number

C--
Cbegin


      if ( .not.PDSOPEN ) return

      if ( PDSTYPE.eq.4 ) call dsx_p_sscur ( kn )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_GBOX -- Get the box chosen
C
C  alan penny             ral             1990 Jan

      subroutine ds_p_gbox ( kdef, cmdlsts, kopt, thelp, nthelp, ierr )

      implicit none

      include 'ST_DS_PANEL_INC'

      integer	     kdef	!i: Option choice
      character*(*)  cmdlsts	!i: Command list
      integer        kopt	!o: Chosen box
      character*(*)  thelp	!i: Help text
      integer	     nthelp	!i: No of help lines
      integer        ierr	!o: Error flag (0=ok; 1=bad)
C--
Cbegin


      if ( .not.PDSOPEN ) return

      if ( PDSTYPE.eq.4 ) return


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_GSBOX -- Get the box chosen (sections)
C
C  alan penny             ral             1990 Jan

      subroutine ds_p_gsbox ( cmdlsts, hopt, nopt, ktopt, ktdef,
     +                        thelp, nh, opt_num, opt_text, opt_head,
     +                        opt_help, title, hpaneldo, ierr )

      implicit none

      include 'ST_DS_PANEL_INC'

      character*(*)  ktdef		 !i: Option default choice
      integer        nopt		 !i: No of sections
      character*(*)  cmdlsts(nopt)	 !i: Command lists
      character*(*)  hopt(nopt)		 !i: Section headers
      character*(*)  ktopt		 !o: Chosen option
      integer	     nh			 !i: No of help lines
      character*68   thelp(nh)		 !i: Help text
      integer        opt_num             !i: Number of options
      character*12   opt_text(opt_num)   !i: Possible options  (ignores
      character*68   opt_head(opt_num)   !i: Single line helps
      character*68   opt_help(6,opt_num) !i: Fuller helps
      character*(*)  title               !i: Panel title
      logical        hpaneldo            !i: Use Help panel
      integer        ierr    		 !o: Error flag (0=ok; 1=bad)
C--
Cbegin


      if ( .not.PDSOPEN ) return

      if ( PDSTYPE.eq.4 ) call dsx_p_gsbox ( cmdlsts, hopt, nopt,
     +                    ktopt, ktdef, thelp, nh, opt_num, opt_text,
     +                    opt_head, opt_help, title, hpaneldo, ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_ERASE -- Erase display
C
C    a j penny                    ral         1990 jan

      subroutine ds_p_erase ()

      implicit none

      include 'ST_DS_PANEL_INC'
C--
Cbegin


      if ( .not.PDSOPEN ) return

      if ( PDSTYPE.eq.4 ) call dsx_p_erase


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_LOAD -- Load a panel
C
C    alan penny           ral                       1990-02-01

      subroutine ds_p_load ( cmdlsts, num )

      implicit none

      include 'ST_DS_PANEL_INC'

      character*(*) cmdlsts	!i: Text of options
      integer	    num		!i: Panel list option code number
C--
Cbegin


      if ( .not.PDSOPEN ) return

      if ( PDSTYPE.eq.4 ) return


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_SLOAD -- Load a panel with sections
C
C    alan penny           ral                       1990-02-01

      subroutine ds_p_sload ( cmdlsts, hopt, nopt, num )

      implicit none

      include 'ST_DS_PANEL_INC'

      integer       nopt		!i: Number of sections
      character*(*) cmdlsts(nopt)	!i: Text of options in sections
      character*(*) hopt(nopt)		!i: Headers for sections
      integer	    num			!i: Panel list option code number
C--
Cbegin


      if ( .not.PDSOPEN ) return

      if ( PDSTYPE.eq.4 ) call dsx_p_sload ( cmdlsts, hopt, nopt, num )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_UNBHELP -- Relase the button help box
C
C    alan penny           ral                       1990-02-01

      subroutine ds_p_unbhelp ( )

      implicit none
      include 'ST_DS_PANEL_INC'
C--
Cbegin


      if ( .not.PDSOPEN ) return

      if ( PDSTYPE.eq.4 ) call dsx_p_sbox ( -2, 'HELP BTNS', 1, 1 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_SWITCH -- Switch between panel and keyboard for option choice
C
C    a j penny                    ral         1990 jun

      subroutine ds_p_switch ( title, cmdlst, loadnum )

      implicit none

      include 'ST_DS_PANEL_INC'

      character*(*)	title		!i: Panel title
      character*(*)	cmdlst		!i: Panel command list
      integer		loadnum		!i: Code no for panel contents
C--
      integer ierr, kdoh
Cbegin


      kdoh = 0
      if ( DOHPANEL ) kdoh = 1

      if ( .not.DOPANEL ) then
         DOPANEL = .true.
         if ( .not.PDSOPEN ) then
            call ds_p_gtype ( ierr )
            if ( ierr.eq.0 ) then
               call ds_p_init ( PNSNX, PNSNY, title, kdoh, ierr )
               if ( ierr.eq.0 ) then
                  PDSOPEN = .true.
                  call ds_p_dset
                  call ds_p_load ( cmdlst, loadnum )
               endif
            else
               DOPANEL = .false.
               PDSOPEN = .false.
               PDNUML = 0
            endif
         endif
      else
         call ds_p_close ( ierr )
         DOPANEL = .false.
         PDSOPEN = .false.
         PDNUML = 0
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_SWITCHS -- Switch between panel and keyboard for option choice (sections)
C
C    a j penny                    ral         1990 jun

      subroutine ds_p_switchs ( title, cmdlsts, hopt, nopt, loadnum )

      implicit none

      include 'ST_DS_PANEL_INC'

      character*(*)	title		!i: Panel title
      integer           nopt		!i: No of sections
      character*(*)	cmdlsts(nopt)	!i: Panel command lists
      character*(*)     hopt(nopt)	!i: Section headers
      integer		loadnum		!i: Code no for panel contents
C--
      integer ierr, kdoh
Cbegin


      kdoh = 0
      if ( DOHPANEL ) kdoh = 1

      if ( .not.DOPANEL ) then
         DOPANEL = .true.
         if ( .not.PDSOPEN ) then
            call ds_p_gtype ( ierr )
            if ( ierr.eq.0 ) then
               call ds_p_init ( PNSNX, PNSNY, title, kdoh, ierr )
               if ( ierr.eq.0 ) then
                  PDSOPEN = .true.
                  call ds_p_dset
                  call ds_p_sload ( cmdlsts, hopt, nopt, loadnum )
               endif
            else
               DOPANEL = .false.
               PDSOPEN = .false.
               PDNUML = 0
            endif
         endif
      else
         call ds_p_close ( ierr )
         DOPANEL = .false.
         PDSOPEN = .false.
         PDNUML = 0
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_HX_LOAD -- Load panel help box with buttons general help
C
C    alan penny           ral                       1990-02-01

      subroutine ds_p_hx_load ( title )

      implicit none

      include 'ST_DS_PANEL_INC'

      character*(*) title		!i: Title of panel
C--
Cbegin


      if ( .not.PDSOPEN ) return

      if ( PDSTYPE.eq.4 ) call dsx_p_hx_load ( title )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_P_HX_HLOAD -- Load panel help box with help for chosen option
C
C    alan penny           ral                       1990-02-01

      subroutine ds_p_hx_hload ( thead, thelp )

      implicit none
      include 'ST_DS_PANEL_INC'

      character*68 thead		!i: One line description of option
      character*545 thelp		!i: Help text for option chosen
C--
Cbegin


      if ( .not.PDSOPEN ) return

      if ( PDSTYPE.eq.4 ) call dsx_p_hx_hload ( thead, thelp )


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DSX_GENF.F     Fortran subroutines of X window displays
C
C Contains:-
C DSX_GCUR      Get cursor image posn on button (monitor posn,value) (X windows)
C DSX_GSCUR     Get cursor screen posn on button (monitor posn,value) (X windows)
C DSX_GTCUR     Get cursor posn on button (monitor posn,value)  (X windows)
C DSX_PPANST    Start panel posn, value display (decw vaxstation)
C DSX_PPANDS    Display panel posn, value display (X windows)
C DSX_GPCUR     Get the cursor position  (X windows)
C DSX_WAITBUT   Wait for button to be pressed or to be up  (X windows) (F77 version)
C DSX_OVCUS     Pan an oval cursor  (X windows)
C DSX_SETELL    Set ellipse shape
C DSX_ZOOM      Display (zoomed/panned) actual image from virtual image  (X windows)
C DSX_BLINK     Blink an image



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DSX_GCUR -- Get cursor image posn on button (monitor posn,value)  (X windows)
C
C   alan penny                  ral                        1990-02-03

      subroutine dsx_gcur ( monitor, kx, ky, kbut, ierr )

      implicit none

      include 'ST_DS_PANEL_INC'
      include 'ST_DS_GEN_INC'

      logical   monitor		!i: Flag to output posn, val on terminal/panel
      integer   kx		!o: Cursor image X position
      integer   ky		!o: Cursor image Y position
      integer   kbut		!o: Cursor Button number
      integer   ierr	    	!o: Error flag (0=ok; 1=bad)
C--
      logical loop
      integer istat, kpx, kpy, kb(3)
Cbegin


      if ( PDSOPEN .and. monitor ) call dsx_ppanst			!Open panel

      call dsx_waitbut ( .true., .false., kbut, kpx, kpy )		!Buttons up?

      loop = .true.
      do while ( loop )

         call dsx_mswait ( 50 )						!Wait 50ms before looping

         call dsx_getcurpb ( .true., kpx, kpy, kb, istat )		!Button down?

         if ( PDSOPEN .and. monitor ) call dsx_ppands (istat, kpx, kpy)

         if ( kb(1).eq.1 .or. kb(2).eq.1 .or. kb(3).eq.1 ) then
            loop = .false.
            if ( kb(1).eq.1 ) kbut = 1
            if ( kb(2).eq.1 ) kbut = 2
            if ( kb(3).eq.1 ) kbut = 3
         endif

      enddo

      kx = 0
      ky = 0
      if ( istat.eq.0 ) call vt_tsi ( kpx, kpy, kx, ky )

      DSCURPOSX = kpx							!Store cursor screen posn
      DSCURPOSY = kpy
      DSCURSET = .true.

      ierr = 0
      if ( istat.ne.0 ) ierr = 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DSX_GSCUR -- Get cursor screen posn on button (monitor posn,value)  (X windows)
C
C   alan penny                  ral                        1990-02-03

      subroutine dsx_gscur ( monitor, kx, ky, kbut, ierr )

      implicit none

      include 'ST_DS_PANEL_INC'
      include 'ST_DS_GEN_INC'

      logical   monitor		!i: Flag to output posn, val on terminal/panel
      integer   kx		!o: Cursor screen X position
      integer   ky		!o: Cursor screen Y position
      integer   kbut		!o: Cursor Button number
      integer   ierr	    	!o: Error flag (0=ok; 1=bad)
C--
      logical loop
      integer istat, kpx, kpy, kb(3)
Cbegin


      if ( PDSOPEN .and. monitor ) call dsx_ppanst			!Open panel

      call dsx_waitbut ( .true., .false., kbut, kpx, kpy )		!Buttons up?

      loop = .true.
      do while ( loop )

         call dsx_mswait ( 50 )						!Wait 50ms before looping

         call dsx_getcurpb ( .true., kpx, kpy, kb, istat )		!Button down?

         if ( PDSOPEN .and. monitor ) call dsx_ppands (istat, kpx, kpy)

         if ( kb(1).eq.1 .or. kb(2).eq.1 .or. kb(3).eq.1 ) then
            loop = .false.
            if ( kb(1).eq.1 ) kbut = 1
            if ( kb(2).eq.1 ) kbut = 2
            if ( kb(3).eq.1 ) kbut = 3
         endif

      enddo

      kx = kpx
      ky = kpy

      DSCURPOSX = kpx							!Store cursor screen posn
      DSCURPOSY = kpy
      DSCURSET = .true.

      ierr = 0
      if ( istat.ne.0 ) ierr = 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DSX_GTCUR -- Get cursor posn on button (monitor posn,value)  (X windows)
C
C   alan penny                  ral                        1990-02-03

      subroutine dsx_gtcur ( monitor, kx, ky, kbut, ierr )

      implicit none

      include 'ST_DS_PANEL_INC'
      include 'ST_DS_GEN_INC'

      logical   monitor		!i: Flag to output posn, val on terminal/panel
      integer   kx		!o: Cursor X position
      integer   ky		!o: Cursor Y position
      integer   kbut		!o: Cursor Button number
      integer   ierr	    	!o: Error flag (0=ok; 1=bad)
C--
      logical loop
      real    px, py
      integer istata, istatp, kpx, kpy, kb(3)
Cbegin


      if ( PDSOPEN .and. monitor ) call dsx_ppanst			!Open panel

      call dsx_waitbut ( .true., .false., kbut, kpx, kpy )		!Buttons up?

      loop = .true.
      do while ( loop )

         call dsx_mswait ( 50 )						! Wait 50ms before looping

         call dsx_getcurpb ( .true., kpx, kpy, kb, istatp )

         if ( PDSOPEN .and. monitor ) call dsx_ppands (istatp, kpx,kpy)

         if ( kb(1).eq.1 .or. kb(2).eq.1 .or. kb(3).eq.1 ) then		!Buttons pressed?
            if ( kb(1).eq.1 .or. kb(2).eq.1 ) then			!Button 3 pressed?
               loop = .true.
               if ( .not.PDSOPEN .and. monitor ) then
                  istata = 0
                  if ( istatp.ne.0 ) istata = 1
                  px = kpx
                  py = kpy
                  call ds_vtype ( istata, px, py )
               endif
            else
               loop = .false.
            endif
            if ( kb(1).eq.1 ) kbut = 1
            if ( kb(2).eq.1 ) kbut = 2
            if ( kb(3).eq.1 ) kbut = 3
         endif
      enddo

      kx = 0
      ky = 0
      if ( istatp.eq.0 ) call vt_tsi ( kpx, kpy, kx, ky )

      DSCURPOSX = kpx							!Store cursor screen posn
      DSCURPOSY = kpy
      DSCURSET = .true.

      ierr = 0
      if ( istatp.ne.0 ) ierr = 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DSX_PPANST -- Start panel posn, value display (X windows)
C
C  alan penny             ral             1990 Jan

      subroutine dsx_ppanst ( )

      implicit none
      include 'ST_DS_PANEL_INC'

      integer	kxs		! X location of panel start display
      integer	kys		! Y location of panel start display
      integer	kxo		! X Last cursor X posn
      integer	kyo		! Y Last cursor Y posn
      common / ppancom / kxs, kys, kxo, kyo
C--
      integer jx, jy
Cbegin


      jx = PNVPOSX							!Location to monitor posn
      jy = PNVPOSY							! val in panel

      call dsx_p_carea ( jx+15, jy+1, 70, 55, 0 )
      call dsx_p_puttxt ( 'X=', 2,   jx, jy+40, 9 )
      call dsx_p_puttxt ( 'Y=', 2, jx+2, jy+22, 9 )
      call dsx_p_puttxt ( 'V=', 2, jx+2,  jy+4, 9 )

      call dsx_p_carea ( jx-2,     jy, 87,  1, 1 )
      call dsx_p_carea ( jx-1,   jy+1, 86,  1, 1 )
      call dsx_p_carea ( jx+85,  jy+1,  1, 54, 1 )
      call dsx_p_carea ( jx+84,  jy+2,  1, 53, 1 )
      call dsx_p_carea ( jx-2,  jy+55, 86,  1, 2 )
      call dsx_p_carea ( jx-2,  jy+56, 87,  1, 2 )
      call dsx_p_carea ( jx-2,     jy,  1, 56, 2 )
      call dsx_p_carea ( jx-1,   jy+1,  1, 55, 2 )

      KXS = jx
      KYS = jy

      KXO = -1
      KYO = -1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DSX_PPANDS -- Display panel posn, value display (X windows)
C
C  alan penny             ral             1990 Jan

      subroutine dsx_ppands ( ierr, kpx, kpy )

      implicit none

      include 'ST_IMAGE_INC'

      integer	ierr		!i: Cursor got error flag (0=ok; 1=bad)
      integer   kpx		!i: Cursor Screen X posn
      integer 	kpy		!i: Cursor Screen Y posn

      integer	kxs		! X location of panel start display
      integer	kys		! Y location of panel start display
      integer	kxo		! X Last cursor X posn
      integer	kyo		! Y Last cursor Y posn
      common / ppancom / kxs, kys, kxo, kyo
C--
      integer   kx, ky, lx, ly, iv, kl, j
      real      val
      logical   valid
      integer*2 is
      character ata*13, atb*13, texta*25
Cbegin


      kx = 0								!Get position
      ky = 0
      if ( ierr.eq.0 ) then
         call vt_tsi ( kpx, kpy, lx, ly )
         if ( lx.ge.1 .and. lx.le.NX ) kx = lx
         if ( ly.ge.1 .and. ly.le.NY ) ky = ly
      endif

      if ( kx.eq.KXO .and. ky.eq.KYO ) return				!Not a new position
      KXO = kx
      KYO = ky

      val = 0.0								!Pixel value
      valid = .true.
      if ( ierr.eq.0 ) then
         if ( lx.ge.1 .and. lx.le.NX .and. ly.ge.1 .and. ly.le.NY ) then
            if ( IMTYPE.eq.'SHORT' ) then
               call cops1 ( %val(IPIM), NX, NY, kx, ky, is )
               iv = is
               if ( iv.eq.INVAL ) then
                  valid = .false.
                  val = 0.0
               else
                  valid = .true.
                  val = BS*real(iv) + BZ
               endif
            else
               call copr1 ( %val(IPIM), NX, NY, kx, ky, val )
               if ( val.eq.RINVAL ) then
                  valid = .false.
                  val = 0.0
               else
                  valid = .true.
                  val = BS*val + BZ
               endif
            endif
         endif
      endif

      call dsx_p_carea ( KXS+23, KYS+2, 65, 52, 0 )
      call dsx_p_carea ( KXS+85, KYS+1,  1, 54, 1 )
      call dsx_p_carea ( KXS+84, KYS+2,  1, 53, 1 )

      ata = 'Outside'
      atb = 'Outside'
      if ( ierr.eq.0 ) then						!Write new position
         if ( kx.ge.1 .and. kx.le.NX ) write ( ata, '(i7)' ) kx
         if ( ky.ge.1 .and. ky.le.NY ) write ( atb, '(i7)' ) ky
      endif
      call dsx_p_puttxt ( ata, 7, KXS+25, KYS+40, 9 )
      call dsx_p_puttxt ( atb, 7, KXS+25, KYS+22, 9 )

      if ( ierr.ne.0 .or. kx.lt.1 .or. kx.gt.NX .or. ky.lt.1 .or. 	!Write pixel value
     +     ky.gt.NY ) then
         call dsx_p_puttxt ( 'None', 4, KXS+23, KYS+4, 9 )
      else
         if ( .not.valid ) then
            call dsx_p_puttxt ( 'Invalid', 7, KXS+23, KYS+4, 9 )
         else
            if ( val.eq.0.0 ) then
               texta = '0.0'
            elseif ( abs(val).gt.1.0e10 .or. abs(val).lt.1.0e-3 ) then
               write ( texta, '(g17.5)' ) val
            else
               write ( texta, '(f20.7)' ) val
               j = 15 + max(0.0,alog10(1000000.0/abs(val)))
               if ( val.lt.10.0 ) j = j - 1
               if ( j.lt.20 ) texta(j:) = ' '
            endif
            call lbgone ( texta )
            call charln ( texta, kl )
            kl = min(12,kl)
            if ( kl.gt.8 ) then
               call dsx_p_puttxt ( texta, kl, KXS+23, KYS+1, 9 )
            else
               call dsx_p_puttxt ( texta, kl, KXS+23, KYS+4, 9 )
            endif
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DSX_GPCUR -- Get the cursor position  (X windows)
C
C  alan penny             ral             1990 Jan

      subroutine dsx_gpcur ( kx, ky, ierr )

      implicit none

      include 'ST_DS_GEN_INC'

      integer   kx		!o: Cursor X position
      integer   ky		!o: Cursor Y position
      integer  ierr	    	!o: Error flag (0=ok; 1=bad)
C--
      integer istat, kpx, kpy, kb(3)
Cbegin


      call dsx_getcurpb ( .true., kpx, kpy, kb, istat )

      call vt_tsi ( kpx, kpy, kx, ky )

      DSCURPOSX = kpx
      DSCURPOSY = kpy
      DSCURSET = .true.

      ierr = 0
      if ( istat.ne.0 ) ierr = 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DSX_WAITBUT -- Wait for button to be pressed or to be up  (X windows)
C
C   alan penny                  ral                        1990-02-03

      subroutine dsx_waitbut ( isimage, down, kbut, kpx, kpy )

      implicit none

      include 'ST_DS_GEN_INC'

      logical   isimage         !i: Looking in the image (T) or panel (F)?
      logical	down		!i: Flag to wait till button pressed
				!    (true) or all up (false)
      integer	kbut		!o: Which button pressed
      integer   kpx		!o: X screen position
      integer   kpy		!o: Y screen position
C--
      integer istat, kb(3)
      logical loop
Cbegin


      loop = .true.
      do while ( loop )
         call dsx_getcurpb ( isimage, kpx, kpy, kb, istat )

         if ( istat.eq.0 ) then
           if ( down ) then
              if ( kb(1).eq.1 .or. kb(2).eq.1 .or. kb(3).eq.1 ) loop =
     +                                                          .false.
              if ( kb(1).eq.1 ) kbut = 1
              if ( kb(2).eq.1 ) kbut = 2
              if ( kb(3).eq.1 ) kbut = 3
           else
              kbut = 0
              if ( kb(1).eq.0 .and. kb(2).eq.0 .and. kb(3).eq.0 ) loop=
     +                                                           .false.
           endif
         endif
         if ( loop ) call dsx_mswait ( 50 )					!Wait 50ms before looping
      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DSX_OVCUS -- Pan an oval cursor  (X windows)
C
C    alan penny           ral                       1990-02-01

      subroutine dsx_ovcus ( monitor, rx1, kc1, rx2, kc2,
     +                       ang, elli, kopt, kxp, kyp, kbut )

      implicit none
      include 'ST_DS_PANEL_INC'
      include 'ST_DS_GEN_INC'
      include 'virt.inc'

      logical  monitor		!i: Display pos,val on terminal/panel?
      real     rx1		!i/o: Image pixel 'major axis' radius
      integer  kc1		!i: Colour (1=red;2=green;3=blue;4=yellow;
				!    5=cyan;6=mauve;7=tan;8=pink;
				!    9=black;10=white) (0=none)
      real     rx2(2)		!i/o: Image pixel inner/outer annular
				!     'major axis' radii
      integer  kc2		!i: Colour (1=red;2=green;3=blue;4=yellow;
				!    5=cyan;6=mauve;7=tan;8=pink)
				!    9=black;10=white) (0=none)
      real     ang		!i: Angle of ellipse to X-axis in degrees
      real     elli		!i: Ellipticity of oval
      integer  kopt		!i: Change radii flag (0=no;1=oval;2=inner
				!             annulus;3=outer annulus)
      integer  kxp		!o: Image Y position
      integer  kyp		!o: Image X position
      integer  kbut		!o: Button pressed
C--
      real    px, py, rxo, ryo, pxo, pyo, pxb, pyb, radx,
     +        rxa1, rya1, rxa2(2), rya2(2), rxod
      integer k, istat, ierr, kpy, kpx, kb(3)
      integer  kcx(185*3), kcy(185*3), kcxa(185*3), kcya(185*3)
      logical loop, simple
Cbegin


      if ( rx1.le.0.0 .and. rx2(1).le.0.0 .and. rx2(2).le.0.0 ) return
      if ( kc1.le.0 .and. kc2.le.0 ) return

      if ( PDSOPEN .and. monitor ) call dsx_ppanst			!Start posn,val panel display

      pxo = DSCURPOSX							!Start cursor at last posn
      pyo = DSCURPOSY
      pxb = pxo
      pyb = pyo

      radx = 0.0
      if ( kc1.gt.0 ) radx = rx1
      if ( kc2.gt.0 ) radx = max(radx,rx2(1),rx2(2))
      call dsx_osize ( radx, ang, elli, rxo, ryo )
      rxo = rxo + 4
      ryo = ryo + 4

      if ( ang.eq.0.0 .or. ang.eq.90.0 .or. ang.eq.-90.0 .or.
     +     elli.eq.0.0 ) then
         simple = .true.
         if ( ang.eq.90.0 .or. ang.eq.-90.0 ) then
            rya1 = rx1
            rxa1 = rx1*(1.0-elli)
            rya2(1) = rx2(1)
            rxa2(1) = rx2(1)*(1.0-elli)
            rya2(2) = rx2(2)
            rxa2(2) = rx2(2)*(1.0-elli)
         else
            rxa1 = rx1
            rya1 = rx1*(1.0-elli)
            rxa2(1) = rx2(1)
            rya2(1) = rx2(1)*(1.0-elli)
            rxa2(2) = rx2(2)
            rya2(2) = rx2(2)*(1.0-elli)
         endif
      else
         simple = .false.
      endif

      if ( PDSOPEN ) call dsx_p_hstat ( 7 )

      if ( simple ) then						!Paint oval
         call dsx_paintova ( pxo, pyo, rxo, ryo, pxo, pyo, rxa1, rya1,
     +                       kc1, rxa2, rya2, kc2 )
      else
         call dsx_setell ( ang, elli, rx1, rx2, kcxa, kcya )
         do k = 1, 181*3
            kcx(k) = kcxa(k) + pxo
            kcy(k) = kcya(k) + pyo
         enddo
         call dsx_paintovb ( kcx, kcy, kc1, kc2, pxo, pyo, rxo, ryo )
      endif
      call dsx_waitbut ( .true., .false., kbut, kpx, kpy )		!Wait till all buttons up

      loop = .true.							!Get a position, moving ovals
      do while ( loop )

         call dsx_mswait ( 50 )						!Wait 50ms in looping

         call dsx_getcurpb ( .true., kpx, kpy, kb, istat )		!Get cursor posn
         px = kpx
         py = kpy

         if ( PDSOPEN .and. monitor ) call dsx_ppands (istat,kpx,kpy)	!Display posn, val on panel

         if ( px.ne.pxo .or. py.ne.pyo ) then
            if ( PDSOPEN ) call dsx_p_hstat ( 6 )
            if ( istat.eq.0 ) then
               if ( simple ) then
                  call dsx_paintova ( pxo, pyo, rxo, ryo, px, py,
     +                           rxa1, rya1, kc1, rxa2, rya2, kc2 )
               else
                  do k = 1, 181*3
                     kcx(k) = kcxa(k) + px
                     kcy(k) = kcya(k) + py
                  enddo
                  call dsx_paintovb ( kcx, kcy, kc1, kc2, pxo, pyo,	!Paint ovals
     +                                rxo, ryo )
               endif
               pxo = px
               pyo = py
            else
              if ( simple ) then
                 call dsx_paintova ( pxo, pyo, rxo, ryo, px, py, rxa1,
     +                               rya1, -1, rxa2, rya2, -1 )
               else
                 call dsx_paintovb ( kcx, kcy, -1, -1, pxo, pyo, rxo,
     +                               ryo )
              endif
            endif
            pxb = px
            pyb = py
            if ( PDSOPEN ) call dsx_p_hstat ( 7 )
         endif

         call dsx_getcurpb ( .true., kpx, kpy, kb, istat )
         px = kpx
         py = kpy

         if ( istat.eq.0 ) then
            if ( kb(1).eq.1 .or. kb(2).eq.1 .or. kb(3).eq.1 ) then
               if ( kb(1).eq.1 ) kbut = 1
               if ( kb(2).eq.1 ) kbut = 2
               if ( kb(3).eq.1 ) kbut = 3
               if ( kopt.eq.0 .or. kbut.eq.3 ) then			!Exit
                  loop = .false.
               else
                  if ( PDSOPEN ) call dsx_p_hstat ( 6 )
                  if ( istat.eq.0 ) then

                     rxod = 0.0
                     if ( kc1.ne.0 ) rxod = rx1
                     if ( kc2.ne.0 ) rxod = max(rxod,rx2(1),rx2(2))
                     if ( kopt.eq.1 ) then				!Repaint changed radius oval
                        if ( kbut.eq.2 ) then
                           rx1 = rx1 + 0.5
                        else
                           rx1 = max(1.0,(rx1-0.5))
                        endif
                        rxo = max(rxod,rx1) + 4
                     elseif ( kopt.eq.2 ) then
                        if ( kbut.eq.2 ) then
                           rx2(1) = rx2(1) + 0.5
                        elseif ( kbut.eq.1 ) then
                           rx2(1) = max(1.0,(rx2(1)-0.5))
                        endif
                        rxo = max(rxod,rx2(1)) + 4
                     else
                        if ( kbut.eq.2 ) then
                           rx2(2) = rx2(2) + 0.5
                        elseif ( kbut.eq.1 ) then
                           rx2(2) = max(1.0,(rx2(2)-0.5))
                        endif
                        rxo = max(rxod,rx2(2)) + 4
                     endif
                     ryo = rxo

                     if ( simple ) then

                        if ( ang.eq.90.0 .or. ang.eq.-90.0 ) then
                           rya1 = rx1
                           rxa1 = rx1*(1.0-elli)
                           rya2(1) = rx2(1)
                           rxa2(1) = rx2(1)*(1.0-elli)
                           rya2(2) = rx2(2)
                           rxa2(2) = rx2(2)*(1.0-elli)
                        else
                           rxa1 = rx1
                           rya1 = rx1*(1.0-elli)
                           rxa2(1) = rx2(1)
                           rya2(1) = rx2(1)*(1.0-elli)
                           rxa2(2) = rx2(2)
                           rya2(2) = rx2(2)*(1.0-elli)
                        endif
                        call dsx_paintova ( pxo, pyo, rxo, ryo, px,
     +                       py, rxa1, rya1, kc1, rxa2, rya2, kc2 )
                     else
                        call dsx_setell ( ang, elli, rx1, rx2,
     +                                    kcx, kcy )
                        do k = 1, 181*3
                           kcx(k) = kcxa(k) + pxo
                           kcy(k) = kcya(k) + pyo
                        enddo
                        call dsx_paintovb ( kcx, kcy, kc1, kc2,  	!Paint ovals
     +                                      pxo, pyo, rxo, ryo )
                     endif

                  else

                     if ( simple ) then
                        call dsx_paintova ( pxo, pyo, rxo, ryo, px, py,
     +                               rxa1, rya1, -1, rxa2, rya2, -1 )
                     else
                        call dsx_paintovb ( kcx, kcy, -1, -1,		!Paint ovals
     +                                      pxo, pyo, rxo, ryo )
                     endif

                  endif
                  if ( PDSOPEN ) call dsx_p_hstat ( 7 )
               endif
            endif
         endif

      enddo

      if ( simple ) then						!Paint $
         call dsx_paintova ( pxo, pyo, rxo, ryo, px, py, rxa1, rya1,
     +                       -1, rxa2, rya2, -1 )
      else
         do k = 1, 181*3
            kcx(k) = kcxa(k) + px
            kcy(k) = kcya(k) + py
         enddo
         call dsx_paintovb ( kcx, kcy, -1, -1, pxo, pyo, rxo, ryo )
      endif
      if ( PDSOPEN ) call dsx_p_hstat ( 1 )

      kpx = px
      kpy = py
      call vt_tsi ( kpx, kpy, kxp, kyp )				!Image posn

      DSCURPOSX = px
      DSCURPOSY = py
      DSCURSET = .true.

      ierr = 0
      if ( istat.ne.0 ) ierr = 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DSX_SETELL -- Set ellipse shape in screen coords
C
C    alan penny           ral                       1990-02-01

      subroutine dsx_setell ( ang, elli, rx1, rx2, kcx, kcy )

      implicit none
      include 'ST_DS_GEN_INC'

      real    ang		!i: angle
      real    elli		!i: Ellipticity
      real    rx1		!i: 1st ellipse major axis
      real    rx2(2)		!i: 2nd and third ellipses major axes
      integer kcx(*)		!o: X coords of ellipses in screen coords
      integer kcy(*)		!o: Y coords of ellipses in screen coords

C--
      integer j, k, ka, kk
      real  rv, trv, ttrv, sav, cav, dx, dy, rx, ry, afx, afy, dfx,
     +      dfy, ryy, sdx(4), sdy(4), dax(45), day(45), f1, f2, f3, f4
      data sdx / 1.0, 1.0, -1.0, -1.0 /
      data sdy / 1.0, -1.0, -1.0, 1.0 /
Cbegin


      sav = -1.0*sin(ang*3.14159/180.0)
      cav = cos(ang*3.14159/180.0)

      ry = 1.0 - elli
      ry = min(1.0,max(0.0,ry))
      ryy = 1.0e12
      if ( ry.gt.1.0e-6 ) ryy = 1.0/(ry*ry)
      do k = 1, 45

         rv = real(k-1)*2*3.14159/180.0
         trv = tan(rv)
         ttrv = trv*trv

         if ( abs(trv).lt.1.0e-5 ) then
            dax(k) = 0.0
            day(k) = ry
         elseif ( trv.gt.1.0e5 .or. trv.lt.-1.0e5 ) then
            dax(k) = 1.0
            day(k) = 0.0
         else
            dax(k) = sqrt(1.0/(1.0+(ryy/ttrv)))
            day(k) = sqrt(1.0/(ryy+(1.0*ttrv)))
         endif
      enddo

      afx = real(DSZM)/real(DSCOMFX)
      afy = real(DSZM)/real(DSCOMFY)
      dfx = DSZM/2 - 1
      dfy = DSZM/2 - 1
      do j = 1, 3

         if ( j.eq.1 ) then
            rx = rx1
         elseif ( j.eq.2 ) then
            rx = rx2(1)
         else
            rx = rx2(2)
         endif

         do kk = 1, 4
            ka = (j-1)*181 + (kk-1)*45
            f1 = cav*rx*sdx(kk)
            f2 = sav*rx*sdy(kk)
            f3 = -1.0*sav*rx*sdx(kk)
            f4 = cav*rx*sdy(kk)
            do k = 1, 45
               if ( kk.eq.2 .or. kk.eq.4 ) then
                  dx = f1*dax(46-k) + f2*day(46-k)
                  dy = f3*dax(46-k) + f4*day(46-k)
               else
                  dx = f1*dax(k) + f2*day(k)
                  dy = f3*dax(k) + f4*day(k)
               endif
               kcx(k+ka) = dx*afx + dfx
               kcy(k+ka) = dy*afy + dfy
            enddo
         enddo
         kcx(((j-1)*181)+181) = kcx(((j-1)*181)+1)
         kcy(((j-1)*181)+181) = kcy(((j-1)*181)+1)

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DSX_ZOOM -- Display (zoomed/panned) actual image from virtual image  (X windows)
C
C    alan penny           ral                       1990-02-01

      subroutine dsx_zoom ( monitor, kopt, kopta )

      implicit none
      include 'ST_DS_GEN_INC'

      logical monitor 		!i: Display posn, val on terminal/panel?
      integer kopt		!i: Zoom type (0=mouse control;1=reset to null)
      integer kopta		!i: Flag (1=Start/stop message; 0 =not)
C--
      integer ierr, kbuta, kbutb, kxov, kyov, jsx, jsy, jsxa, jsya, jd
      logical more, isin, doit
Cbegin


      if ( kopt.eq.1 ) then
         DSZM = 1							!Reset to centre
         DSZPX = 1
         DSZPY = 1
         call dsx_vtim ( 1, DSSNX, 1, DSSNY, 1 )
         return
      endif

      if ( kopta.eq.1 ) call printo ( 'Start two-button zoom/pan' )

      more = .true.							!Zoom/pan by hand
      do while ( more )

         isin = .false.							!1st press
         do while ( .not.isin )
            call dsx_gscur ( monitor, jsx, jsy, kbuta, ierr )
            if ( ierr.ne.0 ) then
               call printo ( 'ERROR: Cant do that' )
               return
            endif
            if ( jsx.ge.1 .and. jsx.le.DSSNX .and.
     +           jsy.ge.1 .and. jsy.le.DSSNY ) isin = .true.
         enddo

         if ( kbuta.ne.3 ) then						!2nd press
            call dsx_p_hstat ( 8 )
            isin = .false.
            do while ( .not.isin )
               call dsx_gscur ( monitor, jsx, jsy, kbutb, ierr )
               if ( ierr.ne.0 ) then
                  call printo ( 'ERROR: Cant do that' )
                  return
               endif
               if ( jsx.ge.1 .and. jsx.le.DSSNX .and.
     +              jsy.ge.1 .and. jsy.le.DSSNY ) isin = .true.
            enddo
         endif

         if ( kbuta.eq.3 .or. kbutb.eq.3 ) then				!End

            more = .false.

         elseif ( kbuta.eq.3 .or. kbutb.eq.3 ) then			!Error

            call printo ('WARNING: Forbidden two-button combination')
            call printo ('         - ignored')

         elseif ( (kbuta.eq.1 .and. kbutb.eq.2) .or.			!Get shift of display
     +            (kbuta.eq.2 .and. kbutb.eq.1) ) then

            jsxa = jsx - (DSSNX/2)
            jsya = jsy - (DSSNY/2)
            doit = .false.
            if ( abs(jsxa).gt.(DSZM/2) ) then
               jd = 1 + int((abs(jsxa)-(DSZM/2))/DSZM)
               jd = jd*jsxa/abs(jsxa)
               DSZPX = DSZPX + jd
               doit = .true.
            endif
            if ( abs(jsya).gt.(DSZM/2) ) then
               jd = 1 + int((abs(jsya)-(DSZM/2))/DSZM)
               jd = jd*jsya/abs(jsya)
               DSZPY = DSZPY + jd
               doit = .true.
            endif
            if ( doit ) then
               call dsx_p_hstat ( 4 )
               call dsx_vtim ( 1, DSSNX, 1, DSSNY, 1 )
               call dsx_p_hstat ( 0 )
               DSCURPOSX = DSSNX/2
               DSCURPOSY = DSSNY/2
               call dsx_pscur ( DSCURPOSX, DSCURPOSY )
            endif

         else								!Zoom

            doit = .false.
            call vt_tsv ( jsx, jsy, kxov, kyov )
            if ( kbuta.eq.1 .and. DSZM.ne.1 ) then
               DSZM = DSZM/2
               doit = .true.
            endif
            if ( kbuta.eq.2 .and. DSZM.ne.DSZOOMMAX ) then
               DSZM = DSZM*2
               doit = .true.
            endif
            if ( doit ) then
               DSZPX = kxov - ((jsx-1)/DSZM)
               DSZPY = kyov - ((jsy-1)/DSZM)
               call dsx_p_hstat ( 5 )
               call dsx_vtim ( 1, DSSNX, 1, DSSNY, 1 )
               call dsx_p_hstat ( 0 )
               call vt_tvs ( kxov, kyov, jsx, jsy )
               DSCURPOSX = jsx + (DSZM/2)
               DSCURPOSY = jsy + (DSZM/2)
               call dsx_pscur ( DSCURPOSX, DSCURPOSY )
            endif

         endif
         call dsx_p_hstat ( 0 )

      enddo

      if ( kopta.eq.1 ) call printo ( 'End two-button zoom/pan' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DSX_BLINK -- Blink an image
C
C  alan penny           ral                         1990-06-09

      subroutine dsx_blink ( frames, period, fraction )

      implicit none


      integer	frames(2)	!i: dummy
      real	period		!i: dummy
      real	fraction	!i: dummy
C--
      integer kf, kp, istat, iwait, kbut, kbuta, kpx, kpy, kb(3)
      logical loop, loopa
Cbegin


      kf = 1
      iwait = 512
      kp = 1
      loop = .true.
      do while ( loop )

         call get_job ('BLINK_CH','under:over:auto:exit',kp,kp,' ',0)

         if ( kp.eq.3 ) then
            call dsx_waitbut ( .true., .false., kbut, kpx, kpy )	!Wait till all buttons up
            call dsx_getcurpb ( .true., kpx, kpy, kb, istat )

            loopa = .true.
            do while ( loopa )
               kbut = 0
               call dsx_getcurpb ( .true., kpx, kpy, kb, istat )	!Get if button

               if ( istat.eq.0 ) then
                  if ( kb(1).eq.1 .or. kb(2).eq.1 .or. kb(3).eq.1 )then
                     loop = .false.
                     if ( kb(1).eq.1 ) kbut = 1
                     if ( kb(2).eq.1 ) kbut = 2
                     if ( kb(3).eq.1 ) kbut = 3
                  endif
                  if ( kbut.ne.0 ) call dsx_waitbut ( .true., .false.,
     +                                            kbuta, kpx, kpy )
               endif

               if ( kbut.eq.1 ) iwait = min(16384,(iwait*2))
               if ( kbut.eq.2 ) iwait = max(16,(iwait/2))
               if ( kbut.eq.3 ) loopa = .false.

               if ( loopa ) then
                  call dsx_mswait ( iwait )
                  call dsx_updown ( kf )
                  if ( kf.eq.1 ) then
                     kf = 2
                  else
                     kf = 1
                  endif
               endif
            enddo

         endif

         if ( kp.eq.1 .or. kp.eq.2 ) then
            call dsx_updown ( kf )
            if ( kf.eq.1 ) then
               kf = 2
            elseif ( kf.eq.2 ) then
               kf = 1
            else
               loop = .false.
            endif
            kp = kf
         endif

         if ( kp.eq.4 ) loop = .false.

      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DSX_PANELF.FOR    Fortran 'panel' sections subroutines
C
C
C DSX_P_SLOAD     Load a panel (sections)  (dsxindows)
C DSX_P_GSBOX     Get the box chosen (sections) (dsxindows)




CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DSX_P_SLOAD -- Load a panel (sections) (X windows)
C
C    alan penny           ral                       1990-02-01

      subroutine dsx_p_sload ( cmdlsts, hopt, nopt, num )

      implicit none
      include 'ST_DS_PANEL_INC'

      integer       nopt		!i: Number of sections
      character*(*) cmdlsts(nopt)	!i: Text of options in sections
      character*(*) hopt(nopt)		!i: Headers for sections
      integer	    num			!i: Option list code number
C--
      integer   k, ks, ke, ksw(200), klw(200),
     +          kl, ka, jx, jy, jtot, jb
      integer lens
      external lens
      character cmdlst*2000
Cbegin


      PDNUML = num							!Load option list code

      do k = 1, PNNCOL							!Load background
         ks = (k-1)*91
         call dsx_p_carea ( ks, 0, 91, PNSNY, 0 )
      enddo

      jb = 0
      jtot = 0
      do ka = 1, nopt

         call gcmdlsta ( cmdlsts(ka), cmdlst, kl, PNNUMS(ka), ksw,klw)	!Squeeze input list

         jtot = jtot + 1						!Header
         if ( (PNNROW*(jtot/PNNROW)).eq.jtot ) jtot = jtot +1		!No 'header' at column bottom
         jx = 1 + ((jtot-1)/PNNROW)
         jy = PNNROW - (jtot-(jx-1)*PNNROW) + 1
         jx = (jx-1)*91 + 4
         jy = (jy-1)*26 + 4
         kl = lens(hopt(ka))
         call dsx_p_puttxt ( hopt(ka), kl, jx+9, jy+5, 3 )

         if ( cmdlst.ne.' ' .and. PNNUMS(ka).gt.0 ) then		!Load boxes
            do k = 1, PNNUMS(ka)
               ks = ksw(k)
               ke = ks + klw(k) - 1
               jtot = jtot + 1
               jx = 1 + ((jtot-1)/PNNROW)
               jy = PNNROW - (jtot-(jx-1)*PNNROW) + 1
               jb = jb + 1
               PNX(jb) = (jx-1)*91 + 4
               PNY(jb) = (jy-1)*26 + 4
               call dsx_p_sbox ( jb, cmdlst(ks:ke), 1, 0 )
            enddo
         endif

      enddo

      jtot = jtot + 2							!Help box
      jx = 1 + (jtot-1)/PNNROW
      jy = PNNROW - (jtot-(jx-1)*PNNROW) + 1
      if ( jy.eq.PNNROW ) then
         jtot = jtot + 1
         jx = 1 + (jtot-1)/PNNROW
         jy = PNNROW - (jtot-(jx-1)*PNNROW) + 1
      endif
      PNHPOSX = (jx-1)*91 + 4
      PNHPOSY = (jy-1)*26 + 4
      call dsx_p_hbox

      jtot = jtot + 3							!Posn, Value box
      jx = 1 + (jtot-1)/PNNROW
      jy = PNNROW - (jtot-(jx-1)*PNNROW) + 1
      if ( jy.ge.(PNNROW-1) ) then
         if ( jy.eq.PNNROW ) jtot = jtot + 2
         if ( jy.eq.(PNNROW-1) ) jtot = jtot + 1
         jx = 1 + (jtot-1)/PNNROW
         jy = PNNROW - (jtot-(jx-1)*PNNROW) + 1
      endif
      PNVPOSX = (jx-1)*91 + 4
      PNVPOSY = (jy-1)*26 + 13 + 4


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DSX_P_GSBOX -- Get the box chosen (sections) (dsxindows)
C
C  alan penny             ral             1990 Jan

      subroutine dsx_p_gsbox ( cmdlsts, hopt, nopt, ktopt, ktdef,
     +                         thelp, nh, opt_num, opt_text, opt_head,
     +                         opt_help, title, hpaneldo, ierr )

      implicit none
      include 'ST_DS_PANEL_INC'

      integer        nopt		 !i: No of sections
      character*(*)  cmdlsts(nopt)	 !i: Command lists
      character*(*)  hopt(nopt)		 !i: Section headers
      character*(*)  ktopt		 !i/o: Last Chosen option/New chosen option
      character*(*)  ktdef		 !i: Option default choice
      integer	     nh			 !i: No of help lines
      character*68   thelp(nh)		 !i: Help text
      integer        opt_num		 !i: Number of options
      character*12   opt_text(opt_num)   !i: Possible options  (ignores case)
      character*68   opt_head(opt_num)   !i: Single line helps
      character*68   opt_help(6,opt_num) !i: Fuller helps
      character*(*)  title		 !i: Panel title
      logical        hpaneldo		 !i: Use help panel
      integer        ierr    		 !o: Error flag (0=ok; 1=bad)
C--
      logical loop, inbox
      real px, py, xs, xe, ys, ye, akx, aky
      integer kx, ky, k, jcon(200), kww, ksw(200), klw(200), num, kl,
     +        ka, ks, ke, kbb(3), istat
      character cmdlst*2000, textc*2000, ktopta*12
      integer lens
      external lens
Cbegin


      textc = cmdlsts(1)
      if ( nopt.gt.1 ) then
         do k = 2, nopt
            kl = lens(textc)
            textc = textc(1:kl)//':'//cmdlsts(k)
         enddo
      endif

      call gcmdlsta ( textc, cmdlst, kl, num, ksw, klw )		!Squeeze command list

      do k = 1, 200							!'Order' of commands
         jcon(k) = k
      enddo

      ka = 0								!Location of last chosen box
      do k = 1, num							! Make box 'up'
         ks = ksw(k)
         ke = ks + klw(k) - 1
         if ( ktopt.eq.cmdlst(ks:ke) ) ka = k
      enddo
      if ( ka.ne.0 ) call dsx_p_sbox ( ka, ' ', 1, 1 )

      ka = 1								!Location of default box
      do k = 1, num							! Set cursor at box
         ks = ksw(k)
         ke = ks + klw(k) - 1
         if ( ktdef.eq.cmdlst(ks:ke) ) ka = k
      enddo
      call dsx_p_sscur ( ka )

      call dsx_p_hstat ( 1 )						!Tell waiting

      ktopt = ' '
      loop = .true.
      do while ( loop )

         ktopta = ktopt
         kww = 0

         call dsx_mswait ( 50 )
         call dsx_getcurpb ( .false., kx, ky, kbb, istat )

         px = kx
         py = ky

         inbox = .false.
         k = 0								!In box?
         do while ( .not.inbox .and. k.lt.num )
            k = k + 1
            xs = PNX(k)
            ys = PNY(k)
            xe = xs + 86.0
            ye = ys + 21.0
            if ( px.ge.xs .and. px.le.xe .and. py.ge.ys .and.
     +           py.le.ye ) then
               inbox = .true.
               kww = 1
               ks = ksw(k)
               ke = ks + klw(k) - 1
               ktopt = cmdlst(ks:ke)
            endif
         enddo

         akx = PNHPOSX					 		!In Help box?
         aky = PNHPOSY
         if ( px.ge.akx .and. px.le.(akx+86.0) .and.
     +        py.ge.aky .and. py.le.(aky+46.0) ) then
             kww = 2
             inbox = .true.
             ktopt = 'XX_HELP'
         endif

         if ( inbox ) then
            if ( ktopt.ne.ktopta .and. hpaneldo ) then
               call hx_hload ( ktopt, opt_text, opt_head, opt_help,
     +                         opt_num )
            endif
         else
            if ( kx.le.1 .or. kx.ge.PNSNX .or. ky.le.1 .or.
     +           ky.ge.PNSNY ) then
               if ( ktopta.ne.' ' .and. hpaneldo ) then
                  call dsx_p_hx_load ( title )
               endif
               ktopt = ' '
            endif
         endif

         if ( inbox .and. (kbb(1).eq.1 .or. kbb(2).eq.1 .or.
     +        kbb(3).eq.1) ) then
            loop = .false.
            if ( kww.eq.1 ) then
               call dsx_p_sbox ( k, ' ', 0, 1 )				!Mark edge box down
               call dsx_p_hstat ( 0 )					!Tell working
            endif
            if ( kww.eq.2 ) then
               call dsx_p_hstat ( 2 )					!Tell helping
               call tyhelp ( thelp, nh, cmdlst,ksw,klw,jcon, num )
               call dsx_p_hstat ( 1 )					!Tell waiting
            endif
         endif

      enddo

      ierr = 0


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C VT_GENF.FOR    (Fortran) Screen, image, virtual image transformations
C
C  Includes:-
C
C VT_TIS     Translate image to screen coords
C VT_TSI     Translate screen to image coords
C VT_TSV     Translate screen to virtual coords
C VT_TVS     Translate virtual to screen coords


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C VT_TIS -- Translate image cords to screen coords
C
C    alan penny           ral                       1990-02-01

      subroutine vt_tis ( kxi, kyi, kxo, kyo )

      implicit none
      include 'ST_DS_GEN_INC'

      integer    kxi		!i: Image X position
      integer    kyi		!i: Image Y position
      integer    kxo		!o: Screen X position at b.l.h. of image pixel
      integer    kyo		!o: Screen Y position at b.l.h. of image pixel
C--
      integer j, k
Cbegin


      call ds_tiv ( kxi, kyi, j, k )			!Convert image to virtual
      call vt_tvs ( j, k, kxo, kyo )			!Convert virtual to screen

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C VT_TSI -- Translate screen coords to image coords
C
C    alan penny           ral                       1990-02-01

      subroutine vt_tsi ( kxi, kyi, kxo, kyo )

      implicit none
      include 'ST_DS_GEN_INC'

      integer   kxi		!i: Screen X position
      integer   kyi		!i: Screen Y position
      integer   kxo		!o: Image X position
      integer   kyo		!o: Image Y position
C--
      integer j, k
Cbegin


      call vt_tsv ( kxi, kyi, j, k )			!Screen to virtual
      call ds_tvi ( j, k, kxo, kyo )			!Virtual to image


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C VT_TSV -- Translate screen to virtual screen coords
C
C    alan penny           ral                       1990-02-01

      subroutine vt_tsv ( kxi, kyi, kxo, kyo )

      implicit none
      include 'ST_DS_GEN_INC'

      integer    kxi		!i: Screen X position
      integer    kyi		!i: Screen Y position
      integer    kxo		!o: Virtual X position
      integer    kyo		!o: Virtual Y position
C--
Cbegin


      kxo = DSZPX + int((kxi-1)/DSZM)
      kyo = DSZPY + int((kyi-1)/DSZM)

CX      PRINT*,'F VT_TSV i:', kxi,kyi,' o: ', kxo, kyo
CX      PRINT*,' '

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C VT_TVS -- Translate virtual to screen coords
C
C    alan penny           ral                       1990-02-01

      subroutine vt_tvs ( kxi, kyi, kxo, kyo )

      implicit none
      include 'ST_DS_GEN_INC'

      integer    kxi		!i: Virtual X position
      integer    kyi		!i: Virtual Y position
      integer    kxo		!o: Screen X position at b.l.h. of image pixel
      integer    kyo		!o: Screen Y position at b.l.h. of image pixel
C--
Cbegin


      kxo = 1 + DSZM*(kxi-DSZPX)
      kyo = 1 + DSZM*(kyi-DSZPY)

CX      PRINT*,'F VT_TVS i:', kxi,kyi,' o: ', kxo, kyo
CX      PRINT*,' '


      end
