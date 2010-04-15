#if HAVE_CONFIG_H
# include "config.h"
#endif
*+  ECHWIND  -  interactive configuration program for echelle spectrographs

      program echwind
*
*   Description :
*
*     Displays a representation of the spectral format of the UCLES or
*     the UES and allows the user to determine the desired wavelength
*     coverage interactively
*
*   Invocation :
*
*     ECHWIND
*
*   Parameters :
*
*     DEVICE    = CHARACTER (READ, WRITE)   the image display device to use
*     ERASE     = LOGICAL (READ)            whether to reset the display
*     OBSFILE   = CHARACTER (READ)          file in which to store the results
*     SPECTROGRAPHS = CHARACTER (READ)      file defining avail spectrographs
*     ECHELLE   = CHARACTER (READ)          which echelle grating to use
*     CONFIG    = CHARACTER (READ)          which instrument configuration
*                                           to use
*     DETECTOR  = CHARACTER (READ, WRITE)   which detector to use
*     ANGWAVE   = REAL (READ)               the detector rotation angle or
*                                           wavelength whose order is to be
*                                           aligned with the detector
*     LINELIST  = CHARACTER (READ)          file with list of lines to plot
*
*   Arguments:
*
*     STATUS = INTEGER (READ, WRITE)        Global status value
*
*   Method :
*
*
*   Bugs :
*
*     None known.
*
*   Authors :
*
*     C.J. Hirst  UCL  (ZUVAD::CJH)
*
*   History :
*
*     12 Jan 3000BC : Original, prehistoric version (ZUVAD::CJH)
*     26 Jul 1988   : Total re-write; now much more useful (ZUVAD::CJH)
*     13 Nov 1988   : Include files now taken directly from .INC files and not
*                     from text library.
*                     Marked boxes now drawn in green instead of red.
*                     Menu items now only selected when cursor actually on them.
*                     Arguments chan and dispid in idi_open call put into
*                     correct order (although CJH's IDI implementation
*                     ignores them.)
*                     Order of menu items changed slightly, and EXIT changed
*                     to Save & EXIT, and ABANDON change to QUIT.
*                     Call to idi_close added.  All RETURNs except for the
*                     very first change to `goto 500's in order to ensure
*                     idi_close gets called on the way out. (AAOCBN::KS)
*     17 Feb 1989   : Use logical names for INCLUDE files; correct declaration
*                     error in CENTRE_DETECTOR; pass selected echelle to
*                     LOG_POSITION; add ECHELLE menu option to change echelle;
*                     change "Save and EXIT" and "QUIT" to "SAVE" and "EXIT",
*                     always prompting for the file-name; pass X and Y
*                     positions to WHERE_AM_I to permit calculation of
*                     wavelengths at positions other than the centre; re-work
*                     LOG_POSITION to display min and max wavelengths as WELL
*                     (AAOCBN::WFL)
*     18 Feb 1989   : Detect error to open line-list file; make min/max wave-
*                     lengths the true extremes; make NEW GRATING SWAP GRATING;
*                     rename WHERE_AM_I as HOW_DO_I_GET_THERE and use new
*                     faster WHERE_AM_I to calculate things for display
*     19 Feb 1989   : Correct bug due to misuse of echelle model routines;
*                     use private crash-proof DISP routine (AAOCBN::WFL)
*     10 Sep 1989   : Change all references to MANUAL to refer to ECHWIND;
*                     support files containing lists of detector names;
*                     use ERR_OUT for errors; remove private version of DISP
*                     (AAOEPP::WFL)
*     11 Sep 1989   : Support detector rotation (AAOEPP::WFL)
*     12 Sep 1989   : Support FORMAT=format parameter to control the format of
*                     file output by the SAVE option; support formats of
*                     AAT (as at present), WHT (hardware default file) and
*                     TEXT (human readable); remove FCAM parameter; support
*                     specification of angle or wavelength and provide menu
*                     option to alter it (AAOEPP::WFL)
*     12 Sep 1989   : Support WAVELENGTH and FACTOR parameters to set initial
*                     central wavelength and zoom factors; rework WHERE_AM_I
*                     algorithm to give best estimates for MIN and MAX
*                     wavelengths (AAOEPP::WFL)
*     08 Dec 1989   : Use new SPECTROGRAPHS parameter and load spectrograph
*                     parameters file; add WAVELENGTH menu option; improve
*                     error messages for non-existent files; use DFLFILE for
*                     default list name; alter some internal interfaces
*     12 Dec 1989   : Use new ECH_ model routines
*
*      July 1991    : A complete hack to make it work under Unix  (SJM/MSSSO)
*     23 Sep 1994   : Amended to support UES as well as UCLES, same changes
*                     as made to VAX version. (MPF/RGO)
*     22 Feb 1995   : Many changes to graphics routines because there is only
*                     one memory; tidied up use of PGPLOT routines; removed
*                     unused variables.
*      3 Mar 1995   : Added more data to text file, now same as on VAX version.
*                     Remove marked boxes on echelle change. Added instrument
*                     name to display; draw menu in white for clarity, chosen
*                     item in green. If detector box is completely outside
*                     the echellogram area, write message instead of wavelength
*                     coverage. Zoom is always 1 on startup. (MPF/RGO)
*     16 Jul 1996   : Modified number of colours to be used for spectrum from
*                     120 to 80 to allow for default number of colours in
*                     native PGPLOT /xwin window as define by pgxwin_server
*                     in pgplot v5.1.  (BLY/RAL)
*     26 Sep 1996   : Removed output from save_text about 'Echwind V2.0 -
*                     UCL Optical Laboratory'. Removed echelle from
*                     argument list for save_text. It was not used. (ACC/RAL)
*     30 Sep 1996   : Since save_AAT was commented out, got rid of it.
*                     Expanded save_text to write out maximum and minimum
*                     wavelengths and detector length. (ACC/RAL)
*      1 Oct 1996   : Moved all graphics calls to after all prompts were
*                     made. This entailed the creation of three new subroutines:
*                     prepare_graphics, setup_colourtable, write_instrument.
*                     (ACC/RAL)
*     05 Feb 1997   : Modified calls to str$upcase and str$trim to str_upcase
*                     and str_trim respectively for Linux port (BLY,RAL).
*     28 Feb 1998   : Initialise STATUS to 0.
*     21 Aug 2005   : Use FPP for ACCESS mode. Replace LNBLNK with internal
*                     version (str_trim) to be more portable.
*
*   Type definitions :
*
      implicit none              ! no default typing allowed
*
*   Global constants :
*
      include 'SAE_PAR'
*
*   Status :
*
      integer status
*
*   External References :
*
*   Global Variables :
*
      include 'ech_common'
      include 'echwind_par'
      include 'echwind_orders'
      include 'echwind_mmpix'
*
*   Local variables :
*
      logical dopen, done
      real slit_angle, prism_pos, theta, gamma
      character*80 colour, spectrographs, echelle, buff, format
      integer xsize, ysize        ! size of whole display device in pixels
      character*80 action
      integer mc, tx, ty, lbuff
      real detx1, dety1, detx2, dety2, factor, wc
      real detxc, detyc, echxc, echyc
      real dsize(2)               ! detector size in mm (x and y)
      integer mstatus, istat
      integer cursor_present
      integer leninst
      character*16 curinst
      character*80 config
*
*-----------------------------------------------------------------------
*  Initialise status to zero (otherwise it won't work on Linux!)

      status = 0
*
*  Get the name of the spectrograph parameters file and load it
*
      call ech_load('SPECTROGRAPHS', spectrographs, status)
      if(status.ne.sai__ok) goto 500
*
*  Get the name of the echelle grating to use
*
      call ech_init('CONFIG', echelle, status)
      if(status.ne.sai__ok) goto 500
*
*  Get the names of supported detectors and the detector size
*
      call det_names
      call det_size(dsize, status)
      if(status.ne.sai__ok) goto 500
*
*  Get the angle through which to rotate the detector
*
      call det_angle(angle, status)
      if(status.ne.sai__ok) goto 500
*
*  Determine the format of files written as a result of SAVE. The interface
*  file guarantees either "AAT" or "TEXT", although only "TEXT" is supported
*  in the Unix version
*
*      call par_get0c('FORMAT', format, status)
*      call str_upcase(format, format)
*      if(status.ne.sai__ok) goto 500
      format = 'TEXT'
*
*  Determine the initial detector position and window size (100x100mm will be
*  scaled by the zoom factor later)
*
      call wind_cent(echxc, echyc, status)
      if(status.ne.sai__ok) goto 500
      swinx=100.0
      swiny=100.0
      factor = 1
*
*  Read any lines that are to be displayed
*
      call read_lines
*
*  Prepare and display graphics window
*
      call prepare_graphics(dopen,xsize,ysize,istat)
      if(istat.ne.1) goto 500
*
*  Set up a rainbow colour table between lut values IR and UV.  Also
*  set up a grey lut value
*
      call setup_colourtable(xsize,ysize)
*
* Write name of instrument
*
      call write_instrument(xsize,ysize,tx,ty,curinst,leninst)
*
*  Determine the initial detector centre (mm) and draw the detector
*
      call echtodet(echxc, echyc, detxc, detyc)
      call centre_detector(dsize, detxc, detyc, factor)
*
*  Draw all the spectral orders and write some labels (including the
*  size of the detector)
*
      call draw_orders(dsize, xsize, ysize)
*
*  Plot any lines that are to be displayed
*
      call draw_lines
*
*  Draw detector and display wavelength range
*
      call plot_drawdet(WHITE)
      call log_position(xsize, ysize)
*
*  Initialise number of boxes and enter polling loop
*
      nboxes=0
      done=.false.
      do while (.not.done)     ! until told to stop
*
*  Check the mouse buttons (and cursor position)
*
         call plot_movcur(cursor_present,mstatus)

         if(cursor_present.ne.1) then
            write(*,*)
     :        'You have requested a device which has no cursor.'
            write(*,*)
     :        'If you requested /ps, a plot file has been created.'
            goto 500
         endif

         if(mstatus.eq.0)then  ! cursor moved to new detector position
            action='CONTINUE'
            call draw_display(dsize, xsize, ysize)
         else                  ! menu button was pressed, get selected action
            call get_action(action, factor, xsize, ysize)
         endif

         if(action.eq.'CONTINUE')then
*
*  No action selected - do nothing
*
         elseif(action.eq.'BOX')then
*
*  If there's room, read the current detector position (in mm) and
*  mark it on the display
*
            if(nboxes.lt.MAX_BOXES)then
               nboxes=nboxes+1
               call det_mm(boxes(1, nboxes), boxes(2, nboxes),
     :                     boxes(3, nboxes), boxes(4, nboxes))
               call draw_box(boxes(1, nboxes))
            endif
*
*  Find out where instrument would be for this configuration
*
            call det_mm(detx1, dety1, detx2, dety2)
            detxc=(detx1+detx2)/2.0
            detyc=(dety1+dety2)/2.0
            call dettoech(detxc, detyc, echxc, echyc)
            call how_do_i_get_there(echxc, echyc, theta, gamma,
     :                              colour, prism_pos, slit_angle)
*
*  Work out nearest wavelength to detector centre
*
            call ech_wavecen(theta, gamma, wc, mc, status)
*
*  and tell the user
*
            write(buff, '(''Wavelength nearest centre is'', f9.2,'//
     :            ''' Angstroms in Order'', i4)') wc, mc
            write(*,*) buff

         elseif(action.eq.'DELETEBOX')then
*
*  Delete the last detector position stored and redraw the boxes
*
            nboxes=nboxes-1
            nboxes=max(0, nboxes)
            call draw_display(dsize, xsize, ysize)

         elseif(action.eq.'CLEARBOXES')then
*
*  Clear all the stored detector positions
*
            nboxes=0
            call draw_display(dsize, xsize, ysize)

         elseif(action.eq.'LINES')then
*
*  Read and draw another list of line wavelengths and labels
*
            call read_lines
            call draw_lines

         elseif(action.eq.'DETECTOR')then
*
*  Let the user specify a different detector to use from now on
*
            status=sai__ok
            call det_tx(dsize, xsize, ysize, BLACK)
            call det_size(dsize, status)    ! get the new size
            call det_mm(detx1, dety1, detx2, dety2)
            detxc=(detx1+detx2)/2.0         ! initialise the new det. window
            detyc=(dety1+dety2)/2.0
            call centre_detector(dsize, detxc, detyc, factor)
            call draw_display(dsize, xsize, ysize)
            call det_tx(dsize, xsize, ysize, WHITE)

         elseif(action.eq.'ANGLE')then
*
*  Let the user specify a different detector angle or wavelength
*  for which the orders will be horizontal
*
            status=sai__ok
            call det_angle(angle, status)      ! get the new angle
            call det_mm(detx1, dety1, detx2, dety2)
            detxc=(detx1+detx2)/2.0            ! the new detector window
            detyc=(dety1+dety2)/2.0
            call centre_detector(dsize, detxc, detyc, factor)
            call draw_display(dsize, xsize, ysize)

         elseif(action.eq.'WAVELENGTH')then
*
* Let the user specify a different central wavelength
*
            status=sai__ok
            call wind_cent(echxc, echyc, status)
            call echtodet(echxc, echyc, detxc, detyc)
            call centre_detector(dsize, detxc, detyc, factor)
            call draw_display(dsize, xsize, ysize)

         elseif(action.eq.'ECHELLE')then
*
*  Use the other echelle
*
            status=sai__ok
            if(echelle.eq.'31')then
               echelle='79'
            else
               echelle='31'
            endif

            config = curinst(1:leninst)//'/'//echelle
            call ech_init(' ', config, status)
            call det_mm(detx1, dety1, detx2, dety2)
            detxc=(detx1+detx2)/2.0            ! the new detector window
            detyc=(dety1+dety2)/2.0
            call pgsci(BLACK)
            call pgrect(float(tx),float(xsize-1),float(ty),
     :                  float(ysize-1))
            write(buff, '(''Echelle:           '',f5.1,'' lines/mm'')')
     :            ech_d
            call str_trim(buff, buff, lbuff)
            call plot_text(buff, tx, ty, WHITE)
            call centre_detector(dsize, detxc, detyc, factor)
            nboxes=0
            call draw_display(dsize, xsize, ysize)

         elseif(action.eq.'CENTRE')then
*
*  Redraw the echellogram centred on the current detector position
*
            call det_mm(detx1, dety1, detx2, dety2)
            detxc=(detx1+detx2)/2.0            ! the new detector window
            detyc=(dety1+dety2)/2.0
            call centre_detector(dsize, detxc, detyc, factor)
            call draw_display(dsize, xsize, ysize)

         elseif(action.eq.'ZOOM')then
*
*  Redraw the echellogram at a different scale (and centred on the
*  current detector position)
*
            call det_mm(detx1, dety1, detx2, dety2)
            detxc=(detx1+detx2)/2.0            ! the new detector window
            detyc=(dety1+dety2)/2.0
            call centre_detector(dsize, detxc, detyc, factor)
            call draw_display(dsize, xsize, ysize)

         elseif(action.eq.'SAVE')then
            call save_text

         elseif(action.eq.'EXIT')then
*
*  Leave the program
*
            done=.true.
         endif

*
*  If we haven't finished, go back to wait for button press
*
      enddo
*
*  Return and close down device on the way out.
*
  500 continue
      if (dopen) call pgend
      end

      subroutine prepare_graphics(dopen,xsize,ysize,status)
*
*  Prepare and display graphics window
*
*  Modification History:
*    01-OCT-1996, ACC:
*    Subroutine created in order to place graphics window creation after
*    all screen prompts, rather than before.
*
      implicit none
*
*  Given:
*    None
*
*  Returned:
*    Whether display was opened successfully
      logical dopen
*    Size of the whole display device in pixels
      integer xsize, ysize
*    Whether display was opened successfully
      integer status
*
*  Local:
      integer pgbegin
      integer low,high
*
*  Global:
      include 'echwind_par'

      dopen=.false.
*
*  Get name of image display device to use
*
      status = pgbegin(0,'?',1,1)
      if (status .ne. 1) goto 500
      dopen=.true.
*
*  Inquire the display dimensions and set up the correct window on
*  the screen in pixels
*
      call plot_dispsize(xsize, ysize)     ! get world coord space
      call pgpaper(8.0,1.0)                ! size of view surface in inches
      call pgvport(0.0,1.0,0.0,1.0)        ! set viewport
      call pgwindow(0.0,float(xsize),0.0,float(ysize))
      call pgask(.false.)
*
*  Check there are enough colours for display
*
      call pgqcol(low,high)
      if (high - low .lt. 99) then
         print *,'I need at least 99 colours to be able to run'
         print *,'There are only ',high - low,' free.'
         status = 0
         goto 500
      endif
      white = high
      black = high - 1
      grey = high - 2
      ir = 16
      uv = 96
      ov_green = 3                        ! standard pgplot colour

500   continue
      end



      subroutine setup_colourtable(xsize,ysize)
*
*  Set up a rainbow colour table between lut values IR and UV.  Also
*  set up a grey lut value
*
*  Modification History:
*    01-OCT-1996, ACC:
*    Subroutine created in order to place graphics window creation after
*    all screen prompts, rather than before.
*
      implicit none
*
*  Given:
*    Size of the whole display device in pixels
      integer xsize, ysize
*
*  Returned:
*    None
*
*  Local:
      integer i
      real wlut(3)
*
*  Global:
      include 'echwind_par'
      include 'echwind_mmpix'

      call irain(IR, UV, GREY)
      do i=1,3
           wlut(i)=1.0               ! define a white level
      enddo
      call PGSCR(WHITE, wlut(1), wlut(2), wlut(3))
      do i=1,3
           wlut(i)=0.0               ! define a black level
      enddo
      call PGSCR(BLACK, wlut(1), wlut(2), wlut(3))

      pixx0=xsize*0.01
      pixy0=pixx0
      npixx=min(xsize, ysize)*0.70-pixx0
      npixy=npixx

      end



      subroutine write_instrument(xsize,ysize,tx,ty,curinst,leninst)
*
* Write name of instrument
*
*  Modification History:
*    01-OCT-1996, ACC:
*    Subroutine created in order to place graphics window creation after
*    all screen prompts, rather than before.
*
      implicit none
*
*  Given:
*    Size of the whole display device in pixels
      integer xsize, ysize
*
*  Returned:
      integer tx,ty
*    Name of instrument
      character*16 curinst
      integer leninst
*
*  Local:
      character*80 buff
      integer lbuff
*
*  Global:
      include 'ech_common'
      include 'echwind_par'
      include 'echwind_mmpix'

      call str_trim(curinst, ech_instrument, leninst)
      tx = pixx0
      ty = pixy0 + npixy+0.25*ysize
      write(buff,'(a)')curinst
      call pgscf(2)
      call pgsch(1.5)
      call plot_text(buff, tx, ty, WHITE)
      call pgscf(1)
      call pgsch(1.0)
      tx=pixx0+0.2*xsize
      ty=pixy0+npixy+0.25*ysize
      write(buff, '(''Echelle:       '',f5.1,'' lines/mm'')') ech_d
      call str_trim(buff, buff, lbuff)
      call plot_text(buff, tx, ty, WHITE)

      end


      subroutine det_names
*
*  Prompt for the names of files containing supported detectors and their
*  sizes. Read the files and store the information. Set an appropriate
*  prompt string
*
      implicit none
      include 'SAE_PAR'
      include 'echwind_dets'

      integer status, ios, lun, i, l, str_len
      character*80 detfile, string,echdir,fname
      external str_len

      ndets = 0
      det_list = ' '
      det_listlen = 0

      lun = 27
      status=sai__ok
      do i = 1,NDETFILES
         call par_get0c(detparams(i), detfile, status)
         if (detfile .ne. ' ') then
            if (i .ne. 1) then
               call getenv("ECHWIND_HOME",echdir)
               fname = echdir(:str_len(echdir))
     :                 //detfile(:str_len(detfile))
            else
               fname = detfile(:str_len(detfile))
            endif
            if(status.ne.sai__ok)then
               write(*,*) 'Error getting parameter ',detparams(i)
            elseif(fname .ne. ' ')then
               open(unit=lun, file=fname(:str_len(fname)), status='old',
     :              iostat=ios)
               if(ios.ne.0)then
                  write(*,*) 'Error opening detector file ',
     :                 fname(:str_len(fname))
                  ios=-1
               endif
               do while (ios.eq.0)
                  read(lun , *, iostat=ios) dets(ndets+1),
     :                 det_sizes(1,ndets+1), det_sizes(2,ndets+1)
                  if(ios.eq.0.and.dets(ndets+1).ne.' ')then
                     call str_upcase(dets(ndets+1), dets(ndets+1))
                     l=index(dets(ndets+1), ' ')-1
                     string=' '//dets(ndets+1)(:l)//','
                     if(index(det_list, string(:l+2)).eq.0.and.
     :                    det_listlen+l+2.le.len(det_list))then
                        det_list(det_listlen+1:)=string
                        det_listlen=det_listlen+l+2
                     endif
                     ndets=ndets+1
                  endif
               enddo
               if(ios.gt.0)then
                  write(*,*) 'Error opening detector file ',
     :                 fname(:str_len(fname))
               endif
               close(lun, iostat=ios)
            endif
         endif
      enddo

      end


      subroutine det_size(dsize, status)
*
*   Ask the user for a new detector to use.
*
      implicit none
      include 'SAE_PAR'
      include 'echwind_dets'

      integer status, i, l
      logical found
      real dsize(2)
      character*80 detector

      if(status.ne.sai__ok) RETURN

*
*   Read the detector name
*

      if(det_listlen.ge.3)then
         write(6,6800) 'Detector ('//det_list(2:det_listlen-1)//
     :              ' or xx,yy in mm)? '
 6800    format(a,$)

      endif

      call par_get0c('DETECTOR', detector, status)
      if(status.ne.sai__ok.or.detector.eq.' ')then
         write(*,*) 'Error getting DETECTOR parameter'
*
*  If the name has no comma in it, assume it is the name of a known detector.
*  Look it up in the list of detectors.
*
      elseif(index(detector, ',').eq.0)then
         call str_upcase(detector, detector)
         l=index(detector, ' ')-1
         i=0
         found=.false.
         dowhile(i.lt.ndets.and..not.found)
            i=i+1
            found=detector(:l).eq.dets(i)(:l)
         enddo
         if(.not.found)then
            write(*,*) 'Unknown detector name '//detector
            status=1
         else
            dsize(1)=det_sizes(1,i)
            dsize(2)=det_sizes(2,i)
         endif

*
*   The name contained a comma.  Try to decode the size from the string
*   which should be Xsize,Ysize  in mm
*
      else
         read(detector, *, iostat=status) dsize
         if(status.ne.sai__ok)then
            write(*,*) 'Error getting detector size'
         endif
      endif

      end


      subroutine det_angle(angle, status)
*
*   Get the angle through which the detector is to be rotated or the wavelength
*   at which orders are to be horizontal. If less than 1000, interpret as an
*   angle and apply fudge factor so that if the slit angle offset is used it
*   will give horizontal orders at the wavelength for which the detector was
*   set up, and convert to radians. Otherwise, interpret as a wavelength and
*   calculate the angle
*
      implicit none
      include 'SAE_PAR'
      include 'ech_common'
      include 'echwind_par'

      integer status
      real angle, deltheta
      integer m, mmax, mmin, bestm
      real r, wc, w1, w2, d1, d2, x1, x2, a, bestsofar, bestw
      character*80 buff

      if(status.ne.sai__ok)return
      deltheta = ech_thetab - ech_blaze0

*
*   Read the angle or wavelength
*
      call par_get0r('ANGWAVE', r, status)
      if(status.ne.sai__ok)then
         write(*,*) 'Error getting slit angle offset / wavelength'

*
*   Positive values between 0 and 1000 are assumed to be angles (although
*   they are not very sensible)
*
      elseif(0.0.le.r.and.r.le.1000.0)then
         angle=r/ANGLE_FUDGE/RAD

*
*   Negative values are reasonable and will cause some order to be horizontal.
*   Determine which one
*
      elseif(r.lt.0.0)then
         angle=r/ANGLE_FUDGE/RAD
         call ech_ordernum(WAVEMIN, ech_theta0, ech_gamma0, mmax,
     :                     status)
         call ech_ordernum(WAVEMAX, ech_theta0, ech_gamma0, mmin,
     :                     status)
         bestsofar=1.0e30
         do m=mmin, mmax
            call ech_wcentral(m, deltheta, ech_gamma0, wc, status)
            w1=wc-1.0
            w2=wc+1.0
            call ech_disp(w1, m, deltheta, ech_gamma0, d1, status)
            call ech_disp(w2, m, deltheta, ech_gamma0, d2, status)
            call ech_xdispfs(w1, ech_gamma0, x1, status)
            call ech_xdispfs(w2, ech_gamma0, x2, status)
            a=-atan((x2-x1)/(d2-d1))
            if(abs(a-angle).lt.bestsofar)then
               bestsofar=abs(a-angle)
               bestw=wc
               bestm=m
            endif
         enddo
         write(buff, '(''Angle corr to wavelength nearest '//
     :     'centre of'', f9.2, '' Angstroms in Order'', i4)')
     :                                           bestw, bestm
         write(*,*) buff

*
*   Positive values greater than 1000 are interpreted as wavelengths and the
*   corresponding angle is calculated
*
      else
         wc=r
         call ech_findorder(wc, m, status)
         w1=wc-1.0
         w2=wc+1.0
         call ech_disp(w1, m, deltheta, ech_gamma0, d1, status)
         call ech_disp(w2, m, deltheta, ech_gamma0, d2, status)
         call ech_xdispfs(w1, ech_gamma0, x1, status)
         call ech_xdispfs(w2, ech_gamma0, x2, status)
         angle=-atan((x2-x1)/(d2-d1))
         write(buff, '(f5.2)')angle*ANGLE_FUDGE*RAD
         write(*,*) 'Corresponds to a slit angle offset of ',buff(:5),
     :              ' degrees'
      endif

      end


      subroutine wind_cent(echxc, echyc, status)
*
*   Get the initial window centre
*
      implicit none
      include 'SAE_PAR'
      include 'echwind_par'
      include 'ech_common'

      integer status
      real echxc, echyc
      integer mc
      real slit_angle, prism_pos, theta, gamma, wc, deltheta
      character*80 colour
      logical echvals

      if(status.ne.sai__ok)return
      deltheta = ech_thetab - ech_blaze0

      call par_get0r('WAVELENGTH', wc, status)
      if(status.ne.sai__ok)then
         write(*,*) 'Error getting initial wavelength; centre '//
     :              'of echellogram assumed'
         theta = deltheta
         gamma = ech_gamma0
         echvals = .false.
      elseif(wc.le.0.0)then
         theta = deltheta
         gamma = ech_gamma0
         echvals = .false.
      elseif (wc .lt. WAVEMIN .or. wc .gt. WAVEMAX) then
         write(*,*) 'Wavelength must be in range ', WAVEMIN,
     :                ' to ', WAVEMAX
      else
         call ech_findorder(wc, mc, status)
         call ech_disp(wc, mc, deltheta, ech_gamma0, echxc, status)
         call ech_xdispfs(wc, ech_gamma0, echyc, status)
         call how_do_i_get_there(echxc, echyc, theta, gamma,
     :                           colour, prism_pos, slit_angle)
         echvals = .true.
      endif
*
*   and nearest wavelength to centre
*
      if (status .eq. sai__ok) then
         call ech_wavecen(theta, gamma, wc, mc, status)
         if (.not. echvals) then
            call ech_disp(wc, mc, deltheta, ech_gamma0, echxc, status)
            call ech_xdispfs(wc, ech_gamma0, echyc, status)
         endif
      endif

      end


      subroutine save_text
*
*   Save details of current detector position in human readable text file
*
      implicit none
      include 'SAE_PAR'
      include 'ech_common'
      include 'echwind_par'
      include 'echwind_orders'

      integer status, lun, ios,str_len
      integer cen_mc, max_mc, min_mc
      logical created
      logical on_echellogram
      real cen_slit_angle, cen_prism_pos, cen_theta, cen_gamma
      real cen_wc, max_wc, min_wc, range
      real del, xdel, ddel, dxdel, deltheta
      real detx1, dety1, detx2, dety2
      real echxc, echyc
      character*80 file, buff, colour
      data created/.false./
      save created, file
      external str_len

      status = sai__ok
      deltheta = ech_thetab - ech_blaze0
*
*  If there's room, read the current detector position (in mm) and
*  mark it on the display
*
      call det_mm(detx1, dety1, detx2, dety2)
      if(nboxes.lt.MAX_BOXES)then
         nboxes=nboxes+1
         boxes(1, nboxes)=detx1
         boxes(2, nboxes)=dety1
         boxes(3, nboxes)=detx2
         boxes(4, nboxes)=dety2
         call draw_box(boxes(1, nboxes))
      endif
*
*  Calculate the current detector centre position
*
      call calculate_config(
     :   cen_wc, cen_mc, max_wc, max_mc, min_wc, min_mc,
     :   cen_theta, cen_gamma, cen_prism_pos, cen_slit_angle,
     :   range, on_echellogram)
*
*   Tell the user
*
      if (on_echellogram) then
         write(buff, '(''Wavelength nearest centre is'', f9.2,'//
     :      ''' Angstroms in Order'', i4)') cen_wc, cen_mc
         write(*,*) buff
*
*   Determine dispersion and cross-dispersion at field centre
*   (Note: I can't see why the dispersions and offsets are computed.
*    They don't seem to be used. 30-SEP-1996, ACC)
*
*        call ech_disp(cen_wc, cen_mc, deltheta, ech_gamma0, del,
*    :                 status)
*        call ech_xdispfs(cen_wc, ech_gamma0, xdel, status)
*
*   and offsets
*
*        ddel=echxc-del
*        dxdel=echyc-xdel
*
*    Append to / create the text file
*
         lun = 27
         if(.not.created)then
            status=sai__ok
            call par_get0c('TEXTFILE', file, status)
            file = file(1:str_len(file))//'.txt'
            open(unit=lun, file=file, status='new', iostat=ios)
            if(status.ne.sai__ok.or.ios.ne.0)then
               write(*,*) 'Error creating text file ',
     :                     file(:str_len(file))
            else
               write(*,*) 'Created text file ',file(:str_len(file))
               created=.true.
               write(lun,*) 'ECHWIND V3.0'
            endif
         else
            file = file(1:str_len(file))
            open(unit=lun, file=file, status='old',
#if HAVE_FC_OPEN_POSITIONAPPEND
     :           access='sequential',position='append',
#elif HAVE_FC_OPEN_ACCESSAPPEND
     :           access='append',
#else
      Do not know how to APPEND to a file
#endif
     :           iostat=ios)
            if(ios.ne.0)then
               write(*,*) 'Error re-opening text file ',
     :                     file(:str_len(file))
            endif
         endif
*
*   If OK, write details
*
         if(ios.eq.0)then
            write(lun, *) ' '
            write(lun, *) '  Instrument: ', ech_instrument
            write(lun, *) '  Echelle: ', ech_echelle
            write(lun, *) '  Camera: ', ech_camera
            write(lun, *) '  No. of prisms: ', ech_npr
            write(lun, *) '  Detector length: ', range
            write(lun, *) '  Maximum wavelength: ', max_wc,
     :                    '  in order no: ', max_mc
            write(lun, *) '  Minimum wavelength: ', min_wc,
     :                    '  in order no: ', min_mc
            write(lun, *) '  Central Wavelength: ', cen_wc,
     :                    '  in order no: ', cen_mc
            write(lun, *) '       Theta        Gamma'
     :                  //'    prism (mm)   slit angle'
            write(lun, '(f12.3, f13.3, f12.2, f13.3)')
     :            cen_theta*rad, cen_gamma*rad, cen_prism_pos,
     :            cen_slit_angle*rad
         endif
*
*    Close the file
*
         close(lun, iostat=ios)

      else

         write(*,*) '*** Detector is not on echellogram'

      endif

      end


      subroutine echtodet(echx, echy, detx, dety)
*
*   Convert a position measured in (disp,xdisp) mm on the echellogram to a
*   position measured in (x,y) mm on the detector. This is simply a rotation
*   about the origin by the detector rotation angle.
*
      implicit none
      include 'echwind_mmpix'

      real echx, echy, detx, dety

      detx = echx*cos(angle) - echy*sin(angle)
      dety = echx*sin(angle) + echy*cos(angle)
      end



      subroutine dettoech(detx, dety, echx, echy)
*
*   Convert a position measured in (x,y) mm on the detector to a position
*   measured in (disp,xdisp) mm on the echellogram. This is simply a rotation
*   about the origin by minus the detector rotation angle.
*
      implicit none
      include 'echwind_mmpix'

      real detx, dety, echx, echy

      echx = detx*cos(-angle) - dety*sin(-angle)
      echy = detx*sin(-angle) + dety*cos(-angle)
      end


      subroutine dettopix(detx, dety, pixx, pixy)
*
*   Convert a position measured in (x,y) mm on the detector to a screen
*   position in pixels.
*
      implicit none
      include 'echwind_mmpix'

      real detx, dety
      integer pixx, pixy

      pixx = nint(pixx0 + (detx-winx0)/swinx*npixx)
      pixy = nint(pixy0 + (dety-winy0)/swiny*npixy)
      end



      subroutine pixtodet(pixx, pixy, detx, dety)
*
*   Convert a screen position in pixels to a position measured in (x,y) mm on
*   the detector.
*
      implicit none
      include 'echwind_mmpix'

      integer pixx, pixy
      real detx, dety

      detx = float(pixx-pixx0)/npixx*swinx + winx0
      dety = float(pixy-pixy0)/npixy*swiny + winy0
      end



      subroutine echtopix(echx, echy, pixx, pixy)
*
*   Convert a position measured in (disp,xdisp) mm on the echellogram to a
*   screen position in pixels.
*
      implicit none
      real echx, echy, detx, dety
      integer pixx, pixy

      call echtodet(echx, echy, detx, dety)
      call dettopix(detx, dety, pixx, pixy)
      end



      subroutine pixtoech(pixx, pixy, echx, echy)
*
*   Convert a screen position in pixels to a position measured in (disp,xdisp)
*   mm on the echellogram.
*
      implicit none
      integer pixx, pixy
      real echx, echy, detx, dety

      call pixtodet(pixx, pixy, detx, dety)
      call dettoech(detx, dety, echx, echy)
      end


      subroutine draw_display(dsize, xsize, ysize)
      implicit none
      include 'echwind_par'

      integer xsize, ysize
      real dsize(2)

      call draw_orders(dsize, xsize, ysize)    ! the orders
      call draw_boxes                          ! the boxes
      call draw_lines                          ! the line list
      call plot_drawdet(WHITE)                 ! the detector
      call log_position(xsize, ysize)          ! wavelength info
      end

      subroutine draw_orders(dsize, xsize, ysize)
*
*   Work out and store the end coordinates of all the echelle orders,
*   Then draw them in the appropriate colour.
*
      implicit none
      include 'SAE_PAR'
      include 'ech_common'
      include 'echwind_par'
      include 'echwind_orders'
      include 'echwind_mmpix'

      real dsize(2)
      integer status

      logical visible
      integer  m, icol, mmin, mmax
      real wc, free, wleft, wright, deltheta

      integer ox1(MAX_ORDER), oy1(MAX_ORDER)   ! order positions in pixels
      integer ox2(MAX_ORDER), oy2(MAX_ORDER)

      integer xpl(MAX_POLY), ypl(MAX_POLY)
      integer xsize, ysize

      status = sai__ok
      deltheta = ech_thetab - ech_blaze0
*
*   Get the range of orders covering the whole echellogram
*
      call ech_ordernum(WAVEMIN, ech_theta0, ech_gamma0, mmax, status)
      call ech_ordernum(WAVEMAX, ech_theta0, ech_gamma0, mmin, status)
*
*   For each order...
*
      do m=mmin, mmax

         call ech_wcentral(m, deltheta, ech_gamma0, wc, status)
         call ech_frspr(m, deltheta, ech_gamma0, free, status)
         wleft=wc-free/2.0
         wright=wc+free/2.0
*
*   amount of dispersion and cross-dispersion of the ends.  i.e the
*   order end coordinates in mm.
*
         call ech_disp(wleft, m, deltheta, ech_gamma0, orderdl(m),
     :                 status)
         call ech_disp(wright, m, deltheta, ech_gamma0, orderdr(m),
     :                 status)
         call ech_xdispfs(wleft, ech_gamma0, orderxdl(m), status)
         call ech_xdispfs(wright, ech_gamma0, orderxdr(m), status)
      enddo
*
*  Clear display window
*
      call pgsci(BLACK)
      call pgrect(0.0, float(xsize-1), 0.0, float(ysize)*0.94)
*
*   draw a box around the displayed part of the echellogram
*
      call plot_box(pixx0, pixx0+npixx, pixy0, pixy0+npixy, WHITE)
*
*   For each order...
*
      do m=mmin, mmax

         call ech_wcentral(m, deltheta, ech_gamma0, wc, status)
         icol=UV-(wc-WAVEMIN)/(WAVEMAX-WAVEMIN)*(UV-IR)
         icol=max(icol, IR)
         icol=min(icol, UV)

*
*   convert end coords. to screen pixels
*
         call echtopix(orderdl(m), orderxdl(m), ox1(m), oy1(m))
         call echtopix(orderdr(m), orderxdr(m), ox2(m), oy2(m))
*
*   Clip the order to fit in the display window
*
         xpl(1)=ox1(m)
         ypl(1)=oy1(m)
         xpl(2)=ox2(m)
         ypl(2)=oy2(m)
         call plot_clip(xpl(1), ypl(1), xpl(2), ypl(2),
     :                  pixx0+1, pixy0+1, pixx0+npixx-1, pixy0+npixy-1,
     :                  visible)
*
*   And plot it if any part is visible
*
         if(visible)then
            call plot_line(xpl, ypl, 2, icol)
         endif
      enddo

*
*   Plot information about the detector size
*
      call det_tx(dsize, xsize, ysize, WHITE)

      call pgupdt

      end



      subroutine read_lines
*
*   Prompt for the name of a file containing line wavelengths and labels.
*   Read the file and store the information.
*
      implicit none
      include 'SAE_PAR'
      include 'echwind_lines'

      integer status, ios, lun, str_len, prefix_pos
      character*80 linelist, echdir
      external str_len

      nlines=0
      lun = 27
      status=sai__ok
      call par_get0c('LINELIST', linelist, status)
      if(linelist.ne.' ')then
*
* If filename is prefixed by $ECHWIND_HOME, then translate it.
* 25-Sep-1996, ACC
*
         prefix_pos = index(linelist,'${ECHWIND_HOME}')
         if (prefix_pos.ne.0) then
            call getenv("ECHWIND_HOME",echdir)
            linelist = echdir(:str_len(echdir))
     :              //linelist(prefix_pos+len('${ECHWIND_HOME}')
     :              :str_len(linelist))
         endif

         write(*,*) 'linelist file =',
     $                  linelist(:str_len(linelist))
         open(unit=lun, file=linelist(:str_len(linelist)),
     $        status='old', iostat=ios)
         if(ios.ne.0)then
            write(*,*) 'Error opening linelist file ',
     $                  linelist(:str_len(linelist))
            ios=-1
         endif
         do while (ios.eq.0)
            read(lun , *, iostat=ios) lines(nlines+1),
     :                                line_labels(nlines+1)
            if(ios.eq.0) nlines=nlines+1
         enddo
         if(ios.gt.0)then
            write(*,*) 'Error reading linelist file ',
     $                  linelist(:str_len(linelist))
         endif
         close(lun, iostat=ios)
      endif

      end



      subroutine draw_lines
*
*   Plot and label all the lines in the line list
*
      implicit none
      include 'SAE_PAR'
      include 'ech_common'
      include 'echwind_par'
      include 'echwind_lines'
      include 'echwind_mmpix'

      integer status

      integer n, m, mcen, idx, idy, lenlabel, x(2), y(2)
      real dx, dy, deltheta

      status=sai__ok
      deltheta = ech_thetab - ech_blaze0

      do n=1, nlines
         call ech_findorder(lines(n), mcen, status)
*
*  Plot in the main order, and both adjacent orders
*
         do m=mcen-1, mcen+1

            call ech_disp(lines(n), m, deltheta, ech_gamma0, dx,
     :                    status)
            call ech_xdispfs(lines(n), ech_gamma0, dy, status)
            call echtopix(dx, dy, idx, idy)
*
*  only try to plot if it's within the display window
*
            if(idx.ge.pixx0.and.idx.le.pixx0+npixx .and.
     :         idy.ge.pixy0.and.idy.le.pixy0+npixy)then
               x(1) = idx
               y(1) = idy-3
               x(2) = idx
               y(2) = idy+3
            if(m.eq.mcen)then
                  call plot_line(x, y, 2, WHITE)
               else
                  call plot_line(x, y, 2, GREY)
               endif
               call str_trim(line_labels(n), line_labels(n), lenlabel)
               if (m.eq.mcen)then
                  call plot_text(line_labels(n),idx+2, idy, WHITE)
               else
                  call plot_text(line_labels(n),idx+2, idy, GREY)
               endif
            endif
         enddo
      enddo

      call pgupdt

      end



      subroutine get_action(action, factor, xsize, ysize)
*
*   Get the name of an action to be carried out, and an associated
*   real number (which is usually the factor to ZOOM).  Do this by
*   putting up a menu on the display from which the user selects an
*   item with the cursor
*
      implicit none
      include 'echwind_par'
      include 'echwind_mmpix'

      character*(*) action
      real factor
      integer nmenu, item, i, cx, cy
      integer xsize, ysize

      integer MAX_MENU          ! Max. no of items in menu
      parameter (MAX_MENU = 20)

      real MENU_STARTX, MENU_STARTY, MENU_SPACING
      parameter (MENU_STARTX = 0.03)  ! menu X start as a fraction of the
                                      ! screen size, BEYOND the display window
      parameter (MENU_STARTY = 0.9)   ! menu Y start position as a fraction
                                      ! of the screen size from the bottom
      parameter (MENU_SPACING = 0.045) ! spacing between items (frac. of screen)

      real TOLX, TOLY                 ! tolerance accepted when selecting item
      parameter (TOLX = 0.5, TOLY = 0.03)  ! (fraction of screen size)

      character*80 menu_items(MAX_MENU) ! the menu items
      integer litems(MAX_MENU)          ! their lengths

      character*80 actions(MAX_MENU)    ! associated actions to be performed
      real factors(MAX_MENU)            ! associated factors

      integer mx, my(MAX_MENU) ! menu item coords. in pixels
      real cur_x, cur_y
      character ch
      logical gotone, firstone, first_time
*
*   The complete list of menu items and associated information.  The list
*   must end with the special item *END*
*
      data (menu_items(i), actions(i), factors(i), i=1, MAX_MENU) /
     :    'Clear BOXES',   'CLEARBOXES',    1.0,
     :    'Delete BOX',    'DELETEBOX',     1.0,
     :    'Mark BOX',      'BOX',           1.0,
     :    'Mark LINES',    'LINES',         1.0,
     :    'New ECHELLE',   'ECHELLE',       1.0,
     :    'New DETECTOR',  'DETECTOR',      1.0,
     :    'New ANGLE',     'ANGLE',         1.0,
     :    'New WAVELENGTH','WAVELENGTH',    1.0,
     :    ' ',             ' ',             1.0,
     :    'x 1.3',         'ZOOM',          1.3,
     :    'x 2.0',         'ZOOM',          2.0,
     :    'x 3.0',         'ZOOM',          3.0,
     :    'x 0.77',        'ZOOM',          0.7692301,
     :    'x 0.5',         'ZOOM',          0.5,
     :    'x 0.33',        'ZOOM',          0.3333333,
     :    'CENTRE',        'CENTRE',        1.0,
     :    ' ',             ' ',             1.0,
     :    'SAVE',          'SAVE',          1.0,
     :    'EXIT',          'EXIT',          1.0,
     :    '*END*',         ' ',             1.0/

      data item /19/           ! initially set to EXIT
      save item

      data first_time /.true./
      save first_time

      save nmenu, mx, my, litems
*
*   The first time only (for speed), work out the lengths and screen
*   coordinates of all the menu items.
*
      if(first_time)then
         first_time=.false.
         mx=nint(MENU_STARTX*xsize) + pixx0+npixx
         i=1
         do while(menu_items(i).ne.'*END*'.and.i.lt.MAX_MENU)
            call str_trim(menu_items(i), menu_items(i), litems(i))
            my(i)=nint(MENU_STARTY*ysize) - (i-1)*MENU_SPACING*ysize
            i=i+1
         enddo
         nmenu=i-1
      endif
*
*   Draw the menu in white
*
      do i=1, nmenu
         call plot_text(menu_items(i), mx, my(i), WHITE)
      enddo

      action='CONTINUE'     ! default action is just CONTINUE
      firstone=.true.
*
*   Read the cursor position (and we also get a button press)
*
      cur_x = float(mx)
      cur_y = float(my(item))
      call PGCURSE(cur_x, cur_y, ch)
      cx = nint(cur_x)
      cy = nint(cur_y)

      gotone=.false.
      i=1
*
*  check if it is pointing near a menu item
*
      do while (.not.gotone.and.i.le.nmenu)
         if(cx.ge.mx-TOLX*xsize.and.cx.le.mx+TOLX*xsize .and.
     :         cy.ge.my(i).and.cy.le.my(i)+TOLY*ysize)then
            gotone=.true.
         else
            i=i+1
         endif
      enddo
*
*  if it is, and it's not the item already chosen (except for the
*  first one to be chosen)
*
      if (gotone) then
*         if(i.ne.item.or.firstone)then
*
*  Set the chosen item to green
*
            call plot_text(menu_items(i), mx, my(i), OV_GREEN)
*            firstone=.false.
            item=i                ! remember the chosen item number
            action=actions(item)  ! and its action name
            factor=factors(item)  ! and factor
*         endif
      else
*
*  No item selected, set action to the default 'CONTINUE'.
*
         item=0
         action='CONTINUE'
      endif
*
*   Erase all the menu items
*
      do i=1, nmenu
         call plot_text(menu_items(i), mx, my(i), BLACK)
      enddo
*
*  Return with the action name chosen
*
      end


      subroutine centre_detector(dsize, detxc, detyc, factor)
*
*   Calculate the detector coordinates on the screen for the new
*   magnification factor
*
      implicit none
      include 'echwind_par'
      include 'echwind_mmpix'

      real dsize(2)
      real factor
      integer status
      integer pixx1, pixy1, pixx2, pixy2
      real detx1, dety1, detx2, dety2, detxc, detyc
*
*   Calculate new position and size of window onto echellogram
*
      swinx=swinx/factor
      swiny=swiny/factor
      winx0=detxc-swinx/2.0
      winy0=detyc-swiny/2.0
*
*   new detector coordinates
*
      detx1=detxc-dsize(1)/2.0
      dety1=detyc-dsize(2)/2.0
      detx2=detxc+dsize(1)/2.0
      dety2=detyc+dsize(2)/2.0
*
*   Set the appropriate coordinates (in pixels) for the new detector
*
      call dettopix(detx1, dety1, pixx1, pixy1)
      call dettopix(detx2, dety2, pixx2, pixy2)
      call plot_wrdet(pixx1, pixx2, pixy1, pixy2, status)
*
*   If this fails, the detector must now be too big to fit on the display.
*   We don't allow this, so revert to the old size (ie don't ZOOM)
*
      if(status.ne.0)then
         swinx=swinx*factor
         swiny=swiny*factor
         winx0=detxc-swinx/2.0
         winy0=detyc-swiny/2.0
      endif

      end



      subroutine det_mm(detx1, dety1, detx2, dety2)
*
*   Read the detector coordinates in mm
*
      implicit none
      include 'echwind_par'

      integer wx1, wy1, wx2, wy2
      real detx1, dety1, detx2, dety2

*
*   First get the detector window position in screen pixels
*
      call plot_rddet(wx1, wx2, wy1, wy2)
*
*   and convert to mm
*
      call pixtodet(wx1, wy1, detx1, dety1)
      call pixtodet(wx2, wy2, detx2, dety2)

      end



      subroutine draw_boxes
*
*   Draw boxes at all the stored detector positions
*
      implicit none
      include 'echwind_par'
      include 'echwind_orders'

      integer i

      do i=1, nboxes
         call draw_box(boxes(1, i))     ! and draw them all
      enddo

      end



      subroutine draw_box(corners_mm)
*
*   draw a single detector box at the specified coordinates in mm
*
      implicit none
      include 'echwind_par'
      include 'echwind_mmpix'

      real corners_mm(4)
      integer corners(4), i, status

*
*    convert the coordinates to screen pixels
*
      do i=1, 3, 2
         call dettopix(corners_mm(i), corners_mm(i+1),
     :                 corners(i),    corners(i+1))
      enddo

*
*   Draw each line of the box in green, after clipping it to the display window.
*
      call draw_clipped_line(corners(1), corners(2), corners(3),
     :     corners(2), pixx0+1, pixy0+1, pixx0+npixx-1,
     :     pixy0+npixy-1, OV_GREEN, status)

      call draw_clipped_line(
     :     corners(3), corners(2), corners(3), corners(4),
     :     pixx0+1, pixy0+1, pixx0+npixx-1, pixy0+npixy-1,
     :     OV_GREEN, status)

      call draw_clipped_line(
     :     corners(3), corners(4), corners(1), corners(4),
     :     pixx0+1, pixy0+1, pixx0+npixx-1, pixy0+npixy-1,
     :     OV_GREEN, status)

      call draw_clipped_line(
     :     corners(1), corners(4), corners(1), corners(2),
     :     pixx0+1, pixy0+1, pixx0+npixx-1, pixy0+npixy-1,
     :     OV_GREEN, status)


      end


      subroutine draw_clipped_line(x1, y1, x2, y2, cx1, cy1, cx2, cy2,
     :                             colour, status)
*
*   Draw a line from (x1, y1) to (x2, y2), clipped to fit in window
*   (cx1, cy1) (cx2, cy2), in the specified colour.
*
      implicit none
      integer x1, y1, x2, y2, colour, status
      integer cx1, cy1, cx2, cy2
      integer xpl(2), ypl(2)
      logical visible

      xpl(1)=x1
      ypl(1)=y1
      xpl(2)=x2
      ypl(2)=y2
      call plot_clip(xpl(1), ypl(1), xpl(2), ypl(2),
     :               cx1, cy1, cx2, cy2, visible)

      if(visible)then
         call plot_line(xpl, ypl, 2, colour)
      endif

      end


      subroutine where_am_i(opt, echx, echy, w, bestm)
*
*   Find out the nearest wavelength and order number corresponding to a given
*   detector window position. The option argument specifies whether the
*   position is in the CENtre of the detector, at the point of MINimum
*   wavelength or at the point of MAXimum wavelength.
*
      implicit none
      include 'SAE_PAR'
      include 'ech_common'

      character*(*) opt
      real echx, echy, w
      integer bestm

      real ERRDX, ERRDY        ! acceptable errors (0.01mm = 10 micron)
      parameter (ERRDX = 0.010, ERRDY = 0.010)

      real dx, dy, olddx, olddy, delw, deltheta
      real bestsofar, free
      integer status, m, mmin, mmax

      status=sai__ok
      deltheta = ech_thetab - ech_blaze0
*
*   Iterate to find the wavelength that is cross-dispersed to the
*   exact Y-value of the detector centre
*
      w=ech_wave0
      delw=1000.0
      dy=echy+ERRDY*2.0
      do while (abs(dy-echy).gt.ERRDY)
         olddy=dy
         call ech_xdispfs(w, ech_gamma0, dy, status)
         if(abs(dy-echy).gt.ERRDY)then
            if(dy.gt.echy)then
               w=w-delw
            else
               w=w+delw
            endif
         endif
         if(sign(1.0, dy-echy) .ne. sign(1.0, olddy-echy))then
            delw=delw/2.0
         endif
      enddo

*
*   Find out which order this wavelength is being observed in.  It may not
*   be the obvious order so a single call to ordernum is not enough.  Instead
*   we try a few orders either side and check which one gives the nearest to
*   the right amount of dispersion for this wavelength
*
      bestsofar=1.0e30
      call ech_ordernum(w, ech_theta0, ech_gamma0, m, status)
      mmin=m-2
      mmax=m+2
      do m=mmin, mmax
         call ech_disp(w, m, deltheta, ech_gamma0, dx, status)
         if(abs(dx-echx).lt.bestsofar)then
            bestsofar=abs(dx-echx)
            bestm=m
         endif
      enddo

*
*   Iterate to find the wavelength that is dispersed to the exact Y-value of
*   the detector centre. Start with the wavelength that has the correct
*   cross-dispersion
*
      delw=20.0
      dx=echx+ERRDX*2.0
      do while (abs(dx-echx).gt.ERRDX)
         olddx=dx
         call ech_disp(w, bestm, deltheta, ech_gamma0, dx, status)
         if(abs(dx-echx).gt.ERRDX)then
            if(dx.gt.echx)then
               w=w-delw
            else
               w=w+delw
            endif
         endif
         if(sign(1.0, dx-echx) .ne. sign(1.0, olddx-echx))then
            delw=delw/2.0
         endif
      enddo

*
*   If this is the point of MINimum or MAXimum wavelength, determine the
*   cross-dispersion at this point of exactly matching dispersion and adjust
*   the order number so that the wavelength lies on the side of the detector
*   rather than outside it (this assumes that the detector rotation is not
*   excessively large)
*
      if(opt.eq.'MIN'.or.opt.eq.'MAX')then
         call ech_xdispfs(w, ech_gamma0, dy, status)
         call ech_frspr(bestm, deltheta, ech_gamma0, free, status)
         if(opt.eq.'MIN'.and.dy.lt.echy)then
            w=w+free
            bestm=bestm-1
         elseif(opt.eq.'MAX'.and.dy.gt.echy)then
            w=w-free
            bestm=bestm+1
         endif
      endif
      end


      subroutine how_do_i_get_there(echxc, echyc, theta, gamma, col,
     :                                                       pp, sa)
*
*   Find out the exact echelle angles corresponding to the current
*   detector window position.
*
      implicit none
      include 'SAE_PAR'
      include 'ech_common'

      real echxc, echyc, theta, gamma, pp, sa
      character*(*) col

      real w, w1, w2
      real dx1, dx2, dy1, dy2
      real th1, gam1, th2, gam2
      real pp1, pp2, sa1, sa2
      real free, deltheta
      integer status, bestm, m1, m2

      status=sai__ok
      deltheta = ech_thetab - ech_blaze0
*
*   Calculate the wavelength and order number nearest the detector centre.
*
      call where_am_i('CEN', echxc, echyc, w, bestm)
*
*   Now we will take two wavelengths in adjacent orders near this
*   position.  Work out Theta and Gamma for these wavelengths, calculate
*   the amount of dispersion and cross-dispersion for these wavelengths,
*   use the known detector centre position to interpolate between the
*   positions of these other wavelengths to get the exact values of
*   Theta and Gamma for the detector centre, and Robert is your mother's
*   brother.
*
      w1=w+1.0        ! detector centre wavelength plus 1 Angstrom
      m1=bestm        ! the nearest order
      m2=bestm+1      ! the next one

      call ech_frspr(bestm, deltheta, ech_gamma0, free, status)
      w2=w-free       ! the wavelength directly below the detector centre
                      ! in the next order

*
*   Instrument configuration for these two wavelengths
*
      call ech_form_cent(w1, m1, th1, gam1, col, pp1, sa1, status)
      call ech_form_cent(w2, m2, th2, gam2, col, pp2, sa2, status)
*
*   Position in echellogram of these two wavelengths
*
      call ech_disp(w1, m1, deltheta, ech_gamma0, dx1, status)
      call ech_disp(w2, m2, deltheta, ech_gamma0, dx2, status)
      call ech_xdispfs(w1, ech_gamma0, dy1, status)
      call ech_xdispfs(w2, ech_gamma0, dy2, status)
*
*   Get Theta, Gamma, Prism position, and slit angle by interpolation,
*   (since we know the detector centre position in the echellogram)
*
      call interp(dx1, th1, dx2, th2, echxc, theta)
      call interp(dy1, gam1, dy2, gam2, echyc, gamma)
      call interp(gam1, pp1, gam2, pp2, gamma, pp)
      call interp(gam1, sa1, gam2, sa2, gamma, sa)
*
*   Find out the correct collimator using the known value of Gamma
*
      call ech_collim(-1.0, gamma, col, status)

      end


      subroutine interp(x1, y1, x2, y2, x, y)
*
*   Given two coordinates and an other X value, calculate the corresponding
*   Y value by linear interpolation
*
      implicit none
      real x1, y1, x2, y2, x, y

      if (x1 .eq. x2) then
         y = y1
      else
         y = y1 + (y2-y1)/(x2-x1) * (x-x1)
      endif

      end


      subroutine det_tx(dsize, xsize, ysize, colour)
*
*    Write the size of the detector on to the display in the
*    requested colour
*
      implicit none
      include 'echwind_par'
      include 'echwind_mmpix'

      real dsize(2)
      integer colour
      character*80 buff
      integer lbuff, xsize, ysize, tx, ty

      tx=pixx0
      ty=pixy0+npixy+0.21*ysize
      write(buff, '(''Detector size:     '',f5.1,'' x'',f5.1,'//
     :     ''' mm'')') dsize
      call str_trim(buff, buff, lbuff)
      call plot_text(buff, tx, ty, colour)

      end


      subroutine log_position(xsize, ysize)
*
*   Outputs the current detector center position (wavelength and
*   order) on the display screen.
*
*   Modified:
*     30-SEP-1996, ACC:
*     Extracted code from here to create new subroutine calculate_config,
*     which is now called from here and from save_text.
*     Features uncovered:
*       xsize is not used in log_positon
*
      implicit none
      include 'SAE_PAR'
      include 'echwind_par'
      include 'echwind_mmpix'

      character*80 maxbuff, cenbuff, ranbuff, minbuff
      character*80 oldmaxbuff, oldcenbuff, oldranbuff, oldminbuff
      integer cen_mc, max_mc, min_mc
      integer xsize, ysize, tx, ty, status
      integer lmax, lcen, lran, lmin
      logical first, on_echellogram
      real cen_wc, max_wc, min_wc
      real range
      real cen_theta, cen_gamma, cen_prism_pos, cen_slit_angle

      save first
      save oldmaxbuff, oldcenbuff, oldranbuff, oldminbuff
      save lmin, lcen, lran, lmax
      data first/ .true./

      status=sai__ok
*
*  Determine current detector position in mm and
*  calculate the spectrograph configuration to put this
*  wavelength at the centre of the detector (and also estimate the width
*  of the detector in Angstroms).
*
      call calculate_config(
     :   cen_wc, cen_mc, max_wc, max_mc, min_wc, min_mc,
     :   cen_theta, cen_gamma, cen_prism_pos, cen_slit_angle,
     :   range, on_echellogram)
      if (on_echellogram) then
         write(cenbuff, '(''Central wavelength:'', i5,'//
     :                ''' A in order '', i3)') nint(cen_wc), cen_mc
         write(ranbuff,'(''Detector length:   '', f5.1,'' A'')')range
         write(maxbuff, '(''Maximum wavelength:'', I5,'//
     :                ''' A in order '', i3)') nint(max_wc), max_mc
         write(minbuff, '(''Minimum wavelength:'', i5,'//
     :                ''' A in order '', i3)') nint(min_wc), min_mc
      else
         write(ranbuff,'(''***'')')
         write(maxbuff,'(''***'')')
         write(minbuff,'(''***'')')
         write(cenbuff,'(''*** Detector is not on echellogram'')')
         call plot_drawdet(BLACK)
      endif
*
*  Output to the display device
*
      tx=pixx0
      ty=pixy0+npixy+0.17*ysize
      if (.not.first) then
         call plot_text(oldranbuff, tx, ty, BLACK)
      end if
      call str_trim(ranbuff, ranbuff, lran)
      call plot_text(ranbuff, tx, ty, WHITE)
      ty=ty-0.04*ysize
      if (.not.first) then
         call plot_text(oldmaxbuff, tx, ty, BLACK)
      end if
      call str_trim(maxbuff, maxbuff, lmax)
      call plot_text(maxbuff, tx, ty, WHITE)
      ty=ty-0.04*ysize
      if (.not.first) then
         call plot_text(oldcenbuff, tx, ty, BLACK)
      end if
      call str_trim(cenbuff, cenbuff, lcen)
      call plot_text(cenbuff, tx, ty, WHITE)
      ty=ty-0.04*ysize
      if (.not.first) then
         call plot_text(oldminbuff, tx, ty, BLACK)
      end if
      call str_trim(minbuff, minbuff, lmin)
      call plot_text(minbuff, tx, ty, WHITE)
*
*       Update saved values
*
      oldminbuff=minbuff
      oldcenbuff=cenbuff
      oldranbuff=ranbuff
      oldmaxbuff=maxbuff
      first=.false.

      end


      subroutine calculate_config(
     :   cen_wc, cen_mc, max_wc, max_mc, min_wc, min_mc,
     :   cen_theta, cen_gamma, cen_prism_pos, cen_slit_angle,
     :   range, on_echellogram)
*
*   Determine and calculate the current detector center position
*   (wavelength and order).
*
      implicit none
*
* Returned:
*   Central, maximum, and minumum order
      integer cen_mc, max_mc, min_mc
*   Central, maximum, and minumum wavelength
      real    cen_wc, max_wc, min_wc
*   Other central position parameters
      real cen_theta, cen_gamma, cen_prism_pos, cen_slit_angle
*   Detector length (length of one order in Angstroms, at center of
*                    detector)
      real    range
*   Whether detector is on the echellogram
      logical on_echellogram

      include 'SAE_PAR'
      include 'echwind_par'
      include 'echwind_mmpix'

      character*80 colour
      integer status
      real detxc, detyc, detx1, dety1, detx2, dety2
      real echxc, echyc, echx1, echy1, echx2, echy2
      real dx1, dx2

      status=sai__ok
*
*  Determine current detector position in mm
*
      call det_mm(detx1, dety1, detx2, dety2)
      if (detx2.gt.winx0.and.detx1.lt.winx0+swinx.and.
     &    dety2.gt.winy0.and.dety1.lt.winy0+swiny) then
         on_echellogram = .true.
         detxc = (detx1+detx2)/2.0
         detyc = (dety1+dety2)/2.0
         call dettoech(detxc, detyc, echxc, echyc)
*
*  Calculate the spectrograph configuration to put this
*  wavelength at the centre of the detector (and also estimate the width
*  of the detector in Angstroms).
*
         call how_do_i_get_there(echxc, echyc, cen_theta, cen_gamma,
     :      colour, cen_prism_pos, cen_slit_angle)
         call ech_wavecen(cen_theta, cen_gamma, cen_wc, cen_mc, status)

         call ech_disp(cen_wc, cen_mc, cen_theta, cen_gamma, dx1,
     :      status)
         call ech_disp(cen_wc+1.0, cen_mc, cen_theta, cen_gamma, dx2,
     :      status)
         range=(detx2-detx1)/(dx2-dx1)
*
*  Calculate the wavelength at the right of the order corresponding to
*  the highest wavelength
*
         call dettoech(detx2, dety2, echx2, echy2)
         call where_am_i('MAX', echx2, echy2, max_wc, max_mc)
*
*  and at the left of the order corresponding to the lowest wavelength.
*
         call dettoech(detx1, dety1, echx1, echy1)
         call where_am_i('MIN', echx1, echy1, min_wc, min_mc)

      else

         on_echellogram = .false.

      endif

      end
