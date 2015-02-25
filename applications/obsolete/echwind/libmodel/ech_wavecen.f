*+  ECH_WAVECEN  -  Determine the central wavelength of the order
*                   nearest to the centre of the field at a specified
*                   configuration

      subroutine ech_wavecen (theta, gamma, wc, mc, status)
*
*   Description :
*
*     Use a binary chop search to determine the order which corresponds most
*     closely to the given echelle gamma angle at the centre of the free
*     spectral range. This may be out by 1 so find the next nearest order as
*     well. Having determined the two nearest orders, perform a similar search
*     on both orders to determine the wavelength within each order which
*     corresponds to the given echelle theta angle. Choose the wavelength and
*     order number that are closest to the input gamma.
*
*     ECH_INIT must be called before calling this routine.
*
*   Invocation :
*
*     CALL ECH_WAVECEN (THETA, GAMMA, WC, MC, STATUS)
*
*   Arguments :
*
*     THETA   =  REAL (READ)           Echelle theta angle corresponding to the
*                                      current configuration (radians)
*     GAMMA   =  REAL (READ)           Echelle gamma angle corresponding to the
*                                      current configuration (radians)
*     WC      =  REAL (WRITE)          Central wavelength corresponding to the
*                                      current configuration (ie that wavelength
*                                      which lies within order M and is closest
*                                      to that in the centre of the field)
*     MC      =  INTEGER (WRITE)       Order number corresponding to the current
*                                      configuration (the order closest to that
*                                      in the centre of the field)
*     STATUS  =  INTEGER (READ, WRITE) Global status value
*
*   Bugs :
*
*     None known.
*
*   Authors :
*
*     C.J. Hirst  UCL  (ZUVAD::CJH)
*     M.P. Fisher RGO  (GXSEG0::MPF)
*
*   History :
*
*     ?? ??? 1988  : Original version (ZUVAD::CJH)
*     01 Aug 1989  : Added comments (AAOEPP::WFL)
*     16 Dec 1989  : Changed name to ECH_WAVECEN, cut out ECHELLE argument (and
*                    don't call ECH_INIT) and added STATUS argument
*                    (AAOEPP::WFL)
*     23 Sep 1994  : Call ech_wavecen with theta = (ech_thetab - ech_blaze0).
*                    Find the two orders whose centres are nearest the input
*                    gamma; find wavelength in both orders nearest to input
*                    theta; choose best fit. (GXSEG0::MPF)
*
*
*   Type definitions :
*
      implicit none             ! no default typing allowed
*
*   Global constants :
*
      include 'SAE_PAR'        ! ADAM error codes
*
*   Import :
*
      real theta                ! echelle theta angle
      real gamma                ! echelle gamma angle
*
*   Export :
*
      real wc                   ! central wavelength in Angstroms
      integer mc                ! central order number
*
*   Status :
*
      integer status
*
*   Global Variables :
*
      include 'ech_common'
*
*   Local Constants :
*
      integer maxiter           ! maximum number of wavelength iterations
      parameter (maxiter=50)
      real rad                  ! degrees per radian
      parameter (rad=57.29577951)
*
*   Local variables :
*
      integer niter             ! number of iterations
      integer mdel              ! current order step size
      integer oldmdel           ! previous order step size
      integer mc1               ! alternative order number
      integer mc3               ! copy of chosen order number
      logical got_order         ! whether order determined
      logical got_wave          ! whether wavelength determined
      logical done              ! flag set for second wavelength search
      real thetaoff             ! blaze0 minus true blaze
      real wc3                  ! wavelength after first wavelength search
      real th1                  ! current echelle theta
      real th2                  ! previous echelle theta
      real gam1                 ! current echelle gamma
      real gam2                 ! previous echelle gamma
      real gam3                 ! gamma of order below chosen order
      real gamma3               ! gamma after first wavelength search
      real pp                   ! prism position
      real sa                   ! slit angle
      real wdel                 ! current wavelength step
      real oldwdel              ! previous wavelength step
      character*80 col          ! collimator colour
*
*-----------------------------------------------------------------------
*
*   Check status on entry - return if not OK.
*
      if (status.ne.sai__ok) RETURN
*
*   Set initial order and wavelength steps.
*
      mdel=20
      wdel=25.0
*
*   Theta offset
*
      thetaoff = ech_thetab - ech_blaze0
*
*   Perform binary chop search to determine the order resulting from the value
*   of gamma closest to the actual gamma value. Start at an arbitrary but fairly
*   central point.
*
      wc=4000.0
      call ech_ordernum(4000.0, 0.0, 0.0, mc, status)
      call ech_form_cent(wc, mc, th2, gam2, col, pp, sa, status)
      got_order=.false.
      do while (.not. got_order)
         th1=th2
         gam1=gam2
         oldmdel=mdel
         if(gam1.gt.gamma)then
            mdel=-abs(mdel)
         else
            mdel=abs(mdel)
         endif
         mc=mc+mdel
         call ech_wcentral(mc, thetaoff, gam2, wc, status)
         call ech_form_cent(wc, mc, th2, gam2, col, pp, sa, status)
         if(sign(1, mdel) .ne. sign(1, oldmdel))then
            if(abs(mdel).eq.1)then
               got_order=.true.
            else
               mdel=mdel/2
            endif
         endif
      enddo
*
*   This may have given an order number that is out by one. Choose the two
*   orders whose centres are closest to the input gamma.
*
*
      call ech_wcentral(mc+1, thetaoff, gam2, wc, status)
      call ech_form_cent(wc, mc+1, th1, gam1, col, pp, sa, status)
      call ech_wcentral(mc-1, thetaoff, gam2, wc, status)
      call ech_form_cent(wc, mc-1, th1, gam3, col, pp, sa, status)
      mc1 = mc+1
      if(abs(gam3-gamma).lt.abs(gam1-gamma)) mc1 = mc-1
*
*   Now perform a similar binary chop search for the wavelength within this
*   order which results from the value of theta closest to the actual value
*   of theta. Again start from an arbitrary but fairly central point.
*
      done = .false.
 10   continue
      got_wave=.false.
      call ech_wcentral(mc, thetaoff, gamma, wc, status)
      call ech_form_cent(wc, mc, th2, gam2, col, pp, sa, status)
      niter=0
      do while (.not. got_wave)
         niter=niter+1
         th1=th2
         gam1=gam2
         oldwdel=wdel
         if(th1.gt.theta)then
            wdel=-abs(wdel)
         else
            wdel=abs(wdel)
         endif
         wc=wc+wdel
         call ech_form_cent(wc, mc, th2, gam2, col, pp, sa, status)
         if(sign(1.0, wdel) .ne. sign(1.0, oldwdel))then
            if(abs(wdel).lt.0.01)then
               got_wave=.true.
            else
               wdel=wdel/2.0
            endif
         endif
         if(niter.ge.MAXITER)then
            got_wave=.true.
         endif
      enddo
*
*   Repeat for the alternative order
*
      if (.not. done) then
         gamma3 = gam2
         wc3    = wc
         mc3    = mc
         mc     = mc1
         wdel   = 25.0
         done = .true.
         goto 10
      endif
*
*   Choose the wavelength and order number that give gamma
*   closest to the input gamma.
*
      if (abs(gamma3-gamma) .lt. abs(gam2-gamma)) then
         mc = mc3
         wc = wc3
      endif

      end
