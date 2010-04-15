*+  ECH_FORM_CENT  -  Determine the appropriate spectrograph settings
*                         to place a specified wavelength in a specified order
*                         at the centre of the field

      subroutine ech_form_cent (wc, mcen, theta4, gamma4,
     :                     col_colour, prism_pos, slit_angle, status)
*
*   Description :
*
*     Firstly, calculate the prism system rotation and position. The required
*     echelle gamma angle is not quite the same as the prism rotation angle.
*     This is because of the 12 degree separation between the incoming ray to
*     to the echelle grating and the outgoing ray to the camera - the prism
*     rotation angle has to be multiplied by an empirical correction factor
*     of 1.011 (I am not sure of the complete justification for this).
*
*     Having determined the appropriate echelle gamma angle, the cross
*     dispersion is now correct and it is necessary to determine the
*     appropriate echelle theta angle to give the correct wavelength in the
*     centre of the field. If the order number was not explicitly specified,
*     assume the order within which the given wavelength lies with the free
*     spectral range. Theta is determined simply by stepping along the order
*     and, once having stepped past the required wavelength, binary chopping
*     until convergence.
*
*     Finally, determine the appropriate collimator to use and the necessary
*     slit assembly rotation in order to give vertical slit images on the
*     detector.
*
*     ECH_INIT must be called before calling this routine.
*
*   Invocation :
*
*     CALL ECH_FORM_CENT (WC, MCEN, THETA, GAMMA, COL_COLOUR, PRISM_POS,
*                                                    SLIT_ANGLE, STATUS)
*
*   Arguments :
*
*     WC          =  REAL (READ)           Desired central wavelength
*                                          (Angstroms)
*     MCEN        =  INTEGER (READ)        Desired central order. If <=0 the
*                                          order in which the above wavelength
*                                          lies within the free spectral range
*                                          is used
*     THETA       =  REAL (WRITE)          Echelle theta (radians)
*     GAMMA       =  REAL (WRITE)          Echelle gamma (radians)
*     COL_COLOUR  =  CHARACTER (WRITE)     Collimator (UV or WIDE)
*     PRISM_POS   =  REAL (WRITE)          Prism position (mm)
*     SLIT_ANGLE  =  REAL (WRITE)          Slit angle (radians)
*     STATUS      =  INTEGER (READ, WRITE) Global status value
*
*   Bugs :
*
*     None known.
*
*   Authors :
*
*     F. Diego  UCL  (ZUVAD::FD)
*
*   History :
*
*     ?? ??? 1988  : Original version (ZUVAD::FD)
*     23 Nov 1987  : Include slit rotation (ZUVAD::FD)
*     01 Aug 1989  : Added comments; cut out code that was never used; used
*                    ECH_THETA0 and ECH_GAMMA0 in ORDERNUM call (AAOEPP::WFL)
*     18 Dec 1989  : Convert to ECH_FORMAT_CENTRE and add STATUS argument;
*                    cut out unnecessary code and call more low-level routines
*                    to make its operation more obvious (AAOEPP::WFL)
*     19 May 1992  : Change name to ECH_FORM_CENT (archive truncates names
*                    of more than 15 characters) (SJM/MSSS0)
*     23 Sep 1994  : Replaced gamafac and hard-coded beam separation with
*                    common variables ech_gamafac & ech_thetacam; ech_blaze0
*                    used in grating equations. (MPF/RGO)
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
      real wc                   ! central wavelength
      integer mcen              ! central order
*
*   Export :
*
      real theta4               ! echelle theta
      real gamma4               ! echelle gamma
      character*(*) col_colour  ! collimator colour
      real prism_pos            ! prism position
      real slit_angle           ! slit angle
*
*   Status :
*
      integer status
*
*   Global Variables :
*
      include 'ech_common'      ! current echelle parameters
*
*   Local Constants :
*
      double precision rad      ! degrees per radian
      parameter (rad=57.29577951)
*
*   Local variables :
*
      integer mc                ! actual central order
      logical w                 ! whether still in wavelength search
      real gam                  ! rotation of 3 prism system
      double precision d        ! copy of echelle rules per mm
      double precision thetab   ! copy of echelle blaze angle
      double precision blaze0   ! copy of echelle blaze0
      double precision wavec    ! copy of desired central wavelength
      double precision gamma    ! rotation of 3 prism system
      double precision gamafac  ! empirical gamma factor
      double precision gamemp   ! empirical gamma
      double precision wavpeak  ! wavelength at peak blaze response
      double precision delta    ! step in theta during search
      double precision theta    ! echelle theta
      double precision wavecal  ! calculated wavelength during search
      double precision thetacam ! beam separation of camera
*
*-----------------------------------------------------------------------
*
*   Check status on entry - return if not OK.
*
      if (status.ne.sai__ok) RETURN
*
*   If the order was not specified, assume the one which has the desired
*   wavelength within the free spectral range.
*
      if(mcen.gt.0)then
         mc=mcen
      else
         call ech_ordernum(wc, ech_theta0, ech_gamma0, mc, status)
      endif
*
*   Copy COMMON variables to DOUBLE PRECISION equivalents (some of these are
*   not in COMMON but should be).
*
      d=ech_d                   ! echelle rules / mm
      thetab=ech_thetab         ! true blaze angle (radians)
      blaze0 = ech_blaze0       ! ideal blaze angle (radians)
      thetacam = ech_thetacam   ! beam separation (radians)
      gamafac = ech_gamafac     ! empirical gamma factor
*
*   Copy the central wavelength to DOUBLE PRECISION equivalent.
*
      wavec=wc
*
*-----------------------------------------------------------------------
*
*   Calculate prism system rotation and position.
*
      call ech_prismpos(wc, gam, prism_pos, status)
      gamma=gam
*
*-----------------------------------------------------------------------
*
*   Empirical GAMAFAC compensates for the 12 degree beam separation effect
*   on GAMMA.
*
      gamemp=gamma*gamafac
*
*-----------------------------------------------------------------------
*
*   Search along the order to get the correct value for THETA.
*
      wavpeak=(sin(blaze0+0.5*thetacam)+sin(blaze0-0.5*thetacam))
     :         *cos(gamemp) /mc/d*10**7
      delta=0.01
      if(wavec.lt.wavpeak)then
         theta=+delta
         do while (delta.gt.0.000001)
            w=.true.
            do while (w)
               theta=theta-delta
               wavecal=(sin(blaze0+0.5*thetacam+theta)
     :                 +sin(blaze0-0.5*thetacam+theta))
     :                 *cos(gamemp)/mc/d*10**7
               if(wavecal.le.wavec)w=.false.
            enddo
            theta=theta+delta
            delta=delta/2.0
         enddo
      else if(wavec.gt.wavpeak)then
         theta=-delta
         do while (delta.gt.0.000001)
            w=.true.
            do while (w)
               theta=theta+delta
               wavecal=(sin(blaze0+0.5*thetacam+theta)
     :                 +sin(blaze0-0.5*thetacam+theta))
     :                 *cos(gamemp)/mc/d*10**7
               if(wavecal.ge.wavec)w=.false.
            enddo
            theta=theta-delta
            delta=delta/2.0
         enddo
      else if(wavec.eq.wavpeak)then
         theta=0.0
      endif
*
*   Copy theta and gamma to the relevant arguments.
*
      theta4=theta
      gamma4=gamemp
*
*-----------------------------------------------------------------------
*
*   Determine the optimal collimator to use.
*
      call ech_collim(wc, gamma4, col_colour, status)
*
*-----------------------------------------------------------------------
*
*   Calculate the slit rotation angle.
*
      call ech_slitangle(gamma4, slit_angle, status)

      end
