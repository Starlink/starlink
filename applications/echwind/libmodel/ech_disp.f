*+  ECH_DISP  -  Determine the dispersion in mm for a given wavelength
*                in a given order at a specified configuration

      subroutine ech_disp (wave, m, theta, gamma, del, status)
*
*   Description :
*
*     Use the grating equation to calculate the echelle beta angle at the given
*     wavelength in the given order. Then calculate it at the point where the
*     dispersion is by definition zero. Knowing the focal length of the camera,
*     calculate the dispersion in mm.
*
*     ECH_INIT must be called before calling this routine.
*
*   Invocation :
*
*     CALL ECH_DISP (WAVE, M, THETA, GAMMA, DEL, STATUS)
*
*   Arguments :
*
*     WAVE    =  REAL (READ)           Wavelength of point at which dispersion
*                                      is to be calculated (Angstroms)
*     M       =  INTEGER (READ)        Order number in which this wavelength
*                                      lies (it doesn't have to be within the
*                                      free spectral range)
*     THETA   =  REAL (READ)           Echelle theta angle corresponding to the
*                                      current configuration (radians)
*     GAMMA   =  REAL (READ)           Echelle gamma angle corresponding to the
*                                      current configuration (radians)
*     DEL     =  REAL (WRITE)          Dispersion (mm)
*     STATUS  =  INTEGER (READ, WRITE) Global status value
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
*     ?? ??? 1988  : Original version (ZUVAD::CJH)
*     01 Aug 1989  : Added comments; made it crashproof (AAOEPP::WFL)
*     18 Dec 1989  : Convert to ECH_DISP and add STATUS argument (AAOEPP::WFL)
*     23 Sep 1994  : Replaced hard-wired beam separation by common variable
*                    ech_thetacam; use ech_blaze0 in grating equation. (copy
*                    of VAX changes by TONSCH@HRDKSW5) (MPF/RGO)
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
      real wave                 ! wavelength in Angstroms
      integer m                 ! order number
      real theta                ! echelle theta angle
      real gamma                ! echelle gamma angle
*
*   Export :
*
      real del                  ! dispersion in mm
*
*   Status :
*
      integer status            ! global status value
*
*   Global Variables :
*
      include 'ech_common'      ! current echelle parameters
*
*   Local Constants :
*
      real pi                   ! PI
      parameter (pi=3.141592654)
      real rad                  ! degrees per radian
      parameter (rad=57.29577951)
*
*   Local variables :
*
      real w                    ! dummy wavelength argument
      real th                   ! dummy theta argument
      real gam                  ! dummy gamma argument
      real sinbeta              ! statement function that calculates beta
      real sinbeta1             ! sin(beta) at point of interest
      real sinbeta2             ! sin(beta) at point where dispersion = 0
      real beta1                ! beta at point of interest
      real beta2                ! beta at point where dispersion = 0
*
*   Statement functions (this is just the grating equation) :
*
      sinbeta(w,m,th,gam)=ech_d*1.0e-7*m*w/cos(gam)-
     :                   sin(ech_blaze0+th+0.5*ech_thetacam)
*
*-----------------------------------------------------------------------
*
*   Check status on entry - return if not OK.
*
      if (status.ne.sai__ok) RETURN
*
*   Calculate sin(beta) at the position of interest and cope with out-of-range
*   input.
*
      sinbeta1=sinbeta(wave,m,theta,gamma)
      if(sinbeta1.lt.-1.0)then
         beta1=-pi/2.0
      elseif(sinbeta1.gt.+1.0)then
         beta1=+pi/2.0
      else
         beta1=asin(sinbeta1)
      endif
*
*   Similarly at the dispersion zero point.
*
      sinbeta2=sinbeta(ech_wave0,ech_m0,ech_theta0,ech_gamma0)
      if(sinbeta2.lt.-1.0)then
         beta2=-pi/2.0
      elseif(sinbeta2.gt.+1.0)then
         beta2=+pi/2.0
      else
         beta2=asin(sinbeta2)
      endif
*
*   Use the camera focal length to calculate the dispersion.
*
      del=(beta1-beta2)*ech_fcam

      end
