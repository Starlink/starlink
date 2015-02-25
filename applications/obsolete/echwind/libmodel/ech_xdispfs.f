*+  ECH_XDISPFS  -  Determine the cross-dispersion in mm for a given wavelength
*                   at a specified configuration

      subroutine ech_xdispfs (wave, gamma, del, status)
*
*   Description :
*
*     Calculate the refractive index at the given wavelength. Then calculate it
*     at the point where the cross-dispersion is by definition zero. Knowing
*     the focal length of the camera, calculate the cross-dispersion in mm on
*     the assumption that the light enters and leaves the prisms at the same
*     angles of incidence (a first order approximation is used).
*
*     ECH_INIT must be called before calling this routine.
*
*   Invocation :
*
*     CALL ECH_XDISPFS (WAVE, GAMMA, DEL, STATUS)
*
*   Arguments :
*
*     WAVE    =  REAL (READ)           Wavelength of point at which
*                                      cross-dispersion is to be calculated
*                                      (Angstroms)
*     GAMMA   =  REAL (READ)           Echelle gamma angle corresponding to the
*                                      current configuration (radians)
*     DEL     =  REAL (WRITE)          Cross-dispersion (mm)
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
*     16 Dec 1989  : Renamed to ECH_XDISPFS and added STATUS argument
*                    (AAOEPP::WFL)
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
      real gamma                ! echelle gamma angle
*
*   Export :
*
      real del                  ! dispersion in mm
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
      real rad                  ! degrees per radian
      parameter (rad=57.29577951)
*
*   Local variables :
*
      double precision ref    ! refractive index at wavelength
      double precision ref0   ! refractive index at wavelength where xdisp = 0
      real s                  ! sin(ech_angle/2)
      real delr               ! some temporary quantity or other
*
*-----------------------------------------------------------------------
*
*   Check status on entry - return if not OK.
*
      if (status.ne.sai__ok) RETURN
*
*   Calculate the refractive index at the position of interest and at the point
*   of zero cross-dispersion.
*
      call ech_refindex (dble(wave), ref, status)
      call ech_refindex (dble(ech_wave0), ref0, status)
*
*   Use the camera focal length to calculate the cross-dispersion.
*
      s=sin(ech_angle/2.0)
      delr=((ref0-ref)*2*s/sqrt(1-(((ref0+ref)*s/2)**2))*ech_npr)
      del=(delr+sin(2.0*(gamma-ech_gamma0)))*ech_fcam

      end

