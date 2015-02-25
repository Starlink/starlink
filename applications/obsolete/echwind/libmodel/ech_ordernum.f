*+  ECH_ORDERNUM  -  Determine the order number of a specified wavelength
*                    at a specified configuration

      subroutine ech_ordernum (w, theta, gamma, m, status)
*
*   Description :
*
*     Use the grating equation to calculate the free spectral range and then
*     derive the order number directly from it. The order number may be out
*     by 1 if the wavelength is near the edge of the free spectral range.
*     A more accurate routine is ECH_FINDORDER.
*
*     ECH_INIT must be called before calling this routine.
*
*   Invocation :
*
*     CALL ECH_ORDERNUM (W, THETA, GAMMA, M, STATUS)
*
*   Arguments :
*
*     W       =  REAL (READ)           Wavelength whose order number is to be
*                                      determined (ie the order number within
*                                      which it lies within the free spectral
*                                      range) (Angstroms)
*     THETA   =  REAL (READ)           Echelle theta angle corresponding to the
*                                      current configuration (radians)
*     GAMMA   =  REAL (READ)           Echelle gamma angle corresponding to the
*                                      current configuration (radians)
*     M       =  INTEGER (WRITE)       Order number corresponding to wavelength
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
*     01 Aug 1989  : Added comments (AAOEPP::WFL)
*     16 Dec 1989  : Changed name to ECH_ORDERNUM and added STATUS argument
*                    (AAOEPP::WFL)
*     23 Sep 1994  : Replaced hard-coded beam separation with ech_thetacam;
*                    blaze0 used in free spectral range calculation (MPF/RGO)
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
      real w                    ! wavelength
      real theta                ! echelle theta angle
      real gamma                ! echelle gamma angle
*
*   Export :
*
      integer m                 ! order number
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
      real fsr                  ! free spectral range
*
*-----------------------------------------------------------------------
*
*   Check status on entry - return if not OK.
*
      if (status.ne.sai__ok) RETURN
*
*   Determine the free spectral range at this wavelength.
*
      fsr=w*w*ech_d*1.0e-7/(cos(gamma)*
     :    (sin(ech_blaze0+0.5*ech_thetacam+(ech_thetab-ech_blaze0))+
     :     sin(ech_blaze0-0.5*ech_thetacam+(ech_thetab-ech_blaze0))))
*
*   Directly calculate the order number.
*
      m=nint(w/fsr)

      end

