*+  ECH_FRSPR  -  Determine the free spectral range in Angstroms
*                 of a specified order at a specified configuration

      subroutine ech_frspr (m, theta, gamma, free, status)
*
*   Description :
*
*     Determine the central wavelength of the specified order and then
*     use the grating equation directly to calculate the free spectral
*     range.
*
*     ECH_INIT must be called before calling this routine.
*
*   Invocation :
*
*     CALL ECH_FRSPR (M, THETA, GAMMA, FREE, STATUS)
*
*   Arguments :
*
*     M       =  INTEGER (READ)        Order number for which to calculate the
*                                      free spectral range
*     THETA   =  REAL (READ)           Echelle theta angle corresponding to the
*                                      current configuration (radians)
*     GAMMA   =  REAL (READ)           Echelle gamma angle corresponding to the
*                                      current configuration (radians)
*     FREE    =  REAL (WRITE)          Free spectral range (Angstroms)
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
*     16 Dec 1989  : Changed name to ECH_FRSPR and added STATUS argument
*                    (AAOEPP::WFL)
*     23 Sep 1994  : Replaced hard-coded beam separation with common variable
*                    ech_thetacam; free spectral range calculation changed
*                    to correspond with that used in ech_form_cent (MPF/RGO)
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
      integer m                 ! order number
      real theta                ! echelle theta angle
      real gamma                ! echelle gamma angle
*
*   Export :
*
      real free                 ! free spectral range
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
*   Local variables
*
      real wc                   ! central wavelength
*
*-----------------------------------------------------------------------
*
*   Check status on entry - return if not OK.
*
      if (status.ne.sai__ok) RETURN
*
*   Determine central wavelength of the specified order.
*
      call ech_wcentral(m, theta, gamma, wc, status)
*
*   Calculate the free spectral range
*
      free=wc*wc*ech_d*1.0e-7/(cos(gamma)*
     :     (sin(ech_blaze0+0.5*ech_thetacam+theta)+
     :     sin(ech_blaze0-0.5*ech_thetacam+theta)))

      end
