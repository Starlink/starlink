*+  ECH_WCENTRAL  -  Determine the central wavelength in Angstroms
*                    in a specified order at a given configuration

      subroutine ech_wcentral (m, theta, gamma, wc, status)
*
*   Description :
*
*     Use the grating equation directly to determine the central wavelength.
*
*     ECH_INIT must be called before calling this routine.
*
*   Invocation :
*
*     CALL ECH_WCENTRAL (M, THETA, GAMMA, WC, STATUS)
*
*   Arguments :
*
*     M          =  INTEGER (READ)     Order number
*     THETA      =  REAL (READ)        Echelle theta angle corresponding to the
*                                      current configuration (radians)
*     GAMMA      =  REAL (READ)        Echelle gamma angle corresponding to the
*                                      current configuration (radians)
*     WC         =  REAL (WRITE)       Central wavelength of this order
*                                      (Angstroms)
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
*     16 Dec 1989  : Renamed to ECH_WCENTRAL and added STATUS argument
*                    (AAOEPP::WFL)
*     23 Sep 1994  : Changed calculation of central wavelength to correspond
*                    with that used by ech_form_cent (MPF/RGO)
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
      real wc                   ! central wavelength
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
*-----------------------------------------------------------------------
*
*   Check status on entry - return if not OK.
*
      if (status.ne.sai__ok) RETURN
*
*   Directly calculate the central wavelength.
*
      wc=(sin(ech_blaze0+0.5*ech_thetacam+theta)
     :   +sin(ech_blaze0-0.5*ech_thetacam+theta))
     :   *cos(gamma)/(ech_d*m*1e-7)

      end

