*+  ECH_SLITANGLE  -  Determine the optimum slit angle
*                     at a specified configuration

      subroutine ech_slitangle (gamma, ang, status)
*
*   Description :
*
*     Determine the slit angle by direct calculation.
*
*     ECH_INIT must be called before calling this routine.
*
*   Invocation :
*
*     CALL ECH_SLITANGLE (GAMMA, ANG, STATUS)
*
*   Arguments :
*
*     GAMMA   =  REAL (READ)           Echelle gamma angle corresponding to the
*                                      current configuration (radians)
*     ANG     =  REAL (WRITE)          Optimal slit angle (radians)
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
*     12 Dec 1989  : Renamed to ECH_SLITANGLE, added STATUS argument and
*                    inserted code from ECH_FORMAT_CENTRE (AAOEPP::WFL)
*     23 Sep 1994  : Replaced hard-coded beam separation with common variable
*                    ech_thetacam
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
      real gamma                ! echelle gamma angle
*
*   Export :
*
      real ang                  ! slit angle
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
*-----------------------------------------------------------------------
*
*   Check status on entry - return if not OK.
*
      if (status.ne.sai__ok) RETURN
*
*   Calculate the optimum slit angle.
*   Approximation, errors up to 2 percent for 12 degrees rotation.
*
      ang=2.0*tan(ech_thetab)*gamma*cos(ech_thetab-0.5*ech_thetacam)
     :                             /cos(ech_thetab+0.5*ech_thetacam)

      end

