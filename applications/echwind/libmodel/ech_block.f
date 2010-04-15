*+  ECH_BLOCK  -  BLOCK DATA to initialise spectrograph configuration
*                 COMMON block

      block data ech_block
*
*   Description :
*
*     This refers to the ECH_PARAMS COMMON block, which contains the parameters
*     of all possible spectrographs, not to the ECH_COMMON COMMON block, which
*     contains the details only of the current configuration.
*
*     This BLOCK DATA initialises the COMMON block in such a way that a call
*     to ECH_INIT without a call to ECH_LOAD to load the parameters of a
*     specific spectrograph configuration will use the values which are correct
*     for UCLES. These values also serve to provide defaults for quantities
*     which are not overridden as a result of ECH_LOAD.
*
*   Bugs :
*
*     None known.
*
*   Authors :
*
*     W.F. Lupton  AAO  (AAOEPP::WFL)
*
*   History :
*
*     07 Dec 1989  : Original version (AAOEPP::WFL)
*     23 Sep 1994  : Added blaze0, thetacam, collxe, prface, prapex (MPF/RGO)
*
*   Type definitions :
*
      implicit none             ! no default typing allowed
*
*   Global Variables :
*
      include 'ech_params'      ! parameters of all possible spectrographs
*
*   Local Constants :
*
      real rad                  ! degrees per radian
      parameter (rad = 57.29577951)
      real angle_ucles          ! prism angle for UCLES (radians)
      parameter (angle_ucles = 54.1/rad)
      real thetab_31            ! blaze angle for 31 echelle (radians)
      parameter (thetab_31 = 64.6/rad)
      real thetab_79            ! blaze angle for 79 echelle (radians)
      parameter (thetab_79 = 63.55/rad)
*
*-----------------------------------------------------------------------
*
*   Set COMMON values to describe a single instrument (UCLES) with two
*   echelles (31 and 79) and a single camera (LONG). Do it in such a way
*   that 0 indices result in the default values for everything and so that
*   instrument 0 is UCLES with the LONG camera.

      data ninsts /0/

      data npr(0) /3/
      data angle(0) /angle_ucles/
      data fcol(0) /6000.0/
      data insts(0) /'UCLES'/
      data collxe(0) /-1.800/
      data prface(0) /0.313/
      data prapex(0) /0.015/

      data nechs(0) /2/

      data m0(0,0) /138/
      data d(0,0) /31.6046/
      data thetab(0,0) /thetab_31/
      data blaze0(0,0) /thetab_31/
      data theta0(0,0) /0.0/
      data gamma0(0,0) /0.0/
      data wave0(0,0) /4119.68/
      data echs(0,0) /'31'/

      data m0(1,0) /138/
      data d(1,0) /31.6046/
      data thetab(1,0) /thetab_31/
      data blaze0(1,0) /thetab_31/
      data theta0(1,0) /0.0/
      data gamma0(1,0) /0.0/
      data wave0(1,0) /4119.68/
      data echs(1,0) /'31'/

      data m0(2,0) /55/
      data d(2,0) /79.0115/
      data thetab(2,0) /thetab_79/
      data blaze0(2,0) /thetab_79/
      data theta0(2,0) /0.0/
      data gamma0(2,0) /0.0/
      data wave0(2,0) /4097.99/
      data echs(2,0) /'79'/

      data ncams(0) /1/

      data fcam(0,0) /700.0/
      data thetacam(0,0) /12.0/
      data cams(0,0) /'LONG'/

      data fcam(1,0) /700.0/
      data thetacam(1,0) /12.0/
      data cams(1,0) /'LONG'/

      end

