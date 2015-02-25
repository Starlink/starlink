*+  ECH_COLLIM  -  Determine which collimator to use at a given wavelength

      subroutine ech_collim (wave, gamma, colour, status)
*
*   Description :
*
*     If a central wavelength is given, use the WIDE collimator for wavelengths
*     longer than 4000 Angstroms and the UV collimator otherwise. Otherwise
*     make the decision based on whether gamma is greater or less than 0.1369
*     degrees. For UCLES, this value of gamma corresponds a central wavelength
*     of very close to 4000 Angstroms. For UES the corresponding wavelength is
*     approximately 4150 Angstroms.
*
*   Invocation :
*
*     CALL ECH_COLLIM (WAVE, GAMMA, COLOUR, STATUS)
*
*   Arguments :
*
*     WAVE    =  REAL (READ)           Central wavelength of detector. If
*                                      specified as <= 0, is not used
*                                      (Angstroms)
*     GAMMA   =  REAL (READ)           Echelle gamma angle corresponding to the
*                                      current configuration. Is used only if
*                                      WAVE is <= 0
*                                      (radians)
*     COLOUR  =  CHARACTER (WRITE)     The optimal collimator, either WIDE or UV
*     STATUS  =  INTEGER (READ, WRITE) Global status value
*
*   Bugs :
*
*     The algorithm is too simplistic. It should take account of the detector
*     size and guarantee that the best choice of collimator is made for the
*     whole detector rather than just for its centre (although this is rather
*     hard without knowledge of the configuration history!).
*
*   Authors :
*
*     C.J. Hirst  UCL  (ZUVAD::CJH)
*
*   History :
*
*     ?? ??? 1988  : Original version (ZUVAD::CJH)
*     01 Aug 1989  : Added comments; changed RED and BLUE to WIDE and UV;
*                    corrected bug that would have meant that when the central
*                    wavelength was not specified the WIDE collimator would
*                    always have been chosen (AAOEPP::WFL)
*     18 Dec 1989  : Convert to ECH_COLLIMATOR and add STATUS argument
*                    (AAOEPP::WFL)
*     23 Sep 1994  : Changed name to ECH_COLLIM.F as archive truncates names
*                    longer than 15 characters. (MPF/RGO)
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
      real gamma                ! corresponding echelle gamma angle
*
*   Export :
*
      character*(*) colour      ! optimal collimator
*
*   Status :
*
      integer status            ! global status value
*
*   Local Constants :
*
      real rad                  ! degrees per radian
      parameter (rad = 57.29577951)
*
*-----------------------------------------------------------------------
*
*   Check status on entry - return if not OK.
*
      if (status.ne.sai__ok) RETURN
*
*   If a central wavelength has been supplied, use it.
*
      if(wave.gt.0.0)then
         if(wave.gt.4000.0)then
            colour='WIDE'
         else
            colour='UV'
         endif
*
*   Otherwise, decide on the basis of the echelle gamma angle.
*
      else
         if(gamma*rad.gt.0.1369)then
            colour='UV'
         else
            colour='WIDE'
         endif
      endif

      end
