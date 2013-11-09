*+  SPEC_INIT - Initialise new spectral fitting session
      SUBROUTINE SPEC_INIT( STATUS )
*
*    Description :
*
*     Performs initialisation of the spectral fitting common blocks required
*     at the start of a new application. At the moment this consists only of
*     resetting the SPEC_CMN_RZ system to ensure the search for new RZ cubes
*     occurrs properly inside ICL.
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*      9 Sep 93 : Original (DJA)
*      2 Mar 94 : RZ error buffering added (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*
*    Global variables :
*
      INCLUDE 'SPEC_CMN_RZ'
*
*    Status :
*
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set FIRST flag
      S_FIRST = .TRUE.

*    Reset RZ buffering by switching it on and then off. As no errors can
*    occur between these two calls the effect is to reset the error counters.
      CALL SPEC_RZ_ERR( .TRUE., STATUS )
      CALL SPEC_RZ_ERR( .FALSE., STATUS )

      END
