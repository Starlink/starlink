*+  PSS_RTN_SEL - Select routine on processing mode
      INTEGER FUNCTION PSS_RTN_SEL( RTN, URTN )
*
*    Description :
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     26 Feb 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      EXTERNAL                 RTN                     ! Optimising routine
      EXTERNAL                 URTN                    ! Upper limit routine
*
*    Functions :
*
      INTEGER                  UTIL_PLOC               ! Replaces %LOC
*-

      IF ( CP_OPT ) THEN
        PSS_RTN_SEL = UTIL_PLOC( RTN )
      ELSE
        PSS_RTN_SEL = UTIL_PLOC( URTN )
      END IF

      END
