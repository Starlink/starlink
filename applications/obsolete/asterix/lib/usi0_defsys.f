*+  USI0_DEFSYS - Define a new parameter sstem
      SUBROUTINE USI0_DEFSYS( NAME, PARID, STATUS )
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     21 May 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USI0_PAR'
*
*    Global variables :
*
      INCLUDE 'USI_CMN'
*
*    Import :
*
      CHARACTER*(*)		NAME 			! System name
*
*    Export :
*
      INTEGER			PARID			! Parameter system id
*
*    Status :
*
      INTEGER 			STATUS
*
*    Functions :
*
      INTEGER CHR_LEN
*
*    Local variables :
*
      INTEGER			I
*-

*    Increment counter
      USI_NPSYS = USI_NPSYS + 1
      PARID = USI_NPSYS

*    Store the name
      USI_PSYS(PARID).NAME = NAME
      USI_PSYS(PARID).NLEN = CHR_LEN(NAME)

*    Zero the routine storage
      DO I = 1, USI__MXPRTN
        USI_PSYS(PARID).RTN(I) = 0
      END DO

      END
