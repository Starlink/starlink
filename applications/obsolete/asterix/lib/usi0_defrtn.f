*+  USI0_DEFRTN - Define a parameter system method
      SUBROUTINE USI0_DEFRTN( PSYS, CODE, RTN, STATUS )
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
      INTEGER			PSYS			! Par ystem id
      INTEGER			CODE			! Par system method
      EXTERNAL			RTN			! Routine
*
*    Status :
*
      INTEGER 			STATUS
*
*    External references :
*
      EXTERNAL			USI_BLK
      EXTERNAL			UTIL_PLOC
        INTEGER                   UTIL_PLOC
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check code in range
      IF ( (CODE .LT. 1) .OR. (CODE.GT.USI__MXPRTN) ) THEN
	PRINT *, 'Illegal par code'

      ELSE

*      Set routine pointer
        USI_PSYS(PSYS).RTN(CODE) = UTIL_PLOC(RTN)

      END IF

      END
