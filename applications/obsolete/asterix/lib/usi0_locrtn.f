*+  USI0_LOCRTN - Locate a parameter system routine for current context
      SUBROUTINE USI0_LOCRTN( CODE, RPTR, STATUS )
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
      INTEGER			CODE			! Par system method
*
*    Export :
*
      INTEGER			RPTR			! Method address
*
*    Status :
*
      INTEGER 			STATUS
*
*    External references :
*
      EXTERNAL			USI_BLK
*
*    Local Variables :
*
      INTEGER			PSYS
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get current context system
      PSYS = USI_CTX(USI_ICTX).TYPE

*  Get routine pointer
      RPTR = USI_PSYS(PSYS).RTN(CODE)

*  Report error if zero
      IF ( RPTR .EQ. 0 ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETI( 'CODE', CODE )
        CALL ERR_REP( ' ', 'Null parameter system method, code'/
     :                /' = ^CODE', STATUS )

      END IF

      END
