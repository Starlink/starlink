*+  USI_CANCL - Cancel environment value
      SUBROUTINE USI_CANCL( PAR, STATUS )
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
      CHARACTER*(*)		PAR			! Parameter name
*
*    Status :
*
      INTEGER 			STATUS
*
*    External references :
*
      EXTERNAL			USI_BLK
*
*    Local variables :
*
      INTEGER			RPTR			! Facility routine
*-

*    New error context
      CALL ERR_MARK

*    Check USI initialised
      IF ( .NOT. USI_SYINIT ) THEN
        CALL USI_INIT( )
      END IF

*    Locate the CANCL facility in this context
      CALL USI0_LOCRTN( USI__F_CANCL, RPTR, STATUS )

*    Invoke it
      CALL USI_CANCL_E( %VAL(RPTR), PAR, STATUS )

*    Restore error context
      CALL ERR_RLSE

      END



*+  USI_CANCL_E - Invoke parameter cancelling routine
      SUBROUTINE USI_CANCL_E( CANCL_RTN, PAR, STATUS )
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
*
*    Import :
*
      EXTERNAL			CANCL_RTN		! Cancel routine
      CHARACTER*(*)		PAR			! Parameter name
*
*    Status :
*
      INTEGER 			STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Invoke the CANCL routine
      CALL CANCL_RTN( PAR, STATUS )

      END
