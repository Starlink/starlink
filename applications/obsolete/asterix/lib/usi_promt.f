*+  USI_PROMT - Set prompt of environment value
      SUBROUTINE USI_PROMT( PAR, PSTR, STATUS )
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
      CHARACTER*(*)   		PSTR			! Prompt string
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
        CALL USI_INIT( STATUS )
      END IF

*    Locate the STATE facility in this context
      CALL USI0_LOCRTN( USI__F_PROMT, RPTR, STATUS )

*    Invoke it
      CALL USI_PROMT_E( %VAL(RPTR), PAR, PSTR, STATUS )

*    Restore error context
      CALL ERR_RLSE

      END



*+  USI_PROMT_E - Invoke parameter cancelling routine
      SUBROUTINE USI_PROMT_E( STATE_RTN, PAR, PSTR, STATUS )
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
      EXTERNAL			STATE_RTN		! Cancel routine
      CHARACTER*(*)		PAR			! Parameter name
      CHARACTER*(*)		PSTR			! Prompt string
*
*    Status :
*
      INTEGER 			STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Invoke the STATE routine
      CALL STATE_RTN( PAR, PSTR, STATUS )

      END
