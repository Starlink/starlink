*+  USI0_PARSE - Cancel environment value
      SUBROUTINE USI0_PARSE( CSTRING, STATUS )
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
      CHARACTER*(*)		CSTRING			! Command string
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local variables :
*
      INTEGER			RPTR			! Facility routine
*-

*    New error context
      CALL ERR_MARK

*    Locate the PARSE facility in this context
      CALL USI0_LOCRTN( USI__F_PARSE, RPTR, STATUS )

*    Invoke it
      CALL USI0_PARSE_E( %VAL(RPTR), CSTRING, STATUS )

*    Restore error context
      CALL ERR_RLSE

      END



*+  USI0_PARSE_E - Invoke parameter cancelling routine
      SUBROUTINE USI0_PARSE_E( PARSE_RTN, CSTRING, STATUS )
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
      EXTERNAL			PARSE_RTN		! Cancel routine
      CHARACTER*(*)		CSTRING			! Command string
*
*    Status :
*
      INTEGER 			STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Invoke the PARSE routine
      CALL PARSE_RTN( CSTRING, STATUS )

      END
