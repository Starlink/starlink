*+  USI0_SETCL1 - Retrieve top-level command string
      SUBROUTINE USI0_SETCL1( STATUS )
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
*
*    Status :
*
      INTEGER 			STATUS
*
*    Functions :
*
      INTEGER			CHR_LEN
      INTEGER			IARGC
*
*    Local variables :
*
      CHARACTER*132	        CARG			! Command argument
      CHARACTER*512		CSTRING			! Command string

      INTEGER			IARG			! Loop over arguments
      INTEGER			CLEN			! Length of CSTRING
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Loop while non-blank arguments
      IARG = 0
      CLEN = 0
      DO IARG = 0, IARGC()
        CALL GETARG( IARG, CARG )
        IF ( CARG .GT. ' ' ) THEN
          CSTRING(CLEN+1:) = CARG
          CLEN = CLEN + CHR_LEN(CARG) + 1
        END IF
      END DO

*    Set command string
      CALL USI0_SETCLI( CSTRING, CLEN-1, STATUS )

      END
