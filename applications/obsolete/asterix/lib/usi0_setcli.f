*+  USI0_SETCLI - Set program command line in current context
      SUBROUTINE USI0_SETCLI( CSTRING, CLEN, STATUS )
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
*    Status :
*
      INTEGER 			STATUS
*
*    Import :
*
      CHARACTER*(*)		CSTRING			! Command string
      INTEGER			CLEN			! Length of CSTRING
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Define the string
      USI_CTX(USI_ICTX).CSTRING = CSTRING(1:CLEN)
      USI_CTX(USI_ICTX).CLEN = CLEN

      END
