*+  USI0_RLSE - Release parameter context
      SUBROUTINE USI0_RLSE( STATUS )
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
*    Global variables :
*
      INCLUDE 'USI_CMN'
*
*    Status :
*
      INTEGER 			STATUS
*
*  Local Variables:
*
      INTEGER			I			! Loop over parameters
      INTEGER			ID			! File identifier
      INTEGER			PSID			! Parameter store
      INTEGER			NPAR			! # parameters

      LOGICAL			THERE			! Object exists?
*-

*  Check existing count

*  Log file
      CALL USI0_OUTLOG( STATUS )

*  Export data to external environment
      CALL ADI_NCMP( CTX_PST(USI_ICTX), NPAR, STATUS )
      DO I = 1, NPAR

*    Locate the parameter
        CALL ADI_INDCMP( CTX_PST(USI_ICTX), I, PSID, STATUS )

*    Get its 3 bits of info
        CALL ADI_THERE( PSID, 'ID', THERE, STATUS )
        IF ( THERE ) THEN
          CALL ADI_CGET0I( PSID, 'ID', ID, STATUS )

*      Close the file
          CALL ADI_FCLOSE( ID, STATUS )
        END IF

*    Release parameter
        CALL ADI_ERASE( PSID, STATUS )

      END DO

*  Reset context data
      CTX_TYPE(USI_ICTX) = 0
      CALL ADI_ERASE( CTX_PST(USI_ICTX), STATUS )

*  Lower context count
      USI_ICTX = USI_ICTX - 1

      END
