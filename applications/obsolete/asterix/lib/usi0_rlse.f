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
*-

*  Check existing count

*  Log file
      CALL USI0_OUTLOG( STATUS )

*  Export data to external environment

*  Reset context data
      CTX_TYPE(USI_ICTX) = 0
      CALL ADI_ERASE( CTX_PST(USI_ICTX), STATUS )

*  Lower context count
      USI_ICTX = USI_ICTX - 1

      END
