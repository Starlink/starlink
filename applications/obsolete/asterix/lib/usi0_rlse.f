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
*    External references :
*
      EXTERNAL			USI_BLK
*-

*  Check existing count

*  Log file
      CALL USI0_OUTLOG( STATUS )

*  Export data to external environment

*  Lower context count
      USI_CTX(USI_ICTX).TYPE = 0
      IF ( USI_CTX(USI_ICTX).PSTORE .NE. DAT__NOLOC ) THEN
        CALL DAT_ANNUL( USI_CTX(USI_ICTX).PSTORE, STATUS )
      END IF
      USI_ICTX = USI_ICTX - 1

      END
