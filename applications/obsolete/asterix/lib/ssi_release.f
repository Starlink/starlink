*+  SSI_RELEASE - Release all mapped items
      SUBROUTINE SSI_RELEASE( ID, STATUS )
*
*    Description :
*
*     Look up mapped item table for appropriate item. Free all resources
*     associated with it.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     12 Jul 91 : Original (DJA)
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
      INTEGER STATUS
*
*    Import :
*
      CHARACTER*(DAT__SZLOC)     LOC                ! The dataset
*
*    Local variables :
*
      INTEGER ID
*-

      IF(STATUS.EQ.SAI__OK) THEN
        CALL ADI1_GETLOC(LOC,ID,STATUS)
        CALL SSO_RELEASE( ID, STATUS )
        IF ( STATUS.NE.SAI__OK ) THEN
          CALL AST_REXIT( 'SSI_RELEASE', STATUS )
        ENDIF
      ENDIF
      END
