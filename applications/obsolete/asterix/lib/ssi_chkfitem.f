*+  SSI_CHKFITEM - Check existance and validity of a field item
      SUBROUTINE SSI_CHKFITEM( ID, FLD, ITEM, OK, STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Aug 91 : Original (DJA)
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
*    Import :
*
      CHARACTER*(DAT__SZLOC)       LOC           ! SSDS locator
      CHARACTER*(*)                FLD           ! Field to find
      CHARACTER*(*)                ITEM          ! Field item to create
*
*    Export :
*
      LOGICAL                      OK            ! Field item is ok?
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER ID
*-

      IF(STATUS.EQ.SAI__OK) THEN
        CALL ADI1_GETLOC(LOC,ID,STATUS)
        CALL SSO_CHKFITEM( ID, FLD, ITEM, OK, STATUS )
        IF ( STATUS.NE.SAI__OK ) THEN
          CALL AST_REXIT( 'SSI_CHKFITEM', STATUS )
        ENDIF
      ENDIF
      END
