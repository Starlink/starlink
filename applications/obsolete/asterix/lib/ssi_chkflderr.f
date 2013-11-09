*+  SSI_CHKFLDERR - Check existance of field error
      SUBROUTINE SSI_CHKFLDERR( ID, FLD, OK, STATUS )
*    Description :
*
*     Check presence an old format SSDS field error. Assumes presence of
*     field itself.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     17 Jun 91 : Original (DJA)
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
*
*    Export :
*
      LOGICAL                      OK            ! Field exists?
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
        CALL ADI1_GETLOC(ID,LOC,STATUS)
        CALL SSO_CHKFLDERR( LOC, FLD, OK, STATUS )
        IF ( STATUS.NE.SAI__OK ) THEN
          CALL AST_REXIT( 'SSI_CHKFLDERR', STATUS )
        ENDIF
      ENDIF
      END
