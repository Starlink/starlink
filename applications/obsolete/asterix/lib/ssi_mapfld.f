*+  SSI_MAPFLD - Map an SSO field
      SUBROUTINE SSI_MAPFLD( ID, FLD, TYPE, MODE, PTR, STATUS )
*    Description :
*
*     Map an old format SSDS field. A wrap up for all the old routines.
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
      CHARACTER*(*)                FLD           ! Field to map
      CHARACTER*(*)                TYPE          ! Mapping type
      CHARACTER*(*)                MODE          ! Access mode
*
*    Export :
*
      INTEGER                      PTR           ! Ptr to mapped field
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
        CALL SSO_MAPFLD( ID, FLD, TYPE, MODE, PTR, STATUS )
        IF ( STATUS.NE.SAI__OK ) THEN
          CALL AST_REXIT( 'SSI_MAPFLD', STATUS )
        ENDIF
      ENDIF
      END
