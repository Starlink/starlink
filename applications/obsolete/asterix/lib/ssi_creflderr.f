*+  SSI_CREFLDERR - Create an SSO field error component
      SUBROUTINE SSI_CREFLDERR( ID, FLD, TYPE, NDAT, NLEV, STATUS )
*    Description :
*
*     Create an old format SSDS field error. A wrap up for all the old routines.
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
      CHARACTER*(*)                TYPE          ! Field type
      INTEGER                      NDAT          ! # data items per level
      INTEGER                      NLEV          ! # levels
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
        CALL SSO_CREFLDERR( LOC, FLD, TYPE, NDAT, NLEV, STATUS )
        IF ( STATUS.NE.SAI__OK ) THEN
          CALL AST_REXIT( 'SSI_CREFLDERR', STATUS )
        ENDIF
      ENDIF
      END
