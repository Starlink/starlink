*+  SSI_LOCBOOK - Locate an BOOK component based on file number
      SUBROUTINE SSI_LOCBOOK( ID, FILE, BID, STATUS )
*    Description :
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
      INTEGER                      FILE          ! File number
*
*    Export :
*
      CHARACTER*(DAT__SZLOC)       FBLOC         ! File's bookeeping structure
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
        CALL SSO_LOCBOOK( LOC, FILE, FBLOC, STATUS )
        CALL ADI1_PUTLOC( FBLOC,BID,STATUS)
        IF ( STATUS.NE.SAI__OK ) THEN
          CALL AST_REXIT( 'SSI_LOCBOOK', STATUS )
        ENDIF
      ENDIF
      END
