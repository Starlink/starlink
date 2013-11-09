*+  SSI_PUTPARS - Write a structured parameter to an SSDS file
      SUBROUTINE SSI_PUTPARS( ID, FILE, PAR, VID, STATUS )
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
*     19 Jun 91 : Original (DJA)
*
*      9 Mar 92 : Added FILE argument (DJA)
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
      INTEGER                      FILE          ! File slot
      CHARACTER*(*)                PAR           ! Parameter to create
      CHARACTER*(DAT__SZLOC)       VLOC          ! Value locator
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER ID,VID
*-

      IF(STATUS.EQ.SAI__OK) THEN
        CALL ADI1_GETLOC(ID,LOC,STATUS)
        CALL ADI1_GETLOC(VID,VLOC,STATUS)
        CALL SSO_PUTPARS( LOC, FILE, PAR, VLOC, STATUS )
        IF ( STATUS.NE.SAI__OK ) THEN
          CALL AST_REXIT( 'SSI_PUTPARS', STATUS )
        ENDIF
      ENDIF
      END
