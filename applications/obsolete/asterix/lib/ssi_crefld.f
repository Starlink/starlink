*+  SSI_CREFLD - Create an SSDS field
      SUBROUTINE SSI_CREFLD( ID, FLD, TYPE, STATUS )
*    Description :
*
*     Create an old format SSDS field. A wrap up for all the old routines.
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
      INTEGER			ID			!
      CHARACTER*(*)                FLD           ! Field to find
      CHARACTER*(*)                TYPE          ! Field type
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)       LOC           ! SSDS locator
*-

*    Status ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

        CALL ADI1_GETLOC( ID, LOC, STATUS )
        CALL SSO_CREFLD( LOC, FLD, TYPE, STATUS )

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL AST_REXIT( 'SSI_CREFLD', STATUS )
        END IF

      END IF

      END
