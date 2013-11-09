*+  SSI_PUTNSRC - Create SSDS POSIT object and set number of sources
      SUBROUTINE SSI_PUTNSRC( ID, NSRC, STATUS )
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
      INTEGER			ID
      INTEGER                      NSRC          ! Number of sources
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
        CALL SSO_PUTNSRC( LOC, NSRC, STATUS )

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'SSI_PUTNSRC', STATUS )
        END IF

      END IF

      END
