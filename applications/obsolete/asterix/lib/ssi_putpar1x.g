*+  SSI_PUTPAR1<T> - Write a vector parameter to an SSDS file
      SUBROUTINE SSI_PUTPAR1<T>( ID, FILE, PAR, NVAL, VALUE, STATUS )
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
      INTEGER                      NVAL          ! Number of values
      <TYPE>                       VALUE(*)      ! Parameter values
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
        CALL SSO_PUTPAR1<T>( ID, FILE, PAR, NVAL, VALUE, STATUS )
        IF ( STATUS.NE.SAI__OK ) THEN
          CALL AST_REXIT( 'SSI_PUTPAR1<T>', STATUS )
        ENDIF
      ENDIF
      END
