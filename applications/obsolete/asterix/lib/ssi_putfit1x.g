*+  SSI_PUTFITEM1<T> - Write a field item of type <COMM>
      SUBROUTINE SSI_PUTFITEM1<T>( ID, FLD, ITEM, NVAL, VALUE, STATUS )
*    Description :
*
*     Write a field item for field FLD.
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
      CHARACTER*(*)                ITEM          ! Field item to create
      INTEGER                      NVAL          ! Number of values
      <TYPE>                       VALUE(*)      ! Field item values
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
        CALL SSO_PUTFITEM1<T>( ID, FLD, ITEM, NVAL, VALUE, STATUS )
        IF ( STATUS.NE.SAI__OK ) THEN
          CALL AST_REXIT( 'SSI_PUTFITEM1<T>', STATUS )
        ENDIF
      ENDIF
      END
