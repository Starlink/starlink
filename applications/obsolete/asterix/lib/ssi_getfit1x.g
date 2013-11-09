*+  SSI_GETFITEM1<T> - Read a field item of type <COMM>[]
      SUBROUTINE SSI_GETFITEM1<T>( ID, FLD, ITEM, MAXVAL, VALUE,
     :                                           ACTVAL, STATUS )
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
      INTEGER                      MAXVAL        ! Maximum number of values
*
*    Export :
*
      <TYPE>                       VALUE(*)      ! Field item values
      INTEGER                      ACTVAL        ! Actual number of values
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
        CALL SSO_GETFITEM1<T>( LOC, FLD, ITEM, MAXVAL, VALUE,
     :                                           ACTVAL, STATUS )
        IF ( STATUS.NE.SAI__OK ) THEN
          CALL AST_REXIT( 'SSI_GETFITEM1<T>', STATUS )
        ENDIF
      ENDIF
      END
