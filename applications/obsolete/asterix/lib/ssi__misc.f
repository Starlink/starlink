*+  SSI_PUTFITEM0C - Write a field item of type _CHAR*LEN
      SUBROUTINE SSI_PUTFITEM0C( ID, FLD, ITEM, LEN, VALUE, STATUS )
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
      INTEGER                      LEN           ! Length of character item
      CHARACTER*(*)                VALUE         ! Field item value
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
        CALL SSO_PUTFITEM0C( LOC, FLD, ITEM, LEN, VALUE, STATUS )
        IF ( STATUS.NE.SAI__OK ) THEN
          CALL AST_REXIT( 'SSI_PUTFITEM0C', STATUS )
        ENDIF
      ENDIF
      END
*+  SSI_UNMAPFLDERR - Unmap a field
      SUBROUTINE SSI_UNMAPFLDERR( ID, FLD, STATUS )
*
*    Description :
*
*     Look up mapped item table for appropriate item. Free all resources
*     associated with it.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     12 Jul 91 : Original (BHVAD::DJA)
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
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      CHARACTER*(DAT__SZLOC)     LOC                ! The dataset
      CHARACTER*(*)              FLD                ! The field name
*
*    Local variables :
*
      INTEGER ID
*-

      IF(STATUS.EQ.SAI__OK) THEN
        CALL ADI1_GETLOC(ID,LOC,STATUS)
        CALL SSO_UNMAPFLDERR( LOC, FLD, STATUS )
        IF ( STATUS.NE.SAI__OK ) THEN
          CALL AST_REXIT( 'SSI_UNMAPFLDERR', STATUS )
        ENDIF
      ENDIF
      END
