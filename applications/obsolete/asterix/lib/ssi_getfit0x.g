*+  SSI_GETFITEM0<T> - Read a field item of type <COMM>
      SUBROUTINE SSI_GETFITEM0<T>( ID, FLD, ITEM, VALUE, STATUS )
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
      INTEGER			ID			! Dataset id
      CHARACTER*(*)                FLD           ! Field to find
      CHARACTER*(*)                ITEM          ! Field item to create
*
*    Export :
*
      <TYPE>                       VALUE         ! Field item value
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
        CALL SSI_GETFITEM0<T>( ID, FLD, ITEM, VALUE, STATUS )
        IF ( STATUS.NE.SAI__OK ) THEN
          CALL AST_REXIT( 'SSI_GETFITEM0<T>', STATUS )
        ENDIF
      ENDIF
      END
