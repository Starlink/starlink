*+  GMD_GETINDEX - gets specified entry from multiple dataset index
      SUBROUTINE GMD_GETINDEX(LOC,NUM,ENTRY,STATUS)
*    Description :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Import :
      CHARACTER*(DAT__SZLOC) LOC	! locator to dataset
      INTEGER NUM			! entry number
*    Import-Export :
*    Export :
      CHARACTER*(*) ENTRY		! text
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) ILOC
      CHARACTER*(DAT__SZLOC) ELOC
      INTEGER SIZE
      LOGICAL THERE
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL DAT_THERE(LOC,'INDEX',THERE,STATUS)
*  see if index exists
        IF (.NOT.THERE) THEN
          ENTRY=' '
        ELSE
*  see if specified entry there
          CALL CMP_SIZE(LOC,'INDEX',SIZE,STATUS)
          IF (SIZE.LT.NUM) THEN
            ENTRY=' '
          ELSE
*  get entry
            CALL DAT_FIND(LOC,'INDEX',ILOC,STATUS)
            CALL DAT_CELL(ILOC,1,NUM,ELOC,STATUS)
            CALL DAT_GET0C(ELOC,ENTRY,STATUS)
            CALL DAT_ANNUL(ELOC,STATUS)
            CALL DAT_ANNUL(ILOC,STATUS)
          ENDIF
        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GMD_GETINDEX',STATUS)
        ENDIF

      ENDIF

      END
