*+  GMD_PUTINDEX - adds specified entry to multiple dataset index
      SUBROUTINE GMD_PUTINDEX(LOC,NUM,ENTRY,STATUS)
*    Description :
*         if NUM<=0 then entry is added for last graph
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
      CHARACTER*(*) ENTRY		! text to be entered
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) ILOC
      CHARACTER*(DAT__SZLOC) ELOC
      INTEGER SIZE
      INTEGER NINDX
      LOGICAL THERE
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  add entry for last graph?
        IF (NUM.LE.0) THEN
          CALL GMD_QNDF(LOC,NINDX,STATUS)
        ELSE
          NINDX=NUM
        ENDIF

        CALL DAT_THERE(LOC,'INDEX',THERE,STATUS)
*  create index if not already there
        IF (.NOT.THERE) THEN
          CALL DAT_NEW(LOC,'INDEX','_CHAR*75',1,NINDX,STATUS)
          CALL DAT_FIND(LOC,'INDEX',ILOC,STATUS)
        ELSE
          CALL DAT_FIND(LOC,'INDEX',ILOC,STATUS)
*  make index bigger if necessary
          CALL DAT_SIZE(LOC,SIZE,STATUS)
          IF (SIZE.LT.NINDX) THEN
            CALL DAT_ALTER(ILOC,1,NINDX,STATUS)
          ENDIF
        ENDIF

*  add entry
        CALL DAT_CELL(ILOC,1,NINDX,ELOC,STATUS)
        CALL DAT_PUT0C(ELOC,ENTRY,STATUS)
        CALL DAT_ANNUL(ELOC,STATUS)
        CALL DAT_ANNUL(ILOC,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GMD_PUTINDEX',STATUS)
        ENDIF

      ENDIF

      END
