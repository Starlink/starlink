
*+  GFX_STAMP - put user-specified stamp on plot
      SUBROUTINE GFX_STAMP(STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*            BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*80 TEXT
      LOGICAL OK
      LOGICAL FLAG
      REAL X,Y
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_GETL('STAMP_FLAG',OK,FLAG,STATUS)
        IF (.NOT.OK) THEN
          FLAG=.FALSE.
        ENDIF
        CALL GCB_GETC('STAMP_TEXT',OK,TEXT,STATUS)

        IF (FLAG) THEN
          IF (OK) THEN
*  select whole display surface
            CALL PGVPORT(0.0,1.0,0.0,1.0)
            CALL PGWINDOW(0.0,1.0,0.0,1.0)
            X=1.0-0.75/40.0
            Y=0.5
*  plot text
              CALL PGSCH(0.75)
              CALL PGPTEXT(X,Y,270.0,0.5,TEXT)
            ELSE
*  no text specified - default to ID, time and date
            CALL PGIDEN()
          ENDIF
        ENDIF

        CALL GTR_RESTORE(STATUS)
        CALL GCB_SETDEF(STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_STAMP',STATUS)
        ENDIF

      ENDIF

      END
