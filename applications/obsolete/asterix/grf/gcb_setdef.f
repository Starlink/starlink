*+  GCB_SETDEF - set current default attributes
      SUBROUTINE GCB_SETDEF(STATUS)
*    Description :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Global variables :
      INCLUDE 'GCB_CMN'
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      REAL RVAL
      INTEGER IVAL
      LOGICAL OK
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (G_MEMPTR.NE.0) THEN
          CALL GCB_GETI('DEFAULT_STYLE',OK,IVAL,STATUS)
          IF (.NOT.OK) THEN
            IVAL=1
          ENDIF
          CALL PGSLS(IVAL)
          CALL GCB_GETI('DEFAULT_WIDTH',OK,IVAL,STATUS)
          IF (.NOT.OK) THEN
            IVAL=1
          ENDIF
          CALL PGSLW(IVAL)
          CALL GCB_GETI('DEFAULT_COLOUR',OK,IVAL,STATUS)
          IF (.NOT.OK) THEN
            IVAL=1
          ENDIF
          CALL PGSCI(IVAL)
          CALL GCB_GETI('DEFAULT_FONT',OK,IVAL,STATUS)
          IF (.NOT.OK) THEN
            IVAL=1
          ENDIF
          CALL PGSCF(IVAL)
          CALL GCB_GETR('DEFAULT_SIZE',OK,RVAL,STATUS)
          IF (.NOT.OK) THEN
            RVAL=1.0
          ENDIF
          CALL PGSCH(RVAL)

        ENDIF

      ENDIF

      END
