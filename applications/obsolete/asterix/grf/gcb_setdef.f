*+  GCB_SETDEF - set current default attributes
      SUBROUTINE GCB_SETDEF(STATUS)
*    Description :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
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
      REAL R,G,B
      INTEGER C1,C2
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
          CALL GCB_GETI('DEFAULT_BGND',OK,IVAL,STATUS)
          IF (.NOT.OK) THEN
            IVAL=0
          ENDIF
          IF (IVAL.EQ.0) THEN
            R=0.0
            G=0.0
            B=0.0
          ELSE
            CALL PGQCR(IVAL,R,G,B)
          ENDIF
          CALL PGQCOL(C1,C2)
          CALL PGSCR(C1,R,G,B)
          CALL GCB_GETI('DEFAULT_COLOUR',OK,IVAL,STATUS)
          IF (.NOT.OK) THEN
            IVAL=1
          ENDIF
          IF (IVAL.EQ.0) THEN
            R=0.0
            G=0.0
            B=0.0
            IVAL=1
            CALL PGSCR(1,R,G,B)
          ELSEIF (IVAL.EQ.1) THEN
            R=1.0
            G=1.0
            B=1.0
            CALL PGSCR(1,R,G,B)
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
