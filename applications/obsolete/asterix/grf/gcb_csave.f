*+  GCB_CSAVE - saves Grafix Control Block direct from cache to HDS component
      SUBROUTINE GCB_CSAVE(CACHE,LOC,STATUS)
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
      INTEGER CACHE
      CHARACTER*(DAT__SZLOC) LOC
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC,GCBLOC
      INTEGER GCBPTR
      INTEGER NSCAL,NSTRUC
      INTEGER DISP
      INTEGER NBYTE,BYTES
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  find size required
        DISP=GCB__SZPTR+1
        CALL GCB_GETI_SUB(%val(CACHE),DISP,GCB__SZPTR,GCB__PTRFMT,
     :                                                NSCAL,STATUS)
        DISP=DISP+GCB__SZPTR
        CALL GCB_GETI_SUB(%val(CACHE),DISP,GCB__SZPTR,GCB__PTRFMT,
     :                                                NSTRUC,STATUS)
        NBYTE=GCB__NHDBLK*GCB__SZPTR+NSCAL+NSTRUC

        CALL BDA_CHKGRAF(LOC,OK,STATUS)
        IF (.NOT.OK) THEN
          CALL BDA_CREGRAF(LOC,STATUS)
        ENDIF
        CALL BDA_LOCGRAF(LOC,GLOC,STATUS)
*  see if component already there
        CALL DAT_THERE(GLOC,'GRAFIX_CONTROL',OK,STATUS)
        IF (OK) THEN
          CALL DAT_FIND(GLOC,'GRAFIX_CONTROL',GCBLOC,STATUS)
          CALL DAT_SIZE(GCBLOC,BYTES,STATUS)
*  alter if necessary
          IF (BYTES.NE.NBYTE) THEN
            CALL DAT_ALTER(GCBLOC,1,NBYTE,STATUS)
          ENDIF
          CALL DAT_MAP(GCBLOC,'_BYTE','UPDATE',1,NBYTE,GCBPTR,STATUS)

*  if not then create it
        ELSE
          CALL DAT_NEW(GLOC,'GRAFIX_CONTROL','_BYTE',1,NBYTE,STATUS)
          CALL DAT_FIND(GLOC,'GRAFIX_CONTROL',GCBLOC,STATUS)
          CALL DAT_MAP(GCBLOC,'_BYTE','WRITE',1,NBYTE,GCBPTR,STATUS)
        ENDIF

*  copy cache to output
        CALL ARR_COP1B(NBYTE,%val(CACHE),%val(GCBPTR),STATUS)

*  release output
        CALL DAT_ANNUL(GCBLOC,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GCB_CSAVE',STATUS)
        ENDIF

      ENDIF

      END
