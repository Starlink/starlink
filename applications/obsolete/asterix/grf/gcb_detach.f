*+  GCB_DETACH - detach current context from GCB
      SUBROUTINE GCB_DETACH(STATUS)
*    Description :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
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
      INTEGER ISTAT
*-

      IF (G_LOCAL) THEN
        CALL DYN_UNMAP(G_DMEMPTR)
        G_DMEMPTR=0
        G_LOCAL=.FALSE.
      ELSE
        CALL GCB_DETACH_REMCONTXT(STATUS)
        IF (G_NCONTEXT.EQ.0) THEN
          CALL DYN_UNMAP(G_GMEMPTR)
          G_GMEMPTR=0
c          ISTAT=SAI__OK
c          CALL NBS_LOSE_NOTICEBOARD(G_ID,'FORCE',ISTAT)
c          G_ID=0
        ENDIF
      ENDIF

      END


*+  GCB_DETACH_REMCONTXT - detach current graphics context
      SUBROUTINE GCB_DETACH_REMCONTXT(STATUS)
*    Description :
*    Authors :
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
*-
      IF (STATUS.EQ.SAI__OK) THEN


        IF (G_CONTEXT.EQ.'GRAFIX') THEN
          G_GRAFIX=.FALSE.
          G_NCONTEXT=G_NCONTEXT-1
          IF (G_GRAFIX_PTR.NE.0) THEN
            CALL GCB_DELCACHE(G_GRAFIX_PTR,STATUS)
            G_GRAFIX_PTR=0
          ENDIF
        ELSEIF (G_CONTEXT.EQ.'IMAGE') THEN
          G_IMAGE=.FALSE.
          G_NCONTEXT=G_NCONTEXT-1
          IF (G_IMAGE_PTR.NE.0) THEN
            CALL GCB_DELCACHE(G_IMAGE_PTR,STATUS)
            G_IMAGE_PTR=0
          ENDIF
        ELSEIF (G_CONTEXT.EQ.'SPECTRUM') THEN
          G_SPECTRUM=.FALSE.
          G_NCONTEXT=G_NCONTEXT-1
          IF (G_SPECTRUM_PTR.NE.0) THEN
            CALL GCB_DELCACHE(G_SPECTRUM_PTR,STATUS)
            G_SPECTRUM_PTR=0
          ENDIF
        ELSEIF (G_CONTEXT.EQ.'TIME') THEN
          G_TIME=.FALSE.
          G_NCONTEXT=G_NCONTEXT-1
          IF (G_TIME_PTR.NE.0) THEN
            CALL GCB_DELCACHE(G_TIME_PTR,STATUS)
            G_TIME_PTR=0
          ENDIF
        ENDIF

        G_CONTEXT=CHAR(0)

      ENDIF

      END
