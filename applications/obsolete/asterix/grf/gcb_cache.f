*+  GCB_CACHE - cache the current Grafix Control NoticeBoard
      SUBROUTINE GCB_CACHE(PTR,STATUS)
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
      INTEGER PTR
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER NBYTE,NSCAL,NSTRUC
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_CSIZE(NBYTE,NSCAL,NSTRUC,STATUS)
        CALL GCB_SAVE_SUB(NSCAL,NSTRUC,%val(G_MEMPTR),%val(PTR),STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GCB_CACHE',STATUS)
        ENDIF

      ENDIF

      END
