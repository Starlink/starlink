*+  GCB_UNCACHE - uncache into the current Grafix Control Block
      SUBROUTINE GCB_UNCACHE(PTR,STATUS)
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
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_LOAD_SUB(%val(PTR),%val(G_MEMPTR),STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GCB_UNCACHE',STATUS)
        ENDIF

      ENDIF

      END
