*+  GCB_CLEAR - clears Grafix Control Block
      SUBROUTINE GCB_CLEAR(STATUS)
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
      INTEGER GCB_REL2ABS
*    Local constants :
*    Local variables :
      INTEGER DISP,SIZ
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  skip past end pointer
        DISP=GCB__SZPTR+1
*  clear to top of scalar part
        SIZ=G_TOPSCAL-GCB__SZPTR
        CALL GCB_ZERO(%val(G_MEMPTR),DISP,SIZ,STATUS)

*  for structured part just need to zero pointers to start of linked lists
        DISP=GCB_REL2ABS(G_STARTLISTS)
        SIZ=G_STARTLISTS
        CALL GCB_ZERO(%val(G_MEMPTR),DISP,SIZ,STATUS)

*  reset pointer to next available space to start of storage area
        CALL GCB_PUTRELPTR(G_STARTLISTS,G_ENDSTRUC,STATUS)


      ENDIF

      END
