*+  GCB_CRECACHE - creates dynamic memory for cache
      SUBROUTINE GCB_CRECACHE(PTR,STATUS)
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
      INTEGER PTR
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER GCB_SIZE
*    Local constants :
*    Local variables :
      INTEGER SIZ
*-

      IF (STATUS.EQ.SAI__OK) THEN

        SIZ=GCB_SIZE()+GCB__NHDBLK*GCB__SZPTR
        CALL DYN_MAPB(1,SIZ,PTR,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GCB_CRECACHE',STATUS)
        ENDIF

      ENDIF

      END
