*+  GCB_CAN1 - cancel vector attribute
      SUBROUTINE GCB_CAN1(NAME,IFIRST,N,STATUS)
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
      CHARACTER*(*) NAME
      INTEGER IFIRST,N
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*16 FMT,TYPE
      INTEGER DISP,SIZ
      INTEGER INDX
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO INDX=IFIRST,IFIRST+N-1
          CALL GCB_LOCCOMP(NAME,INDX,DISP,SIZ,FMT,TYPE,STATUS)
          CALL GCB_CAN_SUB(%val(G_MEMPTR),DISP,SIZ,STATUS)
        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GCB_CAN1',STATUS)
        ENDIF

      ENDIF

      END
