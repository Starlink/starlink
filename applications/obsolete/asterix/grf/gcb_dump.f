*+  GCB_DUMP - raw dump of GCB
      SUBROUTINE GCB_DUMP(STATUS)
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
      INTEGER NBYTE,NSCAL,NSTRUC
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  get size of two parts of GCB
        CALL GCB_CSIZE(NBYTE,NSCAL,NSTRUC,STATUS)

*  dump contents
        CALL GCB_DUMP_SUB(%val(G_MEMPTR),NSCAL,NSTRUC,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GCB_DUMP',STATUS)
        ENDIF

      ENDIF

      END


*+
      SUBROUTINE GCB_DUMP_SUB(MEM,NSCAL,NSTRUC,STATUS)
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
      BYTE MEM(*)
      INTEGER NSCAL,NSTRUC
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER GCB_SIZE
*    Local constants :
*    Local variables :
      CHARACTER*79 BUFF
      INTEGER IB
      INTEGER I,J,K,L
*-

      IF (STATUS.EQ.SAI__OK) THEN


*  write scalar components
        CALL MSG_PRNT('Scalar components..')
        K=0
        DO I=1,NSCAL,79
          BUFF=' '
          DO J=1,79
            K=K+1
            IB=MEM(K)
            IF (IB.GT.32.AND.IB.LT.128) THEN
              BUFF(J:J)=CHAR(IB)
            ELSEIF (IB.EQ.32) THEN
              BUFF(J:J)='~'
            ELSE
              BUFF(J:J)='`'
            ENDIF
          ENDDO
          CALL MSG_PRNT(BUFF)
        ENDDO
        IF (K.LT.NSCAL) THEN
          J=0
          BUFF=' '
          DO I=K+1,NSCAL
            J=J+1
            IB=MEM(I)
            IF (IB.GT.32.AND.IB.LT.128) THEN
              BUFF(J:J)=CHAR(IB)
            ELSEIF (IB.EQ.32) THEN
              BUFF(J:J)='~'
            ELSE
              BUFF(J:J)='`'
            ENDIF
          ENDDO
          CALL MSG_PRNT(BUFF)
        ENDIF

*  write structured components
        CALL MSG_PRNT('Structured components..')
        K=GCB_SIZE()-NSTRUC+1
        L=0
        DO I=1,NSTRUC,79
          L=L+1
          BUFF=' '
          DO J=1,79
            IB=MEM(K)
            IF (IB.GT.32.AND.IB.LT.128) THEN
              BUFF(J:J)=CHAR(IB)
            ELSEIF (IB.EQ.32) THEN
              BUFF(J:J)='~'
            ELSE
              BUFF(J:J)='`'
            ENDIF
            K=K+1
          ENDDO
          CALL MSG_PRNT(BUFF)
        ENDDO
        IF (L.LT.NSTRUC) THEN
          BUFF=' '
          J=0
          DO I=L+1,NSTRUC
            J=J+1
            IB=MEM(K)
            IF (IB.GT.32.AND.IB.LT.128) THEN
              BUFF(J:J)=CHAR(IB)
            ELSEIF (IB.EQ.32) THEN
              BUFF(J:J)='~'
            ELSE
              BUFF(J:J)='`'
            ENDIF
            K=K+1
          ENDDO
          CALL MSG_PRNT(BUFF)
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GCB_DUMP_SUB',STATUS)
        ENDIF

      ENDIF

      END
