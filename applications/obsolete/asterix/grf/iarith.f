*+  IARITH - add/sub/mult/div an image, array or constant to loaded image
      SUBROUTINE IARITH(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*       22 Jan 93: V1.7-0 original
*       13 Sep 94: V1.7-1 updates data min/max (RJV)
*       19 Oct 95: V2.0-0 ADI port (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*10 OPER,CMD
      REAL VAL
      INTEGER NDIM,DIMS(2),FID
      INTEGER DPTR,VPTR,QPTR
      LOGICAL MATCH
      LOGICAL DATASET,SCALAR
      LOGICAL VOK,QOK
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = ' Version 2.0-0')
*-

      CALL USI_INIT()

      CALL USI_GET0C('OPER',OPER,STATUS)
      IF (OPER.EQ.'ADD') THEN
        CALL MSG_PRNT('IADD'//VERSION)
        CMD='IADD'
      ELSEIF (OPER.EQ.'SUB') THEN
        CALL MSG_PRNT('ISUB'//VERSION)
        CMD='ISUB'
      ELSEIF (OPER.EQ.'MULT') THEN
        CALL MSG_PRNT('IMULT'//VERSION)
        CMD='IMULT'
      ELSEIF (OPER.EQ.'DIV') THEN
        CALL MSG_PRNT('IDIV'//VERSION)
        CMD='IDIV'
      ENDIF

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSE

*  get image or whatever to add/sub etc
        CALL USI_ASSOC( 'INP', 'BinDS|Array|Scalar', 'READ',
     :                  FID, STATUS )
        SCALAR = .FALSE.
        CALL BDI_GETSHP( FID, 2, DIMS, NDIM, STATUS )
        CALL ADI_DERVD( FID, 'BinDS', DATASET, STATUS )

        IF ( .NOT. DATASET ) THEN
*  scalar input
          IF (NDIM.EQ.0) THEN
            CALL BDI_GET0R(FID,'Data',VAL,STATUS)
            SCALAR=.TRUE.
            MATCH=.TRUE.
            QOK=.FALSE.
            VOK=.FALSE.
*  primitive array - check dimensions match
          ELSEIF (NDIM.EQ.2.AND.DIMS(1).EQ.I_NX.AND.DIMS(2).EQ.I_NY)
     :                                                           THEN
            MATCH=.TRUE.
          ELSE
            CALL MSG_PRNT(
     :            'AST_ERR: array size does not match loaded image')
            MATCH=.FALSE.
          ENDIF
        ELSE
*  dataset - do fuller check
          CALL IMG_MATCH( FID, MATCH, STATUS )
        ENDIF

        IF (MATCH) THEN

          IF (SCALAR) THEN

*  copy existing data to work area
            CALL IMG_COPY(.FALSE.,.FALSE.,STATUS)
            I_CAN_UNDO=.FALSE.

            IF (OPER.EQ.'ADD') THEN
              CALL IARITH_SCALAR_ADD(VAL,%VAL(I_DPTR_W),STATUS)
            ELSEIF (OPER.EQ.'SUB') THEN
              CALL IARITH_SCALAR_SUB(VAL,%VAL(I_DPTR_W),STATUS)
            ELSEIF (OPER.EQ.'MULT') THEN
              CALL IARITH_SCALAR_MULT(VAL,%VAL(I_DPTR_W),STATUS)
            ELSEIF (OPER.EQ.'DIV') THEN
              CALL IARITH_SCALAR_DIV(VAL,%VAL(I_DPTR_W),STATUS)
            ENDIF

          ELSE

            CALL BDI_MAPR( FID, 'Data', 'READ', DPTR, STATUS )
            IF (I_VOK) THEN
              CALL BDI_MAPR( FID, 'Variance', 'READ', VPTR, STATUS )
            ENDIF
            CALL BDI_CHK( FID, 'Quality', QOK, STATUS )
            IF (I_QOK.OR.OPER.EQ.'DIV') THEN
              QOK=.TRUE.
            ENDIF
            IF (QOK) THEN
              CALL BDI_MAP( FID, 'Variance', 'UBYTE', 'READ', QPTR,
     :                      STATUS )
            ENDIF

*  copy existing data to work area, creating quality if necessary
            CALL IMG_COPY(.FALSE.,QOK,STATUS)
            I_CAN_UNDO=.FALSE.

            IF (OPER.EQ.'ADD') THEN
              CALL IARITH_ADD(%VAL(DPTR),%VAL(VPTR),%VAL(QPTR),
     :                  %VAL(I_DPTR_W),%VAL(I_VPTR_W),%VAL(I_QPTR_W),
     :                                                        STATUS)
            ELSEIF (OPER.EQ.'SUB') THEN
              CALL IARITH_SUB(%VAL(DPTR),%VAL(VPTR),%VAL(QPTR),
     :                  %VAL(I_DPTR_W),%VAL(I_VPTR_W),%VAL(I_QPTR_W),
     :                                                        STATUS)
            ELSEIF (OPER.EQ.'MULT') THEN
              CALL IARITH_MULT(%VAL(DPTR),%VAL(VPTR),%VAL(QPTR),
     :                  %VAL(I_DPTR_W),%VAL(I_VPTR_W),%VAL(I_QPTR_W),
     :                                                        STATUS)
            ELSEIF (OPER.EQ.'DIV') THEN
              CALL IARITH_DIV(%VAL(DPTR),%VAL(VPTR),%VAL(QPTR),
     :                  %VAL(I_DPTR_W),%VAL(I_VPTR_W),%VAL(I_QPTR_W),
     :                                                        STATUS)
            ENDIF

          ENDIF

          CALL IMG_SWAP(STATUS)
          CALL IMG_MINMAX(STATUS)

          IF (STATUS.EQ.SAI__OK) THEN
            I_CAN_UNDO=.TRUE.
            I_PROC_COUNT=I_PROC_COUNT+1
            I_LAST_CMD=CMD
          ENDIF

        ENDIF

        CALL USI_CANCL('INP',STATUS)

      ENDIF

      CALL USI_CLOSE()

      END


      SUBROUTINE IARITH_SCALAR_ADD(VAL,D,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL VAL
*    Import/export :
      REAL D(I_NX,I_NY)
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER I,J
*-
      IF (STATUS.EQ.SAI__OK) THEN

        DO J=1,I_NY
          DO I=1,I_NX
            D(I,J)=D(I,J)+VAL
          ENDDO
        ENDDO

      ENDIF

      END





      SUBROUTINE IARITH_SCALAR_SUB(VAL,D,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL VAL
*    Import/export :
      REAL D(I_NX,I_NY)
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER I,J
*-
      IF (STATUS.EQ.SAI__OK) THEN

        DO J=1,I_NY
          DO I=1,I_NX
            D(I,J)=D(I,J)-VAL
          ENDDO
        ENDDO

      ENDIF

      END





      SUBROUTINE IARITH_SCALAR_MULT(VAL,D,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL VAL
*    Import/export :
      REAL D(I_NX,I_NY)
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER I,J
*-
      IF (STATUS.EQ.SAI__OK) THEN

        DO J=1,I_NY
          DO I=1,I_NX
            D(I,J)=D(I,J)*VAL
          ENDDO
        ENDDO

      ENDIF

      END





      SUBROUTINE IARITH_SCALAR_DIV(VAL,D,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL VAL
*    Import/export :
      REAL D(I_NX,I_NY)
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER I,J
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (VAL.NE.0.0) THEN
          DO J=1,I_NY
            DO I=1,I_NX
              D(I,J)=D(I,J)/VAL
            ENDDO
          ENDDO
        ELSE
          CALL MSG_PRNT('AST_ERR: attempted division by zero')
          STATUS=SAI__ERROR
        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IARITH_ADD(D,V,Q,DD,VV,QQ,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL D(I_NX,I_NY)
      REAL V(I_NX,I_NY)
      BYTE Q(I_NX,I_NY)
*    Import/export :
      REAL DD(I_NX,I_NY)
      REAL VV(I_NX,I_NY)
      BYTE QQ(I_NX,I_NY)
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ORUB
*    Local constants :
*    Local variables :
      INTEGER I,J
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (I_QOK) THEN
          DO J=1,I_NY
            DO I=1,I_NX
              QQ(I,J)=BIT_ORUB(QQ(I,J),Q(I,J))
            ENDDO
          ENDDO
        ENDIF

        IF (I_VOK) THEN
          DO J=1,I_NY
            DO I=1,I_NX
              VV(I,J)=VV(I,J)+V(I,J)
            ENDDO
          ENDDO
        ENDIF

        DO J=1,I_NY
          DO I=1,I_NX
            DD(I,J)=DD(I,J)+D(I,J)
          ENDDO
        ENDDO


      ENDIF

      END




*+
      SUBROUTINE IARITH_SUB(D,V,Q,DD,VV,QQ,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL D(I_NX,I_NY)
      REAL V(I_NX,I_NY)
      BYTE Q(I_NX,I_NY)
*    Import/export :
      REAL DD(I_NX,I_NY)
      REAL VV(I_NX,I_NY)
      BYTE QQ(I_NX,I_NY)
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ORUB
*    Local constants :
*    Local variables :
      INTEGER I,J
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (I_QOK) THEN
          DO J=1,I_NY
            DO I=1,I_NX
              QQ(I,J)=BIT_ORUB(QQ(I,J),Q(I,J))
            ENDDO
          ENDDO
        ENDIF

        IF (I_VOK) THEN
          DO J=1,I_NY
            DO I=1,I_NX
              VV(I,J)=VV(I,J)-V(I,J)
            ENDDO
          ENDDO
        ENDIF

        DO J=1,I_NY
          DO I=1,I_NX
            DD(I,J)=DD(I,J)-D(I,J)
          ENDDO
        ENDDO


      ENDIF

      END




*+
      SUBROUTINE IARITH_MULT(D,V,Q,DD,VV,QQ,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL D(I_NX,I_NY)
      REAL V(I_NX,I_NY)
      BYTE Q(I_NX,I_NY)
*    Import/export :
      REAL DD(I_NX,I_NY)
      REAL VV(I_NX,I_NY)
      BYTE QQ(I_NX,I_NY)
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ORUB
*    Local constants :
*    Local variables :
      INTEGER I,J
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (I_QOK) THEN
          DO J=1,I_NY
            DO I=1,I_NX
              QQ(I,J)=BIT_ORUB(QQ(I,J),Q(I,J))
            ENDDO
          ENDDO
        ENDIF

        IF (I_VOK) THEN
          DO J=1,I_NY
            DO I=1,I_NX
              VV(I,J)=VV(I,J)*D(I,J)**2 + V(I,J)*DD(I,J)**2
            ENDDO
          ENDDO
        ENDIF

        DO J=1,I_NY
          DO I=1,I_NX
            DD(I,J)=DD(I,J)*D(I,J)
          ENDDO
        ENDDO


      ENDIF

      END




*+
      SUBROUTINE IARITH_DIV(D,V,Q,DD,VV,QQ,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL D(I_NX,I_NY)
      REAL V(I_NX,I_NY)
      BYTE Q(I_NX,I_NY)
*    Import/export :
      REAL DD(I_NX,I_NY)
      REAL VV(I_NX,I_NY)
      BYTE QQ(I_NX,I_NY)
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ORUB
*    Local constants :
*    Local variables :
      INTEGER I,J
*-
      IF (STATUS.EQ.SAI__OK) THEN

        DO J=1,I_NY
          DO I=1,I_NX
            IF (D(I,J).NE.0.0) THEN
              DD(I,J)=DD(I,J)/D(I,J)
              IF (I_VOK) THEN
                VV(I,J)=VV(I,J)/D(I,J)**2 +
     :                       V(I,J)*DD(I,J)**2/D(I,J)**4
              ENDIF
              QQ(I,J)=BIT_ORUB(QQ(I,J),Q(I,J))
            ELSE
              QQ(I,J)=BIT_ORUB(QQ(I,J),QUAL__ARITH)
            ENDIF
          ENDDO
        ENDDO

      ENDIF

      END
