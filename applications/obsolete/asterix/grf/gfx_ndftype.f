*+  GFX_NDFTYPE - determines type of graph data in HDS/NDF form
      SUBROUTINE GFX_NDFTYPE(ID,STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'ADI_PAR'
*    Import :
      INTEGER			ID
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*20		TYPE       		! Dataset type

      INTEGER 			DIMS(ADI__MXDIM)        ! Dimensions
      INTEGER 			NDIM                    ! Dimensionality

      LOGICAL 			DOK			! Primary data is ok?
      LOGICAL 			OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

        G_NDF1=.FALSE.
        G_NDF2=.FALSE.
        G_PRIM1=.FALSE.
        G_PRIM2=.FALSE.
        G_1DSET=.FALSE.
        G_ASCIIY=.FALSE.
        G_ASCIIXY=.FALSE.
        G_ASCIIXYZ=.FALSE.
        G_CINDEX=.FALSE.

        OK=.FALSE.

*  see what kind of NDF
        CALL BDI_GETSHP( ID, ADI__MXDIM, DIMS, NDIM, STATUS )
        CALL BDI_CHK( ID, 'Data', DOK, STATUS )

        IF (DOK) THEN
          IF (NDIM.EQ.1) THEN
*  1D dataset
            G_NDF1=.TRUE.
            OK=.TRUE.
          ELSEIF (NDIM.EQ.2) THEN
*  see if 1D set stored in 2D array
            CALL ADI_CGET0C( ID, 'DatasetType', TYPE, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
              CALL ERR_ANNUL( STATUS )
              G_NDF2=.TRUE.
              OK=.TRUE.
            ELSE IF (INDEX(TYPE,'_SET').NE.0.OR.
     :            INDEX(TYPE,'_SERIES').NE.0) THEN
              G_1DSET=.TRUE.
              OK=.TRUE.
            ELSE IF (TYPE.EQ.'COLOUR_INDEX') THEN
              G_CINDEX=.TRUE.
              OK=.TRUE.
            ELSE
              G_NDF2=.TRUE.
              OK=.TRUE.
            END IF
          ELSE
            CALL MSG_PRNT(
     :       'AST_ERR:data have dimensions >2 and cannot be drawn')
            STATUS=SAI__ERROR
          ENDIF

        ELSE
          CALL MSG_PRNT('AST_ERR: DATA_ARRAY not present, or empty')
          STATUS=SAI__ERROR
        ENDIF


        IF (.NOT.OK.AND.STATUS.EQ.SAI__OK) THEN
          CALL MSG_PRNT('AST_ERR: unrecognised data type')
          STATUS=SAI__OK
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT('GFX_NDFTYPE',STATUS)
        ENDIF

      ENDIF
      END
