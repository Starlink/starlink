*+  GFX_NDFTYPE - determines type of graph data in HDS/NDF form
      SUBROUTINE GFX_NDFTYPE(LOC,STATUS)

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
*    Import :
      CHARACTER*(DAT__SZLOC) LOC
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZTYP) TYPE       ! object type
      INTEGER DIMS(DAT__MXDIM)          ! dimensions
      INTEGER NDIM                      ! dimensionality
      LOGICAL DOK			! DATA_ARRAY has values
      LOGICAL OK
*    Internal References :
*    Local data :
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
        CALL BDA_CHKDATA(LOC,DOK,NDIM,DIMS,STATUS)

        IF (DOK) THEN
          IF (NDIM.EQ.1) THEN
*  1D dataset
            G_NDF1=.TRUE.
            OK=.TRUE.
          ELSEIF (NDIM.EQ.2) THEN
*  see if 1D set stored in 2D array
            CALL DAT_TYPE(LOC,TYPE,STATUS)
            IF (INDEX(TYPE,'_SET').NE.0.OR.
     :            INDEX(TYPE,'_SERIES').NE.0) THEN
              G_1DSET=.TRUE.
              OK=.TRUE.
            ELSEIF (TYPE.EQ.'COLOUR_INDEX') THEN
              G_CINDEX=.TRUE.
              OK=.TRUE.
            ELSE
              G_NDF2=.TRUE.
              OK=.TRUE.
            ENDIF
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
          CALL ERR_REP(' ','from GFX_NDFTYPE',STATUS)
        ENDIF

      ENDIF
      END
