*+  DYN_MAPT - maps dynamic memory for array of specified type
      SUBROUTINE DYN_MAPT(NDIM,DIMS,TYPE,PTR,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
      INTEGER STATUS
*    Import :
      INTEGER NDIM
      INTEGER DIMS(NDIM)
      CHARACTER*(*) TYPE
*    Import/Export :
*    Export :
      INTEGER PTR
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER BYTESIZE
*-
      IF (STATUS.EQ.SAI__OK) THEN
*  check number of dimensions
        IF (NDIM.GT.0.AND.NDIM.LE.DAT__MXDIM) THEN
*  get pointer
          CALL HDX_TYPSIZ( TYPE, BYTESIZE, STATUS )
          CALL DYN_MAP(NDIM,DIMS,BYTESIZE,PTR,STATUS)
        ELSE
          CALL MSG_PRNT('! number of dimensions invalid')
          STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP('DYN_MAPT','from DYN_MAPT',STATUS)
        ENDIF
      ENDIF
      END
