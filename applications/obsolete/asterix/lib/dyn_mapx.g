*+  DYN_MAP<T> - Maps dynamic memory for <TYPE> array
      SUBROUTINE DYN_MAP<T>(NDIM,DIMS,PTR,STATUS)
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
      INCLUDE 'PRM_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
      INTEGER STATUS
*    Import :
      INTEGER NDIM
      INTEGER DIMS(NDIM)
*    Import/Export :
*    Export :
      INTEGER PTR
*-
      IF (STATUS.EQ.SAI__OK) THEN
*  check number of dimensions
        IF (NDIM.GT.0.AND.NDIM.LE.DAT__MXDIM) THEN
*  get pointer
          CALL DYN_MAP(NDIM,DIMS,VAL__NB<T>,PTR,STATUS)
        ELSE
          STATUS=SAI__ERROR
          CALL ERR_REP( ' ', 'Number of dimensions invalid', STATUS )
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from DYN_MAP<T>',STATUS)
        ENDIF
      ENDIF
      END
