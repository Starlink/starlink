*+  HDX_SAMESHAPE - returns .TRUE. if objects are same shape
      LOGICAL FUNCTION HDX_SAMESHAPE(ALOC,BLOC)

*    Description :
*     Returns a true value if two objects are of same shape.  .FALSE.
*     returned if either locator invalid.
*    Author :
*             (BHVAD::RJV)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC) ALOC,BLOC  ! locators to data objects
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      LOGICAL AVALID,BVALID             ! whether locators valid
      LOGICAL SAME                      ! temporary value of function
      INTEGER NADIMS,ADIMS(DAT__MXDIM)  ! dimensionality and dimensions
      INTEGER NBDIMS,BDIMS(DAT__MXDIM)  ! of two data objects being compared
      INTEGER IDIM                      ! index to dimensions
*    Internal References :
*    Local data :
*-
      STATUS=SAI__OK
      SAME=.FALSE.
*  check validity of locators
      CALL DAT_VALID(ALOC,AVALID,STATUS)
      CALL DAT_VALID(BLOC,BVALID,STATUS)
      IF (AVALID.AND.BVALID) THEN
*  get dimensions of two objects
        CALL DAT_SHAPE(ALOC,DAT__MXDIM,ADIMS,NADIMS,STATUS)
        CALL DAT_SHAPE(BLOC,DAT__MXDIM,BDIMS,NBDIMS,STATUS)

*  must be same number of dimensions
        SAME=(NADIMS.EQ.NBDIMS)
        IDIM=1

*  each dimension must be same size
        DO WHILE (SAME.AND.IDIM.LE.NADIMS)
          SAME=(ADIMS(IDIM).EQ.BDIMS(IDIM))
          IDIM=IDIM+1
        ENDDO
      ENDIF
      HDX_SAMESHAPE=SAME
      END
