*+  HRESHAPE - changes dimensions of an HDS data object
      SUBROUTINE HRESHAPE(STATUS)

*    Description :

*    Parameters :

*     INP  = CHAR  - the name of the object
*     DIMS   = INTEGER - new dimensions
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='HRESHAPE Version 1.8-0')
*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC	! locator to object
      INTEGER NDIM,DIMSO(DAT__MXDIM)	! old dimensions
      INTEGER NVAL			! number of values read
      INTEGER DIMSN(DAT__MXDIM)		! new dimensions

*-
      CALL MSG_PRNT(VERSION)

      CALL AST_INIT()

      CALL USI_DASSOC('INP','UPDATE',LOC,STATUS)

*  get existing shape of object
      CALL DAT_SHAPE(LOC,DAT__MXDIM,DIMSO,NDIM,STATUS)

*  check for scalar
      IF (NDIM.EQ.0.AND.STATUS.EQ.SAI__OK) THEN
        CALL MSG_PRNT('! cannot change shape of scalar object')
        STATUS=SAI__ERROR
      ELSEIF (STATUS.EQ.SAI__OK) THEN
*  get new dimensions
        CALL USI_GET1I('DIMS',DAT__MXDIM,DIMSN,NVAL,STATUS)
*  check for correct number of values
        IF (NVAL.NE.NDIM) THEN
          CALL MSG_PRNT('! number of dimensions cannot be changed')
          STATUS=SAI__ERROR
        ELSEIF (STATUS.EQ.SAI__OK) THEN
*  if OK then do change
          IF (NDIM.EQ.1) THEN
            CALL DAT_ALTER(LOC,1,DIMSN(1),STATUS)
          ELSE
            CALL DAT_MOULD(LOC,NDIM,DIMSN,STATUS)
          ENDIF
        ENDIF
      ENDIF

      CALL DAT_ANNUL(LOC,STATUS)

      CALL AST_CLOSE()
      CALL AST_ERR(STATUS)

      END

