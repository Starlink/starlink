*+  SCULIB_NDF_OUT - write an array out to an NDF
      SUBROUTINE SCULIB_NDF_OUT (NAME, ARRAY, NDIM, DIMS, STATUS)
*    Description :
*    Invocation :
*    Parameters :
*     NAME                   = CHARACTER*(*) (Given)
*           name of NDF to be created
*     ARRAY                  = REAL (Given)
*           array to be stored
*     NDIM                   = INTEGER (Given)
*           number of dimensions in array
*     DIMS (NDIM)            = INTEGER (Given)
*           dimensions of array
*     STATUS                 = INTEGER (Given and returned)
*           global status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*    History :
*     $Id$
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) NAME
      REAL ARRAY (1)
      INTEGER NDIM
      INTEGER DIMS (NDIM)
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER I
      INTEGER PLACE
      INTEGER LBND (10), UBND (10)
      INTEGER INDF
      INTEGER D_PTR
      INTEGER NELM
      CHARACTER*(DAT__SZLOC) LOC
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

*  create HDS file and NDF

      CALL HDS_NEW (NAME, NAME, 'NDF', 0, 0, LOC, STATUS)
      CALL NDF_BEGIN
      CALL NDF_PLACE (LOC, NAME, PLACE, STATUS)
      DO I = 1, NDIM
         LBND (I) = 1
         UBND (I) = DIMS (I)
      END DO
      CALL NDF_NEW ('_REAL', NDIM, LBND, UBND, PLACE, INDF, STATUS)

*  map the data array and copy the input array into it

      CALL NDF_MAP (INDF, 'DATA', '_REAL', 'WRITE', D_PTR, NELM,
     :   STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_COPYR (NELM, ARRAY, %val(D_PTR))
      END IF

*  close the NDF context and the HDS file

      CALL NDF_END (STATUS)
      CALL HDS_CLOSE (LOC, STATUS)

      END
