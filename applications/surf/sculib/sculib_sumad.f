*+  SCULIB_SUMAD - sum the elements of a double precision array
      SUBROUTINE SCULIB_SUMAD (NELM, ARRAY, SUM)
*    Description :
*     This routine adds up the NELM elements of the double precision ARRAY 
*     and puts the result in SUM.
*    Invocation :
*     CALL SCULIB_SUMAD (NELM, ARRAY, SUM)
*    Parameters :
*     NELM               = INTEGER (Given)
*           the number of elements in the array
*     ARRAY (NELM)       = DOUBLE PRECISION (Given)
*           the array whose elements are to be summed
*     SUM                = DOUBLE PRECISION (Returned)
*            the sum
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*    History :
*     $Id$
*     24-SEP-1993: Original version.
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER NELM
      DOUBLE PRECISION ARRAY (NELM)
*    Import-Export :
*    Export :
      DOUBLE PRECISION SUM
*    Status :
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER I
*    Internal References :
*    Local data :
*-

      SUM = 0.0D0

      DO I = 1, NELM
         SUM = SUM + ARRAY (I)
      END DO

      END
