*+  SCULIB_ADDCAR - add a constant to a double array
      SUBROUTINE SCULIB_ADDCAD (N, IN, DVAL, OUT)
*    Description :
*     adds a real constant to a double array
*    Invocation :
*     CALL SCULIB_ADDCAD (N, IN, RVAL, OUT)
*    Parameters :
*     N              = INTEGER (Given)
*           number of elements in arrays
*     IN (N)         = DOUBLE PRECISION (Given)
*           input array
*     RVAL           = DOUBLE PRECISION (Given)
*           real constant to be added to array
*     OUT (N)        = DOUBLE PRECISION (Returned)
*           output array (may be same as input)
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*    History :
*     $Id$
*     25.11.1992: Original version
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
*    Import :
      INTEGER N
      DOUBLE PRECISION IN (N)
      DOUBLE PRECISION DVAL
*    Import-Export :
*    Export :
      DOUBLE PRECISION OUT (N)
*    Status :
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER I
*    Internal References :
*    Local data :
*-

      DO I = 1, N
         OUT (I) = IN (I) + DVAL
      END DO

      END
