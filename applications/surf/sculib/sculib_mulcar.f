*+  SCULIB_MULCAR - multiply real array by a constant
      SUBROUTINE SCULIB_MULCAR (N, IN, RVAL, OUT)
*    Description :
*     multiplies a real array by a real constant
*    Invocation :
*     CALL SCULIB_MULCAR (N, IN, RVAL, OUT)
*    Parameters :
*     N              = INTEGER (Given)
*           number of array elements
*     IN (N)         = REAL (Given)
*           array to be multiplied
*     RVAL           = REAL (Given)
*           multiplication factor
*     OUT (N)        = REAL (Returned)
*           output array (can be same as input)
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
      REAL IN (N)
      REAL RVAL
*    Import-Export :
*    Export :
      REAL OUT (N)
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
         OUT (I) = IN (I) * RVAL
      END DO

      END
