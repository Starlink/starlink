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
*     T.Jenness   (JACH)
*    History :
*     $Id$
*     25.11.1992: Original version
*     05.12.1996: Check for bad values
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'PRM_PAR'
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
         IF (IN(I) .NE. VAL__BADR) THEN
            OUT (I) = IN (I) * RVAL
         ELSE
            OUT(I) = IN(I)
         END IF
      END DO

      END
