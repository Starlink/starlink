*+  SCULIB_ADDCAI - add a constant to an integer array
      SUBROUTINE SCULIB_ADDCAI (N, IN, IVAL, OUT)
*    Description :
*     adds an integer constant to an integer array
*    Invocation :
*     CALL SCULIB_ADDCAI (N, IN, IVAL, OUT)
*    Parameters :
*     N              = INTEGER (Given)
*           number of elements in arrays
*     IN (N)         = INTEGER (Given)
*           input array
*     IVAL           = INTEGER (Given)
*           constant to be added to array
*     OUT (N)        = INTEGER (Returned)
*           output array (may be same as input)
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*    History :
*     $Id$
*     9-JUN-1993: Original version
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
*    Import :
      INTEGER N
      INTEGER IN (N)
      INTEGER IVAL
*    Import-Export :
*    Export :
      INTEGER OUT (N)
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
         OUT (I) = IN (I) + IVAL
      END DO

      END
