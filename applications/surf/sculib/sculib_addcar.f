*+  SCULIB_ADDCAR - add a constant to a real array
      SUBROUTINE SCULIB_ADDCAR (N, IN, RVAL, OUT)
*    Description :
*     adds a real constant to a real array
*    Invocation :
*     CALL SCULIB_ADDCAR (N, IN, RVAL, OUT)
*    Parameters :
*     N              = INTEGER (Given)
*           number of elements in arrays
*     IN (N)         = REAL (Given)
*           input array
*     RVAL           = REAL (Given)
*           real constant to be added to array
*     OUT (N)        = REAL (Returned)
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
         OUT (I) = IN (I) + RVAL
      END DO

      END
