*+  SCULIB_CFILLR - fill a real array with a constant
      SUBROUTINE SCULIB_CFILLR (N, RVAL, ARRAY)
*    Description :
*     fills a real array with a constant
*    Invocation :
*     CALL SCULIB_CFILLR (N, RVAL, ARRAY)
*    Parameters :
*     N                  = INTEGER (Given)
*           number of array elements
*     RVAL               = REAL (Given)
*           constant to which array is to be set
*     ARRAY (N)          = REAL (Returned)
*           array to be set
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
      REAL RVAL
*    Import-Export :
*    Export :
      REAL ARRAY (N)
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
         ARRAY (I) = RVAL
      END DO

      END
