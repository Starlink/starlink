*+  SCULIB_CFILLD - fill a double precision array with a constant
      SUBROUTINE SCULIB_CFILLD (N, DVAL, ARRAY)
*    Description :
*     fills a double precision array with a constant
*    Invocation :
*     CALL SCULIB_CFILLD (N, DVAL, ARRAY)
*    Parameters :
*     N                  = INTEGER (Given)
*           number of array elements
*     DVAL               = DOUBLE PRECISION (Given)
*           constant to which array is to be set
*     ARRAY (N)          = DOUBLE PRECISION (Returned)
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
      DOUBLE PRECISION DVAL
*    Import-Export :
*    Export :
      DOUBLE PRECISION ARRAY (N)
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
         ARRAY (I) = DVAL
      END DO

      END
