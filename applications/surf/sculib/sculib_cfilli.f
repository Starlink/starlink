*+  SCULIB_CFILLI - fill an integer array with a constant
      SUBROUTINE SCULIB_CFILLI (N, IVAL, ARRAY)
*    Description :
*     fills an integer array with a constant
*    Invocation :
*     CALL SCULIB_CFILLI (N, IVAL, ARRAY)
*    Parameters :
*     N                  = INTEGER (Given)
*           number of array elements
*     IVAL               = INTEGER (Given)
*           constant to which array is to be set
*     ARRAY (N)          = INTEGER (Returned)
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
      INTEGER IVAL
*    Import-Export :
*    Export :
      INTEGER ARRAY (N)
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
         ARRAY (I) = IVAL
      END DO

      END
