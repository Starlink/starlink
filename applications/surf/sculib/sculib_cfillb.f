*+  SCULIB_CFILLI - fill a byte array with a constant
      SUBROUTINE SCULIB_CFILLB (N, IVAL, ARRAY)
*    Description :
*     fills an byte array with a constant
*    Invocation :
*     CALL SCULIB_CFILLB (N, IVAL, ARRAY)
*    Parameters :
*     N                  = INTEGER (Given)
*           number of array elements
*     IVAL               = BYTE (Given)
*           constant to which array is to be set
*     ARRAY (N)          = BYTE (Returned)
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
      BYTE IVAL
*    Import-Export :
*    Export :
      BYTE ARRAY (N)
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
