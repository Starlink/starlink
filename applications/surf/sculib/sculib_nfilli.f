*+  SCULIB_NFILLI - fill an integer array with its indices
      SUBROUTINE SCULIB_NFILLI (N, ARRAY)
*    Description :
*     fills an integer array with its indices
*    Invocation :
*     CALL SCULIB_NFILLI (N, ARRAY)
*    Parameters :
*     N            = INTEGER (Given)
*           number of array elements
*     ARRAY (N)    = INTEGER (Returned)
*           array to be filled
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*    History :
*     $Id$
*     25.11.1992: Original version.
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
*    Import :
      INTEGER N
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
         ARRAY (I) = I
      END DO

      END
