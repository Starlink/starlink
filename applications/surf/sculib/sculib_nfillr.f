*+  SCULIB_NFILLR - fill a real array with its indices
      SUBROUTINE SCULIB_NFILLR (N, ARRAY)
*    Description :
*     fills a real array with its indices
*    Invocation :
*     CALL SCULIB_NFILLR (N, ARRAY)
*    Parameters :
*     N            = INTEGER (Given)
*           number of array elements
*     ARRAY (N)    = REAL (Returned)
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
         ARRAY (I) = REAL (I)
      END DO

      END
