*+  SCULIB_COPYI - copy one integer array to another
      SUBROUTINE SCULIB_COPYI (N, FROM, TO)
*    Description :
*     copies one integer array into another
*    Invocation :
*     CALL SCULIB_COPYI (N, FROM, TO)
*    Parameters :
*     N          = INTEGER (Given)
*           number of integers in arrays
*     FROM (N)   = INTEGER (Given)
*           array copied from
*     TO (N)     = INTEGER (Returned)
*           array copied to
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
      INTEGER FROM (N)
*    Import-Export :
*    Export :
      INTEGER TO (N)
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
         TO (I) = FROM (I)
      END DO

      END
