*+  SCULIB_COPYR - copy one real array to another
      SUBROUTINE SCULIB_COPYR (N, FROM, TO)
*    Description :
*     copies one real array into another
*    Invocation :
*     CALL SCULIB_COPYR (N, FROM, TO)
*    Parameters :
*     N          = INTEGER (Given)
*           number of reals in arrays
*     FROM (N)   = REAL (Given)
*           array copied from
*     TO (N)     = REAL (Returned)
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
      REAL FROM (N)
*    Import-Export :
*    Export :
      REAL TO (N)
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
