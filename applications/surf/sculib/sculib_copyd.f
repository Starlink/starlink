*+  SCULIB_COPYD - copy one double precision array to another
      SUBROUTINE SCULIB_COPYD (N, FROM, TO)
*    Description :
*     copies one double precision array into another
*    Invocation :
*     CALL SCULIB_COPYD (N, FROM, TO)
*    Parameters :
*     N          = INTEGER (Given)
*           number of elements in arrays
*     FROM (N)   = DOUBLE PRECISION (Given)
*           array copied from
*     TO (N)     = DOUBLE PRECISION (Returned)
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
      DOUBLE PRECISION FROM (N)
*    Import-Export :
*    Export :
      DOUBLE PRECISION TO (N)
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
