*+  SCULIB_COPYI - copy one byte array to another
      SUBROUTINE SCULIB_COPYB (N, FROM, TO)
*    Description :
*     copies one integer array into another
*    Invocation :
*     CALL SCULIB_COPYB (N, FROM, TO)
*    Parameters :
*     N          = INTEGER (Given)
*           number of integers in arrays
*     FROM (N)   = BYTE (Given)
*           array copied from
*     TO (N)     = BYTE (Returned)
*           array copied to
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*     T. Jenness  (timj@jach.hawaii.edu)
*    History :
*     $Id$
*     25.11.1992: Original version
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
*    Import :
      INTEGER N
      BYTE FROM (N)
*    Import-Export :
*    Export :
      BYTE TO (N)
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
