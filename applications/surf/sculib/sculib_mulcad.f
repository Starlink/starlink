      SUBROUTINE SCULIB_MULCAD (N, IN, DVAL, OUT, STATUS)
*+  
*  Name:
*     SCULIB_MULCAR

*  Purpose:
*     multiply double precision array by a constant double
*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     SCULIB subroutine
 
*  Description:
*     multiplies a double precision array by a double constant

*  Invocation:
*     CALL SCULIB_MULCAD (N, IN, DVAL, OUT, STATUS)

*  Arguments:
*     N              = INTEGER (Given)
*           number of array elements
*     IN (N)         = DOUBLE (Given)
*           array to be multiplied
*     DVAL           = DOUBLE (Given)
*           multiplication factor
*     OUT (N)        = DOUBLE (Returned)
*           output array (can be same as input)
*     STATUS         = INTEGER (Given & Returned)
*           Global status

*  Method:

*  Deficiencies:

*  Bugs:

*  Authors:
*     T.Jenness   (JACH)

*  History:
*     $Log$
*     Revision 1.1  1998/01/14 02:34:10  timj
*     Initial revision
*

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'PRM_PAR'
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION IN (N)
      DOUBLE PRECISION DVAL

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION OUT (N)

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      DO I = 1, N
         IF (IN(I) .NE. VAL__BADD) THEN
            OUT (I) = IN (I) * DVAL
         ELSE
            OUT(I) = IN(I)
         END IF
      END DO

      END
