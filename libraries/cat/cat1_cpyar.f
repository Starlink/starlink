      SUBROUTINE CAT1_CPYAR (SIZE, INARR, OUTARR, STATUS)
*+
*  Name:
*     CAT1_CPYAR
*  Purpose:
*     Copy the contents of an INTEGER array into a second array.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_CPYAR (SIZE, INARR; OUTARR; STATUS)
*  Description:
*     Copy the contents of an INTEGER array into a second array.
*  Arguments:
*     SIZE  =  INTEGER (Given)
*        Array size.
*     INARR(SIZE)  =  INTEGER (Given)
*        Input array.
*     OUTARR(SIZE)  =  INTEGER (Returned)
*        Output array.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Copy the array.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     14/4/94 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  SIZE,
     :  INARR(SIZE)
*  Arguments Returned:
      INTEGER
     :  OUTARR(SIZE)
*  Status:
      INTEGER STATUS  ! Global status.
*  Local Variables:
      INTEGER
     :  LOOP          ! Loop index.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         DO LOOP = 1, SIZE
            OUTARR(LOOP) = INARR(LOOP)
         END DO

      END IF

      END
