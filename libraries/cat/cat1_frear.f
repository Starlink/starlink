      SUBROUTINE CAT1_FREAR (PTR, STATUS)
*+
*  Name:
*     CAT1_FREAR
*  Purpose:
*     Wrap-around to PSX routine for releasing a dynamic array.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_FREAR (PTR; STATUS)
*  Description:
*     Wrap-around to PSX routine for releasing a dynamic array.
*  Arguments:
*     PTR  =  INTEGER (Given)
*        Pointer to the array.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Call the appropriate PSX routine.
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
      INCLUDE 'CAT_PAR'          ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  PTR
*  Status:
      INTEGER STATUS             ! Global status.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         CALL PSX_FREE (PTR, STATUS)

      END IF

      END
