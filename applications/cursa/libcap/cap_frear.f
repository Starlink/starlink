      SUBROUTINE CAP_FREAR (PTR, STATUS)
*+
*  Name:
*     CAP_FREAR
*  Purpose:
*     Wrap-around to PSX routine for releasing a dynamic array.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_FREAR (PTR; STATUS)
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     14/6/96 (ACD): Original version (based on CAT1_FREAR).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
*  Arguments Given:
      INTEGER
     :  PTR
*  Status:
      INTEGER STATUS             ! Global status.
*.

      IF (STATUS .EQ. SAI__OK) THEN

         CALL PSX_FREE (PTR, STATUS)

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAP_FREAR_ERR', 'CAP_FREAR: Failed to '/
     :        /'free dynamic memory.', STATUS)
         END IF

      END IF

      END
