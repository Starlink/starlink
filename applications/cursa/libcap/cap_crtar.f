      SUBROUTINE CAP_CRTAR (SIZE, TYPE, PTR, STATUS)
*+
*  Name:
*     CAP_CRTAR
*  Purpose:
*     Wrap-around to PSX routine for creating a dynamic array.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CRTAR (SIZE, TYPE; PTR; STATUS)
*  Description:
*     Wrap-around to PSX routine for creating a dynamic array.
*  Arguments:
*     SIZE  =  INTEGER (Given)
*        Size of the array (the number of elements).
*     TYPE  =  CHARACTER*(*) (Given)
*        The data type of the array.
*     PTR  =  INTEGER (Returned)
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
*     8/2/95 (ACD): Original version (based on CAT1_CRTAR).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard SAE constants.
*  Arguments Given:
      INTEGER
     :  SIZE
      CHARACTER
     :  TYPE*(*)
*  Arguments Returned:
      INTEGER
     :  PTR
*  Status:
      INTEGER STATUS             ! Global status.
*.

      IF (STATUS .EQ. SAI__OK) THEN

         CALL PSX_CALLOC (SIZE, TYPE, PTR, STATUS)

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETI ('SIZE', SIZE)
            CALL ERR_REP ('CAP_CRTAR_ERR', 'CAP_CRTAR: Failed to '/
     :        /'allocate ^SIZE elements of dynamic memory.', STATUS)
         END IF

      END IF

      END
