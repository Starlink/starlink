      SUBROUTINE CAP_GROWS (STATUS)
*+
*  Name:
*     CAP_GROWS
*  Purpose:
*     Display the number of rows in the current buffer.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GROWS (STATUS)
*  Description:
*     Display the number of rows in the current buffer.
*  Arguments:
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the catalogue is open then
*       Determine the number of rows in the current selection.
*       Output the number of rows.
*     else
*       Report message: catalogue not open.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     17/10/94  (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'SGZ_PAR'           ! StarGaze parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! StarGaze common block.
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  SI,      ! Selection identifier.
     :  ROWS     ! Number of rows in the selection.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Determine the number of rows in the current selection.

            SI = SELID__SGZ(CSEL__SGZ)

            CALL CAT_TROWS (SI, ROWS, STATUS)

*
*          Output the number of rows.

            CALL MSG_SETI ('ROWS', ROWS)
            CALL CAP_OUT (GUI__SGZ, ' ', '^ROWS', STATUS)

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

      END IF

      END
