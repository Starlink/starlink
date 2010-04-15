      SUBROUTINE CAP_GPREV (STATUS)
*+
*  Name:
*     CAP_GPREV
*  Purpose:
*     List the previous page in the current selection to the screen.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GPREV (STATUS)
*  Description:
*     List the previous page in the current selection to the screen,
*     showing the currently chosen components.
*  Arguments:
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the catalogue is open then
*       If all the rows are not being listed then
*         Compute the new first page.
*       end if
*       List the page.
*     else
*       Report message: catalogue not open.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     18/10/94 (ACD): Original version.
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
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          If all the rows are not to be listed then compute the new
*          first row.

            IF (NLIST__SGZ  .NE.  SGZ__LALL) THEN
               CROW__SGZ = CROW__SGZ + 2 - (NLIST__SGZ * 2)
               CROW__SGZ = MAX(CROW__SGZ, 1)
            END IF

*
*          List the page.

            CALL CAP_GLIST (STATUS)

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

      END IF

      END
