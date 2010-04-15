      SUBROUTINE CAP_GCSEL (SELNO, STATUS)
*+
*  Name:
*     CAP_GCSEL
*  Purpose:
*     Choose an existing selection as the current selection.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GCSEL (SELNO; STATUS)
*  Description:
*     Choose an existing selection as the current selection.
*  Arguments:
*     SELNO  =  INTEGER (Given)
*        Number of the required selection.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the catalogue is open then
*       If the selection lies in range then
*         Set it as the current selection.
*         Set the current row to the first row in the selection.
*       else
*         Display warning.
*       end if
*     else
*       Display warning.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     28/4/94 (ACD): Original version.
*     27/9/94 (ACD): First stable version.
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
*  Arguments Given:
      INTEGER
     :  SELNO
*  Status:
      INTEGER STATUS             ! Global status
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Check that the given selection lies in the range of the
*          existing selections.  If so then adopt it and set the current
*          row to the first row in the selection; otherwise report a
*          message.

            IF (SELNO .GT. 0  .AND.  SELNO .LE. SELS__SGZ) THEN
               CSEL__SGZ = SELNO
               CROW__SGZ   = 1

            ELSE
               CALL CAP_WARN (GUI__SGZ, ' ', 'Selection not set; '/
     :           /'there is no selection with this number.', STATUS)

               CALL MSG_SETI ('SELS', SELS__SGZ)
               CALL CAP_WARN (GUI__SGZ, ' ', 'The permitted range is '/
     :           /'1 to ^SELS.', STATUS)
            END IF

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

      END IF

      END
