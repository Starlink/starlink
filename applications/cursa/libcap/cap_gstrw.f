      SUBROUTINE CAP_GSTRW (ROWNO, STATUS)
*+
*  Name:
*     CAP_GSTRW
*  Purpose:
*     Set the current row number in the current selection.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSTRW (ROWNO; STATUS)
*  Description:
*     Set the current row number in the current selection.
*  Arguments:
*     ROWNO  =  INTEGER (Given)
*        Required row number.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is a catalogue open then
*       Determine the number of rows in the current selection.
*       If the row number is in the range of the selection then
*         Set the row number.
*       else
*         Report a warning.
*       end if
*     else
*       display a warning: there is no catalogue open.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     2/6/94  (ACD): Original version.
*     27/9/94 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'SGZ_PAR'           ! StarGaze parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! StarGaze common block.
*  Arguments Given:
      INTEGER
     :  ROWNO
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Variables:
      INTEGER
     :  SI,      ! Identifier for the current selection.
     :  ROWS     ! Number of rows in the current selection.
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
*          Check whether the given row number lies in the range for
*          current selection.  If so then adopt it, otherwise report
*          a message (NOT an error).

            IF (ROWNO .GT. 0  .AND.  ROWNO .LE. ROWS) THEN
               CROW__SGZ = ROWNO

            ELSE
               CALL MSG_SETI ('ROWNO', ROWNO)
               CALL CAP_WARN (GUI__SGZ, ' ', 'The given row number, '/
     :           /'^ROWNO, is out of range for the current selection.',
     :           STATUS)

               CALL MSG_SETI ('ROWS', ROWS)
               CALL CAP_WARN (GUI__SGZ, ' ', 'The permitted range is '/
     :           /'1 - ^ROWS.', STATUS)

            END IF

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

      END IF

      END
