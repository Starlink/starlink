      SUBROUTINE CAP_GTSTR (PARAM, STRING, STATUS)
*+
*  Name:
*     CAP_GTSTR
*  Purpose:
*     Get a string from the user.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL (PARAM; STRING; STATUS)
*  Description:
*     Get a string from the user.
*
*     A long string may be input by splitting the text across several
*     lines.  Continuation is indicated by including a continuation
*     character ('@') at the end of the current line.  If there is
*     no continuation character the current line is interpretted as the
*     last.
*  Arguments:
*     PARAM  =  CHARACTER*(*) (Given)
*        Name of the ADAM parameter from which the value is to be
*        obtained.
*     STRING  =  CHARACTER*(*) (Returned)
*        The returned string.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the size of the return string.
*     Initialise the return string.
*     Do while (more input is required)
*       Get a line of input.
*       If the line is not blank then
*         Determine its length
*         If the last character is a continuation character then
*           Remove the continuation character.
*         else
*           Set the termination flag.
*         end if
*       else
*         Set the termination flag.
*       end if
*       If the line is still not blank then
*         Re-determine its length
*         If there is space in the return string then
*           Append the current line to the return string.
*         else
*           Set the termination flag.
*           Set the status.
*           Report an error.
*         end if
*       end if
*       If the status is not ok then
*         Set the termination flag.
*       end if
*     end do
*  Implementation Deficiencies:
*     <...>
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     19/12/95 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
C      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
C      INCLUDE 'CAT_ERR'           ! CAT error codes.
C      INCLUDE 'SGZ_PAR'           ! StarGaze parametric constants.
*  Global Variables:
C      INCLUDE 'SGZ_CMN'           ! StarGaze common block.
*  Arguments Given:
      CHARACTER
     :  PARAM*(*)
*  Arguments Returned:
      CHARACTER
     :  STRING*(*)
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER
     :  CHR_SIZE,
     :  CHR_LEN
*  Local Constants:
      CHARACTER CONTIN*1       ! Continuation character.
      PARAMETER (CONTIN = '@')
*  Local Variables:
      CHARACTER
     :  BUFFER*100   ! Buffer for the current line.
      INTEGER
     :  STRSIZ,      ! Declared size of STRING.
     :  STRPOS,      ! Current position in STRING.
     :  BUFLEN       ! Length of BUFFER (excl. trail. blanks).
      LOGICAL
     :  MORE         ! Flag; more lines to be read?
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Determine the size of the return string.

         STRSIZ = CHR_SIZE(STRING)

*
*       Loop reading lines from the input parameter and assembling the
*       return string.

         STRING = ' '
         STRPOS = 0

         MORE = .TRUE.

         DO WHILE (MORE)

*
*          Obtain a line of input.

            CALL PAR_GET0C (PARAM, BUFFER, STATUS)
            CALL PAR_CANCL (PARAM, STATUS)

*
*          Check that the line is not blank, and if not then either
*          remove the continuation character or set the termination
*          flag (if the line does not end in a continuation character
*          it must be the last line).  If the line is blank then also
*          set the termination flag.

            IF (BUFFER .NE. ' ') THEN
               BUFLEN = CHR_LEN(BUFFER)

               IF (BUFFER(BUFLEN : BUFLEN) .EQ. CONTIN) THEN
                  BUFFER(BUFLEN : BUFLEN) = ' '
               ELSE
                  MORE = .FALSE.
               END IF
            ELSE
               MORE = .FALSE.
            END IF

*
*          If the line is still not blank then either append it to the
*          return string if there is room, or (if there is insufficient
*          room) set the termination flag, set the status and report an
*          error.

            IF (BUFFER .NE. ' ') THEN
               BUFLEN = CHR_LEN(BUFFER)

               IF (BUFLEN+STRPOS .LT. STRSIZ) THEN
                  CALL CHR_PUTC (BUFFER(1 : BUFLEN), STRING, STRPOS)

               ELSE
                  MORE = .FALSE.
                  STATUS = SAI__ERROR

                  CALL MSG_SETI ('STRSIZ', STRSIZ)

                  CALL ERR_REP ('CAP_GTSTR_ERR', 'Input is too long '/
     :              /'(maximum ^STRSIZ characters).', STATUS)
               END IF
            END IF

*
*          Ensure that the termination flag is set if the status is
*          not ok.

            IF (STATUS .NE. SAI__OK) THEN
               MORE = .FALSE.
            END IF
         END DO

      END IF

      END
