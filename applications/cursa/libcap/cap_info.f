      SUBROUTINE CAP_INFO (EXCL, PARAM, TEXT, STATUS)
*+
*  Name:
*     CAP_INFO
*  Purpose:
*     Output an information message.
*  Language:
*     Fortran 77.
*  Invocation:
*     CAP_INFO (EXCL, PARAM, TEXT; STATUS)
*  Description:
*     Output an information message.
*
*     This routine is a wrap-around for MSG_OUT.  It differs from
*     MSG_OUT in that it optionally allows an exclamation mark ('!') to
*     be inserted at the start of every line.  This apparently bizarre
*     behaviour is required by the application catview when it is
*     communicating with the tcl/tk GUI.
*  Arguments:
*     EXCL  =  LOGICAL (Given)
*        Logical flag indicating whether an exclamation mark ('!') is to be
*        inserted at the start of each line.  It is coded as follows:
*        .TRUE.  -  insert the exclamation mark.
*        .FALSE. -  do not insert the exclamation mark.
*     PARAM  =  CHARACTER*(*) (Given)
*        Name of the ADAM parameter to which the output is to be sent.
*     TEXT  =  CHARACTER*(*) (Given)
*        Text to be output.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the exclamation mark is required then
*       Insert an exclamation mark at the start of the line.
*     end if
*     Append the given text to the line.
*     Output the line.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     23/10/94 (ACD): Original version (based on CAP_WARN).
*     1/7/99   (ACD): Explicitly defined the length of the output line
*       to correspond to the ADAM message system.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants.
      INCLUDE 'SGZ_PAR'     ! catview parametric constants.
*  Arguments Given:
      LOGICAL
     :  EXCL
      CHARACTER
     :  PARAM*(*),
     :  TEXT*(*)
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  BUFFER*(SGZ__SZOMS) ! Output buffer.
      INTEGER
     :  BUFLEN,     ! Length of BUFFER (excl. trail. blanks).
     :  LTEXT       !   "    "  TEXT   ( "  .   "  .   "   ).
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       If required then start the line with an exclamation mark.

         BUFFER = ' '
         BUFLEN = 0

         IF (EXCL) THEN
            CALL CHR_PUTC ('!', BUFFER, BUFLEN)
         END IF

*
*       Introduce the text as an information message.

         CALL CHR_PUTC ('(Info.) ', BUFFER, BUFLEN)

*
*       Append the given text to the output line.

         IF (TEXT .NE. ' ') THEN
            LTEXT = CHR_LEN(TEXT)
            CALL CHR_PUTC (TEXT(1 : LTEXT), BUFFER, BUFLEN)
         END IF

*
*       Output the line.

         IF (BUFLEN .GT. 0) THEN
            CALL MSG_OUT (PARAM, BUFFER(1 : BUFLEN), STATUS)
         ELSE
            CALL MSG_OUT (PARAM, ' ', STATUS)
         END IF

      END IF

      END
