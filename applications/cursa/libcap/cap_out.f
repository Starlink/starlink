      SUBROUTINE CAP_OUT (BAR, PARAM, TEXT, STATUS)
*+
*  Name:
*     CAP_OUT
*  Purpose:
*     Output a line of text to the standard output device.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_OUT (BAR, PARAM, TEXT; STATUS)
*  Description:
*     Output a line of text to the standard output device.
*
*     This routine is a wrap-around for MSG_OUT.  It differs from
*     MSG_OUT in that it optionally allows a vertical bar ('|') to
*     be inserted at the start of every line.  This apparently bizarre
*     behaviour is required by the application catview when it is
*     communicating with the tcl/tk GUI.
*  Arguments:
*     BAR  =  LOGICAL (Given)
*        Logical flag indicating whether a vertical bar ('|') is to be
*        inserted at the start of each line.  It is coded as follows:
*        .TRUE.  -  insert the bar,
*        .FALSE. -  do not insert the bar.
*     PARAM  =  CHARACTER*(*) (Given)
*        Name of the ADAM parameter to which the output is to be sent.
*     TEXT  =  CHARACTER*(*) (Given)
*        Text to be output.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the bar is required then
*       Insert a bar at the start of the line.
*     end if
*     Append the given text to the line.
*     Output the line.
*  Implementation Deficiencies:
*     The line of text is output with Fortran WRITE statements rather
*     than MSG_OUT (see above).
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     19/9/94 (ACD): Original version.
*     28/2/96 (ACD): Modified to use Fortran WRITE statements rather
*       than MSG_OUT in order to allow lines longer than 80 characters
*       to be output.
*     4/12/96 (ACD): Made the length of the output buffer a global
*       parameteric constant rather than a hard-wired value, which
*       resolved an inconsistency with other routines.  Also a kludge
*       to turn off token expansion and truncation in long lines.
*     1/7/99  (ACD): Reinstated output via the ADAM message system.
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
     :  BAR
      CHARACTER
     :  PARAM*(*),
     :  TEXT*(*)
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER IOOK          ! Success status for Fortran I/O.
      PARAMETER (IOOK = 0)
*  Local Variables:
      CHARACTER
     :  BUFFER*(SGZ__SZOMS) ! Output buffer.
      INTEGER
     :  BUFLEN,     ! Length of BUFFER (excl. trail. blanks).
     :  LTEXT       !   "    "  TEXT   ( "  .   "  .   "   ).
*.

      IF (STATUS .EQ. SAI__OK) THEN

         BUFFER = ' '
         BUFLEN = 0

*
*       If required then start the line with a bar.

         IF (BAR) THEN
            CALL CHR_PUTC ('|', BUFFER, BUFLEN)
         END IF

*
*       Append the given text to the output line.

         IF (TEXT .NE. ' ') THEN
            LTEXT = CHR_LEN(TEXT)
            CALL CHR_PUTC (TEXT(1 : LTEXT), BUFFER, BUFLEN)
         END IF

*
*       Output the line.

C        print3000, buflen, buffer(1 : buflen)
C3000    format(1x, i3, ': >' a)

         IF (BUFLEN .GT. 0) THEN
            CALL MSG_OUT (PARAM, BUFFER(1 : BUFLEN), STATUS)
         ELSE
            CALL MSG_OUT (PARAM, ' ', STATUS)
         END IF

      END IF

      END
