      SUBROUTINE CAP_PRERR (LINE, MESSGE, STATUS)
*+
*  Name:
*     CAP_PRERR
*  Purpose:
*     Report an error parsing a line in the graphics translation file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PRERR (LINE, MESSGE; STATUS)
*  Description:
*     Report an error parsing a line in the graphics translation file.
*
*     Note that these parser errors are reported as ADAM messages, not
*     ADAM errors.
*  Arguments:
*     LINE  =  INTEGER (Given)
*        The number of the line where the error occurred.
*     MESSGE  =  CHARACTER*(*) (Given)
*        The text describing the error.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Assemble the message.
*     Report the message.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     11/8/96 (ACD): Original version.
*     6/6/97  (ACD): Changed the subroutine prefix from CIO to CAP.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      INTEGER
     :  LINE
      CHARACTER
     :  MESSGE*(*)
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  ERRTXT*75  ! Full text of the parser error message.
      INTEGER
     :  MSGLEN,    ! Length of MESSGE (excl. trail. blanks).
     :  ERRLEN     !   "    "  ERRTXT ( "  .   "  .   "   ).
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Assemble the message.

         ERRLEN = 0
         ERRTXT = ' '

         CALL CHR_PUTC ('Error in line ^LINE: ', ERRTXT, ERRLEN)

         IF (MESSGE .NE. ' ') THEN
            MSGLEN = CHR_LEN(MESSGE)
            CALL CHR_PUTC (MESSGE(1 : MSGLEN), ERRTXT, ERRLEN)
         ELSE
            CALL CHR_PUTC ('<no text supplied.>', ERRTXT, ERRLEN)
         END IF

*
*       Report the message.

         CALL MSG_SETI ('LINE', LINE)
         CALL MSG_OUT (' ', ERRTXT(1 : ERRLEN), STATUS)
      END IF

      END
