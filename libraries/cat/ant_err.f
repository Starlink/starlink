      SUBROUTINE ANT_ERR (MESSGE)
*+
*  Name:
*     ANT_ERR
*  Purpose:
*     Report an error message from the parser.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL ANT_ERR (MESSGE)
*  Description:
*     Report an error message from the parser.
*
*     The parser knows nothing of the CAT or ADAM status, so the error
*     is reported using MSG_OUT and a local status.  If MSG_OUT fails
*     then an error is reported with ERR_REP.
*  Arguments:
*     MESSGE  =  CHARACTER*(*) (Given)
*        Error message to be reported.
*  Algorithm:
*     Assemble the text string and report the message.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     13/8/99 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      CHARACTER
     :  MESSGE*(*)
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  MSGLEN,    ! length of MESSGE (excl. trail. blanks).
     :  BUFPOS,    !   "    "  BUFFER ( "  .   "  .   "   ).
     :  STATUS     ! Local CAT / ADAM status.
      CHARACTER
     :  BUFFER*80  ! Output buffer.
*.

*
*    Assemble the text string containing the error report.

      BUFFER = ' '
      BUFPOS = 0

      CALL CHR_PUTC ('!(Parser) ', BUFFER, BUFPOS)

      IF (MESSGE .NE. ' ') THEN
         MSGLEN = CHR_LEN(MESSGE)
         CALL CHR_PUTC (MESSGE(1 : MSGLEN), BUFFER, BUFPOS)
      ELSE
         CALL CHR_PUTC ('<No text available>', BUFFER, BUFPOS)
      END IF

*
*    Report the message.
*
*    Code for reporting the message via both the ADAM message
*    system and a Fortran PRINT statement is included below.  Comment
*    out whichever is not required.

      STATUS = CAT__OK
      CALL MSG_OUT (' ', BUFFER(1 : BUFPOS), STATUS)

      IF (STATUS .NE. CAT__OK) THEN
         CALL ERR_REP (' ', 'Failure reporting parser error.',
     :     STATUS)
      END IF

C     PRINT2000, BUFFER(1 : BUFPOS)
C2000 FORMAT(1X, A / )

      END
