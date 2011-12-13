      SUBROUTINE CAT1_ERREP (PARAM, MESSGE, STATUS)
*+
*  Name:
*     CAT1_ERREP
*  Purpose:
*     Report an error message.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ERREP (PARAM, MESSGE; STATUS)
*  Description:
*     Report an error message.
*
*     Note that this routine attempts to execute irrespective of the
*     status on input.
*  Arguments:
*     PARAM  =  CHARACTER*(*) (Given)
*        Name of the external parameter with which the error is
*        associated.
*     MESSGE  =  CHARACTER*(*) (Given)
*        Message to be reported with the error.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Construct the local version of the error message and determine
*     its length.
*     Translate the CAT status into a message and determine its
*     length.
*     Construct the error message based on the translation of the error
*     code.
*     Construct the parameter name for the second message.
*     Report the error messages.
*
*     Notes:
*
*     (1) This routine contains code for reporting the error via both
*         the ADAM error system and a simple Fortran PRINT statement.
*         Whichever is not required should be commented out.
*
*     (2) The translation and reporting of the CAT status has been
*         commented out because this facility is now provided by the
*         ADAM error system.  However, the code is retained (as
*         comments) so that it can be reinstated if CAT was ever
*         used without ADAM.  The revised routine is little more than
*         a wrap-around for ERR_REP.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     28/6/93 (ACD): Original version.
*     25/1/94 (ACD): Re-written: the previous version had been a kludge.
*        It was replaced with a fully functional version.
*     27/5/98 (ACD): Commented out the translation and reporting of the
*        CAT status.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      CHARACTER
     :  PARAM*(*),
     :  MESSGE*(*)
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  OUTBUF*75   ! Buffer for error message.
C    :  ERRTXT*75,  ! Buffer for text translation of error code.
C    :  ERRMSG*75,  ! Buffer holding error message from error code.
C    :  PARA1*20    ! Name of parameter for second message.
      INTEGER
     :  OUTLEN   ! length of OUTBUF (excl. trail. blanks).
C    :  ERRLEN,  !   "    "  ERRTXT ( "  .   "  .   "   ).
C    :  MSGLEN,  !   "    "  ERRMSG ( "  .   "  .   "   ).
C    :  LPARAM,  !   "    "  PARAM  ( "  .   "  .   "   ).
C    :  LPARA1   !   "    "  PARA1  ( "  .   "  .   "   ).
*.

*
*    Construct a local version of the error message and determine its
*    length.

      IF (MESSGE .NE. ' ') THEN
         OUTBUF = MESSGE
      ELSE
         OUTBUF = '<no message text>'
      END IF

      OUTLEN = CHR_LEN (OUTBUF)

*
*    Translate the StarBase status into a message and determine its
*    length.

C     CALL CAT1_ERRTR (ERRTXT, STATUS)

C     IF (ERRTXT .NE. ' ') THEN
C        ERRLEN = CHR_LEN (ERRTXT)
C     ELSE
C        ERRLEN = 1
C     END IF

*
*    Construct the error message based on the translation of the error
*    code.

C     ERRMSG = ' '
C     MSGLEN = 0

C     CALL CHR_PUTC ('*status* ', ERRMSG, MSGLEN)
C     CALL CHR_PUTC (ERRTXT(1 : ERRLEN), ERRMSG, MSGLEN)

*
*    Construct the parameter name for the second message.

C     IF (PARAM .NE. ' ') THEN
C        PARA1 = ' '
C        LPARA1 = 0

C        LPARAM = CHR_LEN (PARAM)
C        CALL CHR_PUTC (PARAM(1 : LPARAM), PARA1, LPARA1)

C        CALL CHR_PUTC ('1', PARA1, LPARA1)

C     ELSE
C        PARA1 = ' '

C     END IF

*
*    Report the errors.
*    Code for reporting the error both via the ADAM error system and
*    via a Fortran PRINT statement is included below.  Comment out
*    whichever is not required.

      CALL ERR_REP (PARAM, OUTBUF(1 : OUTLEN), STATUS)
C     CALL ERR_REP (PARA1, ERRMSG(1 : MSGLEN), STATUS)

C     PRINT2000, OUTBUF(1 : OUTLEN), ERRMSG(1 : MSGLEN)
C2000 FORMAT(1X, '*** ERROR ***' /
C    :  1X, A /
C    :  1X, A /
C    :  1X, '*************' )

      END
