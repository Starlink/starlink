      SUBROUTINE ERR_REP( PARAM, TEXT, STATUS )
*+
*  Name:
*     ERR_REP

*  Purpose:
*     Report an error message.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR_REP( PARAM, TEXT, STATUS )

*  Description:
*     Report an error message. According to the error context, the 
*     error message is either sent to the user or retained in the 
*     error table. The latter case allows the application to take
*     further action before deciding if the user should receive the 
*     message. On exit the values associated with any existing message 
*     tokens are left undefined. On successful completion, the global 
*     status is returned unchanged; if the status argument is set to 
*     SAI__OK on entry, an error report to this effect is made on behalf 
*     of the application and the status argument is returned set to 
*     ERR__BADOK; the given message is still reported with an associated
*     status of ERR__UNSET.
*     If an output error occurs, the status argument is
*     returned set to ERR__OPTER. The status argument may also be returned
*     set to an EMS_ fault error value, indicating an error occuring 
*     within the error reporting software.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The error message name.
*     TEXT = CHARACTER * ( * ) (Given)
*        The error message text.
*     STATUS = INTEGER (Given)
*        The global status: it is left unchanged on successful completion,
*        or is set an appropriate error value if an internal error has 
*        occurred.

*  Algorithm:
*     -  Use MSG1_FORM to create the complete message text.
*     -  Use EMS1_ESTOR to store the message in the error table.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1983, 1989-1991, 1994 Science & Engineering
*     Research Council.
*     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright (UCL)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     17-APR-1983 (SLW):
*        Added MARK and RELEASE mods.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     12-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     13-DEC-1989 (PCTR):
*        Converted to use EMS_ calls.
*     19-MAR-1990 (PCTR):
*        Changed handling of status returned from ERR_FLUSH.
*     25-SEP-1990 (PCTR):
*        Changed call from EMS1_IELEV to EMS_LEVEL.
*     22-JAN-1991 (PCTR):
*        Removed default level behaviour (it now exists in EMS1_ESTOR).
*     10-JUN-1994 (AJC):
*        Associate ERR__BADOK with warning message and ERR__UNSET with
*        the given message if STATUS is given as SAI__OK.
*     15-SEP-1999 (AJC):
*        Add CLEAN argument to call MSG1_FORM
*     21-FEB-2001 (AJC):
*        Use EMS_REP not EMS1_ESTOR
*     31-JUL-2008 (TIMJ):
*        Use common accessor rather than COMMON directly.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
      INCLUDE 'ERR_ERR'                 ! ERR_ error codes
      INCLUDE 'ERR_PAR'                 ! Public ERR_ constants
      INCLUDE 'EMS_ERR'                 ! EMS_ error codes

*  External References:
      LOGICAL ERR1_GTSTM
      EXTERNAL ERR1_GTSTM

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) TEXT

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER * ( ERR__SZMSG ) TSTR   ! Intermediate error message text
      CHARACTER * ( ERR__SZMSG ) MSTR   ! Final error message text
      CHARACTER * ( ERR__SZPAR ) PSTR   ! Local error name text

      INTEGER ISTAT                     ! Internal status
      INTEGER MLEN                      ! Length of final error message text
      INTEGER TLEN                      ! Length of the temporary message
      INTEGER LPOS                      ! String position pointer
      INTEGER TOKPOS                    ! Position of ^ in string
*.
 
*  Check the inherited status: if it is SAI__OK, then set status to
*  ERR__BADOK and store an additional message in the error table.
      IF ( STATUS .EQ. SAI__OK ) THEN

*     Set the report status equal to ERR__BADOK.
         STATUS = ERR__BADOK

*     Make an additional error report.
         PSTR = 'ERR_REP_BADOK'
         MSTR = 'STATUS not set in call to ERR_REP ' //
     :          '(improper use of ERR_REP)'

*     Store the additional message in the error table (first create a new
*     error reporting context to avoid loss of tokens in the base level).
*     Associate status ERR__BADOK with the additional message.
*     If EMS_REP returns an error status it will be ignored but will
*     almost certainly be repeated later with the given message.
         CALL EMS_MARK
         ISTAT = ERR__BADOK
         CALL EMS_REP( PSTR, MSTR, ISTAT )

*     Release the error context.
         CALL EMS_RLSE

*     Set the given message status to ERR__UNSET
         ISTAT = ERR__UNSET

*  Else, a normal bad status is given - set ISTAT to the given status value
      ELSE
         ISTAT = STATUS

      END IF

*  Now form the given error message.
*  Status is not altered by this routine.
      CALL MSG1_FORM( PARAM, TEXT, .NOT.ERR1_GTSTM(), TSTR, TLEN, ISTAT)

*  Any double ^ will now be single - we must protect it from EMS_REP
      LPOS = 1
      MLEN = 0
      TOKPOS = INDEX( TSTR(1:TLEN), '^' )
      DOWHILE ( TOKPOS .GT. 0 )
         CALL MSG1_PUTC( TSTR(LPOS:LPOS+TOKPOS-1), MSTR, MLEN, ISTAT )
         CALL MSG1_PUTC( '^', MSTR, MLEN, ISTAT )
         LPOS = LPOS + TOKPOS
         TOKPOS = INDEX( TSTR(LPOS:), '^' )
      END DO

*  Now copy the remainder of the string
      IF ( LPOS .LE. TLEN ) THEN
         MSTR(MLEN+1:) = TSTR(LPOS:TLEN)
         MLEN = MLEN + ( TLEN - LPOS + 1 )
      END IF

*  Report the already constructed message with EMS_REP
      CALL EMS_REP( PARAM, MSTR(1:MLEN), STATUS )

*  Check the returned status for message output errors and attempt to
*  report an additional error in the case of failure - but only on the 
*  first occasion.
      IF ( ISTAT .EQ. EMS__OPTER .AND. STATUS .NE. ERR__OPTER ) THEN
         STATUS = ERR__OPTER
         ISTAT = ERR__OPTER
         PSTR = 'ERR_REP_OPTER'
         MSTR = 'ERR_REP: Error encountered during message output'
         CALL EMS_REP( PSTR, MSTR, ISTAT )
      END IF

      END
