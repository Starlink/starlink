      SUBROUTINE MSG1_PRINT( TEXT, STATUS )
*+
*  Name:
*     MSG1_PRINT

*  Purpose:
*     Deliver the text of a message to the user. 

*  Language:
*    Starlink Fortran 77

*  Invocation:
*     CALL MSG1_PRINT( TEXT, STATUS )

*  Description:
*     This uses the parameter system to deliver the message text to the 
*     user. Trailing blanks are removed. If the delivery fails, the 
*     message is given as an error report and a subsequent explanatory 
*     error report made. This should ensure that the message is seen by 
*     the user.

*  Arguments:
*     TEXT = CHARACTER * ( * ) (Given)
*        The message text.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Notes:
*     -  This subroutine is shared by both ADAM and standalone implementation.
*        System specific message delivery is done via MSG1_PRTLN.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1982-1984, 1987, 1989-1992, 1994 Science &
*     Engineering Research Council. Copyright (C) 1999, 2001, 2004 Central
*     Laboratory of the Research Councils.
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
*     SLW: Sid  Wright (UCL)
*     BDK: Dennis Kelly (ROE)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1982 (JRG):
*        Original version.
*     10-Mar-1983 (SLW):
*        Modified to use SAE I/O.
*     13-NOV-1984 (BDK):
*        ADAM version.
*     3-NOV-1987 (BDK):
*        Remove trap on message length.
*     19-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     13-MAR-1990 (PCTR):
*        Changed the name of the subroutine and included error
*        reporting.
*        and EMS_ calls.
*     26-JUN-1991 (PCTR):
*        Added mark and release to prevent message tokens being annulled
*        on error.
*     8-AUG-1991 (PCTR):
*        Added use of EMS1_PFORM to reformat the output messages.
*     19-AUG-1992 (PCTR):
*        Changed argument list in EMS1_PFORM call.
*     26-AUG-1992 (PCTR):
*        Changed call to SUBPAR_WRITE to SUBPAR_WRMSG.
*     3-SEP-1992 (PCTR):
*        Deliver the message text via the error reporting system on
*        failure.
*     12-MAY-1994 (AJC):
*        EMS1_PFORM renamed EMS1_RFORM
*     14-DEC-1994 (AJC):
*        Avoid concatenation of argument text
*     21-JUL-1999 (AJC):
*        Add tuning parameters MSGWSZ and MSGSTM
*     26-FEB-2001 (AJC):
*        Use MSG1_RFORM nor EMS1_RFORM
*     1-JUL-2004 (DSB):
*        Use MSG1_GT... functions to get the values from the MSG_CMN 
*        common blocks rather than directly accessing the common blocks
*        (which are initialised in a different shared library).
*     25-JUL-2008 (TIMJ):
*        The ADAM and standalone versions were identical in all ways
*        except for the call to subpar or write (and the corresponding
*        check of inherited or I/O status). Move the system specific
*        code to a new routine to aid code reuse.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Declarations:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
      INCLUDE 'MSG_ERR'                 ! MSG_ error codes
      INCLUDE 'MSG_PAR'                 ! MSG_ public constants
      INCLUDE 'MSG_SYS'                 ! MSG_ private constants

*  Arguments Given:
      CHARACTER * ( * ) TEXT

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN                   ! String length
      INTEGER MSG1_GTWSZ
      LOGICAL MSG1_GTSTM
      EXTERNAL MSG1_GTSTM
      EXTERNAL MSG1_GTWSZ

*  Local Variables:
      INTEGER IPOSN                     ! Character pointer
      INTEGER ISTAT                     ! Local status
      INTEGER LENG                      ! Given string length
      INTEGER OPLEN                     ! Output string length

      CHARACTER * ( MSG__SZMSG ) LINE   ! Output line of text

*.

*  Check the inherited global status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a new error context and initialise the local status.
      CALL EMS_MARK
      ISTAT = SAI__OK

*  Find the filled length of the message text string and write it 
*  to the standard output stream.
      LENG = CHR_LEN( TEXT )

      IF ( LENG .GT. 0 ) THEN
         LENG = MIN( LENG, MSG__SZMSG )

         IF ( MSG1_GTSTM() ) THEN
*     Output with no messing
            CALL MSG1_PRTLN( TEXT, ISTAT )

         ELSE
*     Call MSG1_RFORM to load the output line and deliver it.
            IPOSN = 1

*     Loop to deliver the message in line-sized chunks.
*     DO WHILE loop.
 10         CONTINUE
            IF ( IPOSN .NE. 0 .AND. ISTAT .EQ. SAI__OK ) THEN
               CALL MSG1_RFORM( TEXT, IPOSN, LINE( : MSG1_GTWSZ()), 
     :                          OPLEN )
               CALL MSG1_PRTLN( LINE( : OPLEN ), ISTAT )
            GO TO 10
            END IF
         END IF

      ELSE
*     If there is no text, then send a blank message.
         LINE = " "
         OPLEN = 1
         CALL MSG1_PRTLN( ' ', ISTAT )
      END IF

*  If the message cannot be delivered, then annul the current error
*  context and report the error.
      IF ( ISTAT .NE. SAI__OK ) THEN
         STATUS = ISTAT
         CALL EMS_ANNUL( ISTAT )
         CALL EMS_MARK
         CALL EMS_REP( 'MSG_PRINT_MESS',
     :                 'MSG1_PRINT: ' // LINE( : OPLEN ), STATUS )
         STATUS = MSG__OPTER
         CALL EMS_REP( 'MSG_PRINT_OPTER', 
     :   'Error encountered during message output', STATUS )
         CALL EMS_RLSE
      END IF

*  Release the current error context.
      CALL EMS_RLSE

      END
