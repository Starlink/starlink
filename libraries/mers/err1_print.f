      SUBROUTINE ERR1_PRINT( TEXT, ERRBEL, STATUS )
*+
*  Name:
*     ERR1_PRINT

*  Purpose:
*     Split the text of an error message up for delivery to the user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR1_PRINT( TEXT, ERRBEL, STATUS )

*  Description:
*     The text of the given error message is split into lines of length
*     ERR__SZOUT. Each line in than delivered to the user by a call to
*     ERR1_PRERR.

*  Arguments:
*     TEXT = CHARACTER * ( * ) (Given)
*        Text to be output.
*     ERRBEL = LOGICAL (Given & Returned)
*        If true, an attempt will be made to ring a terminal bell
*        in addition to flushing the error messages. Will be set to
*        false if the bell was rung.
*     STATUS = INTEGER (Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1983, 1985, 1987, 1989-1994 Science & Engineering
*     Research Council. Copyright (C) 1997, 1999, 2001 Central 
*     Laboratory of the Research Councils. All Rights Reserved.

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
*     BDK: Dennis Kelly (ROE)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J.Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     10-MAR-1983 (SLW):
*        Modified to use SAI I/O.
*     10-APR-1985 (BDK):
*        ADAM version.
*     4-NOV-1987 (BDK):
*        Use ERR_SZMSG.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     12-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     9-APR-1990 (PCTR):
*        Removed unreferenced include file.
*     8-AUG-1991 (PCTR):
*        Re-written to wrap error messages and call ERR1_PRERR.
*     19-AUG-1992 (PCTR):
*        Changed argument list in EMS1_PFORM call.
*     3-SEP-1992 (PCTR):
*        Remove the LSTAT check in the delivery loop.
*     4-OCT-1993 (PCTR):
*        Added bell character behaviour.
*     12-MAY-1994 (AJC):
*        EMS1_PFORM renamed EMS1_RFORM
*     15-AUG-1997 (AJC):
*        Use NEQV to compare ERRBEL
*     21-JUL-1999 (AJC):
*        Add tuning parameters ERRWSZ and ERRSTM
*     26-FEB-2001 (AJC):
*        Change EMS1_RFORM to MSG1_RFORM
*     28-JUL-2008 (TIMJ):
*        Add extra argument so that we do not need ERRBEL common block.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
      INCLUDE 'ERR_PAR'                 ! ERR_ public constants
      INCLUDE 'ERR_SYS'                 ! ERR_ private constants

*  Global Variables:
      INCLUDE 'ERR_CMN'

*  Arguments Given:
      CHARACTER * ( * ) TEXT

*  Arguments Given & Returned:
      LOGICAL ERRBEL

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN                   ! String length 
      EXTERNAL ERR1_BLK                 ! Force inclusion of block data

*  Local Constants:
      INTEGER ASCBEL                    ! ASCII BEL code
      PARAMETER ( ASCBEL = 7 )

      INTEGER CONTAB                    ! Continuation indentation
      PARAMETER ( CONTAB = 7 )

*  Local Variables:
      INTEGER IPOSN                     ! Character position for text
      INTEGER LENG                      ! String length
      INTEGER LSTAT                     ! Local status
      INTEGER LSTART                    ! Start index of LINE
      INTEGER OPLEN                     ! Output string length

      CHARACTER * 1 BELCHR              ! The bell character
      CHARACTER * ( ERR__SZMSG ) LINE   ! Output line of text

*.

*  Get length of text to send.
      LENG = CHR_LEN( TEXT )

*  Initialise the local status.
      LSTAT = SAI__OK

*  Check whether a bell character is to be delivered and initialise the 
*  output line.
      IF ( ERRBEL ) THEN
         BELCHR = CHAR( ASCBEL )
         LINE = BELCHR
         LSTART = 2
         ERRBEL = .FALSE.
      ELSE
         LINE = ' '
         LSTART = 1
      END IF

*  If the text is not blank, then continue.
      IF ( LENG .GT. 0 ) THEN

*     Loop to split the line of text into sensible lengths for output, 
*     then write them to the error stream. First, initialise the 
*     character pointer.
         IPOSN = 1

*     Now output the message in a way determined by the relevant tuning
*     parameters.
         IF ( ERRSTM ) THEN
            IF ( LSTART .GT. 1 ) CALL ERR1_PRERR( LINE, LSTAT )
            CALL ERR1_PRERR( TEXT, LSTAT )

         ELSE
*     Call MSG1_RFORM to load the first output line and deliver
*     the result.
            CALL MSG1_RFORM
     :         ( TEXT, IPOSN, LINE( LSTART : ERRWSZ ), OPLEN )
            CALL ERR1_PRERR( LINE( : OPLEN+LSTART-1 ), LSTAT )

*     Loop to continue the remainder of the message.
*     DO WHILE loop.
            DO WHILE ( IPOSN .NE. 0 )

*        Re-initialise the output line for a continuation.
               LINE = '!'

*        Call MSG1_RFORM to load the continuation line and write the
*        result.
               CALL MSG1_RFORM( TEXT, IPOSN, LINE( CONTAB : ERRWSZ ),
     :                       OPLEN )
               CALL ERR1_PRERR( LINE( : OPLEN+CONTAB-1 ), LSTAT )
            END DO
         END IF
      ELSE

*     If there is no text, then send a blank message.
         LINE( LSTART : ) = '!!'
         CALL ERR1_PRERR( LINE( : LSTART+1 ), LSTAT )
      END IF

*  Check I/O status and set STATUS if necessary.
      IF ( LSTAT .NE. SAI__OK ) THEN
         STATUS = LSTAT
      END IF

      END
