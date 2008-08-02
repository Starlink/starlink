      SUBROUTINE ERR1_FLUSH( ERRBEL, STATUS )
*+
*  Name:
*     ERR1_FLUSH

*  Purpose:
*     Flush the current error context (internal version).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR1_FLUSH( ERRBEL, STATUS )

*  Description:
*     Ensure that all pending error messages in the current error 
*     context have been output to the user. On successful completion, the 
*     error context is annulled and the status argument reset to SAI__OK;
*     if an error occurs during output of the error messages, the 
*     error context is not anulled and the status argument is returned 
*     set to ERR__OPTER. The first argument controls whether a bell is
*     requested in addition to the error messages.

*  Arguments:
*     ERRBEL = LOGICAL (Given & Returned)
*        If true, an attempt will be made to ring a terminal bell
*        in addition to flushing the erro messages. Will be set to
*        false if the bell was rung.
*     STATUS = INTEGER (Returned)
*        The global status: it is set to SAI__OK on return if the 
*        error message output is successful; if not, it is set to 
*        ERR__OPTER.

*  Algorithm:
*     -  Call EMS1_ECOPY to get the error message contents at the current
*     context.
*     -  Call ERR1_PRINT to deliver the error message(s) to the user.
*     -  Call EMS_ANNUL to annul the error table.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1983, 1984, 1989-1991, 1994 Science & Engineering 
*     Research Council. Copyright (C) 1997, 1999, 2001 Central Laboratory
*     of the Research Councils. All Rights Reserved.

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
*     AJC: A.J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     17-ARP-1983 (SLW):
*        Added MARK and RELEASE mods.
*     14-NOV-1984 (BDK):
*        Change name of ERR_PRINT.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     11-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     1-MAR-1990 (PCTR):
*        Converted to use EMS_ calls where possible, and changed the 
*        behaviour of STATUS. 
*     9-APR-1990 (PCTR):
*        Removed unreferenced declarations and replaced DO WHILE construct
*        with ANSI Fortran 77 equivalent.
*     6-JUN-1991 (PCTR):
*        Attempt to print all the pending messages regardless of 
*        output errors.
*     26-JUN-1991 (PCTR):
*        Added mark and release to prevent message tokens being annulled
*        on error.
*      3-AUG-1994 (AJC):
*        Flush ERR_FLUSH error message also
*     15-AUG-1997 (AJC):
*        Use NEQV to compare ERRBEL
*      7-SEP-1999 (AJC):
*        Avoid repetition of messages in 'reveal' mode  
*     20-FEB-2001 (AJC):
*        EMS1_TUNE renamed EMS_TUNE
*        Use EMS_ELOAD not EMS1_ECOPY
*          (means have to add !'s here)
*        Allow for !'s in LINE length
*        Check for NOMSG at base level is not an error
*     26-JUL-2008 (TIMJ):
*        Move to internal shared function that can be called from ERR_FBEL
*        and ERR_FLUSH. This allows us to remove the need for the ERRBEL
*        COMMON block entry.
*     31-JUL-2008 (TIMJ):
*        Use common block accessor functions rather than COMMON itself
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
      INCLUDE 'EMS_PAR'                 ! EMS_ public constants
      INCLUDE 'EMS_ERR'                 ! EMS_ error codes
      INCLUDE 'ERR_ERR'                 ! ERR_ error codes
      INCLUDE 'ERR_PAR'                 ! ERR_ public constants

*  External References:
      LOGICAL ERR1_GTRVL
      EXTERNAL ERR1_GTRVL

*  Arguments Given & Returned:
      LOGICAL ERRBEL

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAXTAB
      PARAMETER ( MAXTAB = 3 )

*  Local Variables:
      CHARACTER * ( ERR__SZMSG ) OPSTR  ! Error message string
      CHARACTER * ( ERR__SZPAR ) PARAM  ! Message name
      CHARACTER * ( ERR__SZMSG + 3) LINE   ! Output line buffer
      CHARACTER * ( MAXTAB ) TABS       ! Line tab

      INTEGER ISTAT                     ! Internal status
      INTEGER LEVEL                     ! Error message context
      INTEGER OPCNT                     ! Output line counter
      INTEGER PARLEN                    ! Length of parameter name
      INTEGER OPLEN                     ! Length of error message string
      INTEGER LINLEN                    ! Length of output line
      INTEGER LSTAT                     ! Status returned by MSG1_PUTC
      INTEGER PSTAT                     ! Status returned by ERR1_PRINT

      LOGICAL NOMSG                     ! If there's really no message
*.

*  Initialise the local status values and the output line counter.
      OPCNT = 0
      ISTAT = SAI__ERROR
      PSTAT = SAI__OK
 
*  Set initial TABS value
      TABS = '!! '

*  Loop to get the error messages at the current context level.
*  DO WHILE loop.
 10   CONTINUE
      IF ( .TRUE. ) THEN

*     Get the last STATUS in case it's needed after EMS_ELOAD has annulled
         CALL EMS_STAT( LSTAT )

*     Get the error message.
         CALL EMS_ELOAD( PARAM, PARLEN, OPSTR, OPLEN, ISTAT )

*     Check for no messages to flush in the base context (i.e. because 
*     they have been delivered immediately to the user). 
*     In that case, just output BEL if required
         NOMSG = .FALSE.
         IF ( ISTAT .EQ. EMS__NOMSG ) THEN
            CALL EMS_LEVEL( LEVEL )
            IF ( ( LEVEL .EQ. EMS__BASE ) 
     :       .AND. ( LSTAT .NE. SAI__OK ) ) NOMSG = .TRUE.
         END IF

         IF ( NOMSG ) THEN
*        Check if any lines have been delivered.
            IF ( OPCNT .EQ. 0 ) THEN

*           Check whether a bell character is to be delivered: if so,
*           deliver it and reset the bell flag.
               IF ( ERRBEL ) THEN
                  CALL ERR1_BELL( PSTAT )
                  ERRBEL = .FALSE.
               END IF
               CALL EMS_ANNUL( ISTAT )
            END IF

*        Repeat the loop. Next will be 'no more messages' but this is needed
*        to reset EMS_ELOAD.
           GO TO 10

         ELSE IF ( ISTAT .NE. SAI__OK ) THEN
*        Construct the output line
            LINE = TABS
            LINLEN = MAXTAB
            IF ( OPLEN .GT. 0 )
     :         CALL MSG1_PUTC( OPSTR(1:OPLEN), LINE, LINLEN, LSTAT )

*        Continue to print messages regardless of output errors.
            CALL ERR1_PRINT( LINE( 1 : LINLEN ), ERRBEL, PSTAT )
            OPCNT = OPCNT + 1

*        Only the first message of a flush has '!! ' prepended.
*        Subsequent messages have '!  '
            TABS = '!  '

            GOTO 10

         ELSE
*        End of messages from EMS_ELOAD
            GOTO 20

         END IF

      END IF

 20   CONTINUE

*  End of the error messages in the current context: if no output error 
*  has occurred, annul the current error context. Ensure 'reveal' is not
*  operative in EMS to avoid duplicate message output.
      IF ( PSTAT .EQ. SAI__OK ) THEN
         IF ( ERR1_GTRVL() ) CALL EMS_TUNE( 'REVEAL', 0, PSTAT )
         CALL EMS_ANNUL( STATUS )
         IF ( ERR1_GTRVL() ) CALL EMS_TUNE( 'REVEAL', 1, PSTAT )

      ELSE

*     Report an error message if an output error has occurred.
*     Don't annul the context in this case.
*     Output it as the last of the current flush
         CALL ERR1_PRINT( 
     :   '!  ERR_FLUSH: Error encountered during message output', 
     :    ERRBEL, PSTAT )
         STATUS = ERR__OPTER

      END IF

      END
