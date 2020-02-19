      SUBROUTINE KPG1_LGCMD( APPN, PACK, CPUTIM, STATUS )
*+
*  Name:
*     KPG1_LGCMD

*  Purpose:
*     Log the running of a command.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL  KPG1_LGCMD( APPN, PACK, CPUTIM, STATUS )

*  Description:
*     The routine writes a record to a log file specified by environment
*     variable <PACK>_LOG. The record includes the application name and
*     the parameter values. No record is written if the environment
*     variable is undefined.

*  Arguments:
*     APPN = CHARACTER * ( * ) (Given)
*        Name of the current application.
*     PACK = CHARACTER * ( * ) (Given)
*        Name of the package (e.g. "KAPPA").
*     CPUTIM( 4 ) = INTEGER (Given and returned)
*        An array holding the CPU time at the start of the command that
*        is being logged. This should be obtained by calling KPG1_CPUTM
*        immediately before the command starts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2015,2020 East Asian Observatory.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     23-SEP-2016 (DSB):
*        Original version, based on the NDF1_HWENV.
*     6-DEC-2019 (DSB):
*        Renamed from KPS1_LGCMD to KPG1_LGCMD and added PACK argument.
*     19-FEB-2020 (DSB):
*        Added CPUTIM argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ public constants
      INCLUDE 'PAR_PAR'          ! PAR_ public constants
      INCLUDE 'PSX_ERR'          ! PSX_ public error constants

*  Arguments Given:
      CHARACTER * ( * ) APPN
      CHARACTER * ( * ) PACK

*  Arguments Given and Returned:
      INTEGER CPUTIM( 4 )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER MXOUT              ! Output buffer size in characters
      PARAMETER ( MXOUT = 2 * MSG__SZMSG )

      INTEGER L                  ! Max lengthof one line of text
      PARAMETER ( L = 256 )

      INTEGER INDENT             ! Indentation
      PARAMETER ( INDENT = 3 )

*  Local Variables:
      CHARACTER BUF*( MSG__SZMSG ) ! Intermediate text buffer
      CHARACTER ENVNAM*100       ! Environment variable name
      CHARACTER FMT*6            ! Parameter message format string
      CHARACTER LOGFILE*200      ! Path to log file
      CHARACTER MSG*( MSG__SZMSG ) ! Expanded message text
      CHARACTER OUT( 1 )*( MXOUT ) ! Formatted output buffer
      CHARACTER PARAM*( PAR__SZNAM ) ! Parameter name
      CHARACTER VALUE*( MSG__SZMSG ) ! Parameter value
      DOUBLE PRECISION CPUSEC    ! CPU time since previous call to KPG1_CPUTM
      INTEGER BR                 ! Character position for breaking text
      INTEGER FD                 ! File descriptor for log file
      INTEGER IAT                ! Current length of string
      INTEGER IPAR               ! Index into parameter names list
      INTEGER LBUF               ! No. characters used in BUF
      INTEGER LMSG               ! Length of expanded message text
      INTEGER LOUT               ! No. characters used in OUT
      INTEGER LPAR               ! Length of parameter name
      INTEGER NCODE              ! Parameter namecode
      INTEGER NEWLEN             ! New buffer length
      INTEGER STATE              ! Parameter state
      LOGICAL FIRST              ! First parameter?

*.

*  Start a new error reporting context, since we want to log the task
*  even if it failed.
      CALL ERR_BEGIN( STATUS )

*  Create the environment variable name.
      ENVNAM = ' '
      IAT = 0
      CALL CHR_APPND( PACK, ENVNAM, IAT )
      CALL CHR_APPND( '_LOG', ENVNAM, IAT )

*  Get the path to the log file, if any.
      CALL PSX_GETENV( ENVNAM, LOGFILE, STATUS )

*  If there was no translation, simply annul the error.
      IF ( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )

*  If we are logging the information, open the log file so that we
*  can append new text to it.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         CALL FIO_OPEN( LOGFILE, 'APPEND', 'NONE', L, FD, STATUS )

*  If the file could not be opened, flush the error and jump to the end.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'A', APPN )
            CALL MSG_SETC( 'E', ENVNAM )
            CALL ERR_REP( ' ', 'Failed to log kappa task ^A to '//
     :                    'file specified by environment variable '//
     :                    '^E.', STATUS )
            CALL ERR_FLUSH( STATUS )
         ELSE

*  Write the command name to the log file.
            BUF = ' '
            LBUF = 0
            CALL CHR_APPND( 'Command:', BUF, LBUF )
            LBUF = LBUF + 1
            CALL CHR_APPND( APPN, BUF, LBUF )
            CALL FIO_WRITE( FD, ' ', STATUS )
            CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )

*  Write the CPU used to the log file, in seconds.
            CALL KPG1_CPUTM( CPUTIM, CPUSEC )
            BUF = ' '
            LBUF = 0
            CALL CHR_APPND( 'CPU used:', BUF, LBUF )
            LBUF = LBUF + 1
            CALL CHR_PUTD( CPUSEC, BUF, LBUF )
            LBUF = LBUF + 1
            CALL CHR_APPND( 'sec', BUF, LBUF )
            CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )

*  Initialise BUF with a heading for the list of parameters which will
*  follow.
            LOUT = 0
            LBUF = 11
            BUF( : LBUF ) = 'Parameters:'

*  Loop to process each parameter.
            IPAR = 0
    1       CONTINUE                ! Start of 'DO WHILE' loop
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the length of text to be output.
               LMSG = 0

*  Obtain the name of the next parameter.
               FIRST = ( IPAR .EQ. 0 )
               CALL SUBPAR_INDEX( IPAR, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If there are no parameters (none is found on the first attempt),
*  then provide some text to document their absence.
                  IF ( ( IPAR .EQ. 0 ) .AND. FIRST ) THEN
                     MSG = '<none>'
                     LMSG = 6

*  Quit looping when there are no more parameters to process.
                  ELSE IF ( IPAR .EQ. 0 ) THEN
                     GO TO 3
                  ELSE

*  Otherwise, obtain the parameter's name and state.
                     CALL SUBPAR_PARNAME( IPAR, PARAM, LPAR, STATUS )
                     CALL PAR_STATE( PARAM, STATE, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Classify the parameter state and assign an appropriate message
*  format for representing its value. States which are not recognised
*  here have their format left blank and are subsequently ignored.
                        FMT = ' '
                        IF ( STATE .EQ. PAR__ACTIVE ) THEN
                           FMT = '%^P=^V'
                        ELSE IF ( STATE .EQ. PAR__NULLST ) THEN
                           FMT = '%^P=!'
                        ELSE IF ( STATE .EQ. PAR__CANCEL ) THEN
                           FMT = '%^P='
                        END IF

*  If the state was recognised, then define a message token for the
*  parameter name. If the parameter is in the active state, then also
*  obtain its current value as a character string and assign this to a
*  message token.
                        IF ( FMT .NE. ' ' ) THEN
                           CALL MSG_SETC( 'P', PARAM )
                           IF ( STATE .EQ. PAR__ACTIVE ) THEN
                              CALL SUBPAR_FINDPAR( PARAM, NCODE,
     :                                             STATUS )

*  A parameter will be in the active state if a value is supplied for it
*  on the command line, but that value is only saved as the current value
*  when the value of the parameter is accessed by the application using
*  PAR_GETx etc. So if this routine is called before the parameter is
*  accessed, it will be in the active state but may not have a current
*  value. In which case the following call to SUBPAR_CURVAL will report
*  an error. Annull this error and use a "value" that indicates that the
*  real value is unknown. This is not fool-proof since even if a
*  parameter has a current value, it may have been set on a previous
*  invocation of the application, and a different value may be used on
*  the current invocation if the parameter has not yet been accessed. For
*  this reason this routine should be called once all parameters have been
*  accessed. Maybe this routine should report an error if the parameter
*  value has not yet been accessed by the application. But how do you
*  tell if the current value of a parameter has been set during the
*  current invocation of the application or a previous one?
                              IF( STATUS .EQ. SAI__OK ) THEN
                                 CALL SUBPAR_CURVAL( NCODE, VALUE,
     :                                               STATUS )
                                 IF( STATUS .NE. SAI__OK ) THEN
                                    CALL ERR_ANNUL( STATUS )
                                    VALUE = '<not yet accessed>'
                                 END IF
                                 CALL MSG_SETC( 'V', VALUE )
                              END IF
                           END IF

*  Expand the message text.
                           CALL MSG_LOAD( ' ', FMT, MSG, LMSG, STATUS )
                           IF ( STATUS .EQ. SAI__OK ) THEN

*  Convert any non-printing characters to blanks and re-evaluate the
*  expanded text length.
                              CALL CHR_CLEAN( MSG( : LMSG ) )
                              LMSG = CHR_LEN( MSG( : LMSG ) )
                           END IF
                        END IF
                     END IF
                  END IF
               END IF

*  If OK, and there is some text to output, then find how much text
*  there will be in BUF if it is appended to the existing contents.
*  Allow for indentation if BUF is currently empty, otherwise allow a
*  separating blank.
               IF ( ( STATUS .EQ. SAI__OK ) .AND. ( LMSG .GT. 0 ) ) THEN
                  NEWLEN = LBUF + LMSG
                  IF ( LBUF .EQ. 0 ) THEN
                     NEWLEN = NEWLEN + INDENT
                  ELSE
                      NEWLEN = NEWLEN + 1
                  END IF

*  If the output line length will not be exceeded, then append the new
*  text to BUF, with indentation or a separating space as appropriate.
                  IF ( NEWLEN .LE. L ) THEN
                     IF ( LBUF .EQ. 0 ) THEN
                        LBUF = INDENT
                        IF ( LBUF .GT. 0 ) BUF( : LBUF ) = ' '
                     ELSE
                        LBUF = LBUF + 1
                        BUF( LBUF : LBUF ) = ' '
                     END IF
                     BUF( LBUF + 1 : NEWLEN ) = MSG( : LMSG )
                     LBUF = NEWLEN

*  Otherwise, transfer the existing contents of BUF (if any) to the
*  next output line and loop to repeatedly refill BUF from the expanded
*  text buffer MSG so as to break it into further output lines.
                  ELSE
                     BR = 0
    2                CONTINUE       ! Start of 'DO WHILE' loop
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        IF ( LBUF .GT. 0 ) THEN
                           OUT( 1 )( LOUT + 1 : LOUT + L )
     :                                = BUF( : LBUF )
                           LOUT = LOUT + L
                        END IF

*  If the output buffer is now full (there is no longer room for
*  further lines), then flush it by appending its contents to the
*  log file.
                        IF ( ( MXOUT - LOUT ) .LT. L ) THEN
                           CALL FIO_WRITE( FD, OUT( 1 )( : L ), STATUS )
                           LOUT = 0
                        END IF

*  Determine how many of the characters remaining in MSG will fit into
*  a single output line, starting at the character following the last
*  line break position BR. Allow for indentation.
                        LBUF = MIN( L - INDENT, LMSG - BR )

*  Transfer this number of characters into BUF, adding indentation, and
*  update BR.
                        IF ( LBUF .GT. 0 ) THEN
                           IF ( INDENT .GT. 0 ) BUF( : INDENT ) = ' '
                           BUF( INDENT + 1 : INDENT + LBUF ) =
     :                                  MSG( BR + 1 : BR + LBUF )
                           LBUF = LBUF + INDENT
                        END IF
                        BR = BR + LBUF - INDENT

*  If BUF now contains a complete output line, then return to transfer
*  it to the output buffer. Otherwise, continue to process the next
*  parameter.
                        IF ( LBUF .EQ. L ) GO TO 2
                     END IF
                  END IF
               END IF

*  Return to process the next parameter.
               GO TO 1
            END IF
    3       CONTINUE

*  Transfer any remaining text from BUF to the output buffer and
*  perform a final flush.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( LBUF .GT. 0 ) THEN
                  OUT( 1 )( LOUT + 1 : LOUT + L ) = BUF( : LBUF )
                  LOUT = LOUT + L
               END IF
               IF ( LOUT .GT. 0 ) CALL FIO_WRITE( FD, OUT( 1 )( : L ),
     :                                            STATUS )
            END IF

*  CLose the log file.
            CALL FIO_CLOSE( FD, STATUS )
         END IF
      END IF

*  End the error context.
      CALL ERR_END( STATUS )

      END
