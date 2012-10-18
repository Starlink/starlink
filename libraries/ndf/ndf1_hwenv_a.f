      SUBROUTINE NDF1_HWENV( IDCB, APPN, STATUS )
*+
*  Name:
*     NDF1_HWENV

*  Purpose:
*     Write default history text derived from the ADAM environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_HWENV_ADAM( IDCB, APPN, STATUS )

*  Description:
*     The routine writes default history text to the current history
*     record of an NDF, creating a new history record if necessary.
*     This version is specific to the ADAM software environment and
*     writes a list of current parameter keywords and values.

*  Arguments:
*     IDCB = INTEGER (Given)
*        DCB index identifying the NDF whose history record is to be
*        updated.
*     APPN = CHARACTER * ( * ) (Given)
*        Name of the current application. This will be used to
*        initialise the new history record if a current record does not
*        initially exist, otherwise it is ignored. If a blank value is
*        given, then s suitable default will be used instead.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     An NDF history structure must exist and DCB information must be
*     available for it. This routine does not check for this itself.

*  Implementation Deficiencies:
*     -  This routine makes calls to SUBPAR routines in order to obtain
*     the current values of parameters. This deficiency is a
*     work-around for a problem with message token expansion in
*     MSG_LOAD, which prevents the string '$^P' from expanding to the
*     current value of the parameter whose name is in message token P.
*     Calls to SUBPAR should be eliminated when this problem is fixed.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     13-MAY-1993 (RFWS):
*        Original version.
*     14-MAY-1993 (RFWS):
*        Re-written to prevent parameter values containing significant
*        blanks from being wrapped across line boundaries.
*     17-MAY-1993 (RFWS):
*        Improved the error handling.
*     19-MAY-1993 (RFWS):
*        Added loop to break parameter values that are too long into
*        several output lines if necessary.
*     2-JUN-1993 (RFWS):
*        Do not write history information unless the history update
*        mode is at least 'NORMAL'.
*     16-JUN-1993 (RFWS):
*        Documented defaulting of the application name.
*     4-AUG-1993 (RFWS):
*        Add mark and release of the error stack and prevent unwanted
*        final flush of a zero-length output buffer.
*     7-SEP-1993 (RFWS):
*        Call SUBPAR_INDEX to identify the parameters.
*     7-SEP-1993 (RFWS):
*        Added indentation to output lines.
*     9-MAR-1994 (RFWS):
*        Use PAR__ constants to test for parameter state.
*     2011-09-08 (TIMJ):
*        Correct offsetting when breaking a long parameter over multiple
*        lines. We were losing INDENT characters at each line break.
*     18-OCT-2012 (DSB):
*        If a parameter value has not yet been retrieved by the application
*        when this routine is called, it may not have a current value,
*        causing an error to be reported by SUBPAR_CURVAL. Catch this error
*        and use a value of "<not yet accessed>".
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'MSG_PAR'          ! MSG_ public constants
      INCLUDE 'PAR_PAR'          ! PAR_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HTLEN( NDF__MXDCB ) = INTEGER (Read)
*           Text length for the current history record.
*        DCB_HUMOD( NDF__MXDCB ) = INTEGER (Read)
*           History recording update mode.

*  Arguments Given:
      INTEGER IDCB
      CHARACTER * ( * ) APPN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER MXOUT              ! Output buffer size in characters
      PARAMETER ( MXOUT = 2 * NDF__SZHMX )

*  Local Variables:
      CHARACTER * ( 6 ) FMT      ! Parameter message format string
      CHARACTER * ( MSG__SZMSG ) MSG ! Expanded message text
      CHARACTER * ( MSG__SZMSG ) VALUE ! Parameter value
      CHARACTER * ( MXOUT ) OUT( 1 ) ! Formatted output buffer
      CHARACTER * ( NDF__SZHMX ) BUF ! Intermediate text buffer
      CHARACTER * ( PAR__SZNAM ) PARAM ! Parameter name
      INTEGER BR                 ! Character position for breaking text
      INTEGER INDENT             ! No. spaces of indentation to use
      INTEGER IPAR               ! Index into parameter names list
      INTEGER L                  ! Text width for formatting
      INTEGER LBUF               ! No. characters used in BUF
      INTEGER LMSG               ! Length of expanded message text
      INTEGER LOUT               ! No. characters used in OUT
      INTEGER LPAR               ! Length of parameter name
      INTEGER NCODE              ! Parameter namecode
      INTEGER NEWLEN             ! New buffer length
      INTEGER STATE              ! Parameter state
      LOGICAL FIRST              ! First parameter?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Only write history information if the history update mode is at
*  least 'NORMAL'.
      IF ( DCB_HUMOD( IDCB ) .GE. NDF__HNORM ) THEN

*  Mark the error stack to prevent use of message tokens from affecting
*  any which are already defined.
         CALL ERR_MARK

*  Initialise.
         LOUT = 0

*  Determine the text length to be used for the current history record.
*  If this is zero (because the current record does not yet exist),
*  then use the default preferred text length NDF__SZHIS.
         L = DCB_HTLEN( IDCB )
         IF ( L .EQ. 0 ) L = NDF__SZHIS

*  Determine how much indentation to use. Normally use 3 spaces, but
*  reduce this if the line length is short.
         INDENT = MIN( L / 6, 3 )

*  Initialise BUF with a heading for the list of parameters which will
*  follow.
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
                           CALL SUBPAR_FINDPAR( PARAM, NCODE, STATUS )

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
                              CALL SUBPAR_CURVAL( NCODE, VALUE, STATUS )
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
                        OUT( 1 )( LOUT + 1 : LOUT + L ) = BUF( : LBUF )
                        LOUT = LOUT + L
                     END IF

*  If the output buffer is now full (there is no longer room for
*  further lines), then flush it by appending its contents to the
*  current history record.
                     IF ( ( MXOUT - LOUT ) .LT. L ) THEN
                        CALL NDF1_HWRT( IDCB, APPN, LOUT / L,
     :                                  OUT( 1 )( : L ), STATUS )
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
     :                                         MSG( BR + 1 : BR + LBUF )
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
            IF ( LOUT .GT. 0 ) CALL NDF1_HWRT( IDCB, APPN, LOUT / L,
     :                                         OUT( 1 )( : L ), STATUS )
         END IF

*  Release the error stack.
         CALL ERR_RLSE
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_HWENV', STATUS )

      END
