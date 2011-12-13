      SUBROUTINE NDF1_HWENV( IDCB, APPN, STATUS )
*+
*  Name:
*     NDF1_HWENV

*  Purpose:
*     Write default history text derived from the standalone environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_HWENV( IDCB, APPN, STATUS )

*  Description:
*     The routine writes default history text to the current history
*     record of an NDF, creating a new history record if necessary.
*     This version is specific to the standalone environment and writes
*     a list of command line arguments.

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

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

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
*     {enter_new_authors_here}

*  History:
*     10-SEP-1993 (RFWS):
*        Original version, based on the ADAM version.
*     5-OCT-1998 (RFWS):
*        Note if argument information is not available.
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
      CHARACTER * ( MXOUT ) OUT( 1 ) ! Formatted output buffer
      CHARACTER * ( NDF__SZHMX ) ARG ! Argument value
      CHARACTER * ( NDF__SZHMX ) BUF ! Intermediate text buffer
      INTEGER BR                 ! Character position for breaking text
      INTEGER IARG               ! Index into argument list
      INTEGER INDENT             ! No. spaces of indentation to use
      INTEGER L                  ! Text width for formatting
      INTEGER LARG               ! Length of argument value
      INTEGER LARG0              ! Length of zeroth argument
      INTEGER LBUF               ! No. characters used in BUF
      INTEGER LOUT               ! No. characters used in OUT
      INTEGER NEWLEN             ! New buffer length

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

*  Initialise BUF with a heading for the list of command line arguments
*  which will follow.
         LBUF = 10
         BUF( : LBUF ) = 'Arguments:'

*  Obtain the length of the zeroth argument. (If this is zero, and
*  subsequent arguments also have zero length, then we will assume that
*  no argument information is available.)
         CALL NDF1_GTARG( 0, ARG, LARG0, STATUS )

*  Loop to process each argument.
         IARG = 0
 1       CONTINUE                ! Start of 'DO WHILE' loop
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain the next argument.
            IARG = IARG + 1
            CALL NDF1_GTARG( IARG, ARG, LARG, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If there are no arguments (the first one is blank), then manufacture
*  one to document their absence. Use an appropriate string, according
*  to whether there are simply no arguments, or whether argument
*  information is not available.
               IF ( ( IARG .EQ. 1 ) .AND. ( LARG .EQ. 0 ) ) THEN
                  IF ( LARG0 .NE. 0 ) THEN
                     ARG = '<none>'
                     LARG = 6
                  ELSE
                     ARG = '<unknown>'
                     LARG = 9
                  END IF
               END IF

*  Quit looping if there are no more arguments to process.
               IF ( LARG .EQ. 0 ) GO TO 3

*  Convert any non-printing characters to blanks and re-evaluate the
*  argument length.
               CALL CHR_CLEAN( ARG( : LARG ) )
               LARG = CHR_LEN( ARG( : LARG ) )
               IF ( LARG .GT. 0 ) THEN

*  Find how much text there will be in BUF if this argument value is
*  appended to the existing contents. Allow for indentation if BUF is
*  currently empty, otherwise allow a separating blank.
                  NEWLEN = LBUF + LARG
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
                     BUF( LBUF + 1 : NEWLEN ) = ARG( : LARG )
                     LBUF = NEWLEN

*  Otherwise, transfer the existing contents of BUF (if any) to the
*  next output line and loop to repeatedly refill BUF from the argument
*  value ARG so as to break it into further output lines.
                  ELSE
                     BR = 0
 2                   CONTINUE ! Start of 'DO WHILE' loop
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        IF ( LBUF .GT. 0 ) THEN
                           OUT( 1 )( LOUT + 1 : LOUT + L ) =
     :                                                  BUF( : LBUF )
                           LOUT = LOUT + L
                        END IF

*  If the output buffer is now full (there is no longer room for
*  further lines), then flush it by appending its contents to the
*  current history record.
                        IF ( ( MXOUT - LOUT ) .LT. L ) THEN
                           CALL NDF1_HWRT( IDCB, APPN, LOUT / L,
     :                                     OUT( 1 )( : L ), STATUS )
                           LOUT = 0
                        END IF

*  Determine how many of the characters remaining in ARG will fit into
*  a single output line, starting at the character following the last
*  line break position BR. Allow for indentation.
                        LBUF = MIN( L - INDENT, LARG - BR )

*  Transfer this number of characters into BUF, adding indentation, and
*  update BR.
                        IF ( LBUF .GT. 0 ) THEN
                           IF ( INDENT .GT. 0 ) BUF( : INDENT ) = ' '
                           BUF( INDENT + 1 : INDENT + LBUF ) =
     :                                      ARG( BR + 1 : BR + LBUF )
                           LBUF = LBUF + INDENT
                        END IF
                        BR = BR + LBUF

*  If BUF now contains a complete output line, then return to transfer
*  it to the output buffer. Otherwise, continue to process the next
*  parameter.
                        IF ( LBUF .EQ. L ) GO TO 2
                     END IF
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
