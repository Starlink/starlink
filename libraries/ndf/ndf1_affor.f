      SUBROUTINE NDF1_AFFOR( IFMT, STATUS )
*+
*  Name:
*     NDF1_AFFOR

*  Purpose:
*     Perform post-processing on a dataset.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AFFOR( IFMT, STATUS )

*  Description:
*     The routine obtains a post-processing command to apply to a
*     dataset after it has been released, by translating the appropriate
*     environment variable. It then substitutes the necessary message
*     token values into this command and has it executed so as to
*     perform the required post-processing.

*  Arguments:
*     IFMT = INTEGER (Given)
*        FCB code identifying the format of the dataset (may be zero to
*        indicate a native format NDF dataset).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine does not have access to data in the DCB describing
*     the dataset being processed. This is because the dataset will
*     already have been released by the time this routine is executed.
*     Instead, the name of the dataset (and any related items) should be
*     supplied via message tokens which have been defined earlier.
*     -  All message tokens in the current context will be left in an
*     undefined state after this routine exits.
*     -  This routine does not make any checks on the existence or
*     accessibility of any file or dataset before attempting to process
*     it.

*  Copyright:
*     Copyright (C) 1994 Particle Physics & Astronomy Research Council

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
*     19-SEP-1994 (RFWS):
*        Original version.
*     23-DEC-1994 (RFWS):
*        Updated prologue.
*     23-DEC-1994 (RFWS):
*        Ensure that all message tokens are undefined on exit.
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

*  Global Variables:
      INCLUDE 'NDF_FCB'          ! NDF_ Format Conversion Block
*        FCB_FMT = CHARACTER * ( 2 * NDF__SZFMT ) (Read)
*           Foreign format list string.
*        FCB_FMT1( 2 * NDF__MXFMT ) = INTEGER (Read)
*           Character positions of start of each foreign format name.
*        FCB_FMT2( 2 * NDF__MXFMT ) = INTEGER (Read)
*           Character positions of end of each foreign format name.

      INCLUDE 'NDF_TCB'          ! NDF_ Tuning Control Block
*        TCB_SHCVT = LOGICAL (Read)
*           Whether to display information about data conversion
*           operations.

*  Arguments Given:
      INTEGER IFMT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZCVT ) CMD ! Buffer for raw command text
      CHARACTER * ( NDF__SZCVT ) POST ! Translated command text
      INTEGER LCMD               ! Length of blank command text
      INTEGER LPOST              ! Length of converted text
      LOGICAL DEF                ! Environment variable defined?

*.

*  If STATUS is set on entry, make a dummy call to MSG_LOAD to ensure
*  that all message tokens become undefined. Do not make any further
*  error reports.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_LOAD( ' ', ' ', POST, LPOST, STATUS )

*  Otherwise, ensure that the TCB is initialised.
      ELSE
         CALL NDF1_INTCB( STATUS )

*  Attempt to translate an environment variable of the form
*  NDF_POST_<FMT> to obtain the post-processing command. Set the format
*  to "NDF" if there is no foreign format file.
         IF ( IFMT .EQ. 0 ) THEN
            CALL NDF1_GTENV( 'NDF_POST_NDF', DEF, CMD, LCMD, STATUS )
         ELSE
            CALL NDF1_GTENV( 'NDF_POST_' //
     :                       FCB_FMT( FCB_FMT1( IFMT ) :
     :                                FCB_FMT2( IFMT ) ),
     :                       DEF, CMD, LCMD, STATUS )
         END IF

*  If no command was defined (or it was blank), then there is nothing
*  more to do. Otherwise, substitute the pre-defined message tokens into
*  the blank command, returning the resulting post-processing command
*  and its length. Use a low-level (EMS) routine to ensure the message
*  text supplied is used without change.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( LCMD .NE. 0 ) THEN
               CALL EMS_MLOAD( ' ', CMD( : LCMD ), POST, LPOST, STATUS )
               LPOST = MAX( 1, LPOST )

*  If required, report details of the dataset being processed.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( TCB_SHCVT ) THEN
                     CALL MSG_RENEW
                     IF ( IFMT .EQ. 0 ) THEN
                        CALL MSG_OUT( ' ',
     :                       '--> Post-proc: NDF object ^NDF',
     :                       STATUS )
                     ELSE
                        CALL MSG_OUT( ' ',
     :                       '--> Post-proc: ^FMT file ' //
     :                       '^DIR^NAME^TYPE^VERS', STATUS )
                     END IF

*  Display the values of the flags which controlled the release of the
*  dataset.
                     CALL MSG_RENEW
                     CALL MSG_OUT( ' ',
     :                    '        flags: keep=^KEEP mod=^MOD del=^DEL',
     :                    STATUS )

*  Display the command being used.
                     CALL MSG_SETC( 'POST', POST( : LPOST ) )
                     CALL MSG_OUT( ' ',
     :                    '      command: ^POST', STATUS )
                  END IF

*  Execute the post-processing command.
                  CALL NDF1_DOCMD( POST( : LPOST ), STATUS )
               END IF
            END IF
         END IF

*  Before exiting, make a dummy call to MSG_LOAD to ensure that all
*  message tokens become undefined.
         CALL MSG_LOAD( ' ', ' ', POST, LPOST, STATUS )

*  Call error tracing routine and exit.
         IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AFFOR',
     :                                               STATUS )
      END IF

      END
