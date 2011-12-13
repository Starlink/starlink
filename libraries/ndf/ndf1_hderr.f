      SUBROUTINE NDF1_HDERR( IDCB, REL, STATUS )
*+
*  Name:
*     NDF1_HDERR

*  Purpose:
*     Dump logged error messages to an NDF history record.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_HDERR( IDCB, REL, STATUS )

*  Description:
*     The routine writes information about any logged error messages to
*     the end of the current history record of an NDF. The error
*     message information is extracted from the Error Logging Block
*     (ELB). No history information is written unless errors (or an
*     associated error status) have previously been logged in this
*     common block (by NDF1_HLERR).

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the DCB entry for the data object whose history
*        component is to be updated.
*     REL = LOGICAL (Given)
*        A .TRUE. value indicates that the NDF to which error
*        information is to be logged is in the process of being
*        released from the NDF system. Otherwise, it is assumed to be
*        remaining in use. This flag only affects the wording of the
*        history text.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  No history information will be written by this routine if
*     there is no history component present in the NDF, nor if the
*     history update mode is DISABLED.

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
*     {enter_new_authors_here}

*  History:
*     15-JUN-1993 (RFWS):
*        Original version.
*     16-JUN-1993 (RFWS):
*        Removed defaulting of the application name (now done at a lower
*        level).
*     11-AUG-1993 (RFWS):
*        Leave blank lines to improve formatting of history text.
*     28-SEP-1993 (RFWS):
*        Added the REL argument.
*     28-SEP-1993 (RFWS):
*        Fixed bug - history text was too long.
*     12-MAY-1994 (RFWS):
*        Adapt message to allow for absence of any pending error
*        messages (if a status value alone has been logged).
*     15-NOV-1994 (RFWS):
*        Display the translation of the error status value.
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
      INCLUDE 'EMS_PAR'          ! EMS_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for NDF history component.
*        DCB_HTLEN( NDF__MSDCB ) = INTEGER (Read)
*           History record text width (zero if no text yet written).
*        DCB_HUMOD( NDF__MXDCB ) = INTEGER (Read)
*           History recording update mode.

      INCLUDE 'NDF_ELB'          ! NDF_ Error Logging Block
*        ELB_MSG( NDF__MXERR ) = CHARACTER * ( EMS__SZMSG ) (Read)
*           Pending error message text.
*        ELB_NERR = INTEGER (Read)
*           Number of pending error messages.
*        ELB_STAT = INTEGER (Read)
*           Status value associated with most recent message.

*  Arguments Given:
      INTEGER IDCB
      LOGICAL REL

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER * ( 1 ) BLANK( 1 ) ! Blank text line
      CHARACTER * ( NDF__SZHMX ) MSG( 1 ) ! Error message text
      CHARACTER * ( NDF__SZHIS ) TEXT( 3 ) ! History text
      INTEGER I                  ! Loop counter for error messages

*  Local Data:
      DATA BLANK / ' ' /

*.

*  Check whether the ELB contains any error messages which have been
*  logged for recording in the history record, or whether an associated
*  STATUS value has been logged (otherwise, there is nothing to do).
      IF ( ( ELB_NERR .GT. 0 ) .OR. ( ELB_STAT .NE. SAI__OK ) ) THEN

*  Begin a new error reporting environment.
         CALL ERR_BEGIN( STATUS )

*  Ensure that history component information is available in the DCB.
         CALL NDF1_DH( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that a history component is present and that the history
*  recording mode is not disabled. Otherwise there is nothing to do.
            IF ( ( DCB_HLOC( IDCB ) .NE. DAT__NOLOC ) .AND.
     :           ( DCB_HUMOD( IDCB ) .NE. NDF__HDISA ) ) THEN

*  Write a separating blank line to the history record if history text
*  has previously been written to it.
               IF ( DCB_HTLEN( IDCB ) .NE. 0 )
     :            CALL NDF1_HFWRT( IDCB, ' ', 1, BLANK, .FALSE.,
     :                             .FALSE., .FALSE., STATUS )

*  Set up text to report the logged error status value and its
*  translation.
               IF ( REL ) THEN
                  TEXT( 1 ) = '*** WARNING *** This data structure ' //
     :                 'was released by the'
                  TEXT( 2 ) = 'application while its status was set ' //
     :                 'to the error value ^STAT,'
                  TEXT( 3 ) = 'which translates to the message:'
               ELSE
                  TEXT( 1 ) = '*** WARNING *** This application ' //
     :                 'ended with its status set to'
                  TEXT( 2 ) = 'the error value ^STAT,'
                  TEXT( 3 ) = 'which translates to the message:'
               END IF

*  Define the message token and write the expanded text to the history
*  record, followed by a blank line.
               CALL MSG_SETI( 'STAT', ELB_STAT )
               CALL NDF1_HFWRT( IDCB, ' ', 3, TEXT, .TRUE., .TRUE.,
     :                          .FALSE., STATUS )
               CALL NDF1_HFWRT( IDCB, ' ', 1, BLANK, .FALSE., .FALSE.,
     :                          .FALSE., STATUS )

*  Set up text for the error status translation.
               TEXT( 1 ) = '   ^MESSAGE'

*  Define the message token and write the expanded text to the history
*  record, followed by a blank line.
               CALL EMS_FACER( 'MESSAGE', ELB_STAT )
               CALL NDF1_HFWRT( IDCB, ' ', 1, TEXT, .TRUE., .FALSE.,
     :                          .FALSE., STATUS )
               CALL NDF1_HFWRT( IDCB, ' ', 1, BLANK, .FALSE., .FALSE.,
     :                          .FALSE., STATUS )

*  Set up text to explain the list of error messages (if any) which
*  follows.
               IF ( ELB_NERR .GT. 0 ) THEN
                  TEXT( 1 ) = 'The following additional error ' //
     :                 'messages were pending...'
               ELSE
                  TEXT( 1 ) = 'There were no additional error ' //
     :                 'messages pending.'
               END IF

*  Write this text to the history record, followed by a blank line if
*  appropriate.
               CALL NDF1_HFWRT( IDCB, ' ', 1, TEXT, .TRUE., .TRUE.,
     :                          .FALSE., STATUS )
               IF ( ELB_NERR .GT. 0 ) CALL NDF1_HFWRT( IDCB, ' ', 1,
     :              BLANK, .FALSE., .FALSE., .FALSE., STATUS )

*  Loop to append any logged error messages to the history record. Add
*  an appropriate number of '!' characters at the start to simulate a
*  normal error report.
               DO 1 I = 1, ELB_NERR
                  IF ( I .EQ. 1 ) THEN
                     MSG( 1 ) = '!! ' // ELB_MSG( I )
                  ELSE
                     MSG( 1 ) = '!  ' // ELB_MSG( I )
                  END IF

*  If this causes any message to become too long to be accommodated by
*  the history recording system, then append an ellipsis to indicate
*  truncation.
                  IF ( CHR_LEN( ELB_MSG( I ) ) .GT.
     :                 ( NDF__SZHMX - 3 ) ) THEN
                     MSG( 1 )( NDF__SZHMX - 2 : ) = '...'
                  END IF

*  Write each message inside its own error reporting environment so
*  that we get the maximum information transferred if errors occur.
                  CALL ERR_BEGIN( STATUS )
                  CALL NDF1_HFWRT( IDCB, ' ', 1, MSG, .FALSE., .FALSE.,
     :                             .FALSE., STATUS )
                  CALL ERR_END( STATUS )
 1             CONTINUE
            END IF
         END IF

*  Call error tracing routine.
         IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_HDERR',
     :                                               STATUS )

*  End the outer error reporting environment.
         CALL ERR_END( STATUS )
      END IF

      END
