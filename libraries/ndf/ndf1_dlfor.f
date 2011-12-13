      SUBROUTINE NDF1_DLFOR( FILE, IFMT, STATUS )
*+
*  Name:
*     NDF1_DLFOR

*  Purpose:
*     Delete a foreign format file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DLFOR( FILE, IFMT, STATUS )

*  Description:
*     The routine deletes a foreign format file associated with an NDF.
*     If required, it first attempts to translate the environment
*     variable NDF_DEL_xxx (where xxx is the foreign format name) to see
*     if this contains an external deletion command. If so, it
*     substitutes the appropriate fields of the file name into this
*     command and then has it executed so as to delete the file.
*     Otherwise, if no external deletion command is defined (or if one
*     is not requested), then the file is deleted directly.

*  Arguments:
*     FILE = CHARACTER * ( * ) (Given)
*        Name of the foreign format file.
*     IFMT = INTEGER (Given)
*        FCB code identifying the foreign file format. If this is
*        non-zero, then an external deletion command corresponding to
*        this format will be used (if available). If it is set to zero,
*        then the file will be deleted directly (and its deletion will
*        not be reported as a foreign format conversion operation, even
*        if the TCB parameter SHCVT is set).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine will attempt to execute even if it is called with
*     STATUS set, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Copyright:
*     Copyright (C) 2000 Science & Engineering Research Council

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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-OCT-1993 (RFWS):
*        Original version.
*     15-APR-1994 (RFWS):
*        Cater for file type extensions containing '.'.
*     20-MAY-1994 (RFWS):
*        Define a token for the file version field.
*     25-MAY-1994 (RFWS):
*        Allow zero as a valid IFMT value.
*     17-NOV-1994 (RFWS):
*        Removed code to set message tokens explicitly.
*     30-JAN-1995 (RFWS):
*        Make the routine attempt to execute even if STATUS is set on
*        entry.
*     30-JAN-1995 (RFWS):
*        Remove calls to mark and release the error stack to protect
*        message tokens. This is no longer needed since the whole
*        routine executes in its own error reporting environment.
*     17-JUL-2000 (DSB):
*        Report an error if FILE includes a foreign extension specifier.
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
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

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
      CHARACTER * ( * ) FILE
      INTEGER IFMT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER * ( 7 ) DIR      ! Direct access permitted?
      CHARACTER * ( 7 ) FOR      ! Formatted access permitted?
      CHARACTER * ( 7 ) SEQ      ! Sequential access permitted?
      CHARACTER * ( 7 ) UNF      ! Unformatted access permitted?
      CHARACTER * ( NDF__SZCVT ) CMD ! Buffer for blank command text
      CHARACTER * ( NDF__SZCVT ) DEL ! Deletion command
      INTEGER F1                 ! First character of format name
      INTEGER F2                 ! Last character of format name
      INTEGER IERR               ! I/O status value
      INTEGER LCMD               ! Length of blank command text
      INTEGER LDEL               ! Length of deletion command
      INTEGER UNIT               ! I/O unit to use
      INTEGER X1                 ! First character of for. extension specifier
      INTEGER X2                 ! Last character of for. extension specifier
      LOGICAL DEF                ! Environment variable defined?
      LOGICAL EXIST              ! I/O unit exists?
      LOGICAL OPENED             ! I/O unit attached to a file?
      LOGICAL THERE              ! File exists?

*.

*  Begin a new error reporting environment.
      CALL ERR_BEGIN( STATUS )

*  Ensure there is no foreign extension specifier in the supplied file
*  name.
      CALL NDF1_FORXT( FILE, X1, X2, STATUS )
      IF( X1 .LE. X2 ) THEN
         STATUS = NDF__FATIN
         CALL MSG_SETC( 'ROUTINE', 'NDF1_DLFOR' )
         CALL MSG_SETC( 'FILE', FILE )
         CALL ERR_REP( 'NDF1_DLFOR_FXS',
     :      'Routine ^ROUTINE called with an invalid FILE argument ' //
     :      'of ^FILE; this value should not include any foreign '//
     :      'extension specifier (internal programming error).',
     :      STATUS )
      END IF

*  Ensure that the TCB is initialised.
      CALL NDF1_INTCB( STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Inquire whether the file exists and check for errors. There is
*  nothing to do if it does not exist.
         INQUIRE ( FILE = FILE, EXIST = THERE, IOSTAT = IERR )
         IF ( IERR .NE. 0 ) THEN
            STATUS = NDF__INQER
            CALL MSG_SETC( 'FILE', FILE )
            CALL ERR_FIOER( 'MESSAGE', IERR )
            CALL ERR_REP( 'NDF1_DLFOR_INQ',
     :           'Error enquiring about the existence of the ' //
     :           'file ''^FILE'' - ^MESSAGE', STATUS )

*  If the file exists, and a foreign file format has been supplied, then
*  obtain the character limits of the file format name in the FCB format
*  list string.
         ELSE
            LCMD = 0
            IF ( THERE ) THEN
               IF ( IFMT .NE. 0 ) THEN
                  F1 = FCB_FMT1( IFMT )
                  F2 = FCB_FMT2( IFMT )

*  Attempt to translate the environment variable which (optionally)
*  holds a command for deleting files with the specified format.
                  CALL NDF1_GTENV( 'NDF_DEL_' // FCB_FMT( F1 : F2 ),
     :                             DEF, CMD, LCMD, STATUS )
               END IF

*  External deletion command.
*  =========================
*  If a non-blank deletion command was obtained above, we must now use
*  it to delete the file. Define standard message tokens for the
*  deletion operation.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( LCMD .GT. 0 ) THEN
                     CALL NDF1_CVTOK( FILE, IFMT, DAT__ROOT, ' ',
     :                                STATUS )

*  Substitute these token values into the blank command, returning the
*  resulting deletion command and its length. Use a low-level (EMS)
*  routine to ensure the message text supplied is used without change.
                     CALL EMS_MLOAD( ' ', CMD( : LCMD ), DEL, LDEL,
     :                               STATUS )
                     LDEL = MAX( 1, LDEL )

*  If required, display what's happening and the command to be executed.
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        IF ( TCB_SHCVT ) THEN
                           CALL MSG_RENEW
                           CALL MSG_OUT( ' ',
     :                          '-->  Deleting: ^FMT file ' //
     :                          '^DIR^NAME^TYPE^VERS', STATUS )
                           CALL MSG_SETC( 'DEL', DEL( : LDEL ) )
                           CALL MSG_OUT( ' ',
     :                          '      command: ^DEL', STATUS )
                        END IF
                     END IF

*  Execute the command to delete the file.
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        CALL NDF1_DOCMD( DEL( : LDEL ), STATUS )
                     END IF

*  No external deletion command.
*  ============================
*  If no deletion command has been specified (or none is required), then
*  we must delete the file directly. This will involve opening the file.
                  ELSE

*  If required, display what is happening.
                     IF ( TCB_SHCVT .AND. ( IFMT .NE. 0 ) ) THEN
                        CALL MSG_SETC( 'FILE', FILE )
                        CALL MSG_SETC( 'FMT', FCB_FMT( F1 : F2 ) )
                        CALL MSG_OUT( ' ',
     :                       '-->  Deleting: ^FMT file ^FILE', STATUS )
                     END IF

*  Loop to search for an I/O unit which exists and is not currently
*  connected to a file. Check for errors during this process.
                     DO 1 UNIT = NDF__UNIT1, NDF__UNIT2
                        INQUIRE ( UNIT = UNIT, EXIST = EXIST,
     :                            OPENED = OPENED, IOSTAT = IERR )
                        IF ( IERR .NE. 0 ) THEN
                           STATUS = NDF__INQER
                           CALL MSG_SETI( 'UNIT', UNIT )
                           CALL ERR_FIOER( 'MESSAGE', IERR )
                           CALL ERR_REP( 'NDF1_DLFOR_UNIT',
     : 'Error enquiring whether Fortran I/O unit ^UNIT exists and ' //
     : 'is connected to a file - ^MESSAGE',
     :                                   STATUS )
                           GO TO 2

*  Quit searching when a suitable I/O unit has been found.
                        ELSE IF ( EXIST .AND. ( .NOT. OPENED ) ) THEN
                           GO TO 2
                        END IF
 1                   CONTINUE

*  Report an error if no suitable I/O unit could be found.
                     STATUS = NDF__NOFIO
                     CALL MSG_SETI( 'UNIT1', NDF__UNIT1 )
                     CALL MSG_SETI( 'UNIT2', NDF__UNIT2 )
                     CALL MSG_SETC( 'FILE', FILE )
                     CALL ERR_REP( 'NDF1_DLFOR_NOU',
     :                    'Unable to find a free Fortran I/O unit ' //
     :                    'in the range ^UNIT1 to ^UNIT2 on which ' //
     :                    'to open the file ''^FILE''.', STATUS )
 2                   CONTINUE

*  We must now open the file on the identified unit. Since the Fortran
*  77 standard permits a file open to fail unless the ACCESS and FORM
*  values are in the set of values allowed for the file, we must first
*  enquire which values are permitted.
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        INQUIRE( FILE = FILE, SEQUENTIAL = SEQ,
     :                           DIRECT = DIR, FORMATTED = FOR,
     :                           UNFORMATTED = UNF, IOSTAT = IERR )

*  Check for errors.
                        IF ( IERR .NE. 0 ) THEN
                           STATUS = NDF__INQER
                           CALL MSG_SETC( 'FILE', FILE )
                           CALL ERR_FIOER( 'MESSAGE', IERR )
                           CALL ERR_REP( 'NDF1_DLFOR_ACC',
     :                          'Error enquiring about the access ' //
     :                          'methods permitted for the ' //
     :                          'file ''^FILE'' - ^MESSAGE', STATUS )

*  Unfortunately, 'UNKNOWN' is a valid response to this enquiry, so we
*  may still have to try all combinations until one succeeds. We can,
*  however, eliminate attempts which are expressly forbidden by the
*  enquiry results. Attempt those combinations most likely to be OK for
*  typical data files first.
                        ELSE

*  Try unformatted sequential access.
                           IF ( ( UNF .NE. 'NO' ) .AND.
     :                          ( SEQ .NE. 'NO' ) ) THEN
                              OPEN( FILE = FILE, UNIT = UNIT,
     :                              STATUS = 'OLD', IOSTAT = IERR,
     :                              FORM = 'UNFORMATTED',
     :                              ACCESS = 'SEQUENTIAL' )
                              IF ( IERR .EQ. 0 ) GO TO 3
                           END IF

*  Try unformatted direct access.
                           IF ( ( UNF .NE. 'NO' ) .AND.
     :                          ( DIR .NE. 'NO' ) ) THEN
                              OPEN( FILE = FILE, UNIT = UNIT,
     :                              STATUS = 'OLD', IOSTAT = IERR,
     :                              FORM = 'UNFORMATTED',
     :                              ACCESS = 'DIRECT' )
                              IF ( IERR .EQ. 0 ) GO TO 3
                           END IF

*  Try formatted sequential access.
                           IF ( ( FOR .NE. 'NO' ) .AND.
     :                          ( SEQ .NE. 'NO' ) ) THEN
                              OPEN( FILE = FILE, UNIT = UNIT,
     :                              STATUS = 'OLD', IOSTAT = IERR,
     :                              FORM = 'FORMATTED',
     :                              ACCESS = 'SEQUENTIAL' )
                              IF ( IERR .EQ. 0 ) GO TO 3
                           END IF

*  Try formatted direct access.
                           IF ( ( FOR .NE. 'NO' ) .AND.
     :                          ( DIR .NE. 'NO' ) ) THEN
                              OPEN( FILE = FILE, UNIT = UNIT,
     :                              STATUS = 'OLD', IOSTAT = IERR,
     :                              FORM = 'FORMATTED',
     :                              ACCESS = 'DIRECT' )
                              IF ( IERR .EQ. 0 ) GO TO 3
                           END IF
 3                         CONTINUE

*  If the file is not yet open (possibly because none of the above were
*  executed), then try one last time using the OPEN statement defaults.
                           OPEN( FILE = FILE, UNIT = UNIT,
     :                           STATUS = 'OLD', IOSTAT = IERR )

*  If the file could not be opened, then report an error.
                           IF ( IERR .NE. 0 ) THEN
                              STATUS = NDF__DELER
                              CALL MSG_SETC( 'FILE', FILE )
                              CALL MSG_SETI( 'UNIT', UNIT )
                              CALL ERR_FIOER( 'MESSAGE', IERR )
                              CALL ERR_REP( 'NDF1_DLFOR_OPN',
     :                             'Error opening file ''^FILE'' on ' //
     :                             'Fortran unit ^UNIT in order to ' //
     :                             'delete it - ^MESSAGE', STATUS )
                           ELSE

*  If OK, then close the file again, specifying that it be deleted.
*  Check for errors.
                              CLOSE( UNIT = UNIT, STATUS = 'DELETE',
     :                               IOSTAT = IERR )
                              IF ( IERR .NE. 0 ) THEN
                                 STATUS = NDF__DELER
                                 CALL MSG_SETC( 'FILE', FILE )
                                 CALL MSG_SETI( 'UNIT', UNIT )
                                 CALL ERR_FIOER( 'MESSAGE', IERR )
                                 CALL ERR_REP( 'NDF1_DLFOR_CLS',
     :                                'Error closing the file ' //
     :                                '''^FILE'' on Fortran unit ' //
     :                                '^UNIT with the file deletion ' //
     :                                'option - MESSAGE', STATUS )

*  If the attempt to delete the file failed, then perform an ordinary
*  close on the I/O unit to ensure it does not remain in use. Ignore
*  any further errors.
                                 CLOSE( UNIT = UNIT, IOSTAT = IERR )
                              END IF
                           END IF
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Call error tracing routine if appropriate.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DLFOR', STATUS )

*  End the error reporting environment.
      CALL ERR_END( STATUS )

      END
