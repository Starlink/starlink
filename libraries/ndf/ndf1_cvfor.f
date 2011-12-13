      SUBROUTINE NDF1_CVFOR( FORFIL, IFMT, NDFLOC, NDFNAM, FROM,
     :                       STATUS )
*+
*  Name:
*     NDF1_CVFOR

*  Purpose:
*     Perform data format conversion on a foreign file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CVFOR( FORFIL, IFMT, NDFLOC, NDFNAM, FROM, STATUS )

*  Description:
*     The routine obtains the conversion command which converts between
*     a foreign format data file and a native format NDF object (or
*     vice versa) by translating the appropriate environment variable.
*     It then substitutes the necessary file name (and other) fields
*     into this command and has it executed so as to perform the
*     conversion.

*  Arguments:
*     FORFIL = CHARACTER * ( * ) (Given)
*        Name of the foreign format file, optionally containing a foreign
*        extension specifier.
*     IFMT = INTEGER (Given)
*        FCB code identifying the format of the foreign file (must be
*        non-zero).
*     NDFLOC = CHARACTER * ( * ) (Given)
*        Locator which, in conjunction with the NDFNAM argument,
*        identifies the native format NDF object. If a value of
*        DAT__ROOT is given, then NDFNAM should contain the absolute
*        name of this object.
*     NDFNAM = CHARACTER * ( * ) (Given)
*        Relative HDS name of the native format NDF object (or the
*        absolute name if NDFLOC is set to DAT__ROOT).
*     FROM = LOGICAL (Given)
*        If a .TRUE. value is given, conversion is from the foreign
*        format to native NDF format. Otherwise the reverse conversion
*        is performed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine does not make any checks on the existence or
*     accessibility of the input file before conversion (it should
*     exist and be readable), nor on the existence of the output file
*     (typically, it should not exist prior to conversion and should
*     exist afterwards).

*  Copyright:
*     Copyright (C) 2000 Central Laboratories of the Research Councils

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
*     PWD: Peter W. Draper (STARLINK, Durham University)
*     {enter_new_authors_here}

*  History:
*     12-OCT-1993 (RFWS):
*        Original version.
*     5-NOV-1993 (RFWS):
*        Improved error messages.
*     12-NOV-1993 (RFWS):
*        Installed importing and exporting of extension information.
*     15-APR-1994 (RFWS):
*        Cater for file type extensions containing '.'.
*     28-APR-1994 (RFWS):
*        Call NDF1_HTOP to obtain a top-level locator.
*     20-MAY-1994 (RFWS):
*        Define a token for the file version field.
*     1-JUN-1994 (RFWS):
*        Include file version in informational messages.
*     15-SEP-1994 (RFWS):
*        Use new token names.
*     17-NOV-1994 (RFWS):
*        Removed code to set message tokens explicitly.
*     17-JUL-2000 (DSB):
*        Allow for foreign extension specifiers in FORFIL.
*     11-MAR-2004 (PWD):
*        Changed EMS_MLOAD call to EMS_EXPND. This removes the
*        limitation on buffer size.
*     9-FEB-2005 (DSB):
*        Modified to use NDF1_CVCMD to get the conversion command.
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
      INCLUDE 'NDF_TCB'          ! NDF_ Tuning Control Block
*        TCB_SHCVT = LOGICAL (Read)
*           Whether to display information about data conversion
*           operations.

*  Arguments Given:
      CHARACTER * ( * ) FORFIL
      INTEGER IFMT
      CHARACTER * ( * ) NDFLOC
      CHARACTER * ( * ) NDFNAM
      LOGICAL FROM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) TOPLOC ! Top level locator
      CHARACTER * ( NDF__SZCVT ) CMD ! Buffer for raw command text
      CHARACTER * ( NDF__SZCVT ) CVT ! Translated command text
      INTEGER LCMD               ! Length of blank command text
      INTEGER LCVT               ! Length of converted text
      LOGICAL DEF                ! Environment variable defined?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that the TCB is initialised.
      CALL NDF1_INTCB( STATUS )

*  Attempt to translate the appropriate environment variable to obtain
*  the conversion command.
      CALL NDF1_CVCMD( FORFIL, IFMT, NDFLOC, NDFNAM, FROM, .TRUE.,
     :                 DEF, CMD, LCMD, STATUS )

*  If a valid conversion command was obtained, and we are converting to
*  a foreign file format, then export any NDF extension information
*  before performing the conversion.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF( LCMD .GT. 0 ) THEN
            IF ( .NOT. FROM ) THEN
               CALL NDF1_XTFOR( FORFIL, IFMT, NDFLOC, NDFNAM, .FALSE.,
     :                          STATUS )
            END IF

*  If OK, mark the error stack to prevent any interference with
*  previously defined message tokens and define standard message tokens
*  for the conversion operation.
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL ERR_MARK
               CALL NDF1_CVTOK( FORFIL, IFMT, NDFLOC, NDFNAM, STATUS )

*  Substitute these token values into the blank command, returning the
*  resulting conversion command and its length. Use a low-level (EMS)
*  routine to ensure the message text supplied is used without change.
               CALL EMS_EXPND( CMD( : LCMD ), CVT, LCVT, STATUS )
               LCVT = MAX( 1, LCVT )

*  If required, report details of the conversion being performed.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( TCB_SHCVT ) THEN

*  Converting from a foreign format.
                     IF ( FROM ) THEN
                        CALL MSG_RENEW
                        CALL MSG_OUT( ' ',
     :                       '-->Converting: ^FMT file ' //
     :                       '^DIR^NAME^TYPE^VERS^FXS', STATUS )
                        CALL MSG_RENEW
                        CALL MSG_OUT( ' ',
     :                       '      to give: NDF object ^NDF', STATUS )

*  Converting to a foreign format.
                     ELSE
                        CALL MSG_RENEW
                        CALL MSG_OUT( ' ',
     :                       '-->Converting: NDF object ^NDF', STATUS )
                        CALL MSG_RENEW
                        CALL MSG_OUT( ' ',
     :                       '      to give: ^FMT file ' //
     :                       '^DIR^NAME^TYPE^VERS^FXS', STATUS )
                     END IF

*  Display the command being used.
                     CALL MSG_SETC( 'CVT', CVT( : LCVT ) )
                     CALL MSG_OUT( ' ',
     :                    '      command: ^CVT', STATUS )
                  END IF
               END IF

*  Release the error stack.
               CALL ERR_RLSE

*  If the NDF container file is already open (we have a locator to it),
*  then we must flush modifications and release all locks on it so that
*  the data conversion process can access it. Obtain a top-level HDS
*  locator in order to do this.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( NDFLOC .NE. DAT__ROOT ) THEN
                     CALL NDF1_HTOP( NDFLOC, 'UPDATE', TOPLOC, STATUS )

*  After freeing the file, do not perform any more operations on it
*  until data conversion has completed, since this may cause the file
*  to be locked again.
                     CALL HDS_FREE( TOPLOC, STATUS )
                  END IF

*  Execute the conversion command.
                  CALL NDF1_DOCMD( CVT( : LCVT ), STATUS )

*  Annul the top level locator, if obtained.
                  IF ( NDFLOC .NE. DAT__ROOT ) THEN
                     CALL DAT_ANNUL( TOPLOC, STATUS )
                  END IF

*  If converting from a foreign file format, then import any NDF
*  extension information after performing the conversion.
                  IF ( FROM ) THEN
                     CALL NDF1_XTFOR( FORFIL, IFMT, NDFLOC, NDFNAM,
     :                                .TRUE., STATUS )
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CVFOR', STATUS )

      END
