      SUBROUTINE NDF1_XTFOR( FORFIL, IFMT, NDFLOC, NDFNAM, IMP, STATUS )
*+
*  Name:
*     NDF1_XTFOR

*  Purpose:
*     Import or export NDF extension information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_XTFOR( FORFIL, IFMT, NDFLOC, NDFNAM, IMP, STATUS )

*  Description:
*     The routine obtains conversion commands which import or export
*     extension information for an NDF which is being converted from/to
*     a foreign format file, by translating the appropriate environment
*     variables. It then substitutes the necessary file name (and
*     other) fields into each command and has them executed so as to
*     perform the extension import/export operations.

*  Arguments:
*     FORFIL = CHARACTER * ( * ) (Given)
*        Name of the foreign format file, optionally including a foreign
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
*     IMP = LOGICAL (Given)
*        If a .TRUE. value is given, then extension information is being
*        imported. Otherwise it is being exported.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine does not make any checks on the existence or
*     accessibility of the foreign file or the associated NDF object.

*  Copyright:
*     Copyright (C) 1993, 1994 Science & Engineering Research Council
*     Copyright (C) 2000 CCLRC
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     12-NOV-1993 (RFWS):
*        Original version.
*     15-APR-1994 (RFWS):
*        Cater for file type extensions containing '.'.
*     28-APR-1994 (RFWS):
*        Call NDF1_HTOP to obtain a top-level locator.
*     20-MAY-1994 (RFWS):
*        Define a token for the file version field.
*     15-SEP-1994 (RFWS):
*        Use new token names.
*     1-NOV-1994 (RFWS):
*        Changed to translate environment variables locally, instead of
*        using extension names previously stored in the FCB. Added
*        support for format-specific extension name lists.
*     4-NOV-1994 (RFWS):
*        Scan extension names in reverse order on export.
*     4-NOV-1994 (RFWS):
*        Check for extension presence before attempting export.
*     17-NOV-1994 (RFWS):
*        Removed code to set message tokens explicitly.
*     17-JUL-2000 (DSB):
*        Allow for foreign extension specifiers in FORFIL.
*     24-DEC-2005 (TIMJ):
*        USE HDS_FIND rather than NDF1_HFIND
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'DAT_ERR'          ! DAT_ error codes
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
      CHARACTER * ( * ) FORFIL
      INTEGER IFMT
      CHARACTER * ( * ) NDFLOC
      CHARACTER * ( * ) NDFNAM
      LOGICAL IMP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! Temporary NDF locator
      CHARACTER * ( DAT__SZLOC ) MLOC ! Locator to MORE structure
      CHARACTER * ( DAT__SZLOC ) TOPLOC ! Top level locator
      CHARACTER * ( NDF__SZCVT ) CMD ! Buffer for raw command text
      CHARACTER * ( NDF__SZCVT ) XTN ! Translated command text
      CHARACTER * ( NDF__SZXLS ) XLST ! List of NDF extension names
      INTEGER F1                 ! First format name character position
      INTEGER F2                 ! Last format name character position
      INTEGER IXTN               ! Loop counter for NDF extensions
      INTEGER LCMD               ! Length of blank command text
      INTEGER LXTN               ! Length of converted text
      INTEGER NXTN               ! Number of NDF extension names
      INTEGER X1                 ! First extn. name character position
      INTEGER X2                 ! Last extn. name character position
      INTEGER XEND               ! Last extension to consider
      INTEGER XINC               ! Extension increment
      INTEGER XSTART             ! First extension to consider
      INTEGER XTN1( NDF__MXEXT ) ! Starting position of extension names
      INTEGER XTN2( NDF__MXEXT ) ! Ending position of extension names
      LOGICAL DEF                ! Environment variable defined?
      LOGICAL MORE               ! MORE structure present?
      LOGICAL XTHERE             ! Extension present?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that the TCB is initialised.
      CALL NDF1_INTCB( STATUS )

*  If we are exporting extension information, then obtain a locator for
*  the NDF data object.
      MORE = .FALSE.
      IF ( .NOT. IMP ) THEN
         CALL HDS_FIND( NDFLOC, NDFNAM, 'READ', LOC, STATUS )

*  Mark the error stack and attempt to obtain a (primary) locator to the
*  NDF's extension (MORE) structure.
         IF ( STATUS .EQ. SAI__OK ) THEN
            MORE = .TRUE.
            CALL ERR_MARK
            CALL DAT_FIND( LOC, 'MORE', MLOC, STATUS )
            CALL DAT_PRMRY( .TRUE., MLOC, .TRUE., STATUS )

*  Note if the extension structure does not exist.
            IF ( STATUS .EQ. DAT__OBJNF ) THEN
               CALL ERR_ANNUL( STATUS )
               MORE = .FALSE.
            END IF
            CALL ERR_RLSE
         END IF

*  Annul the temporary NDF locator.
         CALL DAT_ANNUL( LOC, STATUS )
      END IF

*  There is nothing to do if the extension structure is absent and we
*  are exporting extension information. Otherwise, obtain the character
*  positions of the foreign format name in the FCB format list string.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( IMP .OR. MORE ) ) THEN
         F1 = FCB_FMT1( IFMT )
         F2 = FCB_FMT2( IFMT )

*  Attempt to translate an environment variable specific to the data
*  format being used in order to obtain a list of the NDF extensions to
*  be recognised. If a specific environment variable is not defined,
*  then use the default environment variable name 'NDF_XTN' instead.
         DEF = .TRUE.
         CALL NDF1_GTXTN( 'NDF_XTN_' // FCB_FMT( F1 : F2 ), NDF__MXEXT,
     :                    DEF, XLST, XTN1, XTN2, NXTN, STATUS )
         IF ( .NOT. DEF ) THEN
            CALL NDF1_GTXTN( 'NDF_XTN', NDF__MXEXT, DEF, XLST, XTN1,
     :                       XTN2, NXTN, STATUS )
         END IF

*  If an extension list was obtained, then define loop parameters to
*  scan the extension names in the required order (we scan them forwards
*  on import and backwards on export).
         IF ( ( STATUS .EQ. SAI__OK ) .AND. DEF ) THEN
            IF ( IMP ) THEN
               XSTART = 1
               XEND = NXTN
               XINC = 1
            ELSE
               XSTART = NXTN
               XEND = 1
               XINC = -1
            END IF

*  Loop to process each NDF extension named in the list. Obtain the
*  character string limits of each name.
            DO 1 IXTN = XSTART, XEND, XINC
               X1 = XTN1( IXTN )
               X2 = XTN2( IXTN )

*  If exporting extension information, check the NDF's extension
*  structure to see whether the extension exists. If not, then skip it.
               XTHERE = .TRUE.
               IF ( .NOT. IMP ) CALL DAT_THERE( MLOC, XLST( X1 : X2 ),
     :                                          XTHERE, STATUS )
               IF ( ( STATUS .EQ. SAI__OK ) .AND. XTHERE ) THEN

*  Attempt to translate the appropriate environment variable to obtain
*  an extension import/export command specific to the foreign file
*  format (e.g. NDF_IMP_<XTN>_<FMT> or NDF_EXP_<XTN>_<FMT>).
                  DEF = .FALSE.
                  IF ( IMP ) THEN
                     CALL NDF1_GTENV( 'NDF_IMP_' // XLST( X1 : X2 ) //
     :                                '_' // FCB_FMT( F1 : F2 ), DEF,
     :                                CMD, LCMD, STATUS )
                  ELSE
                     CALL NDF1_GTENV( 'NDF_EXP_' // XLST( X1 : X2 ) //
     :                                '_' // FCB_FMT( F1 : F2 ), DEF,
     :                                CMD, LCMD, STATUS )
                  END IF

*  If no command was found, then try to obtain one which is not specific
*  to the foreign file format by translating the appropriate environment
*  variable (e.g. NDF_IMP_<XTN> or NDF_EXP_<XTN>).
                  IF ( .NOT. DEF ) THEN
                     IF ( IMP ) THEN
                        CALL NDF1_GTENV( 'NDF_IMP_' // XLST( X1 : X2 ),
     :                                   DEF, CMD, LCMD, STATUS )
                     ELSE
                        CALL NDF1_GTENV( 'NDF_EXP_' // XLST( X1 : X2 ),
     :                                   DEF, CMD, LCMD, STATUS )
                     END IF
                  END IF

*  If a valid (non blank) command was obtained, then mark the error
*  stack to prevent any interference with previously defined message
*  tokens and define standard message tokens for the import/export
*  operation.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( LCMD .GT. 0 ) THEN
                        CALL ERR_MARK
                        CALL NDF1_CVTOK( FORFIL, IFMT, NDFLOC, NDFNAM,
     :                                   STATUS )

*  Also define the extension name token.
                        CALL MSG_SETC( 'XTN', XLST( X1 : X2 ) )

*  Substitute these token values into the blank command, returning the
*  resulting import/export command and its length. Use a low-level (EMS)
*  routine to ensure the message text supplied is used without change.
                        CALL EMS_MLOAD( ' ', CMD( : LCMD ), XTN, LXTN,
     :                                  STATUS )
                        LXTN = MAX( 1, LXTN )

*  If required, report details of the extension import/export operation
*  being performed.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           IF ( TCB_SHCVT ) THEN

*  Importing an extension.
                              IF ( IMP ) THEN
                                 CALL MSG_RENEW
                                 CALL MSG_OUT( ' ',
     :                                '--> Importing: extension ^XTN',
     :                                STATUS )
                                 CALL MSG_RENEW
                                 CALL MSG_OUT( ' ',
     :                                '         into: NDF object ^NDF',
     :                                STATUS )
                                 CALL MSG_RENEW
                                 CALL MSG_OUT( ' ',
     :                                ' derived from: ^FMT file ' //
     :                                '^DIR^NAME^TYPE^VERS^FXS',
     :                                STATUS )

*  Exporting an extension.
                              ELSE
                                 CALL MSG_RENEW
                                 CALL MSG_OUT( ' ',
     :                                '--> Exporting: extension ^XTN',
     :                                STATUS )
                                 CALL MSG_RENEW
                                 CALL MSG_OUT( ' ',
     :                                '         from: NDF object ^NDF',
     :                                STATUS )
                                 CALL MSG_RENEW
                                 CALL MSG_OUT( ' ',
     :                                ' destined for: ^FMT file ' //
     :                                '^DIR^NAME^TYPE^VERS^FXS',
     :                                STATUS )
                              END IF

*  Display the command being used.
                              CALL MSG_SETC( 'XTN', XTN( : LXTN ) )
                              CALL MSG_OUT( ' ',
     :                             '      command: ^XTN', STATUS )
                           END IF
                        END IF

*  Release the error stack.
                        CALL ERR_RLSE

*  If the NDF container file is already open (we have a locator to it),
*  then we must flush modifications and release all locks on it so that
*  the extension import/export process can access it. Obtain a top-level
*  HDS locator in order to do this.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           IF ( NDFLOC .NE. DAT__ROOT ) THEN
                              CALL NDF1_HTOP( NDFLOC, 'UPDATE', TOPLOC,
     :                                        STATUS )

*  After freeing the file, do not perform any more operations on it
*  until import/export has completed, since this may cause the file
*  to be locked again.
                              CALL HDS_FREE( TOPLOC, STATUS )
                           END IF

*  Execute the extension import/export command.
                           CALL NDF1_DOCMD( XTN( : LXTN ), STATUS )

*  Annul the top level locator, if obtained.
                           IF ( NDFLOC .NE. DAT__ROOT ) THEN
                              CALL DAT_ANNUL( TOPLOC, STATUS )
                           END IF
                        END IF
                     END IF
                  END IF
               END IF

*  Quit processing extensions if an error occurs.
               IF ( STATUS .NE. SAI__OK ) GO TO 2
 1          CONTINUE
 2          CONTINUE
         END IF
      END IF

*  Annul the locator for the extension (MORE) structure if necessary.
      IF ( MORE ) CALL DAT_ANNUL( MLOC, STATUS )

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_XTFOR', STATUS )

      END
