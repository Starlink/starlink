      SUBROUTINE SHOWPROV( STATUS )
*+
*  Name:
*     SHOWPROV

*  Purpose:
*     Display provenance information for an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SHOWPROV( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application displays details of the NDFs that were used in
*     the creation of the supplied NDF. This information is read from the
*     PROVENANCE extension within the NDF, and includes both immediate
*     parent NDFs and older ancestor NDFs (i.e. the parents of the
*     parents, etc).
*
*     The screen output can be either tabular or non-tabular. In tabular
*     format the screen output consists of a number of lines of text,
*     each holding a complete description of an NDF containing several
*     fields. In non-tabular mode, each NDF is described by a block of
*     lines, each holding a single field. Non-tabular mode is usually
*     better for screen display since tabular mode can produce very long
*     lines that wrap. In both cases, each NDF has an integer index number
*     that is displayed at the start of the line or block. The first line
*     or block will always describe the supplied NDF itself. Subsequent
*     lines or blocks will describe the NDFs that were used in the creation
*     of the supplied NDF.
*
*     Each line or block contains a number of fields, each with a
*     heading as follows:
*
*     - "ID": An integer index for the NDF.
*     - "PATH": The path of the NDF. Note, this is where the NDF was when
*     the provenance information was recorded. It is of course possible that
*     the NDF may subsequently have been moved or deleted .
*     - "DATE": The formatted UTC date and time at which the provenance
*     information for the NDF was recorded.
*     - "CREATOR": A string identifying the software that created the
*     NDF.
*     - "PARENTS": A comma separated list of integers that are the indices
*     of the immediate parents of the NDF. The integers refer to the "ID"
*     column.
*     - "MORE": A summary of any extra information about the NDF stored with
*     the provenance information. In general this may be an arbitrary HDS
*     structure and so full details cannot be given on a single line. The
*     HDSTRACE command can be used to examine the MORE field in detail. To
*     see full details of the NDF with "ID" value of 12 (say), do
*     "hdstrace fred.more.provenance.ancestors'(12)'", where "fred" is
*     the name of the NDF supplied for parameter "NDF".
*
*     By default, not all of these fields are displayed on the screen. The
*     choice and order of fields to be displayed can be controlled using
*     parameter SHOW.

*  Usage:
*     showprov ndf show basename tabular

*  ADAM Parameters:
*     BASENAME = _LOGICAL (Read)
*        If TRUE, then the Path field contains only the NDF base name
*        (i.e. the path is removed). This is useful for creating shorter
*        lines of text. [current value]
*     NDF = NDF (Read)
*        The NDF data structure.
*     SHOW = LITERAL (Read)
*        A string indicating which items of provenance information are
*        to be displayed and in what order. The supplied string should
*        consist entirely of characters taken from the set P, D, C, T
*        and M. These refer to the Path, Date, Creator, Parent and More
*        fields respectively. The initial default value is "PT". If a
*        character occurs more than once in the supplied string, then the
*        second and subsequent occurrences of it are ignored. [current value]
*     TABULAR = _LOGICAL (Read)
*        If TRUE, then tabular screen output is produced. [FALSE]

*  Notes:
*     - If a KAPPA application uses one or more input NDFs to create an
*     output NDF, the output NDF may or may not contain provenance
*     information depending on two things: 1) whether any of the
*     input NDFs already contain provenance information and 2) the value
*     of the AUTOPROV environment variable. It is usually necessary to
*     set the AUTOPROV variable to "1" in order to create output NDFs that
*     contain provenance information. The exception to this if you are
*     supplied with NDFs from another source that already contain
*     provenance. If such NDFs are used as inputs to KAPPA applicatiosn
*     then the output NDFs will contain provenance even if the AUTOPROV
*     variable is unset. However, setting AUTOPROV to "0" will always
*     prevent provenance information being stored in the output NDFs.
*     - Some other packages, such as CCDPACK, follow the same strategy
*     for creating and propagating provenance information.

*  Examples:
*     showprov m51
*        This displays the provenance information in the NDF m51.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JAN-2008 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! PAR constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER BUFLEN             ! Length of text buffer
      PARAMETER ( BUFLEN = 256 )

*  Local Variables:
      CHARACTER KEY*20           ! Key for entry within KeyMap
      CHARACTER SHOW*20          ! Supplied value for parameter SHOW
      CHARACTER TEXT*(BUFLEN)    ! Buffer for line output
      CHARACTER USHOW*5          ! Verified codes for fields to display
      CHARACTER VALUE*(BUFLEN)   ! Buffer for one field value
      INTEGER I                  ! Loop index
      INTEGER INDF               ! NDF identifier
      INTEGER IROW               ! Row index
      INTEGER KYMAP1             ! AST KeyMap holding all prov info
      INTEGER KYMAP2             ! AST KeyMap holding field widths
      INTEGER MXLEN              ! Max field width
      INTEGER NC                 ! String length
      INTEGER NFIELD             ! The number of fields to display
      INTEGER NROW               ! No. of lines to display
      INTEGER SHOWLN             ! Length of string SHOW
      INTEGER TAB( 6 )           ! Tab stops for each field
      LOGICAL BASE               ! Value of the BASENAME parameter
      LOGICAL CDONE              ! Has the "C" field already be done?
      LOGICAL DDONE              ! Has the "D" field already be done?
      LOGICAL DONE               ! Has this field already be done?
      LOGICAL MDONE              ! Has the "M" field already be done?
      LOGICAL PDONE              ! Has the "P" field already be done?
      LOGICAL TABLE              ! Produce tabular output?
      LOGICAL TDONE              ! Has the "T" field already be done?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Obtain an identifier for the NDF.
      CALL LPG_ASSOC( 'NDF', 'READ', INDF, STATUS )

*  Determine if directory paths are to be stripped from the NDF paths.
      CALL PAR_GET0L( 'BASENAME', BASE, STATUS )

*  See what format of output is required.
      CALL PAR_GET0L( 'TABULAR', TABLE, STATUS )

*  Format the provenance information in the NDF. The resulting strings
*  are returned in an AST KeyMap.
      CALL NDG_FMPRV( INDF, BASE, KYMAP1, STATUS )

*  Get the number of entries in the returned keymap. This will be one
*  more than the number of NDFs described in the displayed table.
      NROW = AST_MAPSIZE( KYMAP1, STATUS ) - 1

*  Get a pointer to a KeyMap that holds the maximum field width for each
*  field.
      IF( .NOT. AST_MAPGET0A( KYMAP1, 'MXLEN', KYMAP2, STATUS ) ) THEN
         IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'No MXLEN entry found in KeyMap '//
     :                    'returned by NDG_FMPRV (programming error).',
     :                    STATUS )
         END IF
         GO TO 999
      END IF

*  Determine what is to be shown and in what order.
      CALL PAR_GET0C( 'SHOW', SHOW, STATUS )
      CALL CHR_UCASE( SHOW )
      SHOWLN = CHR_LEN( SHOW )

*  Store the width of every required column. Always do the ID column first.
      IF( .NOT. AST_MAPGET0I( KYMAP2, 'ID', MXLEN, STATUS ) ) THEN
         STATUS = SAI__ERROR
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL ERR_REP( ' ', 'No ID entry found in MXLEN KeyMap '//
     :                    'returned by NDG_FMPRV (programming error).',
     :                    STATUS )
         END IF
         GO TO 999
      END IF

      TAB( 1 ) = MAX( LEN( 'ID' ), MXLEN ) + 2

*  Now do the other columns that are to be displayed. We also construct a
*  copy of SHOW from which any duplicated characters have been removed.
      PDONE = .FALSE.
      DDONE = .FALSE.
      CDONE = .FALSE.
      TDONE = .FALSE.
      MDONE = .FALSE.

      USHOW = ' '
      NFIELD = 0

      DO I = 1, SHOWLN
         IF( SHOW( I : I ) .EQ. 'P' ) THEN
            KEY = 'PATH'
            DONE = PDONE
            PDONE = .TRUE.

         ELSE IF( SHOW( I : I ) .EQ. 'D' ) THEN
            KEY = 'DATE'
            DONE = DDONE
            DDONE = .TRUE.

         ELSE IF( SHOW( I : I ) .EQ. 'C' ) THEN
            KEY = 'CREATOR'
            DONE = CDONE
            CDONE = .TRUE.

         ELSE IF( SHOW( I : I ) .EQ. 'T' ) THEN
            KEY = 'PARENTS'
            DONE = TDONE
            TDONE = .TRUE.

         ELSE IF( SHOW( I : I ) .EQ. 'M' ) THEN
            KEY = 'MORE'
            DONE = MDONE
            MDONE = .TRUE.

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'C', SHOW( I : I ) )
            CALL MSG_SETC( 'S', SHOW )
            CALL ERR_REP( ' ', 'Illegal character "^C" included in '//
     :                    'value supplied for parameter SHOW ("^S").',
     :                    STATUS )
            GO TO 999
         END IF

         IF( .NOT. DONE ) THEN
            NFIELD = NFIELD + 1
            USHOW( NFIELD : NFIELD ) = SHOW( I : I )

            IF( .NOT. AST_MAPGET0I( KYMAP2, KEY, MXLEN, STATUS ) ) THEN
               STATUS = SAI__ERROR
               IF( STATUS .EQ. SAI__OK ) THEN
                  CALL MSG_SETC( 'K', KEY )
                  CALL ERR_REP( ' ', 'No ^K entry found in MXLEN '//
     :                          'KeyMap returned by NDG_FMPRV '//
     :                          '(programming error).', STATUS )
               END IF
               GO TO 999
            END IF

            TAB( NFIELD + 1 ) = TAB( NFIELD ) +
     :                          MAX( 9, LEN( KEY ), MXLEN ) + 2

         END IF

      END DO

*  If producing tabular output, display the header line.
      IF( TABLE ) THEN
         CALL MSG_BLANK( STATUS )

         TEXT = 'ID'

         DO I = 1, NFIELD

            IF( USHOW( I : I ) .EQ. 'P' ) THEN
               KEY = 'PATH'

            ELSE IF( USHOW( I : I ) .EQ. 'D' ) THEN
               KEY = 'DATE'

            ELSE IF( USHOW( I : I ) .EQ. 'C' ) THEN
               KEY = 'CREATOR'

            ELSE IF( USHOW( I : I ) .EQ. 'T' ) THEN
               KEY = 'PARENTS'

            ELSE IF( USHOW( I : I ) .EQ. 'M' ) THEN
               KEY = 'MORE'

            END IF

            IF( TAB( I ) + LEN( KEY ) .LE. BUFLEN ) THEN
               TEXT( TAB( I ) : ) = KEY
            ELSE
               CALL MSG_OUT( ' ', 'WARNING: Line truncated.', STATUS )
            END IF

         END DO

         CALL MSG_OUT( ' ', TEXT, STATUS )
         CALL MSG_BLANK( STATUS )
      END IF

*  Loop round each NDF to be described.
      DO IROW = 1, NROW

*  Get the KeyMap holding details for this row.
         CALL CHR_ITOC( IROW - 1, KEY, NC )
         IF( .NOT. AST_MAPGET0A( KYMAP1, KEY( : NC ), KYMAP2,
     :                           STATUS ) ) THEN
            STATUS = SAI__ERROR
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL MSG_SETI( 'I', IROW )
               CALL ERR_REP( ' ', 'No "^I" entry found in KeyMap '//
     :                       'returned by NDG_FMPRV (programming '//
     :                       'error).', STATUS )
            END IF
            GO TO 999
         END IF

* First deal with tabular output.
         IF( TABLE ) THEN
            TEXT = ' '

*  Append each field to the text buffer. First the ID value.
            IF( .NOT. AST_MAPGET0C( KYMAP2, 'ID', TEXT, NC,
     :                              STATUS ) ) THEN
               IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'No "ID" entry found in KeyMap '//
     :                          'returned by NDG_FMPRV (programming '//
     :                          'error).', STATUS )
               END IF
               GO TO 999
            END IF

* Now the other fields.
            DO I = 1, NFIELD

               IF( USHOW( I : I ) .EQ. 'P' ) THEN
                  KEY = 'PATH'

               ELSE IF( USHOW( I : I ) .EQ. 'D' ) THEN
                  KEY = 'DATE'

               ELSE IF( USHOW( I : I ) .EQ. 'C' ) THEN
                  KEY = 'CREATOR'

               ELSE IF( USHOW( I : I ) .EQ. 'T' ) THEN
                  KEY = 'PARENTS'

               ELSE IF( USHOW( I : I ) .EQ. 'M' ) THEN
                  KEY = 'MORE'

               END IF

               IF( .NOT. AST_MAPGET0C( KYMAP2, KEY, VALUE, NC,
     :                                 STATUS ) ) THEN
                  VALUE = '<unknown>'
                  NC = 9
               END IF

               IF( TAB( I ) + NC .LE. BUFLEN ) THEN
                  TEXT( TAB( I ) : ) = VALUE( : NC )
               ELSE
                  CALL MSG_OUT( ' ', 'WARNING: Line truncated.',
     :                          STATUS )
               END IF

            END DO

*  Display the buffer.
            CALL MSG_OUT( ' ', TEXT, STATUS )

*  Now deal with non-tabular output.
         ELSE
            CALL MSG_BLANK( STATUS )

*  First do the ID value.
            IF( .NOT. AST_MAPGET0C( KYMAP2, 'ID', VALUE, NC,
     :                              STATUS ) ) THEN
               IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'No "ID" entry found in KeyMap '//
     :                          'returned by NDG_FMPRV (programming '//
     :                          'error).', STATUS )
               END IF
               GO TO 999
            END IF

            CALL MSG_SETC( 'V', VALUE( : NC ) )
            CALL MSG_OUT( ' ', 'ID: ^V', STATUS )

* Now the other fields.
            DO I = 1, NFIELD

               IF( USHOW( I : I ) .EQ. 'P' ) THEN
                  KEY = 'PATH'

               ELSE IF( USHOW( I : I ) .EQ. 'D' ) THEN
                  KEY = 'DATE'

               ELSE IF( USHOW( I : I ) .EQ. 'C' ) THEN
                  KEY = 'CREATOR'

               ELSE IF( USHOW( I : I ) .EQ. 'T' ) THEN
                  KEY = 'PARENTS'

               ELSE IF( USHOW( I : I ) .EQ. 'M' ) THEN
                  KEY = 'MORE'

               END IF

               IF( .NOT. AST_MAPGET0C( KYMAP2, KEY, VALUE, NC,
     :                                 STATUS ) ) THEN
                  VALUE = '<unknown>'
                  NC = 9
               END IF

               CALL MSG_SETC( 'K', KEY )
               CALL MSG_SETC( 'V', VALUE( : NC ) )
               CALL MSG_OUT( ' ', '   ^K: ^V', STATUS )

            END DO

         END IF

*  Annul the keymap holding details for this row.
         CALL AST_ANNUL( KYMAP2, STATUS )
      END DO

*  A final blank line.
      CALL MSG_BLANK( STATUS )

*  Arrive here if an error occurs.
 999  CONTINUE

*  Annul the NDF identifier.
      CALL NDF_ANNUL( INDF, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SHOWPROV_ERR', 'SHOWPROV: Failed to display '//
     :                 'provenance information in an NDF.', STATUS )
      END IF

      END
