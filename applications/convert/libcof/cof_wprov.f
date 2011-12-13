      SUBROUTINE COF_WPROV( NDF, FUNIT, STATUS )
*+
*  Name:
*     COF_WPROV

*  Purpose:
*     Writes general provenance records to the current FITS header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_WPROV( NDF, FUNIT, STATUS )

*  Description:
*     This creates headers in the current FITS header that record the
*     provenance information stored in the supplied NDF.
*
*     The tabulated indexed headers below, all with string values, are
*     written for each ancestor NDF.  The keyword index n is the
*     provenanxce identifier for each NDF, starting at 0 corresponding
*     to the current NDF.  All have value '<unknown>' if the information
*     could not be found, except for MORE information; the PRVMn header
*     is omitted if there is no MORE information to record.
*
*     Keyword   Comment
*     -------   -------
*     PRVPn     Path of the <nth> parent
*     PRVIn     Identifiers of direct parents for <nth> ancestor
*     PRVDn     Creation date of <nth> parent
*     PRVCn     Creator software of <nth> parent
*     PRVMn     Contents of MORE of <nth> parent
*
*     where <nth> is the appropriate ordinal string, e.g. 1st, 2nd,
*     3rd, 4th.
*
*     The above headers are prefaced by a blank header and a title
*     "Provenance:" comment.  There is a blank header between each set
*     for improved legibility.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF whose PROVENANCE is to be written to
*        the FITS headers.
*     FUNIT = INTEGER (Given)
*        The logical unit number of the output FITS file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Supports up to 9999 ancestors.
*     -  The PRVPn, PRVIn, and PRVMn keywords may often need more than
*     the maximum 68 characters allowed in a keyword character value.
*     Therefore this routine uses the non-standard Long-string Keyword
*     Convention that permits long strings to continue over multiple
*     headers.  If any of the aforementioned keywords' values exceeds 68
*     characters, this routine adds a warning in some COMMENT headers
*     and writes a LONGSTRN header, whose value is the version of the
*     convention being used.

*  Prior Requirements:
*     The NDF and the FITS file must already be open.  The current
*     HDU in the FITS file should be the primary and the standard
*     headers should have been written.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008 February 5 (MJC):
*        Original version, adapting some code from DSB's PROVSHOW.
*     29-JUN-2009 (DSB):
*        Change to use NDG_READPROV and NDG_FREEPROV.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER NDF
      INTEGER FUNIT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      CHARACTER*2 CHR_NTH        ! Ordinal string

*  Local Constants:
      INTEGER FITSOK             ! Good status for FITSIO library
      PARAMETER ( FITSOK = 0 )

*  Local Variables:
      CHARACTER*47 ANCCOM        ! Ancestor header comment
      CHARACTER*80 CARD          ! FITS header card
      INTEGER CPOS               ! Current string position
      INTEGER FSTAT              ! FITSIO status
      CHARACTER*10 ID            ! Integer index for the current NDF
      INTEGER IDENT              ! Identifier undex for the current NDF
      INTEGER IPROV              ! Identifier for provenance structure
      INTEGER IREC               ! Loop counter for provenance records
      CHARACTER*4 KEY            ! Current key in KeyMap of root anc.
      CHARACTER*8 KEYWRD         ! Header keyword
      INTEGER KYMAP1             ! AST KeyMap holding all prov info
      INTEGER KYMAP2             ! AST KeyMap holding field widths
      INTEGER NC                 ! Character length
      INTEGER NCMAX              ! Maximum character length
      INTEGER NROW               ! Number of provenance records
      LOGICAL PRVPRS             ! PROVENANCE present?
      CHARACTER*512 VALUE        ! Buffer for one field value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  First check that there is provenance to record.
      CALL NDF_XSTAT( NDF, 'PROVENANCE', PRVPRS, STATUS )
      IF ( PRVPRS ) THEN

*  Write a title for the block of provenance headers.
         CARD = ' '
         CPOS = 31
         CALL CHR_APPND( '/ Provenance:', CARD, CPOS )
         CALL FTPREC( FUNIT, CARD, FSTAT )

*  Format the provenance information in the NDF.  The resulting strings
*  are returned in an AST KeyMap.
        CALL NDG_READPROV( NDF, ' ', IPROV, STATUS )
        CALL NDG_FORMATPROV( IPROV, .FALSE., KYMAP1, STATUS )
        CALL NDG_FREEPROV( IPROV, STATUS )

*  Get the number of entries in the returned KeyMap.  This will be one
*  more than the number of NDFs described in the displayed table.
         NROW = AST_MAPSIZE( KYMAP1, STATUS ) - 1
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Loop round each NDF to be described.
*  ====================================
         NCMAX = -1
         DO IREC = 1, NROW

*  Get the KeyMap holding details for this row.
            CALL CHR_ITOC( IREC - 1, KEY, NC )
            IF ( .NOT. AST_MAPGET0A( KYMAP1, KEY( : NC ), KYMAP2,
     :                               STATUS ) ) THEN
               STATUS = SAI__ERROR
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL MSG_SETI( 'I', IREC )
                  CALL ERR_REP( ' ', 'No "^I" entry found in KeyMap '//
     :                          'returned by NDG_FMPRV (programming '//
     :                          'error).', STATUS )
               END IF
               GO TO 999
            END IF

*  Obtain the identifier for the current row.  This is recorded
*  through its use in the number of the indexed keywords.
            ID = ' '
            IF ( .NOT. AST_MAPGET0C( KYMAP2, 'ID', ID, NC,
     :                               STATUS ) ) THEN
               IF ( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'No "ID" entry found in KeyMap '//
     :                          'returned by NDG_FMPRV (programming '//
     :                          'error).', STATUS )
               END IF
               GO TO 999
            END IF
            CALL CHR_CTOI( ID, IDENT, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Write PATH information.
*  =======================

*  The first header records the NDF path.
            VALUE = ' '
            IF ( .NOT. AST_MAPGET0C( KYMAP2, 'PATH', VALUE, NC,
     :                               STATUS ) ) THEN
               VALUE = '<unknown>'
               NC = 9
            END IF
            NCMAX = MAX( NC, NCMAX )

*  Form keyword without leading zeroes (the FITS Standard says it shall
*  be done this way).
            KEYWRD = 'PRVP'
            CPOS = 4
            CALL CHR_APPND( ID, KEYWRD, CPOS )

*  Form comment.
            ANCCOM = 'Path of the '
            CPOS = 12
            CALL CHR_APPND( ID, ANCCOM, CPOS )
            CALL CHR_APPND( CHR_NTH( IDENT ), ANCCOM, CPOS )
            CALL CHR_APPND( ' parent', ANCCOM, CPOS )

*  Write the PRVPn header.  Use the longstring convention as the PATH
*  may well be longer than 68 characters.
            CALL FTPKLS( FUNIT, KEYWRD, VALUE( :NC ),
     :                   ANCCOM( :CPOS ), FSTAT )


*  Write identifiers of the immediate-parent NDFs.
*  ===============================================

*  The next header records the list of identifiers for the
*  immediate-parent NDFs.
            VALUE = ' '
            IF ( .NOT. AST_MAPGET0C( KYMAP2, 'PARENTS', VALUE, NC,
     :                               STATUS ) ) THEN
               VALUE = '<unknown>'
               NC = 9
            END IF
            NCMAX = MAX( NC, NCMAX )

*  Form keyword without leading zeroes.
            KEYWRD = 'PRVI'
            CPOS = 4
            CALL CHR_APPND( ID, KEYWRD, CPOS )

*  Form comment.
            ANCCOM = 'Identifiers of direct parents for '
            CPOS = 34
            CALL CHR_APPND( ID, ANCCOM, CPOS )
            CALL CHR_APPND( CHR_NTH( IDENT ), ANCCOM, CPOS )
            CALL CHR_APPND( ' ancestor', ANCCOM, CPOS )

*  Write the PRVIn header.  Use the longstring convention as the list
*  may well be longer than 68 characters.
            CALL FTPKLS( FUNIT, KEYWRD, VALUE( :NC ),
     :                   ANCCOM( :CPOS ), FSTAT )

*  Write the provenance date.
*  ==========================

*  The next header records the date at which the provenance was stored
*  in the NDF.
            VALUE = ' '
            IF ( .NOT. AST_MAPGET0C( KYMAP2, 'DATE', VALUE, NC,
     :                               STATUS ) ) THEN
               VALUE = '<unknown>'
               NC = 9
            END IF

*  Form keyword without leading zeroes.
            KEYWRD = 'PRVD'
            CPOS = 4
            CALL CHR_APPND( ID, KEYWRD, CPOS )

*  Form comment.
            ANCCOM = 'Creation date of '
            CPOS = 17
            CALL CHR_APPND( ID, ANCCOM, CPOS )
            CALL CHR_APPND( CHR_NTH( IDENT ), ANCCOM, CPOS )
            CALL CHR_APPND( ' parent', ANCCOM, CPOS )

*  Write the PRVDn header.
            CALL FTPKYS( FUNIT, KEYWRD, VALUE( :NC ),
     :                   ANCCOM( :CPOS ), FSTAT )

*  Write the name of the software that created the NDF.
*  ====================================================

*  The next header records the software that created the NDF.
            VALUE = ' '
            IF ( .NOT. AST_MAPGET0C( KYMAP2, 'CREATOR', VALUE, NC,
     :                               STATUS ) ) THEN
               VALUE = '<unknown>'
               NC = 9
            END IF

*  Form keyword without leading zeroes.
            KEYWRD = 'PRVC'
            CPOS = 4
            CALL CHR_APPND( ID, KEYWRD, CPOS )

*  Form comment.
            ANCCOM = 'Creator software of '
            CPOS = 20
            CALL CHR_APPND( ID, ANCCOM, CPOS )
            CALL CHR_APPND( CHR_NTH( IDENT ), ANCCOM, CPOS )
            CALL CHR_APPND( ' parent', ANCCOM, CPOS )

*  Write the PRVCn header.
            CALL FTPKYS( FUNIT, KEYWRD, VALUE( :NC ),
     :                   ANCCOM( :CPOS ), FSTAT )

*  Write summary of the contents of MORE.
*  ======================================

*  Next header recrds a summary of any extra information describing the
*  NDF.
            VALUE = ' '
            IF ( AST_MAPGET0C( KYMAP2, 'MORE', VALUE, NC,
     :                         STATUS ) ) THEN
               NCMAX = MAX( NC, NCMAX )

*  Form keyword without leading zeroes.
               KEYWRD = 'PRVM'
               CPOS = 4
               CALL CHR_APPND( ID, KEYWRD, CPOS )

*  Form comment.
               ANCCOM = 'Contents of MORE of '
               CPOS = 20
               CALL CHR_APPND( ID, ANCCOM, CPOS )
               CALL CHR_APPND( CHR_NTH( IDENT ), ANCCOM, CPOS )
               CALL CHR_APPND( ' parent', ANCCOM, CPOS )

*  Write the PRVMn header.  Use the longstring convention as there
*  may well be several items totalling more than 68 characters.
               CALL FTPKLS( FUNIT, KEYWRD, VALUE( :NC ),
     :                      ANCCOM( :CPOS ), FSTAT )

            END IF

*  Write a blank header.  The final blank card seems to be moved by
*  FITSIO to before the other provenance headers, hence there is
*  no initial blank card written before the title comment card.
            CARD = ' '
            CALL FTPREC( FUNIT, CARD, FSTAT )

*  Annul the keymap holding details for this row.
            CALL AST_ANNUL( KYMAP2, STATUS )
         END DO

* Record the fact that the longstring convention is being used.
         IF ( NCMAX .GT. 68 ) CALL FTPLSW( FUNIT, FSTAT )

      END IF

*  Arrive here if an error occurs.
  999 CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Check for an error.  Handle a bad status.  Negative values are
*  reserved for non-fatal warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_WPROV_ERR', 'FTPKYS',
     :                   'Error writing provenance header card.',
     :                   STATUS )
      END IF

      END
