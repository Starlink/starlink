      SUBROUTINE COF_WGBFE( FUNIT, FILE, NDF, PROFIT, OBSNO, NCOL, USED,
     :                      NHEAD, MXHEAD, HEADER, STATUS )
*+
*  Name:
*     COF_WGBFE

*  Purpose:
*     Creates the FITS extension for Green Bank convention binary table.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_WGBFE( FUNIT, FILE, NDF, PROFIT, OBSNO, NCOL, USED,
*                     NHEAD, MXHEAD, HEADER, STATUS )

*  Description:
*     This creates the FITS extension or `airlock' from a previously
*     opened FITS file containing a binary table using the Green Bank
*     convention.  The FITS extension will comprise information from two
*     parts of the FITS file.  First come the headers from the primary
*     header-and-data unit, which should be supplied in the header
*     array.  Following these, but before the END card, come FITS
*     header cards containing the values of columns of the nominated
*     row of the binary table that have been unused to form the NDF.
*     These additional headers use the TTYPEn values as their keywords.
*     If the primary header array supplied already contains such
*     keywords, the existing values are overwritten.  The headers are
*     then copied verbatim to the extension.
*
*     There is an option just to merge the headers without writing the
*     FITS extension.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the FITS file or tape device to appear in the
*        error messages.
*     NDF = INTEGER (Given)
*        The identifier for the NDF which is to have a new FITS
*        extension.  This argument is ignored if PROFIT = .FALSE..
*     PROFIT = LOGICAL (Given)
*        If .TRUE., the merged FITS headers are written to the NDF's
*        FITS extension.
*     OBSNO = INTEGER (Given)
*        The observation number.  This is equivalent to the row in the
*        binary table.
*     NCOL = INTEGER (Given)
*        Number of columns in the binary table (and dimension of USED).
*     USED( NCOL ) = INTEGER (Given)
*        A set of flags that indicate whether or not a column has been
*        used to define some attribute of the NDF.  Only those columns
*        for which USED is .FALSE. will appear in the FITS extension.
*     NHEAD = INTEGER (Given)
*        Number of headers from the primary HDU, supplied in HEADER.
*     MXHEAD = INTEGER (Given)
*        Maximum number of headers.
*     HEADER( MXHEAD ) = CHARACTER * ( 80 ) (Given and Returned)
*        On entry it contains the NHEAD headers from the primary HDU.
*        On exit it has the additional headers based on the unused
*        binary-table columns appended to and/or replacing the supplied
*        headers.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The FITS files must already be opened with the FITSIO library,
*     and the current HDU must be the desired binary table.
*
*     [optional_subroutine_items]...

*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     {enter_new_authors_here}

*  History:
*     1996 June 18 (MJC):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER FUNIT
      CHARACTER * ( * ) FILE
      INTEGER NDF
      LOGICAL PROFIT
      INTEGER OBSNO
      INTEGER NCOL
      LOGICAL USED( NCOL )
      INTEGER NHEAD
      INTEGER MXHEAD

*  Arguments Given and Returned:
      CHARACTER * ( 80 ) HEADER( MXHEAD )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a string less trailing
                                 ! blanks

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

      INTEGER HEDLEN             ! FITS header length
      PARAMETER( HEDLEN = 80 )

      INTEGER KEYLEN             ! FITS header keyword maximum length
      PARAMETER( KEYLEN = 8 )

*  Local Variables:
      LOGICAL BAD                ! True if column value is bad (dummy)
      CHARACTER * ( 10 ) BTYPE ! Column TFORM data type
      CHARACTER * ( HEDLEN ) BUFFER ! Buffer for a FITS header
      CHARACTER * ( HEDLEN ) CARD ! Buffer for a FITS header
      CHARACTER * ( KEYLEN ) COLNAM ! Column name and new keyword
      INTEGER CHEAD              ! Current final-header counter
      INTEGER COLNUM             ! Column loop counter
      CHARACTER * ( 48 ) COMENT  ! Keyword comment
      CHARACTER * ( 10 ) CON     ! Observation number for error reports
      INTEGER CPOS               ! Character position in keyword
                                 ! name
      CHARACTER * ( DAT__SZTYP ) CTYPE ! Column HDS data type
      CHARACTER * ( HEDLEN - KEYLEN - 4 ) CVALUE ! Column value
      DOUBLE PRECISION DVALUE    ! D.p. value
      CHARACTER * ( 48 ) DUMMY   ! Dummy keyword comment
      CHARACTER * ( 256 ) ERRBUF ! Buffer for error message
      CHARACTER * ( DAT__SZLOC ) FLOC ! Locator to the FITS extension
      INTEGER FSTAT              ! FITSIO status
      INTEGER HDTYPE             ! Dummy returned by FTTNUL
      CHARACTER * ( DAT__SZLOC ) HLOC ! Locator to a cell of the FITS
                                 ! extension
      INTEGER IHEAD              ! Loop counter for headers
      INTEGER IVALUE             ! Integer value
      LOGICAL LOOP               ! Duplicate header found?
      INTEGER J                  ! Loop counter for overwriting
                                 ! duplicate keywords
      CHARACTER * ( KEYLEN ) KEYWRD ! FITS keyword
      INTEGER NC                 ! Number of characters
      INTEGER NCF                ! Number of characters in the FITS file
                                 ! name
      INTEGER NCO                ! Number of characters in the
                                 ! observation number
      CHARACTER * ( 6 ) ROUTIN   ! Routine name for error
      REAL RVALUE                ! Real value
      LOGICAL THERE              ! TTYPEn is present


*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Get the length of the filename.
      NCF = CHR_LEN( FILE )

*  Convert the observation number to a string.  It is needed for error
*  messages.
      CALL CHR_ITOC( OBSNO, CON, NCO )

*  Initialise next header number where a new card can be appended.
      CHEAD = NHEAD + 1

*  Form additional headers.
*  ========================
*  Loop through all the binary-table fields, creating headers for
*  those not already used and wanted.
      DO COLNUM = 1, NCOL
         IF ( .NOT. USED( COLNUM ) ) THEN

*  Obtain the name of the column.  First form the name of the name
*  keyword for this column.
            CALL FTKEYN( 'TTYPE', COLNUM, KEYWRD, FSTAT )
            CALL COF_GKEYC( FUNIT, KEYWRD, THERE, COLNAM, COMENT,
     :                      STATUS )
            IF ( THERE ) THEN

*  Obtain its data type.
               CALL FTKEYN( 'TFORM', COLNUM, KEYWRD, FSTAT )
               CALL COF_GKEYC( FUNIT, KEYWRD, THERE, BTYPE, DUMMY,
     :                         STATUS )

*  Convert the binary table type into an HDS type.  Note we assume
*  that there are no strings.
               CTYPE = ' '
               IF ( THERE ) CALL COF_BN2HT( BTYPE, CTYPE, STATUS )

*  Create a dummy header.  Leave a space after the name which is limited
*  to eight characters.
               BUFFER = COLNAM( 1:KEYLEN )
               CPOS = KEYLEN + 2

*  Access the table using an appropriate data type.
               IF ( CTYPE( 1:5 ) .EQ. '_CHAR' ) THEN

*  Place strings in quotes.
                  CALL CHR_APPND( '''', BUFFER, CPOS )

*  Obtain the data value as a string.
                  CALL FTGCVS( FUNIT, COLNUM, OBSNO, 1, 1, ' ',
     :                         CVALUE, BAD, FSTAT )
                  ROUTIN = 'FTGCVS'

*  Access the table using an appropriate data type, and convert into
*  a string.
               ELSE IF ( CTYPE .EQ. '_UBYTE' .OR.
     :                   CTYPE .EQ. '_WORD' .OR.
     :                   CTYPE .EQ. '_INTEGER' ) THEN
                  CALL FTGCVJ( FUNIT, COLNUM, OBSNO, 1, 1, VAL__BADI,
     :                         IVALUE, BAD, FSTAT )
                  ROUTIN = 'FTGCVJ'
                  CALL CHR_ITOC( IVALUE, CVALUE, NC )

               ELSE IF ( CTYPE .EQ. '_REAL' ) THEN
                  CALL FTGCVE( FUNIT, COLNUM, OBSNO, 1, 1, VAL__BADR,
     :                         RVALUE, BAD, FSTAT )
                  ROUTIN = 'FTGCVE'
                  CALL CHR_RTOC( RVALUE, CVALUE, NC )

               ELSE IF ( CTYPE .EQ. '_DOUBLE' ) THEN
                  CALL FTGCVD( FUNIT, COLNUM, OBSNO, 1, 1, VAL__BADD,
     :                         DVALUE, BAD, FSTAT )
                  ROUTIN = 'FTGCVD'
                  CALL CHR_DTOC( DVALUE, CVALUE, NC )

               END IF

*  Report an contextual error message if something went wrong.
               IF ( FSTAT .NE. FITSOK ) THEN
                  ERRBUF = 'Error obtaining the value of keyword '/
     :                     /COLNAM//' for FITS file '//FILE( :NCF )/
     :                     /', observation '//CON( :NCO )//'.'
                  CALL COF_FIOER( FSTAT, 'COF_WGBFE_GETVAL', ROUTIN,
     :                            ERRBUF, STATUS )
                  GOTO 999
               END IF

*  Append the data value
               CALL CHR_APPND( CVALUE, BUFFER, CPOS )

*  Place strings in quotes.
               IF ( CTYPE( 1:5 ) .EQ. '_CHAR' )
     :           CALL CHR_APPND( '''', BUFFER, CPOS )

*  Append comment by appending the delimiter, shifting along obne for a
*  space after the delimiter, then appending the comment from the TTYPEn
*  card.
               CALL CHR_APPND( ' /', BUFFER, CPOS )
               CPOS = CPOS + 1
               CALL CHR_APPND( COMENT, BUFFER, CPOS )

*  Tidy the buffer to produce a bona fide FITS header card.
               CALL FTGTHD( BUFFER, CARD, HDTYPE, FSTAT )

*  Report an contextual error message if something went wrong.
               IF ( FSTAT .NE. FITSOK ) THEN
                  ERRBUF = 'Error generating the header card for '/
     :                     /'keyword '//COLNAM//' for FITS file '/
     :                     /FILE( :NCF )//', observation'//CON( :NCO )/
     :                     /'.'
                  CALL COF_FIOER( FSTAT, 'COF_WGBFE_GETVAL', 'FTGTHD',
     :                            ERRBUF, STATUS )
                  GOTO 999
               END IF

*  Search through the existing columns.  If a match is found, replace
*  the existing global header with the one of the same name, but for the
*  current observation.  Exit the loop once a match is found.
               LOOP = .TRUE.
               J = 1
               DO WHILE ( LOOP .AND. J .LE. NHEAD )
                  IF ( HEADER( J )( 1:8 ) .EQ. COLNAM ) THEN
                     LOOP = .FALSE.
                     HEADER( J ) = CARD
                  END IF
                  J = J + 1
               END DO

*  Append the new header at the end, if this is a new keyword in the
*  headers.  Increment the counters.
               IF ( LOOP ) THEN
                  HEADER( CHEAD ) = CARD
                  CHEAD = CHEAD + 1
               END IF

            END IF
         END IF
      END DO

*  Append the END card.
      HEADER( CHEAD ) = 'END'

*  Create the FITS extension.
*  ==========================
      IF ( PROFIT ) THEN

*  Create the FITS extension of the appropriate length.
         CALL NDF_XNEW( NDF, 'FITS', '_CHAR*80', 1, CHEAD, FLOC,
     :                  STATUS )

*  Loop through the headers excluding the END card.
         DO IHEAD = 1, CHEAD

*  Obtain a cell into the extension.  Put the header into the cell,
*  and tidy the temporary locator.
            CALL DAT_CELL( FLOC, 1, IHEAD, HLOC, STATUS )
            CALL DAT_PUT0C( HLOC, HEADER( IHEAD ), STATUS )
            CALL DAT_ANNUL( HLOC, STATUS )
         END DO

*  Tidy the locator to the extension.
         CALL DAT_ANNUL( FLOC, STATUS )
      END IF

*  Restore the bad status when something went wrong with the FITSIO
*  calls.
      IF ( FSTAT .GT. FITSOK ) STATUS = SAI__ERROR

  999 CONTINUE

      END
