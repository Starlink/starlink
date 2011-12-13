      SUBROUTINE FITSIMP( STATUS )
*+
*  Name:
*     FITSIMP

*  Purpose:
*     Imports FITS information into an NDF extension.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FITSIMP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application extracts the values of FITS keywords from a FITS
*     extension in an NDF and uses them to construct another NDF
*     extension.  The list of new extension components required, their
*     data types and the names of the FITS keywords from which to
*     derive their values are specified in a "keyword translation
*     table" held in a separate text file.

*  Usage:
*     fitsimp ndf table xname xtype

*  ADAM Parameters:
*     NDF = NDF (Read and Write)
*        The NDF in which the new extension is to be created.
*     TABLE = FILENAME (Read)
*        The text file containing the keyword translation table.  The
*        format of this file is described under "Table Format".
*     XNAME = LITERAL (Read)
*        The name of the NDF extension which is to receive the values
*        read from the FITS extension.  If this extension does not
*        already exist, then it will be created.  Otherwise, it should
*        be a scalar structure extension within which new components
*        may be created (existing components of the same name will be
*        over-written).  Extension names may contain up to 15
*        alpha-numeric characters, beginning with an alphabetic
*        character.
*     XTYPE = LITERAL (Read)
*        The HDS data type of the output extension.  This value will
*        only be required if the extension does not initially exist and
*        must be created.  New extensions will be created as scalar
*        structures.

*  Examples:
*     fitsimp datafile fitstable ccdinfo ccd_ext
*        Creates a new extension called CCDINFO (with a data type of
*        CCD_EXT) in the NDF structure called datafile.  Keyword values
*        are read from the NDF's FITS extension and written into the new
*        extension as separate components under control of a keyword
*        translation table held in the file fitstable.
*     fitsimp ndf=n1429 table=std_table xname=std_extn
*        FITS keyword values are read from the FITS extension in the
*        NDF structure n1429 and written into the pre-existing
*        extension STD_EXTN under control of the translation table
*        std_table.  Components which already exist within the
*        extension may be over-written by this process.

*  Related Applications:
*     KAPPA: FITSHEAD, FITSLIST, FITSDIN, FITSIN; CONVERT: FITS2NDF;
*     Figaro: RDFITS.

*  Implementation Deficiencies:
*     -  The imaginary part of complex FITS keyword values will be lost.
*     -  When the data type is _CHAR, the value associated with the
*     FITS keyword in the `card image' must be enclosed in quotes.

*  Table Format:
*     The keyword translation table should be held in a text file, with
*     one extension component specified per line.  Each line should
*     contain 3 fields, separated by spaces and/or tabs, as follows.
*
*     -  Field 1:
*        The name of the component in the output extension for which a
*        value is to be obtained.
*
*     -  Field 2:
*        The data type of the output component, to which the keyword
*        value will be converted (one of _INTEGER, _REAL, _DOUBLE,
*        _LOGICAL or _CHAR).
*
*     -  Field 3:
*        The name of the FITS keyword from which the value is to be
*        obtained.  Hierarchical keywords are permissible; the format
*        is concatenated keywords joined with full stops and no spaces,
*        e.g. HIERARCH.ESO.NTT.HUMIDITY, ING.DETHEAD.
*
*     Comments may appear at any point in the table and should begin
*     with an exclamation mark. The remainder of the line will then be
*     ignored.

*  Timing:
*     Approximately proportional to the number of FITS keywords to be
*     translated.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     28-FEB-1991 (RFWS):
*        Original version.
*     1991 July 15 (MJC):
*        Added hierarchical keywords.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'FIO_ERR'          ! FIO_ error codes
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER * ( 80 ) CVAL    ! Character FITS value
      CHARACTER * ( 48 ) COMENT  ! FITS comment
      CHARACTER * ( 132 ) LINE   ! Buffer for file reading
      CHARACTER * ( DAT__SZLOC ) LOC ! FITS extension locator
      CHARACTER * ( DAT__SZLOC ) LOCT ! Dummy locator
      CHARACTER * ( 1 ) TOK( 3 ) ! Buffer for parsing lines
      CHARACTER * ( DAT__SZTYP ) XTYPE ! New extension type
      CHARACTER * ( NDF__SZXNM ) XNAME ! New extension name
      DOUBLE PRECISION DVAL      ! Double precision FITS value
      INTEGER CARD               ! Fits header element (card) found
      INTEGER DIM( 1 )           ! Dummy dimension array
      INTEGER EL                 ! Number of FITS header records mapped
      INTEGER HIC                ! Position of comment character
      INTEGER I1( 3 )            ! Pointer to start of tokens
      INTEGER I2( 3 )            ! Pointer to end of tokens
      INTEGER IFIL               ! File descriptor
      INTEGER ILINE              ! Input line counter
      INTEGER INDF               ! NDF identifier
      INTEGER IVAL               ! Integer FITS value
      INTEGER LENGTH             ! Length of a character being mapped
      INTEGER LSTAT              ! Local status variable
      INTEGER NC                 ! Number of characters
      INTEGER NTOK               ! Number of tokens on line
      INTEGER PNTR( 1 )          ! Pointer to mapped FITS headers
      LOGICAL FOUND              ! Was a value found?
      LOGICAL LVAL               ! Logical FITS value
      LOGICAL THERE              ! Whether extension is already there
      REAL RVAL                  ! Real FITS value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the NDF to be updated.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', INDF, STATUS )

*  Find the FITS extension and map it.
      CALL NDF_XLOC( INDF, 'FITS', 'READ', LOC, STATUS )
      CALL DAT_MAPV( LOC, '_CHAR*80', 'READ', PNTR( 1 ), EL, STATUS )
      LENGTH = 80

*  Obtain the name of the output extension and see if it is already
*  there.
      CALL PAR_GET0C( 'XNAME', XNAME, STATUS )
      CALL NDF_XSTAT( INDF, XNAME, THERE, STATUS )

*  If not, then obtain its data type and create it (as a scalar
*  structure). Annul the extension locator, which is not needed.
      IF ( .NOT. THERE ) THEN
         CALL PAR_GET0C( 'XTYPE', XTYPE, STATUS )
         CALL NDF_XNEW( INDF, XNAME, XTYPE, 0, DIM, LOCT, STATUS )
         CALL DAT_ANNUL( LOCT, STATUS )
      END IF

*  Open the translation table file and defer error reporting.
      CALL FIO_ASSOC( 'TABLE', 'READ', 'LIST', 0, IFIL, STATUS )
      CALL ERR_MARK

*  loop to read from the file, counting the lines read.
      ILINE = 0
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      CALL FIO_READ( IFIL, LINE, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         ILINE = ILINE + 1

*  If the line is not blank, then look for a comment delimiter. If
*  found, adjust the line length to omit the comment and remove any
*  resulting trailing blanks.
         IF ( NC. NE. 0 ) THEN
            HIC = INDEX( LINE( : NC ), '!' )
            IF ( HIC .NE. 0 ) THEN
               NC = HIC - 1
               IF ( NC .GT. 0 ) NC = CHR_LEN( LINE( : NC ) )
            END IF
         END IF

*  Ignore lines which are now blank. Remove non-printable characters
*  (e.g. tabs) from the rest and convert to upper case.  Decompose the
*  line into tokens.
         IF ( NC .NE. 0 ) THEN
            CALL CHR_CLEAN( LINE( : NC ) )
            CALL CHR_UCASE( LINE( : NC ) )
            CALL CHR_DCWRD( LINE( : NC ), 3, NTOK, I1, I2, TOK, LSTAT )

*  Report an error if more than 3 tokens were found.
            IF ( LSTAT .NE. 0 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'FITSIMP_JUNK',
     :                       'Extra information at end of line.',
     :                       STATUS )

*  Report an error if fewer than 3 tokens were found.
            ELSE IF ( NTOK .LT. 3 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'FITSIMP_2FEW',
     :                       'Insufficient information on line.',
     :                       STATUS )

*  Handle each data type separately....
            ELSE

*  Character:
*  =========
*  Obtain a value for the keyword from the FITS header.  Note
*  the length of the character-array elements is passed by value
*  after the last genuine argument.  This is for UNIX and does not
*  harm on VMS.  The second character argument is no problem since
*  it is not passed by pointer.  Ditto for extract data of other types
*  from the FITS extension.
               IF ( LINE( I1( 2 ):I2( 2 ) ) .EQ. '_CHAR' ) THEN
                  CALL FTS1_GKEYC( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 1,
     :                             LINE( I1( 3 ):I2( 3 ) ), 1, FOUND,
     :                             CVAL, COMENT, CARD, STATUS,
     :                             %VAL( CNF_CVAL( LENGTH ) ) )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If a value was found, then find its length and store it in the
*  extension.
                     IF ( FOUND ) THEN
                        NC = MAX ( 1, CHR_LEN( CVAL ) )
                        CALL NDF_XPT0C( CVAL( : NC ), INDF, XNAME,
     :                                  LINE( I1( 1 ):I2( 1 ) ),
     :                                  STATUS )
                     END IF
                  END IF

*  Double precision:
*  ================
*  Obtain a value for the keyword from the FITS header.
               ELSE IF ( LINE( I1( 2 ):I2( 2 ) ) .EQ. '_DOUBLE' ) THEN
                  CALL FTS1_GKEYD( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 1,
     :                             LINE( I1( 3 ):I2( 3 ) ), 1, FOUND,
     :                             DVAL, COMENT, CARD, STATUS,
     :                             %VAL( CNF_CVAL( LENGTH ) ) )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If a value was found, then store it in the extension.
                     IF ( FOUND ) THEN
                        CALL NDF_XPT0D( DVAL, INDF, XNAME,
     :                                  LINE( I1( 1 ):I2( 1 ) ),
     :                                  STATUS )
                     END IF
                  END IF

*  Integer:
*  =======
*  Obtain a value for the keyword from the FITS header.
               ELSE IF ( LINE( I1( 2 ):I2( 2 ) ) .EQ. '_INTEGER' ) THEN
                  CALL FTS1_GKEYI( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 1,
     :                             LINE( I1( 3 ):I2( 3 ) ), 1, FOUND,
     :                             IVAL, COMENT, CARD, STATUS,
     :                             %VAL( CNF_CVAL( LENGTH ) ) )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If a value was found, then store it in the extension.
                     IF ( FOUND ) THEN
                        CALL NDF_XPT0I( IVAL, INDF, XNAME,
     :                                  LINE( I1( 1 ):I2( 1 ) ),
     :                                  STATUS )
                     END IF
                  END IF

*  Logical:
*  =======
*  Obtain a value for the keyword from the FITS header.
               ELSE IF ( LINE( I1( 2 ):I2( 2 ) ) .EQ. '_LOGICAL' ) THEN
                  CALL FTS1_GKEYL( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 1,
     :                             LINE( I1( 3 ):I2( 3 ) ), 1, FOUND,
     :                             LVAL, COMENT, CARD, STATUS,
     :                             %VAL( CNF_CVAL( LENGTH ) ) )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If a value was found, then store it in the extension.
                     IF ( FOUND ) THEN
                        CALL NDF_XPT0L( LVAL, INDF, XNAME,
     :                                  LINE( I1( 1 ):I2( 1 ) ),
     :                                  STATUS )
                     END IF
                  END IF

*  Real:
*  ====
*  Obtain a value for the keyword from the FITS header.
               ELSE IF ( LINE( I1( 2 ):I2( 2 ) ) .EQ. '_REAL' ) THEN
                  CALL FTS1_GKEYR( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 1,
     :                             LINE( I1( 3 ):I2( 3 ) ), 1, FOUND,
     :                             RVAL, COMENT, CARD, STATUS,
     :                             %VAL( CNF_CVAL( LENGTH ) ) )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If a value was found, then store it in the extension.
                     IF ( FOUND ) THEN
                        CALL NDF_XPT0R( RVAL, INDF, XNAME,
     :                                  LINE( I1( 1 ):I2( 1 ) ),
     :                                  STATUS )
                     END IF
                  END IF

*  If the data type was not recognised, then report an error.
               ELSE
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'TYPE', LINE( I1( 2 ):I2( 2 ) ) )
                  CALL ERR_REP( 'FITSIMP_TYPE',
     :              'Invalid data type ''^TYPE'' specified.', STATUS )
               END IF

*  If there has been no other error, then check if the keyword was
*  found. Report an error if it was not, but carry on anyway.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( .NOT. FOUND ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETC( 'KEYWORD', LINE( I1( 3 ):I2( 3 ) ) )
                     CALL ERR_REP( 'FITSIMP_ABSENT',
     :                 'A value for the FITS keyword '/
     :                 /'''^KEYWORD'' could not be found.', STATUS )
                     CALL ERR_FLUSH( STATUS )
                  END IF
               END IF
            END IF

*  If a fatal error has occurred, report which line of the translation
*  table it occurred in and display the line contents.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETI( 'ILINE', ILINE )
               CALL ERR_REP( 'FITSIMP_ILINE',
     :           'Error occurred in line ^ILINE of the '/
     :           /'keyword translation table $TABLE.', STATUS )
               CALL MSG_SETC( 'LINE', LINE )
               CALL ERR_REP( 'FITSIMP_LINE',
     :           'Line read was: ''^LINE''.', STATUS )
            END IF
         END IF
         GO TO 1
      END IF

*  Annul end-of-file errors, end the deferral of error reporting and
*  close the translation table file.
      IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE
      CALL FIO_CLOSE( IFIL, STATUS )

*  Annul (thereby unmapping) the FITS extension locator and the NDF
*  identifier.
      CALL DAT_ANNUL( LOC, STATUS )
      CALL NDF_ANNUL( INDF, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FITSIMP_ERR',
     :   'FITSIMP: Error importing FITS information into an NDF ' //
     :   'extension.',
     :   STATUS )
      END IF

      END
