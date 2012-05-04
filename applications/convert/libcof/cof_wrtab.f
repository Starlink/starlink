      SUBROUTINE COF_WRTAB( FUNITH, FUNITD, LOC, STATUS )
*+
*  Name:
*     COF_WRTAB

*  Purpose:
*     Converts an FITS binary or ASCII table into a <TABLE> structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_WRTAB( FUNITH, FUNITD, LOC, STATUS )

*  Description:
*     This routine converts a FITS binary-table or ASCII-table extension
*     into an HDS structure of data type <TABLE>.

*  Arguments:
*     FUNITH = INTEGER (Given)
*        Logical-unit number of the FITS file for headers.
*     FUNITD = INTEGER (Given)
*        Logical-unit number of the FITS file for data.
*     LOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the structure to contain the <TABLE>
*        structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The conversion from table columns to NDF objects in the TABLE
*     structure is as follows:
*
*        TTYPEn              Component name of nth COLUMNS structure,
*                            which is COLUMNn if TTYPEn is absent
*        Comment of TTYPEn   COMMENT component in nth COLUMN structure
*        TFORMn              Data type of DATA component in nth COLUMN
*                            structure (no TSCALn, TZEROn present)
*        TSCALn, TZEROn      Data scale and offset for DATA component in
*                            nth COLUMN structure, and hence data type
*        TUNITn              UNITS component of nth COLUMN structure

*  Prior Requirements:
*     -  The FITS file should already have been opened by FITSIO, and
*     the current HDU is a BINTABLE or TABLE extension.
*     [routine_prior_requirements]...

*  Copyright:
*     Copyright (C) 1996, 1998, 2000, 2004 Central Laboratory of the
*     Research Councils.
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     AJC: Alan Chipperfield (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1996 April 18 (MJC):
*        Original version.
*     1998 August 14 (MJC):
*        Fixed a bug where if a TTYPEn keyword were absent, there
*        could was a spurious error annulling an unused locator.  Use
*        COLUMNn as the component name in this case, so data are not
*        lost.
*     2000 August 25 (AJC):
*        Separate FUNITs for header and data (allows header merging)
*          if different units, set scaling explicitly
*        Use COF_ASC2HT (new) not COF_BN2HT if it's an ASCII table so
*          FITS format I -> _INTEGER not _WORD
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2012 April 30 (MJC):
*        Add 64-bit integer.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER FUNITH
      INTEGER FUNITD
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a string less trailing
                                 ! blanks
*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      LOGICAL BAD                ! Column array contains bad values?
      CHARACTER * ( 10 ) BTYPE   ! Column TFORM data type
      CHARACTER * ( DAT__SZLOC ) CLOC ! Locator to a COLUMN structure
      CHARACTER * ( 3 ) CNUM     ! Column number (1-999)
      CHARACTER * ( DAT__SZNAM ) COLNAM ! Column name
      CHARACTER * ( DAT__SZLOC ) COLLOC ! Locator to the COLUMNS
                                 ! structure
      INTEGER COLNUM             ! Column number
      CHARACTER * ( 48 ) COMENT  ! Comment describing column's meaning
      INTEGER CPOS               ! Character pointer in string
      CHARACTER * ( DAT__SZTYP ) CTYPE ! Column HDS data type
      INTEGER DATCOD             ! FITSIO data-type code
      CHARACTER * ( DAT__SZLOC ) DLOC ! Locator to the DATA component
      INTEGER DIMS( NDF__MXDIM ) ! Dimensions of the column
      INTEGER EL                 ! Number of rows in the table
      CHARACTER * ( 256 ) FILE   ! Filename
      CHARACTER * ( 12 ) FORM    ! Display format of the column
      INTEGER FSTAT              ! FITSIO status
      CHARACTER * ( 8 ) KEYWRD   ! FITS header keyword
      INTEGER NC                 ! Number of characters in string
      INTEGER NDIM               ! Dimensionality of a column
      INTEGER NFIELD             ! Number of fields in table
      INTEGER NV                 ! Number of values in a column
      INTEGER PNTR               ! Pointer to a mapped column array
      INTEGER REPEAT             ! Number of values in a field
      CHARACTER * ( 8 ) SCAKEY   ! TSCALn keyword name
      LOGICAL THERE              ! Header keyword is present?
      CHARACTER * ( DAT__SZTYP ) TYPE ! Type of a string component
      CHARACTER * ( 70 ) UNITS   ! Units of the column
      INTEGER WIDTH              ! Width of a character field
      CHARACTER * ( 70 ) VALUE   ! Keyword value
      CHARACTER * ( 8 )  ZERKEY  ! TZEROn keyword name
      LOGICAL SCAPRE             ! If scaling keyword present
      DOUBLE PRECISION SCALE     ! TSCALn value
      DOUBLE PRECISION OFFSET    ! RZEROn value
      CHARACTER * ( 20 ) XTENS   ! Type of FITS extension

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Define the shape of the Table.
*  ==============================

*  Obtain the number of elements.
      CALL COF_GKEYI( FUNITH, 'NAXIS2', THERE, EL, COMENT, STATUS )

*  Obtain the number of fields in the table.
      CALL COF_GKEYI( FUNITH, 'TFIELDS', THERE, NFIELD, COMENT, STATUS )

*  Create and assign the NROWS component.
      CALL DAT_NEW( LOC, 'NROWS', '_INTEGER', 0, 0, STATUS )
      CALL CMP_PUT0I( LOC, 'NROWS', EL, STATUS )

*  Create the COLUMNS structure, and obtain a locator to it.
      CALL DAT_NEW( LOC, 'COLUMNS', 'COLUMNS', 0, 0, STATUS )
      CALL DAT_FIND( LOC, 'COLUMNS', COLLOC, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Write each column to the data structure.
*  ========================================

*  Loop through all the fields.
      DO COLNUM = 1, NFIELD

*  Find the column name and create the <COLUMN> component.
*  =======================================================

*  Obtain the name of the column.  First form the name of the name
*  keyword for this column.
         CALL FTKEYN( 'TTYPE', COLNUM, KEYWRD, FSTAT )
         CALL COF_GKEYC( FUNITH, KEYWRD, THERE, COLNAM, COMENT, STATUS )

*  Set a default name (COLUMNn) if the column name is absent.  The
*  TTYPEn keyword is only reserved, not mandatory.
         IF ( .NOT. THERE ) THEN
            CALL CHR_ITOC( COLNUM, CNUM, NC )
            COLNAM = 'COLUMN'
            CPOS = 6
            CALL CHR_APPND( CNUM, COLNAM, CPOS )
         END IF

*  Create the component of the extension, and get a locator to the
*  component.
         CALL DAT_NEW( COLLOC, COLNAM, 'COLUMN', 0, 0, STATUS )
         CALL DAT_FIND( COLLOC, COLNAM, CLOC, STATUS )

*  Find the description of the column and create the COMMENT component.
*  ====================================================================

*  Obtain the comment string of the TTYPEn keyword.
         IF ( THERE ) THEN
            CALL FTGKEY( FUNITH, KEYWRD, VALUE, COMENT, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Annul active locators.
            IF ( FSTAT .GT. FITSOK ) THEN
               CALL COF_FIOER( FSTAT, 'COF_WRTAB_COMMENT', 'FTGKEY',
     :           'Error obtaining the comment for column '//KEYWRD,
     :           STATUS )

               CALL DAT_ANNUL( CLOC, STATUS )
               CALL DAT_ANNUL( COLLOC, STATUS )
               GOTO 999
            END IF

*  Find the length of the string.  Create the HDS type including this
*  length.
            IF ( COMENT .NE. ' ' ) THEN
               NC = CHR_LEN( COMENT )
               CPOS = 6
               TYPE = '_CHAR*'
               CALL CHR_PUTI( NC, TYPE, CPOS )

*  Create and assign the COMMENT component.
               CALL DAT_NEW( CLOC, 'COMMENT', TYPE, 0, 0, STATUS )
               CALL CMP_PUT0C( CLOC, 'COMMENT', COMENT, STATUS )
            END IF
         END IF

*  Find the data type of the output data column.
*  =============================================

*  Determine whether or not scaling is to be applied.  Find the data
*  type required for the output array based upon the number of
*  significant digits in the TSCALn and TZEROn keywords.  If these have
*  values of 1.0D0 and 0.0D0 respectively either explicitly, or because
*  one or both are absent, then the data type can be set to the null
*  string.  This instructs later code to use the data type specified by
*  the TFORMn keyword or the default _REAL when there is no TFORMn
*  keyword.
         CALL FTKEYN( 'TSCAL', COLNUM, SCAKEY, FSTAT )
         CALL FTKEYN( 'TZERO', COLNUM, ZERKEY, FSTAT )
         CTYPE = ' '
         CALL COF_DSTYP( FUNITH, SCAKEY, ZERKEY, CTYPE, STATUS )

*  Obtain the data type of the column, when the type has not been
*  specificed by the presence of TSCALn and TZEROn keywords.
         IF ( CTYPE .EQ. ' ' ) THEN
            CALL FTKEYN( 'TFORM', COLNUM, KEYWRD, FSTAT )
            CALL COF_GKEYC( FUNITH, KEYWRD, THERE, BTYPE, COMENT,
     :                      STATUS )

*  Convert the table type into an HDS type.
            IF ( THERE ) THEN
               CALL COF_GKEYC( FUNITD, 'XTENSION', THERE, XTENS,
     :                         COMENT, STATUS )

               IF ( XTENS .EQ. 'TABLE' ) THEN
                  CALL COF_ASC2HT( BTYPE, CTYPE, STATUS )

               ELSE
                  CALL COF_BN2HT( BTYPE, CTYPE, STATUS )

               END IF
            ELSE
               CTYPE = '_REAL'
            END IF
         END IF

*  If the header and data FUNITs are different, set the scaling factors
*  explicitly for the data unit according to the header unit just in case
*  it inherited them from the primary.
         IF ( FUNITH .NE. FUNITD ) THEN
            CALL COF_GKEYD(
     :         FUNITH, 'BSCALE', SCAPRE, SCALE, COMENT, STATUS )
            IF ( .NOT. SCAPRE ) SCALE = 1.0D0

            CALL COF_GKEYD(
     :         FUNITH, 'BZERO', SCAPRE, OFFSET, COMENT, STATUS )
            IF ( .NOT. SCAPRE ) OFFSET = 0.0D0
            CALL FTPSCL( FUNITD, SCALE, OFFSET, FSTAT )
         END IF

*  Obtain the repeat count and width if it's a string.
         CALL FTGTCL( FUNITH, COLNUM, DATCOD, REPEAT, WIDTH, FSTAT )

*  Check for an error.  Flush the error stack.
         IF ( FSTAT .GT. FITSOK ) THEN
            CALL COF_FIOER( FSTAT, 'COF_WRTAB_SHAPE', 'FTGTCL',
     :        'Error obtaining the shape of column '//KEYWRD,
     :        STATUS )
            GOTO 999
         END IF

*  Append a string's length to the HDS type.
         IF ( CTYPE( 1:5 ) .EQ. '_CHAR' ) THEN
            CTYPE = '_CHAR*'
            CPOS = 6
            CALL CHR_PUTI( WIDTH, CTYPE, CPOS )

*  Redefine the repeat count to be the number of elements in each entry
*  in the column.
            REPEAT = REPEAT / WIDTH
         END IF

*  Define the dimensions of the column.
         IF ( REPEAT .GT. 1 ) THEN
            NDIM = 2
            DIMS( 1 ) = REPEAT
            DIMS( 2 ) = EL
            NV = REPEAT * EL
         ELSE
            NDIM = 1
            DIMS( 1 ) = EL
            NV = EL
         END IF

*  Create the component of the extension, and get a locator to the
*  component.
         CALL DAT_NEW( CLOC, 'DATA', CTYPE, NDIM, DIMS, STATUS )
         CALL DAT_FIND( CLOC, 'DATA', DLOC, STATUS )

*  Map the component for writing.
         IF ( CTYPE( 1:5 ) .EQ. '_CHAR' ) THEN
            CALL DAT_MAPC( DLOC, 'WRITE', NDIM, DIMS, PNTR, STATUS )
         ELSE
            CALL DAT_MAP( DLOC, CTYPE, 'WRITE', NDIM, DIMS, PNTR,
     :                    STATUS )
         END IF

*  Read the column into the data array.  Call the appropriate routine
*  for the chosen type.  Null values are substituted with magic values,
*  except for strings, where the exisiting null value is retained
*  verbatim.
         IF ( CTYPE .EQ. '_UBYTE' ) THEN
            CALL FTGCVB( FUNITD, COLNUM, 1, 1, NV, VAL__BADUB,
     :                   %VAL( CNF_PVAL( PNTR ) ), BAD, FSTAT )

         ELSE IF ( CTYPE .EQ. '_WORD' ) THEN
            CALL FTGCVI( FUNITD, COLNUM, 1, 1, NV, VAL__BADW,
     :                   %VAL( CNF_PVAL( PNTR ) ), BAD, FSTAT )

         ELSE IF ( CTYPE .EQ. '_INTEGER' ) THEN
            CALL FTGCVJ( FUNITD, COLNUM, 1, 1, NV, VAL__BADI,
     :                   %VAL( CNF_PVAL( PNTR ) ), BAD, FSTAT )

         ELSE IF ( CTYPE .EQ. '_INT64' ) THEN
            CALL FTGCVK( FUNITD, COLNUM, 1, 1, NV, VAL__BADI,
     :                   %VAL( CNF_PVAL( PNTR ) ), BAD, FSTAT )

         ELSE IF ( CTYPE .EQ. '_REAL' ) THEN
            CALL FTGCVE( FUNITD, COLNUM, 1, 1, NV, VAL__BADR,
     :                   %VAL( CNF_PVAL( PNTR ) ), BAD, FSTAT )

         ELSE IF ( CTYPE .EQ. '_DOUBLE' ) THEN
            CALL FTGCVD( FUNITD, COLNUM, 1, 1, NV, VAL__BADD,
     :                   %VAL( CNF_PVAL( PNTR ) ), BAD, FSTAT )

         ELSE IF ( CTYPE( 1:5 ) .EQ. '_CHAR' ) THEN
            CALL FTGCVS( FUNITD, COLNUM, 1, 1, NV, ' ',
     :                   %VAL( CNF_PVAL( PNTR ) ), BAD, FSTAT,
     :                   %VAL( CNF_CVAL( 1 ) ),
     :                   %VAL( CNF_CVAL( WIDTH ) ) )

         END IF

*  Tidy the locator to the DATA component.
         CALL DAT_ANNUL( DLOC, STATUS )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Annul active locators.
         IF ( FSTAT .GT. FITSOK ) THEN
            CALL COF_FIOER( FSTAT, 'COF_WRTAB_VALUES', 'FTGCVx',
     :        'Error writing the values for column '//KEYWRD,
     :        STATUS )

            CALL DAT_ANNUL( CLOC, STATUS )
            CALL DAT_ANNUL( COLLOC, STATUS )
            GOTO 999
         END IF

*  Obtain the units associated with the column.
         CALL FTKEYN( 'TUNIT', COLNUM, KEYWRD, FSTAT )
         CALL COF_GKEYC( FUNITH, KEYWRD, THERE, UNITS, COMENT, STATUS )

         IF ( THERE ) THEN

*  Find the length of the string.  Create the HDS type including this
*  length.  Do not create a units component when the value is blank.
            NC = CHR_LEN( UNITS )
            IF ( NC .GT. 0 ) THEN
               CPOS = 6
               TYPE = '_CHAR*'
               CALL CHR_PUTI( NC, TYPE, CPOS )

*  Create and assign the UNITS component.
               CALL DAT_NEW( CLOC, 'UNITS', TYPE, 0, 0, STATUS )
               CALL CMP_PUT0C( CLOC, 'UNITS', UNITS, STATUS )
            END IF
         END IF

*  Obtain the display format associated with the column.
         CALL FTKEYN( 'TDISP', COLNUM, KEYWRD, FSTAT )
         CALL COF_GKEYC( FUNITH, KEYWRD, THERE, FORM, COMENT, STATUS )

         IF ( THERE ) THEN

*  Find the length of the string.  Create the HDS type including this
*  length.  Do not create a format component when the value is blank.
            NC = CHR_LEN( FORM )
            IF ( NC .GT. 0 ) THEN
               CPOS = 6
               TYPE = '_CHAR*'
               CALL CHR_PUTI( NC, TYPE, CPOS )

*  Create and assign the FORMAT component.
               CALL DAT_NEW( CLOC, 'FORMAT', TYPE, 0, 0, STATUS )
               CALL CMP_PUT0C( CLOC, 'FORMAT', FORM, STATUS )
            END IF
         END IF

*  Tidy the locator to the current COLUMN structure.
         CALL DAT_ANNUL( CLOC, STATUS )

*  Exit if something has gone wrong.  Use an error context to help
*  pinpoint the problem.  Tidy the locator.
         IF ( STATUS .NE. SAI__OK ) THEN
            INQUIRE( UNIT=FUNITD, NAME=FILE )
            CALL MSG_SETI( 'COLUMN', COLNUM )
            CALL MSG_SETC( 'FILE', FILE )
            CALL ERR_REP( 'COF_WRTAB_ERR',
     :        'An error occurred when transferring column ^COLUMN of '/
     :        /'a FITS table in ^FILE.', STATUS )
            CALL DAT_ANNUL( COLLOC, STATUS )
            GOTO 999
         END IF

      END DO

*  Tidy the locator to the COLUMNS structure.
      CALL DAT_ANNUL( COLLOC, STATUS )

  999 CONTINUE

      END
