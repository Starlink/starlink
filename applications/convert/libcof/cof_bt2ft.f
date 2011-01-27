      SUBROUTINE COF_BT2FT( FUNIT, EXTNAM, EXTVER, EXTLEVEL, TABLE,
     :                      STATUS )
*+
*  Name:
*     COF_BT2FT

*  Purpose:
*     Creates an AST FitsTable from a FITS binary table.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_BT2FT( FUNIT, EXTNAM, EXTVER, EXTLEVEL, TABLE, STATUS )

*  Description:
*     This function creates a new AST FitsTable object holding the data
*     read from a FITS binary table held in a named extension of the
*     supplied FITS file.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     EXTNAM = CHARACTER * ( * ) (Given)
*        The name of the FITS extension containing the required binary
*        table.
*     EXTVER = INTEGER (Given)
*        The value of the EXTVER keyword in the required binary-table
*        HDU.
*     EXTLEVEL = INTEGER (Given)
*        The value of the EXTLEVEL keyword in the required binary-table
*        HDU.
*     TABLE = INTEGER (Returned)
*        A pointer to the new FitsTable. A value of AST__NULL is
*        returned (without error) if the named extension cannot be
*        found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JAN-2011 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST_ public constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER ( FITSOK = 0 )
      INTEGER BAD_HDU_NUM        ! "No extension found" FITSIO status
      PARAMETER ( BAD_HDU_NUM = 301 )

*  Arguments Given:
      INTEGER FUNIT
      CHARACTER EXTNAM*(*)
      INTEGER EXTVER
      INTEGER EXTLEVEL

*  Arguments Returned:
      INTEGER TABLE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CARD*80          ! A formatted header card
      CHARACTER CNAME*50         ! Column name
      CHARACTER NAME*50          ! Keyword or attribute name
      CHARACTER TFORM*30         ! TFORM keyword value for current column
      INTEGER CLEN               ! Length of each fixed-length string
      INTEGER CTYPE              ! Column data type
      INTEGER DELIM              ! Index of delimiter at end of repeat count
      INTEGER FSTAT              ! FITSIO error status
      INTEGER FSTAT2             ! FITSIO error status
      INTEGER HDUTYPE            ! Type of HDU
      INTEGER HEADER             ! FitsChan holding extension's headers
      INTEGER IAT                ! Current used length of a string
      INTEGER ICOL               ! Index of current column
      INTEGER IHDU0              ! Index of current HDU on entry
      INTEGER IP                 ! Pointer to memory holding column data
      INTEGER IPASS              ! Loop count
      INTEGER NCOL               ! Number of columns in the table
      INTEGER NEL                ! Number of elements in each cell
      INTEGER NROW               ! Number of rows in the table
      INTEGER REPEAT             ! TFORM repeat count
      INTEGER SIZE               ! No of bytes need to hold column data
      INTEGER TOTNEL             ! Total number of elements to read
      LOGICAL ANYF               ! Were any null values read?
*.

*  Initialise
      TABLE = AST__NULL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin a new AST context.
      CALL AST_BEGIN( STATUS )

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Get the number of the current HDU in the FITS file (primary array
*  = 1) so that we can re-instate it at the end.
      CALL FTGHDN( FUNIT, IHDU0 )

*  Move to the first HDU that holds a binary table with the requested
*  name. The current HDU will remain unchanged if no suitable extension
*  is found.
      CALL FTMNHD( FUNIT, 2, EXTNAM, EXTVER, FSTAT )

*  Do nothing more if a suitable extension was not found.
      IF( FSTAT .EQ. FITSOK ) THEN

*  Create a FitsChan to hold the headers in the extension HDU.
         HEADER = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )

*  Extract the headers from the HDU and store them in the above
*  FitsChan.
         CALL COF_HD2FC( FUNIT, HEADER, STATUS )

*  Since the FITSIO "FTGCF<x>" routine applies any required scaling
*  (specified by the column's TSCALn and TZEROn header values) to the
*  returned column values, the values stored in the FitsTable will be
*  unscaled, and so no TSCALn and TZEROn headers should be stored in the
*  FitsTable. Delete such headers now.
         DO IPASS = 1, 2

            IF( IPASS .EQ. 1 ) THEN
               NAME = 'TSCAL%d'
            ELSE
               NAME = 'TZERO%d'
            END IF

            CALL AST_CLEAR( HEADER, 'Card', STATUS )
            DO WHILE( AST_FINDFITS( HEADER, NAME, CARD, .FALSE.,
     :                              STATUS ) )
               CALL AST_DELFITS( HEADER, STATUS )
            END DO

         END DO

*  Get the number of rows in the table.
         IF( .NOT. AST_GETFITSI( HEADER, 'NAXIS2', NROW, STATUS )
     :       .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Keyword NAXIS2 not found in '//
     :                    'binary table header.', STATUS )
         END IF

*  Create a FitsTable, creating columns equivalent to those in the
*  extension header.
         TABLE = AST_FITSTABLE( HEADER, ' ', STATUS )

*  Copy the data for each column from the binary table into the
*  FitsTable. Loop over all columns.
         NCOL = AST_GETI( TABLE, 'NColumn', STATUS )
         DO ICOL = 1, NCOL

*  Get the column name.
            CNAME = AST_COLUMNNAME( TABLE, ICOL, STATUS )

*  Get the column data type.
            NAME = 'ColumnType('
            IAT = 11
            CALL CHR_APPND( CNAME, NAME, IAT )
            CALL CHR_APPND( ')', NAME, IAT )
            CTYPE = AST_GETI( TABLE, NAME( : IAT ), STATUS )

*  Get the total number of elements to be read. If a column holds vector
*  values, then each cell in the column will hold more than one element.
            NAME = 'ColumnLength('
            IAT = 13
            CALL CHR_APPND( CNAME, NAME, IAT )
            CALL CHR_APPND( ')', NAME, IAT )
            NEL = AST_GETI( TABLE, NAME( : IAT ), STATUS )
            TOTNEL = NROW*NEL

*  Do each data type in turn. First four-byte integer.
            IF( CTYPE .EQ. AST__INTTYPE ) THEN

*  Allocate memory to store the column values.
               SIZE = VAL__NBI*TOTNEL
               CALL PSX_MALLOC( SIZE, IP, STATUS )

*  Read the FITS file to get all the data values in the column, storing
*  them in the above memory. The FitsTable will have inherited the null
*  value from the FITS header, and so we do not need to replace null
*  values in the following call.
               CALL FTGCVJ( FUNIT, ICOL, 1, 1, TOTNEL, 0,
     :                      %VAL( CNF_PVAL( IP ) ), ANYF, FSTAT )

*  Store the column values in the FitsTable.
               CALL AST_PUTCOLUMNDATA( TABLE, CNAME, 0, SIZE,
     :                                 %VAL( CNF_PVAL( IP ) ), STATUS )

*  Free the memory.
               CALL PSX_FREE( IP, STATUS )

*  Do the same for two-byte integer.
            ELSE IF( CTYPE .EQ. AST__SINTTYPE ) THEN
               SIZE = VAL__NBW*TOTNEL
               CALL PSX_MALLOC( SIZE, IP, STATUS )
               CALL FTGCVI( FUNIT, ICOL, 1, 1, TOTNEL, 0,
     :                      %VAL( CNF_PVAL( IP ) ), ANYF, FSTAT )
               CALL AST_PUTCOLUMNDATA( TABLE, CNAME, 0, SIZE,
     :                                 %VAL( CNF_PVAL( IP ) ), STATUS )
               CALL PSX_FREE( IP, STATUS )

*  Do the same for one-byte unsigned integer.
            ELSE IF( CTYPE .EQ. AST__BYTETYPE ) THEN
               SIZE = VAL__NBUB*TOTNEL
               CALL PSX_MALLOC( SIZE, IP, STATUS )
               CALL FTGCVB( FUNIT, ICOL, 1, 1, TOTNEL, 0,
     :                      %VAL( CNF_PVAL( IP ) ), ANYF, FSTAT )
               CALL AST_PUTCOLUMNDATA( TABLE, CNAME, 0, SIZE,
     :                                 %VAL( CNF_PVAL( IP ) ), STATUS )
               CALL PSX_FREE( IP, STATUS )

*  Do the same for double-precision floats. FITS does not allow a bad
*  value to be specified for floating-point values in binary tables, so
*  replace NaNs by VAL__BADD in the following call.
            ELSE IF( CTYPE .EQ. AST__DOUBLETYPE ) THEN
               SIZE = VAL__NBD*TOTNEL
               CALL PSX_MALLOC( SIZE, IP, STATUS )
               CALL FTGCVD( FUNIT, ICOL, 1, 1, TOTNEL, VAL__BADD,
     :                      %VAL( CNF_PVAL( IP ) ), ANYF, FSTAT )
               CALL AST_PUTCOLUMNDATA( TABLE, CNAME, 0, SIZE,
     :                                 %VAL( CNF_PVAL( IP ) ), STATUS )
               CALL PSX_FREE( IP, STATUS )

*  Do the same for single-precision floats.
            ELSE IF( CTYPE .EQ. AST__FLOATTYPE ) THEN
               SIZE = VAL__NBR*TOTNEL
               CALL PSX_MALLOC( SIZE, IP, STATUS )
               CALL FTGCVE( FUNIT, ICOL, 1, 1, TOTNEL, VAL__BADR,
     :                      %VAL( CNF_PVAL( IP ) ), ANYF, FSTAT )
               CALL AST_PUTCOLUMNDATA( TABLE, CNAME, 0, SIZE,
     :                                 %VAL( CNF_PVAL( IP ) ), STATUS )
               CALL PSX_FREE( IP, STATUS )

*  Things are a bit different for string types.
            ELSE IF( CTYPE .EQ. AST__STRINGTYPE ) THEN

*  The strings in the FITS binary table are fixed length, but the
*  FitsTable class holds null-terminated variable-length strings. Get
*  the length of each fixed-length string in the binary table. This is
*  the repeat count in the TFORMn keyword value, devided by the number
*  of elements in each cell.
               CLEN = 0
               NAME = 'TFORM'
               IAT = 5
               CALL CHR_PUTI( ICOL, NAME, IAT )
               IF( .NOT. AST_GETFITSS( HEADER, NAME, TFORM,
     :                                 STATUS ) ) THEN

                  IF( STATUS .EQ. SAI__OK ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETC( 'K', NAME )
                     CALL ERR_REP( ' ', 'Keyword ^K not found in '//
     :                             'binary table header.', STATUS )
                  END IF

               ELSE IF( STATUS .EQ. SAI__OK ) THEN
                  DELIM = INDEX( TFORM, 'A' )

                  IF( DELIM .EQ. 1 ) THEN
                     REPEAT = 1

                  ELSE IF( DELIM .GT. 1 ) THEN
                     CALL CHR_CTOI( TFORM( : DELIM - 1 ), REPEAT,
     :                              STATUS )
                     IF( STATUS .NE. SAI__OK ) THEN
                        REPEAT = 0
                        STATUS = SAI__OK
                     END IF

                  ELSE
                     REPEAT = 0
                  END IF

                  CLEN = REPEAT/NEL

                  IF( CLEN .LE. 0 ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'C', ICOL )
                     CALL ERR_REP( ' ', 'Cannot determine the length '//
     :                             'of fixed-length strings in column'//
     :                             ' ^C.', STATUS )
                  END IF

               END IF

*  Allocate memory to store the column values.
               SIZE = CLEN*TOTNEL
               CALL PSX_MALLOC( SIZE, IP, STATUS )

*  Read the FITS file to get all the data values in the column, storing
*  them in the above memory.
               CALL FTGCVS( FUNIT, ICOL, 1, 1, TOTNEL, ' ',
     :                      %VAL( CNF_PVAL( IP ) ), ANYF, FSTAT )

*  Store the column values in the FitsTable.
               CALL AST_PUTCOLUMNDATA( TABLE, CNAME, CLEN, SIZE,
     :                                 %VAL( CNF_PVAL( IP ) ), STATUS )

*  Free the memory.
               CALL PSX_FREE( IP, STATUS )

*  Report an error if the data type of the fits binary data cannot be
*  stored in a FitsTable.
            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'T', CTYPE )
               CALL ERR_REP( ' ', 'COF_BT2FT: Unsupported data type '//
     :                       '^T (programming error).', status )
            END IF

         END DO

*  Annul local AST objects.
         CALL AST_ANNUL( HEADER, STATUS )

*  Reset the FITS error status if no suitable extension was found, and
*  flush the FITSIO error message stack.
      ELSE IF( FSTAT .EQ. BAD_HDU_NUM ) THEN
         FSTAT = FITSOK
         CALL FTCMSG
      END IF

*  Reinstate the original current HDU in the FITS file. Ensure this
*  happens even if an error has occurred.
      FSTAT2 = FITSOK
      CALL FTMAHD( FUNIT, IHDU0, HDUTYPE, FSTAT2 )

*  Report an error if anything went wrong in a FITSIO routine.
      IF( FSTAT .GT. FITSOK .AND. STATUS .EQ. SAI__OK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_BT2FT_ERR', ' ', 'Failed to read'//
     :                   ' a FITS binary table.', STATUS )

      ELSE IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'X', EXTNAM )
         CALL ERR_REP( ' ', 'Failed to read a binary table from '//
     :                 'FITS extension ^X.', STATUS )

      END IF

*  Export the returned Object pointer from the current AST context, and
*  then end the context, thus annulling any objects created within the
*  current AST context.
      CALL AST_EXPORT( TABLE, STATUS )
      CALL AST_END( STATUS )

      END
