      SUBROUTINE COF_FT2BT( TABLE, FUNIT, EXTNAM, ASTVER, STATUS )
*+
*  Name:
*     COF_FT2BT

*  Purpose:
*     Creates an FITS binary table from a AST FitsTable.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_FT2BT( TABLE, FUNIT, EXTNAM, ASTVER, STATUS )

*  Description:
*     This function creates a new FITS binary table holding data copied
*     from an AST FitsTable. The current HDU is unchanged on exit.

*  Arguments:
*     TABLE = INTEGER (Given)
*        A pointer to the FitsTable.
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     EXTNAM = CHARACTER * ( * ) (Given)
*        The name of the new FITS extension containing the binary table.
*     ASTVER = INTEGER (Given)
*        The table version number used by AST_WRITE when writing out -TAB
*        headers.
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
*     17-JAN-2011 (DSB):
*        Original version.
*     11-FEB-2011 (DSB):
*        Only create a single -TAB table for each NDF. It will be shared
*        by all array components of the NDF.
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
      INTEGER TABLE
      INTEGER FUNIT
      CHARACTER EXTNAM*(*)
      INTEGER ASTVER

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CARD*80
      CHARACTER CNAME*50         ! Column name
      CHARACTER NAME*50          ! Keyword or attribute name
      INTEGER CTYPE              ! Column data type
      INTEGER EXTVER             ! Extension version number
      INTEGER FSTAT              ! FITSIO error status
      INTEGER HDUNUM             ! Index of last HDU
      INTEGER HDUTYPE            ! Type of HDU
      INTEGER HEADER             ! FitsChan holding extension's headers
      INTEGER IAT                ! Current used length of a string
      INTEGER ICOL               ! Index of current column
      INTEGER IHDU0              ! Index of current HDU on entry
      INTEGER IP                 ! Pointer to memory holding column data
      INTEGER NCOL               ! Number of columns in the table
      INTEGER NEL                ! Number of elements in each cell
      INTEGER NELEM              ! Number of elements read from table
      INTEGER NROW               ! Number of rows in the table
      INTEGER SIZE               ! No of bytes need to hold column data
      INTEGER TOTNEL             ! Total number of elements to read
      LOGICAL OK                 ! OK to create new extension?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Get the number of the current HDU in the FITS file (primary array
*  = 1) so that we can re-instate it at the end.
      CALL FTGHDN( FUNIT, IHDU0 )

*  Get a FitsChan holding the other headers to put into extension HDU.
      HEADER = AST_GETTABLEHEADER( TABLE, STATUS )

*  Get the table version number from EXTVER keyword. Use a default of 1.
      IF( .NOT. AST_GETFITSI( HEADER, 'EXTVER', EXTVER, STATUS ) ) THEN
         EXTVER = 1
      END IF

*  Attempt to move to an HDU that holds a binary table with the supplied
*  name. The current HDU will remain unchanged if no matching extension
*  is found.
      CALL FTMNHD( FUNIT, 2, EXTNAM, EXTVER, FSTAT )

*  Initialise a flag to indicate that he ne extension should be created.
      OK = .TRUE.

*  Reset the FITS error status if no matching extension was found, and
*  clear the FITSIO error message stack.
      IF( FSTAT .EQ. BAD_HDU_NUM ) THEN
         FSTAT = FITSOK
         CALL FTCMSG

*  If a matching extension was found, decide what to do about it.
      ELSE IF( FSTAT .EQ. FITSOK .AND. STATUS .EQ. SAI__OK ) THEN

*  If the extension name is the same as that used by AST for storing -TAB
*  coord arrays, and the table version number is the "Magic value" used
*  in COF_FPWCS, then the new table describes the quality or variance
*  array of an NDF, and the existing table describes the data array. In
*  this case we just retain the existing table, since it will contain the
*  same values as the new table. Set a flag to indicate that the new
*  table does not need to be created. */
         IF( EXTNAM .EQ. AST__TABEXTNAME .AND. EXTVER .EQ. ASTVER ) THEN
            OK = .FALSE.

*  Otherwise, report an error.
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'N', EXTNAM )
            CALL MSG_SETI( 'V', EXTVER )
            CALL ERR_REP( ' ', 'Cannot write a binary table to FITS '//
     :                    'extension ^N (version ^V): extension '//
     :                    'already exists.', STATUS )
         END IF
      END IF

*  If required, create the new extension.
      IF( OK ) THEN

*  Get the index of the last HDU in the FITS file, and then make it the
*  current HDU and then move back to the original HDU (this is so that
*  the following call to FTCRHD will create the new HDU at the end of
*  the FITS file).
         CALL FTTHDU( FUNIT, HDUNUM, FSTAT )
         IF( HDUNUM .GT. 0 ) THEN
            CALL FTMAHD( FUNIT, HDUNUM, HDUTYPE, FSTAT )
            CALL FTMAHD( FUNIT, IHDU0, HDUTYPE, FSTAT )
         END IF

*  Create a new header and data section. It will be created at the end
*  of the FITS file. We presume it becomes the current HDU although the
*  documentation does not say anything about its effect on the current
*  HDU.
         CALL FTCRHD( FUNIT, FSTAT )

*  Extract the headers from the FitsChan and store them in the current
*  HDU, then annul the header explicitly since we are not using an AST
*  context.
         CALL COF_FC2HD( HEADER, FUNIT, STATUS )
         CALL AST_ANNUL( HEADER, STATUS)

*  Store the extension name in the HDU.
         CALL FTPKYS( FUNIT, 'EXTNAME', EXTNAM, ' ', FSTAT )

*  Get the number of rows in the table.
         NROW = AST_GETI( TABLE, 'NRow', STATUS )

*  Copy the data for each column from the FitsTable into the binary
*  table. Loop over all columns.
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

*  Get the number of bytes needed to hold the data for the current
*  column, and then allocate it.
            SIZE = AST_COLUMNSIZE( TABLE, CNAME, STATUS )
            CALL PSX_MALLOC( SIZE, IP, STATUS )

*  Copy the column data from the FitsTable into this array.
            CALL AST_GETCOLUMNDATA( TABLE, CNAME, AST__NANR, AST__NAN,
     :                              SIZE, %VAL( CNF_PVAL( IP ) ), NELEM,
     :                              STATUS )

*  Sanity check.
            IF( NELEM .NE. TOTNEL .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'N', NELEM )
               CALL MSG_SETI( 'T', TOTNEL )
               CALL ERR_REP( ' ', 'COF_FT2BT: NELEM (^N) and TOTNEL '//
     :                       '(^T) differ (programming error).',
     :                       STATUS )
            END IF

*  Put these data into the FITS file. Do each data type in turn.
            IF( CTYPE .EQ. AST__INTTYPE ) THEN
               CALL FTPCLJ( FUNIT, ICOL, 1, 1, TOTNEL,
     :                      %VAL(CNF_PVAL(IP)), FSTAT )

            ELSE IF( CTYPE .EQ. AST__SINTTYPE ) THEN
               CALL FTPCLI( FUNIT, ICOL, 1, 1, TOTNEL,
     :                      %VAL(CNF_PVAL(IP)), FSTAT )

            ELSE IF( CTYPE .EQ. AST__BYTETYPE ) THEN
               CALL FTPCLB( FUNIT, ICOL, 1, 1, TOTNEL,
     :                      %VAL(CNF_PVAL(IP)), FSTAT )

            ELSE IF( CTYPE .EQ. AST__DOUBLETYPE ) THEN
               CALL FTPCLD( FUNIT, ICOL, 1, 1, TOTNEL,
     :                      %VAL(CNF_PVAL(IP)), FSTAT )

            ELSE IF( CTYPE .EQ. AST__FLOATTYPE ) THEN
               CALL FTPCLE( FUNIT, ICOL, 1, 1, TOTNEL,
     :                      %VAL(CNF_PVAL(IP)), FSTAT )

            ELSE IF( CTYPE .EQ. AST__STRINGTYPE ) THEN
               CALL FTPCLS( FUNIT, ICOL, 1, 1, TOTNEL,
     :                      %VAL(CNF_PVAL(IP)), FSTAT )

*  Report an error if the data type of the fits binary data cannot be
*  stored in a FitsTable.
            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'T', CTYPE )
               CALL ERR_REP( ' ', 'COF_BT2FT: Unsupported data type '//
     :                       '^T (programming error).', status )
            END IF

*  Free the memory.
            CALL PSX_FREE( IP, STATUS )

         END DO
      END IF

*  Reinstate the original current HDU in the FITS file.
      CALL FTMAHD( FUNIT, IHDU0, HDUTYPE, FSTAT )

*  Report an error if anything went wrong in a FITSIO routine.
      IF( FSTAT .GT. FITSOK .AND. STATUS .EQ. SAI__OK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_FT2BT_ERR', ' ', 'Failed to '//
     :                   'read a FITS binary table.', STATUS )

      ELSE IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'X', EXTNAM )
         CALL ERR_REP( ' ', 'Failed to read a binary table from '//
     :                 'FITS extension ^X.', STATUS )

      END IF

      END
