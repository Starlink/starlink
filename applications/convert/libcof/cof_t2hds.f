      SUBROUTINE COF_T2HDS( FUNIT, LOC, STATUS )
*+
*  Name:
*     COF_T2HDS

*  Purpose:
*     Converts an FITS binary or ASCII table into an HDS structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_T2HDS( FUNIT, LOC, STATUS )

*  Description:
*     This routine converts a FITS binary-table or ASCII-table extension
*     into an existing HDS structure.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        Logical-unit number of the FITS file.
*     LOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the structure to contain the components
*        derived from the table.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The conversion from table columns to NDF objects is as
*     follows:
*     - Ignores the special DUMMY_FOR_STRUC column created by COF_WSTR.

*  Prior Requirements:
*     -  The FITS file should already have been opened by FITSIO, and
*     the current HDU is a BINTABLE or TABLE extension.

*  Copyright:
*     Copyright (C) 1997, 2002, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2008, 2010, 2012 Science & Technology Facilities
*     Council.
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
*     AJC: Alan J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1997 February 28 (MJC):
*        Original version.
*     2002 March 13 (AJC):
*        Adjust dimensions for multi-dimensional CHARACTER arrays.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2008 February 12 (MJC):
*        Ignore the special DUMMY_FOR_STRUC column used to preserve
*        extension structures containing only NDFs.
*     2010 August 19 (MJC):
*        Do not use the dimension for a _CHAR vector produced by FTGTDM.
*        Instead set the dimension to the previously calculated number
*        of elements (TFORMn repeat count normalised by its width).
*
*        Correct AJC's 2002 March 13 fix to handle vector _CHAR arrays.
*        While there is no need to use TDIMn for a one-dimensional
*        character array---most writers would use just TFORMn='rAw'---it
*        is part of the standard.
*     2012 April 30 (MJC):
*        Add _INT64.
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
      INTEGER FUNIT
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      LOGICAL BAD                ! Column array contains bad values?
      CHARACTER * ( 10 ) BTYPE ! Column TFORM data type
      CHARACTER * ( DAT__SZNAM ) COLNAM ! Column name
      INTEGER COLNUM             ! Column number
      CHARACTER * ( 48 ) COMENT  ! Comment describing column's meaning
      INTEGER CPOS               ! Character pointer in string
      CHARACTER * ( DAT__SZTYP ) CTYPE ! Column HDS data type
      INTEGER DATCOD             ! FITSIO data-type code
      CHARACTER * ( DAT__SZLOC ) DLOC ! Locator to the DATA component
      INTEGER DIMS( NDF__MXDIM + 1) ! Dimensions of the column
                                    ! +1 allows for character string length as
                                    ! first dimension for _CHAR arrays
      INTEGER I                  ! Dimensions index
      INTEGER EL                 ! Number of rows in the table
      INTEGER FSTAT              ! FITSIO status
      CHARACTER * ( 8 ) KEYWRD   ! FITS header keyword
      INTEGER NDIM               ! Dimensionality of a column
      INTEGER NFIELD             ! Number of fields in table
      INTEGER NV                 ! Number of values in a column
      INTEGER PNTR               ! Pointer to a mapped column array
      INTEGER REPEAT             ! Number of values in a field
      CHARACTER * ( 8 ) SCAKEY   ! TSCALn keyword name
      LOGICAL THERE              ! Header keyword is present?
      INTEGER WIDTH              ! Width of a character field
      CHARACTER * ( 8 )  ZERKEY  ! TZEROn keyword name

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Define the shape of the Table.
*  ==============================

*  Obtain the number of elements.
      CALL COF_GKEYI( FUNIT, 'NAXIS2', THERE, EL, COMENT, STATUS )

*  Obtain the number of fields in the table.
      CALL COF_GKEYI( FUNIT, 'TFIELDS', THERE, NFIELD, COMENT, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Write each column to the data structure.
*  ========================================

*  Loop through all the fields.
      DO COLNUM = 1, NFIELD

*  Find the column name.
*  =====================

*  Obtain the name of the column.  First form the name of the name
*  keyword for this column.
         CALL FTKEYN( 'TTYPE', COLNUM, KEYWRD, FSTAT )
         CALL COF_GKEYC( FUNIT, KEYWRD, THERE, COLNAM, COMENT, STATUS )

         IF ( THERE .AND. COLNAM .NE. 'DUMMY_FOR_STRUC' ) THEN

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
            CALL COF_DSTYP( FUNIT, SCAKEY, ZERKEY, CTYPE, STATUS )

*  Obtain the data type of the column, when the type has not been
*  specified by the presence of TSCALn and TZEROn keywords.
            IF ( CTYPE .EQ. ' ' ) THEN
               CALL FTKEYN( 'TFORM', COLNUM, KEYWRD, FSTAT )
               CALL COF_GKEYC( FUNIT, KEYWRD, THERE, BTYPE, COMENT,
     :                         STATUS )

*  Convert the table type into an HDS type.
               IF ( THERE ) THEN
                  CALL COF_BN2HT( BTYPE, CTYPE, STATUS )
               ELSE
                  CTYPE = '_REAL'
               END IF
            END IF

*  Obtain the count, and if it's a character field also obtain the
*  width.
            CALL FTGTCL( FUNIT, COLNUM, DATCOD, REPEAT, WIDTH, FSTAT )

*  Check for an error.  Flush the error stack.
            IF ( FSTAT .GT. FITSOK ) THEN
               CALL COF_FIOER( FSTAT, 'COF_T2HDS_SHAPE', 'FTGTCL',
     :           'Error obtaining the shape of column '//KEYWRD,
     :           STATUS )
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

*  Define the dimensions of the column.  There should be a TDIMn card
*  whenever the array is multi-dimensional.  If the card is missing, the
*  array will be treated as a vector of length given by TFORMn
*  (=REPEAT).
            IF ( REPEAT .GT. 1 ) THEN
               CALL FTGTDM( FUNIT, COLNUM, DAT__MXDIM+1, NDIM, DIMS,
     :                      FSTAT )

*  Character is special case because the repeat count is the total size,
*  i.e. the actual array dimension divided by the width.
               IF ( CTYPE( 1:5 ) .EQ. '_CHAR' ) THEN
                  IF ( NDIM .GE. 2 ) THEN

*  The first dimension is the CHARACTER width - remove it.
                     DO I = 2, NDIM
                        DIMS( I - 1 ) = DIMS( I )
                     END DO
                     NDIM = NDIM - 1

*  Just a vector of strings.  Use previously calculated dimension.
                  ELSE IF ( NDIM .EQ. 1 ) THEN
                     DIMS( 1 ) = REPEAT
                  END IF
               END IF

*  Dealing with a scalar.
            ELSE
               NDIM = 0
            END IF

*  If there is more than one row in the table, the array dimensionality
*  has to be enlarged by one, and the additional dimension appended.
            IF ( EL .GT. 1 ) THEN
               NDIM = NDIM + 1
               DIMS( NDIM ) = EL
            END IF
            NV = REPEAT * EL

*  Have to treat scalars different from arrays, because the dimension
*  array is passed as 0.
            IF ( NDIM .EQ. 0 ) THEN

*  Create the scalar component of the extension, and get a locator to
*  the component.
               CALL DAT_NEW( LOC, COLNAM, CTYPE, 0, 0, STATUS )
               CALL DAT_FIND( LOC, COLNAM, DLOC, STATUS )

*  Map the scalar component for writing.
               IF ( CTYPE( 1:5 ) .EQ. '_CHAR' ) THEN
                  CALL DAT_MAPC( DLOC, 'WRITE', 0, 0, PNTR, STATUS )
               ELSE
                  CALL DAT_MAP( DLOC, CTYPE, 'WRITE', 0, 0, PNTR,
     :                          STATUS )
               END IF
            ELSE

*  Create the array component of the extension, and get a locator to the
*  component.
               CALL DAT_NEW( LOC, COLNAM, CTYPE, NDIM, DIMS, STATUS )
               CALL DAT_FIND( LOC, COLNAM, DLOC, STATUS )

*  Map the array component for writing.
               IF ( CTYPE( 1:5 ) .EQ. '_CHAR' ) THEN
                  CALL DAT_MAPC( DLOC, 'WRITE', NDIM, DIMS, PNTR,
     :                           STATUS )
               ELSE
                  CALL DAT_MAP( DLOC, CTYPE, 'WRITE', NDIM, DIMS, PNTR,
     :                          STATUS )
               END IF
            END IF

*  Read the column into the data array.  Call the appropriate routine
*  for the chosen type.  Null values are substituted with magic values,
*  except for strings, where the exisiting null value is retained
*  verbatim.
            IF ( CTYPE .EQ. '_UBYTE' ) THEN
               CALL FTGCVB( FUNIT, COLNUM, 1, 1, NV, VAL__BADUB,
     :                      %VAL( CNF_PVAL( PNTR ) ), BAD, FSTAT )

            ELSE IF ( CTYPE .EQ. '_WORD' ) THEN
               CALL FTGCVI( FUNIT, COLNUM, 1, 1, NV, VAL__BADW,
     :                      %VAL( CNF_PVAL( PNTR ) ), BAD, FSTAT )

            ELSE IF ( CTYPE .EQ. '_INTEGER' ) THEN
               CALL FTGCVJ( FUNIT, COLNUM, 1, 1, NV, VAL__BADI,
     :                      %VAL( CNF_PVAL( PNTR ) ), BAD, FSTAT )

            ELSE IF ( CTYPE .EQ. '_INT64' ) THEN
               CALL FTGCVK( FUNIT, COLNUM, 1, 1, NV, VAL__BADI,
     :                      %VAL( CNF_PVAL( PNTR ) ), BAD, FSTAT )

            ELSE IF ( CTYPE .EQ. '_REAL' ) THEN
               CALL FTGCVE( FUNIT, COLNUM, 1, 1, NV, VAL__BADR,
     :                      %VAL( CNF_PVAL( PNTR ) ), BAD, FSTAT )

            ELSE IF ( CTYPE .EQ. '_DOUBLE' ) THEN
               CALL FTGCVD( FUNIT, COLNUM, 1, 1, NV, VAL__BADD,
     :                      %VAL( CNF_PVAL( PNTR ) ), BAD, FSTAT )

            ELSE IF ( CTYPE( 1:5 ) .EQ. '_CHAR' ) THEN
               CALL FTGCVS( FUNIT, COLNUM, 1, 1, NV, ' ',
     :                      %VAL( CNF_PVAL( PNTR ) ), BAD, FSTAT,
     :                      %VAL( CNF_CVAL( 1 ) ),
     :                      %VAL( CNF_CVAL( WIDTH ) ) )

            END IF

*  Tidy the locator to the DATA component.
            CALL DAT_ANNUL( DLOC, STATUS )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Annul active locators.
            IF ( FSTAT .GT. FITSOK ) THEN
               CALL COF_FIOER( FSTAT, 'COF_T2HDS_VALUES', 'FTGCVx',
     :           'Error writing the values for column '//KEYWRD,
     :           STATUS )
               GOTO 999
            END IF

         END IF

*  Exit if something has gone wrong.  Use an error context to help
*  pinpoint the problem.  Tidy the locator.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'COLUMN', KEYWRD )
            CALL ERR_REP( 'COF_T2HDS_ERR',
     :        'An error occurred when transferring column ^COLUMN of '/
     :        /'the FITS table.', STATUS )
            GOTO 999
         END IF

      END DO

  999 CONTINUE

      END
