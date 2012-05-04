      SUBROUTINE COF_H2BIN( LOC, ALOC, FUNIT, EXTLEV, EXPATH, WRITTN,
     :                      STATUS )
*+
*  Name:
*     COF_H2BIN

*  Purpose:
*     Converts an HDS primitive or structure of primitive objects into
*     a FITS binary table.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_H2BIN( LOC, ALOC, FUNIT, EXTLEV, EXPATH, WRITTN, STATUS )

*  Description:
*     The routine creates a FITS binary table of one row to store the
*     contents of the given HDS structure or primitive component.
*     Within a structure only the primitive objects are stored.

*  Arguments:
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        Locator for the primitive object or structure whose contents
*        are to be converted, or locator to a primitive component.
*     ALOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        If the structure given by argument LOC is a cell, ALOC is the
*        locator to the full structure array.  Otherwise it should be
*        set to DAT__NOLOC.
*     FUNIT = INTEGER (Given)
*        The logical unit number of the output FITS file.
*     EXTLEV = INTEGER (Given)
*        The extension level.  Must be a positive integer.  If not it
*        is not written to the FITS header.
*     EXPATH = CHARACTER * ( * ) (Given)
*        A string (usually a prefix) to exclude from the object path
*        used by the EXTNAME keyword in the FITS header.
*     WRITTN = LOGICAL (Returned)
*        If .TRUE. the binary table has been written successfully.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The component path, less any prefix to omit, is stored in the
*     EXTNAME keyword.
*     -  EXTLEV is written to the EXTLEVEL keyword.

*  Prior Requirements:
*     -  A primary HDU unit exists in the FITS file, and the FITS file
*     is open.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1997, 1999, 2002, 2004 Central Laboratory of the
*     Research Councils. Copyright (C) 2006 Particle Physics &
*     Astronomy Research Council. Copyright (C) 2008, 2009, 2012 Science
*     & Technology Facilities Council. All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     AJC: Alan J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1994 June 18 (MJC):
*        Original version.
*     1997 March (MJC):
*        ALOC argument added.  Writes EXTSHAPE keyword and TDIMn
*        keywords.
*     1997 November 15 (MJC):
*        Extended to convert primitive objects too.
*     1-2-1999 (DSB):
*        Corrected string length arguments used in calls which pass a
*        character array using %VAL.
*     12-MAR-2002 (AJC):
*        Correct CRDNM for TDIMn card error message.  Add string length
*        to dimensions in TDIMn cards for _CHAR arrays.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2006 April 7 (MJC):
*        Use the correct FITISO routine names in two COF_FIOER calls.
*        Some tidying.
*     2008 June 13 (MJC):
*        Only attempt to write null value for integer-valued columns.
*     2009 November 25 (MJC):
*        Allow for long extension names.
*     2012 April 30 (MJC):
*        Added handling of 64-bit integers.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'DAT_ERR'          ! Data-system error constants
      INCLUDE 'PRM_PAR'          ! Primitive-data constants
      INCLUDE 'CNF_PAR'          ! CNF functions

*  Arguments Given:
      CHARACTER * ( * ) LOC
      CHARACTER * ( * ) ALOC
      INTEGER FUNIT
      INTEGER EXTLEV
      CHARACTER * ( * ) EXPATH

*  Arguments Returned:
      LOGICAL WRITTN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Effective string length

*  Local Constants:
      INTEGER   FITSCH           ! Maximum length of a FITS character
      PARAMETER( FITSCH = 68 )   ! value

      INTEGER   FITSOK           ! Good status for FITSIO library
      PARAMETER( FITSOK = 0 )

      INTEGER MXOBJ              ! Maximum number of objects
      PARAMETER ( MXOBJ = 999 )

      INTEGER MXSLEN             ! Maximum string length
      PARAMETER ( MXSLEN = 512 )

*  Local Variables:
      CHARACTER * ( 10 ) CDIM    ! Dimension of the structure
      CHARACTER * ( 80 ) CDUMMY  ! Dummy for reading TFORMn cards
      CHARACTER * ( 3 ) CN       ! Column number
      INTEGER CPOS               ! Character position
      CHARACTER * ( 8 ) CRDNAM   ! Header-card name to insert TNULLn
      CHARACTER * ( MXSLEN ) CVALUE ! Character component value
      INTEGER DIMS( DAT__MXDIM + 1) ! Component and structure dimensions
                                 ! +1 allows for string length for_CHAR
                                 ! array
      DOUBLE PRECISION DVALUE    ! D.p. component value
      INTEGER EL                 ! Number of array elements
      CHARACTER * ( 256 ) EXTNAM ! Name of the component
      CHARACTER * ( DAT__SZTYP ) EXTTYP ! Component type
      CHARACTER * ( 256 ) FILE   ! Name of the HDS file
      INTEGER FSTAT              ! FITSIO status
      INTEGER I                  ! Loop through dimensions
      INTEGER IC                 ! Loop through components
      INTEGER ICI                ! Loop through integer components
      INTEGER ICMD               ! Loop through multi-dimensional
                                 ! components
      INTEGER ICOMP( MXOBJ )     ! Column numbers of integer components
      INTEGER INULL              ! Integer null value
      INTEGER IVALUE             ! Integer component value
      INTEGER KCOMP( MXOBJ )     ! Column no.s 64-bit integer components
      INTEGER*8 KNULL            ! 64-bit integer null value
      INTEGER*8 KVALUE           ! 64-bit integer component value
      CHARACTER * ( DAT__SZLOC ) LCMP ! Component locator
      INTEGER LEL                ! Loop counter for undefined logicals
      LOGICAL LVALUE             ! Logical component value
      INTEGER MDCOMP( MXOBJ )    ! Column numbers of multi-dimensional
                                 ! components
      INTEGER NC                 ! Number of characters in keyword
      INTEGER NCMP               ! Number of structure components
      INTEGER NCEXT              ! Column from which the EXTNAME starts
      INTEGER NCPRE              ! Number of characters in prefix
      INTEGER NDIM               ! Number of component and structure
                                 ! dimensions
      INTEGER NLEV               ! Number of hierarchical levels
      INTEGER NOPRIM             ! Number of primitive component
      INTEGER NICOL              ! Number of integer columns
      INTEGER NKCOL              ! Number of 64-bit integer columns
      INTEGER NMDCOL             ! Number of multi-dimensional columns
      INTEGER OPNTR              ! Pointer to a mapped component
      INTEGER PRECOL             ! Number of component dimensions
      LOGICAL PRIM               ! Object primitive?
      CHARACTER * ( 6 ) ROUTIN   ! Name of the FITSIO routine used to
                                 ! copy data into the binary table
      INTEGER RVALUE             ! Real component value
      CHARACTER * ( FITSCH ) SHAPE ! Shape of the structure
      INTEGER SIZE               ! Size as if vector
      INTEGER STRLEN             ! Length of a string component
      CHARACTER * ( DAT__SZTYP ) TFORM( MXOBJ ) ! Components' types
      CHARACTER * ( DAT__SZNAM ) TTYPE( MXOBJ ) ! Names of the
                                 ! binary-table columns
      CHARACTER * ( 1 ) TUNIT( MXOBJ ) ! Binary-table component types
      CHARACTER * ( DAT__SZTYP ) TYPE ! Type of the component
      LOGICAL VALID              ! Valid locator?

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'    ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'    ! NUM definitions for conversions

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the return value.
      WRITTN = .FALSE.

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Determine whether or not the object is primitive.
      CALL DAT_PRIM( LOC, PRIM, STATUS )

*  There is nothing to do if the initial object is primitive.
      IF ( PRIM ) THEN

*  Assimilate the information to make the binary-table header.
*  ===========================================================

*  Determine its name and record that as the label for the column.
*  Note the FITS type is equivalent to HDS name just to confuse matters.
*  There is only one column for the primitive object.
         CALL DAT_NAME( LOC, TTYPE( 1 ), STATUS )

*  Determine its data type.
         CALL DAT_TYPE( LOC, TYPE, STATUS )

*  Inquire the component's size.
         CALL DAT_SIZE( LOC, SIZE, STATUS )

*  Convert the type to the binary-table code.
         CALL COF_HT2BN( TYPE, SIZE, TFORM( 1 ), STATUS )

*  Assign a null units.
         TUNIT( 1 ) = ' '

*  Obtain the shape of the component.
         CALL DAT_SHAPE( LOC, DAT__MXDIM, DIMS, NDIM, STATUS )

*  Get the object's path name and assign it to the extension name.
         CALL HDS_TRACE( LOC, NLEV, EXTNAM, FILE, STATUS )

*  Remove the prefix if one exists.
         IF ( EXPATH .NE. ' ' ) THEN
            NCPRE = CHR_LEN( EXPATH )

*  Get the length of the string.
            PRECOL = INDEX( EXPATH( :NCPRE ), EXTNAM )

*  Remove the string if it is a prefix.  It is done by adjusting the
*  string lower limit.
            IF ( PRECOL .EQ. 1 ) NCEXT = PRECOL + 1
         ELSE
            NCEXT = 1
         END IF

*  Create the dimension string for the header, as a comma-separated
*  list.  A scalar has value zero.
         IF ( NDIM .EQ. 0 ) THEN
            SHAPE = '0'

         ELSE
            CPOS = 0
            DO I = 1, NDIM
               CALL CHR_ITOC( DIMS( I ), CDIM, NC )
               CALL CHR_APPND( CDIM, SHAPE, CPOS )
               IF ( I .NE. NDIM ) CALL CHR_APPND( ',', SHAPE, CPOS )
            END DO

         END IF

*  Don't create a new extension if there was something wrong with the
*  component.
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Create the binary table.
*  ========================

*  Create new header and data section.
         CALL FTCRHD( FUNIT, FSTAT )

*  Create binary-table header.  There is only one row in the table.
*  The "variable-length data area" has length of 0 bytes.
         CALL FTPHBN( FUNIT, 1, 1, TTYPE, TFORM, TUNIT,
     :                EXTNAM( NCEXT: ), 0, FSTAT )

*  Check that the EXTNAM header is not too long for a single header.
*  Some structures can generate long names, for which the CONTINUE
*  convention is in use by writing the LONGSTRN keyword containing the
*  version number of the convention.
         NC = CHR_LEN( EXTNAM( NCEXT: ) )
         IF ( NC .GT. FITSCH ) THEN

*  Write the NDF's component name.  This writes a dummy EXTNAME,
*  a unique EXTVER, and the full component name in keyword EXTNAMEF.
            CALL COF_WENAM( FUNIT, EXTNAM( NCEXT: ),
     :                      'name of this binary-table extension',
     :                      STATUS )
         END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR1',
     :        'FTPHBN', 'Error writing binary-table header.', STATUS )
            GOTO 999
         END IF

*  Write the TNULL1 card for an integer object (column).
*  =====================================================

*  An integer type must have its null value defined before it is used.
*  Floating-point values take defined NaN values.
         IF ( TYPE .EQ. '_BYTE' .OR. TYPE .EQ. '_UBYTE' .OR.
     :        TYPE .EQ. '_WORD' .OR. TYPE .EQ. '_UWORD' .OR.
     :        TYPE .EQ. '_INTEGER' ) THEN

*  FITSIO does not permit cards to be placed after a named card;
*  it requires that we read that named card first.  So choose the
*  the TFORM1 card.  Note that by definition the table only has one
*  column.
            CDUMMY = ' '        ! valgrind
            CALL FTGCRD( FUNIT, 'TFORM1', CDUMMY, FSTAT )

*  Assign the bad/null value for each of the integer data types.
            IF ( TYPE .EQ. '_BYTE' ) THEN
               INULL = NUM_BTOI( VAL__BADB )
            ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
               INULL = NUM_UBTOI( VAL__BADUB )
            ELSE IF ( TYPE .EQ. '_WORD' ) THEN
               INULL = NUM_WTOI( VAL__BADW )
            ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
               INULL = NUM_UWTOI( VAL__BADUW )
            ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
               INULL = VAL__BADI
            END IF

*  Insert the TNULL1 card.
            CALL FTIKYJ( FUNIT, 'TNULL1', INULL,
     :                   'Starlink bad value', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
            IF ( FSTAT .GT. FITSOK ) THEN
               CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR7', 'FTIKYJ',
     :           'Error writing TNULL1 card for a binary table.',
     :           STATUS )
               GOTO 999
            END IF

*  Repeat as for other integer tpyes but using the 64-bit values and
*  routine.
         ELSE IF ( TYPE .EQ. '_INT64' ) THEN

*  Set the location where to insert the header.
            CDUMMY = ' '        ! valgrind
            CALL FTGCRD( FUNIT, 'TFORM1', CDUMMY, FSTAT )

*  Insert the TNULL1 card.
            KNULL = VAL__BADK
            CALL FTIKYK( FUNIT, 'TNULL1', KNULL,
     :                   'Starlink bad value', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
            IF ( FSTAT .GT. FITSOK ) THEN
               CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR7', 'FTIKYK',
     :           'Error writing TNULL1 card for a binary table.',
     :           STATUS )
               GOTO 999
            END IF

         END IF

*  Write the TDIM1 card for a multi-dimensional column.
*  ====================================================

*  This is only necessary when the object is multi-dimensional.
         IF ( NDIM .GT. 1 ) THEN

*  If the type is _CHAR*n, we must adjust the dimensions to make the
*  first equal the number of characters per element, as required by the
*  rAw convention for TFORM.
            IF ( TYPE(1:6) .EQ. '_CHAR*' ) THEN
               DO I = NDIM, 1, -1
                  DIMS(I+1) = DIMS(I)
               END DO
               NDIM = NDIM + 1
               CALL CHR_CTOI( TYPE(7:), DIMS(1), STATUS )
            END IF

*  FITSIO does not permit cards to be placed after a named card;
*  it requires that we read that named card first.
            CDUMMY = ' '        ! valgrind
            CALL FTGCRD( FUNIT, 'TFORM1', CDUMMY, FSTAT )

*  Insert the TDIM1 card.
            CALL FTPTDM( FUNIT, 1, NDIM, DIMS, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
            IF ( FSTAT .GT. FITSOK ) THEN
               CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR7', 'FTIKYJ',
     :           'Error writing TDIM1 card for a binary table.',
     :           STATUS )
               GOTO 999
            END IF
         END IF

*  Write additional header cards.
*  ==============================

*  Set the extension level.
         CALL FTPKYJ( FUNIT, 'EXTLEVEL', EXTLEV, 'Level in the '/
     :               /'hierarchical structure', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR2', 'FTPKYJ',
     :        'Error writing extension level in header.', STATUS )
            GOTO 999
         END IF

*  Set the extension type.
         CALL FTPKYS( FUNIT, 'EXTTYPE', TYPE, 'HDS data '/
     :                /'type of the primitive object', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR3', 'FTPKYS',
     :        'Error writing extension type in header.', STATUS )
            GOTO 999
         END IF

*  Set the extension shape.
         CALL FTPKYS( FUNIT, 'EXTSHAPE', SHAPE, 'Shape '/
     :                /'of the hierarchical structure', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR8', 'FTPKYS',
     :        'Error writing extension shape in header.', STATUS )
            GOTO 999
         END IF

*  Define the structure of the binary table.
*  =========================================

*  Define the structure of a new binary table.  The "variable-length
*  data area" has length of 0 bytes.  The first 1 is because there is
*  only one column.
         CALL FTBDEF( FUNIT, 1, TFORM, 0, 1, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR3', 'FTBDEF',
     :        'Error defining binary-table structure.', STATUS )
            GOTO 999
         END IF

*  Inquire a character component's length in bytes per value, starting
*  a new error context as we wish to annul an error status if the
*  value is undefined.
         IF ( INDEX( TYPE, '_CHAR' ) .NE. 0 ) THEN
            CALL ERR_MARK
            CALL DAT_LEN( LOC, STRLEN, STATUS )

*  Check for an undefined object.
            IF ( STATUS .EQ. DAT__UNSET ) THEN

*  Annul the error but record the fact by making the length negative.
               CALL ERR_ANNUL( STATUS )
               STRLEN = -1
            END IF

*  Release the new error context.
            CALL ERR_RLSE
         END IF

*  Use a new error context as an undefined HDS value may be processed
*  and so the error must be annulled.
         CALL ERR_MARK

*  Copy a scalar value to the table.
*  =================================

*  Deal with vectors and scalars separately.  First scalars.
         IF ( NDIM .EQ. 0 ) THEN

*  Integer
*  -------

*  Obtain each value using the appropriate type and write it to the
*  binary table.  Certain data types (_BYTE and _UWORD) are not
*  available in FITS.  These must be converted to the next higher
*  integer data type.  Note that there is no DAT_GET0UB or DAT_GET0W,
*  so use the _INTEGER type for these too.  HDS will do the type
*  conversion.  The data will be converted to the desired by the FITSIO
*  routine.
            IF ( TYPE .EQ. '_BYTE' .OR. TYPE .EQ. '_UBYTE' .OR.
     :           TYPE .EQ. '_WORD' .OR. TYPE .EQ. '_UWORD' .OR.
     :           TYPE .EQ. '_INTEGER' ) THEN

*  Set the current null value for undefined values.
               CALL FTTNUL( FUNIT, 1, INULL, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
               IF ( FSTAT .GT. FITSOK ) THEN
                  CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR6', 'FTTNUL',
     :              'Error setting null value for a binary-table '/
     :              /'column.', STATUS )
                  GOTO 999
               END IF

*  Obtain an integer scalar value.
               CALL DAT_GET0I( LOC, IVALUE, STATUS )

*  Check for an HDS undefined value.
               IF ( STATUS .EQ. DAT__UNSET ) THEN

*  Annul the bad status as we can cope with the error.
                  CALL ERR_ANNUL( STATUS )

*  Set the binary-table entry to be undefined in this case.
                  CALL FTPCLU( FUNIT, 1, 1, 1, 1, FSTAT )
                  ROUTIN = 'FTPCLU'

*  Check for a bad (undefined value) of the appropriate data type; set
*  the table entry to be undefined in this case.
               ELSE IF (
     :                 ( IVALUE .EQ. VAL__BADB .AND.
     :                   TYPE .EQ. '_BYTE' ) .OR.
     :                 ( IVALUE .EQ. VAL__BADUB .AND.
     :                   TYPE .EQ. '_UBYTE' ) .OR.
     :                 ( IVALUE .EQ. VAL__BADW .AND.
     :                   TYPE .EQ. '_WORD' ) .OR.
     :                 ( IVALUE .EQ. VAL__BADUW .AND.
     :                   TYPE .EQ. '_UWORD' ) .OR.
     :                 ( IVALUE .EQ. VAL__BADI .AND.
     :                   TYPE .EQ. '_INTEGER' ) ) THEN

                  CALL FTPCLU( FUNIT, 1, 1, 1, 1, FSTAT )
                  ROUTIN = 'FTPCLU'
               ELSE

*  Copy the integer value to the FITS binary table.
                  CALL FTPCLI( FUNIT, 1, 1, 1, 1, IVALUE, FSTAT )
                  ROUTIN = 'FTPCLI'
               END IF

*  64-bit integer.
*  ---------------
            ELSE IF ( TYPE .EQ. '_INT64' ) THEN

*  Set the current null value for undefined values.
               CALL FTPNULLL( FUNIT, 1, KNULL, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
               IF ( FSTAT .GT. FITSOK ) THEN
                  CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR6', 'FTPNULLL',
     :              'Error setting null value for a binary-table '/
     :              /'column.', STATUS )
                  GOTO 999
               END IF

*  Obtain a 64-bit integer scalar value.
               CALL DAT_GET0K( LOC, KVALUE, STATUS )

*  Check for an HDS undefined value.
               IF ( STATUS .EQ. DAT__UNSET ) THEN

*  Annul the bad status as we can cope with the error.
                  CALL ERR_ANNUL( STATUS )

*  Set the binary-table entry to be undefined in this case.
                  CALL FTPCLU( FUNIT, 1, 1, 1, 1, FSTAT )
                  ROUTIN = 'FTPCLU'

*  Check for a bad (undefined value) of the appropriate data type; set
*  the table entry to be undefined in this case.
               ELSE IF ( KVALUE .EQ. VAL__BADK ) THEN
                  CALL FTPCLU( FUNIT, 1, 1, 1, 1, FSTAT )
                  ROUTIN = 'FTPCLU'
               ELSE

*  Copy the 64-bit integer value to the FITS binary table.
                  CALL FTPCLK( FUNIT, 1, 1, 1, 1, KVALUE, FSTAT )
                  ROUTIN = 'FTPCLK'
               END IF


*  Short string
*  ------------
            ELSE IF ( TYPE( 1:5 ) .EQ. '_CHAR' .AND.
     :                STRLEN .LE. MXSLEN ) THEN

*  Start a new error context as we wish to annul an error status if the
*  value is undefined.  Get the value.
               CALL ERR_MARK
               CALL DAT_GET0C( LOC, CVALUE, STATUS )

*  Check for an undefined object.
               IF ( STATUS .EQ. DAT__UNSET ) THEN

*  Annul the error but record the setting the value to be null, i.e.
*  a blank string.
                  CALL ERR_ANNUL( STATUS )
                  CVALUE = ' '
               END IF

*  Release the new error context.
               CALL ERR_RLSE

*  Copy the value to the binary-table.
               CALL FTPCLS( FUNIT, 1, 1, 1, 1, CVALUE ( :STRLEN ),
     :                      FSTAT )
               ROUTIN = 'FTPCLS'

*  String
*  ------

*  Obtain a string.  Copy it to the FITS binary table.  Start a new
*  error context as we wish to annul an error status if the value is
*  undefined.  Get the value.  Only MXSLEN characters can be stored in
*  the character variable so for long strings map the array.
            ELSE IF ( TYPE( 1:5 ) .EQ. '_CHAR' .AND.
     :                STRLEN .GT. MXSLEN ) THEN
               CALL ERR_MARK
               CALL DAT_MAPV( LOC, TYPE, 'READ', OPNTR, EL, STATUS )

*  Check for an undefined object.
               IF ( STATUS .EQ. DAT__UNSET ) THEN

*  Annul the error but record the setting the value to be null, i.e.
*  a blank string.  Copy this blank string to the binary table.
                  CALL ERR_ANNUL( STATUS )
                  CVALUE = ' '
                  CALL FTPCLS( FUNIT, 1, 1, 1, EL, CVALUE, FSTAT )
               ELSE

*  Copy the mapped value to the binary table.
                  CALL FTPCLS( FUNIT, 1, 1, 1, EL,
     :                         %VAL( CNF_PVAL( OPNTR ) ),
     :                         FSTAT, %VAL( CNF_CVAL( STRLEN ) ) )
               END IF

*  Release the new error context.
               CALL ERR_RLSE

*  Record the routine name in case of error and unmap the value.
               ROUTIN = 'FTPCLS'
               CALL DAT_UNMAP( LOC, STATUS )

*  Real scalar value.
*  ------------------
            ELSE IF ( TYPE .EQ. '_REAL' ) THEN
               CALL DAT_GET0R( LOC, RVALUE, STATUS )

*  Check for an HDS-undefined value.  Set the table entry to be
*  undefined in this case, and annul the error.
               IF ( STATUS .EQ. DAT__UNSET ) THEN
                  CALL ERR_ANNUL( STATUS )
                  CALL FTPCLU( FUNIT, 1, 1, 1, 1, FSTAT )
                  ROUTIN = 'FTPCLU'

*  Check for a bad (undefined value); set the table entry to be
*  undefined in this case.
               ELSE IF ( RVALUE .EQ. VAL__BADR ) THEN
                  CALL FTPCLU( FUNIT, 1, 1, 1, 1, FSTAT )
                  ROUTIN = 'FTPCLU'
               ELSE

*  Copy the real value to the FITS binary table.
                   CALL FTPCLE( FUNIT, 1, 1, 1, 1, RVALUE, FSTAT )
                   ROUTIN = 'FTPCLE'
                END IF

*  Double-precision scalar value
*  -----------------------------
            ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN

*  Check for an HDS-undefined value.  Set the table entry to be
*  undefined in this case, and annul the error.
               IF ( STATUS .EQ. DAT__UNSET ) THEN
                  CALL ERR_ANNUL( STATUS )
                  CALL FTPCLU( FUNIT, 1, 1, 1, 1, FSTAT )
                  ROUTIN = 'FTPCLU'

*  Check for a bad (undefined value); set the table entry to be
*  undefined in this case.
               ELSE IF ( DVALUE .EQ. VAL__BADD ) THEN
                  CALL FTPCLU( FUNIT, 1, 1, 1, 1, FSTAT )
                  ROUTIN = 'FTPCLU'

               ELSE

*  Copy the double-precision value to the FITS binary table.
                  CALL FTPCLD( FUNIT, 1, 1, 1, 1, DVALUE, FSTAT )
                  ROUTIN = 'FTPCLD'
               END IF

*  Logical scalar value
*  --------------------
            ELSE IF ( TYPE .EQ. '_LOGICAL' ) THEN
               CALL DAT_GET0L( LOC, LVALUE, STATUS )

*  Check for an HDS-undefined value.  Since there is no logical null
*  value, by convention it is set to be true, and annul the error.
               IF ( STATUS .EQ. DAT__UNSET ) THEN
                  CALL ERR_ANNUL( STATUS )
                  LVALUE = .TRUE.
               END IF

*  Copy the logical value to the FITS binary table.
               CALL FTPCLL( FUNIT, 1, 1, 1, 1, LVALUE, FSTAT )
               ROUTIN = 'FTPCLL'
            END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
            IF ( FSTAT .GT. FITSOK ) THEN
               CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR4', ROUTIN,
     :           'Error copying scalar value to the binary table.',
     :           STATUS )
               GOTO 999
            END IF

         ELSE

*  Copy an array to the table.
*  ===========================

*  Integers
*  --------

*  An integer type must have its null value defined before it is used.
            IF ( TYPE .EQ. '_BYTE' .OR. TYPE .EQ. '_UBYTE' .OR.
     :           TYPE .EQ. '_WORD' .OR. TYPE .EQ. '_UWORD' .OR.
     :           TYPE .EQ. '_INTEGER' ) THEN

*  Set the current null value for undefined values.
               CALL FTTNUL( FUNIT, 1, INULL, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
               IF ( FSTAT .GT. FITSOK ) THEN
                  CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR6', 'FTTNUL',
     :              'Error setting null value for a '/
     :              /'binary-table column.', STATUS )
                  GOTO 999
               END IF

*  An integer type must have its null value defined before it is used.
            ELSE IF ( TYPE .EQ. '_INT64' ) THEN

*  Set the current null value for undefined values.
               CALL FTPNULLL( FUNIT, 1, KNULL, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
               IF ( FSTAT .GT. FITSOK ) THEN
                  CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR6', 'FTPNULLL',
     :              'Error setting null value for a '/
     :              /'binary-table column.', STATUS )
                  GOTO 999
               END IF
            END IF

*  Map each array using the appropriate type and write it to the
*  binary table.  Certain data types (_BYTE and _UWORD) are not
*  available in FITS.  These must be converted to the next higher
*  integer data type.  HDS undefined values are assigned the table
*  undefined value.  A new error context is used handle undefined values
*  transparently.

*  Byte is converted to word for the binary table.
            IF ( TYPE .EQ. '_BYTE' ) THEN
               CALL DAT_MAPV( LOC, '_WORD', 'READ', OPNTR, EL, STATUS )

               IF ( STATUS .EQ. DAT__UNSET ) THEN
                  CALL ERR_ANNUL( STATUS )
                  CALL FTPCLU( FUNIT, 1, 1, 1, EL, FSTAT )
                  ROUTIN = 'FTPCLU'
               ELSE
                  CALL FTPCNI( FUNIT, 1, 1, 1, EL,
     :                         %VAL( CNF_PVAL( OPNTR ) ),
     :                         NUM_BTOW( VAL__BADB ), FSTAT )
                  ROUTIN = 'FTPCNI'
               END IF

*  Copy an unsigned-byte array to the binary table.
            ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
               CALL DAT_MAPV( LOC, TYPE, 'READ', OPNTR, EL, STATUS )

               IF ( STATUS .EQ. DAT__UNSET ) THEN
                  CALL ERR_ANNUL( STATUS )
                  CALL FTPCLU( FUNIT, 1, 1, 1, EL, FSTAT )
                  ROUTIN = 'FTPCLU'
               ELSE
                  CALL FTPCNB( FUNIT, 1, 1, 1, EL,
     :                         %VAL( CNF_PVAL( OPNTR ) ),
     :                         NUM_UBTOI( VAL__BADUB ), FSTAT )
                  ROUTIN = 'FTPCNB'
               END IF

*  Copy a word array to the binary table.
            ELSE IF ( TYPE .EQ. '_WORD' ) THEN
               CALL DAT_MAPV( LOC, TYPE, 'READ', OPNTR, EL, STATUS )

               IF ( STATUS .EQ. DAT__UNSET ) THEN
                  CALL ERR_ANNUL( STATUS )
                  CALL FTPCLU( FUNIT, 1, 1, 1, EL, FSTAT )
                  ROUTIN = 'FTPCLU'
               ELSE
                  CALL FTPCNI( FUNIT, 1, 1, 1, EL,
     :                         %VAL( CNF_PVAL( OPNTR ) ),
     :                         VAL__BADW, FSTAT )
                  ROUTIN = 'FTPCNI'
               END IF

*  Unsigned word is converted to integer for the binary table.
            ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
               CALL DAT_MAPV( LOC, '_INTEGER', 'READ', OPNTR, EL,
     :                        STATUS )

               IF ( STATUS .EQ. DAT__UNSET ) THEN
                  CALL ERR_ANNUL( STATUS )
                  CALL FTPCLU( FUNIT, 1, 1, 1, EL, FSTAT )
                  ROUTIN = 'FTPCLU'
               ELSE
                  CALL FTPCNJ( FUNIT, 1, 1, 1, EL,
     :                         %VAL( CNF_PVAL( OPNTR ) ),
     :                         NUM_UWTOI( VAL__BADUW ), FSTAT )
                  ROUTIN = 'FTPCNJ'
               END IF

*  Copy an integer array to the binary table.
            ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
               CALL DAT_MAPV( LOC, TYPE, 'READ', OPNTR, EL, STATUS )

               IF ( STATUS .EQ. DAT__UNSET ) THEN
                  CALL ERR_ANNUL( STATUS )
                  CALL FTPCLU( FUNIT, 1, 1, 1, EL, FSTAT )
                  ROUTIN = 'FTPCLU'
               ELSE
                  CALL FTPCNJ( FUNIT, 1, 1, 1, EL,
     :                         %VAL( CNF_PVAL( OPNTR ) ),
     :                         VAL__BADI, FSTAT )
                  ROUTIN = 'FTPCNJ'
               END IF

*  Copy a 64-bit integer array to the binary table.
            ELSE IF ( TYPE .EQ. '_INT64' ) THEN
               CALL DAT_MAPV( LOC, TYPE, 'READ', OPNTR, EL, STATUS )

               IF ( STATUS .EQ. DAT__UNSET ) THEN
                  CALL ERR_ANNUL( STATUS )
                  CALL FTPCLU( FUNIT, 1, 1, 1, EL, FSTAT )
                  ROUTIN = 'FTPCLU'
               ELSE
                  CALL FTPCNK( FUNIT, 1, 1, 1, EL,
     :                         %VAL( CNF_PVAL( OPNTR ) ),
     :                         VAL__BADK, FSTAT )
                  ROUTIN = 'FTPCNK'
               END IF

*  Copy a string array to the binary table.
            ELSE IF ( TYPE( 1:5 ) .EQ. '_CHAR' ) THEN
               CALL DAT_MAPV( LOC, TYPE, 'READ', OPNTR, EL, STATUS )

*  There is no null character value, so set to be blank by convention.
*  Since this is really a fault in the NDF, the user can suffer a
*  little by calling the FITSIO routine for every element, instead of
*  getting workspace and filling it with blank values.
               IF ( STATUS .EQ. DAT__UNSET ) THEN
                  CALL ERR_ANNUL( STATUS )
                  CVALUE = ' '
                  DO LEL = 1, EL
                     CALL FTPCLS( FUNIT, 1, 1, LEL, 1,
     :                            CVALUE( :STRLEN ), FSTAT )
                  END DO
               ELSE
                  CALL FTPCLS( FUNIT, 1, 1, 1, EL,
     :                         %VAL( CNF_PVAL( OPNTR ) ),
     :                         FSTAT, %VAL( CNF_CVAL( STRLEN ) ) )
               END IF
               ROUTIN = 'FTPCLS'

*  Copy a real array to the binary table.
            ELSE IF ( TYPE .EQ. '_REAL' ) THEN
               CALL DAT_MAPV( LOC, TYPE, 'READ', OPNTR, EL, STATUS )

               IF ( STATUS .EQ. DAT__UNSET ) THEN
                  CALL ERR_ANNUL( STATUS )
                  CALL FTPCLU( FUNIT, 1, 1, 1, EL, FSTAT )
                  ROUTIN = 'FTPCLU'
               ELSE
                  CALL FTPCNE( FUNIT, 1, 1, 1, EL,
     :                         %VAL( CNF_PVAL( OPNTR ) ),
     :                         VAL__BADR, FSTAT )
                  ROUTIN = 'FTPCNE'
               END IF

*  Copy an double-precision array to the binary table.
            ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
               CALL DAT_MAPV( LOC, TYPE, 'READ', OPNTR, EL, STATUS )

               IF ( STATUS .EQ. DAT__UNSET ) THEN
                  CALL ERR_ANNUL( STATUS )
                  CALL FTPCLU( FUNIT, 1, 1, 1, EL, FSTAT )
                  ROUTIN = 'FTPCLU'
               ELSE
                  CALL FTPCND( FUNIT, 1, 1, 1, EL,
     :                         %VAL( CNF_PVAL( OPNTR ) ),
     :                         VAL__BADD, FSTAT )
                  ROUTIN = 'FTPCND'
               END IF

*  Copy a logical array to the binary table.
            ELSE IF ( TYPE .EQ. '_LOGICAL' ) THEN
               CALL DAT_MAPV( LOC, TYPE, 'READ', OPNTR, EL, STATUS )

*  There is no null logical value, so set to be true by convention.
*  Since this is really a fault in the NDF, the user can suffer a
*  little by calling the FITSIO routine for every element, instead of
*  getting workspace and filling it with TRUE values.
               IF ( STATUS .EQ. DAT__UNSET ) THEN
                  CALL ERR_ANNUL( STATUS )
                  DO LEL = 1, EL
                     CALL FTPCLL( FUNIT, 1, 1, LEL, 1, .TRUE., FSTAT )
                  END DO
               ELSE
                  CALL FTPCLL( FUNIT, 1, 1, 1, EL,
     :                         %VAL( CNF_PVAL( OPNTR ) ),
     :                         FSTAT )
                  ROUTIN = 'FTPCLL'
               END IF

            END IF

*  Unmap the array component.
            CALL DAT_UNMAP( LOC, STATUS )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
            IF ( FSTAT .GT. FITSOK ) THEN
               CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR5', ROUTIN,
     :           'Error copying an array to the binary table.', STATUS )
               GOTO 999
            END IF
         END IF

*  Release the error context.
         CALL ERR_RLSE

*  Record that the extension was written satisfactorily.
         WRITTN = .TRUE.

*  Structure
*  =========
      ELSE

*  Determine how many components the structure has.
         CALL DAT_NCOMP( LOC, NCMP, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Proceed if the structure is not empty?
            IF ( NCMP .GT. 0 ) THEN

*  Loop through all objects within this structure.
               NICOL = 0
               NKCOL = 0
               NMDCOL = 0
               NOPRIM = 0
               DO IC = 1, NCMP

*  Assimilate the information to make the binary-table header.
*  ===========================================================

*  Get a locator to the object.
                  CALL DAT_INDEX( LOC, IC, LCMP, STATUS )

*  Determine whether or not it is primitive.
                  CALL DAT_PRIM( LCMP, PRIM, STATUS )

*  Count the number of primitive objects within this structure.
                  IF ( PRIM ) THEN
                     NOPRIM = NOPRIM + 1

*  Determine its name and record that as the label for the column.
*  Note the FITS type is equivalent to HDS name just to confuse matters.
                     CALL DAT_NAME( LCMP, TTYPE( NOPRIM ), STATUS )

*  Determine its data type.
                     CALL DAT_TYPE( LCMP, TYPE, STATUS )

*  Record and count the number of integer columns.  These are needed to
*  set the TNULLn keywords after creating the basic header section
*  for the binary table.
                     IF ( TYPE .EQ. '_BYTE' .OR. TYPE .EQ. '_UBYTE' .OR.
     :                    TYPE .EQ. '_WORD' .OR. TYPE .EQ. '_UWORD' .OR.
     :                    TYPE .EQ. '_INTEGER' ) THEN
                        NICOL = NICOL + 1
                        ICOMP( NICOL ) = NOPRIM

                     ELSE IF ( TYPE .EQ. '_INT64' ) THEN
                        NKCOL = NKCOL + 1
                        KCOMP( NKCOL ) = NOPRIM
                     END IF

*  Inquire the component's size.
                     CALL DAT_SIZE( LCMP, SIZE, STATUS )

*  Convert the type to the binary-table code.
                     CALL COF_HT2BN( TYPE, SIZE, TFORM( NOPRIM ),
     :                               STATUS )

*  Assign a null units.
                     TUNIT( NOPRIM ) = ' '

*  Obtain the shape of the component.
                     CALL DAT_SHAPE( LCMP, DAT__MXDIM, DIMS, NDIM,
     :                               STATUS )

*  Record any multi-dimensional components.
                     IF ( NDIM .GT. 1 ) THEN
                        NMDCOL = NMDCOL + 1
                        MDCOMP ( NMDCOL ) = NOPRIM
                     END IF

*  Tidy the locator to the object.
                     CALL DAT_ANNUL( LCMP, STATUS )
                  END IF

*  Determine the properties of the object.
               END DO

*  Proceed to write binary table if there is at least one primitive
*  object.
               IF ( NOPRIM .GT. 0 ) THEN

*  Get the structure's path name and assign it to the extension name.
                  CALL HDS_TRACE( LOC, NLEV, EXTNAM, FILE, STATUS )

*  Remove the prefix if one exists.
                  IF ( EXPATH .NE. ' ' ) THEN
                     NCPRE = CHR_LEN( EXPATH )

*  Get the length of the string.
                     PRECOL = INDEX( EXPATH( :NCPRE ), EXTNAM )

*  Remove the string if it is a prefix.  It is done by adjusting the
*  string lower limit.
                     IF ( PRECOL .EQ. 1 ) NCEXT = PRECOL + 1
                  ELSE
                     NCEXT = 1
                  END IF

*  Obtain the data type.
                  CALL DAT_TYPE( LOC, EXTTYP, STATUS )

*  Need to determine the shape of the full array.  Test whether or not
*  the supplementary locator to a full array structure is valid.
                  CALL DAT_VALID( ALOC, VALID, STATUS )
                  IF ( VALID ) THEN

*  Obtain the data shape of the full array of structures.
                     CALL DAT_SHAPE( ALOC, DAT__MXDIM, DIMS, NDIM,
     :                               STATUS )
                  ELSE

*  Obtain the data shape of the supplied structure, as it is not a cell.
                     CALL DAT_SHAPE( LOC, DAT__MXDIM, DIMS, NDIM,
     :                               STATUS )
                  END IF

*  Create the dimension string for the header, as a comma-separated
*  list.  A scalar has value zero.
                  IF ( NDIM .EQ. 0 ) THEN
                     SHAPE = '0'

                  ELSE
                     CPOS = 0
                     DO I = 1, NDIM
                        CALL CHR_ITOC( DIMS( I ), CDIM, NC )
                        CALL CHR_APPND( CDIM, SHAPE, CPOS )
                        IF ( I .NE. NDIM )
     :                    CALL CHR_APPND( ',', SHAPE, CPOS )
                     END DO

                  END IF

*  Create the binary table.
*  ========================

*  Create new header and data section.
                  CALL FTCRHD( FUNIT, FSTAT )

*  Create binary-table header.  There is only one row in the table.
*  The "variable-length data area" has length of 0 bytes.
                  CALL FTPHBN( FUNIT, 1, NOPRIM, TTYPE, TFORM,
     :                         TUNIT, EXTNAM( NCEXT: ), 0, FSTAT )

*  Check that the EXTNAM header is not too long for a single header.
*  Some structures can generate long names, for which the CONTINUE
*  convention is in use by writing the LONGSTRN keyword containing the
*  version number of the convention.
                  NC = CHR_LEN( EXTNAM( NCEXT: ) )
                  IF ( NC .GT. FITSCH ) THEN

*  Write the NDF's component name.  This writes a dummy EXTNAME,
*  a unique EXTVER, and the full component name in keyword EXTNAMEF.
                     CALL COF_WENAM( FUNIT, EXTNAM( NCEXT: ),
     :                               'name of this binary-table '/
     :                               /' extension', STATUS )
                  END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
                  IF ( FSTAT .GT. FITSOK ) THEN
                     CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR1',
     :                 'FTPHBN', 'Error writing binary-table header.',
     :                 STATUS )
                     GOTO 999
                  END IF

*  Write the TNULLn cards for any integer columns.
*  ===============================================

*  This is only necessary when there is at least one column that has one
*  of the integer types.  Floating-point values take defined NaN
*  values.
                  IF ( NICOL .GT. 0 ) THEN
                     DO ICI = 1, NICOL

*  Convert the column number into character form.
                        CALL CHR_ITOC( ICOMP( ICI ), CN, NC )

*  Form the name of the keyword which will immediately precede the
*  inserted TNULLn card.
                        NC = 5
                        CRDNAM = 'TFORM'
                        CALL CHR_APPND( CN, CRDNAM, NC )

*  FITSIO does not permit cards to be placed after a named card;
*  it requires that we read that named card first.
                        CDUMMY = ' ' ! valgrind
                        CALL FTGCRD( FUNIT, CRDNAM, CDUMMY, FSTAT )

*  Get a locator to the object so that we can determine its data type.
*  This information was obtained before, but inquiring again avoids
*  having a large character array.  Release the locator at the end.
                        CALL DAT_INDEX( LOC, ICOMP( ICI ), LCMP,
     :                                  STATUS )
                        CALL DAT_TYPE( LCMP, TYPE, STATUS )
                        CALL DAT_ANNUL( LCMP, STATUS )

*  An integer type must have its null value defined before it is used.
*  Floating-point values take defined NaN values.
                        IF ( TYPE .EQ. '_BYTE' .OR.
     :                       TYPE .EQ. '_UBYTE' .OR.
     :                       TYPE .EQ. '_WORD' .OR.
     :                       TYPE .EQ. '_UWORD' .OR.
     :                       TYPE .EQ. '_INTEGER' ) THEN

*  Assign the bad/null value for each of the integer data types.
                           IF ( TYPE .EQ. '_BYTE' ) THEN
                              INULL = NUM_BTOI( VAL__BADB )
                           ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                              INULL = NUM_UBTOI( VAL__BADUB )
                           ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                              INULL = NUM_WTOI( VAL__BADW )
                           ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                              INULL = NUM_UWTOI( VAL__BADUW )
                           ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                              INULL = VAL__BADI
                           END IF

*  Form the name of the TNULLn keyword.
                           NC = 5
                           CRDNAM = 'TNULL'
                           CALL CHR_APPND( CN, CRDNAM, NC )

*  Insert the TNULLn card.
                           CALL FTIKYJ( FUNIT, CRDNAM, INULL,
     :                                  'Starlink bad value', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
                           IF ( FSTAT .GT. FITSOK ) THEN
                              CALL COF_FIOER( FSTAT,
     :                          'COF_H2BIN_ERR7', 'FTIKYJ',
     :                          'Error writing '//CRDNAM( :NC )//' '/
     :                          /'card for a binary table.', STATUS )
                              GOTO 999
                           END IF
                        END IF
                     END DO
                  END IF

*  This is only necessary when there is at least one column that has the
*  64-bit integer type.
                  IF ( NKCOL .GT. 0 ) THEN
                     DO ICI = 1, NKCOL

*  Convert the column number into character form.
                        CALL CHR_ITOC( KCOMP( ICI ), CN, NC )

*  Form the name of the keyword which will immediately precede the
*  inserted TNULLn card.
                        NC = 5
                        CRDNAM = 'TFORM'
                        CALL CHR_APPND( CN, CRDNAM, NC )

*  FITSIO does not permit cards to be placed after a named card;
*  it requires that we read that named card first.
                        CDUMMY = ' ' ! valgrind
                        CALL FTGCRD( FUNIT, CRDNAM, CDUMMY, FSTAT )

*  Get a locator to the object so that we can determine its data type.
*  This information was obtained before, but inquiring again avoids
*  having a large character array.  Release the locator at the end.
                        CALL DAT_INDEX( LOC, KCOMP( ICI ), LCMP,
     :                                  STATUS )
                        CALL DAT_TYPE( LCMP, TYPE, STATUS )
                        CALL DAT_ANNUL( LCMP, STATUS )

*  An integer type must have its null value defined before it is used.
                        KNULL = VAL__BADK

*  Form the name of the TNULLn keyword.
                        NC = 5
                        CRDNAM = 'TNULL'
                        CALL CHR_APPND( CN, CRDNAM, NC )

*  Insert the TNULLn card.
                        CALL FTIKYK( FUNIT, CRDNAM, KNULL,
     :                               'Starlink bad value', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
                        IF ( FSTAT .GT. FITSOK ) THEN
                           CALL COF_FIOER( FSTAT,
     :                       'COF_H2BIN_ERR7', 'FTIKYK',
     :                       'Error writing '//CRDNAM( :NC )//' '/
     :                       /'card for a binary table.', STATUS )
                            GOTO 999
                        END IF
                     END DO
                  END IF

*  Write the TDIMn cards for any multi-dimensional columns.
*  ========================================================

*  This is only necessary when the object is multi-dimensional.
                  IF ( NMDCOL .GT. 0 ) THEN
                     DO ICMD = 1, NMDCOL

*  Convert the column number into character form.
                        CALL CHR_ITOC( MDCOMP( ICMD ), CN, NC )

*  Form the name of the keyword which will immediately precede the
*  inserted TDIMn card.
                        NC = 5
                        CRDNAM = 'TFORM'
                        CALL CHR_APPND( CN, CRDNAM, NC )

*  FITSIO does not permit cards to be placed after a named card;
*  it requires that we read that named card first.
                        CALL FTGCRD( FUNIT, CRDNAM, CDUMMY, FSTAT )

*  Get a locator to the object so that we can determine its shape and
*  type.  This information was obtained before, but inquiring again
*  avoids having large dimension arrays.  Release the locator at the
*  end.
                        CALL DAT_INDEX( LOC, MDCOMP( ICMD ), LCMP,
     :                                  STATUS )
                        CALL DAT_TYPE( LCMP, TYPE, STATUS )

*  Get the dimensions.  If the type is _CHAR*n, we must adjust the
*  dimensions to make the first equal the number of characters per
*  element, as required by the rAw convention for TFORM.
                        IF( TYPE(1:6) .EQ. '_CHAR*' ) THEN
                           CALL DAT_SHAPE( LCMP, DAT__MXDIM, DIMS(2),
     :                                   NDIM, STATUS )
                           CALL CHR_CTOI( TYPE(7:), DIMS(1), STATUS )
                           NDIM = NDIM + 1
                        ELSE
                           CALL DAT_SHAPE( LCMP, DAT__MXDIM, DIMS, NDIM,
     :                                  STATUS )
                        END IF

                        CALL DAT_ANNUL( LCMP, STATUS )

*  Insert the TDIMn card.
                        CALL FTPTDM( FUNIT, MDCOMP( ICMD ), NDIM, DIMS,
     :                               FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.  Form the
*  name of the TDIMn keyword along the way.
                        IF ( FSTAT .GT. FITSOK ) THEN
                           NC = 4
                           CRDNAM = 'TDIM'
                           CALL CHR_APPND( CN, CRDNAM, NC )
                           CALL COF_FIOER( FSTAT,
     :                       'COF_H2BIN_ERR7', 'FTIKYJ',
     :                       'Error writing '//CRDNAM( :NC )//' card '/
     :                       /'for a binary table.', STATUS )
                           GOTO 999
                        END IF
                     END DO
                  END IF

*  Write additional header cards.
*  ==============================

*  Set the extension level.
                  CALL FTPKYJ( FUNIT, 'EXTLEVEL', EXTLEV, 'Level in '/
     :                         /'the hierarchical structure', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
                  IF ( FSTAT .GT. FITSOK ) THEN
                     CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR2',
     :                 'FTPKYJ', 'Error writing extension level in '/
     :                 /'header.', STATUS )
                     GOTO 999
                  END IF

*  Set the extension type.
                  CALL FTPKYS( FUNIT, 'EXTTYPE', EXTTYP, 'HDS data '/
     :                         /'type of the hierarchical structure',
     :                         FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
                  IF ( FSTAT .GT. FITSOK ) THEN
                     CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR3',
     :                 'FTPKYJ', 'Error writing extension type in '/
     :                 /'header.', STATUS )
                     GOTO 999
                  END IF

*  Set the extension shape.
                  CALL FTPKYS( FUNIT, 'EXTSHAPE', SHAPE, 'Shape '/
     :                         /'of the hierarchical structure', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
                  IF ( FSTAT .GT. FITSOK ) THEN
                     CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR8',
     :                 'FTPKYJ', 'Error writing extension shape in '/
     :                 /'header.', STATUS )
                     GOTO 999
                  END IF

*  Define the structure of the binary table.
*  =========================================

*  Define the structure of a new binary table.  The "variable-length
*  data area" has length of 0 bytes.
                  CALL FTBDEF( FUNIT, NOPRIM, TFORM, 0, 1, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
                  IF ( FSTAT .GT. FITSOK ) THEN
                     CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR3',
     :                 'FTBDEF', 'Error defining binary-table '/
     :                 /'structure.', STATUS )
                     GOTO 999
                  END IF

*  Find the properties of the primitive objects.
*  =============================================

*  Loop for each object, adding a column to the table for each
*  primitive object.
                  NOPRIM = 0
                  DO IC = 1, NCMP

*  Get a locator to the object.
                     CALL DAT_INDEX( LOC, IC, LCMP, STATUS )

*  Determine whether or not it is primitive.
                     CALL DAT_PRIM( LCMP, PRIM, STATUS )

*  Count the number of primitive objects within this structure.
                     IF ( PRIM ) THEN
                        NOPRIM = NOPRIM + 1

*  Determine its data type.
                        CALL DAT_TYPE( LCMP, TYPE, STATUS )

*  Inquire the component's size.
                        CALL DAT_SIZE( LCMP, SIZE, STATUS )

*  Inquire the component's shape.  Is it an array or a scalar?
                        CALL DAT_SHAPE( LCMP, DAT__MXDIM, DIMS, NDIM,
     :                                  STATUS )

*  Inquire a character component's length in bytes per value, starting
*  a new error context as we wish to annul an error status if the
*  value is undefined.
                        IF ( INDEX( TYPE, '_CHAR' ) .NE. 0 ) THEN
                           CALL ERR_MARK
                           CALL DAT_LEN( LCMP, STRLEN, STATUS )

*  Check for an undefined object.
                           IF ( STATUS .EQ. DAT__UNSET ) THEN

*  Annul the error but record the fact by making the length negative.
                              CALL ERR_ANNUL( STATUS )
                              STRLEN = -1
                           END IF

*  Release the new error context.
                           CALL ERR_RLSE
                        END IF

*  Use a new error context as an undefined HDS value may be processed
*  and so the error must be annulled.
                        CALL ERR_MARK

*  Copy a scalar value to the table.
*  =================================

*  Deal with vectors and scalars separately.  First scalars.
                        IF ( NDIM .EQ. 0 ) THEN

*  Obtain each value using the appropriate type and write it to the
*  binary table.  Certain data types (_BYTE and _UWORD) are not
*  available in FITS.  These must be converted to the next higher
*  integer data type.  Note that there is no DAT_GET0UB or DAT_GET0W,
*  so use the _INTEGER type for these too.  HDS will do the type
*  conversion.  The data will be converted to the desired by the FITSIO
*  routine.

*  An integer type must have its null value defined before it is used.
                           IF ( TYPE .EQ. '_BYTE' .OR.
     :                          TYPE .EQ. '_UBYTE' .OR.
     :                          TYPE .EQ. '_WORD' .OR.
     :                          TYPE .EQ. '_UWORD' .OR.
     :                          TYPE .EQ. '_INTEGER' ) THEN

*  Assign the integer bad value for the data type.
                              IF ( TYPE .EQ. '_BYTE' ) THEN
                                 INULL = NUM_BTOI( VAL__BADB )
                              ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                                 INULL = NUM_UBTOI( VAL__BADUB )
                              ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                                 INULL = NUM_WTOI( VAL__BADW )
                              ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                                 INULL = NUM_UWTOI( VAL__BADUW )
                              ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                                 INULL = VAL__BADI
                              END IF

*  Set the current null value for undefined values.
                              CALL FTTNUL( FUNIT, NOPRIM, INULL, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
                              IF ( FSTAT .GT. FITSOK ) THEN
                                 CALL COF_FIOER( FSTAT,
     :                             'COF_H2BIN_ERR6', 'FTTNUL',
     :                             'Error setting null value for a '/
     :                             /'binary-table column.', STATUS )
                                 GOTO 999
                              END IF

*  Obtain an integer scalar value.
                              CALL DAT_GET0I( LCMP, IVALUE, STATUS )

*  Check for an HDS undefined value.
                              IF ( STATUS .EQ. DAT__UNSET ) THEN

*  Annul the bad status as we can cope with the error.
                                 CALL ERR_ANNUL( STATUS )

*  Set the binary-table entry to be undefined in this case.
                                 CALL FTPCLU( FUNIT, NOPRIM, 1, 1, 1,
     :                                        FSTAT )
                                 ROUTIN = 'FTPCLU'

*  Check for a bad (undefined value) of the appropriate data type; set
*  the table entry to be undefined in this case.
                              ELSE IF (
     :                          ( IVALUE .EQ. VAL__BADB .AND.
     :                            TYPE .EQ. '_BYTE' ) .OR.
     :                          ( IVALUE .EQ. VAL__BADUB .AND.
     :                            TYPE .EQ. '_UBYTE' ) .OR.
     :                          ( IVALUE .EQ. VAL__BADW .AND.
     :                            TYPE .EQ. '_WORD' ) .OR.
     :                          ( IVALUE .EQ. VAL__BADUW .AND.
     :                            TYPE .EQ. '_UWORD' ) .OR.
     :                          ( IVALUE .EQ. VAL__BADI .AND.
     :                            TYPE .EQ. '_INTEGER' ) ) THEN

                                 CALL FTPCLU( FUNIT, NOPRIM, 1, 1, 1,
     :                                        FSTAT )
                                 ROUTIN = 'FTPCLU'
                              ELSE

*  Copy the integer value to the FITS binary table.
                                 CALL FTPCLI( FUNIT, NOPRIM, 1, 1, 1,
     :                                        IVALUE, FSTAT )
                                 ROUTIN = 'FTPCLI'
                              END IF

*  64-bit integer
*  --------------
                           ELSE IF ( TYPE .EQ. '_INT64' ) THEN

*  Assign the bad value for the data type.
                              KNULL = VAL__BADK

*  Set the current null value for undefined values.
                              CALL FTPNULLL( FUNIT, NOPRIM, KNULL,
     :                                       FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
                              IF ( FSTAT .GT. FITSOK ) THEN
                                 CALL COF_FIOER( FSTAT,
     :                             'COF_H2BIN_ERR6', 'FTPNULLL',
     :                             'Error setting null value for a '/
     :                             /'binary-table column.', STATUS )
                                 GOTO 999
                              END IF

*  Obtain a 64-bit integer scalar value.
                              CALL DAT_GET0K( LCMP, KVALUE, STATUS )

*  Check for an HDS undefined value.
                              IF ( STATUS .EQ. DAT__UNSET ) THEN

*  Annul the bad status as we can cope with the error.
                                 CALL ERR_ANNUL( STATUS )

*  Set the binary-table entry to be undefined in this case.
                                 CALL FTPCLU( FUNIT, NOPRIM, 1, 1, 1,
     :                                        FSTAT )
                                 ROUTIN = 'FTPCLU'

*  Check for a bad (undefined value) and set the table entry to be
*  undefined in this case.
                              ELSE IF ( KVALUE .EQ. VAL__BADK ) THEN

                                 CALL FTPCLU( FUNIT, NOPRIM, 1, 1, 1,
     :                                        FSTAT )
                                 ROUTIN = 'FTPCLU'
                              ELSE

*  Copy the 64-bit integer value to the FITS binary table.
                                 CALL FTPCLK( FUNIT, NOPRIM, 1, 1, 1,
     :                                        KVALUE, FSTAT )
                                 ROUTIN = 'FTPCLK'
                              END IF

*  Short string
*  ------------
                           ELSE IF ( TYPE( 1:5 ) .EQ. '_CHAR' .AND.
     :                               STRLEN .LE. MXSLEN ) THEN

*  Start a new error context as we wish to annul an error status if the
*  value is undefined.  Get the value.
                              CALL ERR_MARK
                              CALL DAT_GET0C( LCMP, CVALUE, STATUS )

*  Check for an undefined object.
                              IF ( STATUS .EQ. DAT__UNSET ) THEN

*  Annul the error but record the setting the value to be null, i.e.
*  a blank string.
                                 CALL ERR_ANNUL( STATUS )
                                 CVALUE = ' '
                              END IF

*  Release the new error context.
                              CALL ERR_RLSE

*  Copy the value to the binary-table.
                              CALL FTPCLS( FUNIT, NOPRIM, 1, 1, 1,
     :                                     CVALUE ( :STRLEN ), FSTAT )
                              ROUTIN = 'FTPCLS'

*  String
*  ------

*  Obtain a string.  Copy it to the FITS binary table.  Start a new
*  error context as we wish to annul an error status if the value is
*  undefined.  Get the value.  Only MXSLEN characters can be stored in
*  the character variable so for long strings map the array.
                           ELSE IF ( TYPE( 1:5 ) .EQ. '_CHAR' .AND.
     :                               STRLEN .GT. MXSLEN ) THEN
                              CALL ERR_MARK
                              CALL DAT_MAPV( LCMP, TYPE, 'READ', OPNTR,
     :                                       EL, STATUS )

*  Check for an undefined object.
                              IF ( STATUS .EQ. DAT__UNSET ) THEN

*  Annul the error but record the setting the value to be null, i.e.
*  a blank string.  Copy this blank string to the binary table.
                                 CALL ERR_ANNUL( STATUS )
                                 CVALUE = ' '
                                 CALL FTPCLS( FUNIT, NOPRIM, 1, 1, EL,
     :                                        CVALUE, FSTAT )
                              ELSE

*  Copy the mapped value to the binary table.
                                 CALL FTPCLS( FUNIT, NOPRIM, 1, 1, EL,
     :                                      %VAL( CNF_PVAL( OPNTR ) ),
     :                                      FSTAT,
     :                                      %VAL( CNF_CVAL( STRLEN ) ) )
                              END IF

*  Release the new error context.
                              CALL ERR_RLSE

*  Record the routine name in case of error and unmap the value.
                              ROUTIN = 'FTPCLS'
                              CALL DAT_UNMAP( LCMP, STATUS )

*  Real scalar value
*  -----------------
                           ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                              CALL DAT_GET0R( LCMP, RVALUE, STATUS )

*  Check for an HDS-undefined value.  Set the table entry to be
*  undefined in this case, and annul the error.
                              IF ( STATUS .EQ. DAT__UNSET ) THEN
                                 CALL ERR_ANNUL( STATUS )
                                 CALL FTPCLU( FUNIT, NOPRIM, 1, 1, 1,
     :                                        FSTAT )
                                 ROUTIN = 'FTPCLU'

*  Check for a bad (undefined value); set the table entry to be
*  undefined in this case.
                              ELSE IF ( RVALUE .EQ. VAL__BADR ) THEN
                                 CALL FTPCLU( FUNIT, NOPRIM, 1, 1, 1,
     :                                        FSTAT )
                                 ROUTIN = 'FTPCLU'
                              ELSE

*  Copy the real value to the FITS binary table.
                                 CALL FTPCLE( FUNIT, NOPRIM, 1, 1, 1,
     :                                        RVALUE, FSTAT )
                                 ROUTIN = 'FTPCLE'
                              END IF

*  Double-precision scalar value.
*  ------------------------------
                           ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN

*  Check for an HDS-undefined value.  Set the table entry to be
*  undefined in this case, and annul the error.
                              IF ( STATUS .EQ. DAT__UNSET ) THEN
                                 CALL ERR_ANNUL( STATUS )
                                 CALL FTPCLU( FUNIT, NOPRIM, 1, 1, 1,
     :                                        FSTAT )
                                 ROUTIN = 'FTPCLU'

*  Check for a bad (undefined value); set the table entry to be
*  undefined in this case.
                              ELSE IF ( DVALUE .EQ. VAL__BADD ) THEN
                                 CALL FTPCLU( FUNIT, NOPRIM, 1, 1, 1,
     :                                        FSTAT )
                                 ROUTIN = 'FTPCLU'

                              ELSE

*  Copy the double-precision value to the FITS binary table.
                                 CALL FTPCLD( FUNIT, NOPRIM, 1, 1, 1,
     :                                        DVALUE, FSTAT )
                                 ROUTIN = 'FTPCLD'
                              END IF

*  Logical scalar value
*  --------------------
                           ELSE IF ( TYPE .EQ. '_LOGICAL' ) THEN
                              CALL DAT_GET0L( LCMP, LVALUE, STATUS )

*  Check for an HDS-undefined value.  Since there is no logical null
*  value, by convention it is set to be true, and annul the error.
                              IF ( STATUS .EQ. DAT__UNSET ) THEN
                                 CALL ERR_ANNUL( STATUS )
                                 LVALUE = .TRUE.
                              END IF

*  Copy the logical value to the FITS binary table.
                              CALL FTPCLL( FUNIT, NOPRIM, 1, 1, 1,
     :                                        LVALUE, FSTAT )
                              ROUTIN = 'FTPCLL'
                           END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
                           IF ( FSTAT .GT. FITSOK ) THEN
                              CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR4',
     :                           ROUTIN, 'Error copying scalar value '/
     :                           /'to the binary table.', STATUS )
                              GOTO 999
                           END IF

                        ELSE

*  Copy an array to the table.
*  ===========================

*  An integer type must have its null value defined before it is used.
                           IF ( TYPE .EQ. '_BYTE' .OR.
     :                          TYPE .EQ. '_UBYTE' .OR.
     :                          TYPE .EQ. '_WORD' .OR.
     :                          TYPE .EQ. '_UWORD' .OR.
     :                          TYPE .EQ. '_INTEGER' ) THEN

*  Assign the integer bad value for the data type.
                              IF ( TYPE .EQ. '_BYTE' ) THEN
                                 INULL = NUM_BTOI( VAL__BADB )
                              ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                                 INULL = NUM_UBTOI( VAL__BADUB )
                              ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                                 INULL = NUM_WTOI( VAL__BADW )
                              ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                                 INULL = NUM_UWTOI( VAL__BADUW )
                              ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                                 INULL = VAL__BADI
                              END IF

*  Set the current null value for undefined values.
                              CALL FTTNUL( FUNIT, NOPRIM, INULL, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
                              IF ( FSTAT .GT. FITSOK ) THEN
                                 CALL COF_FIOER( FSTAT,
     :                             'COF_H2BIN_ERR6', 'FTTNUL',
     :                             'Error setting null value for a '/
     :                             /'binary-table column.', STATUS )
                                 GOTO 999
                              END IF

                           ELSE IF ( TYPE .EQ. '_INT64' ) THEN

*  Set the current null value for undefined values.
                              KNULL = VAL__BADK
                              CALL FTPNULLL( FUNIT, NOPRIM, KNULL,
     :                                       FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
                              IF ( FSTAT .GT. FITSOK ) THEN
                                 CALL COF_FIOER( FSTAT,
     :                             'COF_H2BIN_ERR6', 'FTPNULLL',
     :                             'Error setting null value for a '/
     :                             /'binary-table column.', STATUS )
                                 GOTO 999
                              END IF

                           END IF

*  Map each array using the appropriate type and write it to the binary
*  table.  Certain data types (_BYTE and _UWORD) are not available in
*  FITS.  These must be converted to the next higher integer data type.
*  HDS undefined values are assigned the table undefined value.  A new
*  error context is used handle undefined values transparently.

*  Byte is converted to word for the binary table.
                           IF ( TYPE .EQ. '_BYTE' ) THEN
                              CALL DAT_MAPV( LCMP, '_WORD', 'READ',
     :                                       OPNTR, EL, STATUS )

                              IF ( STATUS .EQ. DAT__UNSET ) THEN
                                 CALL ERR_ANNUL( STATUS )
                                 CALL FTPCLU( FUNIT, NOPRIM, 1, 1, EL,
     :                                        FSTAT )
                                 ROUTIN = 'FTPCLU'
                              ELSE
                                 CALL FTPCNI( FUNIT, NOPRIM, 1, 1, EL,
     :                                        %VAL( CNF_PVAL( OPNTR ) ),
     :                                        NUM_BTOW( VAL__BADB ),
     :                                        FSTAT )
                                 ROUTIN = 'FTPCNI'
                              END IF

*  Copy an unsigned-byte array to the binary table.
                           ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                              CALL DAT_MAPV( LCMP, TYPE, 'READ',
     :                                       OPNTR, EL, STATUS )

                              IF ( STATUS .EQ. DAT__UNSET ) THEN
                                 CALL ERR_ANNUL( STATUS )
                                 CALL FTPCLU( FUNIT, NOPRIM, 1, 1, EL,
     :                                        FSTAT )
                                 ROUTIN = 'FTPCLU'
                              ELSE
                                 CALL FTPCNB( FUNIT, NOPRIM, 1, 1, EL,
     :                                        %VAL( CNF_PVAL( OPNTR ) ),
     :                                        NUM_UBTOI( VAL__BADUB ),
     :                                        FSTAT )
                                 ROUTIN = 'FTPCNB'
                              END IF

*  Copy a word array to the binary table.
                           ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                              CALL DAT_MAPV( LCMP, TYPE, 'READ',
     :                                       OPNTR, EL, STATUS )

                              IF ( STATUS .EQ. DAT__UNSET ) THEN
                                 CALL ERR_ANNUL( STATUS )
                                 CALL FTPCLU( FUNIT, NOPRIM, 1, 1, EL,
     :                                        FSTAT )
                                 ROUTIN = 'FTPCLU'
                              ELSE
                                 CALL FTPCNI( FUNIT, NOPRIM, 1, 1, EL,
     :                                        %VAL( CNF_PVAL( OPNTR ) ),
     :                                        VAL__BADW, FSTAT )
                                 ROUTIN = 'FTPCNI'
                              END IF

*  Unsigned word is converted to integer for the binary table.
                           ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                              CALL DAT_MAPV( LCMP, '_INTEGER', 'READ',
     :                                       OPNTR, EL, STATUS )

                              IF ( STATUS .EQ. DAT__UNSET ) THEN
                                 CALL ERR_ANNUL( STATUS )
                                 CALL FTPCLU( FUNIT, NOPRIM, 1, 1, EL,
     :                                        FSTAT )
                                 ROUTIN = 'FTPCLU'
                              ELSE
                                 CALL FTPCNJ( FUNIT, NOPRIM, 1, 1, EL,
     :                                        %VAL( CNF_PVAL( OPNTR ) ),
     :                                        NUM_UWTOI( VAL__BADUW ),
     :                                        FSTAT )
                                 ROUTIN = 'FTPCNJ'
                              END IF

*  Copy an integer array to the binary table.
                           ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                              CALL DAT_MAPV( LCMP, TYPE, 'READ',
     :                                       OPNTR, EL, STATUS )

                              IF ( STATUS .EQ. DAT__UNSET ) THEN
                                 CALL ERR_ANNUL( STATUS )
                                 CALL FTPCLU( FUNIT, NOPRIM, 1, 1, EL,
     :                                        FSTAT )
                                 ROUTIN = 'FTPCLU'
                              ELSE
                                 CALL FTPCNJ( FUNIT, NOPRIM, 1, 1, EL,
     :                                        %VAL( CNF_PVAL( OPNTR ) ),
     :                                        VAL__BADI, FSTAT )
                                 ROUTIN = 'FTPCNJ'
                              END IF

*  Copy a 64-bit integer array to the binary table.
                           ELSE IF ( TYPE .EQ. '_INT64' ) THEN
                              CALL DAT_MAPV( LCMP, TYPE, 'READ',
     :                                       OPNTR, EL, STATUS )

                              IF ( STATUS .EQ. DAT__UNSET ) THEN
                                 CALL ERR_ANNUL( STATUS )
                                 CALL FTPCLU( FUNIT, NOPRIM, 1, 1, EL,
     :                                        FSTAT )
                                 ROUTIN = 'FTPCLU'
                              ELSE
                                 CALL FTPCNK( FUNIT, NOPRIM, 1, 1, EL,
     :                                        %VAL( CNF_PVAL( OPNTR ) ),
     :                                        VAL__BADK, FSTAT )
                                 ROUTIN = 'FTPCNK'
                              END IF

*  Copy a string array to the binary table.
                           ELSE IF ( TYPE( 1:5 ) .EQ. '_CHAR' ) THEN
                              CALL DAT_MAPV( LCMP, TYPE, 'READ', OPNTR,
     :                                       EL, STATUS )

*  There is no null character value, so set to be blank by convention.
*  Since this is really a fault in the NDF, the user can suffer
*  a little by calling the FITSIO routine for every element, instead of
*  getting workspace and filling it with blank values.
                              IF ( STATUS .EQ. DAT__UNSET ) THEN
                                 CALL ERR_ANNUL( STATUS )
                                 CVALUE = ' '
                                 DO LEL = 1, EL
                                    CALL FTPCLS( FUNIT, NOPRIM, 1, LEL,
     :                                           1, CVALUE( :STRLEN ),
     :                                           FSTAT )
                                 END DO
                              ELSE
                                 CALL FTPCLS( FUNIT, NOPRIM, 1, 1, EL,
     :                                      %VAL( CNF_PVAL( OPNTR ) ),
     :                                      FSTAT,
     :                                      %VAL( CNF_CVAL( STRLEN ) ) )
                              END IF
                              ROUTIN = 'FTPCLS'

*  Copy a real array to the binary table.
                           ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                              CALL DAT_MAPV( LCMP, TYPE, 'READ',
     :                                       OPNTR, EL, STATUS )

                              IF ( STATUS .EQ. DAT__UNSET ) THEN
                                 CALL ERR_ANNUL( STATUS )
                                 CALL FTPCLU( FUNIT, NOPRIM, 1, 1, EL,
     :                                        FSTAT )
                                 ROUTIN = 'FTPCLU'
                              ELSE
                                 CALL FTPCNE( FUNIT, NOPRIM, 1, 1, EL,
     :                                        %VAL( CNF_PVAL( OPNTR ) ),
     :                                        VAL__BADR, FSTAT )
                                 ROUTIN = 'FTPCNE'
                              END IF

*  Copy an double-precision array to the binary table.
                           ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                              CALL DAT_MAPV( LCMP, TYPE, 'READ',
     :                                       OPNTR, EL, STATUS )

                              IF ( STATUS .EQ. DAT__UNSET ) THEN
                                 CALL ERR_ANNUL( STATUS )
                                 CALL FTPCLU( FUNIT, NOPRIM, 1, 1, EL,
     :                                        FSTAT )
                                 ROUTIN = 'FTPCLU'
                              ELSE
                                 CALL FTPCND( FUNIT, NOPRIM, 1, 1, EL,
     :                                        %VAL( CNF_PVAL( OPNTR ) ),
     :                                        VAL__BADD, FSTAT )
                                 ROUTIN = 'FTPCND'
                              END IF

*  Copy a logical array to the binary table.
                           ELSE IF ( TYPE .EQ. '_LOGICAL' ) THEN
                              CALL DAT_MAPV( LCMP, TYPE, 'READ',
     :                                       OPNTR, EL, STATUS )

*  There is no null logical value, so set to be true by convention.
*  Since this is really a fault in the NDF, the user can suffer a
*  little by calling the FITSIO routine for every element, instead of
*  getting workspace and filling it with TRUE values.
                              IF ( STATUS .EQ. DAT__UNSET ) THEN
                                 CALL ERR_ANNUL( STATUS )
                                 DO LEL = 1, EL
                                    CALL FTPCLL( FUNIT, NOPRIM, 1, LEL,
     :                                           1, .TRUE., FSTAT )
                                 END DO
                              ELSE
                                 CALL FTPCLL( FUNIT, NOPRIM, 1, 1, EL,
     :                                        %VAL( CNF_PVAL( OPNTR ) ),
     :                                        FSTAT )
                                 ROUTIN = 'FTPCLL'
                              END IF

                           END IF

*  Unmap the array component.
                           CALL DAT_UNMAP( LCMP, STATUS )


*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
                           IF ( FSTAT .GT. FITSOK ) THEN
                              CALL COF_FIOER( FSTAT, 'COF_H2BIN_ERR5',
     :                           ROUTIN, 'Error copying an array '/
     :                           /'to the binary table.', STATUS )
                              GOTO 999
                           END IF
                        END IF

*  Release the error context.
                        CALL ERR_RLSE

*  Tidy the locator to the object.
                        CALL DAT_ANNUL( LCMP, STATUS )
                     END IF

                  END DO

*  Record that the extension was written satisfactorily.
                  WRITTN = .TRUE.
               END IF
            END IF
         END IF
      END IF

*  Exit point if something has gone wrong.
  999 CONTINUE

      END
