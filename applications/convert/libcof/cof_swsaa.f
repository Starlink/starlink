      SUBROUTINE COF_SWSAA( FUNIT, FILE, NDF, PROFIT, LOGHDR, FDL, TYPE,
     :                      STATUS )
*+
*  Name:
*     COF_SWSAA

*  Purpose:
*     Converts an ISO SWSAA binary table into an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_SWSAA( FUNIT, FILE, NDF, PROFIT, LOGHDR, FDL, TYPE,
*                     STATUS )

*  Description:
*     This routine converts an ISO SWS AA product stored in a FITS
*     binary table into a one-dimensional NDF.  The NDF contains data
*     and variance arrays, and the detector number is stored in the
*     quality.  The remaining columns (save two) are written to an
*     SWSAA extension.  The primary HDU headers may be written to the
*     standard FITS airlock extension.  See the "Notes" for further
*     details.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        Logical-unit number of the FITS file.
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the FITS file or device being converted.  This is
*        only used for error messages.
*     NDF = INTEGER (Given)
*        The NDF identifier of the output NDF.
*     PROFIT = LOGICAL (Given)
*        If .TRUE., the FITS headers are written to the NDF's FITS
*        extension.
*     LOGHDR = LOGICAL (Given)
*        If .TRUE., a record of the FITS headers is written to a log
*        file given by descriptor FDL.  If .FALSE., no log is made and
*        argument FDL is ignored.
*     FDL = INTEGER (Given)
*        The file descriptor for the log file.  This is ignored when
*        LOGHDR is .FALSE..
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the output NDF.  A null value means use the
*        data type of the FITS file, as given by the BITPIX keyword.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The conversion from binary-table columns to NDF objects is as
*     follows:
*
*          SWAAWAVE               Axis centres, label, and units
*          SWAAFLUX               Data array, label, and units
*          SWAASTDV               Data errors, hence variance
*          SWAADETN               Quality
*          SWAARPID               not copied
*          SWAASPAR               not copied
*          remaining columns      column_name in SWSAA extension
*
*     [optional_subroutine_items]...

*  Prior Requirements:
*     -  The FITS file should already have been opened by FITSIO, and
*     is in the HDU immediately prior to the BINTABLE extension that is
*     going to define the NDF.
*     [routine_prior_requirements]...

*  Copyright:
*     Copyright (C) 1996, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2008 Science & Technology Facilities
*     Council. All Rights Reserved.

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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1996 January 23 (MJC):
*        Original version.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2008 June 18 (MJC):
*        Trim trailing blanks from output NDF character components.
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
      CHARACTER * ( * ) FILE
      INTEGER NDF
      LOGICAL PROFIT
      LOGICAL LOGHDR
      INTEGER FDL
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a string less trailing
                                 ! blanks

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

      INTEGER MXCOLS             ! Maximum number of columns
      PARAMETER( MXCOLS = 14 )

*  Local Variables:
      LOGICAL BAD                ! Column array contains bad values?
      CHARACTER * ( 10 ) BTYPE ! Column TFORM data type
      CHARACTER * ( 256 ) BUFFER ! Used to form error messages
      BYTE BVALUE                ! A byte value
      CHARACTER * ( DAT__SZLOC ) CLOC ! Locator to extension component
      CHARACTER * ( DAT__SZNAM ) COLNAM ! Column name
      INTEGER COLNUM             ! Column number
      CHARACTER * ( 48 ) COMENT  ! Keyword comment
      CHARACTER * ( DAT__SZTYP ) CTYPE ! Column HDS data type
      INTEGER DATCOD             ! FITSIO data code
      INTEGER EL                 ! Number of rows in the table
      INTEGER FSTAT              ! FITSIO status
      INTEGER HDUTYP             ! HDU type (primary, IMAGE, ASCII or
                                 ! binary table)
      INTEGER I                  ! Loop counter
      INTEGER IERR               ! Index of first VEC_ routine error
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Processing and actual type for
                                 ! a column array
      CHARACTER * ( 8 ) KEYWRD   ! FITS header keyword
      INTEGER NC                 ! Used length of string
      INTEGER NCF                ! Number of characters in filename
      INTEGER NERR               ! Number of VEC_ routine errors
      INTEGER NFIELD             ! Number of fields in table
      INTEGER PNTR( 1 )          ! Pointer to a mapped column array
      INTEGER REPEAT             ! Column repeat count
      LOGICAL THERE              ! Header keyword is present?
      CHARACTER * ( 72 ) UNITS   ! Units of the data or axis array
      LOGICAL USED( MXCOLS )     ! Table column used?
      INTEGER WIDTH              ! Column width
      INTEGER WPNTR              ! Pointer to workspace
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator to the SWSAA extension

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read the main header into the FITS extension.  There are no group
*  parameters.
      IF ( PROFIT ) CALL COF_WFEXT( FUNIT, NDF, 0, 0, FILE, STATUS )

*  Write out the headers to a logfile.
      IF ( LOGHDR ) CALL COF_HDLOG( FUNIT, FDL, FILE, 1, STATUS )

*  Obtain the length of the filename.
      NCF = CHR_LEN( FILE )

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Skip to the next HDU.  This is defined to be a BINTABLE for the
*  SWS product.
      CALL FTMRHD( FUNIT, 1, HDUTYP, FSTAT )
      IF ( FSTAT .NE. FITSOK ) THEN
         BUFFER = 'Error skipping to the extension of the SWS FITS '/
     :             /'file '//FILE( :NCF )//'.'
         CALL COF_FIOER( FSTAT, 'COF_SWS_WREXT', 'FTMRHD', BUFFER,
     :                   STATUS )

      ELSE IF ( HDUTYP .NE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FILE', FILE )
         CALL ERR_REP( 'COF_SWS_NOBINTAB',
     :     'The first extension of ^FILE is not a BINTABLE.', STATUS )
      END IF

*  Define the shape of the NDF.
*  ============================

*  Obtain the number of elements.
      CALL COF_GKEYI( FUNIT, 'NAXIS2', THERE, EL, COMENT, STATUS )

*  Obtain the number of fields in the table.
      CALL COF_GKEYI( FUNIT, 'TFIELDS', THERE, NFIELD, COMENT, STATUS )

*  Set the bounds of the NDF.
      CALL NDF_SBND( 1, 1, EL, NDF, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Initialised flags to indicate that none of the binary-table columns
*  has been used.
      NFIELD = MIN( NFIELD, MXCOLS )
      DO I = 1, NFIELD
         USED( I ) = .FALSE.
      END DO

*  Obtain the flux and store as the data array.
*  =============================================

*  Find the column number of the flux.
      CALL FTGCNO( FUNIT, .FALSE., 'SWAAFLUX', COLNUM, FSTAT )

*  Find the data code (effectively the data type) for the column.
*  Use it along with the preferred data type to define the component's
*  type and the implementation type.
      CALL FTGTCL( FUNIT, COLNUM, DATCOD, REPEAT, WIDTH, FSTAT )
      CALL COF_STYPC( NDF, 'Data', TYPE, DATCOD, ITYPE, STATUS )

*  Map the NDF component.
      CALL NDF_MAP( NDF, 'Data', ITYPE, 'WRITE', PNTR, EL, STATUS )

*  Read the column into the data array.  Call the appropriate routine
*  for the chosen type.
      IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL FTGCVB( FUNIT, COLNUM, 1, 1, EL, VAL__BADUB,
     :                %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL FTGCVI( FUNIT, COLNUM, 1, 1, EL, VAL__BADW,
     :                %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL FTGCVJ( FUNIT, COLNUM, 1, 1, EL, VAL__BADI,
     :                %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL FTGCVE( FUNIT, COLNUM, 1, 1, EL, VAL__BADR,
     :                %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL FTGCVD( FUNIT, COLNUM, 1, 1, EL, VAL__BADD,
     :                %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IT', ITYPE )
         CALL ERR_REP( 'COF_SWS_ITYPE',
     :     'Invalid data type (^IT) selected for the SWS flux.',
     :     STATUS )
         GOTO 999
      END IF

*  Set the bad-pixel flag.
      CALL NDF_SBAD( BAD, NDF, 'Data', STATUS )

*  Unmap the data array.
      CALL NDF_UNMAP( NDF, 'Data', STATUS )

*  Check that the transfer was correct.
      IF ( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_SWS_CRDAT', 'FTGCVx',
     :     'Error copying the SWS flux to the NDF data array.', STATUS )
         GOTO 999
      END IF

*  Obtain the units for this column, and place it into the NDF.  To do
*  this first form the names of the units keyword for this column.
*  Note that NDF_CPUT does not truncate trailing blanks.
      CALL FTKEYN( 'TUNIT', COLNUM, KEYWRD, FSTAT )
      CALL COF_GKEYC( FUNIT, KEYWRD, THERE, UNITS, COMENT, STATUS )
      IF ( THERE ) THEN
         NC = CHR_LEN( UNITS )
         CALL NDF_CPUT( UNITS( :NC ), NDF, 'Units', STATUS )
      END IF

*  Set the label for the NDF.
      CALL NDF_CPUT( 'Flux', NDF, 'Label', STATUS )

*  Record that this column is used.
      USED( COLNUM ) = .TRUE.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the standard deviation and store as the error array.
*  ===========================================================

*  Find the column number of the errors.
      CALL FTGCNO( FUNIT, .FALSE., 'SWAASTDV', COLNUM, FSTAT )

*  Find the data code (effectively the data type) for the column.
*  Use it along with the preferred data type to define the component's
*  type and the implementation type.
      CALL FTGTCL( FUNIT, COLNUM, DATCOD, REPEAT, WIDTH, FSTAT )
      CALL COF_STYPC( NDF, 'Variance', TYPE, DATCOD, ITYPE, STATUS )

*  Map the NDF component.
      CALL NDF_MAP( NDF, 'Error', ITYPE, 'WRITE', PNTR, EL, STATUS )

*  Create some workspace to store the errors that may contain negative
*  values.  Read the column into the workspace.  Store the absolute
*  error values in the NDF Error array.  Call the appropriate routines
*  for the chosen type.
      IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL PSX_MALLOC( EL * VAL__NBUB, WPNTR, STATUS )
         CALL FTGCVB( FUNIT, COLNUM, 1, 1, EL, VAL__BADUB,
     :                %VAL( CNF_PVAL( WPNTR ) ), BAD, FSTAT )
         CALL VEC_ABSUB( .FALSE., EL, %VAL( CNF_PVAL( WPNTR ) ),
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL PSX_MALLOC( EL * VAL__NBW, WPNTR, STATUS )
         CALL FTGCVI( FUNIT, COLNUM, 1, 1, EL, VAL__BADW,
     :                %VAL( CNF_PVAL( WPNTR ) ), BAD, FSTAT )
         CALL VEC_ABSW( .FALSE., EL, %VAL( CNF_PVAL( WPNTR ) ),
     :                  %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL PSX_MALLOC( EL * VAL__NBI, WPNTR, STATUS )
         CALL FTGCVJ( FUNIT, COLNUM, 1, 1, EL, VAL__BADI,
     :                %VAL( CNF_PVAL( WPNTR ) ), BAD, FSTAT )
         CALL VEC_ABSI( .FALSE., EL, %VAL( CNF_PVAL( WPNTR ) ),
     :                  %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL PSX_MALLOC( EL * VAL__NBR, WPNTR, STATUS )
         CALL FTGCVE( FUNIT, COLNUM, 1, 1, EL, VAL__BADR,
     :                %VAL( CNF_PVAL( WPNTR ) ), BAD, FSTAT )
         CALL VEC_ABSR( .FALSE., EL, %VAL( CNF_PVAL( WPNTR ) ),
     :                  %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL PSX_MALLOC( EL * VAL__NBD, WPNTR, STATUS )
         CALL FTGCVD( FUNIT, COLNUM, 1, 1, EL, VAL__BADD,
     :                %VAL( CNF_PVAL( WPNTR ) ), BAD, FSTAT )
         CALL VEC_ABSD( .FALSE., EL, %VAL( CNF_PVAL( WPNTR ) ),
     :                  %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IT', ITYPE )
         CALL ERR_REP( 'COF_SWS_ITYPE',
     :     'Invalid data type (^IT) selected for the SWS error array.',
     :     STATUS )
         GOTO 999
      END IF

*  Release the workspace.
      CALL PSX_FREE( WPNTR, STATUS )

*  Set the bad-pixel flag.
      CALL NDF_SBAD( BAD, NDF, 'Variance', STATUS )

*  Unmap the error array.  Note this call is assymetric with NDF_MAP,
*  in that the error array can be mapped, but not unmapped;  we have
*  to specify the variance instead.
      CALL NDF_UNMAP( NDF, 'Variance', STATUS )

*  Check that the transfer was correct.
      IF ( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_SWS_CRVAR', 'FTGCVx',
     :     'Error copying the SWS standard deviation to the NDF '/
     :     /'error array.', STATUS )
         GOTO 999
      END IF

*  Record that this column is used.
      USED( COLNUM ) = .TRUE.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the detector number and store in the quality array.
*  ==========================================================

*  Find the column number of the detector numbers.
      CALL FTGCNO( FUNIT, .FALSE., 'SWAADETN', COLNUM, FSTAT )

*  This assumes that the detector number does not exceed 255, although
*  the data type is nominally J (_INTEGER).
*  Map the QUALITY component.
      CALL NDF_MAP( NDF, 'Quality', '_UBYTE', 'WRITE', PNTR, EL,
     :              STATUS )

*  Read the column into the quality array.  Note that there are no bad
*  values.
      BVALUE = 0
      CALL FTGCVB( FUNIT, COLNUM, 1, 1, EL, BVALUE,
     :             %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

*  Unmap the error array.
      CALL NDF_UNMAP( NDF, 'Quality', STATUS )

*  Check that the transfer was correct.
      IF ( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_SWS_CRQUA', 'FTGCVB',
     :     'Error copying the SWS detector numbers to the NDF '/
     :     /'quality array.', STATUS )
         GOTO 999
      END IF

*  Record that this column is used.
      USED( COLNUM ) = .TRUE.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the wavelengths and store as the axis centres.
*  =====================================================

*  Find the column number of the wavelengths.
      CALL FTGCNO( FUNIT, .FALSE., 'SWAAWAVE', COLNUM, FSTAT )

*  Find the data code (effectively the data type) for the column.
*  Use it along with the preferred data type to define the component's
*  type and the implementation type.
      CALL FTGTCL( FUNIT, COLNUM, DATCOD, REPEAT, WIDTH, FSTAT )
      CALL COF_ATYPC( NDF, 'Centre', 1, ' ', DATCOD, ITYPE, STATUS )

*  Map the NDF component.
      CALL NDF_AMAP( NDF, 'Centre', 1, ITYPE, 'WRITE', PNTR, EL,
     :               STATUS )

*  Read the column into the data array.  Call the appropriate routine
*  for the chosen type.
      IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL FTGCVE( FUNIT, COLNUM, 1, 1, EL, VAL__BADR,
     :                %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL FTGCVD( FUNIT, COLNUM, 1, 1, EL, VAL__BADD,
     :                %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IT', ITYPE )
         CALL ERR_REP( 'COF_SWS_ITYPE',
     :     'Invalid data type (^IT) selected for the SWS wavelengths.',
     :     STATUS )
         GOTO 999
      END IF

*  Unmap the data array.
      CALL NDF_AUNMP( NDF, 'Centre', 1, STATUS )

*  Check that the transfer was correct.
      IF ( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_SWS_CRDAT', 'FTGCVx',
     :     'Error copying the SWS wavelengths to the NDF axis array.',
     :     STATUS )
         GOTO 999
      END IF

*  Obtain the units for this column, and place it into the NDF.  To do
*  this first form the names of the units keyword for this column.
      CALL FTKEYN( 'TUNIT', COLNUM, KEYWRD, FSTAT )
      CALL COF_GKEYC( FUNIT, KEYWRD, THERE, UNITS, COMENT, STATUS )
      IF ( THERE ) THEN
         NC = CHR_LEN( UNITS )
         CALL NDF_ACPUT( UNITS( :NC ), NDF, 'Units', 1, STATUS )
      END IF

*  Set the label for the axis.
      CALL NDF_ACPUT( 'Wavelength', NDF, 'Label', 1, STATUS )

*  Record that this column is used.
      USED( COLNUM ) = .TRUE.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Exclude unwanted columns.
*  =========================

*  Find the column number of the unwanted ancillary data.
      CALL FTGCNO( FUNIT, .FALSE., 'SWAARPID', COLNUM, FSTAT )

*  Record that this column is used.
      USED( COLNUM ) = .TRUE.

*  Find the column number of the unwanted ancillary data.
      CALL FTGCNO( FUNIT, .FALSE., 'SWAASPAR', COLNUM, FSTAT )

*  Record that this column is used.
      USED( COLNUM ) = .TRUE.

*  Create an extension for the remaining columns.
*  ==============================================

*  Here it is assumed that there are still columns left.
      CALL NDF_XNEW( NDF, 'SWSAA', 'SWSAA_EXT', 0, 0, XLOC, STATUS )

*  Loop through all the fields, copying those not already used and
*  wanted.
      DO COLNUM = 1, NFIELD
         IF ( .NOT. USED( COLNUM ) ) THEN

*  Obtain the name of the column.  First form the name of the name
*  keyword for this column.
            CALL FTKEYN( 'TTYPE', COLNUM, KEYWRD, FSTAT )
            CALL COF_GKEYC( FUNIT, KEYWRD, THERE, COLNAM, COMENT,
     :                      STATUS )
            IF ( THERE ) THEN

*  Obtain its data type.
               CALL FTKEYN( 'TFORM', COLNUM, KEYWRD, FSTAT )
               CALL COF_GKEYC( FUNIT, KEYWRD, THERE, BTYPE, COMENT,
     :                         STATUS )

*  Convert the binary table type into an HDS type.  Note we assume
*  that there are no strings.
               IF ( THERE ) THEN
                  CALL COF_BN2HT( BTYPE, CTYPE, STATUS )
               ELSE
                  CTYPE = '_REAL'
               END IF

*  Create the component of the extension, and get a locator to the
*  component.
               CALL DAT_NEW( XLOC, COLNAM, CTYPE, 1, EL, STATUS )
               CALL DAT_FIND( XLOC, COLNAM, CLOC, STATUS )

*  Map the component for writing.
               CALL DAT_MAP( CLOC, CTYPE, 'WRITE', 1, EL, PNTR, STATUS )

*  Read the column into the data array.  Call the appropriate routine
*  for the chosen type.
               IF ( CTYPE .EQ. '_UBYTE' ) THEN
                  CALL FTGCVB( FUNIT, COLNUM, 1, 1, EL, VAL__BADUB,
     :                         %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                         BAD, FSTAT )

               ELSE IF ( CTYPE .EQ. '_WORD' ) THEN
                  CALL FTGCVI( FUNIT, COLNUM, 1, 1, EL, VAL__BADW,
     :                         %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                         BAD, FSTAT )

               ELSE IF ( CTYPE .EQ. '_INTEGER' ) THEN
                  CALL FTGCVJ( FUNIT, COLNUM, 1, 1, EL, VAL__BADI,
     :                         %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                         BAD, FSTAT )

               ELSE IF ( CTYPE .EQ. '_REAL' ) THEN
                  CALL FTGCVE( FUNIT, COLNUM, 1, 1, EL, VAL__BADR,
     :                         %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                         BAD, FSTAT )

               ELSE IF ( CTYPE .EQ. '_DOUBLE' ) THEN
                  CALL FTGCVD( FUNIT, COLNUM, 1, 1, EL, VAL__BADD,
     :                         %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                         BAD, FSTAT )

               END IF

*  Tidy the locator to the component.
               CALL DAT_ANNUL( CLOC, STATUS )
            END IF
         END IF
      END DO

*  Tidy the locator to the extension.
      CALL DAT_ANNUL( XLOC, STATUS )

  999 CONTINUE

      END
