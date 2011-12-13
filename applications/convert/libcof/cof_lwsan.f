      SUBROUTINE COF_LWSAN( FUNIT, FILE, NDF, PROFIT, LOGHDR, FDL, TYPE,
     :                      STATUS )
*+
*  Name:
*     COF_LWSAN

*  Purpose:
*     Converts an ISO LWSAN binary table into an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_LWSAN( FUNIT, FILE, NDF, PROFIT, LOGHDR, FDL, TYPE,
*                     STATUS )

*  Description:
*     This routine converts an ISO LWS AN product stored in a FITS
*     binary table into a four-dimensonal NDF.  The NDF contains data
*     and variance arrays, and has a quality array: the detector number
*     is stored in the first-four bits and the scan direction in the
*     fifth bit.  The first two dimensions are the spatial bins, next is
*     the wavelength axis, and the fourth is the scan count.  The
*     wavelength axis comprises all the distinct values found in the LWS
*     AN file.  The remaining columns (save the filler) are written to
*     an LWSAN extension.  The primary HDU headers may be written to the
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
*          LSANFLX                Data array, label, and units
*          LSANFLXU               Data errors, hence variance
*          LSANDET                Quality (bits 1 to 4)
*          LSANSDIR               Quality (bit 5)
*          LSANRPID               Axis centres, labels, and units
*                                 (x-y positions---dimensions 1 and 2)
*          LSANSCNT               Axis centre, label, and unit (scan
*                                 count index---dimension 4)
*          LSANWAV                Axis centre, label, and unit
*                                 (wavelength---dimension 3)
*          LSANWAVU               Axis errors (wavelength---dimension 3)
*          LSANFILL               not copied
*          remaining columns      column name in LWSAN extension

*  Prior Requirements:
*     -  The FITS file should already have been opened by FITSIO, and
*     is in the HDU immediately prior to the BINTABLE extension that is
*     going to define the NDF.

*  Copyright:
*     Copyright (C) 1996, 2000 Central Laboratory of the Research
*     Councils. Copyright (C) 2008, 2009 Science & Technology Facilities
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
*     AJC: Alan J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 April 30 (MJC):
*        Original version.
*     2000 September 12 (AJC):
*        Make NAXES a genuine array (was 2) in call to FTGSVJ to
*        prevent a crash on Alphas with CFITSIO.  Use CNF_PVAL for
*        safety.
*     2008 March 15 (MJC):
*        Use KAPLIBS routine instead of its cloned CON equivalent.
*     2008 June 18 (MJC):
*        Trim trailing blanks from output NDF character components.
*     2009 June 29 (MJC):
*        Replaced deprecated CON_MOVE with KPG1_COPY from KAPLIBS.
*     2009 June 30 (MJC):
*        Replaced calls to cloned routine with original from KAPLIBS.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'CNF_PAR'          ! CNF_PVAL

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
      PARAMETER( MXCOLS = 13 )

      INTEGER NDIM               ! Dimensionality of the created NDF
      PARAMETER( NDIM = 4 )

*  Local Variables:
      INTEGER APNTR( 1 )         ! Pointer to mapped NDF axis array
      LOGICAL BAD                ! Column array contains bad values?
      CHARACTER * ( 10 ) BTYPE ! Column TFORM data type
      CHARACTER * ( 256 ) BUFFER ! Used to form error messages
      CHARACTER * ( DAT__SZLOC ) CLOC ! Locator to extension component
      CHARACTER * ( DAT__SZNAM ) COLNAM ! Column name
      INTEGER COLNUM             ! Column number
      CHARACTER * ( 48 ) COMENT  ! Keyword comment
      CHARACTER * ( DAT__SZTYP ) CTYPE ! Column HDS data type
      INTEGER DATCOD             ! FITSIO data code
      INTEGER DPNTR              ! Pointer to column of detector ids
      INTEGER EL                 ! Number of rows in the table
      INTEGER EXPNTR             ! Pointer to mapped extension array
                                 ! component
      INTEGER FPNTR              ! Pointer to column of fluxes
      INTEGER FEPNTR             ! Pointer to column of flux errors
      INTEGER FPIXEL( 2 )        ! First element of column to be copied
      INTEGER FSTAT              ! FITSIO status
      INTEGER HDUTYP             ! HDU type (primary, IMAGE, ASCII or
                                 ! binary table)
      INTEGER I                  ! Loop counter
      INTEGER INCS( 2 )          ! Sampling interval for elements in
                                 ! a column
      INTEGER IPNTR              ! Pointer to sorted-wavelength indices
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Processing and actual type for
                                 ! a column array
      CHARACTER * ( 8 ) KEYWRD   ! FITS header keyword
      INTEGER LBND( NDIM )       ! Lower bounds of the NDF
      INTEGER LPIXEL( 2 )        ! Last element of column to be copied
      INTEGER MAXPOS             ! Index of the maximum value of a
                                 ! column
      INTEGER MINPOS             ! Index of the minimum value of a
                                 ! column
      INTEGER NAXES(1)           ! Size of each dimension
      INTEGER NC                 ! Used length of string
      INTEGER NCF                ! Number of characters in filename
      INTEGER NFIELD             ! Number of fields in table
      INTEGER NINVAL             ! Number of bad values in a column
      INTEGER NOBS               ! Number of LWS observations
      INTEGER NWAVEL             ! Number of distinct wavelengths
      INTEGER PNTR( 2 )          ! Pointer to mapped NDF data and error
                                 ! arrays
      INTEGER QPNTR( 1 )         ! Pointer to mapped NDF quality array
      INTEGER REPEAT             ! Column repeat count
      INTEGER REPNTR             ! Pointer to array of distinct and
                                 ! ordered wavelength errors
      INTEGER RWPNTR             ! Pointer to array of distinct and
                                 ! ordered wavelengths
      INTEGER SDPNTR             ! Pointer to column of scan directions
      INTEGER SPNTR              ! Pointer to column of scan counts
      LOGICAL THERE              ! Header keyword is present?
      INTEGER UBND( NDIM )       ! Upper bounds of the NDF
      CHARACTER * ( 72 ) UNITS   ! Units of the data or axis array
      LOGICAL USED( MXCOLS )     ! Table column used?
      INTEGER WAPNTR             ! Pointer to column of wavelengths
      INTEGER WEPNTR             ! Pointer to column of wavelength
                                 ! errors
      INTEGER WIDTH              ! Column width
      INTEGER WIPNTR             ! Pointer to z-axis indices for the
                                 ! column of wavelengths
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator to the LWSAN extension
      INTEGER XPNTR              ! Pointer to column of x indices
      INTEGER YPNTR              ! Pointer to column of x indices
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
     :            /'file '//FILE( :NCF )//'.'

         CALL COF_FIOER( FSTAT, 'COF_SWS_WREXT', 'FTMRHD',
     :                   BUFFER, STATUS )

      ELSE IF ( HDUTYP .NE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FILE', FILE )
         CALL ERR_REP( 'COF_SWS_NOBINTAB',
     :     'The first extension of ^FILE is not a BINTABLE.', STATUS )
      END IF

*  Determine the shape of the binary table.
*  ========================================

*  Obtain the number of elements.
      CALL COF_GKEYI( FUNIT, 'NAXIS2', THERE, NOBS, COMENT, STATUS )

*  Obtain the number of fields in the table.
      CALL COF_GKEYI( FUNIT, 'TFIELDS', THERE, NFIELD, COMENT, STATUS )

*  Initialise flags to indicate that none of the binary-table columns
*  has been used.
      NFIELD = MIN( NFIELD, MXCOLS )
      DO I = 1, NFIELD
         USED( I ) = .FALSE.
      END DO

*  Define the shape of the NDF.
*  ============================

*  This is not straightforward because the instrument team didn't see
*  fit to store this information in the primary header.  Instead one
*  has to find the range of the raster point identifier.  This is
*  stored as as two byte integers in the second column, the first
*  element being the x index, and the second being the y index.
*
*  Likewise the number of individual scans is not given in the
*  main header, so must be counted.
*
*  Obtain workspace for the three columns.

      CALL PSX_CALLOC( NOBS, '_INTEGER', XPNTR, STATUS )
      CALL PSX_CALLOC( NOBS, '_INTEGER', YPNTR, STATUS )
      CALL PSX_CALLOC( NOBS, '_INTEGER', SPNTR, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Find the column number of the raster-point identification.
      CALL FTGCNO( FUNIT, .FALSE., 'LSANRPID', COLNUM, FSTAT )

*  Record that this column is used.
      USED( COLNUM ) = .TRUE.

*  Obtain the x indices from this column.  There are 2 elements in each
*  vector.
      FPIXEL( 1 ) = 1
      LPIXEL( 1 ) = FPIXEL( 1 )
      FPIXEL( 2 ) = 1
      LPIXEL( 2 ) = NOBS

      INCS( 1 ) = 1
      INCS( 2 ) = 1
      CALL FTGSVJ( FUNIT, COLNUM, 1, NAXES(1), FPIXEL, LPIXEL, INCS,
     :             VAL__BADI, %VAL( CNF_PVAL( XPNTR ) ), BAD, FSTAT )

*  Check that the transfer was correct.
      IF ( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_LWS_XINDEX', 'FTGSVJ',
     :     'Error obtaining the LWS x raster identifiers.', STATUS )
         GOTO 999
      END IF

*  Find the range of the x indices.
      CALL KPG1_MXMNI( BAD, NOBS, %VAL( CNF_PVAL( XPNTR ) ), NINVAL,
     :                 UBND( 1 ), LBND( 1 ), MAXPOS, MINPOS, STATUS )

*  Obtain the y indices from this column.  There are 2 elements in each
*  vector.
      NAXES(1) = 2
      FPIXEL( 1 ) = 2
      LPIXEL( 1 ) = FPIXEL( 1 )
      CALL FTGSVJ( FUNIT, COLNUM, 1, NAXES(1), FPIXEL, LPIXEL, INCS,
     :             VAL__BADI, %VAL( CNF_PVAL( YPNTR ) ), BAD, FSTAT )

*  Check that the transfer was correct.
      IF ( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_LWS_YINDEX', 'FTGSVJ',
     :     'Error obtaining the LWS y raster identifiers.', STATUS )
         GOTO 999
      END IF

*  Find the range of the y indices.
      CALL KPG1_MXMNI( BAD, NOBS, %VAL( CNF_PVAL( YPNTR ) ), NINVAL,
     :                 UBND( 2 ), LBND( 2 ), MAXPOS, MINPOS, STATUS )

*  Find the column number of the scan counter.
      CALL FTGCNO( FUNIT, .FALSE., 'LSANSCNT', COLNUM, FSTAT )

*  Record that this column is used.
      USED( COLNUM ) = .TRUE.

*  Obtain the scan count indices from this column.
      CALL FTGCVJ( FUNIT, COLNUM, 1, 1, NOBS, VAL__BADI,
     :             %VAL( CNF_PVAL( SPNTR ) ), BAD, FSTAT )

*  Check that the transfer was correct.
      IF ( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_LWS_XINDEX', 'FTGCVI',
     :     'Error obtaining the LWS scan counters.', STATUS )
         GOTO 999
      END IF

*  Find the range of the scan counts.
      CALL KPG1_MXMNI( BAD, NOBS, %VAL( CNF_PVAL( SPNTR ) ), NINVAL,
     :                 UBND( 4 ), LBND( 4 ), MAXPOS, MINPOS, STATUS )

*  Temporarily set the bounds of the NDF.  This is need to enable the
*  axis type along the third dimension to be set in the COF_ATYPC call.
      LBND( 3 ) = 1
      UBND( 3 ) = 1
      CALL NDF_SBND( NDIM, LBND, UBND, NDF, STATUS )

*  Obtain workspace for the wavelengths, and their errors.
      CALL PSX_CALLOC( NOBS, '_REAL', WAPNTR, STATUS )
      CALL PSX_CALLOC( NOBS, '_REAL', WEPNTR, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Find the column number of the wavelengths.
      CALL FTGCNO( FUNIT, .FALSE., 'LSANWAV', COLNUM, FSTAT )

*  Find the data code (effectively the data type) for the column.
*  Use it along with the preferred data type to define the component's
*  type and the implementation type.
      CALL FTGTCL( FUNIT, COLNUM, DATCOD, REPEAT, WIDTH, FSTAT )
      CALL COF_ATYPC( NDF, 'Centre', 3, ' ', DATCOD, ITYPE, STATUS )

*  Record that this column is used.
      USED( COLNUM ) = .TRUE.

*  Obtain the wavelengths from this column.
      CALL FTGCVE( FUNIT, COLNUM, 1, 1, NOBS, VAL__BADR,
     :             %VAL( CNF_PVAL( WAPNTR ) ), BAD, FSTAT )

*  Check that the transfer was correct.
      IF ( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_LWS_LAMBDA', 'FTGCVE',
     :     'Error obtaining the wavelength column.', STATUS )
         GOTO 999
      END IF

*  Find the column number of the wavelength errors.
      CALL FTGCNO( FUNIT, .FALSE., 'LSANWAVU', COLNUM, FSTAT )

*  Record that this column is used.
      USED( COLNUM ) = .TRUE.

*  Obtain the wavelength errors from this column.
      CALL FTGCVE( FUNIT, COLNUM, 1, 1, NOBS, VAL__BADR,
     :             %VAL( CNF_PVAL( WEPNTR ) ), BAD, FSTAT )

*  Check that the transfer was correct.
      IF ( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_LWS_LAMBER', 'FTGCVE',
     :     'Error obtaining the wavelength-error column.', STATUS )
         GOTO 999
      END IF

*  An NDF requires that its axis co-ordinates applies to all elements,
*  i.e. it comprises a _vector_ along each axis.  In the LWS data the
*  wavelengths and pixel indices are jumbled, so we must find the
*  number and values of the unique wavelengths and their errors.  (This
*  assumes that there are no identical wavelengths arising from
*  different detectors.)
*
*  Obtain the workspace for the reduced list of wavelengths and errors,
*  and indices to the unsorted wavelengths.
      CALL PSX_CALLOC( NOBS, '_REAL', RWPNTR, STATUS )
      CALL PSX_CALLOC( NOBS, '_REAL', REPNTR, STATUS )
      CALL PSX_CALLOC( NOBS, '_INTEGER', IPNTR, STATUS )

*  Call the routine to count and store the unique wavelengths.
      CALL COF_LWS1( NOBS, VAL__EPSR, %VAL( CNF_PVAL( WAPNTR ) ),
     :               %VAL( CNF_PVAL( WEPNTR ) ), NWAVEL,
     :               %VAL( CNF_PVAL( RWPNTR ) ),
     :               %VAL( CNF_PVAL( REPNTR ) ),
     :               %VAL( CNF_PVAL( IPNTR ) ), STATUS )

*  By definition the lower bound of the spectral axis is 1.
      LBND( 3 ) = 1
      UBND( 3 ) = NWAVEL

*  Set the bounds of the NDF.
      CALL NDF_SBND( NDIM, LBND, UBND, NDF, STATUS )

*  Convert the wavelength scale into pixel indices.
*  ================================================

*  Obtain the workspace for the pixel indices of the wavelengths.
      CALL PSX_CALLOC( NOBS, '_REAL', WIPNTR, STATUS )

*  Convert the wavelengths into floating-point indices.
      CALL KPG1_AXCOR( 1, NWAVEL, %VAL( CNF_PVAL( RWPNTR ) ), NOBS,
     :                 %VAL( CNF_PVAL( WAPNTR ) ),
     :                 %VAL( CNF_PVAL( WIPNTR ) ), STATUS )

*  Free the resources used for the columns of wavelengths and their
*  associated errors.
      CALL PSX_FREE( IPNTR, STATUS )
      CALL PSX_FREE( WAPNTR, STATUS )
      CALL PSX_FREE( WEPNTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Create default axes.
*  ====================
      CALL NDF_ACRE( NDF, STATUS )

*  Store the wavelengths as the third-axis centres.
*  ================================================

*  Map the NDF axis centres for the wavelengths.
      CALL NDF_AMAP( NDF, 'Centre', 3, ITYPE, 'WRITE', APNTR, NWAVEL,
     :               STATUS )

*  Copy the wavelength values into the axis centres.  Call the
*  appropriate routine for the chosen type.
      IF ( ITYPE .EQ. '_REAL' .OR. ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_COPY( ITYPE, NWAVEL, RWPNTR, APNTR( 1 ), STATUS )

      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IT', ITYPE )
         CALL ERR_REP( 'COF_LWSAN_ITYPE',
     :     'Invalid data type (^IT) selected for the LWS wavelengths.',
     :     STATUS )
         GOTO 999
      END IF

*  Unmap the data array.
      CALL NDF_AUNMP( NDF, 'Centre', 3, STATUS )

*  Free the resources used for the reduced list of wavelengths.
      CALL PSX_FREE( RWPNTR, STATUS )

*  Store the wavelength errors as the third-axis errors.
*  =====================================================

*  Map the NDF errors for the wavelength errors.
      CALL NDF_AMAP( NDF, 'Error', 3, ITYPE, 'WRITE', APNTR, NWAVEL,
     :               STATUS )

*  Copy the wavelength error values into the axis errors.  Call the
*  appropriate routine for the chosen type.
      CALL KPG1_COPY( ITYPE, NWAVEL, REPNTR, APNTR( 1 ), STATUS )

*  Unmap the variance array.
      CALL NDF_AUNMP( NDF, 'Variance', 3, STATUS )

*  Free the resources used for the reduced list of wavelength errors.
      CALL PSX_FREE( REPNTR, STATUS )

*  Write other axis components.
*  ============================

*  Find the column number of the wavelengths.
      CALL FTGCNO( FUNIT, .FALSE., 'LSANWAV', COLNUM, FSTAT )

*  Obtain the units for this column, and place it into the NDF.  To do
*  this first form the names of the units keyword for this column.
      CALL FTKEYN( 'TUNIT', COLNUM, KEYWRD, FSTAT )
      CALL COF_GKEYC( FUNIT, KEYWRD, THERE, UNITS, COMENT, STATUS )
      IF ( THERE ) THEN
         NC = CHR_LEN( UNITS )
         CALL NDF_ACPUT( UNITS( :NC ), NDF, 'Units', 3, STATUS )
      END IF
      CALL NDF_ACPUT( 'Pixels', NDF, 'Units', 1, STATUS )
      CALL NDF_ACPUT( 'Pixels', NDF, 'Units', 2, STATUS )

*  Set the labels for the axes.
      CALL NDF_ACPUT( 'X position', NDF, 'Label', 1, STATUS )
      CALL NDF_ACPUT( 'Y position', NDF, 'Label', 2, STATUS )
      CALL NDF_ACPUT( 'Wavelength', NDF, 'Label', 3, STATUS )
      CALL NDF_ACPUT( 'Scan count', NDF, 'Label', 4, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Change the data type of the NDF arrays.
*  =======================================

*  Find the column number of the flux.
      CALL FTGCNO( FUNIT, .FALSE., 'LSANFLX', COLNUM, FSTAT )

*  Find the data code (effectively the data type) for the column.
*  Use it along with the preferred data type to define the component's
*  type and the implementation type.
      CALL FTGTCL( FUNIT, COLNUM, DATCOD, REPEAT, WIDTH, FSTAT )
      CALL COF_STYPC( NDF, 'Data', TYPE, DATCOD, ITYPE, STATUS )
      CALL NDF_STYPE( ITYPE, NDF, 'Variance', STATUS )

*  Fill the array components of the NDF.
*  =====================================

*  Map the data and error component, initialising to the bad value.
*  Any pixels not filled from the binary table are undefined, and thus
*  should have the bad value.
      CALL NDF_MAP( NDF, 'Data,Error', '_REAL', 'WRITE/BAD', PNTR, EL,
     :              STATUS )

*  Map the QUALITY component.  This assumes that the detector number
*  does not exceed 255, although the data type is nominally J
*  (_INTEGER) in the binary table.
      CALL NDF_MAP( NDF, 'Quality', '_UBYTE', 'WRITE', QPNTR, EL,
     :              STATUS )

*  Obtain the workspace for the reduced list of fluxes and errors, the
*  detector identification, and the scan direction.
      CALL PSX_CALLOC( NOBS, '_REAL', FPNTR, STATUS )
      CALL PSX_CALLOC( NOBS, '_REAL', FEPNTR, STATUS )
      CALL PSX_CALLOC( NOBS, '_INTEGER', DPNTR, STATUS )
      CALL PSX_CALLOC( NOBS, '_INTEGER', SDPNTR, STATUS )

*  Form the weighted mean flux and errors, and the quality.
      CALL COF_LWS2( FUNIT, NDF, LBND( 1 ), UBND( 1 ), LBND( 2 ),
     :               UBND( 2 ), NWAVEL, LBND( 4 ), UBND( 4 ), NOBS,
     :               NFIELD, %VAL( CNF_PVAL( XPNTR ) ),
     :               %VAL( CNF_PVAL( YPNTR ) ),
     :               %VAL( CNF_PVAL( WIPNTR ) ),
     :               %VAL( CNF_PVAL( SPNTR ) ),
     :               %VAL( CNF_PVAL( PNTR(1) ) ),
     :               %VAL( CNF_PVAL( PNTR(2) ) ),
     :               %VAL( CNF_PVAL( QPNTR(1) ) ), USED,
     :               %VAL( CNF_PVAL( FPNTR ) ),
     :               %VAL( CNF_PVAL( FEPNTR ) ),
     :               %VAL( CNF_PVAL( DPNTR ) ),
     :               %VAL( CNF_PVAL( SDPNTR ) ),
     :               STATUS )

*  Free the workspace.
      CALL PSX_FREE( FPNTR, STATUS )
      CALL PSX_FREE( FEPNTR, STATUS )
      CALL PSX_FREE( DPNTR, STATUS )
      CALL PSX_FREE( SDPNTR, STATUS )
      CALL PSX_FREE( WIPNTR, STATUS )
      CALL PSX_FREE( XPNTR, STATUS )
      CALL PSX_FREE( YPNTR, STATUS )
      CALL PSX_FREE( SPNTR, STATUS )

*  Set the bad-pixel flags.
      CALL NDF_SBAD( .TRUE., NDF, 'Data', STATUS )
      CALL NDF_SBAD( .TRUE., NDF, 'Variance', STATUS )

*  Unmap the NDF arrays.
      CALL NDF_UNMAP( NDF, '*', STATUS )

*  Exclude unwanted columns.
*  =========================

*  Find the column number of the unwanted ancillary data.
      CALL FTGCNO( FUNIT, .FALSE., 'LSANFILL', COLNUM, FSTAT )

*  Record that this column is used.
      USED( COLNUM ) = .TRUE.

*  Create an extension for the remaining columns.
*  ==============================================

*  Here it is assumed that there are still columns left.
      CALL NDF_XNEW( NDF, 'LWSAN', 'LWSAN_EXT', 0, 0, XLOC, STATUS )

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
               CALL DAT_NEW( XLOC, COLNAM, CTYPE, 1, NOBS, STATUS )
               CALL DAT_FIND( XLOC, COLNAM, CLOC, STATUS )

*  Map the component for writing.
               CALL DAT_MAP( CLOC, CTYPE, 'WRITE', 1, NOBS, EXPNTR,
     :                       STATUS )

*  Read the column into the data array.  Call the appropriate routine
*  for the chosen type.
               IF ( CTYPE .EQ. '_UBYTE' ) THEN
                  CALL FTGCVB( FUNIT, COLNUM, 1, 1, NOBS, VAL__BADUB,
     :                         %VAL( CNF_PVAL( EXPNTR ) ), BAD, FSTAT )

               ELSE IF ( CTYPE .EQ. '_WORD' ) THEN
                  CALL FTGCVI( FUNIT, COLNUM, 1, 1, NOBS, VAL__BADW,
     :                         %VAL( CNF_PVAL( EXPNTR ) ), BAD, FSTAT )

               ELSE IF ( CTYPE .EQ. '_INTEGER' ) THEN
                  CALL FTGCVJ( FUNIT, COLNUM, 1, 1, NOBS, VAL__BADI,
     :                         %VAL( CNF_PVAL( EXPNTR ) ), BAD, FSTAT )

               ELSE IF ( CTYPE .EQ. '_REAL' ) THEN
                  CALL FTGCVE( FUNIT, COLNUM, 1, 1, NOBS, VAL__BADR,
     :                         %VAL( CNF_PVAL( EXPNTR ) ), BAD, FSTAT )

               ELSE IF ( CTYPE .EQ. '_DOUBLE' ) THEN
                  CALL FTGCVD( FUNIT, COLNUM, 1, 1, NOBS, VAL__BADD,
     :                         %VAL( CNF_PVAL( EXPNTR ) ), BAD, FSTAT )

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
