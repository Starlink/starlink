      SUBROUTINE COF_IUEMX( FUNIT, FILE, NDF, PROFIT, LOGHDR, FDL,
     :                      STATUS )
*+
*  Name:
*     COF_IUEMX

*  Purpose:
*     Converts an IUE MXLO binary table into an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_IUEMX( FUNIT, FILE, NDF, PROFIT, LOGHDR, FDL, STATUS )

*  Description:
*     This routine converts an IUE MXLO product stored in a FITS binary
*     table into a series of one-dimensional NDFs stored in a single
*     container file corresponding to the supplied NDF identifier.
*     Each NDF contains data, variance and quality arrays, and axes.
*     Only the most-significant eight bits of the quality flags are
*     transferred to the NDF.  Additional columns in the binary table
*     are written as components of an IUE_MX extension.  The primary
*     HDU headers may be written to the standard FITS airlock
*     extension.  See the "Notes" for further details.

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
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The names of multiple NDFs within the HDS container file are
*     LARGE or SMALL depending on the size of the aperture used for the
*     observation.
*     -  The conversion from binary-table columns and headers to NDF
*     objects is as follows:
*
*          NPOINTS                Number of elements
*          WAVELENGTH             Start wavelength, label, and units
*          DELTAW                 Incremental wavelength
*          FLUX                   Data array, label, and units
*          SIGMA                  Data-error array
*          QUALITY                Quality array
*          remaining columns      Component in IUE_MX extension (NET and
*                                 BACKGROUND are NDFs)
*
*     [optional_subroutine_items]...

*  Prior Requirements:
*     -  The FITS file should already have been opened by FITSIO, and
*     is in the HDU immediately prior to the BINTABLE extension that is
*     going to define the NDF.
*     [routine_prior_requirements]...

*  Copyright:
*     Copyright (C) 1996, 1998, 2004 Central Laboratory of the Research
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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1996 June 30 (MJC):
*        Original version.
*     22-JAN-1998 (DSB):
*        - Changed the scheme for naming multiple NDFs within the
*        container file; previously the row number was used, but NDF
*        names cannot start with numeric characters and an error was
*        reported by NDF_OPEN.  Therefore the word "ROW" has been
*        prepended to the row number.
*        - Changed the 4th argument of every call of FTGCVx from IOBS to
*        1.
*     1998 January 22 (MJC):
*        Corrected an COF_FIOER error report to use a buffer and not an
*        MSG token; this makes it consistent with the other error
*        reports.  Created axis centres for extension NDFs.  Added some
*        defensive programming in case the MXLO format changes.
*     1998 January 26 (MJC):
*        Name the multiple NDFs LARGE or SMALL (from the size of the
*        aperture used) rather than ROW1 and ROW2.  Do not delete the
*        data array in the container file; it is needed to make the
*        file a valid NDF for the global FITS airlock.  Correct the
*        zero null value to the appropriate type for the floating-point
*        invocations of FTGCVx, while obtaining the wavelength axis
*        information.
*     10-AUG-1998 (DSB):
*        Corrected interpretation of NPOINTS column value from "no. of
*        spectra" to "no. of points in current spectrum".
*     1998 August 11 (MJC):
*        Make "no. of spectra" no more than two.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2008 March 15 (MJC):
*        Use KAPLIBS routines instead of their cloned CON equivalents.
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

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a string less trailing
                                 ! blanks

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

      INTEGER MXCOLS             ! Maximum number of columns
      PARAMETER( MXCOLS = 9 )

*  Local Variables:
      LOGICAL ARREXT             ! Column in extension is an array?
      CHARACTER * ( 5 ) APERT    ! Aperture size
      CHARACTER * ( NDF__SZTYP ) ATYPE ! Processing type for axis
      LOGICAL BAD                ! Column array contains bad values?
      CHARACTER * ( 10 ) BTYPE ! Column TFORM data type
      CHARACTER * ( 200 ) BUFFER ! Buffer for forming error messages
      CHARACTER * ( DAT__SZLOC ) CLOC ! Locator to extension component
      CHARACTER * ( DAT__SZNAM ) COLNAM ! Column name
      CHARACTER * ( 48 ) COMENT  ! Keyword comment
      CHARACTER * ( 5 ) CVAL     ! Character value
      INTEGER COLNUM             ! Column number
      CHARACTER * ( 9 ) CON      ! Observation number
      CHARACTER * ( DAT__SZTYP ) CTYPE ! Column HDS data type
      INTEGER DATCOD             ! FITSIO data code
      DOUBLE PRECISION DSTARW    ! Start wavelength
      DOUBLE PRECISION DSTEPW    ! Wavelength step
      DOUBLE PRECISION DVAL      ! Scalar column value
      INTEGER EL                 ! Number of pixels in the spectrum
      INTEGER FSTAT              ! FITSIO status
      INTEGER HDUTYP             ! HDU type (primary, IMAGE, ASCII or
                                 ! binary table)
      INTEGER I                  ! Loop counter
      INTEGER IOBS               ! Observations loop counter
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Processing and actual type for
                                 ! a column array
      INTEGER IVAL               ! Scalar column value
      CHARACTER * ( 8 ) KEYWRD   ! FITS header keyword
      INTEGER LBND               ! Lower bound of the array (=1)
      LOGICAL MULTIP             ! Multiple observations?
      INTEGER NC                 ! Number of characters in observation
                                 ! number
      INTEGER NCF                ! Number of characters in filename
      INTEGER NCU                ! Used length of string
      INTEGER NDFE               ! Identifier of effective NDF
      INTEGER NDFO               ! Identifier for output NDF
      INTEGER NFIELD             ! Number of fields in table
      CHARACTER * ( DAT__SZLOC ) NLOC ! Locator to the dummy NDF
      INTEGER NOBS               ! Number of observations in table
      INTEGER NREP               ! Number of bad error values replaced
      INTEGER NREPHI             ! Dummy
      INTEGER PLACE              ! Placeholder for new NDF
      INTEGER PNTR( 1 )          ! Pointer to a mapped column array
      INTEGER REPEAT             ! Column repeat count
      REAL RVAL                  ! Scalar column value
      REAL STARTW                ! Start wavelength
      REAL STEPW                 ! Wavelength step
      LOGICAL THERE              ! Header keyword is present?
      CHARACTER * ( 72 ) UNITS   ! Units of the data or axis array
      INTEGER UBND               ! Upper bound of the array (=dim)
      BYTE UVAL                  ! Scalar column value
      LOGICAL USED( MXCOLS )     ! Table column used?
      INTEGER WIDTH              ! Column width
      INTEGER WPNTR              ! Pointer to work array (for quality)
      INTEGER * 2 WVAL           ! Scalar column value
      INTEGER * 2 WVALUE         ! A word value
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator to the IUE_MX extension

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Read the main header into the FITS extension.  There are no group
*  parameters.
      IF ( PROFIT ) CALL COF_WFEXT( FUNIT, NDF, 0, 0, FILE, STATUS )

*  Write out the headers to a logfile.
      IF ( LOGHDR ) CALL COF_HDLOG( FUNIT, FDL, FILE, 1, STATUS )

*  Obtain the length of the filename.
      NCF = CHR_LEN( FILE )

*  Skip to the next HDU.  This is defined to be a BINTABLE for the
*  IUE MX products.
      CALL FTMRHD( FUNIT, 1, HDUTYP, FSTAT )
      IF ( FSTAT .NE. FITSOK ) THEN
         BUFFER = 'Error skipping to the extension of the IUE MX FITS '/
     :            /'file '//FILE( :NCF )//'.'
         CALL COF_FIOER( FSTAT, 'COF_IUEMX_WREXT', 'FTMRHD', BUFFER,
     :                   STATUS )

      ELSE IF ( HDUTYP .NE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FILE', FILE )
         CALL ERR_REP( 'COF_IUEMX_NOBINTAB',
     :     'The first extension of ^FILE is not a BINTABLE.', STATUS )
      END IF

*  Find the shape of the binary table.
*  ===================================

*  Obtain the number of spectra.   There should be one or two spectra
*  present.
      CALL COF_GKEYI( FUNIT, 'NAXIS2', THERE, NOBS, COMENT, STATUS )
      NOBS = MAX( MIN( 2, NOBS ), 1 )

*  Obtain the number of fields in the table.
      CALL COF_GKEYI( FUNIT, 'TFIELDS', THERE, NFIELD, COMENT, STATUS )

*  Define the structure of the HDS container file.
*  ===============================================

*  In general the number of observations is not going to be one, so
*  a series of NDFs has to be made in the container file.  When the
*  number is one, then a normal NDF can be created.
      MULTIP = NOBS .GT. 1

      IF ( MULTIP ) THEN

*  Get a locator for the dummy NDF already created.
         CALL NDF_LOC( NDF, 'WRITE', NLOC, STATUS )

      END IF

*  Set the number of fields to be no more than the maximum.  This should
*  never be exceeded, and is here defensively in case the format is
*  changed.
      NFIELD = MIN( NFIELD, MXCOLS )

*  By definition the lower bound is always 1.
      LBND = 1

*  Loop for each of the observations.
*  ==================================
      DO IOBS = 1, NOBS

*  Initialise flags to indicate that none of the binary-table columns
*  has been used.
         DO I = 1, NFIELD
            USED( I ) = .FALSE.
         END DO

*  Convert the observation number to a string.  It is needed for error
*  messages.
         CALL CHR_ITOC( IOBS, CON, NC )

*  Read the columns defining the array's shape.
*  ============================================

*  Find the column number of the number of points in the spectrum.
         CALL FTGCNO( FUNIT, .FALSE., 'NPOINTS', COLNUM, FSTAT )

*  Read the value for the current observation.  Note that there
*  are no bad values.  Record the column as being used.
         CALL FTGCVJ( FUNIT, COLNUM, IOBS, 1, 1, 0, UBND, BAD,
     :                FSTAT )
         USED( COLNUM ) = .TRUE.

*  Read the columns defining the array's shape.
*  ============================================

*  Find the column number of the aperture size.
         IF ( MULTIP ) THEN
            CALL FTGCNO( FUNIT, .FALSE., 'APERTURE', COLNUM, FSTAT )

*  Read the value for the current observation.  Note that there
*  are no bad values.  Do not record the column as being used.  This
*  retains the aperture size in the extension even if the user renames
*  or copies the NDF.
            CALL FTGCVS( FUNIT, COLNUM, IOBS, 1, 1, ' ', APERT, BAD,
     :                   FSTAT )
         END IF

*  Inquire where the data array is stored and its data type.
*  =========================================================

*  Find the column number of the calibrated flux values.
         CALL FTGCNO( FUNIT, .FALSE., 'FLUX', COLNUM, FSTAT )

*  Record that this column is used.
         USED( COLNUM ) = .TRUE.

*  Find the data code (effectively the data type) for the column.  Use
*  it to define the implementation type.
         CALL FTGTCL( FUNIT, COLNUM, DATCOD, REPEAT, WIDTH, FSTAT )
         CALL COF_FD2HT( DATCOD, ITYPE, STATUS )

*  Create a NDF for a multiple-image dataset.
*  ==========================================
         IF ( MULTIP ) THEN

*  Create new NDF placeholder.  Use the aperture size as the name of
*  the NDF.
            CALL NDF_PLACE( NLOC, APERT, PLACE, STATUS )

*  Create the new NDF using the placeholder.  It is one-dimensional by
*  definition.
            CALL NDF_NEW( ITYPE, 1, LBND, UBND, PLACE, NDFO, STATUS )

*  Use the existing NDF.
         ELSE

*  Clone the input NDF identifier.
            CALL NDF_CLONE( NDF, NDFO, STATUS )

*  Set the type of the NDF.
            CALL NDF_STYPE( ITYPE, NDFO, 'Data', STATUS )

*  Set the bounds of the NDF.  It is one-dimensional by definition.
            CALL NDF_SBND( 1, LBND, UBND, NDFO, STATUS )
         END IF

         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the flux and store as the data array.
*  ============================================

*  Map the NDF component.
         CALL NDF_MAP( NDFO, 'Data', ITYPE, 'WRITE', PNTR, EL, STATUS )

*  Read the column into the data array.  Call the appropriate routine
*  for the chosen type.  This should be _REAL.
         IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL FTGCVJ( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADI,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL FTGCVE( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADR,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL FTGCVD( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADD,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'IT', ITYPE )
            CALL MSG_SETI( 'ON', IOBS )
            CALL ERR_REP( 'COF_IUEMX_ITYPE',
     :        'Invalid data type (^IT) selected for the IUE MXLO flux '/
     :        /'array in observation ^ON.', STATUS )
            GOTO 999
         END IF

*  Set the bad-pixel flag.
         CALL NDF_SBAD( BAD, NDFO, 'Data', STATUS )

*  Unmap the data array.
         CALL NDF_UNMAP( NDFO, 'Data', STATUS )

*  Check that the transfer was correct.
         IF ( FSTAT .NE. FITSOK ) THEN
            BUFFER = 'Error copying the IUE MXLO flux to the NDF '/
     :               /'data array in observation '//CON( :NC )//'.'
            CALL COF_FIOER( FSTAT, 'COF_IUEMX_CRDAT', 'FTGCVx',
     :                      BUFFER, STATUS )
            GOTO 999
         END IF

*  Obtain the units for this column, and place it into the NDF.  To do
*  this first form the names of the units keyword for this column.
*  Note that NDF_CPUT does not truncate trailing blanks.
         CALL FTKEYN( 'TUNIT', COLNUM, KEYWRD, FSTAT )
         CALL COF_GKEYC( FUNIT, KEYWRD, THERE, UNITS, COMENT, STATUS )
         IF ( THERE ) THEN
            NCU = CHR_LEN( UNITS )
            CALL NDF_CPUT( UNITS( :NCU ), NDFO, 'Units', STATUS )
         END IF

*  Set the label for the NDF.
         CALL NDF_CPUT( 'Flux', NDFO, 'Label', STATUS )

         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the standard deviation and store as the error array.
*  ===========================================================

*  Find the column number of the errors.
         CALL FTGCNO( FUNIT, .FALSE., 'SIGMA', COLNUM, FSTAT )

*  Find the data code (effectively the data type) for the column.  Use
*  it to define the implementation type.
         CALL FTGTCL( FUNIT, COLNUM, DATCOD, REPEAT, WIDTH, FSTAT )
         CALL COF_STYPC( NDFO, 'Variance', ' ', DATCOD, ITYPE, STATUS )

*  Map some workspace.
         CALL PSX_CALLOC( UBND, ITYPE, WPNTR, STATUS )

*  Map the NDF component.
         CALL NDF_MAP( NDFO, 'Error', ITYPE, 'WRITE', PNTR, EL, STATUS )

*  Read the column into the work array, and then replace any negative
*  values (the official -1s meaning undefined error and any other
*  garbage) with the bad value.  Call the appropriate routine for the
*  chosen type.
         IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL FTGCVJ( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADI,
     :                   %VAL( CNF_PVAL( WPNTR ) ), BAD, FSTAT )

            CALL KPG1_THRSI( .TRUE., EL, %VAL( CNF_PVAL( WPNTR ) ),
     :                       0, VAL__MAXI,
     :                       VAL__BADI, VAL__MAXI,
     :                       %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                       NREP, NREPHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL FTGCVE( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADR,
     :                   %VAL( CNF_PVAL( WPNTR ) ), BAD, FSTAT )

            CALL KPG1_THRSR( .TRUE., EL, %VAL( CNF_PVAL( WPNTR ) ),
     :                       0.0, VAL__MAXR,
     :                       VAL__BADR, VAL__MAXR,
     :                       %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                       NREP, NREPHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL FTGCVD( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADD,
     :                   %VAL( CNF_PVAL( WPNTR ) ), BAD, FSTAT )

            CALL KPG1_THRSD( .TRUE., EL, %VAL( CNF_PVAL( WPNTR ) ),
     :                       0.0D0, VAL__MAXD,
     :                       VAL__BADD, VAL__MAXD,
     :                       %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                       NREP, NREPHI, STATUS )

         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'IT', ITYPE )
            CALL MSG_SETI( 'ON', IOBS )
            CALL ERR_REP( 'COF_IUEMX_ITYPE',
     :        'Invalid data type (^IT) selected for the IUE MXLO '/
     :        /'error array in observation ^ON.', STATUS )
            GOTO 999
         END IF

*  Set the bad-pixel flag.
         BAD = BAD .OR. NREP .GT. 0
         CALL NDF_SBAD( BAD, NDFO, 'Variance', STATUS )

*  Unmap the error array.  Note this call is assymetric with NDF_MAP,
*  in that the error array can be mapped, but not unmapped; we have
*  to specify the variance instead.
         CALL NDF_UNMAP( NDFO, 'Variance', STATUS )

*  Free the workspace.
         CALL PSX_FREE( WPNTR, STATUS )

*  Check that the transfer was correct.
         IF ( FSTAT .NE. FITSOK ) THEN
            CALL COF_FIOER( FSTAT, 'COF_IUEMX_CRVAR', 'FTGCVx',
     :        'Error copying the IUE MXLO standard deviation to the '/
     :        /'NDF error array.', STATUS )
            GOTO 999
         END IF

*  Record that this column is used.
         USED( COLNUM ) = .TRUE.
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the quality column store in the quality array.
*  =====================================================

*  Find the column number of the detector numbers.
         CALL FTGCNO( FUNIT, .FALSE., 'QUALITY', COLNUM, FSTAT )

*  Get some workspace for the 16-bit 2's complement quality data.
         CALL PSX_MALLOC( UBND * VAL__NBW, WPNTR, STATUS )

*  Read the column into the work array.  Note that there are no bad
*  values.
         WVALUE = 0
         CALL FTGCVI( FUNIT, COLNUM, IOBS, 1, UBND, WVALUE,
     :                %VAL( CNF_PVAL( WPNTR ) ), BAD, FSTAT )

*  Map the input array component with the desired data type.  Any type
*  conversion will be performed by the FITSIO array-reading routine.
         CALL NDF_MAP( NDFO, 'Quality', '_UBYTE', 'WRITE', PNTR, EL,
     :                 STATUS )

*  Transfer the most-significant IUE quality flags across to the NDF's
*  QUALITY component.
         CALL COF_IUEQ( EL, %VAL( CNF_PVAL( WPNTR ) ),
     :                  %VAL( CNF_PVAL( PNTR( 1 ) ) ), STATUS )

*  Tidy up the workspace and quality.
         CALL PSX_FREE( WPNTR, STATUS )
         CALL NDF_UNMAP( NDFO, 'Quality', STATUS )

*  Check that the transfer was correct.
         IF ( FSTAT .NE. FITSOK ) THEN
            BUFFER = 'Error copying the IUE MXLO quality to the NDF '/
     :                /'quality array in observation '//CON( :NC )//'.'
            CALL COF_FIOER( FSTAT, 'COF_IUEMX_CRQUA', 'FTGCVB',
     :                      BUFFER, STATUS )
            GOTO 999
         END IF

*  Record that this column is used.
         USED( COLNUM ) = .TRUE.

         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the start wavelength and axis units.
*  ===========================================

*  Find the column number of the start wavelength.
         CALL FTGCNO( FUNIT, .FALSE., 'WAVELENGTH', COLNUM, FSTAT )

*  Find the data code (effectively the data type) for the column.
*  Use it along with the preferred data type to define the component's
*  type and the implementation type.
         CALL FTGTCL( FUNIT, COLNUM, DATCOD, REPEAT, WIDTH, FSTAT )
         CALL COF_ATYPC( NDFO, 'Centre', 1, ' ', DATCOD, ITYPE, STATUS )

*  Obtain the start wavelength from the column.  Call the appropriate
*  routine for the chosen type.  This should be _REAL.  Store d.p.
*  values for real data too, so there is always a matching pair of
*  start and increment.
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL FTGCVE( FUNIT, COLNUM, IOBS, 1, 1, 0.0, STARTW,
     :                   BAD, FSTAT )
            DSTARW = DBLE( STARTW )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL FTGCVD( FUNIT, COLNUM, IOBS, 1, 1, 0.0D0, DSTARW,
     :                   BAD, FSTAT )

         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'IT', ITYPE )
            CALL MSG_SETI( 'ON', IOBS )
            CALL ERR_REP( 'COF_IUEMX_ITYPESW',
     :        'Invalid data type (^IT) selected for the IUE MXLO '/
     :        /'start wavelength in observation ^ON.', STATUS )
            GOTO 999
         END IF

*  Obtain the units for this column, and place it into the NDF.  To do
*  this first form the names of the units keyword for this column.
*  Note that NDF_ACPUT does not truncate trailing blanks.
         CALL FTKEYN( 'TUNIT', COLNUM, KEYWRD, FSTAT )
         CALL COF_GKEYC( FUNIT, KEYWRD, THERE, UNITS, COMENT, STATUS )
         IF ( THERE ) THEN
            NCU = CHR_LEN( UNITS )
            CALL NDF_ACPUT( UNITS( :NCU ), NDFO, 'Units', 1, STATUS )
         END IF

*  Set the label for the axis.
         CALL NDF_ACPUT( 'Wavelength', NDFO, 'Label', 1, STATUS )

*  Record that this column is used and the provision axis-centre data
*  type.
         USED( COLNUM ) = .TRUE.
         ATYPE = ITYPE

*  Obtain the incremental wavelength.
*  ==================================

*  Find the column number of the start wavelength.
         CALL FTGCNO( FUNIT, .FALSE., 'DELTAW', COLNUM, FSTAT )

*  Obtain the wavelength step.  Read the column into the data array.
*  Call the appropriate routine for the chosen type (assume the same
*  data type as the start wavelength).  This should be _REAL.  Store
*  d.p. values for real data too, so there is always a matching pair.
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL FTGCVE( FUNIT, COLNUM, IOBS, 1, 1, 0.0, STEPW,
     :                   BAD, FSTAT )
            DSTEPW = DBLE( STEPW )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL FTGCVD( FUNIT, COLNUM, IOBS, 1, 1, 0.0D0, DSTEPW,
     :                   BAD, FSTAT )

         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'IT', ITYPE )
            CALL MSG_SETI( 'ON', IOBS )
            CALL ERR_REP( 'COF_IUEMX_ITYPEIW',
     :        'Invalid data type (^IT) selected for the IUE MXLO '/
     :        /'step wavelength in observation ^ON.', STATUS )
            GOTO 999
         END IF

*  Record that this column is used and the axis-centre data type.  Use
*  double-precision if either the start or increment is double
*  precision.
         USED( COLNUM ) = .TRUE.
         IF ( ITYPE .EQ. '_DOUBLE' ) ATYPE = ITYPE

*  Create the axis centres.
*  ========================

*  Map the NDF component.
         CALL NDF_AMAP( NDFO, 'Centre', 1, ATYPE, 'WRITE', PNTR, EL,
     :                  STATUS )

*  Created the spaced array using the appropriate data type.
         IF ( ATYPE .EQ. '_REAL' ) THEN
            CALL KPG1_SSAZR( UBND, DBLE( STEPW ), DBLE( STARTW ),
     :                       %VAL( CNF_PVAL( PNTR( 1 ) ) ), STATUS )

         ELSE IF ( ATYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_SSAZD( UBND, DSTEPW, DSTARW,
     :                       %VAL( CNF_PVAL( PNTR( 1 ) ) ), STATUS )

         END IF

*  Unmap the data array.
         CALL NDF_AUNMP( NDFO, 'Centre', 1, STATUS )

         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Create an extension for the remaining columns.
*  ==============================================

*  Here it is assumed that there are still columns left.
         CALL NDF_XNEW( NDFO, 'IUE_MX', 'IUEMX_EXT', 0, 0, XLOC,
     :                  STATUS )

*  Loop through all the fields, copying those not already used and
*  wanted.
         DO COLNUM = 1, NFIELD
            IF ( .NOT. USED( COLNUM ) ) THEN

*  Obtain the name of the column.  First form the name of the keyword
*  for this column.
               CALL FTKEYN( 'TTYPE', COLNUM, KEYWRD, FSTAT )
               CALL COF_GKEYC( FUNIT, KEYWRD, THERE, COLNAM, COMENT,
     :                         STATUS )
               IF ( THERE ) THEN

*  Find the length and repeat count for the column.  Initialise the
*  width as the FITSIO routine appears not to do this.
                  WIDTH = 0
                  CALL FTGTCL( FUNIT, COLNUM, DATCOD, REPEAT, WIDTH,
     :                         FSTAT )

*  Obtain its data type.
                  CALL FTKEYN( 'TFORM', COLNUM, KEYWRD, FSTAT )
                  CALL COF_GKEYC( FUNIT, KEYWRD, THERE, BTYPE, COMENT,
     :                            STATUS )

*  Convert the binary table type into an HDS type.  Note we assume
*  that there are no strings.
                  IF ( THERE ) THEN
                     CALL COF_BN2HT( BTYPE, CTYPE, STATUS )
                  ELSE
                     CTYPE = '_REAL'
                  END IF

*  Create NDFs for arrays to make them easier to examine and process.
                  ARREXT = REPEAT .GT. 1 .AND. CTYPE( 1:5 ) .NE. '_CHAR'
                  IF ( ARREXT ) THEN

*  Create a new NDF in the extension via an NDF placeholder.  The data
*  type and bounds will be changed below once they are known.
                     CALL NDF_PLACE( XLOC, COLNAM, PLACE, STATUS )
                     CALL NDF_NEW( CTYPE, 1, 1, REPEAT, PLACE, NDFE,
     :                             STATUS )
                  ELSE

*  Create the component of the extension, and get a locator to the
*  component.
                     CALL DAT_NEW( XLOC, COLNAM, CTYPE, 0, 0, STATUS )
                     CALL DAT_FIND( XLOC, COLNAM, CLOC, STATUS )
                  END IF

*  Create the array (NDF) extension components.
*  --------------------------------------------

*  Map the array component for writing.
                  IF ( ARREXT ) THEN
                     CALL NDF_MAP( NDFE, 'Data', CTYPE, 'WRITE', PNTR,
     :                             EL, STATUS )

*  Read the column into the array.  Call the appropriate routine
*  for the chosen type even though only expect real arrays spare, but
*  let's be defensive in case there are revisions.
                     IF ( CTYPE .EQ. '_UBYTE' ) THEN
                        CALL FTGCVB( FUNIT, COLNUM, IOBS, 1, EL,
     :                               VAL__BADUB,
     :                               %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD,
     :                               FSTAT )

                     ELSE IF ( CTYPE .EQ. '_WORD' ) THEN
                        CALL FTGCVI( FUNIT, COLNUM, IOBS, 1, EL,
     :                               VAL__BADW,
     :                               %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD,
     :                               FSTAT )

                     ELSE IF ( CTYPE .EQ. '_INTEGER' ) THEN
                        CALL FTGCVJ( FUNIT, COLNUM, IOBS, 1, EL,
     :                               VAL__BADI,
     :                               %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD,
     :                               FSTAT )

                     ELSE IF ( CTYPE .EQ. '_REAL' ) THEN
                        CALL FTGCVE( FUNIT, COLNUM, IOBS, 1, EL,
     :                               VAL__BADR,
     :                               %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD,
     :                               FSTAT )

                     ELSE IF ( CTYPE .EQ. '_DOUBLE' ) THEN
                        CALL FTGCVD( FUNIT, COLNUM, IOBS, 1, EL,
     :                               VAL__BADD,
     :                               %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD,
     :                               FSTAT )

                     END IF

*  Unmap the NDF's data array.
                     CALL NDF_UNMAP( NDFE, 'Data', STATUS )

*  Obtain the units for this column, and place it into the NDF.  To do
*  this first form the names of the units keyword for this column.
                     CALL FTKEYN( 'TUNIT', COLNUM, KEYWRD, FSTAT )
                     CALL COF_GKEYC( FUNIT, KEYWRD, THERE, UNITS,
     :                               COMENT, STATUS )
                     IF ( THERE ) THEN
                        NCU = CHR_LEN( UNITS )
                        CALL NDF_CPUT( UNITS( :NCU ), NDFE, 'Units',
     :                                 STATUS )
                     END IF

*  Set the label for the NDF.
                     CALL NDF_CPUT( 'Flux', NDFE, 'Label', STATUS )

*  Create the axis centres.
*  ------------------------

*  Only do it for arrays corresponding to the spectrum.  At present that
*  will be both the remaining array columns, but perhaps the format will
*  change, so let's be on the safe side.
                     IF ( REPEAT .EQ. 640 ) THEN

*  Map the NDF component.
                        CALL NDF_AMAP( NDFE, 'Centre', 1, ATYPE,
     :                                 'WRITE', PNTR, EL, STATUS )

*  Created the spaced array using the appropriate data type.
                        IF ( ATYPE .EQ. '_REAL' ) THEN
                           CALL KPG1_SSAZR( EL, DBLE( STEPW ),
     :                                      DBLE( STARTW ),
     :                                %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                                      STATUS )

                        ELSE IF ( ATYPE .EQ. '_DOUBLE' ) THEN
                           CALL KPG1_SSAZD( EL, DSTEPW, DSTARW,
     :                                %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                                      STATUS )

                        END IF

*  Unmap the axis-centre array.
                        CALL NDF_AUNMP( NDFE, 'Centre', 1, STATUS )

*  Propagate the axis units for the main to the extension NDF.
                        CALL NDF_ACGET( NDFO, 'Units', 1, UNITS,
     :                                  STATUS )
                        IF ( UNITS .NE. ' ' ) THEN
                           NCU = CHR_LEN( UNITS )
                           CALL NDF_ACPUT( UNITS( :NCU ), NDFE, 'Units',
     :                                     1, STATUS )
                        END IF

*  Set the label for the axis.
                        CALL NDF_ACPUT( 'Wavelength', NDFE, 'Label',
     :                                  1, STATUS )

                        IF ( STATUS .NE. SAI__OK ) GOTO 999
                     END IF

*  Tidy the extension NDF.
                     CALL NDF_ANNUL( NDFE, STATUS )

*  Scalar components
*  -----------------
*
*  Extract the scalar value and transfer it to the HDS component.  Only
*  expect character scalar value spare, but let's be defensive in case
*  there are revisions.
                  ELSE
                     IF ( CTYPE( 1:5 ) .EQ. '_CHAR' ) THEN
                        CALL FTGCVS( FUNIT, COLNUM, IOBS, 1, 1,
     :                               ' ', CVAL, BAD, FSTAT )
                        CALL DAT_PUT( CLOC, CTYPE, 0, 0, CVAL, STATUS )

                     ELSE IF ( CTYPE .EQ. '_UBYTE' ) THEN
                        CALL FTGCVB( FUNIT, COLNUM, IOBS, 1, 1,
     :                               VAL__BADUB, UVAL, BAD, FSTAT )
                        CALL DAT_PUT( CLOC, CTYPE, 0, 0, UVAL, STATUS )

                     ELSE IF ( CTYPE .EQ. '_WORD' ) THEN
                        CALL FTGCVI( FUNIT, COLNUM, IOBS, 1, 1,
     :                               VAL__BADW, WVAL, BAD, FSTAT )
                        CALL DAT_PUT( CLOC, CTYPE, 0, 0, WVAL, STATUS )

                     ELSE IF ( CTYPE .EQ. '_INTEGER' ) THEN
                        CALL FTGCVJ( FUNIT, COLNUM, IOBS, 1, 1,
     :                               VAL__BADI, IVAL, BAD, FSTAT )
                        CALL DAT_PUT( CLOC, CTYPE, 0, 0, IVAL, STATUS )

                     ELSE IF ( CTYPE .EQ. '_REAL' ) THEN
                        CALL FTGCVE( FUNIT, COLNUM, IOBS, 1, 1,
     :                               VAL__BADR, RVAL, BAD, FSTAT )
                        CALL DAT_PUT( CLOC, CTYPE, 0, 0, RVAL, STATUS )

                     ELSE IF ( CTYPE .EQ. '_DOUBLE' ) THEN
                        CALL FTGCVD( FUNIT, COLNUM, IOBS, 1, 1,
     :                               VAL__BADD, DVAL, BAD, FSTAT )
                        CALL DAT_PUT( CLOC, CTYPE, 0, 0, DVAL, STATUS )

                     END IF

*  Tidy the locator to the component.
                     CALL DAT_ANNUL( CLOC, STATUS )
                  END IF

               END IF
            END IF
         END DO

*  Tidy the locator to the extension.
         CALL DAT_ANNUL( XLOC, STATUS )

*  Tidy the NDF identifier.
         CALL NDF_ANNUL( NDFO, STATUS )
      END DO

  999 CONTINUE

      END
