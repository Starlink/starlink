      SUBROUTINE COF_INES( FUNIT, FILE, NDF, PROFIT, LOGHDR, FDL,
     :                     STATUS )
*+
*  Name:
*     COF_INES

*  Purpose:
*     Converts an IUE INES binary table spectrum into an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_INES( FUNIT, FILE, NDF, PROFIT, LOGHDR, FDL, STATUS )

*  Description:
*     This routine converts an IUE spectrum from the INES archive
*     stored in a FITS binary table into a 1-dimensional NDF.
*     The NDF contains data, variance and quality arrays, and axes.
*     Only the most-significant 8 bits of the quality flags are
*     transferred to the NDF.  The primary HDU headers may be
*     written to the standard FITS airlock extension.  See the
*     "Notes" for further details.

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
*     -  The conversion from binary-table columns and headers to NDF
*     objects is as follows:
*
*          WAVELENGTH             Start wavelength, label, and units
*          FLUX                   Data array, label, and units
*          SIGMA                  Data-error array
*          QUALITY                Quality array
*     -  This assumes that the axis centres are monotonic.
*
*     [optional_subroutine_items]...

*  Prior Requirements:
*     -  The FITS file should already have been opened by FITSIO, and
*     is in the HDU immediately prior to the BINTABLE extension that is
*     going to define the NDF.
*     [routine_prior_requirements]...

*  Copyright:
*     Copyright (C) 2003-2004 Central Laboratory of the Research
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
*     2003 May 2 (MJC):
*        Original version derived from COF_IUEMX.
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
      PARAMETER( MXCOLS = 4 )

*  Local Variables:
      LOGICAL BAD                ! Column array contains bad values?
      CHARACTER * ( 200 ) BUFFER ! Buffer for forming error messages
      CHARACTER * ( 48 ) COMENT  ! Keyword comment
      INTEGER COLNUM             ! Column number
      CHARACTER * ( 9 ) CON      ! Observation number
      INTEGER DATCOD             ! FITSIO data code
      INTEGER EL                 ! Number of pixels in the spectrum
      INTEGER FSTAT              ! FITSIO status
      INTEGER HDUTYP             ! HDU type (primary, IMAGE, ASCII or
                                 ! binary table)
      INTEGER I                  ! Loop counter
      INTEGER IOBS               ! Observations loop counter
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Processing and actual type for
                                 ! a column array
      CHARACTER * ( 8 ) KEYWRD   ! FITS header keyword
      INTEGER LBND               ! Lower bound of the array (=1)
      INTEGER NC                 ! Number of characters in observation
                                 ! number
      INTEGER NCF                ! Number of characters in filename
      INTEGER NCU                ! Used length of string
      INTEGER NFIELD             ! Number of fields in table
      INTEGER NREP               ! Number of bad error values replaced
      INTEGER NREPHI             ! Dummy
      INTEGER PNTR( 1 )          ! Pointer to a mapped column array
      INTEGER REPEAT             ! Column repeat count
      LOGICAL THERE              ! Header keyword is present?
      CHARACTER * ( 72 ) UNITS   ! Units of the data or axis array
      INTEGER UBND               ! Upper bound of the array (=dim)
      LOGICAL USED( MXCOLS )     ! Table column used?
      INTEGER WIDTH              ! Column width
      INTEGER WPNTR              ! Pointer to work array (for quality)
      INTEGER * 2 WVALUE         ! A word value

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
*  IUE INES products.
      CALL FTMRHD( FUNIT, 1, HDUTYP, FSTAT )
      IF ( FSTAT .NE. FITSOK ) THEN
         BUFFER = 'Error skipping to the extension of the IUE INES '/
     :            /'FITS file '//FILE( :NCF )//'.'
         CALL COF_FIOER( FSTAT, 'COF_INES_WREXT', 'FTMRHD', BUFFER,
     :                   STATUS )

      ELSE IF ( HDUTYP .NE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FILE', FILE )
         CALL ERR_REP( 'COF_INES_NOBINTAB',
     :     'The first extension of ^FILE is not a BINTABLE.', STATUS )
      END IF

*  Find the shape of the binary table.
*  ===================================

*  Obtain the number of spectra.   There should be one or two spectra
*  present.
      CALL COF_GKEYI( FUNIT, 'NAXIS2', THERE, UBND, COMENT, STATUS )

*  Obtain the number of fields in the table.
      CALL COF_GKEYI( FUNIT, 'TFIELDS', THERE, NFIELD, COMENT, STATUS )

*  Set the number of fields to be no more than the maximum.  This should
*  never be exceeded, and is here defensively in case the format is
*  changed.
      NFIELD = MIN( NFIELD, MXCOLS )

*  By definition the lower bound is always 1.
      LBND = 1

*  Loop for each of the observations.
*  ==================================

*  Initialise flags to indicate that none of the binary-table columns
*  has been used.
      DO I = 1, NFIELD
         USED( I ) = .FALSE.
      END DO

*  Convert the observation number to a string.  It is needed for error
*  messages.
      IOBS = 1
      CALL CHR_ITOC( IOBS, CON, NC )

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

*  Create a NDF
*  ============

*  Set the type of the NDF.
      CALL NDF_STYPE( ITYPE, NDF, 'Data', STATUS )

*  Set the bounds of the NDF.  It is one-dimensional by definition.
      CALL NDF_SBND( 1, LBND, UBND, NDF, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the flux and store as the data array.
*  ============================================

*  Map the NDF component.
      CALL NDF_MAP( NDF, 'Data', ITYPE, 'WRITE', PNTR, EL, STATUS )

*  Read the column into the data array.  Call the appropriate routine
*  for the chosen type.  This should be _REAL.
      IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL FTGCVJ( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADI,
     :                %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL FTGCVE( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADR,
     :                %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL FTGCVD( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADD,
     :                %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IT', ITYPE )
         CALL MSG_SETI( 'ON', IOBS )
         CALL ERR_REP( 'COF_INES_ITYPE',
     :     'Invalid data type (^IT) selected for the IUE INES flux '/
     :     /'array in observation ^ON.', STATUS )
         GOTO 999
      END IF

*  Set the bad-pixel flag.
      CALL NDF_SBAD( BAD, NDF, 'Data', STATUS )

*  Unmap the data array.
      CALL NDF_UNMAP( NDF, 'Data', STATUS )

*  Check that the transfer was correct.
      IF ( FSTAT .NE. FITSOK ) THEN
         BUFFER = 'Error copying the IUE INES flux to the NDF '/
     :            /'data array in observation '//CON( :NC )//'.'
         CALL COF_FIOER( FSTAT, 'COF_INES_CRDAT', 'FTGCVx',
     :                   BUFFER, STATUS )
         GOTO 999
      END IF

*  Obtain the units for this column, and place it into the NDF.  To do
*  this first form the names of the units keyword for this column.
*  Note that NDF_CPUT does not truncate trailing blanks.
      CALL FTKEYN( 'TUNIT', COLNUM, KEYWRD, FSTAT )
      CALL COF_GKEYC( FUNIT, KEYWRD, THERE, UNITS, COMENT, STATUS )
      IF ( THERE ) THEN
         NCU = CHR_LEN( UNITS )
         CALL NDF_CPUT( UNITS( :NCU ), NDF, 'Units', STATUS )
      END IF

*  Set the label for the NDF.
      CALL NDF_CPUT( 'Flux', NDF, 'Label', STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the standard deviation and store as the error array.
*  ===========================================================

*  Find the column number of the errors.
      CALL FTGCNO( FUNIT, .FALSE., 'SIGMA', COLNUM, FSTAT )

*  Find the data code (effectively the data type) for the column.  Use
*  it to define the implementation type.
      CALL FTGTCL( FUNIT, COLNUM, DATCOD, REPEAT, WIDTH, FSTAT )
      CALL COF_STYPC( NDF, 'Variance', ' ', DATCOD, ITYPE, STATUS )

*  Map some workspace.
      CALL PSX_CALLOC( UBND, ITYPE, WPNTR, STATUS )

*  Map the NDF component.
      CALL NDF_MAP( NDF, 'Error', ITYPE, 'WRITE', PNTR, EL, STATUS )

*  Read the column into the work array, and then replace any negative
*  values (the official -1s meaning undefined error and any other
*  garbage) with the bad value.  Call the appropriate routine for the
*  chosen type.
      IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL FTGCVJ( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADI,
     :                %VAL( CNF_PVAL( WPNTR ) ), BAD, FSTAT )

         CALL KPG1_THRSI( .TRUE., EL, %VAL( CNF_PVAL( WPNTR ) ),
     :                    0, VAL__MAXI,
     :                    VAL__BADI, VAL__MAXI,
     :                    %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    NREP, NREPHI, STATUS )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL FTGCVE( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADR,
     :                %VAL( CNF_PVAL( WPNTR ) ), BAD, FSTAT )

         CALL KPG1_THRSR( .TRUE., EL, %VAL( CNF_PVAL( WPNTR ) ),
     :                    0.0, VAL__MAXR,
     :                    VAL__BADR, VAL__MAXR,
     :                    %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    NREP, NREPHI, STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL FTGCVD( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADD,
     :                %VAL( CNF_PVAL( WPNTR ) ), BAD, FSTAT )

         CALL KPG1_THRSD( .TRUE., EL, %VAL( CNF_PVAL( WPNTR ) ),
     :                    0.0D0, VAL__MAXD,
     :                    VAL__BADD, VAL__MAXD,
     :                    %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    NREP, NREPHI, STATUS )

      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IT', ITYPE )
         CALL MSG_SETI( 'ON', IOBS )
         CALL ERR_REP( 'COF_INES_ITYPE',
     :     'Invalid data type (^IT) selected for the IUE INES '/
     :     /'error array in observation ^ON.', STATUS )
         GOTO 999
      END IF

*  Set the bad-pixel flag.
      BAD = BAD .OR. NREP .GT. 0
      CALL NDF_SBAD( BAD, NDF, 'Variance', STATUS )

*  Unmap the error array.  Note this call is assymetric with NDF_MAP,
*  in that the error array can be mapped, but not unmapped; we have
*  to specify the variance instead.
      CALL NDF_UNMAP( NDF, 'Variance', STATUS )

*  Free the workspace.
      CALL PSX_FREE( WPNTR, STATUS )

*  Check that the transfer was correct.
      IF ( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_INES_CRVAR', 'FTGCVx',
     :     'Error copying the IUE INES standard deviation to the '/
     :     /'NDF error array.', STATUS )
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
     :             %VAL( CNF_PVAL( WPNTR ) ), BAD, FSTAT )

*  Map the input array component with the desired data type.  Any type
*  conversion will be performed by the FITSIO array-reading routine.
      CALL NDF_MAP( NDF, 'Quality', '_UBYTE', 'WRITE', PNTR, EL,
     :              STATUS )

*  Transfer the most-significant IUE quality flags across to the NDF's
*  QUALITY component.
      CALL COF_IUEQ( EL, %VAL( CNF_PVAL( WPNTR ) ),
     :               %VAL( CNF_PVAL( PNTR( 1 ) ) ), STATUS )

*  Tidy up the workspace and quality.
      CALL PSX_FREE( WPNTR, STATUS )
      CALL NDF_UNMAP( NDF, 'Quality', STATUS )

*  Check that the transfer was correct.
      IF ( FSTAT .NE. FITSOK ) THEN
         BUFFER = 'Error copying the IUE INES quality to the NDF '/
     :             /'quality array in observation '//CON( :NC )//'.'
         CALL COF_FIOER( FSTAT, 'COF_INES_CRQUA', 'FTGCVB',
     :                   BUFFER, STATUS )
         GOTO 999
      END IF

*  Record that this column is used.
      USED( COLNUM ) = .TRUE.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the wavelengths and their units.
*  =======================================

*  Find the column number of the start wavelength.
      CALL FTGCNO( FUNIT, .FALSE., 'WAVELENGTH', COLNUM, FSTAT )

*  Find the data code (effectively the data type) for the column.
*  Use it along with the preferred data type to define the component's
*  type and the implementation type.
      CALL FTGTCL( FUNIT, COLNUM, DATCOD, REPEAT, WIDTH, FSTAT )
      CALL COF_ATYPC( NDF, 'Centre', 1, ' ', DATCOD, ITYPE, STATUS )

*  Obtain the units for this column, and place it into the NDF.  To do
*  this first form the names of the units keyword for this column.
*  Note that NDF_ACPUT does not truncate trailing blanks.
      CALL FTKEYN( 'TUNIT', COLNUM, KEYWRD, FSTAT )
      CALL COF_GKEYC( FUNIT, KEYWRD, THERE, UNITS, COMENT, STATUS )
      IF ( THERE ) THEN
         NC = CHR_LEN( UNITS )
         CALL NDF_ACPUT( UNITS( :NCU ), NDF, 'Units', 1, STATUS )
      END IF

*  Set the label for the axis.
      CALL NDF_ACPUT( 'Wavelength', NDF, 'Label', 1, STATUS )

*  Record that this column is used and the provision axis-centre data
*  type.
      USED( COLNUM ) = .TRUE.

*  Create the axis centres.
*  ========================

*  Map the NDF component.
      CALL NDF_AMAP( NDF, 'Centre', 1, ITYPE, 'WRITE', PNTR, EL,
     :               STATUS )

*  Read the column into the data array.  Call the appropriate routine
*  for the chosen type.  This should be _REAL.  Note this assumes
* that the axis centres are monotonic.
      IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL FTGCVJ( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADI,
     :                %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL FTGCVE( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADR,
     :                %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL FTGCVD( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADD,
     :                %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IT', ITYPE )
         CALL MSG_SETI( 'ON', IOBS )
         CALL ERR_REP( 'COF_INES_ITYPE',
     :     'Invalid data type (^IT) selected for the IUE INES '/
     :     /'axis-centre array in observation ^ON.', STATUS )
         GOTO 999
      END IF

*  Unmap the axis-centre array.
      CALL NDF_AUNMP( NDF, 'Centre', 1, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

  999 CONTINUE

      END
