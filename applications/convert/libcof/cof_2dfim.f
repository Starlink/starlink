      SUBROUTINE COF_2DFIM( FUNIT, FILE, NDF, PROFIT, LOGHDR, FDL,
     :                      FMTCNV, STATUS )
*+
*  Name:
*     COF_2DFIM

*  Purpose:
*     Imports a 2dF FITS file into an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_2DFIM( FUNIT, FILE, NDF, PROFIT, LOGHDR, FDL, FMTCNV,
*                     STATUS )

*  Description:
*     This converts a 2dF FITS file created by NDF2FITS (COF_2DFEX in
*     particular) into an NDF.  It creates the 2dF FIBRES extension and
*     its constituent structures, and NDF_CLASS extension.  In addition
*     the variance, axes, and HISTORY records are converted.

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
*        extension.  It should be set to .TRUE. in normal circumstances.
*     LOGHDR = LOGICAL (Given)
*        If .TRUE., a record of the FITS headers is written to a log
*        file given by descriptor FDL.  If .FALSE., no log is made and
*        argument FDL is ignored. 
*     FDL = INTEGER (Given)
*        The file descriptor for the log file.  This is ignored when
*        LOGHDR is .FALSE..
*     FMTCNV = LOGICAL (Given)
*        This specifies whether or not format conversion will occur.
*        The conversion applies the values of the FITS `keywords'
*        BSCALE and BZERO  to the FITS data to generate the "true" data
*        values.  Keywords is in quotes because the scale and offset
*        are actually in columns of the binary table called BSCALE and
*        BZERO.
*
*        If FMTCNV=.FALSE., the HDS type of the data array in the NDF
*        will be the equivalent of the FITS data format (e.g.
*        BITPIX = 16 creates a _WORD array).  If FMTCNV=.TRUE., the
*        data array in the NDF will be converted from the FITS data
*        type on tape to _REAL or _DOUBLE in the NDF.  The selection
*        of the floating-point type is equivalent to the data types
*        of the BSCALE and BZERO values in the header.
*
*        FMTCNV = .FALSE. is recommended, as there should be no
*        conversion to do.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior requirements:
*     -  The FITS file should already have been opened by FITSIO, and
*     is in the primary HDU.  The NDF must also exist.
*     -  The HISTORY propagation assumes that the FITS HISTORY cards
*     created by routine COF_WHISR have not been tampered.

*  Notes:
*     Details of the conversion are:
*        -  The primary data array becomes the NDF's data array.  Any
*        NaN values present become bad values in the NDF.
*        -  The keywords CRVALn, CDELTn, CRPIXn, CTYPEn, CUNITn are
*        used to create the NDF axis structure. 
*        -  The OBJECT, LABEL, BUNITS keywords define the NDF's title,
*        label, and units components respectively, if they are defined.
*        -  HISTORY cards in a special format created by NDF2FITS are
*        converted back into NDF HISTORY records.
*        -  The NDF variance is derived from the data array in the first
*        (IMAGE) extension.
*        -  The NDF_CLASS extension within the NDF is filled using the
*        next FITS extension---a binary table.
*        -  The FIBRES extension is created using the second binary
*        table.  The OBJECT substructure's component names, data types,
*        and values are taken from the binary-table columns themselves,
*        and the components of the FIELD substructure are extracted
*        from recognised keywords in the binary-table's header.
*        -  A FITS extension in the NDF may be written (PROFIT=.TRUE.)
*        to store the primary data unit's headers.

*  References:
*     Bailey, J.A. 1996, 2dF Software Report 14, version 0.3.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 February 28 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER FUNIT
      CHARACTER * ( * ) FILE
      INTEGER NDF
      LOGICAL PROFIT
      LOGICAL LOGHDR
      INTEGER FDL
      LOGICAL FMTCNV

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Effective string length

*  Local Constants:
      INTEGER  FITSOK            ! Good status for FITSIO library
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      LOGICAL BAD                ! True if bad values may be present in
                                 ! array
      INTEGER BITPIX             ! FITS file's BITPIX
      CHARACTER * ( 256 ) BUFFER ! BUFFER for writing error messages
      CHARACTER * ( 8 ) CLASS    ! Observation class 
      CHARACTER * ( DAT__SZLOC ) CLOC ! Locator to CLASS structure
      CHARACTER * ( DAT__SZLOC ) CLLOC ! Locator to CLASS component
      INTEGER COLEXT             ! Character where extension name begins
      CHARACTER * ( 48 ) COMENT  ! Keyword comment
      LOGICAL DARRAY             ! True if the current HDU contains a
                                 ! data array
      INTEGER DIMS( NDF__MXDIM ) ! NDF dimensions
      INTEGER EL                 ! Number of elements in array
      CHARACTER * ( 68 ) EXTNAM  ! Name of the extension/path
      LOGICAL EXTEND             ! Value of FITS EXTEND keyword
      CHARACTER * ( DAT__SZLOC ) FLOC ! Locator to FIBRES structure
      CHARACTER * ( DAT__SZLOC ) FILOC ! Locator to FIELD structure
      INTEGER FSTAT              ! FITSIO error status
      INTEGER GCOUNT             ! Value of FITS GCOUNT keyword
      INTEGER HDUTYP             ! HDU type (primary, IMAGE, ASCII or
                                 ! binary table)
      INTEGER I                  ! Loop counter
      INTEGER IEXT               ! Extension counter
      CHARACTER * ( NDF__SZTYP ) ITYPE ! NDF implementation data type
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER NCF                ! Number of characters in filename
      INTEGER NDIM               ! Number of dimensions
      CHARACTER * ( DAT__SZLOC ) NLOC ! Locator to NUM_FIBRES component
      INTEGER NOBS               ! Number of rows in table
      LOGICAL NONSDA             ! True if the current HDU contains a
                                 ! non-standard data array
      CHARACTER * ( DAT__SZLOC ) OLOC ! Locator to OBJECT structure
      INTEGER PCOUNT             ! Value of FITS PCOUNT keyword
      INTEGER PNTR( 1 )          ! Pointer to NDF array
      LOGICAL SIMPLE             ! True if the FITS file is simple
      LOGICAL THERE              ! Keyword is present?
      CHARACTER * ( DAT__SZNAM ) TYPE ! Data type

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Obtain the length of the filename.
      NCF = CHR_LEN( FILE )

*  Report the full set of headers and/or write to NDF's FITS extension.
*  ====================================================================
*
*  This is slightly less efficient than combining the two operations,
*  but re-using subroutines does make the code easier to follow.

*  Read the main header into the FITS extension of the NDF.  The FITS
*  headers for the random groups will appear in each group NDF.
      IF ( PROFIT ) CALL COF_WFEXT( FUNIT, NDF, 0, 0, FILE, STATUS )

*  Write out the headers to a logfile, if desired.
      IF ( LOGHDR ) CALL COF_HDLOG( FUNIT, FDL, FILE, 1, STATUS )

*  Data scaling.
*  =============

*  The FMTCNV flag decides whether or not the data scaling is required.
*  The FITSIO routines that obtain the data array(s) will apply the
*  block floating-point scaling as prescribed by the BSCALE and BZERO
*  keywords.
      IF ( FMTCNV ) THEN

*  Scaling is to be applied.  Find the data type required for the
*  output array based upon the number of significant digits in the
*  BSCALE and BZERO keywords.  If these have values of 1.0D0 and 0.0D0
*  respectively either explicitly, or because one or both are absent,
*  then the data type can be set to the null string.  This instructs
*  later routines like COF_STYPC to use the data type specified by the
*  FITSIO data-type code (based on BITPIX).
         CALL COF_DSTYP( FUNIT, 'BSCALE', 'BZERO', TYPE, STATUS )

*  To prevent scaling, the scale and offset must be set to
*  one and zero respectively.  Note that this does not affect the
*  keywords in the header of the input FITS file.  Note that the values
*  are double precision.
      ELSE
         CALL FTPSCL( FUNIT, 1.0D0, 0.0D0, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            BUFFER = 'Error defaulting the scale and offset for FITS '/
     :               /'file '//FILE( :NCF )//'.'
            CALL COF_FIOER( FSTAT, 'COF_2DFIM_SCOF', 'FTPSCL', BUFFER,
     :                      STATUS )
            GOTO 999
         END IF

*  Set the recommended data type to a null string.  This instructs later
*  routines like COF_STYPC to use the data type specified by the FITSIO
*  data-type code (based on BITPIX).
         TYPE = ' '
      END IF

*  Determine the main properties of the FITS object.
*  =================================================

*  Get the mandatory headers of the primary HDU.
      CALL COF_MANDH( FUNIT, .TRUE., 2, SIMPLE, BITPIX, NDIM, DIMS,
     :                PCOUNT, GCOUNT, EXTEND, DARRAY, NONSDA, EL,
     :                STATUS )

*  Report the error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         BUFFER = 'Error occurred during accessing headers in primary '/
     :     /'header and data unit of FITS file '//FILE( :NCF )//'.'

         CALL ERR_REP( 'COF_2DFIM_MANDH', BUFFER, STATUS )
         GOTO 999
      END IF

*  Cannot processed non-standard files.
      IF ( .NOT. SIMPLE ) THEN
         STATUS = SAI__ERROR
         BUFFER = 'The FITS file '//FILE( :NCF )//' is not simple and '/
     :            /'therefore cannot be processed.'

         CALL ERR_REP( 'COF_2DFIM_NOTSIM', BUFFER, STATUS )
         GOTO 999
      END IF

*  Modify the shape and type of the NDF, now that it is known.
*  ===========================================================

*  Test whether or not there is a data array present.
      IF ( DARRAY ) THEN

*  Set the data type of the data and variance arrays.
         CALL COF_STYPE( NDF, 'Data,Variance', TYPE, BITPIX, ITYPE,
     :                   STATUS )

*  Set the shape of the NDF.
         DO I = 1, NDIM
            LBND( I ) = 1
         END DO
         CALL NDF_SBND( NDIM, LBND, DIMS, NDF, STATUS )

*  Copy the data values into the array component.
*  ==============================================

*  First map the input array component with the desired data type.
*  Any type conversion will be performed by the FITSIO array-reading
*  routine.
         CALL NDF_MAP( NDF, 'Data', ITYPE, 'WRITE', PNTR, EL, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Call the appropriate routine for the data type of the created
*  array.  The group is 0, and we always start at the first element.
*  Remember that the input BITPIX values for floating point are one
*  minus the true BITPIX (the non-standard values were needed to
*  determine whether or not scaling was required).  The arrays may have
*  bad pixels.
         IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL FTGPVB( FUNIT, 0, 1, EL, VAL__BADUB,
     :                   %VAL( PNTR( 1 ) ), BAD, FSTAT )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL FTGPVI( FUNIT, 0, 1, EL, VAL__BADW,
     :                   %VAL( PNTR( 1 ) ), BAD, FSTAT )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL FTGPVJ( FUNIT, 0, 1, EL, VAL__BADI,
     :                   %VAL( PNTR( 1 ) ), BAD, FSTAT )
 
         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL FTGPVE( FUNIT, 0, 1, EL, VAL__BADR,
     :                   %VAL( PNTR( 1 ) ), BAD, FSTAT )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL FTGPVD( FUNIT, 0, 1, EL, VAL__BADD,
     :                   %VAL( PNTR( 1 ) ), BAD, FSTAT )

         END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            BUFFER = 'Error reading the data array in FITS file '/
     :               /FILE( :NCF )//'.'

            CALL COF_FIOER( FSTAT, 'COF_2DFIM_READ', 'FTGPVx',
     :                      BUFFER, STATUS )
            CALL NDF_UNMAP( NDF, 'Data', STATUS )
            GOTO 999
         END IF

*  Set the bad-pixel flag.
         CALL NDF_SBAD( BAD, NDF, 'Data', STATUS )

*  The header is only a dummy, so fill the array with bad values by
*  mapping with the appropriate initialisation.  The values will be
*  returned to the output NDF when the array component is unmapped.
      ELSE
         CALL NDF_MAP( NDF, 'Data', '_REAL', 'WRITE/BAD',
     :                 PNTR, EL, STATUS )

      END IF

*  Unmap the array.
      CALL NDF_UNMAP( NDF, 'Data', STATUS )

*  Other components.
*  =================

*  Create the NDF character components from the FITS headers.
      CALL COF_NDFCC( FUNIT, NDF, STATUS )

*  Create the NDF AXIS structure from the FITS headers.
      CALL COF_NDFAX( FUNIT, NDF, STATUS )

*  Transfer the HISTORY information.
      CALL COF_CHISR( FUNIT, NDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Move to the IMAGE extension.
*  ============================

*  Skip to the next HDU.
      CALL FTMRHD( FUNIT, 1, HDUTYP, FSTAT )
      IF ( FSTAT .NE. FITSOK ) THEN

*  Report the error, if it is not the expected end of file (error 107)
*  or has some garbage at the end (unrecognisable FITS record, error
*  252).  In the latter case just annul the error and remove the FITSIO
*  error messages from the stack.
         IF ( FSTAT .EQ. 107 .OR. FSTAT .EQ. 252 ) THEN
            CALL FTCMSG
            FSTAT = FITSOK
         ELSE
            STATUS = SAI__ERROR
            BUFFER = 'Error skipping to the extension of the '/
     :               /'FITS file '//FILE( :NCF )//'.'
            CALL ERR_REP( 'COF_2DFIM_WREXT', BUFFER, STATUS )
            CALL COF_FIOER( FSTAT, 'COF_2DFIM_WREXT', 'FTMRHD',
     :                      ' ', STATUS )
         END IF
         GOTO 999

*  The next HDU is defined to be an IMAGE for the 2dF dataset.
*  products.
      ELSE IF ( HDUTYP .NE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FILE', FILE )
         CALL ERR_REP( 'COF_2DFIM_NOIMAGE',
     :     'The first extension of ^FILE is not IMAGE.', STATUS )
      END IF

*  Map the input array component with the desired data type.  Any type
*  conversion will be performed by the FITSIO array-reading routine.
      CALL NDF_MAP( NDF, 'Variance', '_REAL', 'WRITE', PNTR, EL,
     :              STATUS )

*  Transfer the variance information from the IMAGE extension to the NDF
*  VARIANCE component.
      CALL FTGPVE( FUNIT, 0, 1, EL, VAL__BADR, %VAL( PNTR( 1 ) ), BAD,
     :             FSTAT )

*  Tidy up the variance.
      CALL NDF_UNMAP( NDF, 'Variance', STATUS )

*  Extensions.
*  ===========

*  There should be two BINTABLE extensions corresponding to the two
*  original NDF extensions: NDF_CLASS and FIBRES.
      DO IEXT = 1, 2

*  Skip to the next HDU.  This is defined to be a BINTABLE for the 2dF.
         CALL FTMRHD( FUNIT, 1, HDUTYP, FSTAT )
         IF ( FSTAT .NE. FITSOK ) THEN
            BUFFER = 'Error skipping to the extension of the 2dF FITS '/
     :               /'file '//FILE( :NCF )//'.'
            CALL COF_FIOER( FSTAT, 'COF_2DFIM_WREXT', 'FTMRHD', BUFFER,
     :                      STATUS )

         ELSE IF ( HDUTYP .NE. 2 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FILE', FILE )
            CALL ERR_REP( 'COF_2DFIM_NOBINTAB',
     :        'The extension of ^FILE is not a BINTABLE.', STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain and validate the extension's name.
*  =========================================

*  Obtain the full component name.
         CALL COF_GKEYC( FUNIT, 'EXTNAME', THERE, EXTNAM, COMENT,
     :                   STATUS )

*  Extract the extension name.  This assumes that it was originally an
*  NDF extension.
         COLEXT = INDEX( EXTNAM, 'MORE.' ) + 5
         EXTNAM = EXTNAM( COLEXT: )

         IF ( EXTNAM .NE. 'FIBRES' .AND. EXTNAM .NE. 'NDF_CLASS' ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', EXTNAM )
            CALL MSG_SETC( 'FILE', FILE )
            CALL ERR_REP( 'COF_2DFIM_WRONGEXT',
     :        'The extension of ^FILE has name ^NAME.  It should be '/
     :        /'NDF_CLASS or FIBRES.', STATUS )
            GOTO 999

*  Create the FIBRES extension in the NDF.
*  =======================================
         ELSE IF ( EXTNAM .EQ. 'FIBRES' ) THEN

*  Create the structure to hold the BINTABLE data.
            CALL NDF_XNEW( NDF, 'FIBRES', 'FIBRE_EXT', 0, 0, FLOC,
     :                     STATUS )

*  Obtain the number of elements.
            CALL COF_GKEYI( FUNIT, 'NAXIS2', THERE, NOBS, COMENT,
     :                      STATUS )

*  Create a NUM_FIBRES component, and assign the number of fibres to it
*  using a locator.
            CALL DAT_NEW0I( FLOC, 'NUM_FIBRES', STATUS )
            CALL DAT_FIND( FLOC, 'NUM_FIBRES', NLOC, STATUS )
            CALL DAT_PUT0I( NLOC, NOBS, STATUS )
            CALL DAT_ANNUL( NLOC, STATUS )

*  Create the FIELD sub-structure, and obtain a locator to it.
            CALL DAT_NEW( FLOC, 'FIELD', 'FIB_FIELD', 0, 0, STATUS )
            CALL DAT_FIND( FLOC, 'FIELD', FILOC, STATUS )

*  Transfer any FIELD components from the FITS headers of the FIBRES
*  BINTABLE to the FIELD structure.
            CALL COF_2DFIF( FUNIT, FILOC, STATUS )
            CALL DAT_ANNUL( FILOC, STATUS )

*  Create the OBJECT sub-structure.
            CALL DAT_NEW( FLOC, 'OBJECT', 'FIB_OBJECT', 0, 0, STATUS )
            CALL DAT_FIND( FLOC, 'OBJECT', OLOC, STATUS )

*  Fill the OBJECT structure using the BINTABLE columns.
            CALL COF_T2HDS( FUNIT, OLOC, STATUS )
            CALL DAT_ANNUL( OLOC, STATUS )

*  Free the locator to the FIBRES extension.
            CALL DAT_ANNUL( FLOC, STATUS )

*  Create the NDF_CLASS extension in the NDF.
*  ==========================================
         ELSE IF ( EXTNAM .EQ. 'NDF_CLASS' ) THEN

*  Create the structure to hold the BINTABLE data.
            CALL NDF_XNEW( NDF, 'NDF_CLASS', 'CLASS', 0, 0, CLOC,
     :                     STATUS )
*  Obtain the class.
            CALL FTGCVS( FUNIT, 1, 1, 1, 1, ' ', CLASS, BAD, FSTAT )

            IF ( FSTAT .NE. FITSOK ) THEN
               BUFFER = 'Error obtain the class of the observation in '/
     :                  /'file '//FILE( :NCF )//'.'
               CALL COF_FIOER( FSTAT, 'COF_2DFIM_GTCLASS', 'FTGCVS',
     :                         BUFFER, STATUS )
               GOTO 999
            END IF

*  Create a NAME component, and assign the class to it via a locator.
            CALL DAT_NEWC( CLOC, 'NAME', 8, 0, 0, STATUS )
            CALL DAT_FIND( CLOC, 'NAME', CLLOC, STATUS )
            CALL DAT_PUT0C( CLLOC, CLASS, STATUS )
            CALL DAT_ANNUL( CLLOC, STATUS )

*  Free the locator to the NDF_CLASS extension.
            CALL DAT_ANNUL( CLOC, STATUS )
         END IF
      END DO

  999 CONTINUE

      END
