      SUBROUTINE COF_F2NDF( FILNAM, NDF, LOGHDR, FDL, FMTCNV,
     :                      PROFIT, PROEXT, STATUS )
*+
*  Name:
*     COF_F2NDF

*  Purpose:
*     Converts a FITS file into an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_F2NDF( FILNAM, NDF, LOGHDR, FDL, FMTCNV,
*                     PROFIT, PROEXT, STATUS )

*  Description:
*     This routine converts a FITS file into an NDF.  It can process an
*     arbitrary FITS file to produce an NDF, using NDF extensions to
*     store information conveyed in table and image components of the
*     FITS file.  While no information is lost, in many common cases
*     this would prove inconvenient especially as no meaning is attached
*     to the NDF extension components.  Therefore, this routine
*     recognises certain data products (currently IUE, ISO, and 2dF),
*     and provides tailored conversions that map the FITS data better
*     on to the NDF.  For instance, an image extension storing data
*     errors will have its data array transferred to the NDF's VARIANCE
*     (after being squared).  In addition, FITS2NDF can restore NDFs
*     converted to FITS by the sister task NDF2FITS.

*     Details of the supported formats and rules for processing them,
*     and the general-case processing rules are described below.

*  Arguments:
*     FILNAM = CHARACTER * ( * ) (Given)
*        The name of the input FITS file or device.
*     NDF = INTEGER (Given)
*        The identifier of the NDF to be converted from the FITS file.
*     LOGHDR = LOGICAL (Given)
*        If .TRUE., a record of the FITS headers is written to a log
*        file given by descriptor FDL.  If .FALSE., no log is made and
*        argument FDL is ignored. 
*     FDL = INTEGER (Given)
*        The file descriptor for the log file.  This is ignored when
*        LOGHDR is .FALSE..
*     FMTCNV = LOGICAL (Given)
*        This specifies whether or not format conversion will occur.
*        The conversion applies the values of the FITS keywords BSCALE
*        and BZERO to the FITS data to generate the "true" data values.
*        This applies to IMAGE extensions, as well as the primary data
*        array.  If BSCALE and BZERO are not given in the FITS header,
*        they are taken to be 1.0 and 0.0 respectively.
*
*        If FMTCNV=.FALSE., the HDS type of the data array in the NDF
*        will be the equivalent of the FITS data format on tape (e.g.
*        BITPIX = 16 creates a _WORD array).  If FMTCNV=.TRUE., the
*        data array in the NDF will be converted from the FITS data
*        type on tape to _REAL or _DOUBLE in the NDF.  The choice of
*        floating-point data type depends on the number of significant
*        digits in the BSCALE and BZERO keywords.
*     PROFIT = LOGICAL (Given)
*        If .TRUE., the FITS headers are written to the NDF's FITS
*        extension. 
*     PROEXT = LOGICAL (Given)
*        This governs how any extension (here called a sub-file) within
*        the FITS file are processed in the general case.  If .TRUE.,
*        any FITS sub-file is propagated to the NDF as an NDF extension
*        called NDF_EXT_n, where n is the number of the extension.  If
*        .FALSE., any FITS-file extensions are ignored.   The "Notes"
*        of the general conversion contain details of where and in what
*        form the various FITS extensions are stored in the NDF.
*
*        This parameter is ignored when the supplied FITS file is one
*        of the special formats whose structure in terms of multiple
*        FITS objects is defined.  Specialist NDF extensions may be
*        created in this case.  See topic "Special Formats" for
*        references where to find more details.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Some sources of FITS files that require special conversion
*     rules, particularly because they use binary tables, are
*     recognised.  Details of the processing for these is given within
*     topic "Special Formats".  Both NDF and FITS use the term
*     extension, and they mean different things.  Thus to avoid
*     confusion in the descriptions below, the term `sub-file' is used
*     to refer to a FITS IMAGE, TABLE or BINTABLE extension.
*
*     The general rules for the conversion are as follows.

*     -  The primary data array of the FITS file becomes the NDF's data
*     array.  There is an option using parameter FMTCNV to convert
*     integer data to floating point using the values of FITS keywords
*     BSCALE and BZERO.
*     -  Any integer array elements with value equal to the FITS
*     keyword BLANK become bad values in the NDF data array.  Likewise
*     any floating-point data set to an IEEE not-a-number value also
*     become bad values in the NDF's data array.  The BAD_PIXEL flag is
*     set appropriately.
*     -  NDF quality and variance arrays are not created.
*     -  A verbatim copy of the FITS primary header is placed in the
*     NDF's FITS extension when parameter PROFITS is TRUE.
*     -  Here are details of the processing of standard items from the
*     the FITS header, listed by FITS keyword.
*        CRVALn, CDELTn, CRPIXn, CTYPEn, CUNITn --- define the NDF's
*          AXIS structure along the nth axis.  For a given axis CRVALn,
*          CRPIXn, and CDELTn must all be present to define the axis
*          centres.  Where one or more of these is absent, the axis
*          centres become pixel co-ordinates, and if this applies to all
*          dimensions in a multiple-axis dataset, no NDF AXIS structure
*          is made.  CTYPEn defines the label and CUNITn the units for
*          the nth axis.
*        OBJECT, LABEL, BUNIT --- if present are equated to the NDF's
*          title, label, and units components respectively.
*        LBOUNDn --- if present, this specifies the pixel origin for
*          the nth dimension.
*     -  Additional sub-files within the FITS files are converted into
*     extensions within the NDF if parameter PROEXTS is TRUE.  These
*     extensions are named FITS_EXT_m for the mth sub-file.
*     -  An IMAGE sub-file is treated like the primary data array, and
*     follows the rules give above.  However, the resultant NDF is an
*     extension of the main NDF.
*     -  A BINTABLE or TABLE sub-file are converted into a structure
*     of type TABLE ().  This has a NROWS component specifying the
*     number of rows, and a COLUMNS structure containing a series of
*     further structures, each of which has has the name of the
*     corresponding column in the FITS table.  These COLUMN structures
*     contain a column of table data values in component DATA,
*     preserving the original data type; and optional UNITS and COMMENT
*     components which specify the column's units and the meaning of
*     the column.  Thus for example, for the third sub-file of NDF
*     called ABC, the data for column called RA would be located in
*     ABC.MORE.NDF_EXT_3.COLUMNS.RA.DATA.
*     -  A random-group FITS file creates an NDF for each group.  As
*     they are related observations the series of NDFs are stored in a
*     single HDS container file whose name is still given by parameter
*     OUT.  Each group NDF has component name FITS_Gn, where n is the
*     group number.
*
*     Each group NDF contains the full header in the FITS extension,
*     appended by the set of group parameters.  The group parameters
*     are evaluated using their scales and offsets, and made to look
*     like FITS cards.  The keywords of these FITS cards are derived
*     from the values of PTYPEm in the main header, where m is the
*     number of the group parameter.

*  Special Formats:
*     o  NDF2FITS
*
*        -  This is recognised by the presence of an HDUCLAS1 keyword
*        set to 'NDF'.  The conversion is similar to the general case,
*        except the processing of FITS sub-files and HISTORY headers.
*
*        -  An IMAGE sub-file converts to an NDF variance-array
*        component, provided the HDUCLAS2 keyword is present and has a
*        value that is either 'VARIANCE' or 'ERROR'.
*
*        -  An IMAGE sub-file converts to an NDF quality-array
*        component, provided the HDUCLAS2 keyword is present and has
*        value 'QUALITY'.
*
*        -  FITS ASCII and binary tables become NDF extensions,
*        however, the original structure path and data type are
*        restored using the values of the EXTNAME and EXTTYPE keywords
*        respectively.  An extension may be an array of structures, the
*        shape being stored in the EXTSHAPE keyword.  The shapes of
*        multi-dimensional arrays within the extensions are also
*        restored.
*
*        -  HISTORY cards in a special format created by NDF2FITS are
*        converted back into NDF HISTORY records.  This will only work
*        provided the HISTORY headers have not been tampered.
*
*     o  IUE Final Archive LILO, LIHI, SILO, SIHI
*
*        See routine COF_IUESI for details.
*
*     o  IUE Final Archive MXLO
*
*        See routine COF_IUEMX for details.
*
*     o  ISO CAM auto-analysis (CMAP, CMOS)
*
*        See routine COF_CAMAA for details.
*
*     o  ISO LWS auto-analysis (LWS AN)
*
*        See routine COF_LWSAN for details.
*
*     o  ISO SWS auto-analysis (SWS AA)
*
*        See routine COF_SWSAA for details.
*
*     o  AAO 2dF
*
*        See routine COF_2DFIM for details.

*  Implementation Deficiencies:
*     - There is no propagation of arbitrary HISTORY cards in the FITS
*     header to NDF history records.
*     - There is no support for FITS World Co-ordinate Systems.
*     [routine_deficiencies]...

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1994 May 31 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) FILNAM
      LOGICAL LOGHDR
      INTEGER FDL
      LOGICAL FMTCNV
      LOGICAL PROFIT
      LOGICAL PROEXT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a string less trailing
                                 ! blanks
      CHARACTER * ( 2 ) CHR_NTH  ! Ordinal abbreviation

*  Local Constants:
      INTEGER   FITSOK           ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      LOGICAL BAD                ! True if bad values may be present in
                                 ! array
      INTEGER BITPIX             ! FITS file's BITPIX
      INTEGER BLOCSZ             ! Block size of FITS file (or blocking
                                 ! factor if <11)
      CHARACTER * ( 200 ) BUFFER ! Buffer for error messages
      CHARACTER * ( 48 ) COMENT  ! FITS header comment
      CHARACTER * ( 8 ) COMP     ! NDF array component name
      LOGICAL DARRAY             ! True if the current HDU contains a
                                 ! data array
      INTEGER DIMS( NDF__MXDIM ) ! NDF dimensions (axis length)
      INTEGER EL                 ! Number of elements in array
      CHARACTER * ( DAT__SZLOC ) ELOC ! Locator to NDF extension (MORE)
                                 ! structure
      CHARACTER * ( 80 ) ERRMSG  ! FITSIO error message
      CHARACTER * ( 12 ) EXTNAM  ! NDF-extension name
      LOGICAL EXTEND             ! Value of FITS EXTEND keyword
      LOGICAL EXNDF              ! FITS file originated from an NDF?
      LOGICAL FIRST              ! Processing the first HDU
      INTEGER FSTAT              ! FITSIO error status
      INTEGER FSTATC             ! FITSIO error status for file closure
      INTEGER FUNIT              ! Fortran I/O unit for the FITS file
      INTEGER GCMIN              ! Minimum group number (0|1)
      INTEGER GCOUNT             ! Value of FITS GCOUNT keyword
      CHARACTER * ( 12 ) GRPNAM  ! NDF-extension name for group element
      LOGICAL HDUPRE             ! HDUCLASn keyword is present?
      CHARACTER * ( NDF__SZFTP ) HDUCLA ! Classification of HDU
      INTEGER HDUTYP             ! HDU type (primary, IMAGE, ASCII or
                                 ! binary table)
      INTEGER IGROUP             ! Loop counter for random groups
      CHARACTER * ( NDF__SZTYP ) ITYPE ! NDF implementation data type
      CHARACTER * ( 132 ) LOGNAM ! Logfile name
      LOGICAL LOOP               ! Loop for another FITS extension?
      INTEGER LUNIT              ! Logical unit number of logfile
      LOGICAL MULTIP             ! More than one data array?
      INTEGER NC                 ! Number of characters
      INTEGER NCF                ! Number of characters in filename
      INTEGER NDFE               ! Identifier of effective NDF
      INTEGER NDIM               ! Number of dimensions
      INTEGER NHDU               ! Count of header and data unit
      LOGICAL NONSDA             ! True if the current HDU contains a
                                 ! non-standard data array
      INTEGER NPOS               ! Character position in extension name
      INTEGER PCOUNT             ! Value of FITS PCOUNT keyword
      INTEGER PLACE              ! NDF placeholder for <NDF> extension
      CHARACTER * ( DAT__SZLOC ) PLOC ! Locator to NDF top-level
      INTEGER PNTR( 1 )          ! Pointer to NDF array
      INTEGER RECLEN             ! Record length of logfile
      LOGICAL SIMPLE             ! True if the FITS file is simple
      CHARACTER * ( 6 ) SPENAM   ! Name of special type of FITS file
      CHARACTER * ( NDF__SZTYP ) TYPE ! NDF array's data type
      LOGICAL VALID              ! True if the NDF identifier is valid
      LOGICAL WRTEXT             ! True if write NDF FITS extension
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator to an NDF extension
      CHARACTER * ( 20 ) XTENS   ! Name of FITS extension

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Initialise the root name of NDF extensions for FITS extensions.  The
*  number of the FITS extension is appended to create the NDF-extension
*  name.
      EXTNAM = 'FITS_EXT_'
      GRPNAM = 'FITS_G'

*  Open the FITS file.
*  ===================

*  Find a free logical-unit.
      CALL FIO_GUNIT( FUNIT, STATUS )

*  Open the FITS file with read access.
      CALL FTOPEN( FUNIT, FILNAM, 0, BLOCSZ, FSTAT )

*  Get the length of the filename.
      NCF = CHR_LEN( FILNAM )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         BUFFER = 'Error opening input FITS file '//FILNAM( :NCF )//'.'
         CALL COF_FIOER( FSTAT, 'COF_F2NDF_OPENERR', 'FTOPEN', BUFFER,
     :                   STATUS )
         GOTO 999
      END IF

*  Validate the NDF identifier.
*  ============================
      CALL NDF_VALID( NDF, VALID, STATUS )

*  Report an error if the identifier is not valid.
      IF ( .NOT. VALID ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'COF_F2NDF_INVNDF',
     :     'COF_F2NDF: The identifier to the output NDF is invalid. '/
     :     /'(Probable programming error.)', STATUS )
         GOTO 999
      END IF

*  Validate the logfile.
*  =====================

*  Get the record length of the file.
      IF ( LOGHDR ) THEN
         CALL FIO_UNIT( FDL, LUNIT, STATUS )
         INQUIRE ( UNIT = LUNIT, RECL = RECLEN )

*  Check that the headers can be written to the file.
         IF ( RECLEN .LT. 80 ) THEN
            STATUS = SAI__ERROR
            INQUIRE ( UNIT = LUNIT, NAME = LOGNAM )
            CALL MSG_SETC( 'LF', LOGNAM )
            CALL MSG_SETI( 'RL', RECLEN )
            CALL ERR_REP( 'COF_F2NDF_LFRECL',
     :        'File ^LF has recordlength ^RL; it must be at least 80 '/
     :        /'to report the FITS headers (probable programming '/
     :        /'error).', STATUS )
            GOTO 999
         END IF
      END IF

*  Special sources of FITS files.
*  ==============================
*
*  Find whether the FITS file belongs to one of the special cases
*  supported by this FITS reader.
      CALL COF_SPEC( FUNIT, SPENAM, STATUS )
      IF ( SPENAM .EQ. 'SWSAA' ) THEN
         CALL COF_SWSAA( FUNIT, FILNAM, NDF, PROFIT, LOGHDR, FDL,
     :                   ' ', STATUS )

      ELSE IF ( SPENAM .EQ. 'LWSAA' ) THEN
         CALL COF_LWSAN( FUNIT, FILNAM, NDF, PROFIT, LOGHDR, FDL,
     :                   ' ', STATUS )

      ELSE IF ( SPENAM .EQ. 'AAO2DF') THEN
         CALL COF_2DFIM( FUNIT, FILNAM, NDF, PROFIT, LOGHDR, FDL,
     :                   FMTCNV, STATUS )

      ELSE IF ( SPENAM .EQ. 'CAMAA' ) THEN
         CALL COF_CAMAA( FUNIT, FILNAM, NDF, PROFIT, LOGHDR, FDL,
     :                   FMTCNV, STATUS )

      ELSE IF ( SPENAM .EQ. 'IUESI' .OR. SPENAM .EQ. 'IUELI' ) THEN
         CALL COF_IUESI( FUNIT, FILNAM, NDF, PROFIT, LOGHDR, FDL,
     :                   FMTCNV, STATUS )

      ELSE IF ( SPENAM .EQ. 'IUEMX') THEN
         CALL COF_IUEMX( FUNIT, FILNAM, NDF, PROFIT, LOGHDR, FDL,
     :                   STATUS )

      ELSE

*  Loop for each FITS extension.
*  =============================
         LOOP = .TRUE.
         EXNDF = .FALSE.
         NHDU = 0

*  Continue looping when there are more extensions that are requested to
*  be converted into the NDF, and nothing has gone wrong thus far.
         DO WHILE ( LOOP .AND. FSTAT .EQ. FITSOK )

            IF ( .NOT. PROEXT ) LOOP = .FALSE.

*  Increment the count of the header-and-data units.
            NHDU = NHDU + 1
            FIRST = NHDU .EQ. 1

            IF ( .NOT. FIRST ) THEN

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
                     BUFFER = 'of the FITS file '//FILNAM( :NCF )//'.'
                     CALL MSG_SETI( 'N', NHDU - 1 )
                     CALL MSG_SETC( 'NTH', CHR_NTH( NHDU - 1 ) )
                     CALL MSG_SETC( 'BUF', BUFFER )

                     STATUS = SAI__ERROR
                     CALL ERR_REP( 'COF_F2NDF_WREXT',
     :                 'Error skipping to the ^N^NTH extension ^BUF',
     :                 STATUS )
                     CALL COF_FIOER( FSTAT, 'COF_F2NDF_WREXT', 'FTMRHD',
     :                 ' ', STATUS )
                  END IF
                  GOTO 999
               END IF

*  Obtain the name of the current extension.
               CALL FTGKYS( FUNIT, 'XTENSION', XTENS, COMENT, FSTAT )
               IF ( FSTAT .NE. FITSOK ) THEN
                  BUFFER = 'Error obtaining the extension name from '/
     :                     /'FITS file '//FILNAM( :NCF )//'.'
                  CALL COF_FIOER( FSTAT, 'COF_F2NDF_MANDH', 'FTGKYS',
     :                            BUFFER, STATUS )
                  GOTO 999
               END IF
            END IF

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
                  BUFFER = 'Error defaulting the scale and offset for '/
     :                     /'FITS file '//FILNAM( :NCF )//'.'
                  CALL COF_FIOER( FSTAT, 'COF_F2NDF_SCOF', 'FTPSCL',
     :                            BUFFER, STATUS )
                  GOTO 999
               END IF

*  Set the recommended data type to a null string.  This instructs later
*  routines like COF_STYPC to use the data type specified by the FITSIO
*  data-type code (based on BITPIX).
               TYPE = ' '
            END IF

*  Determine the main properties of the FITS object.
*  =================================================

*  Check that if the current HDU is the primary, or that it is an
*  IMAGE, and thus can be processed by the following routine.  
            IF ( FIRST. OR. XTENS .EQ. 'IMAGE' ) THEN

*  Get the mandatory headers of the primary HDU or an IMAGE extension.
               CALL COF_MANDH( FUNIT, FIRST, NDF__MXDIM, SIMPLE, BITPIX,
     :                         NDIM, DIMS, PCOUNT, GCOUNT, EXTEND,
     :                         DARRAY, NONSDA, EL, STATUS )

*  Report the error context.
               IF ( STATUS .NE. SAI__OK ) THEN
                  BUFFER = 'FITS file '//FILNAM( :NCF )//'.'
                  CALL MSG_SETI( 'N', NHDU )
                  CALL MSG_SETC( 'NTH', CHR_NTH( NHDU ) )
                  CALL MSG_SETC( 'BUF', BUFFER )
                  CALL ERR_REP( 'COF_F2NDF_MANDH',
     :              'Error occurred during accessing headers in the '/
     :              /'^N^NTH header and data unit of ^BUF', STATUS )
                  GOTO 999
               END IF

*  Cannot processed non-standard files.
               IF ( FIRST .AND. .NOT. SIMPLE ) THEN
                  STATUS = SAI__ERROR
                  BUFFER = 'The FITS file '//FILNAM( :NCF )//' is not'
                  CALL MSG_SETC( 'BUF', BUFFER )
                  CALL ERR_REP( 'COF_F2NDF_NOTSIM',
     :              '^BUF simple and therefore cannot be processed.',
     :              STATUS )
                  GOTO 999
               END IF

*  Former NDF?
*  ===========
*
*  First see if the primary data or IMAGE extension data originated in
*  an NDF array component.  If it is, we want to replace it rather than
*  create an extension.  A bad status can be ignored, as the presence
*  flag will be false.
               CALL ERR_MARK
               CALL COF_GKEYC( FUNIT, 'HDUCLAS1', HDUPRE, HDUCLA,
     :                         COMENT, STATUS )
               IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
               CALL ERR_RLSE

*  Define whether or not the extension came from an NDF.
               EXNDF = HDUPRE .AND. HDUCLA .EQ. 'NDF'

            END IF

*  Select type of extension.
*  =========================
*
*  There is support for basic FITS, which creates a typical NDF; the
*  IMAGE extension, which creates an NDF within an extension of the
*  original NDF; BINTABLE and TABLE create <TABLE> type extension
*  within the NDF; and random groups, which creates a series of NDFs in
*  the container file of the supplied NDF.

*  Test for a primary HDU or an IMAGE extension.  Note that this
*  includes random groups as these must be defined in the primary HDU.
            IF ( FIRST. OR. XTENS .EQ. 'IMAGE' ) THEN

*  For ordinary files the first (and only) value of the the "group"
*  counter is zero.  This is what certain FITSIO routines expect.
               GCMIN = 0

*  Only create a series of NDFs in the container file.  When the number
*  is one, then a normal NDF can be created.
               MULTIP = GCOUNT .GT. 1 .AND. NONSDA

               IF ( MULTIP ) THEN

*  Get a locator for the dummy NDF already created.
                  CALL NDF_LOC( NDF, 'WRITE', PLOC, STATUS )

*  Delete the existing dummy data array.
                  CALL DAT_ERASE( PLOC, 'DATA_ARRAY', STATUS )

*  Set the minimum group counter.
                  GCMIN = 1

*  After a random-group, it is not clear where any extensions should
*  go, so ensure that the loop ends.  A random-group dataset with
*  other FITS extensions attached is extremely unlikely to cause
*  difficulties given that random-groups were hardly used anyway,
*  almost entirely in the radio-astronomy community, who then developed
*  the original binary tables long before FITS extensions were
*  ratified, and switched to that more-efficient data structure.
                  LOOP = .FALSE.

*  Prepare for a simple and 1-element random-groups primary HDU, or
*  an IMAGE extension.
               ELSE

*  Ensure that the number of group NDFs to process is 0 unless it is a
*  single group random-groups FITS file (it is unlikely but not
*  impossible).
                  GCOUNT = 0
                  IF ( NONSDA ) GCOUNT = 1

*  Copy the NDF identifier to the temporary clone.
                  IF ( FIRST ) THEN
                     NDFE = NDF

*  By definition the NDF must have a Data component.
                     COMP = 'Data'

*  Do we need to create an extension which is an NDF?
*  ==================================================
                  ELSE IF ( XTENS .EQ. 'IMAGE' ) THEN

*  First see if the IMAGE extension data originated in an NDF array
*  component.  If it is, we want to replace it rather than create an
*  extension.
                     IF ( EXNDF ) THEN

*  Obtain the component.
                        CALL ERR_MARK
                        CALL COF_GKEYC( FUNIT, 'HDUCLAS2', HDUPRE,
     :                                  COMP, COMENT, STATUS )
                        IF ( STATUS .NE. SAI__OK )
     :                    CALL ERR_ANNUL( STATUS )
                        CALL ERR_RLSE

*  Provided HDUCLAS2 keyword is present and has one of the
*  array-component names, make assignments to create the component
*  within the current NDF rather than as the data array in an NDF
*  extension.
                        IF ( HDUPRE .AND.
     :                     ( COMP .EQ. 'QUALITY' .OR.
     :                       COMP .EQ. 'VARIANCE' .OR.
     :                       COMP .EQ. 'ERROR' ) ) THEN
                           NDFE = NDF
                        ELSE

*  By definition the extension NDF must have a Data component.
                           COMP ='Data'
                        END IF
                     ELSE
                        COMP ='Data'
                     END IF

                     IF ( COMP .EQ. 'Data' ) THEN

*  Generate the name of the extension.  NPOS is updated so cannot be
*  defined outside the FITS_extension loop.
                        NPOS = 9
                        CALL CHR_PUTI( NHDU - 1, EXTNAM, NPOS )

*  Create an extension of type NDF.
                        CALL NDF_XNEW( NDF, EXTNAM, 'NDF', 0, 0, XLOC,
     :                                 STATUS )

*  Find the parent structure (i.e. .MORE).
                        CALL DAT_PAREN( XLOC, ELOC, STATUS )

*  Create a new NDF in the extension via an NDF placeholder.  The data
*  type and bounds will be changed below once they are known.
                        CALL NDF_PLACE( ELOC, EXTNAM, PLACE, STATUS )
                        CALL NDF_NEW( '_UBYTE', 1, 1, 1, PLACE, NDFE,
     :                                STATUS )
                     END IF
                  END IF
               END IF

*  Loop for each group.
*  ====================

*  The group count is defined by keyword GCOUNT in the FITS header.
*  The dummy NDF becomes the zeroth NDF for a random-groups FITS file.
               DO IGROUP = GCMIN, GCOUNT 

*  Create a NDF for a group dataset.
*  =================================

*  The group-formats will be stored in the container file as NDFs.
                  IF ( NONSDA .AND. IGROUP .GT. 0 ) THEN

*  Generate the name of the component NDF.  NPOS is updated so cannot
*  be defined outside the FITS_extension loop.
                     NPOS = 6
                     CALL CHR_PUTI( IGROUP, GRPNAM, NPOS )

*  Create a new NDF in the container file via an NDF placeholder from
*  the top-level locator.  The data type and bounds will be changed
*  below once they are known.
                     CALL NDF_PLACE( PLOC, GRPNAM, PLACE, STATUS )
                     CALL NDF_NEW( '_UBYTE', 1, 1, 1, PLACE, NDFE,
     :                             STATUS )

*  By definition the extension NDF must have a Data component.
                     COMP = 'Data'
                  END IF

*  Report the full set of headers and/or write to NDF's FITS extension.
*  ====================================================================
*
*  This is slightly less efficient than combining the two operations,
*  but re-using subroutines does make the code easier to follow.

*  Decide whether or not to save the headers in an extension.
*  Exclude the zeroth group of a random-groups dataset.
                  WRTEXT = ( ( FIRST .AND. .NOT. MULTIP ) .OR.
     :                       ( XTENS .EQ. 'IMAGE' .AND. .NOT. EXNDF )
     :                        .OR. ( IGROUP .GT .0 ) ) .AND. PROFIT

*  Read the main header into the FITS extension of the NDF.  The FITS
*  headers for the random groups will appear in each group NDF.
                  IF ( WRTEXT ) THEN
                     CALL COF_WFEXT( FUNIT, NDFE, IGROUP, PCOUNT,
     :                               FILNAM, STATUS )
                  END IF

*  Write out the headers to a logfile, if desired.
                  IF ( LOGHDR )
     :              CALL COF_HDLOG( FUNIT, FDL, FILNAM, NHDU, STATUS )

*  Modify the shape and type of the NDF, now that it is known.
*  ===========================================================

*  Test whether or not there is a data array present.  The zero'th group
*  is a simple array and is merely filled with dummy data, so test for
*  this.
                  IF ( DARRAY .AND. ( IGROUP .GT. 0 .OR.
     :                 ( IGROUP .EQ. 0 .AND. .NOT. NONSDA ) ) ) THEN
                     CALL COF_STYPE( NDFE, COMP, TYPE, BITPIX, ITYPE,
     :                               STATUS )

*  Specify the bounds of the NDF array component.
                     CALL COF_SBND( FUNIT, NDFE, 'LBOUND', STATUS )

*  Copy the data values into the array component.
*  ==============================================

*  First map the input array component with the desired data type.
*  Any type conversion will be performed by the FITSIO array-reading
*  routine.
                     CALL NDF_MAP( NDFE, COMP, ITYPE, 'WRITE', PNTR, EL,
     :                             STATUS )
                     IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Call the appropriate routine for the data type of the created array.
*  The group is 0 for simple FITS or the group identification
*  otherwise.  We always start at the first element.  The arrays may
*  have bad pixels.
                     IF ( ITYPE .EQ. '_UBYTE' ) THEN
                        CALL FTGPVB( FUNIT, IGROUP, 1, EL, VAL__BADUB,
     :                               %VAL( PNTR( 1 ) ), BAD, FSTAT )

                     ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                        CALL FTGPVI( FUNIT, IGROUP, 1, EL, VAL__BADW,
     :                               %VAL( PNTR( 1 ) ), BAD, FSTAT )

                     ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                        CALL FTGPVJ( FUNIT, IGROUP, 1, EL, VAL__BADI,
     :                               %VAL( PNTR( 1 ) ), BAD, FSTAT )

                     ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
                        CALL FTGPVE( FUNIT, IGROUP, 1, EL, VAL__BADR,
     :                               %VAL( PNTR( 1 ) ), BAD, FSTAT )

                     ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                        CALL FTGPVD( FUNIT, IGROUP, 1, EL, VAL__BADD,
     :                               %VAL( PNTR( 1 ) ), BAD, FSTAT )

                     END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
                     IF ( FSTAT .GT. FITSOK ) THEN
                        BUFFER = 'Error reading the '//COMP( :NC )/
     :                           /' array in FITS file '/
     :                           /FILNAM( :NCF )//'.'
                        NC = CHR_LEN( COMP )
                        CALL COF_FIOER( FSTAT, 'COF_F2NDF_READ',
     :                    'FTGPVx', BUFFER, STATUS )
                        CALL NDF_UNMAP( NDFE, COMP, STATUS )
                        GOTO 999
                     END IF

*  Set the bad-pixel flag.
                     IF ( COMP .NE. 'QUALITY' )
     :                 CALL NDF_SBAD( BAD, NDFE, COMP, STATUS )

*  The header is only a dummy, so fill the array with bad values by
*  mapping with the appropriate initialisation.  The values will be
*  returned to the output NDF when the array component is unmapped.
                  ELSE
                     CALL NDF_MAP( NDFE, COMP, '_REAL', 'WRITE/BAD',
     :                             PNTR, EL, STATUS )

                  END IF

*  Tidy the array.
*  ===============
*  Unmap the array.
                  CALL NDF_UNMAP( NDFE, COMP, STATUS )

*  Other components.
*  =================

*  Only need to define the other components once in an NDF.
                  IF ( COMP .EQ. 'Data' ) THEN

*  Create the NDF character components from the FITS headers.
                     CALL COF_NDFCC( FUNIT, NDFE, STATUS )

*  Create the NDF AXIS structure from the FITS headers.
                     CALL COF_NDFAX( FUNIT, NDFE, STATUS )

*  Transfer HISTORY records, assuming they have not been tampered.
                     IF ( EXNDF ) CALL COF_CHISR( FUNIT, NDFE, STATUS )

*  Annul the temporary NDF, and tidy the locator to the extension.
                     IF ( NONSDA .AND. IGROUP .GT. 0 ) THEN
                        CALL NDF_ANNUL( NDFE, STATUS )

*  Tidy the extension NDF and the locators used to create it.
                     ELSE IF ( XTENS .EQ. 'IMAGE' ) THEN
                        CALL NDF_ANNUL( NDFE, STATUS )
                        CALL DAT_ANNUL( XLOC, STATUS )
                        CALL DAT_ANNUL( ELOC, STATUS )
                     END IF
                  END IF
               END DO

*  Binary or ASCII Table.
*  ======================
            ELSE IF ( XTENS .EQ. 'BINTABLE' .OR.
     :                XTENS .EQ. 'TABLE' ) THEN

*  Test whether the FITS file came from an NDF.  If it does, propagate
*  the FITS table to the NDF extension.
               IF ( EXNDF ) THEN
                  CALL COF_FT2NE( FUNIT, NDF, STATUS )

*  Generate the name of the extension.  NPOS is updated so cannot be
*  defined outside the FITS_extension loop.
               ELSE
                  NPOS = 9
                  CALL CHR_PUTI( NHDU - 1, EXTNAM, NPOS )

*  Create a table extension.
                  CALL NDF_XNEW( NDF, EXTNAM, 'TABLE', 0, 0, XLOC,
     :                           STATUS )

*  Call routine to create the <TABLE> structure from the FITS binary
*  or ASCII table.
                  CALL COF_WRTAB( FUNIT, XLOC, STATUS )

*  Tidy the locator to the extension.
                  CALL DAT_ANNUL( XLOC, STATUS )
               END IF

            END IF

         END DO

*  Tidy the parent-structure locator.
         IF ( MULTIP ) THEN
            CALL DAT_VALID( PLOC, VALID, STATUS )
            IF ( VALID ) CALL DAT_ANNUL( PLOC, STATUS )
         END IF

      END IF

*  Flush any FITSIO errors.  Loop until the stack is flushed (indicated
*  by ERRMSG being a blank string).  Also output a header to indicate
*  which file caused the error.
      FIRST = .TRUE.
      LOOP = .TRUE.
      DO WHILE ( LOOP )
         CALL FTGMSG( ERRMSG )
         LOOP = ERRMSG .NE. ' '

         IF ( LOOP ) THEN
            IF ( FIRST ) THEN
               BUFFER = 'Error processing FITS file '//FILNAM( :NCF )/
     :                  /':'
               CALL MSG_SETC( 'BUF', BUFFER )
               CALL ERR_REP( 'COF_F2NDF_ERRH', '^BUF', STATUS )
               FIRST = .FALSE.
            END IF
             
            CALL ERR_REP( 'FITSIO_ERR', '   '//ERRMSG, STATUS )
      
         END IF
      END DO

  999 CONTINUE      

*  Close the FITS file.  The inherited status used by FITSIO still
*  applies to closedown calls, so use a temporary status.
      IF ( FSTAT .GT. FITSOK ) FSTAT = FITSOK
      CALL FTCLOS( FUNIT, FSTATC )
      IF ( FSTATC .GT. FITSOK ) THEN
         BUFFER = 'Error closing the FITS file '//FILNAM( :NCF )//'.'
         CALL COF_FIOER( FSTATC, 'COF_F2NDF_CLOSE', 'FTCLOS', BUFFER,
     :                   STATUS )
      END IF

*  Release the logical-unit.
      CALL FIO_PUNIT( FUNIT, STATUS )

      END
