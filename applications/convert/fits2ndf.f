      SUBROUTINE FITS2NDF( STATUS )
*+
*  Name:
*     FITS2NDF

*  Purpose:
*     Converts FITS files into NDFs.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FITS2NDF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts one or more files in the FITS format
*     into NDFs.  It can process an arbitrary FITS file to produce an
*     NDF, using NDF extensions to store information conveyed in table
*     and image components of the FITS file.  While no information is
*     lost, in many common cases this would prove inconvenient
*     especially as no meaning is attached to the NDF extension
*     components.  Therefore, FITS2NDF recognises certain data products
*     (currently IUE Final Archive, ISO, and 2dF), and provides
*     tailored conversions that map the FITS data better on to the NDF
*     components.  For instance, a FITS IMAGE extension storing data
*     errors will have its data array transferred to the NDF's VARIANCE
*     (after being squared).  In addition, FITS2NDF can restore NDFs
*     converted to FITS by the sister task NDF2FITS.
*
*     Details of the supported special formats and rules for processing
*     them are given in topic "Special Formats"; the general-case
*     processing rules are described in the "Notes".

*  Usage:
*     fits2ndf in out

*  ADAM Parameters:
*     FMTCNV = LITERAL (Read)
*        This specifies whether or not format conversion will occur.
*        The conversion applies the values of the FITS keywords BSCALE
*        and BZERO to the FITS data to generate the "true" data values.
*        This applies to IMAGE extensions, as well as the primary data
*        array.  If BSCALE and BZERO are not given in the FITS header,
*        they are taken to be 1.0 and 0.0 respectively.
*
*        If FMTCNV=FALSE, the HDS type of the data array in the NDF
*        will be the equivalent of the FITS data format on tape (e.g.
*        BITPIX = 16 creates a _WORD array).  If TRUE, the data array
*        in the NDF will be converted from the FITS data type on tape
*        to _REAL or _DOUBLE in the NDF.  The choice of floating-point
*        data type depends on the number of significant digits in the
*        BSCALE and BZERO keywords.
*
*        FMTCNV must be enclosed in double quotes and may be a list of
*        comma-separated values to be applied to each conversion in
*        turn.  An error results if more values than the number of
*        input FITS files are supplied.  If too few are given, the last
*        value in the list applied to all the conversions; thus a
*        single value is applied to all the input files.  If more than
*        one line is required to enter the information at a prompt then
*        place a "-" at the end of each line where a continuation line
*        is desired.  ["TRUE"]
*     IN = LITERAL (Read)
*        The names of the FITS-format files to be converted to NDFs.
*        It may be a list of file names or direction specifications
*        separated by commas and enclosed in double quotes.  FITS file
*        names may include the regular expressions ("*", "?", "[a-z]"
*        etc.).  Indirection may occur through text files (nested up to
*        seven deep).  The indirection character is "^".  If extra
*        prompt lines are required, append the continuation character
*        "-" to the end of the line.  Comments in the indirection file
*        begin with the character "#".
*     OUT = LITERAL (Write)
*        The names for the output NDFs.  These may be enclosed in
*        double quotes and specified as a list of comma-separated names,
*        OR, using modification elements to specify output NDF names
*        based on the input filenames.  Indirection may be used if 
*        required.
*        
*        The simplest modification element is the asterisk "*", which
*        means call the output NDF files the same name (without any
*        directory specification) as the corresponding input FITS file,
*        but with file extension ".sdf".
*
*        Other types of modification can also occur so OUT = "x*" would
*        mean that the output files would have the same name as the
*        input FITS files except for an "x" prefix.  You can also
*        replace a specified string in the output filename, for example
*        OUT="x*/cal/Starlink/" replaces the string "cal" with
*        "Starlink" in any of the output names "x*".
*     PROEXTS = _LOGICAL (Read)
*        This governs how any extension (here called a sub-file) within
*        the FITS file are processed in the general case.  If TRUE, any
*        FITS sub-file is propagated to the NDF as an NDF extension
*        called NDF_EXT_n, where n is the number of the extension.  If
*        FALSE, any FITS-file extensions are ignored.  The "Notes" of
*        the general conversion contain details of where and in what
*        form the various FITS extensions are stored in the NDF.
*
*        This parameter is ignored when the supplied FITS file is one
*        of the special formats, but excluding NDF2FITS-created files,
*        whose structure in terms of multiple FITS objects is defined.
*        Specialist NDF extensions may be created in this case.  See
*        topic "Special Formats" for details.  [TRUE]
*     PROFITS = _LOGICAL (Read)
*        If TRUE, the primary headers of the FITS file are written
*        verbatim to the NDF's FITS extension.  [TRUE]

*  Examples:
*     fits2ndf 256.fit f256 fmtcnv=f
*        This converts the FITS file called 256.fit to the NDF called
*        f256.  The data type of the NDF's data array matches that of
*        the FITS primary data array.  A FITS extension is created in
*        f256, and FITS sub-files are propagated to NDF extensions.
*     fits2ndf 256.fit f256 noprofits noproexts
*        As the previous example except there will be a format
*        conversion from a FITS integer data type to floating point in
*        the NDF using the BSCALE and BZERO keywords, and there will be
*        no extensions written within f256.
*     fits2ndf "*.fit,p*.fits" "*"
*        This converts a set of FITS files given by the list
*        "*.fit,p*.fits", where * is the match-any-character wildcard.
*        The resultant NDFs take the filenames of the FITS files, so if
*        one of the FITS files was parker.fits, the resultant NDF would
*        be called parker.  Format conversion is performed on integer
*        data types.  A FITS extension is created in each NDF and any
*        FITS sub-files present are propagated to NDF extensions.
*     fits2ndf swp25000.mxlo mxlo25000
*        This converts the IUE MXLO FITS file called swp25000.mxlo to
*        the NDF called mxlo25000.
*     fits2ndf "data/*.silo" "silo*|swp||" noprofits
*        This converts all the IUE SILO FITS files with file extension
*        .silo in directory data to NDFs in the current directory.
*        Each name of an NDF is derived from the corresponding FITS
*        filename; the original name has the "swp" removed and "silo"
*        is prefixed.  So for example, swp25000.silo would become
*        an NDF called silo25000.  No FITS extension is created.
*     fits2ndf "abc.fit,def.fts" "fgh,ijk" fmtcnv="F,T" noproexts
*        This converts the FITS files abc.fit and def.fts to the NDFs
*        called fgh and ijk respectively.  Format conversion is applied
*        to abc.fit but not to def.fts.  FITS extensions are created
*        in the NDFs but there are no extensions for any FITS sub-files
*        that may be present.

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
*     -  This is recognised by the presence of an HDUCLAS1 keyword set
*     to 'NDF'.  The conversion is similar to the general case, except
*     the processing of FITS sub-files and HISTORY headers.
*
*     -  An IMAGE sub-file converts to an NDF variance-array component,
*     provided the HDUCLAS2 keyword is present and has a value that is
*     either 'VARIANCE' or 'ERROR'.
*
*     -  An IMAGE sub-file converts to an NDF quality-array component,
*     provided the HDUCLAS2 keyword is present and has value 'QUALITY'.
*
*     -  FITS ASCII and binary tables become NDF extensions, however,
*     the original structure path and data type are restored using
*     the values of the EXTNAME and EXTTYPE keywords respectively.
*     An extension may be an array of structures, the shape being stored
*     in the EXTSHAPE keyword.  The shapes of multi-dimensional arrays
*     within the extensions are also restored.
*
*     -  HISTORY cards in a special format created by NDF2FITS are
*     converted back into NDF history records.  This will only work
*     provided the HISTORY headers have not been tampered.  Such
*     headers are not transferred to the FITS airlock, when
*     PROFITS=TRUE.


*     o  IUE Final Archive LILO, LIHI, SILO, SIHI
*
*     -  This converts an IUE LI or SI product stored as a FITS primary
*     data array and IMAGE extension containing the quality into an
*     NDF.  Other FITS headers are used to create AXIS structures (SI
*     products only), and character components.

*     -  Details of the conversion are:
*        -  The primary data array of the FITS file becomes NDF main
*        data array.  The value of parameter FMTCNV controls whether
*        keywords BSCALE and BZERO are applied to scale the data;
*        FMTCNV along with the number of significant characters in the
*        keywords decide the data type of the array.  It is expected
*        that this will be _REAL if FMTCNV is TRUE, and _WORD
*        otherwise.
*        -  The quality array comes from the IMAGE extension of the
*        FITS file.  The twos complement values are divided by -128 to
*        obtain the most-significant 8 bits of the 14 in use.  There is
*        no check that the dimension and axis-defining FITS headers in
*        this extension match those of the main data array.  The
*        standard indicates that they will be the same.
*        -  The FILENAME header value becomes the NDF's TITLE component.
*        -  The BUNIT header value becomes the NDF's UNITS component.
*        -  The CDELTn, CRPIXn, and CRVALn define the axis centres.
*        CTYPEn defines the axis labels.  Axis information is only
*        available for the SILO and SIHI products.
*        -  The primary headers may be written to the NDF's FITS
*        extension when parameter PROFITS is TRUE.
*     
*     o  IUE Final Archive MXLO
*
*     -  This will usually be a single 1-dimensional NDF, however, if
*     the binary table contains more than one row, a series of NDFs are
*     stored in a single HDS container file whose name is specified by
*     parameter OUT.  The name of each NDF is the row number.  Thus for
*     OUT=ABC, the second observation will be in NDF ABC.2.
*     -  Only the most-significant 8 bits of the quality flags are
*     transferred to the NDF.
*     -  The primary headers may be written to the standard FITS
*     airlock extension when PROFITS is TRUE.
*     -  The conversion from binary-table columns and headers to NDF
*     objects is as follows:
*
*        NPOINTS                Number of elements
*        WAVELENGTH             Start wavelength, axis label and units
*        DELTAW                 Incremental wavelength
*        FLUX                   Data array, label, units, bad-pixel flag
*        SIGMA                  Data-error array
*        QUALITY                Quality array
*        remaining columns      Component in IUE_MX extension (NET and
*                               BACKGROUND are NDFs)

*     o  ISO CAM auto-analysis (CMAP, CMOS)

*     -  The CAM auto-analysis FITS products have a binary table using
*     the "Green Bank" convention, where rows of the table represent a
*     series of observations, and each row is equivalent to a normal
*     simple header and data unit.  Thus most of the columns have the
*     same names as the standard FITS keywords.
*     -  If there is only one observation, a normal NDF is produced; if
*     there are more than one, the HDS container file of the supplied
*     NDF is used to store a series of NDFs---one for each
*     observation---called OBSn, where n is the observation number.
*     Each observation comprises three rows in the binary table
*     corrsponding to the flux, the r.m.s. errors, and the integration
*     times.
*     -  The conversion from binary-table columns to NDF objects is as
*     follows:
*
*        ARRAY                  Data, error, exposure arrays depending
*                               on the value of column TYPE
*        BLANK                  Data blank (i.e. undefined value)
*        BUNIT                  Data units
*        BSCALE                 Data scale factor
*        BZERO                  Data offset
*        CDELTn                 Pixel increment along axis n
*        CRPIXn                 Axis n reference pixel
*        CRVALn                 Axis n co-ordinate of reference pixel
*        CTYPEn                 Label for axis n
*        CUNITn                 Units for axis n
*        NAXIS                  Number of dimensions
*        NAXISn                 Dimension of axis n
*        remaining columns      keyword in FITS extension
*
*        Some of these remaining columns overwrite the (global) values
*        in the primary headers.  The integration times are stored as
*        an NDF within an extension called EXPOSURE.
*
*        The creation of axis information and extensions does not occur
*        for the error array, as these are already generated when the
*        data-array row in the binary table is processed.
*
*        The BITPIX column is ignored as the data type is determined
*        through the use the TFORMn keyword and the value of FMTCNV in
*        conjunction with the BSCALE and BZERO columns.
*
*     o  ISO LWS auto-analysis (LWS AN)
*
*     -  The conversion from binary-table columns to NDF objects is as
*     follows:
*
*        LSANFLX                Data array, label, and units
*        LSANFLXU               Data errors, hence variance
*        LSANDET                Quality (bits 1 to 4)
*        LSANSDIR               Quality (bit 5)
*        LSANRPID               Axis centres, labels, and units
*                               (x-y positions---dimensions 1 and 2)
*        LSANSCNT               Axis centre, label, and unit (scan
*                               count index---dimension 4)
*        LSANWAV                Axis centre, label, and unit
*                               (wavelength---dimension 3)
*        LSANWAVU               Axis errors (wavelength---dimension 3)
*        LSANFILL               not copied
*        remaining columns      column name in LWSAN extension

*     o ISO SWS auto-analysis (SWS AA)

*     -  The conversion from binary-table columns to NDF objects is as
*     follows:
*
*        SWAAWAVE               Axis centres, label, and units
*        SWAAFLUX               Data array, label, and units
*        SWAASTDV               Data errors, hence variance
*        SWAADETN               Quality
*        SWAARPID               not copied
*        SWAASPAR               not copied
*        remaining columns      column name in SWSAA extension
*
*     o AAO 2dF
*
*     -  The conversion is restricted to a 2dF archive FITS file
*     created by task NDF2FITS.  FITS2NDF restores the original NDF.
*     It creates the 2dF FIBRES extension and its constituent
*     structures, and NDF_CLASS extension.  In addition the variance,
*     axes, and HISTORY records are converted.
*     -  The HISTORY propagation assumes that the FITS HISTORY headers
*     have not been tampered.
*     -  Details of the conversion are:
*        -  The primary data array becomes the NDF's data array.  Any
*        NaN values present become bad values in the NDF.
*        -  The keywords CRVALn, CDELTn, CRPIXn, CTYPEn, CUNITn are
*        used to create the NDF axis centres, labels, and units.
*        -  The OBJECT, LABEL, BUNIT keywords define the NDF's title,
*        label, and units components respectively, if they are defined.
*        -  HISTORY cards in a special format created by NDF2FITS are
*        converted back into NDF history records.
*        -  The NDF variance is derived from the data array of an
*        IMAGE extension (usually the first), if present, provided the
*        IMAGE extension headers have an HDUCLAS2 keyword whose value
*        is either 'VARIANCE' or 'ERROR'.
*        -  The NDF_CLASS extension within the NDF is filled using the
*        a FITS binary-table extension whose EXTNAME keyword's value is
*        NDF_CLASS.  Note: no error is reported if this extension does
*        not exist within the FITS file.
*        -  The FIBRES extension is created from another FITS binary
*        table whose EXTNAME keyword's value is FIBRES.  The OBJECT
*        substructure's component names, data types, and values are
*        taken from the binary-table columns themselves, and the
*        components of the FIELD substructure are extracted from
*        recognised keywords in the binary-table's header.  Note: no
*        error is reported if this extension does not exist within the
*        FITS file.
*        -  Other IMAGE and BINTABLE extensions are propagated to the
*        NDF extension.  It uses the extension name and type found in
*        the EXTNAME and EXTTYPE keywords, or names it NDF_EXT_n for
*        the nth FITS extension.
*        -  A FITS extension in the NDF may be written to store the
*        primary data unit's headers when parameter PROFITS is TRUE.
*        This FITS airlock will not contain any NDF-style HISTORY
*        records.

*  References:
*     Bailey, J.A. 1997, 2dF Software Report 14, version 0.5.
*     NASA Office of Standards and Technology, 1994, "A User's Guide
*       for the Flexible Image Transport System (FITS)", version 3.1.
*     NASA Office of Standards and Technology, 1995, "Definition of
*       the Flexible Image Transport System (FITS)", version 1.1.

*  Related Applications:
*     CONVERT: NDF2FITS; KAPPA: FITSDIN, FITSIN.

*  Implementation Deficiencies:
*     - There is no propagation of arbitrary FITS HISTORY headers to
*     the NDF's history records.
*     - There is no support for FITS World Co-ordinate Systems.
*     [routine_deficiencies]...

*  [optional_A_task_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1994 June 3 (MJC):
*        Original version.
*     1997 March 7 (MJC):
*        Added the documentation for conversion of the special FITS
*        products, included more examples and made other improvements
*        to the documentation.
*     1997 March 24 (MJC):
*        Can recreate NDFs produced by NDF2FITS.
*     1997 November 16 (MJC):
*        Filters out NDF-style history from the FITS airlock.  Fixed bug
*        creating NDF extensions which are arrays of structures.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'FIO_PAR'          ! FIO_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      CHARACTER * ( 2 ) CHR_NTH  ! Ordinal abbreviation

      INTEGER CON_FINDF          ! Find file from wildcarded list
      INTEGER CON_FINDE          ! Routine for closing the file search

      EXTERNAL CON_FINDF
      EXTERNAL CON_FINDE

*  Local Constants:
      INTEGER BLOCKF             ! Blocking factor
      PARAMETER ( BLOCKF = 1 )

*  Local Variables:
      INTEGER ADDED              ! Number of items added to a group
      LOGICAL CFLAG              ! True if a group requires further
                                 ! input via continuation lines
      INTEGER FCGRP              ! Group identifier of FMTCNVs
      INTEGER FDL                ! FIle descriptor for logfile
      LOGICAL FILEXS             ! FITS file exists
      CHARACTER * ( 255 ) FILNAM ! Name of FITS file
      LOGICAL FMTCNV             ! Apply scale and zero?
      CHARACTER * ( 5 ) FMTCON   ! Character form of a FMTCNV value
      CHARACTER * ( 255 ) FSPEC  ! File specification
      LOGICAL GOOD               ! True if all group values are valid
      INTEGER I                  ! Loop counter
      INTEGER IFILE              ! Loop counter for each input NDF
      CHARACTER * ( FIO__SZFNM ) INFILE ! Input-file name
      INTEGER IGRP1              ! Group identifier of input file list
      INTEGER IGRP2              ! Group identifier of input files
      INTEGER IGRP3              ! Group identifier of input purged FITS
                                 ! files
      INTEGER ISTAT              ! Find-file status
      INTEGER IWILD              ! Counter of the wild-carded files
      LOGICAL LEAVE              ! True if the FITS-file testing is
                                 ! finished
      LOGICAL LOGHDR             ! True if there is an open logfile
      INTEGER LP                 ! Loop counter
      INTEGER NDF                ! NDF identifier
      CHARACTER * ( 255 ) NDFNAM ! Name of NDF
      INTEGER NFC                ! Number of FMTCNV values
      INTEGER NGLIST             ! No. of items in input list
      INTEGER NGROUP             ! Group identifier of default list of
                                 ! NDF files
      INTEGER NIFILE             ! Number of NDF files
      INTEGER NOFILE             ! Number of output files
      INTEGER OGROUP             ! Group identifier of output FITS files
      INTEGER PLACE              ! NDF placeholder
      LOGICAL PROEXT             ! True if the other FITS extensions are
                                 ! propagated to NDF extensions
      LOGICAL PROFIT             ! True if the FITS extension is created
      INTEGER TSTAT              ! Temporary status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a logfile.
*  =================

*  Attempt to obtain and open a log file to output the headers and
*  destination files.  A null value, meaning no logfile is required, is
*  handled invisibly.
      LOGHDR = .FALSE.
      CALL ERR_MARK
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 132, FDL, STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         LOGHDR = .TRUE.
      END IF
      CALL ERR_RLSE
      IF ( STATUS .NE. SAI__OK ) GOTO 999

      IF ( LOGHDR ) CALL MSG_OUT( 'LOG', 'Logging to $LOGFILE', STATUS )

*  Get file list and check the number of specifications.
*  =====================================================
*
*  Use GRP to get a list of wildcarded filenames.

*  Create a new group to contain the input file names.
      CALL GRP_NEW( 'Input files', IGRP1, STATUS )

*  Allow for continuation lines.
      CFLAG = .TRUE.
      DO WHILE ( CFLAG .AND. STATUS .EQ. SAI__OK )

*  Get the list of file names from the environment.
         CALL GRP_GROUP( 'IN', GRP__NOID, IGRP1, NGLIST, ADDED, 
     :                   CFLAG, STATUS )

*  Cancel the parameter association in order to get more group values
*  through the parameter, unless there are no more to obtain.
         IF ( CFLAG ) CALL PAR_CANCL( 'IN', STATUS )
      END DO

*  Tidy and exit if there has been an error.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( IGRP1, STATUS )
         GOTO 999
      END IF

*  Create a second group to hold the filenames including expanded
*  wildcards.
      CALL GRP_NEW( 'Expanded wild card files', IGRP2, STATUS )

*  Expand the wildcards.
*  =====================
*
*  Initialise the count of the number of files and the index to the
*  expanded file.
      NIFILE = 0
      DO LP = 1, NGLIST

*  Get a file specification from the input group.
         CALL GRP_GET( IGRP1, LP, 1, FSPEC, STATUS )
         
*  Find the files which match this specification... First initialise
*  the context counter and looping flag.
         IWILD = 0
         LEAVE = .FALSE.

*  Start new error context.
         CALL ERR_MARK

*  Loop for all the files in the wildcard specification.
         DO WHILE ( .NOT. LEAVE )

*  Get a single FITS file that matches this specification.
            ISTAT = CON_FINDF( FSPEC, INFILE, IWILD )

*  Check if a file has been found.  Odd status is good, even is bad.
            IF ( MOD( ISTAT, 2 ) .EQ. 1 ) THEN

*  Inquire whether the file exists or not.  Since the wild carding has
*  obtained the files, a non-existent file indicates that the file name
*  has been truncated to FIO__SZFNM characters.  This is not foolproof
*  as some other file may be obtained.
               INQUIRE ( FILE = INFILE, EXIST=FILEXS )
               IF ( .NOT. FILEXS ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'MXFNC', FIO__SZFNM )
                  CALL MSG_SETC( 'FNAME', INFILE )
                  CALL ERR_REP( 'FITSDIN_TRUNC',
     :              'File name is too long.  Maximum is ^MXFNC '/
     :              /'characters.  Truncated to ^FNAME.', STATUS )
                  CALL ERR_RLSE
                  GOTO 999
               END IF

*  Add this FITS file into the output group.  NIFILE keeps a count of
*  the number of files in the output group.
               CALL GRP_GRPEX( INFILE, GRP__NOID, IGRP2, NIFILE, 
     :                         ADDED, CFLAG, STATUS )

            ELSE

*  Tidy up the file system when the list of files is exhausted.
               ISTAT = CON_FINDE( IWILD )

*  Go to the next GRP expression.
               LEAVE = .TRUE.

            END IF

         END DO

*  Release the error context.
         CALL ERR_RLSE

      END DO

*  Finished with the first group so delete it.
      CALL GRP_DELET( IGRP1, STATUS )

*  Tidy up and exit if something went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( IGRP2, STATUS )
         GOTO 999
      END IF

*  If no files were found, then report an error, and exit.
      IF ( NIFILE .LE. 0 ) THEN
         CALL GRP_GET( IGRP1, 1, 1, FSPEC, STATUS )
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'SPEC', FSPEC )
         CALL ERR_REP( 'FITSDIN_NOFILES',
     :     'No input files found matching the specification "^SPEC".',
     :      STATUS )
         GOTO 999
      END IF

*  Purge any duplication from the FITS files.
      CALL GRP_PURGE( IGRP2, IGRP3, STATUS )

*  Finished with the second group so delete it.
      CALL GRP_DELET( IGRP2, STATUS )

*  Find the number of FITS files after the purge.
      CALL GRP_GRPSZ( IGRP3, NIFILE, STATUS )

*  Tidy up and exit if something went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( IGRP3, STATUS )
         GOTO 999
      END IF

*  At this point the output group contains the paths and names of the
*  FITS files to be processed.  Tell the user how many have been found
*  to help them supply the appropriate number of FMTCNV and output file
*  names.
      CALL MSG_SETI( 'NF', NIFILE )
      IF ( NIFILE .NE. 1 ) THEN
         CALL MSG_OUTIF( MSG__NORM, 'NOFILES', '^NF files selected.',
     :                   STATUS )
      ELSE
         CALL MSG_OUTIF( MSG__NORM, 'NOFILES', '^NF file selected.',
     :                   STATUS )
      END IF

*  Form default list of NDF filenames.
*  ===================================

*  Create a new group to contain the output NDF names.
      CALL GRP_NEW( 'Default output files', NGROUP, STATUS )

*  For the group containing the list of FITS files, remove the file
*  extension and any path.  Store the modified names in the group just
*  created.
      CALL CON_GEXRM( IGRP3, NGROUP, STATUS )

*  Tidy up and exit.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( IGRP3, STATUS )
         CALL GRP_DELET( NGROUP, STATUS )
         GOTO 999
      END IF

*  Get the names of the FITS files.
*  ================================
*
*  Use GRP to get a list of wildcarded filenames.

*  Create a new group to contain the input file names.
      CALL GRP_NEW( 'Output files', OGROUP, STATUS )

*  Allow for continuation lines.
      CFLAG = .TRUE.
      DO WHILE ( CFLAG .AND. STATUS .EQ. SAI__OK )

*  Get the list of output file names from the environment.  Allow
*  modification of the input file names.
         CALL GRP_GROUP( 'OUT', NGROUP, OGROUP, NOFILE, ADDED, CFLAG,
     :                   STATUS )

*  Cancel the parameter association in order to get more group values
*  through the parameter, unless there are no more to obtain.
         IF ( CFLAG ) CALL PAR_CANCL( 'OUT', STATUS )
      END DO

*  Finished with the group of default output file names.
      CALL GRP_DELET( NGROUP, STATUS )

*  Tidy up and exit if something went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( OGROUP, STATUS )
         CALL GRP_DELET( IGRP3, STATUS )
         GOTO 999
      END IF

*  Check that the number of input files matches the number of input
*  files.
      IF ( NOFILE .NE. NIFILE ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NI', NIFILE )
         CALL MSG_SETI( 'NO', NOFILE )
         CALL ERR_REP( 'FITS2NDF_FILECOUNT',
     :     'FITS2NDF: The number of output NDFs (^NO) does not '/
     :     /'equal the number of input FITS files (^NI).', STATUS )

*  Tidy up and exit.
         CALL GRP_DELET( OGROUP, STATUS )
         CALL GRP_DELET( IGRP3, STATUS )
         GOTO 999
      END IF

*  Get the FMTCNVs.
*  ================
*  Loop until all values are acceptable.
      GOOD = .FALSE.
  100 CONTINUE
      IF ( .NOT. GOOD ) THEN

*  Create a new group to contain the input BITPIXs.
         CALL GRP_NEW( 'FMTCNV values', FCGRP, STATUS )

*  Allow for continuation lines.
         CFLAG = .TRUE.
         DO WHILE ( CFLAG .AND. STATUS .EQ. SAI__OK )

*  Get the list of BITPIXs from the environment.
            CALL GRP_GROUP( 'FMTCNV', GRP__NOID, FCGRP, NFC, ADDED,
     :                      CFLAG, STATUS )

*  Cancel the parameter association in order to get more group values
*  through the parameter, unless there are no more to obtain.
            IF ( CFLAG ) CALL PAR_CANCL( 'FMTCNV', STATUS )
         END DO

*  Tidy up and exit.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL GRP_DELET( FCGRP, STATUS )
            CALL GRP_DELET( OGROUP, STATUS )
            CALL GRP_DELET( IGRP3, STATUS )
            GOTO 999
         END IF

*  Assume that the values are good for the moment.
         GOOD = .TRUE.

*  Validate the values.  First get each value, convert it to a logical
*  and then test that the conversion was successful.
         DO I = 1, NFC
            CALL GRP_GET( FCGRP, I, 1, FMTCON, STATUS )

            CALL CHR_CTOL( FMTCON, FMTCNV, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN

*  Report the error, including the incorrect string.
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETC( 'TH', CHR_NTH( I ) )
               CALL MSG_SETC( 'GM', FMTCON )
               CALL MSG_OUT( 'FMTCNV_ERR',
     :           'The ^I^TH value "^GM" is not one of the acceptable '/
     :           /'logical values for FMTCNV.', STATUS )

*  Let the user have another go.  So cancel the parameter value and
*  delete the group.
               CALL PAR_CANCL( 'FMTCNV', STATUS )
               CALL GRP_DELET( FCGRP, STATUS )
               GOOD = .FALSE.
               GOTO 100
            END IF
         END DO
      END IF

*  There are some special cases.  A single value means apply it to all
*  files.  If there are too few, the last value is used for the
*  remainder.  If there are too many, an error results.
      IF ( NFC .GT. NIFILE ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NI', NIFILE )
         CALL MSG_SETI( 'NFC', NFC )
         CALL ERR_REP( 'FITS2NDF_FILECOUNT',
     :     'FITS2NDF: The number of BITPIX values (^NFC) exceeds '/
     :     /'the number of input NDFs (^NI).', STATUS )

*  Tidy up and exit.
         CALL GRP_DELET( FCGRP, STATUS )
         CALL GRP_DELET( OGROUP, STATUS )
         CALL GRP_DELET( IGRP3, STATUS )
         GOTO 999

*  Extend the group by duplication to give the same number of values
*  as input files.  The last value is duplicated.
      ELSE IF ( NFC .LT. NIFILE ) THEN

*  Obtain the last value.
         CALL GRP_GET( FCGRP, NFC, 1, FMTCON, STATUS )

*  Extend the original group by adding the required number of values.
*  This may not be as efficient as having an array but it avoids getting
*  workspace or having a fixed-length array.
         DO I = NFC + 1, NIFILE
            CALL GRP_GRPEX( FMTCON, GRP__NOID, FCGRP, NFC, ADDED,
     :                      CFLAG, STATUS )
         END DO
      END IF

*  Obtain some global parameter values.
*  ====================================

*  Determine whether or not the FITS extension is to be created.
      CALL PAR_GET0L( 'PROFITS', PROFIT, STATUS )

*  Determine whether or not other FITS extensions are to be stored in
*  additional NDF extensions FITS_n.
      CALL PAR_GET0L( 'PROEXTS', PROEXT, STATUS )

*  Process each file.
*  ==================
      DO IFILE = 1, NIFILE

*  Obtain the values for the parameters.
*  =====================================

*  Find the input FITS file name.
         CALL GRP_GET( IGRP3, IFILE, 1, FILNAM, STATUS )

*  Find the output NDF name.
         CALL GRP_GET( OGROUP, IFILE, 1, NDFNAM, STATUS )

*  Find the BITPIX and convert it to an integer value.
         CALL GRP_GET( FCGRP, IFILE, 1, FMTCON, STATUS )
         CALL CHR_CTOL( FMTCON, FMTCNV, STATUS )

*  Access the NDF.
*  ===============

*  Start a new error context.
         CALL ERR_MARK

*  Obtain a placeholder for the output NDF given by the absolute
*  filename.
         CALL NDF_PLACE( DAT__ROOT, NDFNAM, PLACE, STATUS )

*  Create an NDF with a dummy shape and type.  These will be modified
*  once the FITS file is read, and the true shape and data type are
*  known.
         CALL NDF_NEW( '_UBYTE', 1, 1, 1, PLACE, NDF, STATUS )

*  Convert the FITS file.
*  ======================

*  Convert the FITS file into an NDF as best we can.
         CALL COF_F2NDF( FILNAM, NDF, LOGHDR, FDL, FMTCNV,
     :                   PROFIT, PROEXT, STATUS )

*  Tidy the NDF.  Some of the options create a series of NDFs in the
*  original NDF, which has become just a container file and no longer an
*  NDF.  These errors can be ignored, so annul them in a separate error
*  context.
         TSTAT = STATUS
         CALL ERR_MARK
         CALL NDF_ANNUL( NDF, TSTAT )
         IF ( TSTAT .NE. SAI__OK ) CALL ERR_ANNUL( TSTAT )
         CALL ERR_RLSE

*  Flush any errors so that further FITS files can be processed.
         IF ( STATUS .NE. SAI__OK .AND.
     :        IFILE .NE. NIFILE ) CALL ERR_FLUSH( STATUS )

*  Release the error context.
         CALL ERR_RLSE
      END DO

*  Delete the groups.
      CALL GRP_DELET( FCGRP, STATUS )
      CALL GRP_DELET( OGROUP, STATUS )
      CALL GRP_DELET( IGRP3, STATUS )

 999  CONTINUE

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FITS2NDF_ERR',
     :     'FITS2NDF: Error converting a FITS file into an NDF.',
     :     STATUS )
      END IF

      END
