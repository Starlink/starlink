      SUBROUTINE COF_F2NDF( FILNAM, NDF, LOGHDR, FDL, FMTCNV, USETYP,
     :                      PROFIT, PROXTI, CONTNR, NENCOD, ENCODS,
     :                      WCSATT, WCSCMP, STATUS )
*+
*  Name:
*     COF_F2NDF

*  Purpose:
*     Converts a FITS file into an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_F2NDF( FILNAM, NDF, LOGHDR, FDL, FMTCNV, USETYP, PROFIT,
*                     PROXTI, CONTNR, NENCOD, ENCODS, WCSATT, WCSCMP,
*                     STATUS )

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
*
*     Details of the supported formats and rules for processing them,
*     and the general-case processing rules are described below.

*  Arguments:
*     FILNAM = CHARACTER * ( * ) (Given)
*        The name of the input FITS file or device.  It may include a
*        FITS extension specifier.
*     NDF = INTEGER (Given)
*        The identifier of the NDF to be converted from the FITS file.
*     LOGHDR = LOGICAL (Given)
*        If .TRUE., a record of the FITS headers is written to a log
*        file given by descriptor FDL.  If .FALSE., no log is made and
*        argument FDL is ignored.
*     FDL = INTEGER (Given)
*        The file descriptor for the log file.  This is ignored when
*        LOGHDR is .FALSE..  The file should be formatted and have
*        a record length of at least 80 characters.  There is no
*        validation because the record length is machine dependent.
*     FMTCNV = CHARACTER * ( * ) (Given)
*        This specifies whether or not format conversion will occur.
*        The conversion applies the values of the FITS keywords BSCALE
*        and BZERO to the FITS data to generate the "true" data values.
*        This applies to IMAGE extensions, as well as the primary data
*        array.  If BSCALE and BZERO are not given in the FITS header,
*        they are taken to be 1.0 and 0.0 respectively.
*
*        If FMTCNV="FALSE", the HDS type of the data array in the NDF
*        will be the equivalent of the FITS data format on tape (e.g.
*        BITPIX = 16 creates a _WORD array).  If FMTCNV="TRUE", the
*        data array in the NDF will be converted from the FITS data
*        type to _REAL or _DOUBLE in the NDF.
*
*        The special value FMTCNV="NATIVE" is a variant of "FALSE",
*        that in addition creates a scaled form of NDF array, provided
*        the array values are scaled through BSCALE and/or BZERO
*        keywords (i.e. the keywords' values are not the null 1.0
*        and 0.0 respectively).  This NDF scaled array contains the
*        unscaled data values, and the scale and offset.
*
*        The actual NDF data type for FMTCNV="TRUE", and the data type
*        after applying the scale and offset for FMTCNV="NATIVE" are
*        both specified by parameter USETYP.  However, if USETYP is a
*        blank string or null (!), then the choice of floating-point
*        data type depends on the number of significant digits
*        in the BSCALE and BZERO keywords.
*     USETYP = LOGICAL (Given)
*        Specifies the HDS primitive data type of the NDF data and
*        variance arrays.  It should be one of the HDS primitive types
*        or " ".
*
*        A blank value requests that the type be propagated from the
*        FITS (using the BITPIX keyword); or if FMTCNV is "TRUE", the
*        type is either _REAL or _DOUBLE depending on the precision
*        of the BSCALE and BZERO keywords.
*
*        This data type overrides the data type of rescaled data (see
*        argument FMTCNV) given by the precision of the BSCALE and BZERO
*        keywords.  Care should be exercised that the chosen type
*        supports the dynamic range of the data, especially if
*        FMTCNV="TRUE".
*     PROFIT = LOGICAL (Given)
*        If .TRUE., the FITS headers are written to the NDF's FITS
*        extension.
*     PROXTI = LOGICAL (Given)
*        This governs how any extension (here called a sub-file) within
*        the FITS file are processed in the general case.  If .TRUE.,
*        any FITS sub-file is propagated to the NDF as an NDF extension
*        called FITS_EXT_n, where n is the number of the extension.  If
*        .FALSE., any FITS-file extensions are ignored.  The "Notes"
*        of the general conversion contain details of where and in what
*        form the various FITS extensions are stored in the NDF.
*
*        This parameter is ignored when the supplied FITS file is one
*        of the special formats whose structure in terms of multiple
*        FITS objects is defined.  Specialist NDF extensions may be
*        created in this case.  See topic "Special Formats" for
*        references where to find more details.
*     CONTNR = LOGICAL (Given)
*        If TRUE causes each HDU from the FITS file to be written as
*        a component of an HDS container file.  Each component will be
*        named HDU_n, where n is the FITS HDU number.  The primary HDU
*        is numbered 0.  (Note these can be altered for UKIRT_HDS data
*        or where a table is used to assign names to HDUs.)  If the FITS
*        HDU contains a data array, the HDS component will be an NDF; if
*        not, it will be a structure of type FITS_HEADER containing the
*        FITS header as an array of type _CHAR*80.
*     NENCOD = INTEGER (Given)
*        The number of encodings supplied in ENCODS.
*     ENCODS( NENCOD ) = CHARACTER * ( * ) (Given)
*        The preferred AST encodings to use when creating the NDF WCS
*        component, in order of preference (most preferable first).
*        It is ignored if NENCOD is zero.
*     WCSATT = CHARACTER * ( * ) (Given)
*        Attribute settings for the WCS FitsChan.
*     WCSCMP = CHARACTER * ( * ) (Given)
*        This requests where co-ordinate information is stored in the
*        NDF for arbitrary FITS files.  FITS files from certain sources
*        (see "Special Formats" below) adopt their own conventions such
*        as always creating AXIS structures and not WCS, thus ignore
*        this argument.  The allowed values are as follows.
*
*        "Axis" --- Writes co-ordinates of each element in the AXIS
*                   structure.
*        "WCS"  --- Stores co-ordinate information in the WCS component.
*        "Both" --- Writes co-ordinate information in both the AXIS and
*                   WCS components.
*        "None" --- Ignores co-ordinate information.
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
*     The general rules for the conversion apply if the FITS file is not
*     one of the "Special Formats" (including one defined by an EXTABLE)
*     and parameter CONTAINER is not TRUE.
*
*     The general rules are as follows.
*
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
*          WCS and/or AXIS component (depending on argument WCSCMP).
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
*     ABC.MORE.FITS_EXT_3.COLUMNS.RA.DATA.
*     -  A random-group FITS file creates an NDF for each group.  As
*     they are related observations the series of NDFs are stored in a
*     single HDS container file whose name is still given by parameter
*     OUT.  Each group NDF has component name FITS_Gn, where n is the
*     group number.
*      Each group NDF contains the full header in the FITS extension,
*     appended by the set of group parameters.  The group parameters
*     are evaluated using their scales and offsets, and made to look
*     like FITS cards.  The keywords of these FITS cards are derived
*     from the values of PTYPEm in the main header, where m is the
*     number of the group parameter.
*     -  If FILNAM specifies a particular FITS sub-file, that sub-file
*     will be treated as if it were the primary HDU.  No other sub-
*     files will be propagated, regardless of the value of PROEXTS.
*     -  Compressed FITS may be supplied.

*  Implementation Deficiencies:
*     - There is no propagation of arbitrary HISTORY cards in the FITS
*     header to NDF history records.
*     [routine_deficiencies]...
*
*     [optional_subroutine_items]...

*  Special Formats:
*     o  Compressed
*        It can process both external and internal compressed FITS
*        files.
*        -  The external compression applies to the whole file and the
*        conversion recognises gzip (.gz) and UNIX compress (.Z)
*        formats.
*        -  Internal compressions are where a large image is tiled and
*        each tile is compressed.  The supported formats are Rice, the
*        IRAF PLIO, and GZIP.
*
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
*        provided the HISTORY headers have not been tampered.  Such
*        headers are not transferred to the FITS airlock, when
*        PROFIT = .TRUE..
*
*        -  Any SMURF package's ancillary IMAGE sub-files are restored
*        to a SMURF extension, with the original names and structure
*        contents.  Thus the global HISTORY present in each sub-file is
*        not duplicated in each SMURF-extension NDF.
*
*        -  When CONTNR is .TRUE., a former UKIRT_HDS container file,
*        identified by the presence and values of HDSNAME and HDSTYPE 
*        keywords may be created.  The container file has the original
*        structure including the NDFs' names, unless a text file is 
*        used to associate sub-files with NDFs (see FITS2NDF EXTABLE 
*        documentation for details).
*
*     o  IUE Final Archive LILO, LIHI, SILO, SIHI
*
*        See routine COF_IUESI for details.
*
*     o  IUE Final Archive MXLO
*
*        See routine COF_IUEMX for details.
*
*     o  IUE Final Archive MXHI
*
*        See routine COF_IUEMH for details.
*
*     o  IUE INES reduced spectra
*
*        See routine COF_INES for details.
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
*
*     o  EXTABLE format
*
*        A general method of specifying other special conversions is
*        provided by the EXTABLE system - See routine COF_EXTAB for
*        details.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1997-2004 Central Laboratory of the Research
*     Councils.  Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council.  Copyright (C) 2007-2008, 2010-2011 Science
*     & Technology Facilities Council.  All Rights Reserved.

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
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     AJC: Alan J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     1994 May 31 (MJC):
*        Original version.
*     1997 November 16 (MJC):
*        Filter out NDF-style HISTORY.
*     18-DEC-1997 (DSB):
*        Added support for NDF WCS component.
*     1998 January 6 (MJC):
*        Removed the validation of the log file.
*     1998 January 22 (MJC):
*        Added processing of IUE MXHI product.
*     8-JAN-1999 (DSB):
*        Added FMTCNV to argument list for COF_STYPE call.
*     28-MAR-2000 (AJC):
*        Open file with COF_FTOPR to handle multi-extension FITS.  Also
*        prevent PROEXTS if single extension is specified.  Don't set up
*        data conversion if no data array (FTPSCL crashes).
*        Fairly major revision:
*          -  Use NHDU as extension number set in COF_FTOPR (0-n not
*          1-n); alters reference in error messages and logfile
*          captions.
*          -  Rename FIRST to PRMRY as first is not necessarily primary
*          now.
*          -  Increment NHDU and move to next extension at end of loop.
*          - Get mandatory headers and check for non-standard files
*          earlier in loop as require DARRAY.
*      4-APR-2000 (AJC):
*        Add EXTABLE handling.  Use COF_CHKXT to check for required
*        extension.
*     30-JUN-2000 (AJC):
*        Allow top-level NDF to be created.  Use COF_EXTNF to report
*        unused specifiers.
*     12-JUL-2000 (AJC):
*        Correctly obtain parent locator for all multip.  Type
*        NDF_CONTAINER for top-level of multip EXTABLE output.
*     15-AUG-2000 (AJC):
*        Use %VAL(CNF_PVAL(PNTR)) construct.  Extract code into
*        COF_NEEDXT.  Add header merging.
*     30-AUG-2000 (AJC):
*        Correct NDF_EXT_n to FITS_EXT_n in description
*      5-SEP-2000 (AJC):
*        Allow NDFNAM EXTNi.name with i, . and name optional.  Also
*        implement NDFNAM EXTNi.name for tables.
*      7-DEC-2000 (AJC):
*        Create FITS_HEADER containing _CHARACTER array, not NDF, for
*        FITS HDUs with no data array.
*     12-DEC-2000 (AJC):
*        Separate data scaling into COF_DOSCL.
*      5-JAN-2001 (AJC):
*        Check for attempt to add second NDF component to a FITS_HEADER.
*      3-FEB-2001 (AJC):
*        Don't call FTWCS if NDIM=0.
*      1_MAY-2001 (AJC):
*        Allocate NHEAD+1 logicals for RETAIN (allows for blank between
*        last HISTORY record and END.
*     20-FEB-2002 (DSB):
*        Added argument WCSATT.
*     20-DEC-2002 (AJC):
*        Added the MERGED flag as the answer given by the CFITSIO
*        routine FTGHSP seems to incorrectly count the END card for the
*        merged header so the count must be adjusted.
*     2003 May 3 (MJC):
*        Added support for INES IUE spectra.
*     2004 September 10 (TIMJ):
*        Initialise some variables that were causing valgrind trouble.
*     2006 April 7 (MJC):
*        Add USETYP argument.  Put the local variables into order and
*        various tidying steps.
*     2006 April 10 (MJC):
*        Added warning message when the chosen data type has less
*        precision and dynamic range than expected from the file.
*     2006 December 28 (MJC):
*        Fixed bug by initialising TYPE and not passing a null TYPE to
*        COF_TYPSZ.
*     2007 January 3 (MJC):
*        Make argument FMTCNV character to allow for FMTCNV=Native, and
*        create a scaled NDF array for that case.
*     2007 July 9 (PWD):
*        Added support for compressed images.
*     2008 February 12 (MJC):
*        Added support for reading SMURF MEF files, ignoring duplicated
*        HISTORY and AXIS information.
*     2010 May 6 (MJC):
*        Cope better with data that were formerly in a UKIRT_HDS
*        container file using the HDSNAME and HDSTYPE keywords.
*        -  Recreate each component NDF's name from its respective
*        HDSNAME keyword.
*        -  Retain the HEADER NDF instead of replacing it with a
*        component of type FITS_HEADER.
*        -  Reset the top-level type to UKIRT_HDS.
*     2011 January 12 (MJC):
*        Use KPG_TYPSZ instead of COF_TYPSZ.
*     2011 February 24 (MJC):
*        Add WCSCMP argument.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'CNF_PAR'          ! CNF definitions
      INCLUDE 'MSG_PAR'          ! Message-system constants

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) FILNAM
      LOGICAL LOGHDR
      INTEGER FDL
      CHARACTER * ( * ) FMTCNV
      CHARACTER * ( * ) USETYP
      LOGICAL PROFIT
      LOGICAL PROXTI
      LOGICAL CONTNR
      INTEGER NENCOD
      CHARACTER ENCODS( NENCOD ) * ( * )
      CHARACTER WCSATT * ( * )
      CHARACTER WCSCMP * ( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      CHARACTER * ( 2 ) CHR_NTH  ! Ordinal abbreviation

*  Local Constants:
      INTEGER   FITSOK           ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

*  Global Variables:
      INCLUDE 'F2NDF1_CMN'       ! EXTABLE variables
*
*     Symbolic Constants defined:
*        MAXCMP = INTEGER
*           The maximum number of components allowed
*        MAXEXT = INTEGER
*           The maximum number of extension sets allowed
*
*     Global Variables declared:
*        NCMP = INTEGER
*           Number of component lines in EXTABLE
*        NEXTS = INTEGER
*           Number of extension sets in EXTABLE
*        DONE(MAXCMP,MAXEXT) = LOGICAL
*           If extension specifier has been matched
*        EXTNS(MAXCMP,MAXEXT) = CHARACTER*32
*           Extension table from EXTABLE
*        NDFNMS(MAXEXT) = CHARACTER*(DAT__SZNAM)
*           NDF names from EXTABLE
*        COMPS(MAXCMP) = CHARACTER*(DAT__SZNAM*2)
*           Component names from EXTABLE
*        CODES(MAXCMP) = CHARACTER*12
*           Transformation codes from EXTABLE

      INCLUDE 'F2NDF2_CMN'        ! EXTABLE variables
*
*     Global Variables declared:
*        NPHEAD = INTEGER
*           The number of saved primary header cards

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) ALOC ! Locator to an NDF FITS Airlock
      LOGICAL BAD                ! Bad values may be present in array?
      INTEGER BITPIX             ! FITS file's BITPIX
      INTEGER BLOCSZ             ! Block size of FITS file (or blocking
                                 ! factor if <11)
      CHARACTER * ( 200 ) BUFFER ! Buffer for error messages
      CHARACTER * ( 48 ) COMENT  ! FITS header comment
      CHARACTER * ( 8 ) COMP     ! NDF array component name
      CHARACTER * ( DAT__SZTYP ) CTYPE ! Type of component
      LOGICAL DARRAY             ! Current HDU contains a data array?
      INTEGER DIMS( NDF__MXDIM ) ! NDF dimensions (axis length)
      INTEGER EL                 ! Number of elements in array
      CHARACTER * ( DAT__SZLOC ) ELOC ! Locator to NDF extension (MORE)
                                 ! structure
      CHARACTER * ( 80 ) ERRMSG  ! FITSIO error message
      LOGICAL EXTABL             ! Using an EXTABLE?
      CHARACTER * ( 12 ) EXTNAM  ! NDF-extension name
      LOGICAL EXTEND             ! Value of FITS EXTEND keyword
      LOGICAL EXNDF              ! FITS file originated from an NDF?
      LOGICAL FIRST              ! Processing the first HDU
                                 ! (Also used in flushing errors.)
      LOGICAL FMTCVT             ! Local, possibly modified, FMTCNV
      INTEGER FSTAT              ! FITSIO error status
      INTEGER FSTATC             ! FITSIO error status for file closure
      INTEGER FUNITH             ! FITS unit for (merged?) FITS header
      INTEGER FUNITD             ! FITS unit for the FITS file
      INTEGER GCMIN              ! Minimum group number (0|1)
      INTEGER GCOUNT             ! Value of FITS GCOUNT keyword
      CHARACTER * ( 12 ) GRPNAM  ! NDF-extension name for group element
      CHARACTER * ( DAT__SZNAM ) HDSNAM ! Component name from multi-NDF
      CHARACTER * ( DAT__SZTYP ) HDSTYP ! Component type from multi-NDF
      LOGICAL HDUPRE             ! HDUCLASn keyword is present?
      CHARACTER * ( NDF__SZFTP ) HDUCLA ! Classification of HDU
      INTEGER HDUTYP             ! HDU type (primary, IMAGE, ASCII or
                                 ! binary table)
      INTEGER I, J               ! Loop counters
      INTEGER ICMP               ! Component number in EXTABLE
      INTEGER IEXT               ! Extension-set number in EXTABLE
      INTEGER IGROUP             ! Loop counter for random groups
      LOGICAL ISNDF              ! If a genuine NDF is created
      CHARACTER * ( NDF__SZTYP ) ITYPE ! NDF implementation data type
      LOGICAL LOOP               ! Loop for another FITS extension?
      LOGICAL MERGED             ! Headers have been merged?
      LOGICAL NAMPRE             ! HDSNAME keyword is present?
      LOGICAL NATIVE             ! Propagate values, scale, and offset?
      LOGICAL MULTIP             ! More than one data array?
      INTEGER NBFTYP             ! Number of bytes in FITS-implied type
      INTEGER NBUTYP             ! Number of bytes in user-selected type
      INTEGER NC                 ! Number of characters
      INTEGER NCF                ! Number of characters in filename
      INTEGER NDFS(MAXEXT)       ! NDFs for each extn set in EXTABLE
      INTEGER NDFE               ! Identifier of effective NDF
      CHARACTER * ( DAT__SZNAM ) NDFNAM ! Multi NDF name
      INTEGER NDIM               ! Number of dimensions
      LOGICAL NEEDED             ! Extension needed?
      LOGICAL NEWNDF             ! NDF needs sizing?
      INTEGER NHDU               ! Count of header and data unit
      INTEGER NHEAD              ! Number of FITS header cards
      LOGICAL NONSDA             ! True if the current HDU contains a
                                 ! non-standard data array
      INTEGER NPOS               ! Character position in extension name
      INTEGER PCOUNT             ! Value of FITS PCOUNT keyword
      INTEGER PLACE              ! NDF placeholder for <NDF> extension
      CHARACTER * ( DAT__SZLOC ) PLOC ! Locator to NDF top-level
      INTEGER PNTR( 1 )          ! Pointer to NDF array
      LOGICAL PRMRY              ! Processing the primary HDU?
      LOGICAL PROEXT             ! Local version of PROXTI argument (may
                                 ! alter)
      INTEGER REPNTR             ! Pointer to header-propagation flags
      LOGICAL SIMPLE             ! Is the FITS file simple?
      CHARACTER*6 SPENAM         ! Name of special type of FITS file
      CHARACTER*( NDF__SZTYP ) STYPE ! NDF scaled array's data type
      LOGICAL THERE              ! Has NDF already been created?
      LOGICAL TOAXIS             ! Write co-ordinate info. to AXIS?
      LOGICAL TOWCS              ! Write co-ordinate info. to WCS?
      CHARACTER*( NDF__SZTYP ) TYPE ! NDF array's data type
      LOGICAL TYPPRE             ! Is HDSTYPE keyword present?
      CHARACTER*( NDF__SZTYP ) UTYPE ! Use data type
      LOGICAL VALID              ! Is NDF identifier valid?
      LOGICAL WRTEXT             ! Write NDF FITS extension?
      CHARACTER*( DAT__SZLOC ) XLOC ! Locator to an NDF extension
      CHARACTER*20 XTENS         ! Type of FITS extension

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Set EXTABL for clarity of code.
      EXTABL = NCMP .GT. 0

*  Initialise the merged header flag.
      MERGED = .FALSE.

*  Native propagation affects the format-conversion flag.  Once the
*  Native special case is extracted from FMTCNV, FMTCNV can become
*  LOGICAL type again.  Meanwhile the requested type applies to the
*  scaled data type, not the actual storage type of the HDS array.
*  Hence we need a another type variable to store a modified USETYP
*  that requests the HDS data type.
      NATIVE = FMTCNV .EQ. 'NATIVE'
      IF ( NATIVE ) THEN
         FMTCVT = .FALSE.
         STYPE = USETYP
         UTYPE = ' '
      ELSE
         CALL CHR_CTOL( FMTCNV, FMTCVT, STATUS )
         UTYPE = USETYP
      END IF

*  Initialise variables that would otherwise not be initialised by
*  some compilers
      XTENS = ' '
      MULTIP = .FALSE.

      IF ( EXTABL ) THEN

*  Initialize the DONE flag array.  We do it here afresh for each FITS
*  file---this keeps a record of which extensions in EXTABLE have been
*  done---to avoid repetitive checks and confirm completion at the end.
*  Blank means not required so treat as done.
         DO J = 1, NEXTS
            DO I = 1, NCMP
               IF ( EXTNS( I, J ) .EQ. ' ' ) THEN
                  DONE( I, J ) = .TRUE.
               ELSE
                  DONE( I, J ) = .FALSE.
               END IF
            END DO
         END DO
      END IF

*  Initialise the root name of NDF extensions for FITS extensions.  The
*  number of the FITS extension is appended to create the NDF-extension
*  name.
      GRPNAM = 'FITS_G'

*  Initialise the number of saved primary header cards.
      NPHEAD = 0

*  Create flags whether or not to create AXIS, and WCS components.
      TOAXIS = WCSCMP .EQ. 'BOTH' .OR. WCSCMP .EQ. 'AXIS'
      TOWCS = WCSCMP .EQ. 'BOTH' .OR. WCSCMP .EQ. 'WCS'

*  Open the FITS file.
*  ===================

*  Find a free logical-unit.
      CALL FIO_GUNIT( FUNITH, STATUS )

*  Get the length of the filename.
      NCF = CHR_LEN( FILNAM )

*  Open the FITS file with read access.  NHDU is returned negative if
*  there was no extension specified, or to the specified extension
*  number (0 indicates the Primary HDU).
      CALL COF_FTOPR( FUNITH, FILNAM(1:NCF), EXTABL .OR. CONTNR, BLOCSZ,
     :                NHDU, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Set the initial Header and Data Unit number.
      IF ( NHDU .LT. 0 ) THEN

*  If no extension specifier, NHDU is 0 (Primary).  Force propagation of
*  extensions if an EXTABLE is specified; otherwise use the given value
*  of the PROEXTS parameter.
         IF ( EXTABL ) THEN
            PROEXT = .TRUE.
         ELSE
            PROEXT = PROXTI
         END IF
         NHDU = 0
      ELSE

*  If extension specified, don't propagate extensions.
         PROEXT = .FALSE.
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

      CALL NDF_BEGIN

*  Special sources of FITS files.
*  ==============================
*
*  Find whether the FITS file belongs to one of the special cases
*  supported by this FITS reader.
      CALL COF_SPEC( FUNITH, SPENAM, STATUS )

      IF ( SPENAM .EQ. 'SWSAA' ) THEN
         CALL COF_SWSAA( FUNITH, FILNAM, NDF, PROFIT, LOGHDR, FDL,
     :                   ' ', STATUS )

      ELSE IF ( SPENAM .EQ. 'LWSAA' ) THEN
         CALL COF_LWSAN( FUNITH, FILNAM, NDF, PROFIT, LOGHDR, FDL,
     :                   ' ', STATUS )

      ELSE IF ( SPENAM .EQ. 'AAO2DF' ) THEN
         CALL COF_2DFIM( FUNITH, FILNAM, NDF, PROFIT, LOGHDR, FDL,
     :                   FMTCVT, STATUS )

      ELSE IF ( SPENAM .EQ. 'SMURF' ) THEN
         CALL COF_SMFIM( FUNITH, FILNAM, NDF, PROFIT, LOGHDR, FDL,
     :                   FMTCVT, NENCOD, ENCODS, WCSATT, STATUS )

      ELSE IF ( SPENAM .EQ. 'CAMAA' ) THEN
         CALL COF_CAMAA( FUNITH, FILNAM, NDF, PROFIT, LOGHDR, FDL,
     :                   FMTCVT, STATUS )

      ELSE IF ( SPENAM .EQ. 'IUESI' .OR. SPENAM .EQ. 'IUELI' ) THEN
         CALL COF_IUESI( FUNITH, FILNAM, NDF, PROFIT, LOGHDR, FDL,
     :                   FMTCVT, STATUS )

      ELSE IF ( SPENAM .EQ. 'IUEMX') THEN
         CALL COF_IUEMX( FUNITH, FILNAM, NDF, PROFIT, LOGHDR, FDL,
     :                   STATUS )

      ELSE IF ( SPENAM .EQ. 'IUEMH') THEN
         CALL COF_IUEMH( FUNITH, FILNAM, NDF, PROFIT, LOGHDR, FDL,
     :                   STATUS )

      ELSE IF ( SPENAM .EQ. 'INES') THEN
         CALL COF_INES( FUNITH, FILNAM, NDF, PROFIT, LOGHDR, FDL,
     :                  STATUS )

      ELSE

*  Loop for each FITS extension.
*  =============================
         LOOP = .TRUE.
         EXNDF = .FALSE.
         FIRST = .TRUE.

*  Continue looping when there are more extensions that are requested to
*  be converted into the NDF, and nothing has gone wrong thus far.
*  NHDU is the current HDU number.
         DO WHILE ( LOOP
     :              .AND. ( FSTAT .EQ. FITSOK )
     :              .AND. ( STATUS .EQ. SAI__OK ) )

*  End loop after one if extensions are not required.
            IF ( .NOT. PROEXT ) LOOP = .FALSE.

*  Flag if it's the Primary HDU.
            PRMRY = NHDU .EQ. 0

*  Inherit headers if required.  We need to do it here in case EXTABLE
*  specifies an inherited keyword FUNITD becomes the unit number of the
*  current HDU and FUNITH that of the merged header.
            FUNITD = FUNITH
            IF ( .NOT. PRMRY ) THEN
               CALL COF_MRGHD( FUNITD, FILNAM, FUNITH, STATUS )
               MERGED = .TRUE.
            END IF

*  Assume the NDF will need resizing.  This will be overridden for
*  EXTABLE handling where components other than the first are being
*  added.
            NEWNDF = .TRUE.

*  If an EXTABLE was specified, see if this HDU is required.
            IF ( EXTABL ) THEN
               CALL COF_NDXT( FUNITH, NHDU, NEEDED, IEXT, ICMP, LOOP,
     :                        STATUS )
               IF ( .NOT. NEEDED ) GOTO 888
            END IF

*  Obtain the type of the current extension.
            IF ( .NOT. PRMRY ) THEN
               CALL FTGKYS( FUNITD, 'XTENSION', XTENS, COMENT, FSTAT )
               IF ( FSTAT .NE. FITSOK ) THEN
                  BUFFER =  'Error obtaining the extension name from '/
     :                      /'FITS file '//FILNAM(1:NCF )//'.'
                  CALL COF_FIOER( FSTAT, 'COF_F2NDF_GKYS', 'FTGKYS',
     :                            BUFFER, STATUS )
                  GOTO 999

               END IF

*  Some binary tables are compressed images, check for that.
               CALL FTGHDT( FUNITD, HDUTYP, FSTAT )
               IF ( XTENS .EQ. 'BINTABLE' .AND. HDUTYP .EQ. 0 ) THEN
                  XTENS = 'IMAGE'
               END IF
            END IF


*  Select type of HDU.
*  ===================
*
*  There is support for basic FITS, which creates a typical NDF; the
*  IMAGE extension, which creates an NDF within an extension of the
*  original NDF; BINTABLE and TABLE create <TABLE> type extension
*  within the NDF; and random groups, which creates a series of NDFs in
*  the container file of the supplied NDF.

*  Test for a primary HDU or an IMAGE extension.  Note that this
*  includes random groups as these must be defined in the primary HDU.
            IF ( PRMRY. OR. (XTENS .EQ. 'IMAGE') ) THEN

*  Determine the main properties of the FITS object.
*  =================================================

*  Get the mandatory headers of the primary HDU or an IMAGE extension.
               CALL COF_MANDH( FUNITD, PRMRY, NDF__MXDIM, SIMPLE,
     :                         BITPIX, NDIM, DIMS, PCOUNT, GCOUNT,
     :                         EXTEND, DARRAY, NONSDA, EL, STATUS )

*  Report the error context.
               IF ( STATUS .NE. SAI__OK ) THEN
                  BUFFER = 'FITS file '//FILNAM(1:NCF )//'.'
                  CALL MSG_SETI( 'N', NHDU )
                  CALL MSG_SETC( 'NTH', CHR_NTH( NHDU ) )
                  CALL MSG_SETC( 'BUF', BUFFER )
                  CALL ERR_REP( 'COF_F2NDF_MANDH',
     :              'Error occurred during accessing headers in the '/
     :              /'^N^NTH header and data unit of ^BUF', STATUS )
                  GOTO 999
               END IF

*  Cannot process non-standard files.
               IF ( PRMRY .AND. .NOT. SIMPLE ) THEN
                  STATUS = SAI__ERROR
                  BUFFER = 'The FITS file '//FILNAM(1:NCF )//' is not'
                  CALL MSG_SETC( 'BUF', BUFFER )
                  CALL ERR_REP( 'COF_F2NDF_NOTSIM',
     :              '^BUF simple and therefore cannot be processed.',
     :              STATUS )
                  GOTO 999
               END IF

*  Data scaling.
*  =============
*  Check that the HDU has a data array---otherwise setting the scaling
*  fails.  COF_DOSCL sets the scale factors from the BSCALE and BZERO
*  keywords, and returns the data type based upon the precision of the
*  keywords, or a null value if either BSCALE or BZERO is absent, or
*  there is no format conversion.
               IF ( DARRAY ) THEN
                  CALL COF_DOSCL( FUNITH, FUNITD, FMTCVT, TYPE, STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL MSG_SETC( 'FILE', FILNAM(1:NCF) )
                     CALL ERR_REP( 'COF_F2NDF_SCL',
     :                 'for FITS file ^FILE.', STATUS )
                     GO TO 999
                  END IF
               ELSE
                  TYPE = ' '
               END IF

*  Override the type, but retain scaling.  This might be a problem if
*  a one- or two-byte integer type is selected.  On the other hand the
*  precision of BSCALE and BZERO may be spurious _DOUBLE.  Warn the user
*  if there is a potential loss of precision or dynamic range.
               IF ( UTYPE .NE. ' ' ) THEN
                  IF ( TYPE .NE. ' ' ) THEN
                     CALL KPG_TYPSZ( TYPE, NBFTYP, STATUS )
                     CALL KPG_TYPSZ( UTYPE, NBUTYP, STATUS )
                     IF ( NBUTYP .LT. NBFTYP .OR.
     :                    ( TYPE .EQ. '_REAL' .AND.
     :                      UTYPE .EQ. '_INTEGER' ) ) THEN
                        CALL MSG_SETC( 'UT', UTYPE )
                        CALL MSG_SETC( 'FT', TYPE )
                        CALL MSG_OUTIF( MSG__NORM, 'COF_F2NDF_PREC',
     :                    'The chosen data ^UT type may result in a '/
     :                    /'loss of precision and dynamic range.  The '/
     :                    /'FITS file suggests type ^FT.', STATUS )
                     END IF
                  END IF

                  TYPE = UTYPE
               END IF

*  Former NDF?
*  ===========
*
*  First see if the primary data or IMAGE extension data originated in
*  an NDF array component.  If it is, we want to replace it rather than
*  create an extension.  A bad status can be ignored, as the presence
*  flag will be false.
               CALL ERR_MARK
               CALL COF_GKEYC( FUNITH, 'HDUCLAS1', HDUPRE, HDUCLA,
     :                         COMENT, STATUS )
               IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
               CALL ERR_RLSE

*  Define whether or not the extension came from an NDF.
               EXNDF = HDUPRE .AND. HDUCLA .EQ. 'NDF'

*  Later data from multi-NDF container files will cause failure if the 
*  user has forgotten to set the CONTAINER flag.  An attempt to put
*  these into an NDF extension fails because there will be no EXTNAME
*  keyword present.  These can be detected by the presence of the
*  HDSNAME and HDSTYPE keywords.
               CALL ERR_MARK
               CALL COF_GKEYC( FUNITH, 'HDSNAME', NAMPRE, HDSNAM,
     :                         COMENT, STATUS )
               IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
               CALL ERR_RLSE

               CALL ERR_MARK
               CALL COF_GKEYC( FUNITH, 'HDSTYPE', TYPPRE, HDSTYP,
     :                         COMENT, STATUS )
               IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
               CALL ERR_RLSE

*  For ordinary files the first (and only) value of the the "group"
*  counter is zero.  This is what certain FITSIO routines expect.
               GCMIN = 0

*  Set MULTIP if we want to create a series of NDFs in the container
*  file.  For random groups, when the number is one, then a normal NDF
*  can be created.
               MULTIP = CONTNR
     :                  .OR. ( GCOUNT .GT. 1 .AND. NONSDA )
     :                  .OR. EXTABL
               IF ( MULTIP ) THEN

*  We want a series of NDFs in the container file.  Get a locator for
*  the dummy NDF already created.
                  CALL NDF_LOC( NDF, 'WRITE', PLOC, STATUS )

*  Delete any existing dummy data array, unless the NDF is to be at the
*  top level.
                  IF ( CONTNR .OR.
     :                .NOT. (EXTABL .AND. ( NDFNMS(1) .EQ. '*' )) ) THEN

                     CALL DAT_THERE( PLOC, 'DATA_ARRAY', THERE, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        IF ( THERE )
     :                     CALL DAT_ERASE( PLOC, 'DATA_ARRAY', STATUS )
                     END IF

*  Re-type the container file for EXTABLE output unless we think it
*  originated from a UKIRT_HDS file.  The latter is a kludge and ought 
*  to be handled as a Special Format in a UKIRT-specific routine.  It
*  assumes that the first proper NDF is called I1 rather than I<n>, for
*  integer n other than 1.
                     IF ( CONTNR .OR. EXTABL ) THEN
                        IF ( HDSNAM .EQ. 'I1' .AND.
     :                       HDSTYP .EQ. 'NDF' ) THEN
                           CALL DAT_RETYP( PLOC, 'UKIRT_HDS', STATUS )
                        ELSE
                           CALL DAT_RETYP( PLOC, 'NDF_CONTAINER',
     :                                     STATUS )
                        END IF
                     END IF
                  END IF

*  Set the minimum group counter.
                  GCMIN = 1

*  After a random-group, it is not clear where any extensions should
*  go, so ensure that the loop ends.  A random-group dataset with other
*  FITS extensions attached is extremely unlikely to cause difficulties
*  given that random-groups were hardly used anyway, almost entirely in
*  the radio-astronomy community, who then developed the original binary
*  tables long before FITS extensions were ratified, and switched to
*  that more-efficient data structure.
*
*  However if it's MULTIP because of an EXTABLE do not end loop.
!                  IF ( .NOT. EXTABL ) LOOP = .FALSE.
                  IF ( NONSDA ) LOOP = .FALSE.

               ELSE
*  Prepare for a simple and 1-element random-groups primary HDU, or an
*  IMAGE extension as an NDF extension.

*  Ensure that the number of group NDFs to process is 0 unless it is a
*  single group random-groups FITS file (it is unlikely but not
*  impossible).
                  GCOUNT = 0
                  IF ( NONSDA ) GCOUNT = 1

*  Copy the NDF identifier to the temporary clone.
                  IF ( FIRST ) THEN
                     NDFE = NDF

*  By definition the NDF must have a Data component.
                     COMP = 'DATA'

*  Do we need to create an extension which is an NDF?
*  ==================================================
                  ELSE IF ( XTENS .EQ. 'IMAGE' ) THEN

*  First see if the IMAGE extension data originated in an NDF array
*  component.  If it is, we want to replace it rather than create an
*  extension.
                     IF ( EXNDF ) THEN

*  Obtain the component.
                        CALL ERR_MARK
                        CALL COF_GKEYC( FUNITH, 'HDUCLAS2', HDUPRE,
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
                           COMP ='DATA'
                        END IF
                     ELSE
                        COMP ='DATA'
                     END IF

                     IF ( COMP .EQ. 'DATA' ) THEN
                        IF ( .NOT. EXNDF ) THEN

*  Generate the name of the extension.  NPOS is updated so cannot be
*  defined outside the FITS_extension loop.
                           EXTNAM = 'FITS_EXT_'
                           NPOS = 9
                           CALL CHR_PUTI( NHDU, EXTNAM, NPOS )

*  Create an extension of type NDF.
                           CALL NDF_XNEW( NDF, EXTNAM, 'NDF', 0, 0,
     :                                    XLOC, STATUS )

*  Find the parent structure (i.e. .MORE).
                           CALL DAT_PAREN( XLOC, ELOC, STATUS )

*  Create a new NDF in the extension via an NDF placeholder.  The data
*  type and bounds will be changed below once they are known.
                           CALL NDF_PLACE( ELOC, EXTNAM, PLACE, STATUS )
                           CALL NDF_NEW( '_UBYTE', 1, 1, 1, PLACE, NDFE,
     :                                   STATUS )

*  Want to recreate the path if this IMAGE sub-file originally was an
*  NDF within an NDF extension.
                        ELSE
                           CALL COF_FI2NE( FUNITD, NDF, NDFE, STATUS )
                        END IF  ! COMP eq DATA +/- EXNDF
                     END IF
                  END IF  ! Not FIRST and XTENS eq IMAGE

               END IF  ! Not MULTIP

*  Loop for each group.
*  ====================

*  The group count is defined by keyword GCOUNT in the FITS header.
*  The dummy NDF becomes the zeroth NDF for a random-groups FITS file.
               DO IGROUP = GCMIN, GCOUNT

*  Create a NDF for a dataset.
*  ===========================

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
                     COMP = 'DATA'

                  ELSE IF ( EXTABL ) THEN

*  We have an EXTABLE; each extension set produces an NDF in the
*  container file. ICMP and IEXT point to the component and extension
*  set respectively in the EXTABLE arrays.

*  Get the name of the component NDF, as set by COF_EXTAB.
                     NDFNAM = NDFNMS( IEXT )

*  If NDFNAM(1) is * we are to use the top-level NDF.  Table parsing
*  will have checked that there is only one extension set.
                     IF ( NDFNAM .EQ. '*' ) THEN
                        CALL NDF_CLONE( NDF, NDFS(1), STATUS )
                        CALL NDF_CLONE( NDF, NDFE, STATUS )

                     ELSE

*  We require a component NDF.  If the NDF isn't there already, create a
*  new NDF in the container file via an NDF placeholder from the
*  top-level locator.  The data type and bounds will be changed below
*  once they are known.  Save the NDF pointer in an array for re-use.
                        CALL DAT_THERE( PLOC, NDFNAM, THERE, STATUS )
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           IF ( .NOT. THERE ) THEN
                              CALL NDF_PLACE( PLOC, NDFNAM, PLACE,
     :                                        STATUS )
                              CALL NDF_NEW( '_UBYTE', 1, 1, 1, PLACE,
     :                                      NDFE, STATUS )
                              CALL NDF_CLONE( NDFE, NDFS( IEXT ),
     :                                        STATUS )

                           ELSE
                              CALL CMP_TYPE( PLOC, NDFNAM, CTYPE,
     :                                       STATUS )

                              IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :                             ( CTYPE .NE. 'NDF' ) ) THEN
                                 STATUS = SAI__ERROR
                                 CALL ERR_REP( 'COF_F2NDF_CMPER1',
     :                             'Error in EXTABLE.', STATUS )
                                 CALL MSG_SETC( 'NDF', NDFNMS(IEXT) )
                                 CALL ERR_REP( 'COF_F2NDF_CMPER2',
     :                            'Attempt to use a FITS HDU with no '/
     :                            /'data array in constructing ^NDF.',
     :                            STATUS )

                              ELSE

*  We require to add a component to an existing NDF.
                                 NEWNDF = .FALSE.
                                 CALL NDF_CLONE( NDFS( IEXT ), NDFE,
     :                                           STATUS )
                              END IF
                           END IF
                        END IF
                     END IF

*  Get the required component name.
                     COMP = COMPS( ICMP )

*  If it's an extension, create the NDF extension name.
                     IF ( COMP( 1:4 ) .EQ. 'EXTN' ) THEN
                        CALL COF_GXTNM( NHDU, COMP, EXTNAM, STATUS )

                        COMP = 'DATA'

*  Create an extension of type NDF.
                        CALL NDF_XNEW( NDFE, EXTNAM, 'NDF', 0,
     :                                 0, XLOC, STATUS )

*  Find the parent structure (i.e. .MORE).
                        CALL DAT_PAREN( XLOC, ELOC, STATUS )

*  Create a new NDF in the extension via an NDF placeholder.  The data
*  type and bounds will be changed below once they are known.
                        CALL NDF_PLACE( ELOC, EXTNAM, PLACE, STATUS )
                        CALL NDF_ANNUL( NDFE, STATUS )
                        CALL NDF_NEW( '_UBYTE', 1, 1, 1, PLACE, NDFE,
     :                                STATUS )
                        NEWNDF = .TRUE.
                     END IF  ! An NDF extension is required

                  ELSE IF ( CONTNR ) THEN

*  We are forcing a series of NDFs in a container file by using the
*  CONTAINER parameter.
                     
*  The NDFs names may be stored in HDSNAME obtained earlier.
                     IF ( NAMPRE ) THEN
                        NDFNAM = HDSNAM
                     ELSE

*  Generate the name of the component NDF (HDU_n).  NPOS is updated so
*  cannot be defined outside the FITS_extension loop.
                        NDFNAM = 'HDU_'
                        NPOS = 4
                        CALL CHR_PUTI( NHDU, NDFNAM, NPOS )
                     END IF

*  Create a new NDF in the container file via an NDF placeholder from
*  the top-level locator.  The data type and bounds will be changed
*  below once they are known.
                     CALL NDF_PLACE( PLOC, NDFNAM, PLACE, STATUS )
                     CALL NDF_NEW( '_UBYTE', 1, 1, 1, PLACE, NDFE,
     :                             STATUS )

*  By definition the extension NDF must have a Data component.
                     COMP = 'DATA'

                  END IF  ! Create NDF in correct place

*  Report the full set of headers and/or write to NDF's FITS extension.
*  ====================================================================
*
*  This is slightly less efficient than combining the two operations,
*  but re-using subroutines does make the code easier to follow.  There
*  is another factor.  This stage incorporates the recreation of NDF
*  HISTORY records, which should not be propagated to the FITS airlock
*  too.  This intertwined steps requires a set of flags, but the logging
*  does not.

*  Decide whether or not to save the headers in an extension.  Exclude
*  the zeroth group of a random-groups dataset.
                  IF ( EXTABL ) THEN
                     WRTEXT = ( COMP .EQ. 'DATA' ) .AND. PROFIT
                  ELSE
                     WRTEXT = ( ( PRMRY .AND. .NOT. MULTIP ) .OR.
     :                          ( XTENS .EQ. 'IMAGE' .AND.
     :                            .NOT. EXNDF ) .OR.
     :                          ( IGROUP .GT .0 ) )
     :                        .AND. PROFIT
                  END IF

*  Decide when to access the headers.
                  IF ( WRTEXT .OR.
     :                 ( EXNDF .AND. COMP .EQ. 'DATA' ) ) THEN

*  Instruct that the FITS airlock is not to use all the headers
*  regardless whether or not they are NDF-style HISTORY records.  This
*  uses a logical work array to flag whether or not to propagate the
*  header to the airlock.

*  Find the number of FITS headers.  Check that nothing has gone wrong
*  before using the the number to create workspace.
                     CALL COF_NHEAD( FUNITH, FILNAM, NHEAD, STATUS )
                     IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Create the mask and assign .TRUE. to all of its elements.  Allocate
*  one more than the number given by COF_NHEAD in case there is a
*  blank after the last HISTORY record and before the END.  (CFITSIO
*  FTGHSP doesn't count END or preceding blanks.)
                     CALL PSX_CALLOC( NHEAD+1, '_LOGICAL', REPNTR,
     :                                STATUS )
                     CALL CON_CONSL( .TRUE., NHEAD+1,
     :                               %VAL( CNF_PVAL( REPNTR ) ),
     :                               STATUS )
                  END IF

*  Deal with the NDF-style history records in the headers, assuming the
*  history records have not been tampered.  Search for such records,
*  transfer their information back into NDF HISTORY records, and flag
*  that these headers should not to be propagated to the FITS airlock.
*  This is to avoid growing duplication of potentially bulky text, if
*  using FITS files with Starlink tasks.
                  IF ( EXNDF .AND. COMP .EQ. 'DATA' ) THEN
                     CALL COF_CHISR( FUNITH, NDFE, NHEAD,
     :                               %VAL( CNF_PVAL( REPNTR ) ),
     :                               STATUS )
                  END IF

*  Read the main header into the FITS extension of the NDF.  The FITS
*  headers for the random groups will appear in each group NDF.
                  IF ( WRTEXT ) THEN
                     IF ( MERGED ) NHEAD = NHEAD - 1
                     CALL COF_WFEXF( FUNITH, NDFE, IGROUP, PCOUNT,
     :                               FILNAM, NHEAD,
     :                               %VAL( CNF_PVAL( REPNTR ) ),
     :                               STATUS )
                  END IF

*  Free the work space.
                  IF ( WRTEXT .OR. ( EXNDF .AND. COMP .EQ. 'DATA' ) )
     :               CALL PSX_FREE( REPNTR, STATUS )

*  Write out the headers to a logfile, if desired.
                  IF ( LOGHDR )
     :               CALL COF_HDLOG( FUNITH, FDL, FILNAM, NHDU, STATUS )

*  Modify the shape and type of the NDF, now that it is known.
*  ===========================================================

*  Test whether or not there is a data array present.  The zeroth group
*  is a simple array and is merely filled with dummy data, so test for
*  this.
                  IF ( DARRAY .AND. ( IGROUP .GT. 0 .OR.
     :                 ( IGROUP .EQ. 0 .AND. .NOT. NONSDA ) ) ) THEN
                     CALL COF_STYPE( NDFE, COMP, TYPE, BITPIX, FMTCVT,
     :                               ITYPE, STATUS )

*  Specify the bounds of the NDF array component unless it has already
*  been done, in which case check for compatibility.  Note use FUNITD
*  not FUNITH, these could be compressed images, which have to be
*  accessed using the original headers, not merged ones.
                     IF ( NEWNDF ) THEN
                        CALL COF_SBND( FUNITD, NDFE, 'LBOUND', .FALSE.,
     :                                 STATUS )
                     ELSE
                        CALL COF_SBND( FUNITD, NDFE, 'LBOUND', .TRUE.,
     :                                 STATUS )
                        IF ( STATUS .NE. SAI__OK ) THEN
                           CALL MSG_SETC( 'NDF', NDFNAM )
                           CALL ERR_REP( 'COF_F2NDF_BNDER',
     :                       'Error creating component ^NDF.',
     :                       STATUS )
                        END IF
                     END IF

*  Copy the data values into the array component.
*  ==============================================

*  First map the input array component with the desired data type.  Any
*  type conversion will be performed by the FITSIO array-reading
*  routine.
                     CALL NDF_MAP( NDFE, COMP, ITYPE, 'WRITE', PNTR, EL,
     :                             STATUS )
                     IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Call the appropriate routine for the data type of the created array.
*  The group is 0 for simple FITS or the group identification
*  otherwise.  We always start at the first element.  The arrays may
*  have bad pixels.
                     IF ( ITYPE .EQ. '_UBYTE' ) THEN
                        CALL FTGPVB( FUNITD, IGROUP, 1, EL, VAL__BADUB,
     :                               %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD,
     :                               FSTAT )

                     ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                        CALL FTGPVI( FUNITD, IGROUP, 1, EL, VAL__BADW,
     :                               %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD,
     :                               FSTAT )

                     ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                        CALL FTGPVJ( FUNITD, IGROUP, 1, EL, VAL__BADI,
     :                               %VAL( CNF_PVAL( PNTR(1) ) ), BAD,
     :                               FSTAT )

                     ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
                        CALL FTGPVE( FUNITD, IGROUP, 1, EL, VAL__BADR,
     :                               %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD,
     :                               FSTAT )

                     ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                        CALL FTGPVD( FUNITD, IGROUP, 1, EL, VAL__BADD,
     :                               %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD,
     :                               FSTAT )

                     END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
                     IF ( FSTAT .GT. FITSOK ) THEN
                        NC = CHR_LEN( COMP )
                        BUFFER = 'Error reading the '//COMP( :NC )/
     :                           /' array in FITS file '/
     :                           /FILNAM(1:NCF )//'.'
                        CALL COF_FIOER( FSTAT, 'COF_F2NDF_READ',
     :                    'FTGPVx', BUFFER, STATUS )
                        CALL NDF_UNMAP( NDFE, COMP, STATUS )
                        GOTO 999
                     END IF

*  Set the bad-pixel flag.
                     IF ( COMP .NE. 'QUALITY' )
     :                 CALL NDF_SBAD( BAD, NDFE, COMP, STATUS )

*  The data array is only a dummy, so fill it with bad values by
*  mapping with the appropriate initialisation.  The values will be
*  returned to the output NDF when the array component is unmapped.
                  ELSE
                     CALL NDF_MAP( NDFE, COMP, '_REAL', 'WRITE/BAD',
     :                             PNTR, EL, STATUS )

                  END IF

*  Unmap the array.
                  CALL NDF_UNMAP( NDFE, COMP, STATUS )

*  Store scale factors in the NDF array's native form.
                  IF ( NATIVE ) THEN
                     CALL COF_STSCL( FUNITH, NDFE, COMP, STYPE, STATUS )
                  END IF

*  If we are creating a MULTIP other than a Random Groups file, a header
*  without data either becomes a) an HDS structure of type FITS_HEADER, 
*  containing the FITS header as a _CHARACTER array; or b) when the
*  HEADER was an NDF is restored as a HEADER.
                  ISNDF = .TRUE.
                  IF ( .NOT. DARRAY
     :                 .AND. ( MULTIP .AND. .NOT. NONSDA )
     :                 .AND. HDSTYP .NE. 'NDF' ) THEN
                     ISNDF = .FALSE.

*  Copy the FITS airlock to the top level.
                     CALL NDF_XSTAT( NDFE, 'FITS', THERE, STATUS )
                     IF ( ( STATUS .EQ. SAI__OK )
     :                     .AND. THERE ) THEN

*  Get the name of the NDF and a locator to its parent.
                        CALL NDF_LOC( NDFE, 'UPDATE', ELOC, STATUS )
                        CALL DAT_NAME( ELOC, NDFNAM, STATUS )
                        CALL DAT_PAREN( ELOC, PLOC, STATUS )
                        CALL DAT_ANNUL( ELOC, STATUS )

*  Create component of type FITS_HEADER to replace the NDF.
                        CALL DAT_NEW( PLOC, 'TMPHDR', 'FITS_HEADER',
     :                                0, 0, STATUS )
                        CALL DAT_FIND( PLOC, 'TMPHDR', ALOC, STATUS )
                        CALL DAT_ANNUL( PLOC, STATUS )

*  Get a locator to the NDF's FITS airlock...
                        CALL NDF_XLOC( NDFE, 'FITS', 'READ', XLOC,
     :                                 STATUS )

*  and move it to the FITS_HEADER component.  This also annuls XLOC.
                        CALL DAT_MOVE( XLOC, ALOC, 'FITS', STATUS )

                     END IF

*  Delete the NDF and rename the airlock component.
                     CALL NDF_DELET( NDFE, STATUS )
                     CALL DAT_VALID( ALOC, VALID, STATUS )
                     IF ( VALID ) THEN
                        CALL DAT_RENAM( ALOC, NDFNAM, STATUS )
                        CALL DAT_ANNUL( ALOC, STATUS )
                     END IF

                  END IF

                  IF ( ISNDF ) THEN

*  Other components.
*  =================
*  Only need to define the other components once for an NDF.
                     IF ( COMP .EQ. 'DATA' ) THEN

*  Create the NDF character components from the FITS headers.
                        CALL COF_NDFCC( FUNITH, NDFE, STATUS )

*  Create the NDF AXIS structure from the FITS headers.
                        IF ( TOAXIS )
     :                    CALL COF_NDFAX( FUNITH, NDFE, STATUS )

*  Create the AST World Coordinate information from the FITS headers.
                        IF ( NDIM .GT. 0 .AND. TOWCS )
     :                    CALL COF_FTWCS( FUNITH, NDFE, NENCOD, ENCODS,
     :                                    FILNAM, WCSATT, STATUS )

*  Annul the temporary NDF, and tidy the locator to the extension.
                        IF ( NONSDA .AND. IGROUP .GT. 0 ) THEN
                           CALL NDF_ANNUL( NDFE, STATUS )

*  Tidy the extension NDF and the locators used to create it.
                        ELSE IF ( XTENS .EQ. 'IMAGE' ) THEN
                           CALL NDF_ANNUL( NDFE, STATUS )

*  The extension locators are not necessarily used for the EXTABL.
                           CALL DAT_VALID( XLOC, VALID, STATUS )
                           IF ( VALID ) THEN
                              CALL DAT_ANNUL( XLOC, STATUS )
                              CALL DAT_ANNUL( ELOC, STATUS )
                           END IF
                        END IF
                     END IF  ! DATA component
                  END IF ! It is an NDF
               END DO  ! Each group or single standard HDU

*  Binary or ASCII Table.
*  ======================
            ELSE IF ( XTENS .EQ. 'BINTABLE' .OR.
     :                XTENS .EQ. 'TABLE' ) THEN

*  Test whether the FITS file came from an NDF.  If it does, propagate
*  the FITS table to the NDF extension.
               IF ( EXNDF ) THEN
                  CALL COF_FT2NE( FUNITH, NDF, STATUS )

               ELSE

*  Not ex-NDF.  Generate the name of the extension.
                  IF ( CONTNR ) THEN
                     EXTNAM = 'HDU_'
                     NPOS = 4
                     CALL CHR_PUTI( NHDU, EXTNAM, NPOS )

*  Get a locator for the dummy NDF already created.
                     CALL NDF_LOC( NDF, 'WRITE', PLOC, STATUS )

*  Delete any existing dummy data array and re-type the container file
*  (assume it's already done if there is no data array).
                     CALL DAT_THERE( PLOC, 'DATA_ARRAY', THERE, STATUS )
                     IF ( ( STATUS .EQ. SAI__OK ) .AND. THERE ) THEN
                        CALL DAT_ERASE( PLOC, 'DATA_ARRAY', STATUS )
                        CALL DAT_RETYP( PLOC, 'NDF_CONTAINER', STATUS )
                     END IF

*  Create a component to hold the binary table.
                     CALL DAT_NEW( PLOC, EXTNAM, 'TABLE', 0, 0, STATUS )
                     CALL DAT_FIND( PLOC, EXTNAM, XLOC, STATUS )
                     CALL DAT_ANNUL( PLOC, STATUS )

                  ELSE
                     IF ( EXTABL ) THEN

*  Get the associated EXTABLE component name.
                        COMP = COMPS( ICMP )
                     ELSE

*  Otherwise use the default FITS_EXT_n.
                        COMP = 'EXTN'
                     END IF

*  If it's an extension, create the NDF extension name.
*  Tables must be associated with NDF extensions.
                     IF ( COMP( 1:4 ) .EQ. 'EXTN' ) THEN
                        CALL COF_GXTNM( NHDU, COMP, EXTNAM, STATUS )
                     ELSE
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'NHDU', NHDU )
                        CALL ERR_REP( 'COF_F2NDF_INV',
     :                    'FITS extension ^NHDU: table not '/
     :                    /'associated with NDF extension.', STATUS )
                        GOTO 999
                     END IF

*  Create a table extension.
                     CALL NDF_XNEW( NDF, EXTNAM, 'TABLE', 0, 0, XLOC,
     :                              STATUS )

                  END IF

*  Call routine to create the <TABLE> structure from the FITS binary
*  or ASCII table.
                  CALL COF_WRTAB( FUNITH, FUNITD, XLOC, STATUS )

*  Tidy the locator to the extension.
                  CALL DAT_ANNUL( XLOC, STATUS )

               END IF  ! Not ex-NDF

            END IF  ! Binary or ASCII table

*  Clean up FITS units---if a merged header was in use, delete it and
*  restore the original FUNITH value.
            IF ( FUNITH .NE. FUNITD ) THEN
               FSTATC = FITSOK
               CALL FTCLOS( FUNITH, FSTATC )
               FUNITH = FUNITD
            END IF

*  Get next HDU if required
  888       CONTINUE

*  Prepare for next FITS extension if more required
            IF ( LOOP ) THEN

*  Increment the count of the header-and-data units.
               NHDU = NHDU + 1

*  Flag that we are no longer on the first HDU
               FIRST = .FALSE.

*  Skip to the next HDU.
               CALL FTMRHD( FUNITH, 1, HDUTYP, FSTAT )
               IF ( FSTAT .NE. FITSOK ) THEN

*  Report the error.  If it is the expected end of file (error 107)
*  or has some garbage at the end (unrecognisable FITS record, error
*  252),  just annul the error and remove the FITSIO error messages
*  from the stack.  If it is the end of the tape, report if there are
*  still requested extensions to find.
                  IF ( FSTAT .EQ. 107 .OR. FSTAT .EQ. 252 ) THEN
                     CALL FTCMSG
                     FSTAT = FITSOK
                     IF ( EXTABL ) THEN

*  Report any extensions requested in EXTABLE but not found.
                        CALL COF_EXTNF( FILNAM, STATUS )
                     END IF  ! EXTABL at end of file

                  ELSE  ! Plain error - not EOF or rubbish on end
                     BUFFER = 'of the FITS file '//FILNAM(1:NCF )//'.'
                     CALL MSG_SETI( 'N', NHDU )
                     CALL MSG_SETC( 'NTH', CHR_NTH( NHDU ) )
                     CALL MSG_SETC( 'BUF', BUFFER )

                     STATUS = SAI__ERROR
                     CALL ERR_REP( 'COF_F2NDF_WREXT',
     :                 'Error skipping to the ^N^NTH extension ^BUF',
     :                 STATUS )
                     CALL COF_FIOER( FSTAT, 'COF_F2NDF_WREXT', 'FTMRHD',
     :                 ' ', STATUS )
                  END IF  ! Plain error
                  GOTO 999
               END IF  ! FITSIO error moving to next HDU
            END IF  ! LOOP still TRUE - more extensions require

         END DO  ! Each FITS extension while more required and FITS OK

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
      ERRMSG = ' '
      DO WHILE ( LOOP )
         CALL FTGMSG( ERRMSG )
         LOOP = ERRMSG .NE. ' '

         IF ( LOOP ) THEN
            IF ( FIRST ) THEN
               BUFFER = 'Error processing FITS file '//FILNAM(1:NCF )/
     :                  /':'
               CALL MSG_SETC( 'BUF', BUFFER )
               CALL ERR_REP( 'COF_F2NDF_ERRH', '^BUF', STATUS )
               FIRST = .FALSE.
            END IF

            CALL ERR_REP( 'FITSIO_ERR', '   '//ERRMSG, STATUS )

         END IF
      END DO

  999 CONTINUE

      CALL NDF_END( STATUS )

*  Close the FITS file.  The inherited status used by FITSIO still
*  applies to closedown calls, so use a temporary status.
      IF ( FSTAT .GT. FITSOK ) FSTAT = FITSOK
      FSTATC = FITSOK
      CALL FTCLOS( FUNITH, FSTATC )
      IF ( FSTATC .GT. FITSOK ) THEN
         BUFFER = 'Error closing the FITS file '//FILNAM(1:NCF )//'.'
         CALL COF_FIOER( FSTATC, 'COF_F2NDF_CLOSE', 'FTCLOS', BUFFER,
     :                   STATUS )
      END IF

*  Release the logical-unit.
      CALL FIO_PUNIT( FUNITH, STATUS )

      END
