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
*     A more general facility is also provided to associate specified
*     FITS extensions with NDF components by means of entries in a text
*     file (see the EXTABLE parameter).
*
*     Details of the supported special formats and rules for processing
*     them are given in topic "Special Formats"; the general-case
*     processing rules are described in the "Notes".
*
*     FITS2NDF can also process both external and internal compressed
*     FITS files.  The external compression applies to the whole file
*     and FITS2NDF recognises gzip (.gz) and UNIX compress (.Z) formats.
*     Internal compressions are where a large image is tiled and each
*     tile is compressed.  The supported formats are Rice, the IRAF
*     PLIO, and GZIP.
*
*     Both NDF and FITS use the term extension, and they mean different
*     things.  Thus to avoid confusion in the descriptions below, the
*     term `sub-file' is used to refer to a FITS IMAGE, TABLE or
*     BINTABLE Header and Data Unit (HDU).

*  Usage:
*     fits2ndf in out

*  ADAM Parameters:
*     CONTAINER = _LOGICAL (Read)
*        If TRUE causes each HDU from the FITS file to be written as
*        a component of the HDS container file specified by the OUT
*        parameter.  Each component will be named HDU_n, where n is the
*        FITS HDU number.  The primary HDU is numbered 0.  Primary and
*        IMAGE HDUs will become NDFs and if the PROFITS parameter is
*        TRUE, each NDF's FITS extension will be created from the
*        header of the FITS sub-file.  It will have the form of a
*        primary header and may include cards inherited from the
*        primary header.  If the FITS HDU has no data array, an NDF
*        will not be created---if PROFITS is TRUE, a structure of type
*        FITS_HEADER, containing the FITS header as an array of type
*        _CHAR*80, is created; if PROFITS is FALSE, no component is
*        created.  Binary and ASCII tables become components of type
*        'TABLE', formatted as in the general rules under "Notes" below.
*        [FALSE]
*     ENCODINGS = LITERAL (Read)
*        Determines which FITS keywords should be used to define the
*        world co-ordinate systems to be stored in the NDF's WCS
*        component.  This parameter is only relevant when WCSCOMP is
*        "WCS" or "Both".  The allowed values (case-insensitive) are:
*
*        "FITS-IRAF" --- This uses keywords CRVALi CRPIXi, CDi_j, and is
*           the system commonly used by IRAF. It is described in the
*           document "World Coordinate Systems Representations Within
*           the FITS Format"  by R.J. Hanisch and D.G. Wells, 1988,
*           available by ftp from fits.cv.nrao.edu
*           /fits/documents/wcs/wcs88.ps.Z.
*
*        "FITS-WCS" --- This is the FITS standard WCS encoding scheme
*           described in the paper "Representation of celestial
*           coordinates in FITS"
*           (http://www.atnf.csiro.au/people/mcalabre/WCS/).  It is
*           very similar to "FITS-IRAF" but supports a wider range of
*           projections and co-ordinate systems.
*
*        "FITS-PC" --- This uses keywords CRVALi, CDELTi, CRPIXi,
*           PCiiijjj,  etc, as described in a previous (now superseded)
*           draft of the above FITS world co-ordinate system paper by
*           E.W.Greisen and  M.Calabretta.
*
*        "FITS-AIPS" --- This uses conventions described in the document
*           "Non-linear Coordinate Systems in AIPS" by Eric W. Greisen
*           (revised 9th September, 1994), available by ftp from
*           fits.cv.nrao.edu /fits/documents/wcs/aips27.ps.Z.  It is
*           currently employed by the AIPS data-analysis facility
*           (amongst others), so its use will facilitate data exchange
*           with AIPS.  This encoding uses CROTAi and CDELTi keywords to
*           describe axis rotation and scaling.
*
*        "FITS-AIPS++" --- This is an extension to FITS-AIPS which
*           allows the use of a wider range of celestial projections,
*           as used by the AIPS++ project.
*
*        "FITS-CLASS" --- This uses the conventions of the CLASS
*           project.  CLASS is a software package for reducing
*           single-dish radio and sub-mm spectroscopic data.  It
*           supports double-sideband spectra.  See
*           http://www.iram.fr/IRAMFR/GILDAS/doc/html/class-html/class.html.
*
*        "DSS" --- This is the system used by the Digital Sky Survey,
*           and uses keywords AMDXn, AMDYn, PLTRAH, etc.
*
*        "NATIVE" --- This is the native system used by the AST library
*           (see SUN/210) and provides a loss-free method for
*           transferring WCS information between AST-based applications.
*           It allows more complicated WCS information to be stored and
*           retrieved than any of the other encodings.
*
*        A comma-separated list of up to six values may be supplied,
*        in which case the value actually used is the first in the
*        list for which corresponding keywords can be found in the FITS
*        header.
*
*        A FITS header may contain keywords from more than one of these
*        encodings, in which case it is possible for the encodings to
*        be inconsistent with each other.  This may happen for instance
*        if an application modifies the keyword associated with one
*        encoding but fails to make equivalent modifications to the
*        others.
*
*        If a null parameter value (!) is supplied for
*        ENCODINGS, then an attempt is made to determine the most
*        reliable encoding to use as follows.  If both native and
*        non-native encodings are available, then the first non-native
*        encoding to be found which is inconsistent with the native
*        encoding is used.  If all encodings are consistent, then the
*        native encoding is used (if present).  [!]
*     EXTABLE = FILE (Read)
*        This specifies the name of a text file containing a table
*        associating sub-files from a multi-extension FITS file with
*        specific NDF components.  If the null value (!) is given for
*        EXTABLE, FITS sub-files are treated as determined by the
*        PROEXTS parameter (see below).
*
*        An EXTABLE file contains records which may be:
*
*        'component specifier records', which associate FITS sub-files
*          with NDF components;
*        'NDFNAMES records', which specify the names of the NDFs to be
*          created. Normally they will be created within the top-level
*          HDS container file specified by the OUT parameter;
*        'directive records', which inform the table file parser.
*
*        Spaces are allowed between elements within records and blank
*        records are ignored.
*
*        Component specifier records have the form:
*
*          component; sub-file_specifiers; transformation_code
*
*          Where:
*           'component' (case-insensitive) specifies the NDF component
*              and is DATA, VARIANCE, QUALITY or EXTNi.name.  The
*              EXTNi.name form specifies the name 'name' of an NDF
*              extension to be created.  'name' may be omitted in which
*              case 'FITS_EXT_n' is assumed,  where n is the FITS
*              sub-file number. 'i' compries any characters and may be
*              omitted; it serves to differentiate component specifiers
*              where the default name is to be used.
*           'sub-file_specifiers' is a list of FITS sub-file specifiers,
*              separated by commas. The nth sub-file specifier from each
*              component specifier record forms an 'sub-file  set' and
*              each sub-file set will be used to create one NDF in the
*              output file.
*
*              Each sub-file specifier may be:
*              1. An integer specifying the FITS Header and Data Unit
*                 (HDU) number.  The primary HDU number is 0.
*              2. keyword=value (case-insensitive), specifying a FITS
*                 HDU where the specified keyword has the specified
*                 value, e.g. EXTNAME=IM2.  The 'keyword=' may be
*                 omitted, in which case EXTNAME is assumed.
*                 Multiple keyword=value pairs separated by commas and
*                 enclosed in [] may be given as a single sub-file
*                 specifier.  All the given keywords must match the
*                 sub-file header values.
*              3. Omitted to indicate that the component is not required
*                 for the corresponding NDF.  (Commas may be needed to
*                 maintain correct sub-file set alignment for later
*                 sub-file specifiers.)  If the last character of
*                 'sub-file_specifiers' is comma, it indicates an
*                 omitted specifier at the end.  Note that if a sub-file
*                 is not specified for the DATA component of an NDF, an
*                 error will be reported at closedown.
*           'transformation_code' (case-insensitive) is a character
*              string specifying a  transformation to be applied to the
*              FITS data before it is written into the NDF component.
*              The code and preceding ";" may be omitted in which case
*              "NONE" (no transformation) is assumed.  Currently the
*              only permitted code is "NONE".
*          There may be more than one component specifier record for a
*          given component, the sub-file specifiers will be
*          concatenated.  A sub-file specifier may not span records and
*          only the transformation code specified by the last record
*          for the component will be effective.
*
*        An NDFNAMES record has the format:
*          NDFNAMES name_list
*           Where name_list is a list of names for the NDFs to be
*           created, one for each sub-file set specified by the
*           component specifier lines.  The names are separated by
*           commas.  If any of the names are omitted, the last name
*           specified is assumed to be a root name to which an integer
*           counter is to be added until a new name is found.  If no
*           names are specified, 'EXTN_SET' is used as the root name.
*           For example, NDFNAMES NDF,,SET_ would result in NDFs named
*           NDF1, NDF2, SET_1, SET_2 etc. up to the given number of
*           sub-file sets.
*
*           There may be multiple NDFNAMES records, the names will be
*           concatenated.  A name may not span records and a comma as
*           the last non-blank character indicates an omitted name.
*
*           If there is only one sub-file set, the name_list may be '*',
*           in which case the NDF will be created at the top level of
*           the output file.
*
*        Directive records have # in column 1 and will generally be
*           treated as comments and ignored.  An exception is a record
*           starting with '#END', which may optionally be used to
*           terminate the file.
*
*        Each HDU of the FITS file is processed in turn.  If it matches
*        on of the sub-file specifiers in the table, it is used to
*        create the specified component of the appropriate NDF in the
*        output file; otherwise the next HDU is processed.  The table is
*        searched in sub-file set order.  If a table entry is matched it
*        is removed from the table; this means that the same FITS
*        sub-file specifier may be repeated for another NDF component
*        but each FITS HDU can only be used once.  If sub-file
*        specifiers remain unmatched at the end, a warning message is
*        displayed.
*
*        A simple example of an EXTABLE is:
*
*          # A simple example
*          DATA;0,1,2,3,4,5,6
*          #END
*
*        The primary HDU and sub-files 1--6 of the FITS file will be
*        written as the DATA components of NDFs EXTN_SET1--EXTN_SET7
*        within the HDS container file specified by the OUT parameter.
*
*        A contrived example,showing more of the facilities, is:
*
*          # A contrived example
*          NDFNAMES obs_
*          DATA; 1, EXTNAME=IM4, IM7; none
*          VARIANCE; 2,im5, im8
*          EXTN.CAL;3 ,,[extname=cal_3,extver=2]
*          #END
*
*        The HDS container file specified by the OUT parameter will
*        contain three NDFs, the NDFNAMES record specifies that they
*        will be named OBS_1, OBS_2 and OBS_3.
*
*        NDF OBS_1 will have its DATA component created from the first
*        extension  (HDU 1) of the FITS file specified by the IN
*        parameter, and its VARIANCE from the second.  NDF OBS_1 will
*        have an extension named CAL created from the third FITS
*        extension.
*
*        NDF OBS_2 has DATA and VARIANCE components created from the
*        FITS sub-files whose EXTNAME keywords have the value IM4 and
*        IM5 respectively; no CAL extension is created in OBS_2.
*
*        OBS_3 DATA and VARIANCE are created from FITS sub-files named
*        IM7 and IM8 and the CAL extension from the FITS sub-file whose
*        EXTNAME and EXTVER keywords have values "CAL" and "2"
*        respectively.
*
*        In all cases, if the PROFITS parameter is TRUE, the NDF's FITS
*        extension will be created from the header of the sub-file
*        associated with the DATA component of the NDF.  It will have
*        the form of a primary header and may include cards inherited
*        from the primary header [!]
*     FMTCNV = LITERAL (Read)
*        This specifies whether or not format conversion will occur.
*        The conversion applies the values of the FITS keywords BSCALE
*        and BZERO to the FITS data to generate the "true" data values.
*        This applies to IMAGE extensions, as well as the primary data
*        array.  If BSCALE and BZERO are not given in the FITS header,
*        they are taken to be 1.0 and 0.0 respectively.
*
*        If FMTCNV="FALSE", the HDS type of the data array in the NDF
*        will be the equivalent of the FITS data format on tape (e.g.
*        BITPIX = 16 creates a _WORD array).  If FMTCNV="TRUE", the data
*        array in the NDF will be converted from the FITS data type
*        to _REAL or _DOUBLE in the NDF.
*
*        The special value FMTCNV="Native" is a variant of "FALSE",
*        that in addition creates a scaled form of NDF array, provided
*        the array values are scaled through BSCALE and/or BZERO
*        keywords (i.e. the keywords' values are not the null 1.0
*        and 0.0 respectively).  This NDF scaled array contains the
*        unscaled data values, and the scale and offset.
*
*        The actual NDF data type for FMTCNV="TRUE", and the data type
*        after applying the scale and offset for FMTCNV="NATIVE" are
*        both specified by Parameter TYPE.  However, if TYPE is a
*        blank string or null (!), then the choice of floating-point
*        data type depends on the number of significant digits
*        in the BSCALE and BZERO keywords.
*
*        FMTCNV may be a list of comma-separated values, enclosed in
*        double quotes, to be applied to each conversion in turn.  An
*        error results if more values than the number of input FITS
*        files are supplied.  If too few are given, the last value in
*        the list applied to all the conversions; thus a single value
*        is applied to all the input files.  If more than one line is
*        required to enter the information at a prompt then place a
*        "-" at the end of each line where a continuation line is
*        desired.  ["TRUE"]
*     IN = LITERAL (Read)
*        The names of the FITS-format files to be converted to NDFs.
*        It may be a list of file names or indirection specifications
*        separated by commas and enclosed in double quotes.  FITS file
*        names may include the regular expressions ("*", "?", "[a-z]"
*        etc.) but a "[]" construct at the end of the name is assumed to
*        be a sub-file specifier to specify a particular FITS sub-file
*        to be converted.  (See the description of an EXTABLE file
*        above for allowed sub-file specifiers, but note that only a
*        single keyword=value pair is allowed here.  Note also that if a
*        specifier contains a keyword=value pair, the name(s) must be
*        enclosed in double quotes.)  If you really want to have an
*        [a-z]-type regular expression at the end of the filename, you
*        can put a null sub-file specifier "[]" after it.
*
*        Indirection may occur through text files (nested up to seven
*        deep).  The indirection character is "^".  If extra prompt
*        lines are required, append the continuation character "-" to
*        the end of the line.  Comments in the indirection file begin
*        with the character "#".
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
*        OUT="x*|cal|Starlink|" replaces the string "cal" with
*        "Starlink" in any of the output names "x*".
*
*        Some of the options create a series of NDFs in the original
*        NDF, which becomes just an HDS container and no longer an NDF.
*     PROEXTS = _LOGICAL (Read)
*        This governs how any extensions within the FITS file are
*        processed in the general case.  If TRUE, any FITS-file
*        extension is propagated to the NDF as an NDF extension called
*        FITS_EXT_n, where n is the number of the extension.  If FALSE,
*        any FITS-file extensions are ignored.  The "Notes" of the
*        general conversion contain details of where and in what form
*        the various FITS-file extensions are stored in the NDF.
*
*        This parameter is ignored when the supplied FITS file is one
*        of the special formats, including one defined by an EXTABLE but
*        excluding NDF2FITS-created files, whose structure in terms of
*        multiple FITS objects is defined.  Specialist NDF extensions
*        may be created in this case.  See topic "Special Formats" for
*        details.
*
*        It is also ignored if a sub-file is specified as the IN
*        parameter, or parameter CONTAINER is TRUE.  [TRUE]
*     PROFITS = _LOGICAL (Read)
*        If TRUE, the headers of the FITS file are written to the NDF's
*        FITS extension.  If a specific FITS sub-file has been specified
*        or Parameter CONTAINER is TRUE or an EXTABLE is in use, the
*        FITS extension will appear as a primary header and may include
*        cards inherited from the primary HDU; otherwise the FITS header
*        is written verbatim.  [TRUE]
*     TYPE = LITERAL (Read)
*        The data type of the output NDF's data and variance arrays.  It
*        is normally one of the following HDS types: "_BYTE", "_WORD",
*        "_REAL", "_INTEGER", "_DOUBLE", "_UBYTE", "_UWORD"
*        corresponding to signed byte, signed word, real, integer,
*        double precision, unsigned byte, and unsigned word.  See SUN/92
*        for further details.  An unambiguous abbreviation may be given.
*        TYPE is ignored when COMP = "Quality" since the QUALITY
*        component must comprise unsigned bytes (equivalent to
*        TYPE = "_UBYTE") to be a valid NDF.  The suggested default is
*        the current value.  Note that setting TYPE may result in a loss
*        of precision, and should be used with care.
*
*        A null value (!) or blank requests that the type be propagated
*        from the FITS (using the BITPIX keyword); or if FMTCNV is
*        "TRUE", the type is either _REAL or _DOUBLE depending on the
*        precision of the BSCALE and BZERO keywords.
*
*        TYPE may be a list of comma-separated values enclosed in
*        double quotes, that are applied to each conversion in turn.  An
*        error results if more values than the number of input FITS
*        files are supplied.  If too few are given, the last value in
*        the list is applied to all the conversions; thus a single value
*        is applied to all the input files.  If more than one line is
*        required to enter the information at a prompt then place a "-"
*        at the end of each line where a continuation line is desired.
*        [!]
*     WCSATTRS = LITERAL (Read)
*        A comma-separated list of keyword=value pairs which modify
*        the way WCS information is extracted from the FITS headers.
*        Each of the keywords should be an attribute of an AST
*        FitsChan.  This is the object which is responsible for
*        interpreting the FITS WCS headers, and is described full in
*        the documentation for the AST library (see SUN/210).  For
*        instance, to force CAR projections to be interpreted as simple
*        linear mappings from pixel co-ordinates to celestial
*        co-ordinates (rather than the non-linear mapping  implied by
*        the FITS-WCS conventions), use WCSATTRS="CarLin=1".  A null
*        value (!) results in all attributes using default values.  [!]
*     WCSCOMP = LITERAL (Read)
*        This requests where co-ordinate information is stored in the
*        NDF for arbitrary FITS files.  FITS files from certain sources
*        (see "Special Formats" below) adopt their own conventions such
*        as always creating AXIS structures and not WCS, thus ignore
*        this parameter.  The allowed values are as follows.
*
*        "Axis" --- Writes co-ordinates of each element in the AXIS
*                   structure.
*        "WCS"  --- Stores co-ordinate information in the WCS component.
*        "Both" --- Writes co-ordinate information in both the AXIS and
*                   WCS components.
*        "None" --- Omits co-ordinate information.
*
*        "WCS" is the recommended option as it offers most flexibility
*        and many facilities such as transformations between co-ordinate
*        systems.  However, some legacy applications such as Figaro do
*        not recognise WCS and for these "Axis" is more appropriate.
*        If you are mixing data processing packages then you may need
*        "Both", but care should be exercised to avoid inconsistent
*        representations, especially if the data are exported to FITS
*        with NDF2FITS (see its Parameter USEAXIS).  ["WCS"]

*  Examples:
*     fits2ndf 256.fit f256 fmtcnv=f
*        This converts the FITS file called 256.fit to the NDF called
*        f256.  The data type of the NDF's data array matches that of
*        the FITS primary data array.  A FITS extension is created in
*        f256, and FITS sub-files are propagated to NDF extensions.
*     fits2ndf 256.fit f256 fmtcnv=native type=_real
*        As above but now a _REAL type scaled data array is created,
*        assuming that 256.fit contains scaled integer data with
*        BITPIX=8 or 16 and non-default BSCALE and BZERO keywords.
*     fits2ndf 256.fit f256 fmtcnv=t type=_real wcscomp=axis
*        As the first example, but now a _REAL type data array is
*        created by applying the scale and offset from BSCALE and
*        BZERO keywords to the integer values stored in 256.fit.
*        Co-ordinate information is written only to the AXIS structure.
*     fits2ndf 256.fit f256 noprofits noproexts
*        As the previous example except there will be a format
*        conversion from a FITS integer data type to floating point in
*        the NDF using the BSCALE and BZERO keywords, and there will be
*        no extensions written within f256.
*     fits2ndf "*.fit,p*.fits" *
*        This converts a set of FITS files given by the list
*        "*.fit,p*.fits", where * is the match-any-character wildcard.
*        The resultant NDFs take the filenames of the FITS files, so if
*        one of the FITS files was parker.fits, the resultant NDF would
*        be called parker.  Format conversion is performed on integer
*        data types.  A FITS extension is created in each NDF and any
*        FITS sub-files present are propagated to NDF extensions.
*     fits2ndf swp25000.mxlo mxlo25000
*        This converts the IUE MXLO FITS file called swp25000.mxlo to
*        the NDF called mxlo25000.  Should the dataset comprise both
*        the large- and small-aperture spectra, they will be found in
*        NDFs mxlo25000.large and mxlo25000.small respectively.
*     fits2ndf SWP19966.MXHI mxhi19966
*        This converts the IUE MXHI FITS file called SWP19966.MXHI to a
*        series of NDFs within a file mxhi19966.sdf.  Each NDF
*        corresponds to an order.  Thus for instance the one hundredth
*        order will be in the NDF called mxhi19966.order100.
*     fits2ndf data/*.silo silo*|swp|| noprofits
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
*     fits2ndf 256.fit f256 fmtcnv=f encodings=DSS
*        This is the same as the first example except that it is
*        specified that the co-ordinate system information to be stored
*        in the WCS component of the NDF must be based on the FITS
*        keywords written with Digitised Sky Survey (DSS) images.  If
*        these keywords are not present in the FITS header then no WCS
*        component will be created.  All the earlier examples retained
*        the default null value for the ENCODINGS parameter, resulting
*        in the choice of keywords being based on the contents of the
*        FITS header (see the description of the ENCODINGS parameter
*        for details).
*     fits2ndf 256.fit f256 fmtcnv=f encodings="DSS,native"
*        This is the same as the previous example except that if no
*        DSS keywords are available, then the co-ordinate system
*        information stored in the NDF's WCS component will be based on
*        keywords written by applications which use the AST library (see
*        SUN/210).  One such application is NDF2FITS.  This `native'
*        encoding provides a `loss-free' means of transferring
*        information about co-ordinate systems (i.e. no information is
*        lost; this may not be the case with other encodings).  If the
*        file 256.fit contains neither DSS nor native AST keywords, then
*        no WCS component will be created.
*     fits2ndf "multifile.fit[extname=im3]" *
*        This will create an NDF, multifile, from the first FITS
*         extension in file multifile.fit whose EXTNAME keyword has the
*         value "im3".
*     fits2ndf multifile.fit multifile extable=table1
*        This will create a series of NDFs in the container file
*        multifile.sdf according to the specifications in the
*        EXTABLE-format file, table1.

*  Notes:
*     -  Some sources of FITS files that require special conversion
*     rules, particularly because they use binary tables, are
*     recognised.  Details of the processing for these is given within
*     topic "Special Formats".
*
*     Two other special cases are when a particular sub-file is
*     specified by the IN parameter and when conversion is driven by an
*     EXTABLE file.
*
*     The general rules for the conversion apply if the FITS file is not
*     one of the "Special Formats" (including one defined by an EXTABLE)
*     and Parameter CONTAINER is not TRUE.
*
*     The general rules are as follows.
*
*     -  The primary data array of the FITS file becomes the NDF's data
*     array.  There is an option using Parameter FMTCNV to convert
*     integer data to floating point using the values of FITS keywords
*     BSCALE and BZERO.
*     -  Any integer array elements with value equal to the FITS
*     keyword BLANK become bad values in the NDF data array.  Likewise
*     any floating-point data set to an IEEE not-a-number value also
*     become bad values in the NDF's data array.  The BAD_PIXEL flag is
*     set appropriately.
*     -  NDF quality and variance arrays are not created.
*     -  A verbatim copy of the FITS primary header is placed in the
*     NDF's FITS extension when Parameter PROFITS is TRUE.
*     -  Here are details of the processing of standard items from the
*     the FITS header, listed by FITS keyword.
*        CRVALn, CDELTn, CRPIXn, CTYPEn, CUNITn --- define the NDF's
*          WCS and/or AXIS components (see Parameters ENCODINGS and
*          WCSCOMP).
*        OBJECT, LABEL, BUNIT --- if present are equated to the NDF's
*          TITLE, LABEL, and UNITS components respectively.
*        LBOUNDn --- if present, this specifies the pixel origin for
*          the nth dimension.
*     -  Additional sub-files within the FITS files are converted into
*     extensions within the NDF if Parameter PROEXTS is TRUE.  These
*     extensions are named FITS_EXT_m for the mth sub-file.
*     -  An IMAGE sub-file is treated like the primary data array, and
*     follows the rules give above.  However, the resultant NDF is an
*     extension of the main NDF.
*     -  A BINTABLE or TABLE sub-file are converted into a structure
*     of type TABLE ().  This has a NROWS component specifying the
*     number of rows, and a COLUMNS structure containing a series of
*     further structures, each of which takes its name from the label
*     of the corresponding column in the FITS table.  If there is no
*     label for the nth column, the structure is called COLUMNn.
*     These COLUMN structures contain a column of table data values in
*     component DATA, preserving the original data type; and optional
*     UNITS and COMMENT components which specify the column's units and
*     the meaning of the column.  Thus for example, for the third
*     sub-file of NDF called ABC, the data for column called RA would
*     be located in ABC.MORE.FITS_EXT_3.COLUMNS.RA.DATA.
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
*     - You can supply compressed FITS files, such as the Rice
*     compression.
*     - NDF history recording is enabled in the output NDF.

*  Special Formats:
*     o  NDF2FITS
*
*     -  This is recognised by the presence of an HDUCLAS1 keyword set
*     to 'NDF'.  The conversion is similar to the general case, except
*     the processing of FITS sub-files and HISTORY headers.
*
*     -  An IMAGE sub-file converts to an NDF VARIANCE component,
*     provided the HDUCLAS2 keyword is present and has a value that is
*     either 'VARIANCE' or 'ERROR'.
*
*     -  An IMAGE sub-file converts to an NDF QUALITY component,
*     provided the HDUCLAS2 keyword is present and has value 'QUALITY'.
*
*     -  FITS ASCII and binary tables become NDF extensions, however,
*     the original structure path and data type are restored using
*     the values of the EXTNAME and EXTTYPE keywords respectively.  An
*     extension may be an array of structures, the shape being stored
*     in the EXTSHAPE keyword.  The shapes of multi-dimensional arrays
*     within the extensions are also restored.
*
*     -  HISTORY cards in a special format created by NDF2FITS are
*     converted back into NDF history records.  This will only work
*     provided the HISTORY headers have not been tampered.  Such
*     headers are not transferred to the FITS airlock, when
*     PROFITS=TRUE.
*
*     -  Any SMURF package's ancillary IMAGE sub-files are restored
*     to a SMURF extension, with the original names and structure
*     contents.  Thus the global HISTORY present in each sub-file is
*     not duplicated in each SMURF-extension NDF.
*
*     -  When CONTAINER is TRUE, a former UKIRT_HDS container file,
*     identified by the presence and values of HDSNAME and HDSTYPE 
*     keywords, may be recreated.  The container file has the original
*     structure including the NDFs' names, unless an EXTABLE is used.
*
*     o  IUE Final Archive LILO, LIHI, SILO, SIHI
*
*     -  This converts an IUE LI or SI product stored as a FITS primary
*     data array and IMAGE extension containing the quality into an
*     NDF.  Other FITS headers are used to create AXIS structures (SI
*     products only), and character components.
*
*     -  Details of the conversion are:
*        -  The primary data array of the FITS file becomes NDF main
*        data array.  The value of Parameter FMTCNV controls whether
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
*        extension when Parameter PROFITS is TRUE.
*
*     o  IUE Final Archive MXLO
*
*     -  This will usually be a single 1-dimensional NDF, however, if
*     the binary table contains two rows, a pair of NDFs are stored in
*     a single HDS container file whose name is specified by parameter
*     OUT.  The name of each NDF is either SMALL or LARGE depending on
*     the size of the aperture used.  Thus for OUT=ABC, the
*     small-aperture observation will be in an NDF called ABC.SMALL.
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
*
*     o  IUE Final Archive MXHI
*
*     -  This creates a series of NDFs within a single HDS container
*     file whose name is specified by Parameter OUT.  Each NDF
*     corresponds to a spectral order, and may be accessed individually.
*     The name of each NDF is ORDER followed by the spectral-order
*     number.  For instance, when OUT=SWP, the 85th-order spectrum will
*     be in an NDF called SWP.ORDER85.
*     -  Only the most-significant 8 bits of the quality flags are
*     transferred to the NDF.
*     -  The primary headers may be written to the standard FITS
*     airlock extension when PROFITS is TRUE.  To save space, this
*     appears once in the NDF specified by Parameter OUT.
*     -  The conversion from binary-table columns and headers to NDF
*     objects is as follows:
*
*        NPOINTS                Number of non-zero elements
*        WAVELENGTH             Start wavelength of the non-zero
*                               elements, label, and units
*        STARTPIX               Lower bound of the non-zero elements
*        DELTAW                 Incremental wavelength
*        ABS_CAL                Data array, label, and units
*        QUALITY                Quality array
*        remaining columns      Component in IUE_MH extension (NOISE,
*         (except 14-17)        NET, BACKGROUND, and RIPPLE are NDFs
*                               each comprising a data array, label,
*                               units and wavelength axis)
*     -  It may be possible to evaluate an approximate error array for
*     the absolutely calibrated data (ABS_CAL), by multiplying the
*     NOISE by the ratio ABS_CAL / NET for each element.
*     -  The Chebyshev coefficients, limits, and scale factor in
*     columns 14 to 17 are omitted as the evaluated background fit is
*     propagated in BACKGROUND.
*
*     o  IUE INES reduced spectra
*
*     -  This generates a single 1-dimensional NDF.
*     -  Only the most-significant 8 bits of the quality flags are
*     transferred to the NDF.
*     -  The primary headers may be written to the standard FITS
*     airlock extension when PROFITS is TRUE.
*     -  The conversion from binary-table columns and headers to NDF
*     objects is as follows:
*
*        WAVELENGTH             Start wavelength, axis label and units
*        FLUX                   Data array, label, units, bad-pixel flag
*        SIGMA                  Data-error array
*        QUALITY                Quality array
*
*     o  ISO CAM auto-analysis (CMAP, CMOS)
*
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
*     corresponding to the flux, the r.m.s. errors, and the integration
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
*        CDELTn                 Pixel increment along Axis n
*        CRPIXn                 Axis n reference pixel
*        CRVALn                 Axis n co-ordinate of reference pixel
*        CTYPEn                 Label for Axis n
*        CUNITn                 Units for Axis n
*        NAXIS                  Number of dimensions
*        NAXISn                 Dimension of Axis n
*        remaining columns      Keyword in FITS extension
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
*
*     o ISO SWS auto-analysis (SWS AA)
*
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
*        -  The OBJECT, LABEL, BUNIT keywords define the NDF's TITLE,
*        LABEL, and UNITS components respectively, if they are defined.
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
*        the EXTNAME and EXTTYPE keywords, or names it FITS_EXT_n for
*        the nth FITS extension.
*        -  A FITS extension in the NDF may be written to store the
*        primary data unit's headers when Parameter PROFITS is TRUE.
*        This FITS airlock will not contain any NDF-style HISTORY
*        records.

*  References:
*     Bailey, J.A. 1997, 2dF Software Report 14, version 0.5.
*     NASA Office of Standards and Technology, 1994, "A User's Guide
*       for the Flexible Image Transport System (FITS)", version 3.1.
*     NASA Office of Standards and Technology, 1995, "Definition of
*       the Flexible Image Transport System (FITS)", version 1.1.

*  Related Applications:
*     CONVERT: NDF2FITS; CURSA/xcatview; KAPPA: FITSDIN, FITSIN.

*  Implementation Deficiencies:
*     - There is no propagation of arbitrary FITS HISTORY headers to
*     the NDF's history records.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1997-1998, 2000, 2002, 2004 Central Laboratory of
*     the Research Councils. Copyright (C) 2006-2007 Particle Physics &
*     Astronomy Research Council.  2009, 2011 Science & Technology
*     Facilities Council.  All Rights Reserved.

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
*     18-DEC-1997 (DSB):
*        Added support for the NDF WCS component.
*     22-JAN-1998 (DSB):
*        Changed the scheme for naming multiple IUE MXLO NDFs within
*        the output container file; previously the row number was used,
*        but NDF names cannot start with numeric characters and an
*        error was consequently reported.  The word "ROW" is now
*        prepended to the row number.
*     1998 January 26 (MJC):
*        Added IUE MXHI format.  Tidied the prologue.  Changed the
*        MXLO NDF names to LARGE and SMALL instead of ROW1 and ROW2.
*      2-FEB-1998 (DSB):
*        Add ENCODING examples.
*     1998 August 16 (MJC)
*        Improved Notes on BINTABLE conversion and added CURSA to
*        Related Applications.
*     9-NOV-1998 (DSB):
*        Added FITS-IRAF encoding.
*     7-MAR-2000 (DSB):
*        Report an error if no input FITS files are supplied.
*     11-APR-2000 (DSB):
*        Added FITS-PC and FITS-AIPS WCS encodings.
*     12-APR-2000 (AJC):
*        Allow FITS extension specifier in FITS filenames
*        Correctly comment getting FMTCNV values (not BITPIX)
*     13-APR-2000 (AJC):
*        Added EXTABLE parameter
*     17-APR-2000 (AJC):
*        Annul errors before call MSG_OUT on illegal FMTCNV value
*      9-MAY-2000 (AJC):
*        Add NDF names to COF_EXTAB
*     12-JUN-2000 (AJC):
*        Major revision of EXTABLE description
*     12-JUL-2000 (AJC)
*        More tweaks to description
*     30-AUG-2000 (AJC):
*        Correct description FITS_EXT_n not NDF_EXT_n
*        Add the CONTAINER parameter
*        Tweak description of EXTABLE EXTN.name
*     20-FEB-2002 (DSB):
*        Added WCSATTRS parameter, mainly to deal with non-standard CAR
*        projections.
*     11-JUL-2004 (DSB):
*        Added FITS-AIPS++ encoding.
*     27-AUG-2004 (DSB):
*        Added FITS-CLASS encoding.
*     2006 April 7 (MJC):
*        Added TYPE parameter.  Move obtaining FMTCNV group to a
*        subroutine.  Wrap long lines.
*     2007 January 3 (MJC):
*        Allow for FMTCNV=Native.  Add examples using the TYPE parameter.
*     9-SEP-2009 (DSB):
*        Enable default history recording in output NDF.
*     18-SEP-2009 (DSB):
*        Remove default history recording in output NDF since the NDF
*        library now allows the user to control automatic history creation
*        via the "NDF_AUTO_HISTORY" environment variable.
*     2011 February 24 (MJC):
*        Add WCSCOMP parameter.
*     {enter_further_changes_here}

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
      INCLUDE 'ONE_ERR'          ! ONE_ constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL ONE_FIND_FILE      ! Find file from wildcarded list

      EXTERNAL ONE_FIND_FILE

*  Local Constants:
      INTEGER BLOCKF             ! Blocking factor
      PARAMETER ( BLOCKF = 1 )

      INTEGER MAXCOD             ! Number of known AST encodings
      PARAMETER ( MAXCOD = 6 )

*  Local Variables:
      INTEGER ADDED              ! Number of items added to a group
      LOGICAL CFLAG              ! A group requires further input via
                                 ! continuation lines?
      LOGICAL CONTNR             ! FITS HDU's are to be written as
                                 ! components of an HDS container file?
      INTEGER ENCGRP             ! Group identifier of ENCODINGS
      CHARACTER*( GRP__SZNAM ) ENCODS( MAXCOD ) ! AST encodings for WCS
                                 ! component
      INTEGER EXTLEN             ! Length extension part of FSPEC
      INTEGER FCGRP              ! Group identifier of FMTCNVs
      INTEGER FDL                ! FIle descriptor for logfile
      LOGICAL FILEXS             ! FITS file exists
      CHARACTER*255 FILNAM       ! Name of FITS file
      CHARACTER*6 FMTCON         ! Character form of a FMTCNV value
      CHARACTER*255 FSPEC        ! File specification
      INTEGER FLEN               ! Length of filename part of FSPEC
      LOGICAL FOUND              ! Found a wildcarded file?
      CHARACTER*255 IFSPEC       ! Individual File specification
      INTEGER IPOSN              ! String index
      INTEGER IFILE              ! Loop counter for each input NDF
      CHARACTER*( FIO__SZFNM ) INFILE ! Input-file name
      INTEGER IGRP1              ! Group identifier of input file list
      INTEGER IGRP2              ! Group identifier of input files
      INTEGER IGRP3              ! Group identifier of input purged FITS
                                 ! files
      INTEGER IWILD              ! Counter of the wild-carded files
      LOGICAL LEAVE              ! The FITS-file testing is finished?
      LOGICAL LOGHDR             ! There is an open logfile?
      INTEGER LP                 ! Loop counter
      INTEGER NDF                ! NDF identifier
      CHARACTER*255 NDFNAM       ! Name of NDF
      INTEGER NENCOD             ! No. of of WCS encodings supplied
      INTEGER NGLIST             ! No. of items in input list
      INTEGER NGROUP             ! Group identifier of default list of
                                 ! NDF files
      INTEGER NIFILE             ! Number of NDF files
      INTEGER NOFILE             ! Number of output files
      INTEGER OGROUP             ! Group identifier of output FITS files
      INTEGER PLACE              ! NDF placeholder
      LOGICAL PROEXT             ! True if the other FITS extensions are
                                 ! propagated to NDF extensions
      LOGICAL PROFIT             ! The FITS extension is created?
      INTEGER TGROUP             ! Group identifier of output types
      CHARACTER*( NDF__SZTYP ) TYPE ! Data type for processing
      INTEGER TSTAT              ! Temporary status
      CHARACTER*255 WCSATT       ! Attributes for the WCS FitsChan
      CHARACTER*4 WCSCMP         ! Location for co-ordinate information

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
         IF ( IGRP1 .NE. GRP__NOID ) CALL GRP_DELET( IGRP1, STATUS )
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

*  Extract any extension specifier
         CALL COF_EXTXT( FSPEC, FLEN, EXTLEN, STATUS )

*  Find the files which match this specification... First initialise
*  the context counter and looping flag.
         IWILD = 0
         LEAVE = .FALSE.

*  Start new error context.
         CALL ERR_MARK

*  Loop for all the files in the wildcard specification.
         DO WHILE ( .NOT. LEAVE )

*  Get a single FITS file that matches this specification.
            FOUND = ONE_FIND_FILE( FSPEC( 1:FLEN ), .TRUE., INFILE,
     :                             IWILD, STATUS )

*  Check if a file has been found. Status can tell us this.
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Inquire whether the file exists or not.  Since the wild carding has
*  obtained the files, a non-existent file indicates that the file name
*  has been truncated to FIO__SZFNM characters.  This is not foolproof
*  as some other file may be obtained.
               INQUIRE ( FILE = INFILE, EXIST=FILEXS )
               IF ( .NOT. FILEXS ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'MXFNC', FIO__SZFNM )
                  CALL MSG_SETC( 'FNAME', INFILE )
                  CALL ERR_REP( 'FITS2NDF_TRUNC',
     :              'File ^FNAME cannot be opened - the name may have'/
     :              /' been truncated to the maximum ^MXFNC '/
     :              /'characters.', STATUS )
                  CALL ERR_RLSE
                  GOTO 999
               END IF

*  Add this FITS file into the output group.  NIFILE keeps a count of
*  the number of files in the output group.
               IFSPEC = ' '
               IPOSN = 0
               CALL CHR_APPND( INFILE, IFSPEC, IPOSN )
               CALL CHR_APPND( FSPEC( FLEN+1:FLEN+EXTLEN ), IFSPEC,
     :                         IPOSN )
               CALL GRP_GRPEX( IFSPEC, GRP__NOID, IGRP2, NIFILE,
     :                         ADDED, CFLAG, STATUS )

            ELSE

*  Tidy up the file system when the list of files is exhausted.
               CALL ONE_FIND_FILE_END( IWILD, STATUS )

*  No files bad status should now be reset to good.
               IF ( STATUS .EQ. ONE__NOFILES ) CALL ERR_ANNUL( STATUS )

*  Go to the next GRP expression.
               LEAVE = .TRUE.

            END IF

         END DO

*  Release the error context.
         CALL ERR_RLSE

      END DO

*  Finished with the first group so delete it.
      IF ( IGRP1 .NE. GRP__NOID ) CALL GRP_DELET( IGRP1, STATUS )

*  Tidy up and exit if something went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( IGRP2 .NE. GRP__NOID ) CALL GRP_DELET( IGRP2, STATUS )
         GOTO 999
      END IF

*  If no files were found, then report an error, and exit.
      IF ( NIFILE .LE. 0 ) THEN

         IF( IGRP1 .NE. GRP__NOID ) THEN
            CALL GRP_GET( IGRP1, 1, 1, FSPEC, STATUS )
         ELSE
            FSPEC = ' '
         END IF

         STATUS = SAI__ERROR
         IF( FSPEC .NE. ' ' ) THEN
            CALL MSG_SETC( 'SPEC', FSPEC )
            CALL ERR_REP( 'FITSDIN_NOFILES', 'No input files found '/
     :        /'matching the specification "^SPEC".', STATUS )
         ELSE
            CALL ERR_REP( 'FITSDIN_NOFILES', 'No input files found '/
     :        /'matching the supplied specification.', STATUS )
         END IF

         GOTO 999

      END IF

*  Purge any duplication from the FITS files.
      CALL GRP_PURGE( IGRP2, IGRP3, STATUS )

*  Finished with the second group so delete it.
      IF ( IGRP2 .NE. GRP__NOID ) CALL GRP_DELET( IGRP2, STATUS )

*  Find the number of FITS files after the purge.
      CALL GRP_GRPSZ( IGRP3, NIFILE, STATUS )

*  Report an error if the group is empty.
      IF( NIFILE .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FITS2NDF_NOFILES', 'FITS2NDF: No usable '/
     :                 /'input FITS files supplied.', STATUS )
      END IF

*  Tidy up and exit if something went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( IGRP3 .NE. GRP__NOID ) CALL GRP_DELET( IGRP3, STATUS )
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
         IF ( IGRP3 .NE. GRP__NOID ) CALL GRP_DELET( IGRP3, STATUS )
         IF ( NGROUP .NE. GRP__NOID ) CALL GRP_DELET( NGROUP, STATUS )
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
      IF ( NGROUP .NE. GRP__NOID ) CALL GRP_DELET( NGROUP, STATUS )

*  Tidy up and exit if something went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( OGROUP .NE. GRP__NOID ) CALL GRP_DELET( OGROUP, STATUS )
         IF ( IGRP3 .NE. GRP__NOID ) CALL GRP_DELET( IGRP3, STATUS )
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
         IF ( OGROUP .NE. GRP__NOID ) CALL GRP_DELET( OGROUP, STATUS )
         IF ( IGRP3 .NE. GRP__NOID ) CALL GRP_DELET( IGRP3, STATUS )
         GOTO 999
      END IF

*  Get the FMTCNVs.
*  ================

*  Loop until all values are acceptable.
      CALL COF_GETFC( 'FMTCNV', NIFILE, FCGRP, STATUS )

*  Tidy up and exit if there has been an error.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( OGROUP .NE. GRP__NOID ) CALL GRP_DELET( OGROUP, STATUS )
         IF ( IGRP3 .NE. GRP__NOID ) CALL GRP_DELET( IGRP3, STATUS )
         GOTO 999
      END IF

*  Get the TYPEs.
*  ==============
*  Loop until all values are acceptable.
      CALL COF_GETYP( 'TYPE', NIFILE, TGROUP, STATUS )

*  Tidy up and exit.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( FCGRP .NE. GRP__NOID ) CALL GRP_DELET( FCGRP, STATUS )
         IF ( OGROUP .NE. GRP__NOID ) CALL GRP_DELET( OGROUP, STATUS )
         IF ( IGRP3 .NE. GRP__NOID ) CALL GRP_DELET( IGRP3, STATUS )
         GOTO 999
      END IF

*  Obtain some global parameter values.
*  ====================================

*  Determine whether or not the FITS extension is to be created.
      CALL PAR_GET0L( 'PROFITS', PROFIT, STATUS )

*  Determine whether or not other FITS extensions are to be stored in
*  additional NDF extensions FITS_n.
      CALL PAR_GET0L( 'PROEXTS', PROEXT, STATUS )

*  Determine whether or not a container file is to be created.
      CALL PAR_GET0L( 'CONTAINER', CONTNR, STATUS )

*  Obtain destination(s) for WCS information.
      CALL PAR_CHOIC( 'WCSCOMP', 'WCS', 'WCS,Axis,Both,None', .TRUE.,
     :                WCSCMP, STATUS )

*  Abort if there has been an error.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

      ENCGRP = GRP__NOID
      IF ( WCSCMP .EQ. 'WCS' .OR. WCSCMP .EQ. 'Both' ) THEN

*  Get a group from parameter ENCODINGS holding the AST encodings to
*  use when creating the WCS component from the FITS header.
         CALL GRP_NEW( 'AST Encodings', ENCGRP, STATUS )
         CALL GRP_SETCS( ENCGRP, .FALSE., STATUS )

         CFLAG = .TRUE.
         DO WHILE ( CFLAG .AND. STATUS .EQ. SAI__OK )
            CALL GRP_GROUP( 'ENCODINGS', GRP__NOID, ENCGRP, NENCOD,
     :                      ADDED, CFLAG, STATUS )
            IF ( CFLAG ) CALL PAR_CANCL( 'ENCODINGS', STATUS )
         END DO

*  If a NULL parameter value was given for ENCODINGS annul the error,
*  and find the real number of values in the group.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL GRP_GRPSZ( ENCGRP, NENCOD, STATUS )
         END IF

*  Limit the number of encodings used to MAXCOD.
         IF ( NENCOD .GT. MAXCOD ) THEN
            CALL MSG_OUTIF( MSG__NORM, 'ENCODINGS', 'Only the first '/
     :                      /'^MX values for parameter %ENCODINGS '/
     :                      /'will be used.', STATUS )
            NENCOD = MAXCOD
         END IF

*  Extract the encodings from the group into an array.
         CALL GRP_GET( ENCGRP, 1, NENCOD, ENCODS, STATUS )

* Get any extra attributes for the FitsChan.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL PAR_GET0C( 'WCSATTRS', WCSATT, STATUS )
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               WCSATT = ' '
            END IF
         END IF

* Define defaults to satisfy the argument list of the main subroutine.
      ELSE
         NENCOD = 1
         ENCODS( 1 ) = 'FITS-WCS'
         WCSATT = ' '
      END IF

*  Get the EXTABLE information.  (Assumes this is good for all FITS
*  files).
      CALL COF_EXTAB( STATUS )

*  Process each file.
*  ==================
      DO IFILE = 1, NIFILE

*  Obtain the values for the parameters.
*  =====================================

*  Find the input FITS file name.
         CALL GRP_GET( IGRP3, IFILE, 1, FILNAM, STATUS )

*  Find the output NDF name.
         CALL GRP_GET( OGROUP, IFILE, 1, NDFNAM, STATUS )

*  Find the FMTCNV.
         CALL GRP_GET( FCGRP, IFILE, 1, FMTCON, STATUS )

*  Find the output data type.
         CALL GRP_GET( TGROUP, IFILE, 1, TYPE, STATUS )
         CALL CHR_UCASE( TYPE )

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
         CALL COF_F2NDF( FILNAM, NDF, LOGHDR, FDL, FMTCON, TYPE, PROFIT,
     :                   PROEXT, CONTNR, NENCOD, ENCODS, WCSATT, WCSCMP,
     :                   STATUS )

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
      IF ( TGROUP .NE. GRP__NOID ) CALL GRP_DELET( TGROUP, STATUS )
      IF ( FCGRP .NE. GRP__NOID ) CALL GRP_DELET( FCGRP, STATUS )
      IF ( OGROUP .NE. GRP__NOID ) CALL GRP_DELET( OGROUP, STATUS )
      IF ( IGRP3 .NE. GRP__NOID ) CALL GRP_DELET( IGRP3, STATUS )
      IF ( ENCGRP .NE. GRP__NOID ) CALL GRP_DELET( ENCGRP, STATUS )

 999  CONTINUE

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FITS2NDF_ERR',
     :     'FITS2NDF: Error converting a FITS file into an NDF.',
     :     STATUS )
      END IF

      END
