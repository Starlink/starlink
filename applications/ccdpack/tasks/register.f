      SUBROUTINE REGISTER( STATUS )
*+
*  Name:
*     REGISTER

*  Purpose:
*     Determines transformations between lists of positions.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL REGISTER( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine determines the transformations between (labelled)
*     position lists. Six different types of transformation are
*     available. The first 5 are based on the linear transformation,
*     the sixth being a function defined by you. The linear
*     transformations are based on the mappings
*
*        X' = A + B*X + C*Y
*        Y' = D + E*X + F*Y
*
*     and allow:
*       - shift of origin
*       - shift of origin and rotation
*       - shift of origin and magnification
*       - shift of origin, rotation and magnification (solid body)
*       - or a full six parameter fit
*
*     The self defined transform can be any mapping given as an
*     algebraic expression (including functions) using the methods
*     allowed by TRANSFORM (SUN/61).
*
*     When determining linear transformations REGISTER allows many
*     lists to be processed at once performing a simultaneous
*     registration of all the lists. When using a self defined
*     transform only two lists may be registered at any time.
*
*     The results from REGISTER are reported via the logging system
*     and then coded either as new frames with a domain name given
*     by the OUTDOMAIN parameter in the WCS component of the NDFs, 
*     or as TRANSFORM structures, containing the final algebraic 
*     solutions for each input list.  Transform structures may be 
*     stored in the CCDPACK exensions of the associated NDFs or within 
*     a named HDS container file if no associated NDFs exist.  Whether 
*     TRANSFORM structures or frames within the WCS components are 
*     chosen, they can be used by the applications TRANLIST and 
*     TRANNDF to transform position lists and resample the actual 
*     datasets respectively.

*  Usage:
*     register inlist fittype refpos

*  ADAM Parameters:
*     CLASS( ) = LITERAL (Read)
*        If CLASSIFY is TRUE then a list of classifications that
*        describe the properties of the transformation (parameters
*        XFOR, YFOR, XINV and YINV) should be given. This is
*        optional, but the information can be used to make other
*        applications run more efficiently.  Valid values are:
*           - LINEAR        -- Linear and preserves straight lines.
*           - INDEPENDENT   -- Preserves the independence of the axes.
*           - DIAGONAL      -- Preserves the axes themselves.
*           - ISOTROPIC     -- Preserves angles and shapes.
*           - POSITIVE_DET  -- A component of reflection is absent.
*           - NEGATIVE_DET  -- A component of reflection is present.
*           - CONSTANT_DET  -- The scale factor is constant.
*           - UNIT_DET      -- Areas (or volumes etc.) are preserved.
*
*        See SUN/61 Appendix B for details of transformation
*        classification and a table of classifications of common
*        mappings.
*
*        This parameter is ignored unless OUTFORMAT=TRANSFORM.
*     CLASSIFY = _LOGICAL (Read)
*        If TRUE then this indicates that you want to classify the
*        transform. Classifying a transformation can help in later
*        processing and allows applications to reject transformations
*        that they cannot use. The topic of classification is discussed
*        in SUN/61 which should be consulted before using this option.
*        Linear transformations are classified by this routine directly
*        and do not use this parameter.
*
*        This parameter is ignored unless OUTFORMAT=TRANSFORM.
*        [FALSE]
*     FA-FZ = LITERAL (Read)
*        These parameters supply the values of "sub-expressions" used in
*        the expressions XFOR, YFOR, XINV and YINV. These parameters
*        should be used when repeated expressions are present in complex
*        transformations. Sub-expressions may contain references to
*        other sub-expressions and the variables (PA-PZ).
*        An example of using sub-expressions is:
*           XFOR > PA*ASIND(FA/PA)*X/FA
*           YFOR > PA*ASIND(FA/PA)*Y/FA
*           XINV > PA*SIND(FB/PA)*XX/FB
*           YINV > PA*SIND(FB/PA)*YY/FB
*           FA > SQRT(X*X+Y*Y)
*           FB > SQRT(XX*XX+YY*YY)
*
*        This parameter is ignored unless OUTFORMAT=TRANSFORM.
*     FITTYPE = _INTEGER (Read)
*        The type of fit which should be used when determining the
*        transformation between the input positions lists. This may take
*        the values
*           - 1 -- shift of origin
*           - 2 -- shift of origin and rotation
*           - 3 -- shift of origin and magnification
*           - 4 -- shift of origin, rotation and magnification (solid
*                  body)
*           - 5 -- a full six parameter fit
*           - 6 -- self defined function
*
*        [5]
*     FULL = _LOGICAL (Read)
*        If FITTYPE=6 is chosen then this parameter value determines
*        if a full transformation is to be performed or not. If FALSE
*        then you will only be prompted for expressions for XFOR and
*        YFOR and the inverse transformation will remain undefined.
*
*        If TRUE then you will also be prompted for XINV and YINV in
*        response to which the inverse mappings for X' and Y' are
*        required. Not performing a full fit will affect the later
*        uses of the transformation. At present not providing an inverse
*        mapping means that image resampling (TRANNDF) may not be
*        performed.
*        [FALSE]
*     IN = LITERAL (Read)
*        If NDFNAMES is FALSE and PLACEIN is "NDF" then a list of NDF
*        names in which to store the WCS frames or transformation 
*        structures is required. This list of names must correspond 
*        exactly to the order of the associated input lists. A listing 
*        of the order of inputs is shown before this parameter is 
*        accessed.
*
*        The NDF names may (although this is probably not advisable)
*        be specified using wildcards, or may be specified using an
*        indirection file (the indirection character is "^").
*     INLIST = LITERAL (Read)
*        This parameter is used to access the names of the lists
*        which contain the positions and, if NDFNAMES is TRUE, the names
*        of the associated NDFs. If NDFNAMES is TRUE the names of the
*        position lists are assumed to be stored in the extension of the
*        NDFs (in the CCDPACK extension item CURRENT_LIST) and the names
*        of the NDFs themselves should be given (and may include
*        wildcards).
*
*        If NDFNAMES is FALSE then the actual names of the position
*        lists should be given. These may not use wildcards but may be
*        specified using indirection (other CCDPACK position list
*        processing routines will write the names of their results
*        files into files suitable for use in this manner) the
*        indirection character is "^".
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "CCDPACK.LOG".
*        [CCDPACK.LOG]
*     LOGTO = LITERAL (Read)
*        Every CCDPACK application has the ability to log its output
*        for future reference as well as for display on the terminal.
*        This parameter controls this process, and may be set to any
*        unique abbreviation of the following:
*           -  TERMINAL  -- Send output to the terminal only
*           -  LOGFILE   -- Send output to the logfile only (see the
*                           LOGFILE parameter)
*           -  BOTH      -- Send output to both the terminal and the
*                           logfile
*           -  NEITHER   -- Produce no output at all
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     NDFNAMES = _LOGICAL (Read)
*        This parameter specifies whether the names of the input
*        positions lists are stored in the CCDPACK extensions of NDFs.
*        If TRUE then the INLIST parameter accesses a list of NDFs
*        which are used to get the associated positions lists. If FALSE
*        then INLIST just accesses the position list names directly.
*
*        If the names of the lists are stored in the CCDPACK NDF
*        extension then the final WCS frame information or 
*        transformation structure is also written to the NDF 
*        extension.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.
*        [TRUE]
*     OUTDOMAIN = LITERAL (Read)
*        If OUTFORMAT is 'WCS', the transformation information is 
*        written as a new frame to the WCS component of the NDF.
*        The frame normally has a Domain name of 'CCD_REG', but 
*        that can be overridden with this parameter.  When this
*        frame is added, any previously existing frame which has
*        the same Domain is removed.
*
*        The name is converted to upper case, and whitespace is removed.
*        [CCD_REG]
*     OUTFORMAT = LITERAL (Read)
*        If NDFNAMES is TRUE, or PLACEIN is 'NDF', then this parameter
*        specifies how the transformation is to be written into the 
*        NDFs.  There are two possible values:
*
*           - WCS       -- written into WCS component of the NDF
*           - TRANSFORM -- written as a TRANSFORM structure into the 
*                          .MORE.CCDPACK extension of the NDF
*
*        If WCS is chosen, then a new frame, with a Domain value given
*        by OUTDOMAIN, is inserted into the WCS component of the NDF
*        (a new WCS component is created if none previously existed).
*        A mapping between this and the 'PIXEL' domain is defined,
*        which is the unit mapping in the case that the NDF in question
*        is the reference set.  If any frame in the domain OUTDOMAIN
*        already exists in the WCS component, it will be erased, so
*        that after successful processing by REGISTER it will contain 
*        only one such frame.
*
*        WCS is currently only possible for linear transformations 
*        (FITTYPE values between 1 and 5).
*        [WCS]
*     PA-PZ = LITERAL (Read)
*        When FITTYPE=6 these parameters are used for supplying initial
*        guesses at the values of the fit parameters. Normally the
*        values of these parameters are not critical, but occasionally
*        the minimization routine fails due to numeric problems (these
*        are usually caused by trig functions etc. which are given
*        invalid values (outside +/-1 etc.)).
*        [1.0D0]
*     PLACEIN = LITERAL (Read)
*        If NDFNAMES is FALSE then this parameter specifies where
*        you would like to store the final transformation structures.
*        The options are:
*           - NDF  -- store them in NDF extensions
*           - FILE -- store them in a container file.
*
*        If the NDF option is chosen then you will have the option of
*        supplying the NDF names via the parameter IN. If the FILE
*        option is chosen then the name of an HDS container file should
*        be given in response to the TRFILE parameter.
*        [NDF]
*     REFPOS = _INTEGER (Read)
*        The position within the list of inputs which corresponds to
*        the list to be used as the reference set.
*        [1]
*     TOLER = _DOUBLE (Read)
*        The RMS tolerance in positions which is used to determine the
*        best fit. Adjust this value only if the input positions are
*        specified in coordinates with a higher accuracy or smaller
*        units.
*        [0.001]
*     TRFILE = TRFILE (Read)
*        If PLACEIN is given as "FILE" then the value of this parameter
*        specifies the name of the container file to be used to store
*        the resultant transformation structures.
*     USEWCS = _LOGICAL (Read)
*        This parameter specifies whether the coordinates in the 
*        position lists should be transformed from Pixel coordinates 
*        into the coordinates of the Current frame of the WCS 
*        component of the associated NDF before use.  It should
*        normally be set TRUE, in which case the transformation type
*        set by the FITTYPE parameter is the type which will be fit
*        between the Current coordinate frames of the NDFs.  
*        Otherwise the fit will be between the Pixel coordinate 
*        frames.
*
*        This parameter is ignored if NDFNAMES is not TRUE.
*        [TRUE]
*     XFOR = LITERAL (Read)
*        If FITTYPE=6 then this parameter specifies the parameterised
*        algebraic expression to be used as the forward X
*        transformation. The expression may use all the functions
*        specified in SUN/61 (TRANSFORM) as well as the usual
*        mathematical operators (+,-,*,/,**). Functions are
*        parameterised by the strings PA,PB,PC...PZ which are the
*        values which will be determined. The string must contain at
*        least one reference to either X or Y.  So a possible return is
*            PA+PB*X
*
*        which is the same as the linear X transformation which just
*        applies an offset and a scale factor.
*     XINV = LITERAL (Read)
*        If FITTYPE=6 and FULL=TRUE then this parameter specifies
*        the inverse X transformation. The expression may use all the
*        functions specified in SUN/61 (TRANSFORM) as well as the usual
*        mathematical operations (+,-,*,/,**). Functions are
*        parameterised by the strings PA,PB,PC...PZ which are the
*        values which will be determined.  This expression must contain
*        a reference to either XX or YY. So a possible return is
*            (XX-PA)/PB
*
*        which is the same as the inverse linear X transformation for an
*        offset and scale.
*     YFOR = LITERAL (Read)
*        If FITTYPE=6 then this parameter specifies the parameterised
*        algebraic expression to be used as the forward Y
*        transformation. The expression may use all the functions
*        specified in SUN/61 (TRANSFORM) as well as the usual
*        mathematical operators (+,-,*,/,**). Functions are
*        parameterised by the strings PA,PB,PC...PZ which are the
*        values which will be determined.  The string must contain at
*        least one reference to either X or Y.  So a possible return is
*            PC+PD*Y
*
*        which is the same as the linear Y transformation which just
*        applies an offset and a scale factor.
*     YINV = LITERAL (Read)
*        If FITTYPE=6 and FULL=TRUE then this parameter specifies
*        the inverse Y transformation. The expression may use all the
*        functions specified in SUN/61 (TRANSFORM) as well as the usual
*        mathematical operations (+,-,*,/,**). Functions are
*        parameterised by the strings PA,PB,PC...PZ which are the
*        values which will be determined.  This expression must contain
*        a reference to either XX or YY. So a possible return is
*            (YY-PC)/PD
*
*        which is the same as the inverse linear Y transformation for an
*        offset and scale.

*  Examples:
*     register inlist='*' fittype=1
*        In this example all the NDFs in the current directory are
*        accessed and their associated position lists are opened.
*        A global fit between all the datasets is then performed
*        which results in estimates for the offsets from the first
*        input NDF's position.  These offsets are between the Current
*        coordinate frames of the NDFs.  The results are then coded 
*        as new frames, with the domain name 'CCD_REG', in the WCS 
*        component of the NDFs.  Actual registration of the images 
*        can then be achieved by aligning all the NDFs in the CCD_REG 
*        domain.
*
*     register inlist='*' trtype=5 outdomain=result-set1
*        This example works as above but this time the global
*        transformations are derived for a full 6-parameter linear fit
*        (which allows offset, rotation, magnification and shear).
*        The results are coded as frames in the Domain 'RESULT-SET1'.
*
*     register inlist='"myndf1,myndf2"' fittype=4 refpos=2
*        In this example a solid body fit is performed between the
*        position lists associated with the NDFs myndf1 and myndf2.
*        The reference positions are chosen to be those associated with
*        myndf2, so that the CCD_REG frame coordinates will be the
*        same as the Current frame in NDF myndf2.
*
*     register inlist='"one,two"' fittype=6 xfor='pa+pb*x' yfor='pa+pb*y'
*        In this example the position lists associated with the NDFs
*        one and two are said to be related by the algebraic
*        expressions "pa+pb*x" and "pa+pb*y", which indicates that a
*        single offset applies in both directions and a single scale
*        factor. A solution for the values PA and PB is found using a
*        general least-squares minimization technique. Starting values
*        for PA and PB can be given using the parameters PA and PB.
*        Since the fittype is 6, outformat must be 'transform', so
*        the results are coded as transform structures in the CCDPACK
*        extensions of the NDFs (under the item TRANSFORM).  The 
*        transform structures are arranged so that the forward 
*        transformation maps current positions into the reference 
*        coordinate system.
*
*     register inlist='"ndf1,ndf2"' fittype=6 xfor='pa+pb*x+pc*y+pd*x*y'
*              yfor='pe+pf*x+pg*y+ph*x*y'
*        In this example a non-linear transformation is fit between the
*        positions associated with the NDFs ndf1 and ndf2. This analysis
*        may help in determining whether a 6-parameter fit is good
*        enough, or if you just want to transform positions. A problem
*        with proceeding with this transformation in a general fashion
*        is deriving the inverse as this is required if you want to
*        perform image resampling.
*
*     register ndfnames=false inlist='"list1.acc,list2.acc,list3.acc"'
*              fittype=3 placein=ndf outformat=transform 
*              in='"ndf1,ndf2,ndf3"'
*        In this example the input position lists are not associated
*        with NDFs (ndfnames=false) and have to be specified by name
*        (no wildcards allowed). Since the position lists are not
*        associated with NDFs there is no natural home for the
*        transform structures. In this example it has been decided to
*        place the transforms in NDFs anyway. PLACEIN could also be
*        given as "file" in which case the transform structures are
*        written to a container file, under the items TRN_1, TRN_2 ...

*  Notes:
*     - Position list formats.
*
*       CCDPACK supports data in two formats.
*
*       CCDPACK format - the first three columns are interpreted as the
*       following.
*
*          - Column 1: an integer identifier
*          - Column 2: the X position
*          - Column 3: the Y position
*
*       The column one value must be an integer and is used to identify
*       positions which are the same but which have different locations
*       on different images. Values in any other (trailing) columns are
*       usually ignored.
*
*       EXTERNAL format - positions are specified using just an X and
*       a Y entry and no other entries.
*
*          - Column 1: the X position
*          - Column 2: the Y position
*
*       This format is used by KAPPA applications such as CURSOR.
*
*       Comments may be included in a file using the characters "#" and
*       "!". Columns may be separated by the use of commas or spaces.
*
*       Files with EXTERNAL format may be used with this application but
*       all positions have to be present in all lists, no missing
*       positions are allowed.
*
*       In all cases, the coordinates in position lists are pixel
*       coordinates.
*
*     - NDF extension items.
*
*       If NDFNAMES is TRUE then the item "CURRENT_LIST" of the
*       .MORE.CCDPACK structure of the input NDFs will be located
*       and assumed to contain the names of the lists whose positions
*       are to be used for registration.
*
*       On exit, if OUTFORMAT is "WCS" then a new frame with domain
*       name given by the OUTDOMAIN parameter will be added to the 
*       .WCS component of input NDFs.  Taken together these contain the 
*       registration information and can be inspected using NDFTRACE 
*       or WCSSHOW (SUN/95).
*
*       If OUTFORMAT is "TRANSFORM" then an item "TRANSFORM" will be 
*       added to the .MORE.CCDPACK structure of the input NDFs.
*       This contains the registration information as a TRANSFORM
*       structure and may be inspected using the utility HDSTRACE
*       (SUN/102).

*  Behaviour of parameters:
*     All parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application but does mean
*     that additional care needs to be taken when using the application
*     on new datasets or after a break of sometime.  The intrinsic
*     default behaviour of the application may be restored by using the
*     RESET keyword on the command line.
*
*     Certain parameters (LOGTO, LOGFILE and NDFNAMES) have global
*     values. These global values will always take precedence, except
*     when an assignment is made on the command line.  Global values may
*     be set and reset using the CCDSETUP and CCDCLEAR commands.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-JUL-1992 (PDRAPER):
*        Original version.
*     5-OCT-1992 (PDRAPER):
*        Made into CCDPACK task.
*     19-JUL-1995 (PDRAPER):
*        Removed AIF_ calls.
*     6-OCT-1995 (PDRAPER):
*        Updated for CCDPACK version 2.0.
*     18-SEP-1996 (PDRAPER):
*        Removed dependency on NAG calls. This affects fittypes 3,4,5,6.
*     3-MAR-1997 (PDRAPER):
*        Removed top-level locator control (foreign data access upgrade).
*     23-MAR-1998 (PDRAPER):
*        Changed to only open input lists when required. This works
*        around the FIO limit of 40 open file.
*     11-MAR-1999 (MBT):
*        Changed to use WCS components as well as TRANSFORM structures.
*     21-MAY-1999 (MBT):
*        Added USEWCS parameter.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations
      INCLUDE 'PAR_ERR'          ! Parameter system error codes
      INCLUDE 'TRN_PAR'          ! Transform parameters
      INCLUDE 'FIO_PAR'          ! FIO parameters
      INCLUDE 'AST_PAR'          ! AST parameters
      INCLUDE 'USER_ERR'         ! Private error codes

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string excluding trailing
                                 ! blanks

*  Local Variables:
      CHARACTER * ( 12 ) CLASES( TRN__MXCLS ) ! Classification string
      CHARACTER * ( 16 ) INAME  ! Name for object in container file
      CHARACTER * ( 12 ) OUTFM  ! Output format (WCS or TRANSFORM)
      CHARACTER * ( 4 ) PLACE   ! Place to store transformations
      CHARACTER * ( AST__SZCHR ) OUTDM ! Name of domain for new output WCS frame
      CHARACTER * ( AST__SZCHR ) DMN ! Current domain of WCS component
      CHARACTER * ( AST__SZCHR ) DMN1 ! Current domain of first WCS component
      CHARACTER * ( CCD1__BLEN ) LINE ! Buffer for reading data
      CHARACTER * ( CCD1__BLEN ) LINE1 ! Output buffer
      CHARACTER * ( CCD1__BLEN ) LINE2 ! Output buffer
      CHARACTER * ( CCD1__MTRNP ) ALPBET ! Characters which may follow P to indicate a parameter
      CHARACTER * ( CCD1__STRNP ) NAME ! Parameter name
      CHARACTER * ( CCD1__STRNP ) NTEMP ! Parameter name
      CHARACTER * ( CCD1__STRNP ) UNIPAR( CCD1__MTRNP +2 ) ! Unique Parameter name list
      CHARACTER * ( CCD1__STRNP ) XPARNM( CCD1__MTRNP ) ! Parameter names located in X map expression
      CHARACTER * ( CCD1__STRNP ) YPARNM( CCD1__MTRNP ) ! Parameter names located in Y map expression
      CHARACTER * ( CCD1__SZTRN ) FORMAP( 2 ) ! Forward map expressions
      CHARACTER * ( CCD1__SZTRN ) INVMAP( 2 ) ! Inverse map expressions
      CHARACTER * ( CCD1__SZTRN - 3 ) XFOR ! X mapping expression
      CHARACTER * ( CCD1__SZTRN - 3 ) XINV ! X mapping expression
      CHARACTER * ( CCD1__SZTRN - 3 ) YFOR ! Y mapping expression
      CHARACTER * ( CCD1__SZTRN - 3 ) YINV ! Y mapping expression
      CHARACTER * ( DAT__SZLOC ) ELOC ! Locator to object to store transform
      CHARACTER * ( DAT__SZLOC ) LOCEXT ! Locator to extension or top level storage in container file
      CHARACTER * ( FIO__SZFNM ) FNAME( CCD1__MXLIS ) ! Input filenames
      CHARACTER * ( FIO__SZFNM ) NDFNAM( CCD1__MXLIS ) ! Input filenames
      DOUBLE PRECISION FORVAL( CCD1__MTRNP ) ! Forward general coefficients
      DOUBLE PRECISION IDEN( 6 ) ! Identity transformation
      DOUBLE PRECISION INVVAL( CCD1__MTRNP ) ! Inverse general coefficients
      DOUBLE PRECISION RMS      ! Root mean square difference in fitted lists and reference list
      DOUBLE PRECISION TOLER    ! Tolerance in RMS devations
      DOUBLE PRECISION TR( 6, CCD1__MXLIS ) ! The transformation coefficients
      DOUBLE PRECISION TRCR( 6 ) ! Approx coefficients for Current->OUTDM map
      INTEGER FDIN              ! FIO file descriptors
      INTEGER FDREFO            ! Output reference set descriptor
      INTEGER FIOGR             ! IRH group of input list names
      INTEGER FRREG             ! AST pointer to frame in OUTDM domain
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Position within string
      INTEGER ID                ! NDF identifier
      INTEGER IFIT              ! The fittype
      INTEGER IP                ! Positional pointer into appended lists
      INTEGER IPDAT( CCD1__MXLIS + 1 ) ! Pointer to file data
      INTEGER IPID              ! Pointer to identifier workspace
      INTEGER IPIDR             ! Pointer to identifier workspace
      INTEGER IPIND( CCD1__MXLIS + 1 ) ! Pointer to file identifiers
      INTEGER IPOK              ! Pointer to logical workspace
      INTEGER IPREF             ! Points to position of reference list
      INTEGER IPWID             ! Pointer to identifier workspace
      INTEGER IPWX              ! Pointer to X positions workspace
      INTEGER IPWY              ! Pointer to Y positions workspace
      INTEGER IPX               ! Pointer to X positions workspace
      INTEGER IPXR              ! Pointer to X positions workspace
      INTEGER IPXRT             ! Pointer to temporary X positions
      INTEGER IPXT              ! Pointer to temporary X positions
      INTEGER IPY               ! Pointer to Y positions workspace
      INTEGER IPYR              ! Pointer to Y positions workspace
      INTEGER IPYRT             ! Pointer to temporary Y positions
      INTEGER IPYT              ! Pointer to temporary Y positions
      INTEGER IWCS              ! AST pointer to WCS component of NDF
      INTEGER J                 ! Loop variable
      INTEGER JCUR              ! Incdex of (original) Current frame in frameset
      INTEGER JPIX              ! Index of Pixel domain frame in frameset
      INTEGER JREG              ! Index of OUTDM domain frame in frameset
      INTEGER MAPS( CCD1__MXLIS + 1 ) ! AST Mapping from current to pixel frames
      INTEGER MAPTFM            ! AST pointer for Pixel - OUTDM mapping
      INTEGER MAP1              ! Temporary AST mapping
      INTEGER MAXIN             ! Maximum number of input lists
      INTEGER MININ             ! Minimum number of input lists
      INTEGER NCLASS            ! Number of classifications
      INTEGER NDFGR             ! IRG group of input NDF names
      INTEGER NNDF              ! Number of NDFs accessed
      INTEGER NOPEN             ! Number of input lists opened
      INTEGER NOUT
      INTEGER NREC( CCD1__MXLIS + 1 ) ! Number of records read from file
      INTEGER NSUBS             ! Number of token substitutions
      INTEGER NSUBSF            ! Number of token substitutions
      INTEGER NSUBSI            ! Number of token substitutions
      INTEGER NTOT              ! Total number of input records
      INTEGER NUMUNI            ! Number of unique parameter names
      INTEGER NVAL( CCD1__MXLIS + 1 ) ! Number of values in file
      INTEGER NVAL1             ! Number of values in tested file
      INTEGER NXPAR             ! Number of parameters in X mapping
      INTEGER NYPAR             ! Number of parameters in Y mapping
      INTEGER XFORL             ! Length of X mapping string
      INTEGER XINVL             ! Length of X mapping string
      INTEGER YFORL             ! Length of Y mapping string
      INTEGER YINVL             ! Length of Y mapping string
      LOGICAL CLASS( TRN__MXCLS ) ! Classification values
      LOGICAL DIFDMN            ! True if not all domains are the same
      LOGICAL DOCLAS            ! Perform a classification of general transformation
      LOGICAL FULL              ! True if a full general transformation is used
      LOGICAL HAVXXF            ! Flags indicating that mapping
      LOGICAL HAVXXI            ! Flags indicating that mapping
      LOGICAL HAVXYF            ! expression contain
      LOGICAL HAVXYI            ! expression contain
      LOGICAL HAVYXF            ! references to X and Y
      LOGICAL HAVYXI            ! references to X and Y
      LOGICAL HAVYYF            !
      LOGICAL HAVYYI            !
      LOGICAL NDFS              ! True if list names came from NDF extensions
      LOGICAL THERE             ! Flag indicating presence of object
      LOGICAL OUTREF            ! Output reference required
      LOGICAL USEWCS            ! Are we transforming to current frame

*  Local Data:
      DATA ALPBET/ 'ABCDEFGHIJKLNMOPQRSTUVWXYZ' /
      DATA IDEN/ 0.0D0, 1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start up the CCDPACK logging system.
      CALL CCD1_START( 'REGISTER', STATUS )

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Output reference set file is not open.
      OUTREF = .FALSE.

*  Find out which type of transformation the user requires.
*  If the fittype is not 6 then allow more than two input lists.
      CALL PAR_GET0I( 'FITTYPE', IFIT, STATUS )
      IFIT = MAX( 1, MIN( 6, IFIT ) )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Set the minimum and maximum numbers of input position lists.
      IF ( IFIT .EQ. 6 ) THEN
         MAXIN = 2
         MININ = 2
      ELSE
         MAXIN = CCD1__MXLIS
         MININ = 2
      END IF

*  Find out what is to be used for the source of the position list
*  names. Are they stored in NDF extensions or will just straight list
*  names be given.
      NDFS = .TRUE.
      CALL PAR_GET0L( 'NDFNAMES', NDFS, STATUS )

*  See if we will transform position lists using WCS components.
      USEWCS = .FALSE.
      IF ( NDFS ) THEN
         USEWCS = .TRUE.
         CALL PAR_GET0L( 'USEWCS', USEWCS, STATUS )
      END IF

*  Get the names of all the input lists.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL CCD1_GTLIG( NDFS, 'CURRENT_LIST', 'INLIST', MININ, MAXIN,
     :                 NOPEN, FIOGR, NDFGR, STATUS )

*  If a USER__003 error is generated by CCD1_GTLIG it indicates that 
*  not all the NDFs in INLIST contained .MORE.CCDPACK.CURRENT_LIST 
*  items.  Appropriate messages will have been output by CCD1_GTLIG.
*  We tolerate this.
      IF ( STATUS .EQ. USER__003 ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  Write the names of the associated NDFs out to the user.
      IF ( NDFS ) THEN 
         CALL CCD1_MSG( ' ', ' ', STATUS )
         LINE1 = '    NDFs containing position lists'
         LINE2 = '    ------------------------------'
         IF ( USEWCS ) THEN
            LINE1( 45: ) = 'Current domain'
            LINE2( 45: ) = '--------------'
         END IF
         CALL CCD1_MSG( ' ', LINE1, STATUS )
         CALL CCD1_MSG( ' ', LINE2, STATUS )
         DIFDMN = .FALSE.

         DO 17 I = 1, NOPEN

*  Get World Coordinate System information from NDFs.
            IF ( USEWCS ) THEN

*  Get pointer to WCS frameset.
               CALL IRG_NDFEX( NDFGR, I, ID, STATUS )
               CALL CCD1_GTWCS( ID, IWCS, STATUS )
               CALL NDF_ANNUL( ID, STATUS )

*  Get Current domain of frameset, and check against previous one.
               DMN = AST_GETC( IWCS, 'Domain', STATUS )
               IF ( I .EQ. 1 ) THEN
                  DMN1 = DMN
               ELSE IF ( DMN .NE. DMN1 ) THEN
                  DIFDMN = .TRUE.
               END IF

*  Get a mapping from the position list as read (PIXEL-domain
*  coordinates) to the values to be used for comparison (coordinates
*  of the Current domain of each NDF).
               JCUR = AST_GETI( IWCS, 'Current', STATUS )
               CALL CCD1_FRDM( IWCS, 'Pixel', JPIX, STATUS )
               MAP1 = AST_GETMAPPING( IWCS, JPIX, JCUR, STATUS )
               MAPS( I ) = AST_SIMPLIFY( MAP1, STATUS )
            END IF

*  Write message about NDF name and domain.
            CALL IRH_GET( NDFGR, I, 1, FNAME, STATUS )
            CALL MSG_SETC( 'FNAME', FNAME )
            CALL MSG_SETI( 'N', I )
            CALL MSG_LOAD( ' ', '  ^N) ^FNAME', LINE, IAT, STATUS )
            IF ( USEWCS ) LINE( MAX( 45, IAT + 2 ): ) = DMN
            CALL CCD1_MSG( ' ', LINE, STATUS )
 17      CONTINUE
      END IF

*  Get the position of the reference set in the input lists.
      IPREF = 1
      CALL PAR_GET0I( 'REFPOS', IPREF, STATUS )
      IPREF = MAX( 1, MIN( IPREF, NOPEN ) )

*  Get all the names of the input lists and report them.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '    Input position lists:', STATUS )
      CALL CCD1_MSG( ' ', '    ---------------------', STATUS )
      DO 6 I = 1, NOPEN
         CALL IRH_GET( FIOGR, I, 1, FNAME( I ), STATUS )
         CALL MSG_SETC( 'FNAME', FNAME( I ) )
         IF ( I .EQ. IPREF ) THEN
            CALL CCD1_MSG( ' ', '  ^FNAME (reference)', STATUS )
         ELSE
            CALL CCD1_MSG( ' ', '  ^FNAME', STATUS )
         END IF
 6    CONTINUE

*  ... and where the position list names originated.
      IF ( NDFS ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :'  Position list names extracted from NDF extensions.', STATUS )
      END IF
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Where are we going to store the transformations? If NDFNAMES is true
*  then the we'll store them in the NDFs.  If NDFNAMES is not true then
*  find out were the user would like them. The two options are: in NDFs
*  (in which case a list of NDF names will be solicited using the 
*  parameter 'IN'), or in a single container file.
      IF ( .NOT. NDFS ) THEN
         CALL PAR_CHOIC( 'PLACEIN', ' ', 'NDF,FILE', .FALSE., PLACE,
     :        STATUS )
         IF ( PLACE .EQ. 'NDF' ) THEN

*  User wants to place the results in some as yet unnamed NDFs.
*  Create a group containing the NDFs.
            CALL CCD1_NDFGU( NDFGR, NNDF, 'IN', NOPEN, NOPEN, STATUS )

         ELSE

*  User wants to store them as transform structures in a container file. 
*  Open one.
            OUTFM = 'TRANSFORM'
            CALL DAT_CREAT( 'TRFILE', 'CCDPACK_TRN', 0, 0, STATUS )
            CALL DAT_ASSOC( 'TRFILE', 'WRITE', LOCEXT, STATUS )
         END IF
      ELSE
         PLACE = 'NDF'
      END IF

*  How are the transformations to be written out?  If we are outputting
*  to a container file (i.e. if PLACE = 'FILE') then they must be stored
*  as TRANSFORM structures.  If they are written to NDFs (PLACE = 'NDF')
*  then find out if they are to be stored as TRANSFORM structures in
*  the .MORE.CCDPACK component, or as frames in the WCS component.
*  Currently for nonlinear (FITTYPE=6) fits, only TRANSFORM output is
*  possible.
      IF ( PLACE .EQ. 'NDF' .AND. IFIT .LE. 5 ) THEN
         CALL PAR_CHOIC( 'OUTFORMAT', 'WCS', 'WCS,TRANSFORM',
     :                   .TRUE., OUTFM, STATUS )

*  Get name of domain for new frame in WCS frameset.  Spaces are 
*  removed and it is folded to upper case since this is how the 
*  AST system will treat it.
         CALL PAR_GET0C( 'OUTDOMAIN', OUTDM, STATUS )
         CALL CHR_RMBLK( OUTDM )
         CALL CHR_UCASE( OUTDM )
      ELSE
         OUTFM = 'TRANSFORM'
      END IF

*  Read the data values in the input lists into workspace.
      IF( STATUS .NE. SAI__OK ) GO TO 99
      DO 1 I = 1, NOPEN

*  Test the input files for the number of entries, prior to extracting
*  the position information.
         CALL CCD1_OPFIO( FNAME( I ), 'READ', 'LIST', 0, FDIN, STATUS )
         CALL CCD1_LTEST( FDIN, LINE, CCD1__BLEN, 2, 0, NVAL1, STATUS )

*  If the position list has three or more columns of data then interpret
*  them as a standard list (identifier, x-position, y-position). If the
*  list has only 2 columns then interpret the inputs as just X and Y and
*  make up some identifiers (1,2,....NREC).
         IF ( NVAL1 .EQ. 2 ) THEN

*  Map in X and Y positions.
            CALL CCD1_NLMAP( FDIN, LINE, CCD1__BLEN, IPDAT( I ),
     :                       NREC( I ), NVAL( I ), STATUS )
            CALL CCD1_MALL( NREC( I ), '_INTEGER', IPIND( I ), STATUS )
            CALL CCD1_GISEQ( 1, 1, NREC( I ), %VAL( IPIND( I ) ),
     :                       STATUS )
         ELSE

*  Standard file format...
            CALL CCD1_LMAP( FDIN, LINE, CCD1__BLEN, IPIND( I ),
     :                      IPDAT( I ), NREC( I ), NVAL( I ), STATUS )
         END IF

*  Now close the file.
         CALL FIO_CLOSE( FDIN, STATUS )
 1    CONTINUE

      IF ( IFIT .NE. 6 .AND. STATUS .EQ. SAI__OK ) THEN

*  Does the user want the extended reference set of position written
*  out? If parameter state is null then no reference set will be
*  written.
         CALL CCD1_ASFIO( 'OUTREF', 'WRITE', 'LIST', 0, FDREFO, OUTREF,
     :                    STATUS )
         IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
      END IF

*  Start logging information in earnest.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '    Initial parameters:', STATUS )
      CALL CCD1_MSG( ' ', '    -------------------', STATUS )

*  Report the fit type.
      LINE = ' '
      IF ( IFIT .EQ. 1 ) THEN
         LINE = 'shift of origin only'
      ELSE IF ( IFIT .EQ. 2 ) THEN
         LINE = 'shift of origin and rotation'
      ELSE IF ( IFIT .EQ. 3 ) THEN
         LINE = 'shift of origin and magnification'
      ELSE IF ( IFIT .EQ. 4 ) THEN
         LINE = 'shift of origin, rotation and magnification '//
     :          '(solid body)'
      ELSE IF ( IFIT .EQ. 5 ) THEN
         LINE = 'full six parameter fit'
      ELSE
         LINE = 'user defined function'
      END IF
      CALL MSG_SETC( 'LINE', LINE )
      CALL CCD1_MSG( ' ', '  Registering using a ^LINE', STATUS )

*  Report how position list coordinates are interpreted.
      IF ( USEWCS ) THEN
         CALL CCD1_MSG( ' ', '  Coordinates will be remapped to'//
     :   ' Current frame before use', STATUS )
      ELSE
         CALL CCD1_MSG( ' ', '  Pixel coordinates will be used'//
     :   ' direct', STATUS )
      END IF

*  Report where the transformation structures will be stored.
      IF ( PLACE .EQ. 'NDF' ) THEN
         IF ( OUTFM .EQ. 'WCS' ) THEN
            CALL MSG_SETC( 'OUTDM', OUTDM )
            CALL CCD1_MSG( ' ', '  Transformations will be'//
     :      ' written as WCS frames in domain ^OUTDM', STATUS )
         ELSE
            CALL CCD1_MSG( ' ', '  Transformation structures will be'//
     :      ' written to .MORE.CCDPACK components of NDFs', STATUS )
         END IF
      ELSE
         CALL DAT_MSG( 'FILE', LOCEXT )
         CALL CCD1_MSG( ' ', '  Transformation structures will be'//
     :   ' written as TRANSFORM structures into ^FILE', STATUS )
      END IF

*  Get the tolerance which is required in the fit.
      CALL PAR_GET0D( 'TOLER', TOLER, STATUS )
      CALL MSG_SETD( 'TOLER', TOLER )
      CALL CCD1_MSG( ' ',
     :     '  Maximum change in positions is ^TOLER', STATUS )

*  Branch to deal with either linear transformations or user defined
*  transformations.
      IF ( IFIT .LE. 5 ) THEN

*  Need to extract all the data and enter into a single appended list of
*  X positions and Y positions. First determine the number of entries
         NTOT = 0
         DO 2 I = 1, NOPEN
            NTOT = NTOT + NREC( I )
 2       CONTINUE

*  Get the workspace for the lists.
         CALL CCD1_MALL( NTOT, '_DOUBLE', IPX, STATUS )
         CALL CCD1_MALL( NTOT, '_DOUBLE', IPY, STATUS )
         CALL CCD1_MALL( NTOT, '_INTEGER', IPID, STATUS )
         CALL CCD1_MALL( NTOT, '_INTEGER', IPOK, STATUS )
         CALL CCD1_MALL( NTOT, '_DOUBLE', IPXR, STATUS )
         CALL CCD1_MALL( NTOT, '_DOUBLE', IPYR, STATUS )
         CALL CCD1_MALL( NTOT, '_INTEGER', IPIDR, STATUS )
         CALL CCD1_MALL( NTOT, '_DOUBLE', IPWX, STATUS )
         CALL CCD1_MALL( NTOT, '_DOUBLE', IPWY, STATUS )
         CALL CCD1_MALL( NTOT, '_INTEGER', IPWID, STATUS )
         IF ( USEWCS ) THEN
            CALL CCD1_MALL( NTOT, '_DOUBLE', IPXT, STATUS )
            CALL CCD1_MALL( NTOT, '_DOUBLE', IPYT, STATUS )
         END IF

*  Split data and transfer into lists, append all positions and
*  identifiers into one.
         IP = 1
         DO 3 I = 1, NOPEN
            CALL CCD1_LEXT( %VAL( IPDAT( I ) ), NREC( I ), NVAL( I ),
     :                      1, %VAL( IPWX ), STATUS )
            CALL CCD1_LEXT( %VAL( IPDAT( I ) ), NREC( I ), NVAL( I ),
     :                      2, %VAL( IPWY ), STATUS )

*  Either just copy the coordinates or transform them using the WCS
*  component of the associated NDF.
            IF ( USEWCS ) THEN
               CALL AST_TRAN2( MAPS( I ), NREC( I ), %VAL( IPWX ),
     :                         %VAL( IPWY ), .TRUE., %VAL( IPXT ),
     :                         %VAL( IPYT ), STATUS )
               CALL CCG1_LAPND( %VAL( IPXT ), NREC( I ), IP,
     :                          %VAL( IPX ), STATUS )
               CALL CCG1_LAPND( %VAL( IPYT ), NREC( I ), IP, 
     :                          %VAL( IPY ), STATUS )
            ELSE
               CALL CCG1_LAPND( %VAL( IPWX ), NREC( I ), IP, 
     :                          %VAL( IPX ), STATUS )
               CALL CCG1_LAPND( %VAL( IPWY ), NREC( I ), IP, 
     :                          %VAL( IPY ), STATUS )
            END IF
            CALL CCG1_LAPNI( %VAL( IPIND( I ) ), NREC( I ), IP,
     :                       %VAL( IPID ), STATUS )
            IP = IP + NREC( I )
 3       CONTINUE

*  Do the first global transformation using the first positions
*  as the reference set.
         CALL CCD1_FITLM( %VAL( IPX ), %VAL( IPY ), %VAL ( IPID ),
     :                    %VAL( IPOK ), NREC, NOPEN, IPREF, FNAME,
     :                    IFIT, TOLER, TR, %VAL( IPXR ), %VAL( IPYR ),
     :                    %VAL( IPIDR ), %VAL( IPWX ), %VAL( IPWY ),
     :                    %VAL( IPWID ), NOUT, RMS, STATUS )

*  Store the transformation information.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop over each data set.
            DO 7 I = 1, NOPEN

               IF ( PLACE .EQ. 'NDF' ) THEN

*  Open the NDF.
                  CALL IRG_NDFEX( NDFGR, I, ID, STATUS )

                  IF ( OUTFM .EQ. 'WCS' ) THEN

*  Begin new AST context.
                     CALL AST_BEGIN( STATUS )

*  Get the WCS component of the NDF.
                     CALL CCD1_GTWCS( ID, IWCS, STATUS )

*  Get the original current frame index for future use.
                     JCUR = AST_GETI( IWCS, 'Current', STATUS )

*  Generate a mapping representing the linear transformation.
                     CALL CCD1_LNMAP( TR( 1, I ), MAPTFM, STATUS )

*  Generate a frame in domain called OUTDM, the purpose of which is to 
*  group the frames produced by this application.
                     FRREG = AST_FRAME( 2, ' ', STATUS )
                     CALL AST_SETC( FRREG, 'Domain', OUTDM, STATUS )
                     CALL AST_SETC( FRREG, 'Title', 
     :                              'Alignment by REGISTER', STATUS )

*  Ensure the Current frame is the one relative to which we've got the 
*  mapping.
                     IF ( .NOT. USEWCS ) THEN
                        CALL CCD1_FRDM( IWCS, 'Pixel', JPIX, STATUS )
                     END IF
                     
*  Add the OUTDM domain frame, with the appropriate mapping, to the 
*  WCS component of the NDF.  This will become the current frame.
                     CALL AST_ADDFRAME( IWCS, AST__CURRENT, MAPTFM, 
     :                                  FRREG, STATUS )

*  Get the index of the OUTDM frame.
                     JREG = AST_GETI( IWCS, 'Current', STATUS )

*  Now remove any previously existing frames in the OUTDM domain;
*  the output frameset should contain only one, to prevent confusion
*  (note CCD1_DMPRG updates JREG if necessary).
                     CALL CCD1_DMPRG( IWCS, OUTDM, .TRUE., JREG, 
     :                                STATUS )

*  Finally ensure that the registration domain is the Current one.
                     CALL AST_SETI( IWCS, 'Current', JREG, STATUS )

*  Write the modified WCS component back to NDF.
                     CALL NDF_PTWCS( IWCS, ID, STATUS )

*  End this AST context.
                     CALL AST_END( STATUS )

*  If we have not already written to a WCS component, write to a 
*  TRANSFORM structure instead.
                  ELSE IF ( OUTFM .EQ. 'TRANSFORM' ) THEN

*  Make sure that the CCDPACK extension exists.
                     CALL CCD1_CEXT( ID, .TRUE., 'UPDATE', LOCEXT, 
     :                               STATUS )

*  Does a transformation exist already?
                     CALL DAT_THERE( LOCEXT, 'TRANSFORM', THERE, 
     :                               STATUS )
                     IF ( THERE ) THEN

*  Need to remove old structure.
                        CALL DAT_ERASE( LOCEXT, 'TRANSFORM', STATUS )
                     END IF

*  Add the transformation - forward, inverse and classification.
                     CALL CCD1_WLTRN( TR( 1, I ), LOCEXT, FNAME( I ),
     :                                IFIT, STATUS )
                     CALL DAT_ANNUL( LOCEXT, STATUS )

                  END IF

*  Release NDF.
                  CALL NDF_ANNUL( ID, STATUS )

               ELSE

                  IF ( OUTFM .EQ. 'TRANSFORM' ) THEN

*  Putting all transformations into a container file.
                     INAME = 'TRN_'
                     CALL CHR_ITOC( I, INAME( 5: ), IAT )
                     CALL DAT_NEW( LOCEXT, INAME, 'TRANSFORM',
     :                             0, 0, STATUS )
                     CALL DAT_FIND( LOCEXT, INAME, ELOC, STATUS )
                     CALL CCD1_WLTRN( TR( 1, I ), ELOC, FNAME( I ),
     :                                IFIT, STATUS )
                     CALL DAT_ANNUL( ELOC, STATUS )

                  ELSE IF ( OUTFM .EQ. 'WCS' ) THEN

*  Exit with an error if we are trying to write a WCS component other
*  than into an NDF.
                     STATUS = SAI__ERROR
                     CALL ERR_REP( 'REGISTER_BADPAR', 
     : 'Output of a WCS structure must currently be to an NDF file',
     :                                STATUS )
                     GO TO 99
                  END IF

               END IF
 7          CONTINUE
         END IF

*  Do we need to write an output reference set?
         IF ( OUTREF ) THEN

*  Write it.  If the coordinates were transformed from pixel coordinates
*  in the first place they will need to be transformed back.
            IF ( USEWCS ) THEN
               CALL CCD1_MALL( NOUT, '_DOUBLE', IPXT, STATUS )
               CALL CCD1_MALL( NOUT, '_DOUBLE', IPYT, STATUS )
               CALL AST_TRAN2( MAPS( IPREF ), NOUT, %VAL( IPXR ),
     :                         %VAL( IPYR ), .FALSE., %VAL( IPXT ),
     :                         %VAL( IPYT ), STATUS )
            ELSE
               IPXT = IPXR
               IPYT = IPYR
            END IF
            CALL CCD1_WRIXY( FDREFO, %VAL( IPIDR ), %VAL( IPXT ),
     :                       %VAL( IPYT ), NOUT, LINE, CCD1__BLEN,
     :                       STATUS )

*  Tell user we have done so.
            CALL FIO_FNAME( FDREFO, LINE, STATUS )
            CALL MSG_SETC( 'FNAME', LINE )
            CALL CCD1_MSG( ' ',
     :      '  Extended reference positions written to file ^FNAME',
     :      STATUS )
         END IF
      ELSE

*  If FITTYPE is 6 then the user wants to supply their own
*  transformations. If this is the case we should ideally like to get
*  both the inverse and the forward transformation, but for user
*  convenience we'll allow the specification of just a single one.
         CALL PAR_GET0L( 'FULL', FULL, STATUS )

*  If the transformation is to be fitted fully get the forward and
*  the inverse transformations. Expand any sub-expressions in the
*  mappings. Note this application does not substitute parameters PA-PZ
*  these are what we will determine.
         CALL PAR_GET0C( 'XFOR', XFOR, STATUS )
         CALL PAR_GET0C( 'YFOR', YFOR, STATUS )
         CALL CCD1_GASTC( 'F', XFOR, STATUS )
         CALL CCD1_GASTC( 'F', YFOR, STATUS )
         IF ( FULL ) THEN
            CALL PAR_GET0C( 'XINV', XINV, STATUS )
            CALL PAR_GET0C( 'YINV', YINV, STATUS )
            CALL CCD1_GASTC( 'F', XINV, STATUS )
            CALL CCD1_GASTC( 'F', YINV, STATUS )
         END IF

*  Get the lengths of the expressions.
         XFORL = CHR_LEN( XFOR )
         YFORL = CHR_LEN( YFOR )
         IF ( FULL ) THEN
            XINVL = CHR_LEN( XINV )
            YINVL = CHR_LEN( YINV )
         END IF

*  Find out which parameter names are in the forward input expression by
*  attempting to replace each token with itself.
         NXPAR = 0
         DO 4 I = 1, CCD1__MTRNP
            NAME = 'P' // ALPBET( I :I )
            NTEMP = NAME

*  Look in forward transformation.
            CALL TRN_STOK( NAME, NTEMP, XFOR( :XFORL ), NSUBSF, STATUS )

*  Now in the inverse transformation.
            IF ( FULL ) CALL TRN_STOK( NAME, NTEMP, XFOR( :XFORL ),
     :                                 NSUBSI, STATUS )

*  The number of substitutions should be the same as the
*  parameterisations are the same.
            IF ( .NOT. FULL .OR. ( NSUBSF .EQ. NSUBSI ) .OR.
     :           ( NSUBSF .GT. 0 .AND. NSUBSI .GT. 0 ) ) THEN

*  Ok to proceed, record this value.
               IF ( NSUBSF .NE. 0 ) THEN
                  NXPAR = NXPAR + 1
                  XPARNM( NXPAR ) = NAME
               END IF
            ELSE

*  There is an inconsistency in the transformations.
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'PARAM', NAME )
               CALL ERR_REP( 'REGISTER_MAPINC',
     : '  The X forward and inverse transformations are inconsistent'//
     : '(parameter ^PARAM)', STATUS )
            END IF
            IF ( STATUS .NE. SAI__OK ) GO TO 99
 4       CONTINUE

*  Now do this for the Y expression.
         NYPAR = 0
         DO 5 I = 1, CCD1__MTRNP
            NAME = 'P' // ALPBET( I :I )
            NTEMP = NAME
            CALL TRN_STOK( NAME, NTEMP, YFOR( :YFORL ), NSUBSF, STATUS )
            IF ( FULL ) CALL TRN_STOK( NAME, NTEMP, YINV( :YINVL ),
     :                                 NSUBSI, STATUS )

*  The number of substitutions should be either zero or non-zero.
            IF ( .NOT. FULL .OR. ( NSUBSF .EQ. NSUBSI ) .OR.
     :           ( NSUBSF .GT. 0 .AND. NSUBSI .GT. 0 ) ) THEN

*  If a parameter name appeared then record it.
               IF ( NSUBSF .NE. 0 ) THEN
                  NYPAR = NYPAR + 1
                  YPARNM( NYPAR ) = NAME
               END IF
            ELSE

*  There is an inconsistency in the transformations.
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'PARAM', NAME )
               CALL ERR_REP( 'REGISTER_MAPINC',
     : '  The Y forward and inverse transformations are inconsistent'//
     : '(parameter ^PARAM)', STATUS )
            END IF
            IF ( STATUS .NE. SAI__OK ) GO TO 99
 5       CONTINUE

*  Look for the coordinates X and Y in the forward transformations.
         HAVXXF = .FALSE.
         HAVXYF = .FALSE.
         CALL TRN_STOK( 'X', 'X', XFOR( :XFORL ), NSUBS, STATUS )
         IF ( NSUBS .NE. 0 ) HAVXXF = .TRUE.
         CALL TRN_STOK( 'Y', 'Y', XFOR( :XFORL ), NSUBS, STATUS )
         IF ( NSUBS .NE. 0 ) HAVXYF = .TRUE.
         HAVYXF = .FALSE.
         HAVYYF = .FALSE.
         CALL TRN_STOK( 'X', 'X', YFOR( :YFORL ), NSUBS, STATUS )
         IF ( NSUBS .NE. 0 ) HAVYXF = .TRUE.
         CALL TRN_STOK( 'Y', 'Y', YFOR( :YFORL ), NSUBS, STATUS )
         IF ( NSUBS .NE. 0 ) HAVYYF = .TRUE.

*  If none present in the forward transformation warn user.
         IF ( .NOT. ( ( HAVXXF .OR. HAVXYF ) .AND.
     :                ( HAVYXF .OR. HAVYYF ) ) ) THEN
            CALL MSG_OUT( 'REGISTER_BADTRAN',
     :      '  One of the forward transformations does not'//
     :      ' contain a reference to X or Y', STATUS )
         END IF

*  Look for the tokens XX and YY in the inverse transformations.
         IF ( FULL ) THEN
            HAVXXI = .FALSE.
            HAVXYI = .FALSE.
            CALL TRN_STOK( 'XX', 'XX', XINV( :XINVL ), NSUBS, STATUS )
            IF ( NSUBS .NE. 0 ) HAVXXI = .TRUE.
            CALL TRN_STOK( 'YY', 'YY', XINV( :XINVL ), NSUBS, STATUS )
            IF ( NSUBS .NE. 0 ) HAVXYI = .TRUE.
            HAVYXI = .FALSE.
            HAVYYI = .FALSE.
            CALL TRN_STOK( 'XX', 'XX', YINV( :YINVL ), NSUBS, STATUS )
            IF ( NSUBS .NE. 0 ) HAVYXI = .TRUE.
            CALL TRN_STOK( 'YY', 'YY', YINV( :YINVL ), NSUBS, STATUS )
            IF ( NSUBS .NE. 0 ) HAVYYI = .TRUE.

*  If none present in the forward transformation warn user.
            IF ( .NOT. ( ( HAVXXI .OR. HAVXYI ) .AND.
     :                 ( HAVYXI .OR. HAVYYI ) ) ) THEN
               CALL MSG_OUT( 'REGISTER_BADTRAN',
     :         '  One of the inverse transformations does not'//
     :         ' contain a reference to XX or YY', STATUS )
            END IF
         END IF

*  Finally add the XX= and YY= to finish the mappings.
         FORMAP( 1 ) = 'XX='
         IAT = 3
         CALL CHR_APPND( XFOR( :XFORL ), FORMAP( 1 ), IAT )
         FORMAP( 2 ) = 'YY='
         IAT = 3
         CALL CHR_APPND( YFOR( :YFORL ), FORMAP( 2 ), IAT )
         IF ( FULL ) THEN
            INVMAP( 1 ) = 'X='
            IAT = 2
            CALL CHR_APPND( XINV( :XINVL ), INVMAP( 1 ), IAT )
            INVMAP( 2 ) = 'Y='
            IAT = 2
            CALL CHR_APPND( YINV( :YINVL ), INVMAP( 2 ), IAT )
         END IF

*  Create a list of parameter tokens which is unique, no duplication is
*  allowed when determining the best fit.
         CALL CCD1_MALL( NREC( 1 ) + NREC( 2 ), '_LOGICAL', IPOK,
     :                   STATUS )
         CALL CCD1_RMULO( XPARNM, NXPAR, YPARNM, NYPAR, %VAL( IPOK ),
     :                    UNIPAR, NUMUNI, STATUS )
         CALL CCD1_MFREE( IPOK, STATUS )

*  Now that we have the parameter's names we need initial guesses at
*  their values.
         DO 12 I = 1, NUMUNI
            CALL PAR_GET0D( UNIPAR( I ), FORVAL( I ), STATUS )
 12      CONTINUE

*  Extract the X and Y positions from the input mapped data areas.
         CALL CCD1_MALL( NREC( 1 ), '_DOUBLE', IPX, STATUS )
         CALL CCD1_MALL( NREC( 1 ), '_DOUBLE', IPY, STATUS )
         CALL CCD1_MALL( NREC( 2 ), '_DOUBLE', IPXR, STATUS )
         CALL CCD1_MALL( NREC( 2 ), '_DOUBLE', IPYR, STATUS )
         CALL CCD1_LEXT( %VAL( IPDAT( 1 ) ), NREC( 1 ), NVAL( 1 ),
     :                   1, %VAL( IPX ), STATUS )
         CALL CCD1_LEXT( %VAL( IPDAT( 1 ) ), NREC( 1 ), NVAL( 1 ),
     :                   2, %VAL( IPY ), STATUS )
         CALL CCD1_LEXT( %VAL( IPDAT( 2 ) ), NREC( 2 ), NVAL( 2 ),
     :                   1, %VAL( IPXR ), STATUS )
         CALL CCD1_LEXT( %VAL( IPDAT( 2 ) ), NREC( 2 ), NVAL( 2 ),
     :                   2, %VAL( IPYR ), STATUS )

*  Transform them into the correct coordinates if necessary.
         IF ( USEWCS ) THEN 
            CALL CCD1_MALL( NREC( 1 ), '_DOUBLE', IPXT, STATUS )
            CALL CCD1_MALL( NREC( 1 ), '_DOUBLE', IPYT, STATUS )
            CALL CCD1_MALL( NREC( 2 ), '_DOUBLE', IPXRT, STATUS )
            CALL CCD1_MALL( NREC( 2 ), '_DOUBLE', IPYRT, STATUS )
            CALL AST_TRAN2( MAPS( 1 ), NREC( 1 ), %VAL( IPX ), 
     :                      %VAL( IPY ), .TRUE., %VAL( IPXT ),
     :                      %VAL( IPYT ), STATUS )
            CALL AST_TRAN2( MAPS( 2 ), NREC( 2 ), %VAL( IPXR ),
     :                      %VAL( IPYR ), .TRUE., %VAL( IPXRT ),
     :                      %VAL( IPYRT ), STATUS )
            IPX = IPXT
            IPY = IPYT
            IPXR = IPXRT
            IPYR = IPYRT
         END IF
        
*  Do the transformation fit. If a reference list has been given then we
*  want to work out the transformation from the other list (stored in
*  position 1) to this. If two lists have been given the first list is
*  assumed to be the "reference" list and a transformation from the
*  second list to this is determined.
         IF ( IPREF .EQ. 2 ) THEN
            CALL CCD1_FITG( FORMAP, 'X', 'Y', UNIPAR, NUMUNI,
     :                      TOLER, IPIND( 2 ), IPXR, IPYR, NREC( 2 ),
     :                      IPIND( 1 ), IPX, IPY, NREC( 1 ),
     :                      FORVAL, STATUS )
         ELSE
            CALL CCD1_FITG( FORMAP, 'X', 'Y', UNIPAR, NUMUNI,
     :                      TOLER, IPIND( 1 ), IPX, IPY, NREC( 1 ),
     :                      IPIND( 2 ), IPXR, IPYR, NREC( 2 ),
     :                      FORVAL, STATUS )
         END IF

*  Derive the inverse transformation parameterisations by performing a
*  second fit. This provides protection against any ill-conditioning in
*  the equations and allows a check for this to be performed.
         IF ( FULL ) THEN

*  Use the parameter values from the first try as guesses for these
*  values.
            DO 13 I = 1, NUMUNI
               INVVAL( I ) = FORVAL( I )
 13         CONTINUE
            IF ( IPREF .EQ. 2 ) THEN
               CALL CCD1_FITG( INVMAP, 'XX', 'YY', UNIPAR, NUMUNI,
     :                         TOLER, IPIND( 2 ), IPX, IPY, NREC( 1 ),
     :                         IPIND( 1 ), IPXR, IPYR, NREC( 2 ),
     :                         INVVAL, STATUS )
            ELSE
               CALL CCD1_FITG( INVMAP, 'XX', 'YY', UNIPAR, NUMUNI,
     :                         TOLER, IPIND( 1 ), IPXR, IPYR, NREC( 2 ),
     :                         IPIND( 2 ), IPX, IPY, NREC( 1 ),
     :                         INVVAL, STATUS )
            END IF
         END IF

*  If necessary check the transformations for conditioning.
         IF ( FULL ) CALL CCD1_CKCON( FORVAL, INVVAL, NUMUNI, STATUS )

*  See if the user wants to supply a classification for the
*  transformation.
 21      CONTINUE
         CALL PAR_GET0L( 'CLASSIFY', DOCLAS, STATUS )
         IF ( DOCLAS ) THEN
            CALL PAR_CHOIV( 'CLASS', TRN__MXCLS,
     :                      'LINEAR,INDEPENDENT,DIAGONAL,'//
     :                      'ISOTROPIC,POSITIVE_DET,NEGATIVE_DET,'//
     :                      'CONSTANT_DET,UNIT_DET',
     :                      CLASES, NCLASS, STATUS )

*  Convert these into a logical array.
            DO 87 I = 1, TRN__MXCLS
               CLASS( I ) = .FALSE.
 87         CONTINUE
            DO 88 I = 1, NCLASS
               IF ( CLASES( I ) .EQ. 'LINEAR' ) THEN
                  CLASS( TRN__LIN ) = .TRUE.
               ELSE IF ( CLASES( I ) .EQ. 'INDEPENDENT' ) THEN
                  CLASS( TRN__INDEP ) = .TRUE.
               ELSE IF ( CLASES( I ) .EQ. 'DIAGONAL' ) THEN
                  CLASS( TRN__DIAG ) = .TRUE.
               ELSE IF ( CLASES( I ) .EQ. 'ISOTROPIC' ) THEN
                  CLASS( TRN__ISOT ) = .TRUE.
               ELSE IF ( CLASES( I ) .EQ. 'POSITIVE_DET' ) THEN
                  CLASS( TRN__POSDT ) = .TRUE.
               ELSE IF ( CLASES( I ) .EQ. 'NEGATIVE_DET' ) THEN
                  CLASS( TRN__NEGDT ) = .TRUE.
               ELSE IF ( CLASES( I ) .EQ. 'CONSTANT_DET' ) THEN
                  CLASS( TRN__CONDT ) = .TRUE.
               ELSE IF ( CLASES( I ) .EQ. 'UNIT_DET' ) THEN
                  CLASS( TRN__UNIDT ) = .TRUE.
               END IF
 88         CONTINUE
         END IF

*  Now store the transformation. One the of the transforms is the just
*  the identity, the other is the derived one.
         DO 8 I = 1, 2
            IF ( PLACE .EQ. 'NDF' ) THEN

*  Get the NDF and access the MORE extension.
               CALL IRG_NDFEX( NDFGR, I, ID, STATUS )

*  Make sure that the CCDPACK extension exists.
               CALL CCD1_CEXT( ID, .TRUE., 'UPDATE', LOCEXT, STATUS )

*  Does a transformation exist already?
               CALL DAT_THERE( LOCEXT, 'TRANSFORM', THERE, STATUS )
               IF ( THERE ) THEN

*  Need to remove old structure.
                  CALL DAT_ERASE( LOCEXT, 'TRANSFORM', STATUS )
               END IF

*  Add the transformation - forward, inverse and classification.
               IF ( I .EQ. IPREF ) THEN

*  The identity.
                  CALL CCD1_WLTRN( IDEN, LOCEXT, FNAME( I ), 1, STATUS )
               ELSE
                  CALL CCD1_WGTRN( LOCEXT, FORMAP, INVMAP, UNIPAR,
     :                             NUMUNI, FORVAL, INVVAL, FULL,
     :                             DOCLAS, CLASS, FNAME( I ), STATUS )
               END IF

*  Release NDF.
               CALL DAT_ANNUL( LOCEXT, STATUS )
               CALL NDF_ANNUL( ID, STATUS )
            ELSE

*  Putting all transformations into a container file.
               INAME = 'TRN_'
               CALL CHR_ITOC( I, INAME( 5: ), IAT )
               CALL DAT_NEW( LOCEXT, INAME, 'TRANSFORM',
     :                       0, 0, STATUS )
               CALL DAT_FIND( LOCEXT, INAME, ELOC, STATUS )

*  Write the appropriate transformation.
               IF ( I .EQ. IPREF ) THEN

*  The identity.
                  CALL CCD1_WLTRN( IDEN, ELOC, FNAME( I ), 1, STATUS )
               ELSE
                  CALL CCD1_WGTRN( ELOC, FORMAP, INVMAP, UNIPAR,
     :                             NUMUNI, FORVAL, INVVAL, FULL,
     :                             DOCLAS, CLASS, FNAME( I ), STATUS )
               END IF
               CALL DAT_ANNUL( ELOC, STATUS )
            END IF
 8       CONTINUE
      END IF

*  Exit with error label. Tidy up after this.
 99   CONTINUE

*  Close reference positions, if open.
      IF ( OUTREF ) CALL FIO_CLOSE( FDREFO, STATUS )

*  Release all remaining workspace.
      CALL CCD1_MFREE( -1, STATUS )

*  Close IRH (and IRG).
      CALL IRH_CLOSE( STATUS )

*  End AST context.
      CALL AST_END( STATUS )

*  Release the container file with transforms.
      IF ( PLACE .NE. 'NDF' ) CALL DAT_ANNUL( LOCEXT, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'REGISTER_ERR',
     :   'REGISTER: Error determining transformation.',
     :   STATUS )
      END IF

*  Close down logging system.
      CALL CCD1_END( STATUS )

      END
* $Id$
