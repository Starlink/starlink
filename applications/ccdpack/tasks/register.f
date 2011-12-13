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
*     and then coded as new coordinate systems attached to NDFs.
*     Normally, the new coordinate systems will be attached to the
*     NDFs with which the lists are associated, but if the lists are
*     not associated with NDFs then they can be attached to a named
*     list of NDFs, or a single named one.  The new coordinate system
*     is a copy of the Pixel coordinate system of the refernce image,
*     and so is guaranteed to be a sensible one in which to resample.
*     The resampling can be done by TRANNDF.

*  Usage:
*     register inlist fittype refpos

*  ADAM Parameters:
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
*        This parameter is only used when IFIT=6.
*     FITTYPE = _INTEGER (Read)
*        The type of fit which should be used when determining the
*        transformation between the input positions lists. This may take
*        the values
*           - 1 -- shift of origin
*           - 2 -- shift of origin and rotation
*           - 3 -- shift of origin and magnification
*           - 4 -- shift of origin, rotation and magnification (solid body)
*           - 5 -- a full six parameter fit
*           - 6 -- self defined function
*
*        If more than two position lists are provided, then only the
*        values 1-5 may be used.
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
*        If NDFNAMES is FALSE and PLACEIN is "EACH" then a list of NDF
*        names in which to store the WCS frames is required.  This
*        list of names must correspond  exactly to the order of the
*        associated input lists. A listing of the order of inputs is
*        shown before this parameter is accessed.
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
*        extension then the new coordinate system is attached to
*        the associated NDF.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.
*        [TRUE]
*     OUTDOMAIN = LITERAL (Read)
*        The transformation information is written as a new coordinate
*        system attached to the NDF.  This parameter gives the label
*        (domain) of the new coordinate system.  When the new
*        coordinate system is added, any previously existing one with
*        the same Domain will be removed.
*
*        If PLACEIN is "SINGLE", then the new coordinate systems are all
*        attached to a single NDF.  In this case the  domains are
*        OUTDOMAIN_1, OUTDOMAIN_2, ....
*
*        The name is converted to upper case, and whitespace is removed.
*        [CCD_REG]
*     PA-PZ = LITERAL (Read)
*        When FITTYPE is 6 these parameters are used for supplying initial
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
*           - EACH    -- attach them one per NDF in a set of NDFs
*           - SINGLE  -- attach them all to a single NDF
*
*        If the EACH option is chosen then you will have the option of
*        supplying the NDF names via the parameter IN. If the SINGLE
*        option is chosen then the name of an NDF should be
*        given in response to the WCSFILE parameter; if no NDF by this
*        name exists, a new dummy one will be created.
*        [EACH]
*     REFPOS = _INTEGER (Read)
*        The position within the list of inputs which corresponds to
*        the list to be used as the reference set.
*        [1]
*     SIMPFI = _LOGICAL (Read)
*        If FITTYPE=6 and FULL=TRUE, this gives the value of the
*        mapping's SimpFI attribute (whether it is legitimate to simplify
*        the forward followed by the inverse transformation to a unit
*        transformation).
*        [TRUE]
*     SIMPIF = _LOGICAL (Read)
*        If FITTYPE=6 and FULL=TRUE this gives the value of the
*        mapping's SimpIF attribute (whether it is legitimate to simplify
*        the inverse followed by the forward transformation to a unit
*        transformation).
*        [TRUE]
*     TOLER = _DOUBLE (Read)
*        The RMS tolerance in positions which is used to determine the
*        best fit. Adjust this value only if the input positions are
*        specified in coordinates with a higher accuracy or smaller
*        units.
*        [0.001]
*     USESET = _LOGICAL (Read)
*        This parameter determines whether Set header information should
*        be used in the registration.  If USESET is true, then
*        REGISTER will try to group position lists according to
*        the Set Name attribute of the NDFs to which they are attached.
*        All lists coming from NDFs which share the same (non-blank)
*        Set Name attribute, and which have a CCD_SET coordinate
*        frame in their WCS component, will be grouped together and
*        treated by the program as a single position list.  Images
*        which have no associated position list but are in the same
*        Set as ones which are successfully registered will have
*        a suitable registration frame added too, based on their
*        Set alignment relation to the registered Set member.
*        Thus the assumption is made that the relative alignment of
*        images within a Set is already known and has been fixed.
*
*        If USESET is false, all Set header information is ignored.
*        If NDFNAMES is false, USESET will be ignored.  If the input
*        NDFs have no Set headers, or if they have no CCD_SET frame
*        in their WCS components, the setting of USESET will make
*        no difference.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.
*        [FALSE]
*     USEWCS = _LOGICAL (Read)
*        This parameter specifies whether the coordinates in the
*        position lists should be transformed from Pixel coordinates
*        into the Current coordinate system of the associated NDF
*        before use.  It should normally be set TRUE, in which case
*        the transformation type set by the FITTYPE parameter is the
*        type which will be fit between the Current coordinate systems
*        of the NDFs.  Otherwise the fit will be between the positions
*        in pixel coordinates.
*
*        This parameter is ignored if NDFNAMES is not TRUE.
*        [TRUE]
*     WCSFILE = NDF (Read)
*        If PLACEIN is "SINGLE" then the value of this parameter gives the
*        the name of an NDF which will have the new coordinate systems
*        attached to it.  They will be added with domains given by the
*        OUTDOMAIN parameter with '_1', '_2', ... appended.  If the NDF
*        named by this parameter does not exist, a dummy one will be
*        created.
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
*        accessed and their associated position lists are opened.  A
*        global fit between all the datasets is then performed which
*        results in estimates for the offsets from the first input
*        NDF's position.  These offsets are between the Current
*        coordinate systems of the NDFs.  The results are then
*        attached as new coordinate systems, labelled 'CCD_REG', in
*        the WCS component of the NDFs.  Actual registration of the
*        images  can then be achieved by aligning all the NDFs in the
*        CCD_REG domain using TRANNDF.
*
*     register inlist='*' fittype=5 outdomain=result-set1
*        This example works as above but this time the global
*        transformations are derived for a full 6-parameter linear fit
*        (which allows offset, rotation, magnification and shear).
*        The results are coded as attached coordinate systems labelled
*        'RESULT-SET1'.
*
*     register inlist='"myimage1,myimage2"' fittype=4 refpos=2
*        In this example a solid body fit is performed between the
*        position lists associated with the NDFs myimage1 and myimage2.
*        The reference positions are chosen to be those associated with
*        myimage2, so that the CCD_REG coordinates will be the
*        same as the pixel coordinates of NDF myimage2.
*
*     register inlist='"one,two"' fittype=6 xfor='pa+pb*x' yfor='pa+pb*y'
*        In this example the position lists associated with the NDFs
*        one and two are said to be related by the algebraic
*        expressions "pa+pb*x" and "pa+pb*y", which indicates that a
*        single offset applies in both directions and a single scale
*        factor. A solution for the values PA and PB is found using a
*        general least-squares minimization technique. Starting values
*        for PA and PB can be given using the parameters PA and PB.
*        Since the fittype is 6, only two position lists may be
*        registered in the same run.
*
*     register inlist='"ndf1,ndf2"' fittype=6 xfor='pa+pb*x+pc*y+pd*x*y'
*              yfor='pe+pf*x+pg*y+ph*x*y'
*        In this example a non-linear transformation is fit between the
*        positions associated with the NDFs ndf1 and ndf2. This analysis
*        may help in determining whether a 6-parameter fit is good
*        enough, or if you just want to transform positions. A problem
*        with proceeding with this transformation in a general fashion
*        is deriving the inverse as this is required if you want to
*        perform image resampling using TRANNDF (though the more
*        specialised, and less efficient, DRIZZLE can resample with
*        only the forward transformation).
*
*     register ndfnames=false inlist='"list1.acc,list2.acc,list3.acc"'
*              fittype=3 placein=each in='"ndf1,ndf2,ndf3"'
*        In this example the input position lists are not associated
*        with NDFs (ndfnames=false) and have to be specified by name
*        (no wildcards allowed). Since the position lists are not
*        associated with NDFs there is no natural home for the
*        new coordinate systems. In this example it has been decided to
*        attach the coordinate systems to a set of NDFs anyway.
*        PLACEIN could also be given as "SINGLE" in which case the
*        coordinate systems would be attached to a single NDF with
*        Domain names CCD_REG_1, CCD_REG_2, ...

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
*       On exit, a new coordinate frame with a Domain as given by the
*       OUTDOMAIN parameter will be inserted in the WCS component of
*       the input NDFs.  Taken together these contain the registration
*       information and can be inspected using WCSEDIT.

*  Behaviour of Parameters:
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
*     Certain parameters (LOGTO, LOGFILE, NDFNAMES and USESET) have global
*     values. These global values will always take precedence, except
*     when an assignment is made on the command line.  Global values may
*     be set and reset using the CCDSETUP and CCDCLEAR commands.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1995-2003 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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
*     1-NOV-1999 (MBT):
*        Modified so that output is in units appropriate to Current
*        coordinate frame.
*     12-NOV-1999 (MBT):
*        Modified so that general transformations can be output into
*        WCS components as MathMaps.
*     15-NOV-1999 (MBT):
*        Removed the possibility to write results as TRANSFORM structures.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     22-FEB-2001 (MBT):
*        Upgraded for use with Sets.
*     12-APR-2001 (MBT):
*        Modified back again so that the CCD_REG frame is a copy of the
*        reference NDF's pixel frame.  This is the second time I've
*        changed it to do this and changed it back again.  This is
*        the right way to do it! (so that the CCD_REG frame is guaranteed
*        suitable for resampling), don't be tempted to change it back
*        to a copy of the Current frame again.
*     22-MAY-2001 (MBT):
*        Changed to use empty position list files instead of no file at
*        all when there are no positions.
*     8-MAY-2002 (MBT):
*        Fixed coredumping bug for FITTYPE=6.
*     12-FEB-2003 (MBT):
*        Modified to create a new NDF using WCSFILE parameter when
*        PLACEIN=SINGLE if named NDF does not already exist.
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
      INCLUDE 'NDF_PAR'          ! NDF parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string excluding trailing blanks

*  Local Variables:
      CHARACTER * ( 6 ) PLACE   ! Place to store transformations
      CHARACTER * ( AST__SZCHR ) OUTDM ! Name of domain for new output WCS frame
      CHARACTER * ( AST__SZCHR ) DMN ! Current domain of WCS component
      CHARACTER * ( CCD1__BLEN ) LINE ! Buffer for reading data
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
      CHARACTER * ( FIO__SZFNM ) FNAME ! List filename
      DOUBLE PRECISION FORVAL( CCD1__MTRNP ) ! Forward general coefficients
      DOUBLE PRECISION INVVAL( CCD1__MTRNP ) ! Inverse general coefficients
      DOUBLE PRECISION RMS      ! Root mean square difference in fitted lists and reference list
      DOUBLE PRECISION TOLER    ! Tolerance in RMS devations
      DOUBLE PRECISION TR( 6, CCD1__MXLIS + 1 ) ! The transformation coeffs
      INTEGER BND1( 2 )         ! Bounds of dummy NDF
      INTEGER ELDUM             ! Number of elements in dummy data array
      INTEGER FDIN              ! FIO file descriptors
      INTEGER FDREFO            ! Output reference set descriptor
      INTEGER FIOGR             ! Group of input list names
      INTEGER FRMS( CCD1__MXLIS ) ! AST pointers to Current coordinate frames
      INTEGER FRREG             ! AST pointer to frame in OUTDM domain
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Position within string
      INTEGER ID                ! NDF identifier
      INTEGER IDWCS             ! NDF identifier for frameset container file
      INTEGER IFIT              ! The fittype
      INTEGER ILIS( CCD1__MXLIS + 1 ) ! Array of list indices in superlist order
      INTEGER ILISOF( CCD1__MXLIS + 2 ) ! Offsets into ILIS for each superlist
      INTEGER INLSUP( CCD1__MXLIS + 1 ) ! Superlist index for NDFs without lists
      INTEGER IP                ! Positional pointer into appended lists
      INTEGER IPDAT( CCD1__MXLIS + 1 ) ! Pointer to file data
      INTEGER IPDUM             ! Pointer to dummy data component
      INTEGER IPID              ! Pointer to identifier workspace
      INTEGER IPIDR             ! Pointer to identifier workspace
      INTEGER IPIG( 2 )         ! Pointer to identifier workspace
      INTEGER IPIND( CCD1__MXLIS + 1 ) ! Pointer to file identifiers
      INTEGER IPOK              ! Pointer to logical workspace
      INTEGER IPREF             ! Points to position of reference list
      INTEGER IPREFS            ! Points to position of reference superlist
      INTEGER IPWID             ! Pointer to identifier workspace
      INTEGER IPWX              ! Pointer to X positions workspace
      INTEGER IPWY              ! Pointer to Y positions workspace
      INTEGER IPX               ! Pointer to X positions workspace
      INTEGER IPXR              ! Pointer to X positions workspace
      INTEGER IPXG( 2 )         ! Pointer to X positions workspace
      INTEGER IPXT              ! Pointer to temporary X positions
      INTEGER IPY               ! Pointer to Y positions workspace
      INTEGER IPYR              ! Pointer to Y positions workspace
      INTEGER IPYG( 2 )         ! Pointer to X positions workspace
      INTEGER IPYT              ! Pointer to temporary Y positions
      INTEGER ISUP( CCD1__MXLIS + 1 ) ! Superlist index for each list
      INTEGER IWCS              ! AST pointer to WCS component of NDF
      INTEGER J                 ! Loop variable
      INTEGER JCUR              ! Incdex of (original) Current frame in frameset
      INTEGER JPIX              ! Index of Pixel domain frame in frameset
      INTEGER JREG              ! Index of OUTDM domain frame in frameset
      INTEGER JSET              ! Index of CCD_SET domain frame in frameset
      INTEGER L                 ! List index variable
      INTEGER MAPCP             ! AST mapping from Current to pixel frame of ref
      INTEGER MAPS( CCD1__MXLIS + 1 ) ! AST Mapping from pixel to Current frames
      INTEGER MAPSET( CCD1__MXLIS + 1 ) ! AST Mapping from CCD_SET frame
      INTEGER MAPTFM( CCD1__MXLIS + 1 ) ! AST pointers for Pixel - OUTDM mapping
      INTEGER NDFGR             ! Group of input NDF names
      INTEGER NLGR              ! GRP id for NDFs with no associated list
      INTEGER NNDF              ! Number of NDFs accessed
      INTEGER NNOLIS            ! Number of NDFs without associated lists
      INTEGER NOPEN             ! Number of input lists opened
      INTEGER NOUT              ! Number of positions in list
      INTEGER NREC( CCD1__MXLIS + 1 ) ! Number of records read from file
      INTEGER NRECS( CCD1__MXLIS + 1 ) ! Number of records in superlist
      INTEGER NSUBS             ! Number of token substitutions
      INTEGER NSUBSF            ! Number of token substitutions
      INTEGER NSUBSI            ! Number of token substitutions
      INTEGER NSUP              ! Number of superlists
      INTEGER NTOT              ! Total number of input records
      INTEGER NUMUNI            ! Number of unique parameter names
      INTEGER NVAL( CCD1__MXLIS + 1 ) ! Number of values in file
      INTEGER NVAL1             ! Number of values in tested file
      INTEGER NXPAR             ! Number of parameters in X mapping
      INTEGER NYPAR             ! Number of parameters in Y mapping
      INTEGER OTHER             ! Opposite of IPREFS
      INTEGER XFORL             ! Length of X mapping string
      INTEGER XINVL             ! Length of X mapping string
      INTEGER YFORL             ! Length of Y mapping string
      INTEGER YINVL             ! Length of Y mapping string
      LOGICAL FULL              ! True if a full general transformation is used
      LOGICAL HAVXXF            ! Flags indicating that mapping
      LOGICAL HAVXXI            ! Flags indicating that mapping
      LOGICAL HAVXYF            ! expression contain
      LOGICAL HAVXYI            ! expression contain
      LOGICAL HAVYXF            ! references to X and Y
      LOGICAL HAVYXI            ! references to X and Y
      LOGICAL HAVYYF            !
      LOGICAL HAVYYI            !
      LOGICAL SIMPFI            ! OK to simplify forward then inverse mapping?
      LOGICAL SIMPIF            ! OK to simplify inverse then forward mapping?
      LOGICAL NDFS              ! True if list names came from NDF extensions
      LOGICAL OUTREF            ! Output reference required
      LOGICAL USESET            ! Use Set header information?
      LOGICAL USEWCS            ! Are we transforming to current frame

*  Local Data:
      DATA ALPBET/ 'ABCDEFGHIJKLNMOPQRSTUVWXYZ' /
      DATA BND1/ 1, 1 /
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start up the CCDPACK logging system.
      CALL CCD1_START( 'REGISTER', STATUS )

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Output reference set file is not open.
      OUTREF = .FALSE.

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

*  See if we will use Set header information to group lists into
*  superlists.
      USESET = .FALSE.
      IF ( NDFS ) THEN
         USESET = .TRUE.
         CALL PAR_GET0L( 'USESET', USESET, STATUS )
      END IF

*  Get the names of all the input lists.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL CCD1_GTLIG( NDFS, 'CURRENT_LIST', 'INLIST', 2, CCD1__MXLIS,
     :                 .TRUE., NOPEN, FIOGR, NDFGR, NNOLIS, NLGR,
     :                 STATUS )

*  Get coordinate and Set information about lists.
      CALL CCD1_SWLIS( NDFGR, FIOGR, NOPEN, NLGR, NNOLIS, NDFS, USEWCS,
     :                 USESET, FRMS, MAPS, MAPSET, ISUP, NSUP, ILIS,
     :                 ILISOF, INLSUP, STATUS )

*  Warn the user if some of the input NDF names will be ignored.
      IF ( .NOT. USESET .AND. NNOLIS .GT. 0 ) THEN
         CALL MSG_SETI( 'NNOLIS', NNOLIS )
         CALL CCD1_MSG( ' ', '  ^NNOLIS NDFs will be ignored since'//
     :   ' they have no position lists.', STATUS )
      END IF

*  Find out which type of transformation the user requires.  If there
*  are more than two input superlists then a fittype of 6 is not
*  allowed.
 3    CONTINUE
      CALL PAR_GET0I( 'FITTYPE', IFIT, STATUS )
      IF ( NSUP .GT. 2 .AND. IFIT .EQ. 6 ) THEN
         CALL MSG_OUT( ' ', '  With more than two input lists, IFIT=6'
     :                 //' is not allowed.', STATUS )
         CALL PAR_CANCL( 'FITTYPE', STATUS )
         GO TO 3
      END IF
      IF ( NSUP .GT. 2 ) THEN
         IFIT = MAX( 1, MIN( 5, IFIT ) )
      ELSE
         IFIT = MAX( 1, MIN( 6, IFIT ) )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Get the position of the reference set in the input superlists.
      IPREFS = 1
      CALL PAR_GET0I( 'REFPOS', IPREFS, STATUS )
      IPREFS = MAX( 1, MIN( IPREFS, NSUP ) )
      IPREF = ILIS( ILISOF( IPREFS ) )

*  Get the mapping between the Current and the PIXEL-domain frames
*  of the reference NDF.
      IF ( USEWCS ) THEN
         MAPCP = AST_COPY( MAPS( IPREF ), STATUS )
         CALL AST_INVERT( MAPCP, STATUS )
      END IF

*  Where are we going to store the transformations? If NDFNAMES is true
*  then the we'll store them in the NDFs.  If NDFNAMES is not true then
*  find out were the user would like them. The two options are: in NDFs
*  (in which case a list of NDF names will be solicited using the
*  parameter 'IN'), or in a single container file.
      IF ( NDFS ) THEN
         PLACE = 'EACH'
      ELSE
         CALL PAR_CHOIC( 'PLACEIN', ' ', 'EACH,SINGLE', .FALSE., PLACE,
     :        STATUS )
         IF ( PLACE .EQ. 'EACH' ) THEN

*  User wants to place the results in some as yet unnamed NDFs.
*  Create a group containing the NDFs.
            CALL CCD1_NDFGU( NDFGR, NNDF, 'IN', NOPEN, NOPEN, STATUS )
         ELSE

*  User wants to store them as frames in an existing NDF's WCS component.
*  Get the identifier of this NDF if it already exists.
            CALL NDF_EXIST( 'WCSFILE', 'UPDATE', IDWCS, STATUS )

*  If it does not exist, create a dummy NDF to hold the WCS.
            IF ( IDWCS .EQ. NDF__NOID ) THEN

*  Create the NDF.
                CALL NDF_CREAT( 'WCSFILE', '_INTEGER', 2, BND1, BND1,
     :                          IDWCS, STATUS )

*  Ensure that its DATA component is in a defined state.
                CALL NDF_MAP( IDWCS, 'DATA', '_INTEGER', 'WRITE/BAD',
     :                        IPDUM, ELDUM, STATUS )
                CALL NDF_UNMAP( IDWCS, 'DATA', STATUS )

*  Assign informative values to its character components.
                CALL NDF_CPUT( 'Dummy NDF', IDWCS, 'TITLE', STATUS )
                CALL NDF_CPUT( 'Created by REGISTER to store WCS ' //
     :                         'information', IDWCS, 'LABEL', STATUS )
            END IF
         END IF
      END IF

*  We are writing to WCS components.  We need a domain name
*  for the new frame we will create.  Get it from the parameter system.
*  Spaces are removed and it is folded to upper case since this is how
*  the AST system will treat it.
      CALL PAR_GET0C( 'OUTDOMAIN', OUTDM, STATUS )
      CALL CHR_RMBLK( OUTDM )
      CALL CHR_UCASE( OUTDM )

*  Read the data values in the input lists into workspace.
      IF( STATUS .NE. SAI__OK ) GO TO 99
      DO I = 1, NSUP

*  Initialise the number of items in this superlist.
         NRECS( I ) = 0

*  Loop over each list which comprises this superlist.
         DO J = ILISOF( I ), ILISOF( I + 1 ) - 1
            L = ILIS( J )

*  Test the input files for the number of entries, prior to extracting
*  the position information.
            CALL GRP_GET( FIOGR, L, 1, FNAME, STATUS )
            CALL CCD1_OPFIO( FNAME, 'READ', 'LIST', 0, FDIN, STATUS )
            CALL CCD1_LTEST( FDIN, LINE, CCD1__BLEN, 2, 0, NVAL1,
     :                       STATUS )

*  If the position list has three or more columns of data then interpret
*  them as a standard list (identifier, x-position, y-position). If the
*  list has only 2 columns then interpret the inputs as just X and Y and
*  make up some identifiers (1,2,....NREC).
            IF ( NVAL1 .EQ. 2 ) THEN

*  Map in X and Y positions.
               CALL CCD1_NLMAP( FDIN, LINE, CCD1__BLEN, IPDAT( L ),
     :                          NREC( L ), NVAL( L ), STATUS )
               CALL CCD1_MALL( NREC( L ), '_INTEGER', IPIND( L ),
     :                         STATUS )
               CALL CCD1_GISEQ( NRECS( I ) + 1, 1, NREC( L ),
     :                          %VAL( CNF_PVAL( IPIND( L ) ) ), STATUS )
            ELSE

*  Standard file format...
               CALL CCD1_LMAP( FDIN, LINE, CCD1__BLEN, IPIND( L ),
     :                         IPDAT( L ), NREC( L ), NVAL( L ),
     :                         STATUS )
            END IF

*  Now close the file.
            CALL FIO_CLOSE( FDIN, STATUS )

*  Increment number of items in this superlist.
            NRECS( I ) = NRECS( I ) + NREC( L )
         END DO

*  Check that the superlist is not empty.
         IF ( NRECS( I ) .LE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'LIST', I )
            CALL ERR_REP( 'REGISTER_EMPTY',
     :                    'REGISTER: List ^LIST contains no points',
     :                    STATUS )
            GO TO 99
         END IF
      END DO

*  Does the user want the extended reference set of position written
*  out? If parameter state is null then no reference set will be
*  written.
      IF ( IFIT .NE. 6 .AND. STATUS .EQ. SAI__OK ) THEN
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
      IF ( PLACE .EQ. 'EACH' ) THEN
         CALL MSG_SETC( 'OUTDM', OUTDM )
         CALL CCD1_MSG( ' ', '  New coordinate systems labelled '//
     :   '^OUTDM will be written to each NDF', STATUS )
      ELSE
         CALL MSG_SETC( 'OUTDM', OUTDM )
         CALL MSG_SETC( 'OUTDM', '_1' )
         CALL NDF_MSG( 'OUTFILE', IDWCS )
         CALL CCD1_MSG( ' ', '  New coordinate systems labelled '//
     :   '^OUTDM ... will be written to ^IDWCS', STATUS )
      END IF

*  Get the tolerance which is required in the fit.
      CALL PAR_GET0D( 'TOLER', TOLER, STATUS )
      CALL MSG_SETD( 'TOLER', TOLER )
      CALL CCD1_MSG( ' ',
     :     '  Maximum change in positions is ^TOLER', STATUS )

*  Report the position of the reference list.
      IF ( USESET ) THEN
         CALL MSG_SETI( 'I', IPREFS )
         CALL CCD1_MSG( ' ', '  Reference Set is ^I)', STATUS )
         CALL GRP_GET( NDFGR, IPREF, 1, FNAME, STATUS )
         CALL MSG_SETC( 'NDF', FNAME )
         CALL CCD1_MSG( ' ', '  Reference NDF is ^NDF', STATUS )
      ELSE
         CALL MSG_SETI( 'I', IPREF )
         CALL CCD1_MSG( ' ', '  Reference list is ^I)', STATUS )
      END IF

*  Branch to deal with either linear transformations or user defined
*  transformations.
      IF ( IFIT .LE. 5 ) THEN

*  Need to extract all the data and enter into a single appended list of
*  X positions and Y positions. First determine the number of entries
         NTOT = 0
         DO 2 I = 1, NSUP
            NTOT = NTOT + NRECS( I )
 2       CONTINUE

*  Get the workspace for the lists.
         CALL CCD1_MALL( NTOT, '_DOUBLE', IPX, STATUS )
         CALL CCD1_MALL( NTOT, '_DOUBLE', IPY, STATUS )
         CALL CCD1_MALL( NTOT, '_INTEGER', IPID, STATUS )
         CALL CCD1_MALL( NTOT, '_LOGICAL', IPOK, STATUS )
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
         DO I = 1, NSUP
            DO J = ILISOF( I ), ILISOF( I + 1 ) - 1
               L = ILIS( J )
               CALL CCD1_LEXT( %VAL( CNF_PVAL( IPDAT( L ) ) ),
     :                         NREC( L ), NVAL( L ),
     :                         1, %VAL( CNF_PVAL( IPWX ) ), STATUS )
               CALL CCD1_LEXT( %VAL( CNF_PVAL( IPDAT( L ) ) ),
     :                         NREC( L ), NVAL( L ),
     :                         2, %VAL( CNF_PVAL( IPWY ) ), STATUS )

*  Either just copy the coordinates or transform them using the WCS
*  component of the associated NDF.
               IF ( USEWCS ) THEN
                  CALL AST_TRAN2( MAPS( L ), NREC( L ),
     :                            %VAL( CNF_PVAL( IPWX ) ),
     :                            %VAL( CNF_PVAL( IPWY ) ), .TRUE.,
     :                            %VAL( CNF_PVAL( IPXT ) ),
     :                            %VAL( CNF_PVAL( IPYT ) ), STATUS )
                  CALL CCG1_LAPND( %VAL( CNF_PVAL( IPXT ) ),
     :                             NREC( L ), IP,
     :                             %VAL( CNF_PVAL( IPX ) ), STATUS )
                  CALL CCG1_LAPND( %VAL( CNF_PVAL( IPYT ) ),
     :                             NREC( L ), IP,
     :                             %VAL( CNF_PVAL( IPY ) ), STATUS )
               ELSE
                  CALL CCG1_LAPND( %VAL( CNF_PVAL( IPWX ) ),
     :                             NREC( L ), IP,
     :                             %VAL( CNF_PVAL( IPX ) ), STATUS )
                  CALL CCG1_LAPND( %VAL( CNF_PVAL( IPWY ) ),
     :                             NREC( L ), IP,
     :                             %VAL( CNF_PVAL( IPY ) ), STATUS )
               END IF
               CALL CCG1_LAPNI( %VAL( CNF_PVAL( IPIND( L ) ) ),
     :                          NREC( L ), IP,
     :                          %VAL( CNF_PVAL( IPID ) ), STATUS )
               IP = IP + NREC( L )
            END DO
         END DO

*  Do the first global transformation using the first positions
*  as the reference set.
         CALL CCD1_FITLM( %VAL( CNF_PVAL( IPX ) ),
     :                    %VAL( CNF_PVAL( IPY ) ),
     :                    %VAL( CNF_PVAL( IPID ) ),
     :                    %VAL( CNF_PVAL( IPOK ) ), NRECS, NSUP, IPREFS,
     :                    IFIT, TOLER, TR, %VAL( CNF_PVAL( IPXR ) ),
     :                    %VAL( CNF_PVAL( IPYR ) ),
     :                    %VAL( CNF_PVAL( IPIDR ) ),
     :                    %VAL( CNF_PVAL( IPWX ) ),
     :                    %VAL( CNF_PVAL( IPWY ) ),
     :                    %VAL( CNF_PVAL( IPWID ) ), NOUT, RMS, STATUS )

*  Explain the units of the linear transformation to the user.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '    Transformation coefficients', STATUS )
         CALL CCD1_MSG( ' ', '    ---------------------------', STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )
         IF ( USEWCS ) THEN
            CALL CCD1_MSG( ' ', '  Offsets A and D are in image '//
     :                     'Current coordinate system units;', STATUS )
         ELSE
            CALL CCD1_MSG( ' ', '  Offsets A and D are in in pixels;',
     :                     STATUS )
         END IF
         CALL CCD1_MSG( ' ', '  coefficients B, C, E and F are '//
     :                  'dimensionless.', STATUS )

*  Report the coefficients themselves.
         DO 11 I = 1, NSUP
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL MSG_SETI( 'ID', I )
            CALL CCD1_MSG( ' ', '  List ^ID)', STATUS )
            CALL CCD1_TROUT( TR( 1, I ), FRMS( ILIS( ILISOF( I ) ) ),
     :                       USEWCS, STATUS )
 11      CONTINUE

*  Store the mappings in an array.
         IF ( STATUS .EQ. SAI__OK ) THEN
            DO 7 I = 1, NSUP
               CALL CCD1_LNMAP( TR( 1, I ), MAPTFM( I ), STATUS )

*  If necessary convert these mappings from the Current to the pixel
*  frame of the reference NDF.
               IF ( USEWCS ) THEN
                  MAPTFM( I ) = AST_CMPMAP( MAPTFM( I ), MAPCP,
     :                                      .TRUE., ' ', STATUS )
                  MAPTFM( I ) = AST_SIMPLIFY( MAPTFM( I ), STATUS )
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
               CALL AST_TRAN2( MAPS( IPREF ), NOUT,
     :                         %VAL( CNF_PVAL( IPXR ) ),
     :                         %VAL( CNF_PVAL( IPYR ) ), .FALSE.,
     :                         %VAL( CNF_PVAL( IPXT ) ),
     :                         %VAL( CNF_PVAL( IPYT ) ), STATUS )
            ELSE
               IPXT = IPXR
               IPYT = IPYR
            END IF
            CALL CCD1_WRIXY( FDREFO, %VAL( CNF_PVAL( IPIDR ) ),
     :                       %VAL( CNF_PVAL( IPXT ) ),
     :                       %VAL( CNF_PVAL( IPYT ) ),
     :                       NOUT, LINE, CCD1__BLEN,
     :                       STATUS )

*  Tell user we have done so.
            CALL FIO_FNAME( FDREFO, LINE, STATUS )
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL MSG_SETC( 'FNAME', LINE )
            CALL CCD1_MSG( ' ',
     :      '  Extended reference positions written to file ^FNAME',
     :      STATUS )
         END IF
      ELSE

*  Get some workspace for the lists.
         NTOT = NRECS( 1 ) + NRECS( 2 )
         CALL CCD1_MALL( NTOT, '_DOUBLE', IPWX, STATUS )
         CALL CCD1_MALL( NTOT, '_DOUBLE', IPWY, STATUS )

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
         CALL CCD1_MALL( NRECS( 1 ) + NRECS( 2 ), '_LOGICAL', IPOK,
     :                   STATUS )
         CALL CCD1_RMULO( XPARNM, NXPAR, YPARNM, NYPAR,
     :                    %VAL( CNF_PVAL( IPOK ) ),
     :                    UNIPAR, NUMUNI, STATUS )
         CALL CCD1_MFREE( IPOK, STATUS )

*  Now that we have the parameter's names we need initial guesses at
*  their values.
         DO 12 I = 1, NUMUNI
            CALL PAR_GET0D( UNIPAR( I ), FORVAL( I ), STATUS )
 12      CONTINUE

*  Extract the X and Y positions from the input mapped data areas.
         DO I = 1, 2
            CALL CCD1_MALL( NRECS( I ), '_DOUBLE', IPXG( I ), STATUS )
            CALL CCD1_MALL( NRECS( I ), '_DOUBLE', IPYG( I ), STATUS )
            CALL CCD1_MALL( NRECS( I ), '_INTEGER', IPIG( I ), STATUS )
            NRECS( I ) = 0
            DO J = ILISOF( I ), ILISOF( I + 1 ) - 1
               L = ILIS( J )
               CALL CCD1_LEXT( %VAL( CNF_PVAL( IPDAT( L ) ) ),
     :                         NREC( L ), NVAL( L ),
     :                         1, %VAL( CNF_PVAL( IPWX ) ), STATUS )
               CALL CCD1_LEXT( %VAL( CNF_PVAL( IPDAT( L ) ) ),
     :                         NREC( L ), NVAL( L ),
     :                         2, %VAL( CNF_PVAL( IPWY ) ), STATUS )

*  Transform them into the correct coordinates if necessary.
               IF ( USEWCS ) THEN
                  CALL CCD1_MALL( NREC( L ), '_DOUBLE', IPXT, STATUS )
                  CALL CCD1_MALL( NREC( L ), '_DOUBLE', IPYT, STATUS )
                  CALL AST_TRAN2( MAPS( L ), NREC( L ),
     :                            %VAL( CNF_PVAL( IPWX ) ),
     :                            %VAL( CNF_PVAL( IPWY ) ), .TRUE.,
     :                            %VAL( CNF_PVAL( IPXT ) ),
     :                            %VAL( CNF_PVAL( IPYT ) ), STATUS )
                  IPWX = IPXT
                  IPWY = IPYT
               END IF

*  Copy them into the superlist.
               IP = NRECS( I ) + 1
               CALL CCG1_LAPND( %VAL( CNF_PVAL( IPWX ) ), NREC( L ), IP,
     :                          %VAL( CNF_PVAL( IPXG( I ) ) ), STATUS )
               CALL CCG1_LAPND( %VAL( CNF_PVAL( IPWY ) ), NREC( L ), IP,
     :                          %VAL( CNF_PVAL( IPYG( I ) ) ), STATUS )
               CALL CCG1_LAPNI( %VAL( CNF_PVAL( IPIND( I ) ) ),
     :                          NREC( L ), IP,
     :                          %VAL( CNF_PVAL( IPIG( I ) ) ), STATUS )
               NRECS( I ) = NRECS( I ) + NREC( L )
            END DO
         END DO

*  Do the transformation fit. If a reference list has been given then we
*  want to work out the transformation from the other list (stored in
*  position 1) to this. If two lists have been given the first list is
*  assumed to be the "reference" list and a transformation from the
*  second list to this is determined.
         IF ( IPREFS .EQ. 2 ) THEN
            OTHER = 1
         ELSE
            OTHER = 2
         END IF
         CALL CCD1_FITG( FORMAP, 'X', 'Y', UNIPAR, NUMUNI, TOLER,
     :                   IPIG( IPREFS ), IPXG( IPREFS ), IPYG( IPREFS ),
     :                   NRECS( IPREFS ),
     :                   IPIG( OTHER ), IPXG( OTHER ), IPYG( OTHER ),
     :                   NRECS( OTHER ), FORVAL, STATUS )

*  Derive the inverse transformation parameterisations by performing a
*  second fit. This provides protection against any ill-conditioning in
*  the equations and allows a check for this to be performed.
         IF ( FULL ) THEN

*  Use the parameter values from the first try as guesses for these
*  values.
            DO 13 I = 1, NUMUNI
               INVVAL( I ) = FORVAL( I )
 13         CONTINUE
            CALL CCD1_FITG( INVMAP, 'XX', 'YY', UNIPAR, NUMUNI, TOLER,
     :                      IPIG( IPREFS ), IPXG( OTHER ),
     :                      IPYG( OTHER ), NRECS( OTHER ),
     :                      IPIG( OTHER ), IPXG( IPREFS ),
     :                      IPYG( IPREFS ), NRECS( IPREFS ), INVVAL,
     :                      STATUS )

*  If necessary check the transformations for conditioning.
            CALL CCD1_CKCON( FORVAL, INVVAL, NUMUNI, STATUS )

*  Ask the user whether the transformations can be simplified.
            CALL PAR_GET0L( 'SIMPFI', SIMPFI, STATUS )
            CALL PAR_GET0L( 'SIMPIF', SIMPIF, STATUS )

*  If there is no inverse transformation, then simplification attributes
*  must be false.
         ELSE
            SIMPFI = .FALSE.
            SIMPIF = .FALSE.
         END IF

*  Now store the mappings in an array. One the of the mappings is just
*  the identity, the other is the derived one.
         DO 8 I = 1, 2
            IF ( I .EQ. IPREFS ) THEN
               MAPTFM( I ) = AST_UNITMAP( 2, ' ', STATUS )
            ELSE
               CALL CCD1_GGMAP( FORMAP, INVMAP, UNIPAR, NUMUNI, FORVAL,
     :                          INVVAL, FULL, SIMPFI, SIMPIF,
     :                          MAPTFM( I ), STATUS )
            END IF

*  If necessary convert these mappings from the Current to the pixel
*  frame of the reference NDF.
            IF ( USEWCS ) THEN
               MAPTFM( I ) = AST_CMPMAP( MAPTFM( I ), MAPCP, .TRUE.,
     :                                   ' ', STATUS )
               MAPTFM( I ) = AST_SIMPLIFY( MAPTFM( I ), STATUS )
            END IF
 8       CONTINUE
      END IF

*  We now have pointers to the AST Mapping objects in an array.
*  It's time to store them in wherever they ought to go eventually.

*  If we are writing all to one file, get its frameset identifier.
      IF ( PLACE .EQ. 'SINGLE' ) THEN
         ID = IDWCS
         CALL CCD1_GTWCS( ID, IWCS, STATUS )
      END IF

*  Initialise the array of mappings between Set alignment and
*  registration frames.
      IF ( USESET ) THEN
         DO I = 1, NSUP
            MAPSET( I ) = AST__NULL
         END DO
      END IF

*  Loop through each NDF which had an associated position list.
      DO 9 I = 1, NOPEN

*  Generate the name of the output domain.  This has been got from a
*  parameter, but if we are placing all new frames in a single WCS
*  component append a number to it.
         DMN = OUTDM
         IAT = CHR_LEN( DMN )
         IF ( PLACE .EQ. 'SINGLE' ) THEN
            CALL CHR_PUTC( '_', DMN, IAT )
            CALL CHR_PUTI( I, DMN, IAT )
         END IF

*  Generate a new frame to add to the WCS frameset.
         FRREG = AST_FRAME( 2, ' ', STATUS )
         CALL AST_SETC( FRREG, 'Domain', DMN( 1:IAT ), STATUS )
         CALL AST_SETC( FRREG, 'Title', 'Alignment by REGISTER',
     :                  STATUS )

*  If we are putting frames into each NDF, open the right one here and
*  get its frameset identifier.
         IF ( PLACE .EQ. 'EACH' ) THEN
            CALL NDG_NDFAS( NDFGR, I, 'UPDATE', ID, STATUS )
            CALL CCD1_GTWCS( ID, IWCS, STATUS )
         END IF

*  Get the original current frame index for future use.
         JCUR = AST_GETI( IWCS, 'Current', STATUS )

*  Ensure the Current frame is the one relative to which we've got the
*  mapping.
         IF ( .NOT. USEWCS ) THEN
            CALL CCD1_FRDM( IWCS, 'Pixel', JPIX, STATUS )
            CALL AST_SETI( IWCS, 'Current', JPIX, STATUS )
         END IF

*  Add the new output frame, with the appropriate mapping, to the
*  WCS component of the NDF.  This will become the current frame.
         CALL AST_ADDFRAME( IWCS, AST__CURRENT, MAPTFM( ISUP( I ) ),
     :                      FRREG, STATUS )

*  Get the index of the new registration frame.
         JREG = AST_GETI( IWCS, 'Current', STATUS )

*  If we have not already done so, store the mapping between the Set
*  alignment frame and the registration frame for possible later use.
         IF ( USESET .AND. MAPSET( ISUP( I ) ) .EQ. AST__NULL ) THEN
            CALL CCD1_FRDM( IWCS, 'CCD_SET', JSET, STATUS )
            IF ( JSET .NE. AST__NOFRAME ) THEN
               MAPSET( ISUP( I ) ) = AST_GETMAPPING( IWCS, JSET, JREG,
     :                                               STATUS )
            END IF
         END IF

*  Now remove any previously existing frames in the DMN domain;
*  the output frameset should contain only one, to prevent confusion
*  (note CCD1_DMPRG updates JREG if necessary).
         CALL CCD1_DMPRG( IWCS, DMN, .TRUE., JREG, STATUS )

*  If we are writing to individual NDFs then tidy up.
         IF ( PLACE .EQ. 'EACH' ) THEN

*  Write the modified WCS component back to NDF.
            CALL NDF_PTWCS( IWCS, ID, STATUS )

*  Release the NDF.
            CALL NDF_ANNUL( ID, STATUS )
         END IF

 9    CONTINUE

*  If we are writing all to one file, tidy that up here.
      IF ( PLACE .EQ. 'SINGLE' ) THEN

*  Get the name of the reference frameset's Domain.
         DMN = OUTDM
         IAT = CHR_LEN( DMN )
         CALL CHR_PUTC( '_', DMN, IAT )
         CALL CHR_PUTI( IPREFS, DMN, IAT )

*  Set the Current frame of the output frameset to the reference one.
         CALL CCD1_FRDM( IWCS, DMN, JCUR, STATUS )

*  Write out the frameset to the NDF's WCS component.
         CALL NDF_PTWCS( IWCS, ID, STATUS )

*  Release the NDF.
         CALL NDF_ANNUL( ID, STATUS )
      END IF

*  Now write registration frames to any NDFs which did not have
*  associated position lists, but are members of the same Set as ones
*  which we have managed to register.
      IF ( NDFS .AND. USESET .AND. PLACE .EQ. 'EACH' ) THEN

*  Construct the frame to add.
         FRREG = AST_FRAME( 2, ' ', STATUS )
         CALL AST_SETC( FRREG, 'Domain', OUTDM( 1:CHR_LEN( OUTDM ) ),
     :                  STATUS )
         CALL AST_SETC( FRREG, 'Title', 'Alignment by REGISTER',
     :                  STATUS )

*  Loop over each of the NDFs with no associated lists.
         DO I = 1, NNOLIS
            L = INLSUP( I )

*  Get the NDF identifier.
            CALL NDG_NDFAS( NLGR, I, 'UPDATE', ID, STATUS )

*  Get the WCS frameset.
            CALL CCD1_GTWCS( ID, IWCS, STATUS )

*  Get the frame index of the Set alignment frame, if one exists.
            IF ( L .GT. 0 ) THEN
               CALL CCD1_FRDM( IWCS, 'CCD_SET', JSET, STATUS )
            ELSE
               JSET = AST__NOFRAME
            END IF

*  If the Set alignment frame exists, add the registration frame using
*  the mapping between Set alignment frame and registration frame for
*  another member of the same Set.  Otherwise, add nothing.
            IF ( JSET .NE. AST__NOFRAME ) THEN
               CALL AST_ADDFRAME( IWCS, JSET, MAPSET( L ), FRREG,
     :                            STATUS )
               JREG = AST_GETI( IWCS, 'Current', STATUS )
            ELSE
               JREG = AST__NOFRAME
            END IF

*  Remove any pre-existing frames in the registration domain, whether
*  we have added a new one or not.
            CALL CCD1_DMPRG( IWCS, OUTDM, .TRUE., JREG, STATUS )

*  Write the modified frameset back to the NDF.
            CALL NDF_PTWCS( IWCS, ID, STATUS )

*  Release the NDF.
            CALL NDF_ANNUL( ID, STATUS )
         END DO
      END IF

*  Exit with error label. Tidy up after this.
 99   CONTINUE

*  Close reference positions, if open.
      IF ( OUTREF ) CALL FIO_CLOSE( FDREFO, STATUS )

*  Release all remaining workspace.
      CALL CCD1_MFREE( -1, STATUS )

*  Release group resources.
      CALL CCD1_GRDEL( NDFGR, STATUS )
      CALL CCD1_GRDEL( FIOGR, STATUS )
      CALL CCD1_GRDEL( NLGR, STATUS )

*  End AST context.
      CALL AST_END( STATUS )

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
