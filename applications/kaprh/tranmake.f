      SUBROUTINE TRANMAKE( STATUS )
*+
*  Name:
*     TRANMAKE

*  Purpose:
*     Makes a transformation structure given its co-ordinate mappings.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL TRANMAKE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a transformation data structure from
*     forward and inverse mappings that you supply.  The TRANSFORMER
*     application uses such a structure to transform an NDF by
*     resampling.  The structure can have classification qualifiers and
*     a comment.
*
*     For convenience, TRANMAKE can create a two-dimensional linear
*     transformation merely from the six coefficients, or
*     two-dimensional Cartesian-to-polar transformation given the
*     origin position and angle.

*  Usage:
*     tranmake transform trtype comment
*        { nvin=? nvout=? class=? for1=? inv1=? ... for7=? inv7=?
*          fa-fz=? pa-pz=?
*        { tr
*        trtype

*  ADAM Parameters:
*     CLASS( ) = LITERAL (Read)
*        A list of classifications that describe the properties of the
*        transformation.  This is optional, but the information can be
*        used to make other applications run more efficiently.  This
*        applies particularly to linear or constant determinants.
*        Valid values are:
*           "Linear"       --- linear and preserves straight lines,
*           "Independent"  --- preserves the independence of the axes,
*           "Diagonal"     --- preserves the axes themselves,
*           "Isotropic"    --- preserves angles and shapes,
*           "Positive_det" --- a component of reflection is absent,
*           "Negative_det" --- a component of reflection is present,
*           "Constant_det" --- the scale factor is constant,
*           "Unit_det"     --- areas (or volumes etc.) are preserved.
*
*        See SUN/61 Appendix B for more details of transformation
*        classification and a table of the classifications of common
*        mappings.  The suggested default is null (!) meaning unknown,
*        and no classification is written to the transformation
*        structure.  This parameter is ignored unless TRTYPE="Expres".
*     COMMENT = LITERAL (Read)
*        The comment string associated with the transformation.
*        A null value (!) causes a blank comment to be written into
*        the transformation.  Use the "-->" symbol to indicate the
*        forward transformation.  The suggested default is the null
*        value.
*     FA-FZ = LITERAL (Read)
*        These parameters supply the values of `sub-expressions' used
*        in the expressions FOR1-FOR7, and INV1-INV7.  Any of the 26
*        may appear; there is no restriction on order.  These
*        parameters should be used when repeated expressions are
*        present in complex transformations.  Sub-expressions may
*        contain references to other sub-expressions and constants
*        (PA-PZ).  An example of using sub-expressions is:
*           FOR1 > "XX=PA*ASIND(FA/PA)*X/FA"
*           FOR2 > "YY=PA*ASIND(FA/PA)*Y/FA"
*           INV1 > "X=PA*SIND(FB/PA)*XX/FB"
*           INV2 > "Y=PA*SIND(FB/PA)*YY/FB"
*           FA > SQRT(X*X+Y*Y)
*           PA > 100D0
*           FB > SQRT(XX*XX+YY*YY)
*        where the parameter name is to the left of > and its value is
*        to the right of the >.  This parameter is ignored unless
*        TRTYPE="Expres".
*     FITTYPE = _INTEGER (Read)
*        The type of fit specified by coefficients supplied via the
*        TR parameter.  Appropriate values are:
*           1 -- shift of origin,
*           2 -- shift of origin and rotation,
*           3 -- shift of origin and magnification,
*           4 -- shift of origin, rotation, and magnification
*                (solid body), and
*           5 -- a full six-parameter fit.
*
*        The value of this parameter is used to classify the
*        transformation (see the CLASS parameter).  This parameter is
*        ignored unless TRTYPE="Bilinear".  [5]
*     FOR1-FOR7 = LITERAL (Read)
*        The NVIN expressions that define the forward mapping or
*        mappings of the transformation.  FOR1 applies to first
*        output variable, and so on through to FOR7 for the seventh
*        input variable.  The expressions are written in Fortran-like
*        syntax.  The arithmetic operators (+,-,/,*,**) follow the
*        normal order of precedence.  Using matching (nested)
*        parentheses will explicitly define the order of expression
*        evaluation.  The expression may contain constants and the
*        built-in functions (LOG10, SQRT, SIN, TAND etc.) described in
*        SUN/61 Appendix A.
*
*        For a null forward transformation there must still be NVOUT
*        expressions, each just containing the name of the output
*        variable, for example, "X".  An example expression is
*        "Z=PA*(NINT(ZP)+0.5)".
*
*        This parameter is ignored unless TRTYPE="Expres".
*     INV1-INV7 = LITERAL (Read)
*        The NVOUT expressions that define the inverse mapping or
*        mappings of the transformation.  INV1 applies to first input
*        variable, and so on through to INV7 for the seventh input
*        variable.  The expressions are written in Fortran-like syntax.
*        The arithmetic operators (+,-,/,*,**) follow the normal order
*        of precedence.  Using matching (nested) parentheses will
*        explicitly define the order of expression evaluation.  The
*        expression may contain constants and the built-in functions
*        (LOG10, SQRT, SIN, TAND etc.) described in SUN/61 Appendix A.
*
*        For a null inverse mapping there must still be NVIN
*        expressions, each just containing the name of the input
*        variable, for example, "XX".  Generally, it is the inverse
*        mapping that is required.  An example expression is
*        "X=MOD(10*SQRT((XX+YY)*ZZ),360)"
*
*        This parameter is ignored unless TRTYPE="Expres".
*     NVIN = INTEGER (Read)
*        The number of input variables in the transformation.  It must
*        lie in the range 1 to 7.  The suggested default is the current
*        value, and 2 initially.   This parameter is ignored unless
*        TRTYPE="Expres".
*     NVOUT = INTEGER (Read)
*        The number of output variables in the transformation.  It must
*        lie in the range 1 to 7.  The suggested default is the number
*        of input variables.  This parameter is ignored unless
*        TRTYPE="Expres".
*     PA-PZ = _DOUBLE (Read)
*        These parameters supply the values of constants used in the
*        expressions FOR1-FOR7, and INV1-INV7.  Any of the 26 may
*        appear; there is no restriction on order.  Using parameters
*        allows the substitution of repeated constants using one
*        reference.  This is especially convenient for constants with
*        many significant digits.  It also allows easy modification of
*        parameterised expressions (expressions say with an adjustable
*        centre) provided the application has not been used in the
*        interim.  The parameter PI has a default value of
*        3.14159265359D0.  An example of using parameters is:
*           FOR1 > "XX=SQRT(FX*FX+FY*FY)"
*           FOR2 > "YY=ATAN2D(-FY,FX)"
*           INV1 > "X=XX*SIND(YY)+PA"
*           INV2 > "Y=-YY*COSD(XX)+PB"
*           FX > X-PA
*           FY > Y-PB
*           PA > X-centre-value
*           PB > Y-centre-value
*        where the parameter name is to the left of > and its value is
*        to the right of the >.  This example maps Cartesian
*        co-ordinates (x,y) to polar (r,theta) about a specified centre.
*        This parameter is ignored unless TRTYPE="Expres".
*     PREC = LITERAL (Read)
*        The arithmetic precision with which the transformation
*        functions will be evaluated when used.  This may be either
*        "_REAL" for single precision, "_DOUBLE" for double precision,
*        or "_INTEGER" for integer precision.  Elastic precisions are
*        used, such that a higher precision will be used if the input
*        data warrant it.  So for example if PREC = "_REAL", but
*        double-precision data were to be transformed, double-precision
*        arithmetic would actually be used.    This parameter is
*        ignored unless TRTYPE="Expres". ["_REAL"]
*     TR( 6 ) = _DOUBLE (Read)
*        If TRTYPE="Bilinear" is chosen then the values of this
*        parameter are the 6 coefficients of a linear transformation of
*        the type.
*              X' = TR(1) + TR(2)*X + TR(3)*Y
*              Y' = TR(4) + TR(5)*X + TR(6)*Y
*        The initial suggested default is the identity transformation.
*        ([0,1,0,0,0,1]).
*
*        If TRTYPE="Polar", only three values are needed.  TR(1) and
*        TR(2) are the x-y co-ordinates of the origin of the centre of
*        the polar co-ordinate system.  An optional third value is
*        the angular origin measured in degrees starting from the
*        x-axis in an anticlockwise direction.  If this is omitted, it
*        defaults to 0.  The initial suggested default is [0,0,0].
*
*        This parameter is ignored unless TRTYPE="Bilinear" or "Polar".
*     TRANSFORM = TRN (Write)
*        The actual or implied HDS object to store the created
*        transformation.  It may be an HDS container file, in which
*        case the transformation structure is placed within a structure
*        called TRANSFORM at the top level of the file; or a path to
*        the HDS object.  So for instance, if parameter
*        TRANSFORM=warp9, the transformation will be placed in the
*        top-level structure TRANSFORM within the HDS file warp9.sdf.
*        In this case the container file may already exist.  If, on the
*        other hand, the explicit structure is named, the
*        transformation information will be placed there.  For example,
*        to place the transformation in the extension GALPHOT of the
*        NDF called NGC253, parameter TRANSFORM would be NGC253.MORE.GALPHOT.
*        The structure name is limited to 15 printing characters.  Note
*        that in either case the structure must not already exist.  If
*        it does, an error condition results.
*
*        This has parameter no suggested default.
*     TRTYPE = LITERAL (Read)
*        The type of transform which will be supplied.  Valid values are
*        "Bilinear", "Expres", and "Polar".
*
*        "Bilinear" requests that the transform will be generated from
*        the six coefficients specified by parameter TR in the
*        equations:
*           X' = TR(1) + TR(2)*X + TR(3)*Y
*           Y' = TR(4) + TR(5)*X + TR(6)*Y
*        that define a linear two-dimensional transformation.
*
*        "Expres" lets an arbitrary transformation be specified using
*        algebraic-like statements of the type:
*           FOR1 > "XX=PA+PC*X"
*           FOR2 > "YY=PD+PE*Y"
*           INV1 > "X=(XX-PA)/PC"
*           INV2 > "Y=(YY-PD)/PE"
*        where the parameter name is to the left of > and its value is
*        to the right of the >.  The PA-PZs are reserved for constants
*        (FA-FZ are also reserved for repeated expressions).  This
*        example allows independent offsets and scales in x and y.  The
*        inverse mapping must be supplied.
*
*        "Polar" makes a 2-dimensional Cartesian-to-polar
*        transformation, where the origin in Cartesian co-ordinates, and
*        polar angle are specified by parameter TR.
*
*        ["Expres"]

*  Examples:
*     tranmake xyz nvin=1 nvout=1 for1="xd=1.01*x-0.34"
*       inv1="x=(xd+0.34)/1.01" \
*        This creates a transformation structure TRANSFORM in the HDS
*        file called xyz.sdf.  It specifies mappings between one input
*        and one output variable.  The output variable is 1.01 the
*        output variable less 0.34.
*     tranmake xyz nvin=1 nvout=1 for1="xd=1.01*x-0.34" class=linear
*       inv1="x=(xd+0.34)/1.01" comment="Copier correction"
*        As above, except that because the transformation is linear
*        (it is a magnification and translation), the classification is
*        set to "Linear".  "Copier correction" is written as the comment
*        in the structure.
*     tranmake transform=turn.more.rot45 nvin=2 nvout=2
*       class=["linear","isotropic","unit_det"] pa=45
*       for1="xo=cosd(pa)*xi-sind(pa)*yi"
*       for2="yo=sind(pa)*xi+cosd(pa)*yi"
*       inv1="xi=cosd(pa)*xo+sind(pa)*yo"
*       inv2="yi=-sind(pa)*xo+cosd(pa)*yo" \
*        This creates a transformation structure TURN.MORE.ROT45 (in
*        HDS file turn.sdf) that rotates a two-dimensional co-ordinate
*        system by 45 degrees clockwise.  Three classes---linear,
*        isotropic, unit determinant---are assigned for this
*        transformation.  (As it is a rotation, positive and constant
*        determinants are also applicable.)
*     tranmake shiftim trtype=lin tr=[8.73,1,0,-64.6,0,1] fittype=1 \
*        This creates an HDS file called raw_origin.sdf containing a
*        transformation structure called TRANSFORM at the top-level.
*        This transformation defines a shift of 8.73 of the first
*        variable (usually x in an image) and a negative shift of 64.6
*        in the second variable (normally y).  The shift is specified
*        using the appropriate linear-transformation coefficients
*        [XSHIFT,1,0,YSHIFT,0,1] and is correctly classified as a
*        fit type of 1.  There is no comment.
*     tranmake my_ndf.more.my_extension.tran1 bilinear "15-deg rotation"
*     [0,0.965926,-0.258819,0,0.258819,0.965926] fittype=2
*        This creates a transformation structure called TRAN1
*        in the extension MY_EXTENSION of the NDF called my_ndf.  The
*        structure defines a rotation by 15 degrees in about the (0,0)
*        position in a plane (say x-y of an image).  The rotation
*        is specified using the appropriate linear transformation
*        coefficients [0,COS,-SIN,0,SIN,COS].  The comment stored in the
*        structure is "15-deg rotation".
*     tranmake polish.origin1 trtype=p tr=[100.0,21.3] \
*        This creates an HDS file called polish.sdf containing a
*        transformation structure called ORIGIN1 at the top-level.
*        This structure defines a Cartesian-to-polar transformation
*        about an origin at (100.3,21.3) in pixel co-ordinates.
*        There is no comment stored in ORIGIN1.

*  Notes:
*     -  This routine does not check that the forward and inverse
*     expressions actually define a pair of complementary mappings.
*     -  On completion, the destination structure for the
*     transformation information equates to the current transformation
*     global parameter.
*     -  Expressions supplied for parameters FOR1-FOR7 and INV1-INV7
*     should be enclosed in double quotes (even when given in response
*     to a prompt) to protect the equals sign from interpretation by
*     the shell or parameter system.

*  Related Applications:
*     KAPRH: FLIP, ROTATE, SLIDE, TRANINVERT, TRANJOIN, TRANSFORMER,
*     TRANTRACE; CCDPACK: CCDEDIT, TRANLIST, TRANNDF.

*  Implementation Deficiencies:
*     -  Standard map projections are not yet recognised.

*  [optional_A_task_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 March 22 (MJC):
*        Original version.
*     1995 February (MJC):
*        Included tokens for constants and expressions, and the
*        two-dimensional linear fit.
*     2-JUN-1998 (DSB):
*        Corrected status check after call to TRN_NEW. Previously
*        an error was reported saying that the expression failed
*        to compile if no classifications were given (even if in fact it
*        had compiled succesfully). Also, added a note to the prologue
*        telling the user to encloise expressions in double quotes.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'DAT_ERR'          ! Data-system error constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'TRN_PAR'          ! Transformation constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a character string ignoring
                                 ! trailing spaces

*  Local Constants:
      INTEGER EXPLEN             ! Maximum expression length
      PARAMETER ( EXPLEN = 512 )

*  Local Variables:
      CHARACTER * ( 12 ) CLANAM( TRN__MXCLS ) ! Classifications
      LOGICAL CLASS( TRN__MXCLS ) ! Classifaction flags
      CHARACTER * ( EXPLEN ) COMM ! Comment in the transformation
      DOUBLE PRECISION DEFTR( 6 ) ! Transformation coefficient defaults
      LOGICAL EXIST              ! The transformation object exists?
      CHARACTER * ( 256 ) FILNAM ! HDS file name
      CHARACTER * ( EXPLEN ) FORWRD( DAT__MXDIM ) ! Forward mappings
      CHARACTER * ( 1 ) CDIM     ! Dimension number
      INTEGER IFIT               ! Fit type code for 2-d linear
      CHARACTER * ( EXPLEN ) INVERS( DAT__MXDIM ) ! Inverse mappings
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to the output container
                                 ! file
      CHARACTER * ( DAT__SZLOC ) LOCTR ! Locator to the transformation
      INTEGER I                  ! Loop variable
      CHARACTER * ( DAT__SZNAM ) NAME ! Name of the component holding
                                 ! the transformation structure
      INTEGER NCLASS             ! Number of classifications
      INTEGER NC                 ! Number of characters in the dimension
      INTEGER NCHAR              ! Number of characters to last node
      INTEGER NCOEFF             ! Number of coefficients supplied
      INTEGER NCP                ! Number of characters in the path
      INTEGER NCPREC             ! Number of characters in the precision
      INTEGER NLEV               ! Number of levels in the path
      INTEGER NVIN               ! Number of input variables
      INTEGER NVOUT              ! Number of output variables
      CHARACTER * ( DAT__SZLOC ) PARLOC ! Locator to the parent
                                 ! structure of the transformation
      CHARACTER * ( 132 ) PATH   ! Path of the transformation structure
      CHARACTER * ( 8 ) PREC     ! Precision of the transformation
      DOUBLE PRECISION TR( 6 )   ! Transformation coefficients
      CHARACTER * ( 8 ) TRTYPE   ! Transformation types

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a locator to the supplied structure.
*  ========================================

*  DAT_ASSOC reprompts if the file does not exist, but DAT_EXIST will
*  exit.
      CALL DAT_EXIST( 'TRANSFORM', 'WRITE', LOC, STATUS )

*  Create a new container file, if the nominated file does not exist.
*  Annul the error as this is an expected condition.
      IF ( STATUS .EQ. DAT__FILNF .OR. STATUS .EQ. PAR__ERROR ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL DAT_CREAT( 'TRANSFORM', 'TRN', 0, 0, STATUS )
         CALL DAT_ASSOC( 'TRANSFORM', 'WRITE', LOC, STATUS )
      END IF

*  Obtain the transformation's name and a locator to its parent.
*  =============================================================

*  Look to see if this the top level or to a structure within the file.
*  Get the path of the object and the number of nodes within it.
      CALL HDS_TRACE( LOC, NLEV, PATH, FILNAM, STATUS )

*  Place the transformation at the top-level.
*  ------------------------------------------
      IF ( NLEV .EQ. 1 ) THEN

*  The locator to the structure to contain the transformation is the
*  top-level locator.  Copy the original locator to the parent locator
*  as it is the parent structure.  The name of the transformation
*  structure is TRANSFORM at the top-level of the file.
         PARLOC = LOC
         NAME = 'TRANSFORM'

*  If the component already exists, delete it.
         CALL DAT_THERE( PARLOC, NAME, EXIST, STATUS )
         IF ( EXIST ) CALL DAT_ERASE( PARLOC, NAME, STATUS )
      ELSE

*  Place the transformation in the current object.
*  -----------------------------------------------
*
*  Get the locator to the parent object.
         CALL DAT_PAREN( LOC, PARLOC, STATUS )

*  Look for the last delimiter, searching backwards through the path.
         NCP = CHR_LEN( PATH )
         NCHAR = NCP
         CALL CHR_FIND( PATH, '.', .FALSE., NCHAR )

*  Extract the name of the transformation structure.
         NAME = PATH( NCHAR+1 : NCP )

*  Annul the original locator.
         CALL DAT_ANNUL( LOC, STATUS )

*  TRN_NEW will create a new structure, so the one just made by the
*  DAT_ASSOC/EXIST must be deleted first.
         CALL DAT_ERASE( PARLOC, NAME, STATUS )

      END IF

*  Obtain the comment of the transformation.
*  =========================================
      CALL ERR_MARK
      CALL PAR_GET0C( 'COMMENT', COMM, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         COMM = ' '
      END IF
      CALL ERR_RLSE

*  See which type of transformation is required.
*  =============================================

*  See how the user wants to define the transformation.  Either we will
*  create one using two-dimensional linear-transformation coefficients
*  or allow the user to specify the whole thing.
      CALL PAR_CHOIC( 'TRTYPE', 'EXPRES', 'BILINEAR,EXPRES,POLAR',
     :                .FALSE., TRTYPE, STATUS )

*  Act on the various transformations.
      IF ( TRTYPE .EQ. 'BILINEAR' ) THEN

*  Two-dimensional linear
*  ======================
*
*  Set the prompt string for this option.
         CALL PAR_PROMT( 'TR', '2-d linear-transformation '/
     :                   /'coefficients (6)', STATUS )

*  Set the dynamic defaults for this option.
         DEFTR( 1 ) = 0.0D0
         DEFTR( 2 ) = 1.0D0
         DEFTR( 3 ) = 0.0D0
         DEFTR( 4 ) = 0.0D0
         DEFTR( 5 ) = 0.0D0
         DEFTR( 6 ) = 1.0D0
         CALL PAR_DEF1D( 'TR', 6, DEFTR, STATUS )

*  Supplying transformation as a series of linear-transformation
*  coefficients is easier because the user needs to only supply the
*  forward terms; the number of variables, the expressions, the
*  inverse coefficients and classification are generated automatically.).  Supplying
         CALL PAR_EXACD( 'TR', 6, TR, STATUS )

*  Get the FITTYPE this corresponds to.
         CALL PAR_GDR0I( 'FITTYPE', 5, 1, 5, .FALSE., IFIT, STATUS )

*  Create the transform structure.
         CALL KPS1_WLTRN( TR, PARLOC, NAME, COMM, IFIT, STATUS )

      ELSE IF ( TRTYPE .EQ. 'POLAR' ) THEN

*  Two-dimensional linear
*  ======================
*
*  Set the prompt string for this option.
         CALL PAR_PROMT( 'TR', 'Origins for Cartesian to polar '/
     :                   /'(x,y,angle)', STATUS )

*  Set the dynamic defaults for this option.
         DEFTR( 1 ) = 0.0D0
         DEFTR( 2 ) = 0.0D0
         DEFTR( 3 ) = 0.0D0
         CALL PAR_DEF1D( 'TR', 3, DEFTR, STATUS )

*  Supplying transformation as a Cartesian to polar transformation.
*  The x-y origin of the pole and the angular origin for the
*  co-ordinates are needed.
         CALL PAR_GET1D( 'TR', 3, TR, NCOEFF, STATUS )

*  Pad missing values with zero.
         IF ( NCOEFF .LT. 3 ) THEN
            DO I = NCOEFF + 1, 3
               TR( I ) = 0.0D0
            END DO
         END IF

*  Create the transform structure.
         CALL KPS1_WPTRN( TR, PARLOC, NAME, COMM, STATUS )

*  Obtain the number of transformation variables.
*  ==============================================
      ELSE
         CALL PAR_GDR0I( 'NVIN', 2, 1, DAT__MXDIM, .FALSE., NVIN,
     :                   STATUS )
         CALL PAR_GDR0I( 'NVOUT', NVIN, 1, DAT__MXDIM, .FALSE., NVOUT,
     :                   STATUS )

*  Obtain the precision and classification of the transformation.
*  ==============================================================

*  Get the (elastic) precision from a choice of three.
         CALL PAR_CHOIC( 'PREC', '_DOUBLE', '_INTEGER,_REAL,_DOUBLE',
     :                   .FALSE., PREC, STATUS )
         NCPREC = CHR_LEN( PREC )

*  Obtain a list of choices.  A null value ends the list.
         CALL ERR_MARK
         CALL PAR_CHOIV( 'CLASS', TRN__MXCLS, 'LINEAR,INDEPENDENT,'/
     :                   /'DIAGONAL,ISOTROPIC,POSITIVE_DET,'/
     :                   /'NEGATIVE_DET,CONSTANT_DET,UNIT_DET', CLANAM,
     :                   NCLASS, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
         CALL ERR_RLSE
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain expressions relating co-ordinates in two systems.
*  ========================================================

*  First obtain the forward transformations in uppercase.  Generate the
*  parameter name to do so.
         DO I = 1, NVOUT
            CALL CHR_ITOC( I, CDIM, NC )
            CALL PAR_GET0C( 'FOR'//CDIM( :NC ), FORWRD( I ), STATUS )
            CALL CHR_UCASE( FORWRD( I ) )
            IF ( STATUS .NE. SAI__OK ) GOTO 999
         END DO

*  Now obtain the inverse transformations in uppercase.  Generate the
*  parameter name to do so.
         DO I = 1, NVIN
            CALL CHR_ITOC( I, CDIM, NC )
            CALL PAR_GET0C( 'INV'//CDIM( :NC ), INVERS( I ), STATUS )
            CALL CHR_UCASE( INVERS( I ) )
            IF ( STATUS .NE. SAI__OK ) GOTO 999
         END DO

*  Substitute any tokens in the expressions.
*  =========================================

*  Deal with the sub-expressions first as these may contain constants.
         DO I = 1, NVOUT
            CALL KPG1_SATKC( 'F', FORWRD( I ), STATUS )
            CALL KPG1_SATKD( 'P', FORWRD( I ), STATUS )
         END DO

         DO I = 1, NVIN
            CALL KPG1_SATKC( 'F', INVERS( I ), STATUS )
            CALL KPG1_SATKD( 'P', INVERS( I ), STATUS )
         END DO

*  Attempt to compile the transformation.
*  ======================================
         CALL TRN_NEW( NVIN, NVOUT, FORWRD, INVERS,
     :                 PREC( :NCPREC )//':', COMM, PARLOC, NAME, LOCTR,
     :                 STATUS )

*  Give some help. if the transformation did not compile.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETI( 'NVIN', NVIN )
            CALL MSG_SETI( 'NVOUT', NVOUT )
            CALL ERR_REP( 'TRANMAKE_NOCOMP',
     :        'The transformation does not compile.  Check the '/
     :        /'forward and inverse expressions.  There should be '/
     :        /'^NVIN distinct input variables and ^NVOUT distinct '/
     :        /'output variables.  See SUN/61 for details of the '/
     :        /'permitted operators and functions.', STATUS )


*  Assign classifications to the transformation.
*  =============================================

*  Check whether or not we need to enter any classifications.
         ELSE IF ( NCLASS .GT. 0 ) THEN

*  Initialise the logical array of classifications.
            DO I = 1, TRN__MXCLS
               CLASS( I ) = .FALSE.
            END DO

*  Set up the classifications.
            DO I = 1, NCLASS
               IF ( CLANAM( I ) .EQ. 'LINEAR' ) THEN
                  CLASS( TRN__LIN ) = .TRUE.

               ELSE IF ( CLANAM( I ) .EQ. 'INDEPENDENT' ) THEN
                  CLASS( TRN__INDEP ) = .TRUE.

               ELSE IF ( CLANAM( I ) .EQ. 'DIAGONAL' ) THEN
                  CLASS( TRN__DIAG ) = .TRUE.

               ELSE IF ( CLANAM( I ) .EQ. 'ISOTROPIC' ) THEN
                  CLASS( TRN__ISOT ) = .TRUE.

               ELSE IF ( CLANAM( I ) .EQ. 'POSITIVE_DET' ) THEN
                  CLASS( TRN__POSDT ) = .TRUE.

               ELSE IF ( CLANAM( I ) .EQ. 'NEGATIVE_DET' ) THEN
                  CLASS( TRN__NEGDT ) = .TRUE.

               ELSE IF ( CLANAM( I ) .EQ. 'CONSTANT_DET' ) THEN
                  CLASS( TRN__CONDT ) = .TRUE.

               ELSE IF ( CLANAM( I ) .EQ. 'UNIT_DET' ) THEN
                  CLASS( TRN__UNIDT ) = .TRUE.

               END IF
            END DO

*  Enter the classification information.
            CALL TRN_PTCL( CLASS, LOCTR, STATUS )

         END IF
      END IF

*  Tidy resources.
*  ===============

*  Close the transformation system.
      CALL TRN_CLOSE( STATUS )
  999 CONTINUE

*  Close the output file containing the transformation structure.  The
*  input LOC locator has either already been annulled, or is annulled
*  here, if it points to the top-level object.
      CALL DAT_ANNUL( PARLOC, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'TRANMAKE_ERR',
     :     'TRANMAKE: Unable to make a transformation structure.',
     :     STATUS )
      END IF

      END
