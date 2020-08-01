      SUBROUTINE MATHS( STATUS )
*+
*  Name:
*     MATHS

*  Purpose:
*     Evaluates mathematical expressions applied to NDF data structures.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MATHS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application allows arithmetic and mathematical functions to
*     be applied pixel-by-pixel to a number of NDF data structures and
*     constants so as to produce a new NDF.  The operations to be
*     performed are specified using a Fortran-like mathematical
*     expression.  Up to 26 each input NDF data and variance arrays, 26
*     parameterised `constants', and pixel and data co-ordinates along
*     up to 7 dimensions may be combined in wide variety of ways using
*     this application.  The task can also calculate variance estimates
*     for the result when there is at least one input NDF array.

*  Usage:
*     maths exp out ia-iz=? va-vz=? fa-fz=? pa-pz=? lbound=? ubound=?

*  ADAM Parameters:
*     EXP = LITERAL (Read)
*        The mathematical expression to be evaluated for each NDF
*        pixel, e.g. "(IA-IB+2)*PX".  In this expression, input NDFs are
*        denoted by the variables IA, IB, ... IZ, while constants may
*        either be given literally or represented by the variables PA,
*        PB, ...  PZ.  Values for those NDFs and constants which appear
*        in the expression will be requested via the application's
*        parameter of the same name.
*
*        Fortran-77 syntax is used for specifying the expression, which
*        may contain the usual intrinsic functions, plus a few extra
*        ones. An appendix in SUN/61 gives a full description of the
*        syntax used and an up to date list of the functions available.
*        The expression may be up to 132 characters long and is case
*        insensitive.
*     FA-FZ = LITERAL (Read)
*        These parameters supply the values of `sub-expressions' used
*        in the expression EXP.  Any of the 26 (FA, FB, ... FZ) may
*        appear; there is no restriction on order.  These parameters
*        should be used when repeated expressions are present in
*        complex expressions, or to shorten the value of EXP to fit
*        within the 132-character limit.  Sub-expressions may contain
*        references to other sub-expressions and constants (PA-PZ).  An
*        example of using sub-expressions is:
*           EXP > PA*ASIND(FA/PA)*XA/FA
*           FA > SQRT(XA*XA+XB*YB)
*           PA > 10.1
*        where the parameter name is to the left of > and its value is
*        to the right of the >.
*     IA-IZ = NDF (Read)
*        The set of 26 parameters named IA, IB, ... IZ is used to
*        obtain the input NDF data structure(s) to which the
*        mathematical expression is to be applied.  Only those
*        parameters which actually appear in the expression are used,
*        and their values are obtained in alphabetical order.  For
*        instance, if the expression were "SQRT(IB+IA)", then the
*        parameters IA and IB would be used (in this order) to obtain
*        the two input NDF data structures.
*     LBOUND( ) = _INTEGER (Read)
*        Lower bounds of new NDF, if LIKE=! and there is no input NDF
*        referenced in the expression.  The number of values required
*        is the number of pixel co-ordinate axes in the expression.
*     LIKE = NDF (Read)
*        An optional template NDF which, if specified, will be used to
*        define bounds and data type of the new NDF, when the expression
*        does not contain a reference to an NDF.  If a null response
*        (!) is given the bounds are obtained via parameters LBOUND
*        and UBOUND, and the data type through Parameter TYPE. [!]
*     OUT = NDF (Write)
*        Output NDF to contain the result of evaluating the expression
*        at each pixel.
*     PA-PZ = _DOUBLE (Read)
*        The set of 26 parameters named PA, PB, ... PZ is used to
*        obtain the numerical values of any parameterised `constants'
*        which appear in the expression being evaluated.  Only those
*        parameters which actually appear in the expression are used,
*        and their values are obtained in alphabetical order.  For
*        instance, if the expression were "PT*SIN(IA/PS)", then the
*        parameters PS and PT (in this order) would be used to obtain
*        numerical values for substitution into the expression at the
*        appropriate points.
*
*        These parameters are particularly useful for supplying the
*        values of constants when writing procedures, where the
*        constant may be determined by a command-language variable, or
*        when the constant is stored in a data structure such as a
*        global parameter.  In other cases, constants should normally be
*        given literally as part of the expression, as in "IZ**2.77".
*     QUICK = _LOGICAL (Read)
*        Specifies the method by which values for the VARIANCE
*        component of the output NDF are calculated.  The algorithm used
*        to determine these values involves perturbing each of the
*        input NDF data arrays in turn by an appropriate amount, and
*        then combining the resulting output perturbations.  If QUICK
*        is set to TRUE, then each input data array will be perturbed
*        once, in the positive direction only.  If QUICK is set to
*        FALSE, then each will be perturbed twice, in the positive and
*        negative directions, and the maximum resultant output
*        perturbation will be used to calculate the output variance.
*        The former approach (the normal default) executes more
*        quickly, but the latter is likely to be more accurate in cases
*        where the function being evaluated is highly non-linear,
*        and/or the errors on the data are large.  [TRUE]
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the (alphabetically) first input NDF to be used
*        instead. [!]
*     TYPE = LITERAL (Read)
*        Data type for the new NDF, if LIKE=! and no input NDFs are
*        referenced in the expression.  It must be one either
*        "_DOUBLE" or "_REAL".
*     UBOUND( ) = _INTEGER (Read)
*        Upper bounds of new NDF, if LIKE=! and there is no input NDF
*        referenced in the expression.  These must not be smaller
*        than the corresponding LBOUND.  The number of values required
*        is the number of pixel co-ordinate axes in the expression.
*     UNITS = _LOGICAL (Read)
*        Specifies whether the UNITS component of the (alphabetically)
*        first input NDF or the template NDF will be propagated to the
*        output NDF.  By default this component is not propagated since,
*        in most cases, the units of the output data will differ from
*        those of any of the input data structures.  In simple cases,
*        however, the units may be unchanged, and this parameter then
*        allows the UNITS component to be preserved.  This parameter is
*        ignored if the expression does not contain a token to at least
*        one input NDF structure and LIKE=!.  [FALSE]
*     VA-VZ = NDF (Read)
*        The set of 26 parameters named VA, VB, ... VZ is used to
*        obtain the input NDF variance array(s) to which the
*        mathematical expression is to be applied.  The variance VA
*        corresponds to the data array specified by Parameter IA, and
*        so on.  Only those parameters which actually appear in the
*        expression, and do not have their corresponding data-array
*        Parameter IA-IZ present, have their values obtained in
*        alphabetical order.  For instance, if the expression were
*        "IB+SQRT(VB+VA)", then the parameters VA and IB would be used
*        (in this order) to obtain the two input NDF data structures.
*        The first would use just the variance array, whilst the second
*        would read both data and variance arrays.
*     VARIANCE = _LOGICAL (Read)
*        Specifies whether values for the VARIANCE component of the
*        output NDF should be calculated.  If this parameter is set to
*        TRUE (the normal default), then output variance values will be
*        calculated if any of the input NDFs contain variance
*        information.  Any which do not are regarded as having zero
*        variance.  Variance calculations will normally be omitted only
*        if none of the input NDFs contain variance information.
*        However, if VARIANCE is set to FALSE, then calculation of
*        output variance values will be disabled under all
*        circumstances, with a consequent saving in execution time.
*        This parameter is ignored if the expression does not contain
*        a token to at least one input NDF structure. [TRUE]

*  Examples:
*     maths "ia-1" dat2 ia=dat1
*        The expression "ia-1" is evaluated to subtract 1 from each
*        pixel of the input NDF referred to as IA, whose values reside
*        in the data structure dat1.  The result is written to the NDF
*        structure dat2.
*     maths "(ia-ib)/ic" ia=data ib=back ic=flat out=result units
*        The expression "(ia-ib)/ic" is evaluated to remove a
*        background from an image and to divide it by a flat-field.
*        All the images are held in NDF data structures, the input
*        image being obtained from the data structure data, the
*        background image from back and the flat-field from flat.  The
*        result is written to the NDF structure result.  The data units
*        are unchanged and are therefore propagated to the output NDF.
*     maths "-2.5*log10(ii)+25.7" ii=file1 out=file2
*        The expression "-2.5*log10(ii)+25.7" is evaluated to convert
*        intensity measurements into magnitudes, including a zero
*        point.  Token II represents the input measurements held in the
*        NDF structure file1.  The result is written to the NDF
*        structure file2.  If file1 contains variance values, then
*        corresponding variance values will also be calculated for
*        file2.
*     maths exp="pa*exp(ia+pb)" out=outfile pb=13.7 novariance
*        The expression "pa*exp(ia+pb)" is evaluated with a value of
*        13.7 for the constant PB, and output is written to the NDF
*        structure outfile.  The input NDF structure to be used for
*        token IA and the value of the other numerical constant PA will
*        be prompted for.  NOVARIANCE has been specified so that output
*        variance values will not be calculated.
*     maths exp="mod(XA,32)+mod(XB,64)" out=outfile like=comwest
*        The expression "mod(XA,32)+mod(XB,64)" is evaluated, and
*        output is written to the NDF structure outfile.  The output
*        NDF inherits the shape, bounds, and other properties (except the
*        variance) of the NDF called comwest.  The data type of outfile
*        is _REAL unless comwest has type _DOUBLE.  XA and XB represent
*        the pixel co-ordinates along the x and y axes respectively.
*     maths "xf*xf+0*xa" ord2 lbound=[-20,10] ubound=[20,50]
*        The expression "xf*xf+0*xa" is evaluated, and output is
*        written to the NDF structure ord2.  The output NDF has data
*        type _REAL, is two-dimensional with bounds -20:20, 10:50.  The
*        XA is needed to indicate that XF represents pixel co-ordinates
*        along the y axis.
*     maths "xa/max(1,xb)+sqrt(va)" ord2 va=fuzz title="Fuzz correction"
*        The expression "xa/max(1,xb)+sqrt(va)" is evaluated, and output
*        is written to the NDF structure ord2.  Token VA represents the
*        input variance array held in the NDF structure fuzz.  The
*        output NDF inherits the shape, bounds, and other properties of
*        fuzz.  The title of ord2 is "Fuzz correction".  The data type
*        of ord2 is _REAL unless fuzz has type _DOUBLE.  XA and XB
*        represent the pixel co-ordinates along the x and y axes
*        respectively.

*  Notes:
*     -  The alphabetically first input NDF is regarded as the primary
*     input dataset.  NDF components whose values are not changed by
*     this application will be propagated from this NDF to the output.
*     The same propagation rules apply to the LIKE template NDF,
*     except that the output NDF does have inherit any variance
*     information.
*     -  There are additional tokens which can appear in the expression.
*
*     The set of 7 tokens named CA, CB, ... CG is used to obtain the
*     data co-ordinates from the primary input NDF data structure.  Any
*     of the 7 parameters may appear in the expression.  The order
*     defines which axis is which, so for example, "2*CF+CB*CB" means
*     the first-axis data co-ordinates squared, plus twice the
*     co-ordinates along the second axis.  There must be at least one
*     input NDF in the expression to use the CA-CG tokens, and it must
*     have dimensionality of at least the number of CA-CG tokens given.
*
*     The set of 7 tokens named XA, XB, ... XG is used to obtain the
*     pixel co-ordinates from the primary input NDF data structure.  Any
*     of the 7 parameters may appear in the expression.  The order
*     defines which axis is which, so for example, "SQRT(XE)+XC" means
*     the first-axis pixel co-ordinates plus the square root of the
*     co-ordinates along the second axis.  Here no input NDF need be
*     supplied.  In this case the dimensionality of the output NDF is equal to the
*     number of XA-XG tokens in the expression.  However, if there is at
*     least one NDF in the expression, there should not be more XA-XG
*     tokens than the dimensionality of the output NDF (given as the
*     intersection of the bounds of the input NDFs).
*     -  If illegal arithmetic operations (e.g. division by zero, or
*     square root of a negative number) are attempted, then a bad pixel
*     will be generated as a result.  (However, the infrastructure
*     software that detects this currently does not work on OSF/1
*     systems, and therefore MATHS will crash in this circumstance.)
*     -  All arithmetic performed by this application is floating
*     point.  Single-precision will normally be used, but
*     double-precision will be employed if any of the input NDF arrays
*     has a numeric type of _DOUBLE.

*  Related Applications:
*     KAPPA: CREFRAME, SETAXIS, and numerous arithmetic tasks; Figaro:
*     numerous arithmetic tasks.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of the
*     input NDFs.  HISTORY and extensions are propagated from both the
*     primary NDF and template NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  NDFs with any number of dimensions can be processed.  The NDFs
*     supplied as input need not all be the same shape.

*  Calculating Variance:
*     The algorithm used to calculate output variance values is
*     general-purpose and will give correct results for any reasonably
*     well-behaved mathematical expression.  However, this application
*     as a whole, and the variance calculations in particular, are
*     likely to be less efficient than a more specialised application
*     written knowing the form of the mathematical expression in
*     advance.  For simple operations (addition, subtraction, etc.) the
*     use of other applications (ADD, SUB, etc.) is therefore
*     recommended, particularly if variance calculations are required.
*
*     The main value of the variance-estimation algorithm used here
*     arises when the expression to be evaluated is too complicated, or
*     too infrequently used, to justify the work of deriving a direct
*     formula for the variance.  It is also of value when the data
*     errors are especially large, so that the linear approximation
*     normally used in error analysis breaks down.
*
*     There is no variance processing when there are no tokens for
*     input NDF structures.

*  Timing:
*     If variance calculations are not being performed, then the time
*     taken is approximately proportional to the number of NDF pixels
*     being processed.  The execution time also increases with the
*     complexity of the expression being evaluated, depending in the
*     usual way on the nature of any arithmetic operations and
*     intrinsic functions used. If certain parts of the expression will
*     often give rise to illegal operations (resulting in bad pixels),
*     then execution time may be minimised by placing these operations
*     near the beginning of the expression, so that later parts may not
*     need to be evaluated.
*
*     If output variance values are being calculated and the QUICK
*     parameter is set to TRUE, then the execution time will be
*     multiplied by an approximate factor (N+1), where N is the number
*     of input NDFs which contain a VARIANCE component.  If QUICK is set
*     to FALSE, then the execution time will be multiplied by an
*     approximate factor (2N+1).

*  Copyright:
*     Copyright (C) 1989-1990 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998-1999, 2002, 2004 Central Laboratory of
*     the Research Councils. All Rights Reserved.

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
*     MJC: Malcolm Currie (STARLINK)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1989 August 20 (MJC):
*        First implementation.
*     1989 December 21 (MJC):
*        Workspace managed by AIF_TEMP.
*     1990 June 28 (RFWS):
*        Substantial re-write to use the NDF_ access routines and to
*        calculate output variance values.
*     1990 August 7 (RFWS):
*        Fixed bug in data type of the PARVAL variable.
*     1995 September 18 (MJC):
*        Added FA-FZ, LBOUND, LIKE, PA-PZ, TYPE, UBOUND, VA-VZ
*        parameters, Usage, Related Applications, and additional
*        examples.  Enabled use of variance and co-ordinates in the
*        expression.  Examples changed to lowercase.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     1998 August 12 (MJC):
*        Evaluates the FA-FZ tokens before any others.  Previously
*        this was done after the IA-IZ and VA-VZ were expanded, hence
*        sub-expressions could not contain these array tokens.
*     1999 May 4 (TDCA):
*        Fixed bug in the way _DOUBLE variance arrays are copied into
*        the work arrays: now behaves as for _REAL data.
*     24-SEP-2002 (DSB):
*        Correct "variance supplied" flags passed to KPG1_METHE.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     5-SEP-2011 (DSB):
*        Initialise the "variance supplied" flags in all cases.
*     {enter_further_changes_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'NDF_PAR'          ! NDF__ public constants
      INCLUDE 'PAR_ERR'          ! PAR__ error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER LEXP               ! Maximum expression length
      PARAMETER ( LEXP = 132 )
      INTEGER MXIN               ! Maximum number of input NDFs/params
      PARAMETER ( MXIN = 26 )
      INTEGER MAXNIN             ! Maximum number of array refs in expression
      PARAMETER ( MAXNIN = 50 )
      INTEGER MXBAT
      PARAMETER ( MXBAT = 256 )  ! Size of workspace for pixel batches

*  Local variables :
      INTEGER AEL                 ! Number of pixels along an axis
      CHARACTER * ( MXIN ) ALPBET ! Alphabet
      INTEGER AXPNTR( 1 )        ! Pointer to mapped co-ordinate single-
                                 ! axis workspace arrays
      LOGICAL BAD                ! Whether to check for bad pixels
      LOGICAL BADDR              ! Bad pixels in output data array?
      LOGICAL BADVR              ! Bad pixels in output variance array?
      LOGICAL BOTH               ! Expression has both data and variance
                                 ! from the same NDF?
      CHARACTER * ( 13 ) COMP    ! Overall list of components to process
      CHARACTER * ( 13 ) COMPI( MXIN ) ! Individual NDF component list
      INTEGER DIM( 1 )           ! Workspace array dimensions
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Data type for output array(s)
      INTEGER EL                 ! Number of pixels to process
      CHARACTER * ( 2 * LEXP ) EXPRS ! Expression to evaluate
      LOGICAL EXVAR              ! Expression has NDF variance token?
      CHARACTER * ( 2 * LEXP + 4 ) FOR( 1 ) ! TRANSFORM forward function
      INTEGER I                  ! Loop counter for input NDFs/params
      INTEGER IMAP               ! Compiled mapping identifier
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Implemented numeric type
      INTEGER IVAR               ! Counter for variance arrays
      INTEGER J                  ! Index to the workspace stack
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of output NDF
      INTEGER LIKEID             ! Identifier of template NDF
      CHARACTER * ( DAT__SZLOC ) LOCTR ! Locator for transformation
      CHARACTER * ( 2 ) NAME     ! NDF or parameter name
      INTEGER NC                 ! Number of characters in expression
      INTEGER NDC                ! Number of data co-ordinates
      INTEGER NDF( MXIN )        ! Input NDF identifiers
      CHARACTER * ( 2 ) NDFCMP( MXIN ) ! NDF components in expression
      CHARACTER * ( 2 ) NDFNAM( MXIN ) ! List of input NDFs referenced
      INTEGER NDFOUT             ! Output NDF identifier
      INTEGER NDIM               ! Number of dimensions of template NDF
      INTEGER NIN                ! Number of input NDFs referenced
      INTEGER NINTOT             ! Number of input arrays referenced
      INTEGER NPC                ! Number of pixel co-ordinates
      INTEGER NSUBS              ! Number of token substitutions
      INTEGER NVAR               ! Number of NDFs with variance arrays
      INTEGER PNTR1( 2 )         ! Pointers to mapped input arrays
      INTEGER PNTR2( 2 )         ! Pointers to mapped output arrays
      INTEGER PNTRB              ! Pointer to mapped "batch" work array
      INTEGER PNTRW( 3 )         ! Pointers to mapped workspace arrays
      LOGICAL QUICK              ! Estimate output variance quickly?
      INTEGER STRIDE             ! Number of array elements between
                                 ! increments of co-ordinates
      CHARACTER * ( 2 ) TOKNAM( MXIN ) ! List of NDF-based tokens
                                 ! referenced
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of output NDF
      LOGICAL UNITS              ! Propagate units component?
      LOGICAL VAR                ! Process variance arrays?
      LOGICAL VARI( MXIN )       ! Variance present or needed in an NDF
      LOGICAL VFLAG( MAXNIN )    ! Variance available for a referenced array

*  Local Data:
      DATA ALPBET / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain, analyse and compile the expression.
*  ==========================================

*  Obtain the expression to be evaluated.  Ensure that it is in
*  uppercase for token string comparisons.
      EXPRS = ' '
      CALL PAR_GET0C( 'EXP', EXPRS( : LEXP ), STATUS )
      CALL CHR_UCASE( EXPRS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Substitute any sub-expression (FA-FZ) tokens in the expression.
*  These may contain other tokens, such as those for data arrays,
*  co-ordinates, and constants, hence FA-FZ tokens must be expanded
*  before other tokens  are evaluated.
      CALL KPG1_SATKC( 'F', EXPRS, STATUS )

*  Evaluate the length of the expression.
      NC = MAX( 1, CHR_LEN( EXPRS ) )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Find out which NDF names appear in the expression by attempting to
*  replace each name in turn by itself and seeing if a substitution
*  results.
      NIN = 0
      NINTOT = 0
      DO 10 I = 1, MXIN
         NAME = 'I' // ALPBET( I : I )
         CALL TRN_STOK( NAME, NAME, EXPRS( : NC ), NSUBS, STATUS )

*  If an NDF data-array token appears, then put it into the list of
*  NDFs to be accessed, and into the list of tokens.
         IF ( NSUBS .NE. 0 ) THEN
            NIN = NIN + 1
            NDFNAM( NIN ) = NAME

            NINTOT = NINTOT + 1
            TOKNAM( NINTOT ) = NAME

*  Find out which NDF variances appear in the expression by attempting
*  to replace each name in turn by itself and seeing if a substitution
*  results.
            NAME = 'V' // ALPBET( I : I )
            CALL TRN_STOK( NAME, NAME, EXPRS( : NC ), NSUBS, STATUS )

*  If an NDF variance token appears, then record that the variance is
*  required for that NDF, as well as the data array, and count it.
*  Record the token in the list to be substituted.  Otherwise just note
*  that only the data array is to be accessed.
            IF ( NSUBS .NE. 0 ) THEN
               NINTOT = NINTOT + 1
               TOKNAM( NINTOT ) = NAME
               NDFCMP( NIN ) = 'DV'
            ELSE
               NDFCMP( NIN ) = 'D'
            END IF

         ELSE

*  Find out which NDF variances appear in the expression by attempting
*  to replace each name in turn by itself and seeing if a substitution
*  results.
            NAME = 'V' // ALPBET( I : I )
            CALL TRN_STOK( NAME, NAME, EXPRS( : NC ), NSUBS, STATUS )

*  If an NDF variance token appears, then put it into the list of NDFs
*  to be accessed, and into the list of tokens.
            IF ( NSUBS .NE. 0 ) THEN
               NIN = NIN + 1
               NDFNAM( NIN ) = NAME

               NINTOT = NINTOT + 1
               TOKNAM( NINTOT ) = NAME

*  Record that the only variance is required for that NDF.
               NDFCMP( NIN ) = 'V'
            END IF

         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 999
   10 CONTINUE

*  Find out which tokens for pixel co-ordinate appear in the expression
*  by attempting to replace each name in turn by itself and seeing if a
*  substitution results.
      NPC = 0
      DO 30 I = 1, NDF__MXDIM
         NAME = 'X' // ALPBET( I : I )
         CALL TRN_STOK( NAME, NAME, EXPRS( : NC ), NSUBS, STATUS )

*  If a token for a pixel co-ordinate appears, count it and put it into
*  the list of tokens.
         IF ( NSUBS .NE. 0 ) THEN
            NPC = NPC + 1
            NINTOT = NINTOT + 1
            TOKNAM( NINTOT ) = NAME
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 999
   30 CONTINUE

*  Find out which tokens for data co-ordinate appear in the expression
*  by attempting to replace each name in turn by itself and seeing if a
*  substitution results.
      NDC = 0
      DO 40 I = 1, NDF__MXDIM
         NAME = 'C' // ALPBET( I : I )
         CALL TRN_STOK( NAME, NAME, EXPRS( : NC ), NSUBS, STATUS )

*  If a token for a data co-ordinate appears, count it and put it into
*  the list of tokens.
         IF ( NSUBS .NE. 0 ) THEN
            NDC = NDC + 1
            NINTOT = NINTOT + 1
            TOKNAM( NINTOT ) = NAME
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 999
   40 CONTINUE

*  Abort if too many arrays are references in the expression.
      IF( NINTOT .GT. MAXNIN .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'MAX', MAXNIN )
         CALL MSG_SETI( 'N', NINTOT )
         CALL ERR_REP( 'MATHS_ERR', 'Too many arrays (^N) referenced '//
     :                 'in the expression. Maximum allowed is ^MAX.',
     :                 STATUS )
         GO TO 999
      END IF

*  Finally obtain values for the constants (PA-PZ), as these may not
*  contain any further tokens.
      CALL KPG1_SATKD( 'P', EXPRS, STATUS )

*  Re-evaluate the length of the expression.
      NC = MAX( 1, CHR_LEN( EXPRS ) )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If data co-ordinates are required, we need to check that there is a
*  least one input NDF from which to obtain the co-ordinate system.
      IF ( NDC .GT. 0 .AND. NIN .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'MATHS_NONDF',
     :     'There are data co-ordinates in the expression, but no NDF '/
     :     /'from which to obtain them.', STATUS )
         GOTO 999
      END IF

*  Form the forward transformation function and create a temporary
*  transformation structure.
      FOR( 1 ) = 'OUT=' // EXPRS( : NC )
      CALL TRN_NEW( NINTOT, 1, FOR, TOKNAM, '_REAL:', ' ', ' ', ' ',
     :              LOCTR, STATUS )

*  Compile the transformation and delete the temporary transformation
*  structure.
      CALL TRN_COMP( LOCTR, .TRUE., IMAP, STATUS )
      CALL TRN1_ANTMP( LOCTR, STATUS )

*  If an error occurs, then add contextual information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'MATHS_BADEXP', 'Error compiling expression.',
     :                 STATUS )
         GO TO 999
      END IF

*  Obtain and analyse the input NDFs.
*  =================================

      IF ( NIN .GT. 0 ) THEN

*  Loop to obtain an identifier for each NDF referenced in the
*  expression.
         NVAR = 0
         DO 50 I = 1, NIN
            CALL LPG_ASSOC( NDFNAM( I ), 'READ', NDF( I ), STATUS )

*  Check to see if each NDF has a variance component present and count
*  the number which have.
            CALL NDF_STATE( NDF( I ), 'Variance', VARI( I ), STATUS )

*  Count the number which have
            IF ( VARI( I ) ) NVAR = NVAR + 1

*  Issue a warning whenever variance is in the expression but the NDF
*  has no variance component.
            EXVAR = INDEX( NDFCMP( I ), 'V' ) .NE. 0
            IF ( .NOT. VARI( I ) .AND. EXVAR ) THEN
               CALL MSG_SETC( 'NAME', NDFNAM( I ) )
               CALL MSG_OUT( 'MATHS_NOVAR',
     :           'WARNING: A variance component is not present in the '/
     :           /'NDF ^NAME although one is requested in the '/
     :           /'expression.  Using the default (the data values).',
     :           STATUS )
            END IF
   50    CONTINUE

*  Trim the pixel-index bounds of the input NDFs to match and obtain
*  the number of pixels in the resulting (trimmed) NDFs.
         CALL NDF_MBNDN( 'TRIM', NIN, NDF, STATUS )
         CALL NDF_SIZE( NDF( 1 ), EL, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Determine whether output variance values can be calculated.
         VAR = NVAR .GT. 0

*  If necessary, see whether variance calculations are required.
         IF ( VAR ) CALL PAR_GET0L( 'VARIANCE', VAR, STATUS )

*  If necessary, see whether a quick variance calculation is acceptable
*  (as opposed to a slower but possibly more accurate one).
         IF ( VAR ) CALL PAR_GET0L( 'QUICK', QUICK, STATUS )

*  Generate an overall list of those NDF array components which will
*  need to be considered.
         IF ( VAR ) THEN
            COMP = 'Data,Variance'
         ELSE
            COMP = 'Data'
         END IF

*  Modify the "variance present" flag for each input NDF to turn it
*  into a "variance needed" flag by taking account of whether variance
*  calculations will be performed.  Generate a list of the array
*  components which need to be accessed for each individual input NDF.
         DO 60 I = 1, NIN
            EXVAR = INDEX( NDFCMP( I ), 'V' ) .NE. 0
            BOTH = NDFCMP( I ) .EQ. 'DV'
            VARI( I ) = VARI( I ) .AND. VAR

            IF ( VARI( I ) .OR. BOTH ) THEN
              COMPI( I ) = 'Data,Variance'
            ELSE IF ( EXVAR ) THEN
              COMPI( I ) = 'Variance'
            ELSE
              COMPI( I ) = 'Data'
            END IF
   60    CONTINUE

*  Validate the number of co-ordinate tokens.
*  ==========================================

*  Find the bounds of the trimmed input NDF.
         CALL NDF_BOUND( NDF( 1 ), NDF__MXDIM, LBND, UBND, NDIM,
     :                   STATUS )

*  Validate the number of data co-ordinates in the expression.
         IF ( NDC .GT. NDIM ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'N', NDIM )
            CALL MSG_SETI( 'NDC', NDC )
            CALL ERR_REP( 'MATHS_DATCO',
     :        'There are ^NDC data co-ordinate tokens, but the '/
     :        /'overlapping section of the input NDF only has ^N ' /
     :        /'dimensions.', STATUS )
            GOTO 999
         END IF

*  Validate the number of pixel co-ordinates in the expression.
         IF ( NPC .GT. NDIM ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'N', NDIM )
            CALL MSG_SETI( 'NPC', NPC )
            CALL ERR_REP( 'MATHS_PIXCO',
     :        'There are ^NDC pixel co-ordinate tokens, but the '/
     :        /'overlapping section of the input NDF only has ^N ' /
     :        /'dimensions.', STATUS )
            GOTO 999
         END IF

*  Create an output NDF.
*  ====================

*  See if a units component is to be propagated to the output NDF.
         CALL PAR_GET0L( 'UNITS', UNITS, STATUS )

*  Create an output NDF based on the (alphabetically) first input NDF,
*  propagating the WCS, axis and quality information (and the units
*  information if required).
         IF ( UNITS ) THEN
            CALL LPG_PROP( NDF( 1 ), 'WCS,Axis,Quality,Units', 'OUT',
     :                     NDFOUT, STATUS )
         ELSE
            CALL LPG_PROP( NDF( 1 ), 'WCS,Axis,Quality', 'OUT', NDFOUT,
     :                     STATUS )
         END IF

*  Determine which numeric type to use to process the input arrays and
*  set an appropriate type for the output arrays. This routine supports
*  single- and double-precision floating point arithmetic.
         CALL NDF_MTYPN( '_REAL,_DOUBLE', NIN, NDF, COMP, ITYPE, DTYPE,
     :                   STATUS )
         CALL NDF_STYPE( DTYPE, NDFOUT, COMP, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  No input NDFs
*  =============
      ELSE

*  This occurs when there are only world co-ordinates in the expression.

*  Attempt to open a template NDF, which the output NDF will be based
*  on.  If a NULL response is given, the attributes will be obtained
*  from parameters.  Defer error reporting and attempt to obtain a
*  second NDF to act as a shape template.
         CALL ERR_MARK
         CALL LPG_ASSOC( 'LIKE', 'READ', LIKEID, STATUS )

*  See whether or not a valid template NDF has been given.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Remove the mark set in the error stack above.
            CALL ERR_RLSE

*  Find the bounds of the template NDF.
            CALL NDF_BOUND( LIKEID, NDF__MXDIM, LBND, UBND, NDIM,
     :                      STATUS )

*  See if a units component is to be propagated to the output NDF.
            CALL PAR_GET0L( 'UNITS', UNITS, STATUS )

*  Create an output NDF using the supplied template, propagating the
*  data and axis arrays, the title, label, WCS and history.
            IF ( UNITS ) THEN
               CALL LPG_PROP( LIKEID, 'Data,Units,Axis,WCS', 'OUT',
     :                        NDFOUT, STATUS )
            ELSE
               CALL LPG_PROP( LIKEID, 'Data,Axis,WCS', 'OUT', NDFOUT,
     :                        STATUS )
            END IF

*  Determine which numeric type to use to process the output array.
*  This routine supports single- and double-precision floating point
*  arithmetic.
            CALL NDF_MTYPE( '_REAL,_DOUBLE', NDFOUT, NDFOUT, 'Data',
     :                      ITYPE, DTYPE, STATUS )

*  Interpret a null value for the LIKE parameter as indicating that a
*  template should not be used.  So obtain the values through
*  parameters.
         ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL ERR_RLSE

*  Obtain the data type of the output NDF.  Only allow the primitive
*  floating-point types.
            CALL PAR_CHOIC( 'TYPE', '_REAL', '_DOUBLE,_REAL',
     :                      .TRUE., ITYPE, STATUS )

*  Obtain the required type and bounds for the new NDF, using the
*  attributes of the input NDF as a default.  The number of dimensions
*  required is given by the number of world co-ordinate axes given in
*  the expression.
            CALL PAR_GET1I( 'LBOUND', NPC, LBND, NDIM, STATUS )
            CALL PAR_GET1I( 'UBOUND', NPC, UBND, NDIM, STATUS )

*  Create a new NDF of the right type and size from scratch.
            CALL LPG_CREAT( 'OUT', ITYPE, NPC, LBND, UBND, NDFOUT,
     :                      STATUS )

*  Initialise the axis system.
            CALL NDF_ACRE( NDFOUT, STATUS )

         END IF

*  Find the number of elements in the output NDF.
         CALL NDF_SIZE( NDFOUT, EL, STATUS )

*  By definition there is no variance associated with the pixel
*  co-ordinates.  So set the mapping type for the output NDF and
*  indicate that there is no variance.
         COMP = 'Data'
         VAR = .FALSE.
      END IF

*  Extract the input array values for processing.
*  =============================================

*  Obtain workspace to hold all the input data values.
      DIM( 1 ) = EL * NINTOT
      CALL PSX_CALLOC( DIM( 1 ), ITYPE, PNTRW( 1 ), STATUS )

*  Also obtain workspace for the variance values if necessary.
      IF ( VAR ) THEN
         DIM( 1 ) = EL * NVAR
         CALL PSX_CALLOC( DIM( 1 ), ITYPE, PNTRW( 2 ), STATUS )
      END IF

*  Loop to map the the arrays to be processed in each input NDF for
*  reading.
      BAD = .FALSE.
      IVAR = 0
      J = 0
      IF ( NIN .GE. 1 ) THEN
         DO 70 I = 1, NIN
            CALL KPG1_MAP( NDF( I ), COMPI( I ), ITYPE, 'READ', PNTR1,
     :                    EL, STATUS )

*  Check to see whether any of the input arrays may contain bad pixels.
            IF ( .NOT. BAD ) CALL NDF_BAD( NDF( I ), COMPI( I ),
     :                                     .FALSE., BAD, STATUS )

*  If single-precision arithmetic is being used, then copy the input
*  data into the J'th row of the appropriate work array (considered as
*  2-dimensional).  Count the arrays as they are entered into work
*  space as I loops for the number of NDFs but J counts the number of
*  input arrays.
            IF ( ITYPE .EQ. '_REAL' ) THEN

*  Copy the data array into work space when it is in the expression.
               IF ( INDEX( NDFCMP( I ), 'D' ) .NE. 0 ) THEN
                  J = J + 1
                  CALL KPG1_PROWR( EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                             J,
     :                             %VAL( CNF_PVAL( PNTRW( 1 ) ) ),
     :                             STATUS )
                  VFLAG( J ) = VARI( I )
               END IF

*  Copy the variance array into work space when it is in the expression.
               IF ( INDEX( NDFCMP( I ), 'V' ) .NE. 0 ) THEN
                  J = J + 1
                  CALL KPG1_PROWR( EL, %VAL( CNF_PVAL( PNTR1( 2 ) ) ),
     :                             J,
     :                             %VAL( CNF_PVAL( PNTRW( 1 ) ) ),
     :                             STATUS )
                  VFLAG( J ) = .FALSE.
               END IF

*  If necessary, copy the variance values in the same way into the
*  IVAR'th row of the appropriate work array. Update IVAR to take
*  account of input NDFs which may not have variance information.
               IF ( VARI( I ) ) THEN
                  IVAR = IVAR + 1
                  CALL KPG1_PROWR( EL, %VAL( CNF_PVAL( PNTR1( 2 ) ) ),
     :                             IVAR,
     :                             %VAL( CNF_PVAL( PNTRW( 2 ) ) ),
     :                             STATUS )
               END IF

*  Perform the same process for double-precision arithmetic if
*  necessary.
            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

*  Copy the data array into work space when it is in the expression.
               IF ( INDEX( NDFCMP( I ), 'D' ) .NE. 0 ) THEN
                  J = J + 1
                  CALL KPG1_PROWD( EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                             J,
     :                             %VAL( CNF_PVAL( PNTRW( 1 ) ) ),
     :                             STATUS )
                  VFLAG( J ) = VARI( I )
               END IF

*  Copy the variance array into work space when it is in the expression.
               IF ( INDEX( NDFCMP( I ), 'V' ) .NE. 0 ) THEN
                  J = J + 1
                  CALL KPG1_PROWD( EL, %VAL( CNF_PVAL( PNTR1( 2 ) ) ),
     :                             J,
     :                             %VAL( CNF_PVAL( PNTRW( 1 ) ) ),
     :                             STATUS )
                  VFLAG( J ) = .FALSE.
               END IF

*  If necessary, copy the variance values in the same way into the
*  IVAR'th row of the appropriate work array. Update IVAR to take
*  account of input NDFs which may not have variance information.
               IF ( VARI( I ) ) THEN
                  IVAR = IVAR + 1
                  CALL KPG1_PROWD( EL, %VAL( CNF_PVAL( PNTR1( 2 ) ) ),
     :                             IVAR,
     :                             %VAL( CNF_PVAL( PNTRW( 2 ) ) ),
     :                             STATUS )
               END IF

            END IF

*  Unmap the input arrays once their values have been copied.
            CALL NDF_UNMAP( NDF( I ), COMPI( I ), STATUS )
  70     CONTINUE
      END IF

*  Fill pixel co-ordinate arrays.
*  ==============================

*  Fill the remaining workspace with co-ordinate arrays.  Note that these
*  must be in the order obtained earlier.  First the pixel co-ordinates.
      IF ( NPC .GE. 1 ) THEN

*  Obtain workspace for the full-sized co-ordinate array.
         CALL PSX_CALLOC( EL, ITYPE, PNTRW( 3 ), STATUS )

*  Call different routines depending on the implementation type.
         IF ( ITYPE .EQ. '_REAL' ) THEN

*  Loop for each token, and fill the array with pixel co-ordinates.
            DO I = 1, NPC

*  Compute the stride between co-ordinates values in the full-sized
*  array.
               IF ( I .EQ. 1 ) THEN
                  STRIDE = 1
               ELSE
                  STRIDE = STRIDE * AEL
               END IF

*  Obtain workspace for the co-ordinate array (along one axis).
               AEL = UBND( I ) - LBND( I ) + 1
               CALL PSX_CALLOC( AEL, ITYPE, AXPNTR( 1 ), STATUS )

*  Create a vector of co-ordinates.
               CALL KPG1_SSAZR( AEL, 1.0D0, DBLE( LBND( I ) ) - 0.5D0,
     :                          %VAL( CNF_PVAL( AXPNTR( 1 ) ) ),
     :                          STATUS )

*  Fill the full array with co-ordinates as appropriate for the
*  dimension.
               CALL KPS1_MTHCR( AEL, %VAL( CNF_PVAL( AXPNTR( 1 ) ) ),
     :                          STRIDE, EL,
     :                          %VAL( CNF_PVAL( PNTRW( 3 ) ) ), STATUS )

*  Release the single-axis co-ordinate workspace.
               CALL PSX_FREE( AXPNTR( 1 ), STATUS )

*  Copy the full-sized co-ordinate array into work space.
               J = J + 1
               CALL KPG1_PROWR( EL, %VAL( CNF_PVAL( PNTRW( 3 ) ) ), J,
     :                          %VAL( CNF_PVAL( PNTRW( 1 ) ) ), STATUS )
               VFLAG( J ) = .FALSE.

            END DO

*  Now repeat for double-precision data.
         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

*  Loop for each token, and fill the array with pixel co-ordinates.
            DO I = 1, NPC

*  Compute the stride between co-ordinates values in the full-sized
*  array.
               IF ( I .EQ. 1 ) THEN
                  STRIDE = 1
               ELSE
                  STRIDE = STRIDE * AEL
               END IF

*  Obtain workspace for the co-ordinate array.
               AEL = UBND( I ) - LBND( I ) + 1
               CALL PSX_CALLOC( AEL, ITYPE, AXPNTR( 1 ), STATUS )

*  Create a vector of co-ordinates.
               CALL KPG1_SSAZD( AEL, 1.0D0, DBLE( LBND( I ) ) - 0.5D0,
     :                          %VAL( CNF_PVAL( AXPNTR( 1 ) ) ),
     :                          STATUS )

*  Fill the full array with co-ordinates as appropriate for the
*  dimension.
               CALL KPS1_MTHCD( AEL, %VAL( CNF_PVAL( AXPNTR( 1 ) ) ),
     :                          STRIDE, EL,
     :                          %VAL( CNF_PVAL( PNTRW( 3 ) ) ), STATUS )

*  Release the co-ordinate workspace.
               CALL PSX_FREE( AXPNTR( 1 ), STATUS )

*  Copy the full-sized co-ordinate array into work space.
               J = J + 1
               CALL KPG1_PROWD( EL, %VAL( CNF_PVAL( PNTRW( 3 ) ) ), J,
     :                          %VAL( CNF_PVAL( PNTRW( 1 ) ) ), STATUS )
               VFLAG( J ) = .FALSE.
            END DO
         END IF

*  Release the full-sized co-ordinate workspace.
         CALL PSX_FREE( PNTRW( 3 ), STATUS )

      END IF

*  Fill data co-ordinate arrays.
*  =============================
      IF ( NDC .GE. 1 ) THEN

*  Obtain workspace for the full-sized co-ordinate array.
         CALL PSX_CALLOC( EL, ITYPE, PNTRW( 3 ), STATUS )

*  Call different routines depending on the implementation type.
         IF ( ITYPE .EQ. '_REAL' ) THEN

*  Loop for each token, and fill the array with pixel co-ordinates.
            DO I = 1, NDC

*  Compute the stride between co-ordinates values in the full-sized
*  array.
               IF ( I .EQ. 1 ) THEN
                  STRIDE = 1
               ELSE
                  STRIDE = STRIDE * AEL
               END IF

*  Map the data co-ordinates for the current axis.
               CALL NDF_AMAP( NDF( 1 ), 'Centre', I, ITYPE, 'READ',
     :                        AXPNTR( 1 ), AEL, STATUS )

*  Fill the full array with co-ordinates as appropriate for the
*  dimension.
               CALL KPS1_MTHCR( AEL, %VAL( CNF_PVAL( AXPNTR( 1 ) ) ),
     :                          STRIDE, EL,
     :                          %VAL( CNF_PVAL( PNTRW( 3 ) ) ), STATUS )

*  Release the single-axis co-ordinate array.
               CALL NDF_AUNMP( NDF( 1 ), 'Centre', I, STATUS )

*  Copy the full-sized data co-ordinate array into work space stack.
               J = J + 1
               CALL KPG1_PROWR( EL, %VAL( CNF_PVAL( PNTRW( 3 ) ) ), J,
     :                          %VAL( CNF_PVAL( PNTRW( 1 ) ) ), STATUS )
               VFLAG( J ) = .FALSE.
            END DO

*  Now repeat for double-precision data.
         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

*  Loop for each token, and fill the array with pixel co-ordinates.
            DO I = 1, NDC

*  Compute the stride between co-ordinates values in the full-sized
*  array.
               IF ( I .EQ. 1 ) THEN
                  STRIDE = 1
               ELSE
                  STRIDE = STRIDE * AEL
               END IF

*  Map the data co-ordinates for the current axis.
               CALL NDF_AMAP( NDF( 1 ), 'Centre', I, ITYPE, 'READ',
     :                        AXPNTR( 1 ), AEL, STATUS )

*  Fill the full array with co-ordinates as appropriate for the
*  dimension.
               CALL KPS1_MTHCD( AEL, %VAL( CNF_PVAL( AXPNTR( 1 ) ) ),
     :                          STRIDE, EL,
     :                          %VAL( CNF_PVAL( PNTRW( 3 ) ) ), STATUS )

*  Release the single-axis co-ordinate array.
               CALL NDF_AUNMP( NDF( 1 ), 'Centre', I, STATUS )

*  Copy the full-sized data co-ordinate array into work space stack.
               J = J + 1
               CALL KPG1_PROWD( EL, %VAL( CNF_PVAL( PNTRW( 3 ) ) ), J,
     :                          %VAL( CNF_PVAL( PNTRW( 1 ) ) ), STATUS )
               VFLAG( J ) = .FALSE.
            END DO

         END IF

*  Release the full-sized co-ordinate workspace.
         CALL PSX_FREE( PNTRW( 3 ), STATUS )

      END IF

*  Map the output arrays for writing.
      CALL KPG1_MAP( NDFOUT, COMP, ITYPE, 'WRITE', PNTR2, EL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Evaluate the expression.
*  =======================

*  If variance calculations are not being performed, then apply the
*  compiled mapping to the input data array values held in workspace
*  using the appropriate type of arithmetic.
      IF ( .NOT. VAR ) THEN
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL TRN_TRNR( BAD, EL, NINTOT, EL,
     :                     %VAL( CNF_PVAL( PNTRW( 1 ) ) ),
     :                     IMAP, EL, 1, %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                     STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL TRN_TRND( BAD, EL, NINTOT, EL,
     :                     %VAL( CNF_PVAL( PNTRW( 1 ) ) ),
     :                     IMAP, EL, 1, %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                     STATUS )
         END IF

*  If variance calculations are being performed, then obtain the extra
*  workspace needed for these calculations.
      ELSE
         DIM( 1 ) = MXBAT * ( NINTOT + 3 )
         CALL PSX_CALLOC( DIM( 1 ), ITYPE, PNTRB, STATUS )

*  Call the appropriate routine to apply the compiled mapping and
*  evaluate output variance estimates, depending on the type of
*  arithmetic required.
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_MTHER( BAD, EL, NINTOT,
     :                       %VAL( CNF_PVAL( PNTRW( 1 ) ) ), VFLAG,
     :                       %VAL( CNF_PVAL( PNTRW( 2 ) ) ),
     :                       IMAP, QUICK, MXBAT,
     :                       %VAL( CNF_PVAL( PNTRB ) ),
     :                       %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                       %VAL( CNF_PVAL( PNTR2( 2 ) ) ),
     :                       BADDR, BADVR, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_MTHED( BAD, EL, NINTOT,
     :                       %VAL( CNF_PVAL( PNTRW( 1 ) ) ), VFLAG,
     :                       %VAL( CNF_PVAL( PNTRW( 2 ) ) ),
     :                       IMAP, QUICK, MXBAT,
     :                       %VAL( CNF_PVAL( PNTRB ) ),
     :                       %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                       %VAL( CNF_PVAL( PNTR2( 2 ) ) ),
     :                       BADDR, BADVR, STATUS )
         END IF

*  Release the extra workspace obtained above.
         CALL PSX_FREE( PNTRB, STATUS )

*  Note whether bad pixel values may be present in the output arrays.
         CALL NDF_SBAD( BADDR, NDFOUT, 'Data', STATUS )
         CALL NDF_SBAD( BADVR, NDFOUT, 'Variance', STATUS )
      END IF

*  Obtain a new title for the output NDF.
      CALL NDF_CINP( 'TITLE', NDFOUT, 'Title', STATUS )

*  Clean up.
*  ========
  999 CONTINUE

*  Release the work arrays.
      CALL PSX_FREE( PNTRW( 1 ), STATUS )
      IF ( VAR ) CALL PSX_FREE( PNTRW( 2 ), STATUS )

*  Annul the compiled mapping.
      CALL TRN_ANNUL( IMAP, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report contextual information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'MATHS_ERR',
     :     'MATHS: Error applying a mathematical expression to NDF ' /
     :     /'data structure(s).', STATUS )
      END IF

      END
