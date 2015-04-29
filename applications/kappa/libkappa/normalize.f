      SUBROUTINE NORMALIZE( STATUS )
*+
*  Name:
*     NORMALIZE

*  Purpose:
*     Normalises one NDF to a similar NDF by calculating a scale factor
*     and zero-point difference.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NORMALIZE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application compares the data values in one NDF against the
*     corresponding values in the other NDF.  A least-squares
*     straight-line is then fitted to the relationship between the two
*     sets of data values in order to determine the relative scale
*     factor and any zero-level offset between the NDFs (the offset may
*     optionally be fixed at zero - see parameter ZEROFF).  To reduce
*     computation time, the data points are binned according to the
*     data value in the first NDF.  The mean data value within each bin
*     is used to find the fit and weights are applied according to the
*     number of pixels which contribute to each bin.
*
*     To guard against erroneous data values, which can corrupt the fit
*     obtained, the application then performs a number of iterations.
*     It calculates a noise estimate for each bin according to the rms
*     deviation of data values in the bin from the straight-line fit
*     obtained previously.  It then re-bins the data, omitting values
*     which lie more than a specified number of standard deviations
*     from the expected value in each bin.  The straight-line fit is
*     then re-calculated.  You can specify the number of standard
*     deviations and the number of iterations used.
*
*     A plot is produced after the final iteration showing the bin
*     centres, with error bars representing the spread of values in each
*     bin.  The best fitting straight line is overlayed on this plot.
*
*     Optionally, an output NDF can be created containing a normalised
*     version of the data array from the first input NDF.
*
*     For the special case of two-dimensional images, if IN2 (or IN1) spans
*     only a single row or column, it can be used to normalize each row or
*     column of IN1 (or IN2) in turn.  See Parameter LOOP.

*  Usage:
*     normalize in1 in2 out

*  ADAM Parameters:
*     AXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes are to be drawn around the
*        plot.  The width of the margins left for the annotation may be
*        controlled using Parameter MARGIN.  The appearance of the axes
*        (colours, founts, etc,) can be controlled using the Parameter
*        STYLE.  The dynamic default is TRUE if CLEAR is TRUE, and FALSE
*        otherwise. []
*     CLEAR = _LOGICAL (Read)
*        If TRUE the current picture is cleared before the plot is
*        drawn.  If CLEAR is FALSE not only is the existing plot
*        retained, but also an attempt is made to align the new picture
*        with the existing picture.  Thus you can generate a composite
*        plot within a single set of axes, say using different colours
*        or modes to distinguish data from different datasets.  [TRUE]
*     DATARANGE( 2 ) = _REAL (Read)
*        This parameter may be used to override the auto-scaling
*        feature.   If given, two real numbers should be supplied
*        specifying the lower and upper data values in IN2, between
*        which data will be used.  If a null (!) value is supplied, the
*        values used are the auto-scaled values, calculated according to
*        the value of PCRANGE. Note, this parameter controls the range
*        of data used in the fitting algorithm.  The range of data
*        displayed in the plot can be specified separately using
*        Parameters XLEFT, XRIGHT, YBOT, and YTOP. [!]
*     DEVICE = DEVICE (Read)
*        The graphics workstation on which to produce the plot.  If a
*        null value (!) is supplied no plot will be made.
*        [Current graphics device]
*     IN1 = NDF (Read)
*        The NDF to be normalised.
*     IN2 = NDF (Read)
*        The NDF to which IN1 will be normalised.
*     LOOP = _LOGICAL (Read)
*        If both IN1 and IN2 are two-dimensional, but one of them spans
*        only a single row or column, then setting LOOP to TRUE will cause
*        every row or column in to be normalised independently of each other.
*        Specifically, if IN2 spans only a single row or column, then it
*        will be used to normalise each row or column of IN1 in turn. Any
*        output NDF (see parameter OUT) will have the shape and size of
*        IN1. If IN1 spans only a single row or column, then it will be
*        normalised in turn by each row or column of IN2. Any output NDF
*        (see parameter OUT) will then have the shape and size of IN2. In
*        either case, the details of the fit for each row or column will
*        be displayed separately. [FALSE]
*     MARGIN( 4 ) = _REAL (Read)
*        The widths of the margins to leave for axis annotation, given
*        as fractions of the corresponding dimension of the current
*        picture.  Four values may be given, in the order: bottom,
*        right, top, left.  If less than four values are given, extra
*        values are used equal to the first supplied value.  If these
*        margins are too narrow any axis annotation may be clipped.  If
*        a null (!) value is supplied, the value used is 0.15 (for all
*        edges) if annotated axes are produced, and zero otherwise.
*        [current value]
*     MARKER = _INTEGER (Read)
*        Specifies the symbol with which each position should be marked
*        in the plot.  It should be given as an integer PGPLOT-marker
*        type.  For instance, 0 gives a box, 1 gives a dot, 2 gives a
*        cross, 3 gives an asterisk, 7 gives a triangle.  The value must
*        be larger than or equal to -31.  [current value]
*     MINPIX = _INTEGER (Read)
*        The minimum number of good pixels required in a bin before it
*        contributes to the fitted line.  It must be in the range 1 to
*        the number of pixels per bin.  [2]
*     NBIN = _INTEGER (Read)
*        The number of bins to use when binning the scatter plot prior
*        to fitting a straight line, in the range 2 to 10000.  [50]
*     NITER = _INTEGER (Read)
*        The number of iterations performed to reject bad data values
*        in the range 0 to 100.  [2]
*     NSIGMA = _REAL (Read)
*        The number of standard deviations at which bad data is
*        rejected.  It must lie in the range 0.1 to 1.0E6.  [3.0]
*     OFFSET = _REAL (Write)
*        An output parameter giving the offset in the linear
*        normalisation expression: IN1 = SLOPE * IN2 + OFFSET.
*     OUT = NDF (Write)
*        An optional output NDF to hold a version of IN1 which is
*        normalised to IN2.  A null (!) value indicates that an output
*        NDF is not required. See also parameter LOOP.
*     PCRANGE( 2 ) = _REAL (Read)
*        This parameter takes two real values in the range 0 to 100 and
*        is used to modify the action of the auto-scaling algorithm
*        which selects the data to use in the fitting algorithm.  The
*        two values correspond to the percentage points in the histogram
*        of IN2 at which the lower and upper cuts on data value are
*        placed.  With the default value, the plots will omit those
*        pixels that lie in the lower and upper two-percent intensity
*        range of IN2.  Note, this parameter controls the range of data
*        used in the fitting algorithm.  The range of data displayed in
*        the plot can be specified separately using Parameters XLEFT,
*        XRIGHT, YBOT, and YTOP.  [2,98]
*     SLOPE = _REAL (Write)
*        An output parameter giving the slope of the linear
*        normalisation expression: IN1 = SLOPE * IN2 + OFFSET.
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to
*        use when drawing the annotated axes, data values, error bars,
*        and best-fitting line.
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text
*        file preceded by an up-arrow character "^".  Such text files
*        should contain further comma-separated lists which will be read
*        and interpreted in the same manner.  Attribute settings are
*        applied in the order in which they occur within the list, with
*        later settings overriding any earlier settings given for the
*        same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*
*        where <name> is the name of a plotting attribute, and <value>
*        is the value to assign to the attribute.  Default values will
*        be used for any unspecified attributes.  All attributes will be
*        defaulted if a null value (!)---the initial default---is
*        supplied.  To apply changes of style to only the current
*        invocation, begin these attributes with a plus sign.  A mixture
*        of persistent and temporary style changes is achieved by
*        listing all the persistent attributes followed by a plus sign
*        then the list of temporary attributes.
*
*        See section "Plotting Attributes" in SUN/95 for a description
*        of the available attributes.  Any unrecognised attributes are
*        ignored (no error is reported).
*
*        The appearance of the best-fitting straight line is controlled
*        by the attributes Colour(Curves), Width(Curves), etc. (the
*        synonym Linemay be used in place of Curves).  The appearance of
*        markers is controlled by Colour(Markers), Width(Markers), etc.
*        (the synonym Symbols may be used in place of Markers).  The
*        appearance of the error bars is controlled using
*        Colour(ErrBars), Width(ErrBars), etc.  Note, Size(ErrBars)
*        controls the length of the serifs (i.e. the cross pieces at
*        each end of the error bar), and defaults to 1.0.
*        [current value]
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the NDF supplied for Parameter IN1 to be used
*        instead.  [!]
*     XLEFT = _DOUBLE (Read)
*        The axis value to place at the left hand end of the horizontal
*        axis of the plot.  If a null (!) value is supplied, the value
*        used is the minimum data value used by the fitting algorithm
*        from IN2 (with a small margin).  The value supplied may be
*        greater than or less than the value supplied for XRIGHT.  [!]
*     XRIGHT = _DOUBLE (Read)
*        The axis value to place at the right hand end of the horizontal
*        axis of the plot.  If a null (!) value is supplied, the value
*        used is the maximum data value used by the fitting algorithm
*        from IN2 (with a small margin).  The value supplied may be
*        greater than or less than the value supplied for XLEFT.  [!]
*     YBOT = _DOUBLE (Read)
*        The axis value to place at the bottom end of the vertical axis
*        of the plot.  If a null (!) value is supplied, the value used
*        is the minimum data value used by the fitting algorithm from
*        IN1 (with a small margin).  The value supplied may be greater
*        than or less than the value supplied for YTOP.  []
*     YTOP = _DOUBLE (Read)
*        The axis value to place at the top end of the vertical axis of
*        the plot.  If a null (!) value is supplied, the value used is
*        the maximum data value used by the fitting algorithm from IN1
*        (with a small margin).  The value supplied may be greater than
*        or less than the value supplied for YBOT.  [!]
*     ZEROFF = _LOGICAL (Read)
*        If TRUE, the offset of the linear fit is constrained to be
*        zero. [FALSE]

*  Examples:
*     normalize cl123a cl123b cl123c
*        This normalises NDF cl123a to the NDF cl123b.  A plot of the
*        fit is made on the current graphics device, and the resulting
*        normalisation scale and offset are written only to the
*        normalize.sdf parameter file (as in the all the examples below
*        except where noted).  The NDF cl123c is the normalised version
*        of the input cl123a.
*     normalize cl123a cl123b
*               style="'size(errba)=0,title=Gain calibration'"
*        This normalises NDF cl123a to the NDF cl123b.  A plot of the
*        fit is made on the current graphics device with the title
*        "Gain calibration".  The error bars are drawn with no serifs.
*     normalize cl123a cl123b cl123c offset=(shift) slope=(scale)
*        This normalises NDF cl123a to the NDF cl123b.  A plot of the
*        fit is made on the current graphics device.  The resulting
*        normalisation scale and offset are written to the ICL
*        variables SCALE and SHIFT respectively, where they could be
*        passed to another application via an ICL procedure.  The NDF
*        cl123c is the normalised version of the input cl123a.
*     normalize in2=old in1=new out=! device=xwindows style=^normstyle
*        This normalises NDF new to the NDF old.  A plot of the fit is
*        made on the xwindows device, using the plotting style defined
*        in text file normstyle.  No output NDF is produced.
*     normalize in1=new in2=old out=young niter=5 pcrange=[3,98.5]
*        This normalises NDF new to the NDF old.  It has five iterations
*        to reject outliers from the linear regression, and forms the
*        regression using pixels in old whose data values lie between
*        the 3 and 98.5 percentiles, comparing with the corresponding
*        pixels in new.  A plot of the fit is made on the current
*        graphics device.  The NDF young is the normalised version of
*        the input new.

*  Notes:
*     -  The application stores two pictures in the graphics database
*     in the following order: a FRAME picture containing the annotated
*     axes and data plot, and a DATA picture containing just the data
*     plot.  Note, the FRAME picture is only created if annotated axes
*     have been drawn, or if non-zero margins were specified using
*     Parameter MARGIN.  The world co-ordinates in the DATA picture will
*     correspond to data values in the two NDFs.

*  Related Applications:
*     CCDPACK: MAKEMOS.

*  Implementation Status:
*     -  The routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS, and HISTORY components of an
*     NDF, and propagates all extensions to the output NDF.  All
*     propagated components come from the NDF to be normalised.
*     -  At the moment, variance values are not used in the fitting
*     algorithm but are modified in the output NDF to take account of
*     the scaling introduced by the normalisation.  (A later version may
*     take account of variances in the fitting algorithm.)
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  Only _REAL data can be processed directly.  Other non-complex
*     numeric data types will undergo a type conversion before
*     processing occurs.  _DOUBLE data cannot be processed due to a
*     loss of precision.
*     -  The pixel bounds of the two input NDFs are matched by trimming
*     before calculating the normalisation constants, and are mapped as
*     vectors to allow processing of NDFs of any dimensionality.  An
*     output NDF may optionally be produced which is based on the
*     first input NDF (IN1) by applying the calculated normalisation
*     constants to IN1.

*  Implementation Deficiencies:
*     -  Variances in the input data could be used in the fitting
*     algorithm but are not at the moment.
*     -  Bad pixels are nearly always assumed to be present.

*  Copyright:
*     Copyright (C) 1990-1992 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998-1999, 2001, 2004 Central Laboratory of
*     the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2007, 2010, 2011, 2013 Science & Technology
*     Facilities Council.
*     All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1-JUN-1990 (DSB):
*        Original version.
*     22-JUN-1990 (DSB):
*        Graphics added.
*     1990 October 4 (MJC):
*        Added standard KAPPA axis-style parameters, removed tabs,
*        extended and reformatted the prologue, added some contextual
*        error messages, passed pointers as arrays, and added unmapping
*        calls to prevent the output NDF from being deleted.
*     1991 August 20 (MJC):
*        Added FONT parameter.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1995 May 2 (MJC):
*        Sets the bad-pixel flags.  Removed old histogram subroutine
*        calls.  Used PSX for workspace.  Made the Usage and Examples
*        lowercase.  Added Related Applications.  Title propagated.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     17-JUN-1998 (DSB):
*        Converted graphics to AST/PGPLOT.
*     26-OCT-1999 (DSB):
*        Made MARGIN a fraction of the current picture, not the DATA
*        picture.
*     15-AUG-2001 (DSB):
*        Changed default for MINPIX to [2].
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 February 24 (MJC):
*        Added new CUMUL argument set to .FALSE. to KPG1_GHSTx call.
*     2006 April 12 (MJC):
*        Remove unused variable and wrapped long lines.
*     2010 October 14 (MJC):
*        Document temporary style attributes.
*     2011-08-22 (TIMJ):
*        Add new WGTS and WEIGHT arguments to KPG1_GHSTx calls.
*     16-SEP-2011 (DSB):
*        Added ZEROFF parameter.
*     17-JAN-2013 (DSB):
*        Added LOOP parameter.
*     27-AUG-2013 (DSB):
*        Allow looping over IN2 as well as over IN1.
*     8-APR-2014 (DSB):
*        Fix a bug in looping mode that caused a "no pixels in common"
*        error from NDF_MBND if the single line NDF did not have an
*        origin of 1 on the axis that spans a single pixel.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PAR_ERR'          ! Parameter-system error constants
      INCLUDE 'PRM_PAR'          ! Data-type constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER HISIZE             ! Size of histogram to use for
                                 ! calculating auto scaling range.
      PARAMETER ( HISIZE = 1000 )

*  Local Variables:
      INTEGER  AXIS              ! Index of axis to loop over
      INTEGER  I                 ! Pixel index on looping axis
      INTEGER  IHI               ! Highest looping pixel index
      INTEGER  ILO               ! Lowest looping pixel index
      INTEGER  ISTAT             ! Temporary status value
      INTEGER  LBND1( NDF__MXDIM )! Lower pixel bounds of ndf1
      INTEGER  LBND2( NDF__MXDIM )! Lower pixel bounds of ndf2
      INTEGER  MAXPOS( 2 )       ! Position of maximum in IN2 data array
      INTEGER  MINPIX            ! Minimum number of good pixels per bin
                                 ! when fitting
      INTEGER  MINPOS( 2 )       ! Position of minimum in IN2 data array
      INTEGER  NBAD2             ! No. of bad pixels in IN2 data array
      INTEGER  NBIN              ! No. of bins to use when fitting
      INTEGER  NDF1B             ! Base NDF identifier for input IN1
      INTEGER  NDF1S             ! NDF section identifier for input IN1
      INTEGER  NDF1T             ! Identifier for supplied IN1 NDF
      INTEGER  NDF2S             ! NDF section identifier for input IN2
      INTEGER  NDF2T             ! Identifier for supplied IN2 NDF
      INTEGER  NDFO              ! Identifier for output NDF
      INTEGER  NDFOUT            ! NDF identifier for OUT
      INTEGER  NDIM1             ! No. of axis in ndf1
      INTEGER  NDIM2             ! No. of axis in ndf2
      INTEGER  NEL1B             ! Number of elements mapped from base
                                 ! NDF of IN1
      INTEGER  NELS              ! Number of elements mapped from the
                                 ! NDF sections
      INTEGER  NITER             ! Number of rejection iterations to
                                 ! perform
      INTEGER  PNT1BD( 1 )       ! Pointer to mapped base IN1 data array
      INTEGER  UBND1( NDF__MXDIM )! Upper pixel bounds of ndf1
      INTEGER  UBND2( NDF__MXDIM )! Upper pixel bounds of ndf2
      INTEGER  PNT1BV( 1 )       ! Pointer to mapped base IN1 variance
                                 ! array
      INTEGER  PNT1S( 1 )        ! Pointer to mapped IN1 section data
                                 ! array
      INTEGER  PNT2S( 1 )        ! Pointer to mapped IN2 section data
                                 ! array
      INTEGER  PNTOD( 1 )        ! Pointer to mapped output data array
      INTEGER  PNTOV( 1 )        ! Pointer to mapped output variance
                                 ! array
      INTEGER  PNTW0             ! Pointer to temporary work space
      INTEGER  PNTW1             ! Pointer to temporary work space
      INTEGER  PNTW2             ! Pointer to temporary work space
      INTEGER  PNTW3             ! Pointer to temporary work space
      INTEGER  PNTW4             ! Pointer to temporary work space
      INTEGER  PNTW5             ! Pointer to temporary work space
      INTEGER  SHIFT( 2 )        ! Pixel origin shift
      LOGICAL  BAD               ! Any bad pixels found?
      LOGICAL  DEFIND            ! NDF component is in a defined state?
      LOGICAL  LOOP              ! Loop over rows or columns?
      LOGICAL  LPOVR1            ! Loop over rows or columns in IN1?
      LOGICAL  OUTRQD            ! Is an output NDF is to be generated?
      LOGICAL  VAR1              ! IN1 has a defined variance component?
      LOGICAL  ZEROFF            ! Fix fit offset at zero?
      REAL     DRANGE( 2 )       ! Limits on IN2 data values to be used
      REAL     DRDEF( 2 )        ! Suggested default limits on IN2
                                 ! values
      REAL     MAX2              ! Maximum value in IN2 data array
      REAL     MIN2              ! Minimum value in IN2 data array
      REAL     NSIGMA            ! Number of standard dev's to clip at
      REAL     OFFSET            ! Constant offset in final fit
      REAL     PCDEF( 2 )        ! Suggested default percentage range
      REAL     PCRANG( 2 )       ! Percentage range for autoscaling
      REAL     SLOPE             ! Gradient of final fit
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an NDF context.
      CALL NDF_BEGIN

*  Get NDF identifiers for the two input NDFs.
      CALL LPG_ASSOC( 'IN1', 'READ', NDF1T, STATUS )
      CALL LPG_ASSOC( 'IN2', 'READ', NDF2T, STATUS )

*  If an error occurred, abort.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Get an NDF identifier for the output NDF. The output is based on IN1
*  and inherits all the components of IN1 except the data and variance
*  arrays.
      CALL LPG_PROP( NDF1T, 'WCS,Axis,Quality,Units', 'OUT', NDFO,
     :               STATUS )

*  If an output NDF was obtained successfully...
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Set the OUTRQD flag to indicate that the output values should be
*  calculated.
         OUTRQD = .TRUE.

*  If a null value for OUT was given, set OUTRQD flag appropriately
*  and clear the error condition.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         OUTRQD = .FALSE.
         CALL ERR_ANNUL( STATUS )

         CALL MSG_OUT( 'REPORT', ' ', STATUS )
         CALL MSG_OUT( 'REPORT', ' No output NDF will be created',
     :                  STATUS )
         CALL MSG_OUT( 'REPORT', ' ', STATUS )

      END IF


*  Now see if we need to loop over rows or coluns. First get the bounds
*  of the two input NDFs.
      CALL NDF_BOUND( NDF1T, NDF__MXDIM, LBND1, UBND1, NDIM1, STATUS )
      CALL NDF_BOUND( NDF2T, NDF__MXDIM, LBND2, UBND2, NDIM2, STATUS )

*  We only loop if both are two-dimensional (this restriction could
*  probably be removed if needed).
      IF( NDIM1 .EQ. 2 .AND. NDIM2 .EQ. 2 ) THEN

*  We only loop if IN1 or IN2 spans a single pixel on one of the two pixel
*  axes. The LPOVR1 flag is .TRUE. if we will be looping over the rows or
*  columns in IN1 and is .FALSE if we will be looping over the rows or
*  columns in IN2.
         IF( LBND1( 1 ) .EQ. UBND1( 1 ) ) THEN
            AXIS = 1
            LPOVR1 = .FALSE.
         ELSE IF( LBND1( 2 ) .EQ. UBND1( 2 ) ) THEN
            AXIS = 2
            LPOVR1 = .FALSE.
         ELSE IF( LBND2( 1 ) .EQ. UBND2( 1 ) ) THEN
            AXIS = 1
            LPOVR1 = .TRUE.
         ELSE IF( LBND2( 2 ) .EQ. UBND2( 2 ) ) THEN
            AXIS = 2
            LPOVR1 = .TRUE.
         ELSE
            AXIS = 0
         END IF

      ELSE
         AXIS = 0
      END IF

*  If the above conditions for looping are met, see if the user wants to
*  loop.
      IF( AXIS .GT. 0 ) THEN
         CALL PAR_GET0L( 'LOOP', LOOP, STATUS )
         IF( .NOT. LOOP ) AXIS = 0
      END IF

*  If we are looping over the rows or columns of IN2, we need to reshape
*  the output to match IN2.
      IF( OUTRQD .AND. LOOP .AND. .NOT. LPOVR1 ) THEN
         CALL NDF_SBND( 2, LBND2, UBND2, NDFO, STATUS )
      END IF

*  If so, set the bounds on the looping axis, and store the initial
*  pixel shifts that move IN2 (or IN1)  NDF so that it is in the correct
*  position to normalize (or be normalized by) the first row or column of
*  IN1 (or IN2).
      IF( AXIS .GT. 0 ) THEN
         SHIFT( 1 ) = 0
         SHIFT( 2 ) = 0
         IF( LPOVR1 ) THEN
            ILO = LBND1( AXIS )
            IHI = UBND1( AXIS )
            SHIFT( AXIS ) = ILO - LBND2( AXIS )
         ELSE
            ILO = LBND2( AXIS )
            IHI = UBND2( AXIS )
            SHIFT( AXIS ) = ILO - LBND1( AXIS )
         END IF

*  If we are not looping, we only pass through the loop once, using the
*  supplied NDF identifiers without change.
      ELSE
         ILO = 1
         IHI = 1
         NDF1S = NDF1T
         NDF2S = NDF2T
         NDFOUT = NDFO
      END IF

*  Do all required values on the looping axis (if any)
      DO I = ILO, IHI
         CALL NDF_BEGIN

*  Prepare for the next value on the looping axis.
         IF( AXIS .GT. 0 ) THEN

*  First deal with cases where we are looping over rows or columns in IN1.
            IF( LPOVR1 ) THEN

*  Get a section of IN1 that is restricted to the current value on the
*  looping axis.
               LBND1( AXIS ) = I
               UBND1( AXIS ) = I
               CALL NDF_SECT( NDF1T, 2, LBND1, UBND1, NDF1S, STATUS )

*  If we are producing an output NDF, get a section of it that is
*  restricted to the current value on the looping axis.
               IF( OUTRQD ) CALL NDF_SECT( NDFO, 2, LBND1, UBND1,
     :                                     NDFOUT, STATUS )

*  Shift the supplied IN2 NDF so that it is aligned with the above IN1
*  section. We take a section covering the whole supplied IN1 NDF first
*  because NDF_SHIFT requires write access to the underlying input NDF
*  otherwise.
               CALL NDF_SECT( NDF2T, 2, LBND2, UBND2, NDF2S, STATUS )
               CALL NDF_SHIFT( 2, SHIFT, NDF2S, STATUS )

*  Now deal with cases where we are looping over rows or columns in IN2.
            ELSE

*  Get a section of IN2 that is restricted to the current value on the
*  looping axis.
               LBND2( AXIS ) = I
               UBND2( AXIS ) = I
               CALL NDF_SECT( NDF2T, 2, LBND2, UBND2, NDF2S, STATUS )

*  If we are producing an output NDF, get a section of it that is
*  restricted to the current value on the looping axis.
               IF( OUTRQD ) CALL NDF_SECT( NDFO, 2, LBND2, UBND2,
     :                                     NDFOUT, STATUS )

*  Shift the supplied IN1 NDF so that it is aligned with the above IN2
*  section. We take a section covering the whole supplied IN2 NDF first
*  because NDF_SHIFT requires write access to the underlying input NDF
*  otherwise.
               CALL NDF_SECT( NDF1T, 2, LBND1, UBND1, NDF1S, STATUS )
               CALL NDF_SHIFT( 2, SHIFT, NDF1S, STATUS )

            END IF

*  Update the required shift ready for the next pass.
            SHIFT( AXIS ) = SHIFT( AXIS ) + 1

*  Tell the user what we are doing.
            CALL MSG_SETI( 'I', I )
            IF( AXIS .EQ. 1 ) THEN
               CALL MSG_SETC( 'W1', 'column' )
            ELSE
               CALL MSG_SETC( 'W1', 'row' )
            END IF
            IF( LPOVR1 ) THEN
               CALL MSG_SETC( 'W2', 'IN1' )
            ELSE
               CALL MSG_SETC( 'W2', 'IN2' )
            END IF
            CALL MSG_BLANK( STATUS )
            CALL MSG_BLANK( STATUS )
            CALL MSG_OUT( ' ', ' Doing ^W1 ^I of ^W2...',
     :                    STATUS  )
            CALL MSG_BLANK( STATUS )
         END IF

*  If an output NDF is being created, clone the NDF identifier for IN1
*  for use when calculating the output values.
         IF( OUTRQD ) CALL NDF_CLONE( NDF1S, NDF1B, STATUS )

*  Create sections of the two input NDFs with matched pixel bounds
*  by trimming the input NDFs.
         CALL NDF_MBND( 'TRIM', NDF1S, NDF2S, STATUS )

*  Map the data arrays of the two NDF sections.
         CALL KPG1_MAP( NDF1S, 'DATA', '_REAL', 'READ', PNT1S, NELS,
     :                 STATUS )
         CALL KPG1_MAP( NDF2S, 'DATA', '_REAL', 'READ', PNT2S, NELS,
     :                 STATUS )

*  If an error has occured, abort.
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Get percentage histogram range for scaling the binning of NDF2, using
*  2 to 98 per cent as the dynamic defaults.
         PCDEF( 1 ) = 2.0
         PCDEF( 2 ) = 98.0
         CALL PAR_GDR1R( 'PCRANGE', 2, PCDEF, 0.0, 100.0, .FALSE.,
     :                   PCRANG, STATUS )

*  Convert them to fractions.
         PCRANG( 1 ) = 0.01 * PCRANG( 1 )
         PCRANG( 2 ) = 0.01 * PCRANG( 2 )

*  Find if there are any bad values in the section of the 2nd input NDF.
         CALL NDF_BAD( NDF2S, 'Data', .FALSE., BAD, STATUS )

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get maximum and minimum values in the data array of the IN2 NDF
*  section.
         CALL KPG1_MXMNR( BAD, NELS, %VAL( CNF_PVAL( PNT2S( 1 ) ) ),
     :                    NBAD2, MAX2, MIN2, MAXPOS, MINPOS, STATUS )
         BAD = NBAD2 .NE. 0

*  If we are looping and the current scetion is all bad, annull the error
*  and pass on to the next row/column leaving bad values in the output
*  NDF.
         IF( AXIS .GT. 0 .AND. STATUS .EQ. SAI__ERROR ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL MSG_OUT( ' ', '   No good data.', STATUS )
            IF( OUTRQD ) CALL KPG1_MAP( NDFOUT, 'DATA', '_REAL',
     :                                  'WRITE/BAD', PNTOD, NEL1B,
     :                                  STATUS )
            GO TO 998
         END IF

*  Get temporary workspace to hold the histogram, and map it.
         CALL PSX_CALLOC( HISIZE, '_INTEGER', PNTW0, STATUS )

*  If an error has occurred, abort.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'NORMALIZE_WSP1',
     :        'NORMALIZE: Unable to get workspace to form the '//
     :        'histogram.', STATUS )
            CALL PSX_FREE( PNTW0, STATUS )
            GOTO 999
         END IF

*  Generate the histogram of the section of IN2.
         CALL KPG1_GHSTR( BAD, NELS, %VAL( CNF_PVAL( PNT2S( 1 ) ) ),
     :                    %VAL( CNF_PVAL( PNT2S( 1 ) ) ), 0.0D0,
     :                    HISIZE, .FALSE.,
     :                    MAX2, MIN2, %VAL( CNF_PVAL( PNTW0 ) ),
     :                    STATUS )

*  Find the data values in IN2 which correspond to the required
*  percentage histogram points.
         CALL KPG1_HSTFR( HISIZE, %VAL( CNF_PVAL( PNTW0 ) ),
     :                    MAX2, MIN2, 2, PCRANG,
     :                    DRDEF, STATUS )

*  Unmap and release the temporary work space used to hold the
*  histogram.
         CALL PSX_FREE( PNTW0, STATUS )

*  Obtain a data range to limit the data to be fitted.  Only pixels for
*  which the IN2 data value is inside the given range are used. The
*  auto-scaled range calculated above is used as the dynamic default.
         CALL PAR_GDR1R( 'DATARANGE', 2, DRDEF, VAL__MINR, VAL__MAXR,
     :                   .TRUE., DRANGE, STATUS )

*  Get required scalar parameters.
         CALL PAR_GDR0I( 'NBIN', 50, 2, 100000, .TRUE., NBIN, STATUS )
         CALL PAR_GDR0I( 'NITER', 2, 0, 100, .TRUE., NITER, STATUS )
         CALL PAR_GDR0R( 'NSIGMA', 3.0, 0.1, 1.0E6, .TRUE., NSIGMA,
     :                   STATUS )
         IF( NBIN .GT. 0 ) THEN
            MINPIX = NELS / NBIN
         ELSE
            MINPIX = 1
         END IF
         CALL PAR_GDR0I( 'MINPIX', 2, 1, MINPIX, .TRUE., MINPIX,
     :                   STATUS )

*  Get temporary workspace for use in KPS1_NMPLT, and map it.
         CALL PSX_CALLOC( NBIN, '_INTEGER', PNTW1, STATUS )
         CALL PSX_CALLOC( NBIN, '_REAL', PNTW2, STATUS )
         CALL PSX_CALLOC( NBIN, '_REAL', PNTW3, STATUS )
         CALL PSX_CALLOC( NBIN, '_DOUBLE', PNTW4, STATUS )
         CALL PSX_CALLOC( NBIN, '_REAL', PNTW5, STATUS )

*  If an error has occurred, abort annulling any workspace already
*  obtained.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'NORMALIZE_WSP2',
     :        'NORMALIZE: Unable to get workspace to calculate the '/
     :        /'mapping.', STATUS )
            CALL PSX_FREE( PNTW1, STATUS )
            CALL PSX_FREE( PNTW2, STATUS )
            CALL PSX_FREE( PNTW3, STATUS )
            CALL PSX_FREE( PNTW4, STATUS )
            CALL PSX_FREE( PNTW5, STATUS )
            GOTO 999
         END IF

*  See if the offset should be fixed at zero.
         CALL PAR_GET0L( 'ZEROFF', ZEROFF, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Call KPS1_NMPLT to calculate the linear function which normalises
*  IN1 to IN2.
         CALL KPS1_NMPLT( %VAL( CNF_PVAL( PNT2S( 1 ) ) ),
     :                    %VAL( CNF_PVAL( PNT1S( 1 ) ) ), NELS,
     :                DRANGE( 1 ), DRANGE( 2 ), NBIN, NITER, NSIGMA,
     :                MINPIX, NDF2S, NDF1S, ZEROFF,
     :                %VAL( CNF_PVAL( PNTW1 ) ),
     :                %VAL( CNF_PVAL( PNTW2 ) ),
     :                %VAL( CNF_PVAL( PNTW3 ) ),
     :                %VAL( CNF_PVAL( PNTW4 ) ),
     :                %VAL( CNF_PVAL( PNTW5 ) ), SLOPE, OFFSET,
     :                STATUS )

*  If we are looping and the normalisation failed, annull the error
*  and pass on to the next row/column leaving bad values in the output
*  NDF.
         IF( AXIS .GT. 0 .AND. STATUS .EQ. SAI__ERROR ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL MSG_OUT( ' ', '   Unable to calculate normalization '//
     :                    'constants.', STATUS )
            IF( OUTRQD ) CALL KPG1_MAP( NDFOUT, 'DATA', '_REAL',
     :                                  'WRITE/BAD', PNTOD, NEL1B,
     :                                  STATUS )
            GO TO 998
         END IF

*  Unmap and release the temporary work space.
         CALL PSX_FREE( PNTW1, STATUS )
         CALL PSX_FREE( PNTW2, STATUS )
         CALL PSX_FREE( PNTW3, STATUS )
         CALL PSX_FREE( PNTW4, STATUS )
         CALL PSX_FREE( PNTW5, STATUS )

*  Unmap the NDF sections.
         CALL NDF_UNMAP( NDF1S, '*', STATUS )
         CALL NDF_UNMAP( NDF2S, '*', STATUS )

*  Write the values of the gradient and offset of the final fit to the
*  Parameters SLOPE and OFFSET.
         CALL PAR_PUT0R( 'SLOPE', SLOPE, STATUS )
         CALL PAR_PUT0R( 'OFFSET', OFFSET, STATUS )

*  If an output NDF is to be made containing a normalised copy of IN1,
*  check that calculated slope was not zero.
         IF ( OUTRQD ) THEN

            IF ( ABS( SLOPE ) .LT. VAL__SMLR ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP('NORMALIZE_ZERO',
     :           'NORMALIZE: Zero slope.  Cannot normalize output.',
     :           STATUS )
               GOTO 999
            END IF

*  Map data arrays from base IN1 and output NDFs.
            CALL KPG1_MAP( NDF1B, 'DATA', '_REAL', 'READ', PNT1BD,
     :                     NEL1B, STATUS )
            CALL KPG1_MAP( NDFOUT, 'DATA', '_REAL', 'WRITE', PNTOD,
     :                     NEL1B, STATUS )

            IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Produce the output data array by applying the inverse of the
*  calculated normalisation function to the IN1 data array.
            CALL KPG1_SCLOF( NEL1B, %VAL( CNF_PVAL( PNT1BD( 1 ) ) ),
     :                       DBLE( 1.0 / SLOPE ),
     :                       DBLE( -OFFSET / SLOPE ),
     :                       %VAL( CNF_PVAL( PNTOD( 1 ) ) ), BAD,
     :                       STATUS )

*  Set the bad pixel flag for the output data array.
             CALL NDF_SBAD( BAD, NDFOUT, 'Data', STATUS )

*  Unmap the data arrays so they become defined, and save resources
*  whilst the variance array is being normalised.
             CALL NDF_UNMAP( NDF1B, 'Data', STATUS )
             CALL NDF_UNMAP( NDFOUT, 'Data', STATUS )

*  If IN1 contains a variance component, copy it to the output and
*  multiply by "1/SLOPE" squared to take account of the scaling of the
*  data array.
            CALL NDF_STATE( NDF1B, 'VARIANCE', VAR1, STATUS)

            IF ( VAR1 ) THEN

               CALL KPG1_MAP( NDF1B, 'VARIANCE', '_REAL', 'READ',
     :                        PNT1BV, NEL1B, STATUS )
               CALL KPG1_MAP( NDFOUT, 'VARIANCE', '_REAL', 'WRITE',
     :                        PNTOV, NEL1B, STATUS )

               IF ( STATUS .NE. SAI__OK ) GOTO 999

               CALL KPG1_SCLOF( NEL1B, %VAL( CNF_PVAL( PNT1BV( 1 ) ) ),
     :                          DBLE( 1.0 / SLOPE**2 ), 0.0D0,
     :                          %VAL( CNF_PVAL( PNTOV( 1 ) ) ),
     :                          BAD, STATUS )

*  Set the bad pixel flag for the output variance array.
               CALL NDF_SBAD( BAD, NDFOUT, 'Variance', STATUS )

            END IF

*  Get a value for the output title and store it.
            CALL NDF_CINP( 'TITLE', NDFOUT, 'TITLE', STATUS )

         END IF

*  Annul any NDF sections etc created within this loop.
 998     CALL NDF_END( STATUS )

      END DO

*  Clear up.
 999  CONTINUE

*  If an output NDF was created, but its data array is still undefined,
*  then delete the output NDF. (N.B. including the ERR_MARK and ERR_RLSE
*  routines cause previously generated error reports to disappear for
*  some reason. PCTR has been informed).
C      CALL ERR_MARK
      ISTAT = SAI__OK

      CALL NDF_VALID( NDFO, OUTRQD, ISTAT )
      IF ( OUTRQD ) THEN
         CALL NDF_STATE( NDFO, 'Data', DEFIND, ISTAT )
         IF ( .NOT. DEFIND ) CALL NDF_DELET( NDFO, ISTAT )
      END IF

C      CALL ERR_ANNUL( ISTAT )
C      CALL ERR_RLSE

*  End the NDF context
      CALL NDF_END( STATUS )

      END
