      SUBROUTINE SCATTER( STATUS )
*+
*  Name:
*     SCATTER

*  Purpose:
*     Displays a scatter plot between data in two NDFs.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SCATTER( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application displays a two-dimensional plot in which the
*     horizontal axis corresponds to the data value in the NDF given
*     by Parameter IN1, and the vertical axis corresponds to the data
*     value in the NDF given by Parameter IN2. Optionally, the variance,
*     standard deviation or quality may be used instead of the data value for
*     either axis (see Parameters COMP1 and COMP2). A symbol is displayed
*     at an appropriate position in the plot for each pixel which has a
*     good value in both NDFs, and falls within the bounds specified by
*     Parameters XLEFT, XRIGHT, YBOT, and YTOP. The type of symbol may be
*     specified using Parameter MARKER.
*
*     The supplied arrays may be compressed prior to display (see Parameter
*     COMPRESS). This reduces the number of points in the scatter plot, and
*     also reduces the noise in the data.
*
*     The Pearson correlation coefficient of the displayed scatter plot is
*     also calculated and displayed, and written to output Parameter CORR.
*
*     A linear fit to the data in the scatter plot can be calculated and
*     displayed (see Parameter FIT).

*  Usage:
*     scatter in1 in2 comp1 comp2 device

*  ADAM Parameters:
*     AXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes are to be drawn around the
*        plot. The width of the margins left for the annotation may be
*        controlled using Parameter MARGIN. The appearance of the axes
*        (colours, founts, etc.) can be controlled using the Parameter
*        STYLE. The dynamic default is TRUE if CLEAR is TRUE, and FALSE
*        otherwise. []
*     CLEAR = _LOGICAL (Read)
*        If TRUE the current picture is cleared before the plot is
*        drawn. If CLEAR is FALSE not only is the existing plot retained,
*        but also an attempt is made to align the new picture with the
*        existing picture. Thus you can generate a composite plot within
*        a single set of axes, say using different colours or modes to
*        distinguish data from different datasets.  [TRUE]
*     COMP1 = LITERAL (Read)
*        The NDF array component to be displayed on the horizontal axis.
*        It may be "Data", "Quality", "Variance", or "Error" (where "Error"
*        is an alternative to "Variance" and causes the square root of the
*        variance values to be displayed).  If "Quality" is specified,
*        then the quality values are treated as numerical values (in
*        the range 0 to 255).  ["Data"]
*     COMP2 = LITERAL (Read)
*        The NDF array component to be displayed on the vertical axis.
*        It may be "Data", "Quality", "Variance", or "Error" (where "Error"
*        is an alternative to "Variance" and causes the square root of the
*        variance values to be displayed).  If "Quality" is specified,
*        then the quality values are treated as numerical values (in
*        the range 0 to 255).  ["Data"]
*     COMPRESS() = _INTEGER (Read)
*        The compression factors to be used when compressing the supplied
*        arrays prior to display. If any of the supplied values are greater
*        than 1, then the supplied arrays are compressed prior to display
*        by replacing each box of input pixels by a single pixel equal to
*        the mean of the pixels in the box. The size of each box in pixels
*        is given by the compression factors. No compression occurs if all
*        values supplied for this parameter are 1. If the number of values
*        supplied is smaller than the number of axes, the final value
*        supplied is duplicated for the remaining axes.  [1]
*     CORR = _DOUBLE (Write)
*        The Pearson correlation coefficient of the visible points in the
*        scatter plot (points outside the plot are ignored). A value of
*        zero is stored if the correlation coefficient cannot be calculated.
*     DEVICE = DEVICE (Read)
*        The graphics workstation on which to produce the plot.  If a
*        null value (!) is supplied no plot will be made. [Current graphics
*        device]
*     FIT = _LOGICAL (Read)
*        If TRUE, then a linear fit to the scatter points is added to the
*        plot. The slope and offset of this fit is displayed on the screen
*        and written to output Parameters SLOPE, OFFSET, and RMS. A
*        symmetric linear-fit algorithm is used, which caters for the
*        presence of noise in both X and Y values. Outliers are identified
*        and ignored. Note, the fit is based on just those points that are
*        visible in the scatter plot. Points outside the bounds of the
*        plot are ignored. Points that are inside the plot are also ignored
*        if their reflection through the best-fit line are outside the plot.
*        This avoids biasing the fit if the plot bounds omit more points on
*        one side of the line than the other. [current value]
*     IN1 = NDF (Read)
*        The NDF to be displayed on the horizontal axis.
*     IN2 = NDF (Read)
*        The NDF to be displayed on the vertical axis.
*     MARGIN( 4 ) = _REAL (Read)
*        The widths of the margins to leave for axis annotation, given
*        as fractions of the corresponding dimension of the current picture.
*        Four values may be given, in the order bottom, right, top, left.
*        If less than four values are given, extra values are used equal to
*        the first supplied value. If these margins are too narrow any axis
*        annotation may be clipped. If a null (!) value is supplied, the
*        used value is 0.15 (for all edges) if annotated axes are produced,
*        and zero otherwise.  [current value]
*     MARKER = _INTEGER (Read)
*        Specifies the symbol with which each position should be marked in
*        the plot. It should be given as an integer PGPLOT marker type. For
*        instance, 0 gives a box, 1 gives a dot, 2 gives a cross, 3 gives
*        an asterisk, 7 gives a triangle. The value must be larger than or
*        equal to -31.  [current value]
*     NPIX = _INTEGER (Write)
*        The number of points used to form the correlation coefficient.
*     OFFSET = _DOUBLE (Write)
*        An output parameter giving the offset in the linear fit:
*        IN2 = SLOPE * IN1 + OFFSET. Only used if Parameter FIT is TRUE.
*     PERC1( 2 ) = _REAL (Read)
*        The percentiles that define the default values for XLEFT and
*        XRIGHT. For example, [5,95] would result in the lowest and
*        highest 5% of the data value in IN1 being excluded from the plot
*        if the default values are accepted for XLEFT and XRIGHT.
*        [current value]
*     PERC2( 2 ) = _REAL (Read)
*        The percentiles that define the default values for YBOT and
*        YTOP. For example, [5,95] would result in the lowest and
*        highest 5% of the data value in IN2 being excluded from the plot
*        if the default values are accepted for YBOT and YTOP.
*        [current value]
*     RMS = _DOUBLE (Write)
*        An output parameter giving the RMS residual of the data (excluding
*        outliers) about the linear fit. Only used if Parameter FIT is TRUE.
*     SLOPE = _DOUBLE (Write)
*        An output parameter giving the slope of the linear fit:
*        IN2 = SLOPE * IN1 + OFFSET. Only used if Parameter FIT is TRUE.
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to use
*        when drawing the annotated axes, and markers.
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
*        The appearance of markers is controlled by Colour(Markers),
*        Width(Markers), etc. (the synonym Symbols may be used in place
*        of Markers).  [current value]
*     XLEFT = _DOUBLE (Read)
*        The axis value to place at the left hand end of the horizontal
*        axis. If a null (!) value is suplied, the value used is determined
*        by Parameter PERC1. The value supplied may be greater than or less
*        than the value supplied for XRIGHT.  [!]
*     XRIGHT = _DOUBLE (Read)
*        The axis value to place at the right hand end of the horizontal
*        axis. If a null (!) value is suplied, the value used is determined
*        by Parameter PERC1. The value supplied may be greater than or less
*        than the value supplied for XLEFT.  [!]
*     YBOT = _DOUBLE (Read)
*        The axis value to place at the bottom end of the vertical axis.
*        If a null (!) value is suplied, the value used is determined
*        by Parameter PERC2. The value supplied may be greater than or less
*        than the value supplied for YTOP.  [!]
*     YTOP = _DOUBLE (Read)
*        The axis value to place at the top end of the vertical axis.
*        If a null (!) value is suplied, the value used is determined
*        by Parameter PERC2. The value supplied may be greater than or less
*        than the value supplied for YBOT.  [!]

*  Examples:
*     scatter cl123a cl123b
*        This displays a scatter plot of the data value in NDF cl123b
*        against the data value in NDF cl123a, on the current graphics
*        device.
*     scatter cl123a cl123a pscol_l comp2=error compress=3
*        This displays a scatter plot of the error in NDF cl123a
*        against the data value in the same NDF. The graphics device used
*        is "pscol_l". The data is compressed by a factor of 3 on each
*        axis before forming the plot.

*  Notes:
*     -  Any pixels that are bad (after any compression) in either array
*     are excluded from the plot, and from the calculation of the
*     default axis limits
*     -  The application stores two pictures in the graphics database in
*     the following order: a FRAME picture containing the annotated axes
*     and data plot, and a DATA picture containing just the data plot.
*     Note, the FRAME picture is only created if annotated axes have been
*     drawn, or if non-zero margins were specified using Parameter
*     MARGIN. The world co-ordinates in the DATA picture will correspond
*     to data value in the two NDFs.

*  Related Applications:
*     KAPPA:NORMALIZE.

*  Implementation Status:
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  Only _REAL data can be processed directly.  Other non-complex
*     numeric data types will undergo a type conversion before
*     processing occurs.

*  Copyright:
*     Copyright (C) 1999, 2001, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2010-2011 Science and Technology Facilities Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JUN-1999 (DSB):
*        Original version.
*     26-OCT-1999 (DSB):
*        Made MARGIN a fraction of the current picture, not the DATA
*        picture.
*     19-SEP-2001 (DSB):
*        Ensure that none of the compression factors are bigger than the
*        number of pixels on the corresponding axis.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 February 24 (MJC):
*        Added new CUMUL argument set to .FALSE. to KPG1_GHSTx calls.
*     15-OCT-2009 (DSB):
*        Ignore points that are bad in either input NDF.
*     2010 October 14 (MJC):
*        Document temporary style attributes.
*     16-JUN-2011 (DSB):
*        Added BSCALE argument to KPG1_GRAPH. BSCALE is currently unused
*        by SCATTER. At some point SCATTER should be changed to handle
*        data that exceeds the range of a REAL (as has been done to
*        HISTOGRAM).
*     2011-08-22 (TIMJ):
*        Add new WGTS and WEIGHT arguments to KPG1_GHSTx calls.
*     15-SEP-2011 (DSB):
*        Added calculation of correlation coefficient.
*     5-SEP-2013 (DSB):
*        Added output Parameter NPIX.
*     18-MAR-2020 (DSB):
*        Added Parameters FIT, SLOPE, OFFSET and RMS.
*     19-MAR-2020 (DSB):
*        Change the FIT and CORR parameters to ignore points outside
*        the bounds of the plot.
*     20-MAR-2020 (DSB):
*        Changed KPG1_GRAPH API.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NUMBIN             ! Size of percetiles histogram
      PARAMETER ( NUMBIN = 1000 )

*  Local Variables:
      CHARACTER COMP1*10         ! NDF component from IN1 to be unmapped
      CHARACTER COMP2*10         ! NDF component from IN2 to be unmapped
      CHARACTER LAB1*80          ! Default label for horizontal axis
      CHARACTER LAB2*80          ! Default label for vertical axis
      CHARACTER MCOMP1*10        ! NDF component from IN1 to be mapped
      CHARACTER MCOMP2*10        ! NDF component from IN2 to be mapped
      CHARACTER NDFNAM*80        ! NDF name (without directory path)
      CHARACTER UNITS1*30        ! Units string from NDF IN1
      CHARACTER UNITS2*30        ! Units string from NDF IN2
      DOUBLE PRECISION BSCALE( 2 )! Scaling for plot labels
      DOUBLE PRECISION R         ! Correlation coefficient
      DOUBLE PRECISION SLOPE     ! Slope of linear fit
      DOUBLE PRECISION OFFSET    ! Offset of linear fit
      DOUBLE PRECISION RMS       ! RMS residual of linear fit
      INTEGER CMPRS( NDF__MXDIM )! Compression factors for each axis
      INTEGER CDIM( NDF__MXDIM ) ! Dimensions of compressed array
      INTEGER CEL                ! No of elements in compressed array
      INTEGER DIM( NDF__MXDIM )  ! Dimensions of NDF section
      INTEGER HIST( NUMBIN )     ! Array containing histogram
      INTEGER I                  ! Loop count
      INTEGER IERR               ! Index of first numerical error
      INTEGER INDF1              ! NDF identifier for input IN1
      INTEGER INDF2              ! NDF identifier for input IN2
      INTEGER IP1                ! Pointer to array for horizontal axis
      INTEGER IP2                ! Pointer to array for vertical axis
      INTEGER IPLOT              ! The AST Plot used to do the plotting
      INTEGER IPQ                ! Pointer to a _UBYTE quality array
      INTEGER IPW1               ! Pointer to _REAL quality for IN1
      INTEGER IPW2               ! Pointer to _REAL quality for IN2
      INTEGER IPW3               ! Pointer to compressed array for IN1
      INTEGER IPW4               ! Pointer to compressed array for IN2
      INTEGER IPW5               ! Pointer to work array
      INTEGER IPW6               ! Pointer to work array
      INTEGER LEN1               ! Used length of LAB1
      INTEGER LEN2               ! Used length of LAB2
      INTEGER MAXPOS             ! Position of maximum value
      INTEGER MINPOS             ! Position of minimum value
      INTEGER NCU1               ! Used length of UNITS1
      INTEGER NCU2               ! Used length of UNITS2
      INTEGER NDIM               ! No. of dimensions in array
      INTEGER NEL                ! No. of elements mapped from the NDFs
      INTEGER*8 NEL8             ! No. of elements mapped from the NDFs
      INTEGER NERR               ! Number of numerical errors
      INTEGER NINVAL             ! Number of invalid values
      INTEGER NMLEN              ! Used length of NDFNAM
      INTEGER NPIX               ! Number of values used in corr. coeff.
      INTEGER NVAL               ! Number of supplied values
      LOGICAL FIT                ! Calculate and display a linear fit?
      LOGICAL BLAV               ! Do block averaging?
      REAL PERC1( 2 )            ! Percentiles defining default IN1 data range
      REAL PERC2( 2 )            ! Percentiles defining default IN1 data range
      REAL PERV1( 2 )            ! Data values corresponding to PERC1
      REAL PERV2( 2 )            ! Data values corresponding to PERC2
      REAL RMAXV                 ! Minimum value in the array
      REAL RMINV                 ! Maximum value in the array
      REAL RVAL                  ! Temporary real storage
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise pointers to dynamic work arrays indicate that none of them
*  are being used.
      IPW1 = 0
      IPW2 = 0
      IPW3 = 0
      IPW4 = 0
      IPW5 = 0
      IPW6 = 0

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Get NDF identifiers for the two input NDFs.
      CALL LPG_ASSOC( 'IN1', 'READ', INDF1, STATUS )
      CALL LPG_ASSOC( 'IN2', 'READ', INDF2, STATUS )

*  Find which components to plot.
      CALL KPG1_ARCOG( 'COMP1', INDF1, MCOMP1, COMP1, STATUS )
      CALL KPG1_ARCOG( 'COMP2', INDF2, MCOMP2, COMP2, STATUS )

*  Obtain the units if present.
      CALL KPG1_DAUNI( INDF1, MCOMP1, UNITS1, NCU1, STATUS )
      CALL KPG1_DAUNI( INDF2, MCOMP2, UNITS2, NCU2, STATUS )

*  Find the percentiles defining the default axis limits, and convert to
*  fractions.
      CALL PAR_GDR1R( 'PERC1', 2, -1.0, 0.0, 100.0, .FALSE., PERC1,
     :                STATUS )
      CALL PAR_GDR1R( 'PERC2', 2, -1.0, 0.0, 100.0, .FALSE., PERC2,
     :                STATUS )
      PERC1( 1 ) = PERC1( 1 ) * 0.01
      PERC1( 2 ) = PERC1( 2 ) * 0.01
      PERC2( 1 ) = PERC2( 1 ) * 0.01
      PERC2( 2 ) = PERC2( 2 ) * 0.01

*  Create sections of the two input NDFs with matched pixel bounds
*  by trimming the input NDFs.
      CALL NDF_MBND( 'TRIM', INDF1, INDF2, STATUS )

*  Get the dimensions of the section being display.
      CALL NDF_DIM( INDF1, NDF__MXDIM, DIM, NDIM, STATUS )

*  1D data must be handled as 2D data with a second dimension of 1.
      IF( NDIM .EQ. 1 ) THEN
         NDIM = 2
         DIM( 2 ) = 1
      END IF

*  Map the required arrays of the two NDFs. First deal with DATA,
*  VARIANCE and ERROR (i.e. everything except QUALITY).
      IF( MCOMP1( 1 : 1 ) .NE. 'Q' ) THEN

*  Just map these components as _REAL.
         CALL NDF_MAP( INDF1, MCOMP1, '_REAL', 'READ', IP1, NEL,
     :                 STATUS )

*  Quality arrays have to be handled a bit differently because they can
*  only be accessed as _UBYTE arrays. Therefore, we need to map them as
*  _UBYTE, and then convert them explicitly to _REAL, allocating storage
*  for the result.
      ELSE
         CALL NDF_MAP( INDF1, 'QUALITY', '_UBYTE', 'READ', IPQ, NEL,
     :                 STATUS )
         CALL PSX_CALLOC( NEL, '_REAL', IPW1, STATUS )
         CALL VEC_UBTOR( .FALSE., NEL, %VAL( CNF_PVAL( IPQ ) ),
     :                   %VAL( CNF_PVAL( IPW1 ) ), IERR,
     :                   NERR, STATUS )
         CALL NDF_UNMAP( INDF1, '*', STATUS )
         IP1 = IPW1
      END IF

*  Map the required array from the second NDF in the same way.
      IF( MCOMP2( 1 : 1 ) .NE. 'Q' ) THEN
         CALL NDF_MAP( INDF2, MCOMP2, '_REAL', 'READ', IP2, NEL,
     :                 STATUS )
      ELSE
         CALL NDF_MAP( INDF2, 'QUALITY', '_UBYTE', 'READ', IPQ, NEL,
     :                 STATUS )
         CALL PSX_CALLOC( NEL, '_REAL', IPW2, STATUS )
         CALL VEC_UBTOR( .FALSE., NEL, %VAL( CNF_PVAL( IPQ ) ),
     :                   %VAL( CNF_PVAL( IPW2 ) ), IERR,
     :                   NERR, STATUS )
         CALL NDF_UNMAP( INDF2, '*', STATUS )
         IP2 = IPW2
      END IF

*  Get the compression factors to use when block averaging the supplied data,
*  duplicating the last value if insufficient values are given.
      CALL PAR_GDRVI( 'COMPRESS', NDIM, 1, VAL__MAXI, CMPRS, NVAL,
     :                STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999
      DO I = NVAL + 1, NDIM
         CMPRS( I ) = CMPRS( NVAL )
      END DO

*  Ensure that none of the compression factors are bigger than the number
*  of pixels on the corresponding axis.
      DO I = 1, NDIM
         CMPRS( I ) = MIN( CMPRS( I ), DIM( I ) )
      END DO

*  See if block averaging is required.
      BLAV = .FALSE.
      DO I = 1, NDIM
         IF( CMPRS( I ) .GT. 1 ) BLAV = .TRUE.
      END DO

*  If an error has occured, abort.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  If required, block average the data to reduce the number of points to
*  display.
      IF( BLAV ) THEN

*  Find the size of the output arrays.
         CEL = 1
         DO I = 1, NDIM
            CDIM( I ) = MAX( 1, DIM( I ) / CMPRS( I ) )
            CEL = CEL*CDIM( I )
         END DO

*  Create workspace: four are needed --- two for the compressed
*  arrays, and the other two are needed to perform the averaging
*  calculations.
         CALL PSX_CALLOC( CEL, '_REAL', IPW3, STATUS )
         CALL PSX_CALLOC( CEL, '_REAL', IPW4, STATUS )
         CALL PSX_CALLOC( DIM( 1 ), '_REAL', IPW5, STATUS )
         CALL PSX_CALLOC( DIM( 1 ), '_INTEGER', IPW6, STATUS )

*  Block average the first array.
         CALL KPG1_CMAVR( NDIM, DIM, %VAL( CNF_PVAL( IP1 ) ), CMPRS, 1,
     :                    %VAL( CNF_PVAL( IPW3 ) ),
     :                    %VAL( CNF_PVAL( IPW5 ) ),
     :                    %VAL( CNF_PVAL( IPW6 ) ),
     :                    STATUS )
         IP1 = IPW3

*  Block average the second array.
         CALL KPG1_CMAVR( NDIM, DIM, %VAL( CNF_PVAL( IP2 ) ), CMPRS, 1,
     :                    %VAL( CNF_PVAL( IPW4 ) ),
     :                    %VAL( CNF_PVAL( IPW5 ) ),
     :                    %VAL( CNF_PVAL( IPW6 ) ),
     :                    STATUS )
         IP2 = IPW4

*  Update the number of elements being plotted.
         NEL = CEL

*  If we are not block averaging, copy the input data arrays into two
*  work arrays without change.
      ELSE
         CALL PSX_CALLOC( NEL, '_REAL', IPW3, STATUS )
         CALL PSX_CALLOC( NEL, '_REAL', IPW4, STATUS )
         CALL VEC_RTOR( .FALSE., NEL, %VAL( CNF_PVAL( IP1 ) ),
     :                   %VAL( CNF_PVAL( IPW3 ) ), IERR,
     :                   NERR, STATUS )
         CALL VEC_RTOR( .FALSE., NEL, %VAL( CNF_PVAL( IP2 ) ),
     :                   %VAL( CNF_PVAL( IPW4 ) ), IERR,
     :                   NERR, STATUS )
      END IF

*  Ensure the two work arrays have the same bad pixel mask.
      CALL KPS1_SCAT1( NEL, %VAL( CNF_PVAL( IPW3 ) ),
     :                 %VAL( CNF_PVAL( IPW4 ) ), STATUS )

*  Obtain the maximum and minimum values to define the bounds of the
*  first histogram.
      CALL KPG1_MXMNR( .TRUE., NEL, %VAL( CNF_PVAL( IPW3 ) ),
     :                 NINVAL, RMAXV, RMINV,
     :                 MAXPOS, MINPOS, STATUS )

*  Generate the histogram between those bounds.
      CALL KPG1_GHSTR( .TRUE., NEL, %VAL( CNF_PVAL( IPW3 ) ),
     :                 %VAL( CNF_PVAL( IPW3 ) ), 0.0D0,
     :                 NUMBIN, .FALSE., RMAXV, RMINV,
     :                 HIST, STATUS )

*  Estimate the values at the percentiles.
      CALL KPG1_HSTFR( NUMBIN, HIST, RMAXV, RMINV, 2, PERC1, PERV1,
     :                 STATUS )

*  Ensure they are the right way round.
      IF( PERV1( 1 ) .GT. PERV1( 2 ) ) THEN
         RVAL = PERV1( 1 )
         PERV1( 1 ) = PERV1( 2 )
         PERV1( 2 ) = RVAL
      END IF

*  Do the same for the second NDF.
      CALL KPG1_MXMNR( .TRUE., NEL, %VAL( CNF_PVAL( IPW4 ) ),
     :                 NINVAL, RMAXV, RMINV,
     :                 MAXPOS, MINPOS, STATUS )
      CALL KPG1_GHSTR( .TRUE., NEL, %VAL( CNF_PVAL( IPW4 ) ),
     :                 %VAL( CNF_PVAL( IPW3 ) ), 0.0D0,
     :                 NUMBIN, .FALSE., RMAXV, RMINV,
     :                 HIST, STATUS )
      CALL KPG1_HSTFR( NUMBIN, HIST, RMAXV, RMINV, 2, PERC2, PERV2,
     :                 STATUS )

      IF( PERV2( 1 ) .GT. PERV2( 2 ) ) THEN
         RVAL = PERV2( 1 )
         PERV2( 1 ) = PERV2( 2 )
         PERV2( 2 ) = RVAL
      END IF

*  Calculate the correlation coefficient.
      CALL KPG1_CORRR( NEL, %VAL( CNF_PVAL( IPW3 ) ),
     :                 %VAL( CNF_PVAL( IPW4 ) ), PERV1( 1 ),
     :                 PERV1( 2 ), PERV2( 1 ), PERV2( 2 ), R, NPIX,
     :                 STATUS )
      IF( R .NE. VAL__BADD ) THEN
         CALL MSG_SETD( 'R', R )
         CALL MSG_OUT( ' ', '  Correlation coefficient: ^R', STATUS )
         CALL PAR_PUT0D( 'CORR', R, STATUS )
         CALL PAR_PUT0I( 'NPIX', NPIX, STATUS )
      ELSE
         CALL MSG_OUT( ' ', '  Correlation coefficient: <bad>',
     :                 STATUS )
         CALL PAR_PUT0D( 'CORR', 0.0D0, STATUS )
         CALL PAR_PUT0I( 'NPIX', 0, STATUS )
      ENDIF

*  Construct the default label for the X and Y axes. These include
*  the NDF component name and file name, and the units (if not blank).
      CALL KPG1_NDFNM( INDF1, NDFNAM, NMLEN, STATUS )
      CALL MSG_SETC( 'NDF', NDFNAM )
      CALL MSG_SETC( 'COMP', MCOMP1 )
      CALL MSG_LOAD( ' ', '^COMP value in ^NDF', LAB1, LEN1, STATUS )

      IF( NCU1 .GT. 0 .AND. UNITS1( : NCU1 ) .NE. ' ' ) THEN
         CALL CHR_APPND( ' (', LAB1, LEN1 )
         CALL CHR_APPND( UNITS1( : NCU1), LAB1, LEN1 )
         CALL CHR_APPND( ')', LAB1, LEN1 )
      END IF

      CALL KPG1_NDFNM( INDF2, NDFNAM, NMLEN, STATUS )
      CALL MSG_SETC( 'NDF', NDFNAM )
      CALL MSG_SETC( 'COMP', MCOMP2 )
      CALL MSG_LOAD( ' ', '^COMP value in ^NDF', LAB2, LEN2, STATUS )

      IF( NCU2 .GT. 0 .AND. UNITS2( : NCU2 ) .NE. ' ' ) THEN
         CALL CHR_APPND( ' (', LAB2, LEN2 )
         CALL CHR_APPND( UNITS2( : NCU2), LAB2, LEN2 )
         CALL CHR_APPND( ')', LAB2, LEN2 )
      END IF

*  Produce the scatter plot.
      BSCALE( 1 ) = 1.0D0
      BSCALE( 2 ) = 1.0D0
      IPLOT = AST__NULL
      CALL KPG1_GRAPH( NEL, %VAL( CNF_PVAL( IPW3 ) ),
     :                 %VAL( CNF_PVAL( IPW4 ) ), 0.0, 0.0,
     :                 LAB1( : LEN1 ), LAB2( : LEN2 ), 'Scatter plot',
     :                 'XDATA', 'YDATA', 3, .TRUE., 'KAPPA_SCATTER',
     :                 .FALSE., .FALSE., BSCALE, IPLOT, PERV1( 1 ),
     :                 PERV1( 2 ), PERV2( 1 ), PERV2( 2 ),STATUS )

*  If required, add  a linear fit to the plot.
      CALL PAR_GET0L( 'FIT', FIT, STATUS )
      IF( FIT ) THEN
         NEL8 = NEL
         CALL KPG1_DRFIT(  NEL8, %VAL( CNF_PVAL( IPW3 ) ),
     :                     %VAL( CNF_PVAL( IPW4 ) ), PERV1( 1 ),
     :                     PERV1( 2 ), PERV2( 1 ), PERV2( 2 ), IPLOT,
     :                     SLOPE, OFFSET, RMS, STATUS )

         IF( SLOPE .EQ. VAL__BADD .OR. OFFSET .EQ. VAL__BADD ) THEN
            CALL MSG_OUT( ' ', '  Line of best fit cannot be drawn '//
     :                    'since all points have been rejected.',
     :                    STATUS )
         ELSE
            CALL MSG_BLANK( STATUS )
            CALL MSG_SETR( 'M', REAL(SLOPE) )
            CALL MSG_SETR( 'B', REAL(OFFSET) )
            CALL MSG_OUT( ' ', '  Best fit line is:  Y = ( ^M )*X + '//
     :                    '( ^B )', STATUS )
            CALL MSG_SETR( 'R', REAL(RMS) )
            CALL MSG_OUT( ' ', '  RMS residual from line is: ^R',
     :                    STATUS )
            CALL MSG_BLANK( STATUS )

            CALL PAR_PUT0D( 'SLOPE', SLOPE, STATUS )
            CALL PAR_PUT0D( 'OFFSET', OFFSET, STATUS )
            CALL PAR_PUT0D( 'RMS', RMS, STATUS )
         END IF
      END IF

*  Close the workstation.
      IF( IPLOT .NE. AST__NULL ) CALL KPG1_PGCLS( 'DEVICE', .FALSE.,
     :                                            STATUS )

*  Tidy up from here.
 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Free any dynamic arrays which have been used.
      IF( IPW1 .NE. 0 ) CALL PSX_FREE( IPW1, STATUS )
      IF( IPW2 .NE. 0 ) CALL PSX_FREE( IPW2, STATUS )
      IF( IPW3 .NE. 0 ) CALL PSX_FREE( IPW3, STATUS )
      IF( IPW4 .NE. 0 ) CALL PSX_FREE( IPW4, STATUS )
      IF( IPW5 .NE. 0 ) CALL PSX_FREE( IPW5, STATUS )
      IF( IPW6 .NE. 0 ) CALL PSX_FREE( IPW6, STATUS )

*  Give a contextual error message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SCATTER_ERR', 'SCATTER: Failed to display '//
     :                 'a scatter plot.', STATUS )
      END IF

      END
