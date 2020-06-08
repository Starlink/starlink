      SUBROUTINE MSTATS( STATUS )
*+
*  Name:
*     MSTATS

*  Purpose:
*     Calculate statistics over a group of data arrays or points.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MSTATS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)

*  Description:
*     This application calculates cumulative statistics over a group
*     of NDFs.  It can either generate the statistics of each
*     corresponding pixel in the input array components and output
*     a new NDF with array components containing the result, or
*     calculate statistics at a single point specified in the
*     current co-ordinate Frame of the input NDFs.
*
*     In array mode (SINGLE=FALSE), statistics are calculated for each
*     pixel in one of the array components (DATA, VARIANCE or QUALITY)
*     accumulated over all the input NDFs and written to an output
*     NDF; each pixel of the output NDF is a result of combination
*     of pixels with the same Pixel co-ordinates in all the input NDFs.
*     There is a selection of statistics available to form the output
*     values.
*
*     The input NDFs must all have the same number of dimensions, but
*     need not all be the same shape.  The shape of the output NDF can
*     be set to either the intersection or the union of the shapes of
*     the input NDFs using the TRIM parameter.
*
*     In single pixel mode (SINGLE=TRUE) a position in the current
*     co-ordinate Frame of all the NDFs is given, and the value at
*     the pixel covering this point in each of the input NDFs is
*     accumulated to form the results that comprise the mean, variance,
*     and median.  These statistics, and if environment variable
*     MSG_FILTER is set to VERBOSE, the value of each contributing
*     pixel, is reported directly to you.

*  Usage:
*     mstats in out [estimator]

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*        The NDF array component to be analysed.  It may be "Data",
*        "Quality", "Variance", or "Error" (where "Error" is an
*        alternative to "Variance" and causes the square root of the
*        variance values to be used).  If "Quality" is specified,
*        then the quality values are treated as numerical values (in
*        the range 0 to 255).  In cases other than "Data", which is
*        always present, a missing component will be treated as having
*        all pixels set to the `bad' value.  ["Data"]
*     CLIP = _REAL (Read)
*        The number of standard deviations about the mean at which to
*        clip outliers for the "Mode", "Cmean" and "Csigma" statistics
*        (see Parameter ESTIMATOR).  The application first computes
*        statistics using all the available pixels.  It then rejects
*        all those pixels whose values lie beyond CLIP standard
*        deviations from the mean and will then re-evaluate the
*        statistics.   For "Cmean" and "Csigma" there is currently only
*        one iteration , but up to seven for "Mode".

*        The value must be positive.  [3.0]
*     ESTIMATOR = LITERAL (Read)
*        The method to use for estimating the output pixel values from
*        the multiple input pixels at each pixel index.  It
*        can be one of the following options.
*          "Mean"   -- Mean value
*          "WMean"  -- Weighted mean in which each data value is
*                      weighted by the reciprocal of the associated
*                      variance.  (2)
*          "Mode"   -- Modal value  (4)
*          "Median" -- Median value.  Note that this is extremely memory
*                      and CPU intensive for large datasets; use with
*                      care!  If strange things happen, use "Mean".  (3)
*          "Absdev" -- Mean absolute deviation from the unweighted mean.
*                      (2)
*          "Cmean"  -- Sigma-clipped mean.  (4)
*          "Csigma" -- Sigma-clipped standard deviation.  (4)
*          "Comax"  -- Co-ordinate of the maximum value.
*          "Comin"  -- Co-ordinate of the minimum value.
*          "FBad"   -- Fraction of bad pixel values.
*          "FGood"  -- Fraction of good pixel values.
*          "Integ"  -- Integrated value, being the sum of the products
*                      of the value and pixel width in world
*                      co-ordinates.  Note that for sky co-ordinates the
*                      pixel width is measured in radians.
*          "Iwc"    -- Intensity-weighted co-ordinate, being the sum of
*                      each value times its co-ordinate, all divided by
*                      the integrated value (see the "Integ" option).
*          "Iwd"    -- Intensity-weighted dispersion of the
*                      co-ordinate, normalised like "Iwc" by the
*                      integrated value.  (4)
*          "Max"    -- Maximum value.
*          "Min"    -- Minimum value.
*          "NBad"   -- Number of bad pixel values.
*          "NGood"  -- Number of good pixel values.
*          "Rms"    -- Root-mean-square value. (4)
*          "Sigma"  -- Standard deviation about the unweighted mean. (4)
*          "Sum"    -- The total value.
*
*        Where needed, the co-ordinates are the indices of the input
*        NDFs in the supplied order.  Thus the calculations behave like
*        the NDFs were stacked one upon another to form an extra axis,
*        and that axis had GRID co-ordinates.  Care using wildcards is
*        necessary, to achieve a specific order, say for a time series,
*        and hence assign the desired co-ordinate for a each NDF.
*        Indirection through a text file is recommended.
*
*        The selection is restricted if there are only a few input NDFs.
*        For instance, measures of dispersion like "Sigma" and "Iwd" are
*        meaningless for combining only two NDFs.  The minimum number of
*        input NDFs for each estimator is given in parentheses in the
*        list above.  Where there is no number, there is no restriction.
*        If you supply an unavailable option, you will be informed, and
*        presented with the available options.   ["Mean"]
*     IN = GROUP (Read)
*        A group of input NDFs.  They may have different shapes, but
*        must all have the same number of dimensions.  This should
*        be given as a comma separated list, in which each list element
*        can be one of the following.
*
*        - An NDF name, optionally containing wild-cards and/or regular
*        expressions ("*", "?", "[a-z]" etc.).
*
*        - The name of a text file, preceded by an up-arrow character
*        "^".  Each line in the text file should contain a
*        comma-separated list of elements, each of which can in turn be
*        an NDF name (with optional wild-cards, etc.), or another file
*        specification (preceded by an up-arrow).  Comments can be
*        included in the file by commencing lines with a hash character
*        "#".
*
*        If the value supplied for this parameter ends with a minus
*        sign "-", then the user is re-prompted for further input until
*        a value is given which does not end with a minus sign.  All the
*        images given in this way are concatenated into a single group.
*     MEAN = _DOUBLE (Write)
*        An output parameter to which is written the mean pixel value,
*        if SINGLE=TRUE.
*     MEDIAN = _DOUBLE (Write)
*        An output parameter to which is written the median pixel value,
*        if SINGLE=TRUE.
*     OUT = NDF (Read)
*        The name of an NDF to receive the results.  Each pixel of
*        the DATA (and perhaps VARIANCE) component represents the
*        statistics of the corresponding pixels of the input NDFs.
*        Only used if SINGLE=FALSE.
*     POS = LITERAL (Read)
*        In Single pixel mode (SINGLE=TRUE), this parameter gives the
*        position in the current co-ordinate Frame at which the
*        statistics should be calculated (supplying a colon ":" will
*        display details of the required co-ordinate Frame).  The
*        position should be supplied as a list of formatted axis values
*        separated by spaces or commas.  The pixel covering this point
*        in each input array, if any, will be used.
*     SINGLE = _LOGICAL (Read)
*        Whether the statistics should be calculated in Single pixel
*        mode or Array mode.  If SINGLE=TRUE, then the POS parameter
*        will be used to get the point to which the statistics refer,
*        but if SINGLE=FALSE an output NDF will be generated containing
*        the results for all the pixels.  [FALSE]
*     TITLE = LITERAL (Read)
*        Title for the output NDF.  ["KAPPA - Mstats"]
*     TRIM = _LOGICAL (Read)
*        This parameter controls the shape of the output NDF.  If
*        TRIM=TRUE, then the output NDF is the shape of the intersection
*        of all the input NDFs, i.e. only pixels which appear in all the
*        input arrays will be represented in the output.  If TRIM=FALSE,
*        the output is the shape of the union of the inputs, i.e. every
*        pixel which appears in the input arrays will be represented in
*        the output.  [TRUE]
*     VAR = _DOUBLE (Write)
*        An output parameter to which is written the variance of the
*        pixel values, if SINGLE=TRUE.
*     VARIANCE = _LOGICAL (Read)
*        A flag indicating whether a variance array present in the
*        NDF is used to weight the array values while forming the
*        estimator's statistic, and to derive output variance.  If
*        VARIANCE is TRUE and all the input NDFs contain a variance
*        array, this array will be used to define the weights, otherwise
*        all the weights will be set equal.  [TRUE]
*     WLIM = _REAL (Read)
*        If the input NDFs contain bad pixels, then this parameter
*        may be used to determine at a given pixel location the number
*        of good pixels which must be present within the input NDFs
*        before a valid output pixel is generated.  It can be used, for
*        example, to prevent output pixels from being generated in
*        regions where there are relatively few good pixels to
*        contribute to the result of combining the input NDFs.
*
*        WLIM specifies the minimum fraction of good pixels which must
*        be present in order to generate a good output pixel.  If this
*        specified minimum fraction of good input pixels is not present,
*        then a bad output pixel will result, otherwise a good output
*        value will be calculated.  The value of this parameter should
*        lie between 0.0 and 1.0 (the actual number used will be rounded
*        up if necessary to correspond to at least one pixel). [0.3]

*  Examples:
*     mstats idat* ostats
*        This calculates the mean of each pixel in the Data arrays of
*        all the NDFs in the current directory with names that start
*        "idat", and writes the result in a new NDF called "ostats".
*        The shape of ostats will be the intersection of the volumes of
*        all the indat* NDFs.
*     mstats idat* ostats trim=false
*        This does the same as the previous example, except that the
*        output NDF will be the `union' of the volumes of the input
*        NDFs, that is a cuboid with lower bounds as low as the
*        lowest pixel bound of the input NDFs in each dimension and
*        with upper bounds as high as the highest pixel bound in
*        each dimension.
*     mstats idat* ostats variance
*        This is like the first example except variance information
*        present is used to weight the data values.
*     mstats idat* ostats comp=variance variance
*        This does the same as the first example except that statistics
*        are calculated on the VARIANCE components of all the input
*        NDFs.  Thus the pixels of the VARIANCE component of "ostats"
*        will be the variances of the variances of the input data.
*     mstats m31* single=true pos="0:42:38,40:52:20"
*        This example is analysing the pixel brightness at the indicated
*        sky position in a number of NDFs whose name start with "m31",
*        which all have SKY as their current co-ordinate Frame.  The
*        mean and variance of the pixels at that position in all the
*        NDFs are printed to the screen.  If the reporting level is
*        verbose, the command also prints the value of the sampled pixel
*        in each of the NDFs.  For those in which the pixel at the
*        selected position is bad or falls outside the NDF, this is also
*        indicated.
*     mstats in="arr1,arr2,arr3" out=middle estimator=median wlim=1.0
*        This example calculates the medians of the DATA components of
*        the three named NDFs and writes them into a new NDF called
*        "middle".  All input values must be good to form a non-bad
*        output value.

*  Notes:
*     -  A warning is issued (at the normal reporting level) whenever
*     any output values are set bad because there are too few
*     contributing data values.  This reports the fraction of flagged
*     output data generated by the WLIM parameter's threshold.
*
*     No warning is given when parameter WLIM=0.  Input data containing
*     only bad values are not counted in the flagged fraction, since no
*     potential good output value has been lost.
*
*     - For SINGLE=TRUE the value of the MSG_FILTER environment
*     variable is used to output messages.  If it is QUIET, nothing is
*     reported on the screen.  If it is undefined, NORMAL or VERBOSE,
*     the statistics are reported.  If it is VERBOSE, the individual
*     pixel values are also reported.

*  Related Applications:
*     CCDPACK: MAKEMOS, MAKECAL, MAKEFLAT.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, VARIANCE,
*     QUALITY, LABEL, TITLE, UNITS, WCS, and HISTORY components of the
*     first input NDF and propagates all its extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     Calculations are performed using the more appropriate of the
*     data types real or double precision.  If the input NDFs'
*     structures contain values with other data types, then conversion
*     will be performed as necessary.
*     -  Up to six NDF dimensions are supported.
*     -  Huge NDF are supported.

*  Copyright:
*     Copyright (C) 2001, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research
*     Council.
*     Copyright (C) 2008, 2009, 2012, 2014, Science & Technology
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
*     MBT: Mark Taylor (Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-2001 (MBT):
*        Original version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 April 12 (MJC):
*        Remove unused variables, correct punctuation, and wrapped long
*        lines.
*     2008 April 13 (MJC):
*        Extend the choice of estimators for the full array with nww
*        parameters ESTIMATOR and WLIM, replacing SMODE.  Selecting
*        ESTMATOR="Mean" computes only the mean and no longer generates
*        a variance too unless new parameter VARIANCE is TRUE.  Use NDF
*        blocking.
*     2008 May 4 (MJC):
*        Support estimators that use the co-ordinate.
*     2009 July 5 (MJC):
*        Added Cmean and Csigma estimators and an associated CLIP
*        parameter.
*     2009 July 22 (MJC):
*        Revise tests regarding the fraction of bad pixels created
*        whilst forming statistics.  This is arises because of the
*        changed calculations within the statistics routines.
*     2009 July 22 (MJC):
*        Remove ILEVEL parameter and use the current reporting level
*        instead (set by the global MSG_FILTER environment variable).
*     2012 August 3 (MJC):
*        Added "NGood", "NBad", "FGood", and "FBad" estimators.
*     2014 August 13 (MJC):
*        Call KPS1_MSAGx separately for Data and Variance as this
*        routine only processes one array component at a time.
*        This preserves the pointer to the variance array.  Check for
*        bad status after obtaining parameters to ensure the parameter
*        values are defined before attempting to use them.
*     13-FEB-2020 (DSB):
*        Add support for huge NDFs.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'         ! Global SSE definitions
      INCLUDE  'AST_PAR'         ! AST constants and functions
      INCLUDE  'NDF_PAR'         ! NDF constants
      INCLUDE  'DAT_PAR'         ! HDS system constants
      INCLUDE  'CNF_PAR'         ! For CNF_PVAL function
      INCLUDE  'MSG_PAR'         ! Message-system constants
      INCLUDE  'PRM_PAR'         ! PRIMDAT constants

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      REAL CLPDEF                ! Default no. of standard deviations to
      PARAMETER( CLPDEF = 3.0 )  ! clip for mode, clipped mean & std dev

      INTEGER MAXPIX
      PARAMETER ( MAXPIX = 8388608 ) ! Guestimate a size: 8 megapixels

*  Local Variables:
      INTEGER CAXIS              ! Index of collapse axis
      REAL CLIP                  ! Value of CLIP parameter
      CHARACTER*( DAT__SZNAM ) COMP ! Name of NDF component for stats
      DOUBLE PRECISION DATUM     ! Value of pixel
      INTEGER*8 D                ! A dimension size
      INTEGER*8 EL               ! Number of pixels in mapped o/p array
      INTEGER*8 ELIN             ! Number of pixels in mapped stacked
                                 ! input array
      INTEGER*8 ELO              ! Number of elements in output NDF
      CHARACTER*6 ESTIM          ! Method to use to estimate combined
                                 ! values
      CHARACTER*112 ESTIMO       ! List of available estimators
      LOGICAL HIGHER             ! Significant dimensions above collapse
                                 ! axis?
      INTEGER I                  ! Loop variable
      INTEGER IBLOCK             ! Loop counter for the NDF blocks
      INTEGER*8 IBLSIZ( NDF__MXDIM ) ! Input-NDF sizes for processing
                                 ! large datasets in blocks
      INTEGER IGRP               ! GRP identifier for input-NDFs group
      INTEGER IPCO               ! Pointers to mapped co-ordinate array
      INTEGER IPDAT              ! Pointer to data array
      INTEGER IPIN( 2 )          ! Pointers to mapped input arrays
      INTEGER IPNDF              ! Pointer to array of NDF identifiers
      INTEGER IPOUT( 2 )         ! Pointers to mapped output arrays
      INTEGER IPW1               ! Pointer to first work array
      INTEGER IPW2               ! Pointer to second work array
      INTEGER IPW3               ! Pointer to third work array
      INTEGER IPWID              ! Pointers to mapped width work array
      CHARACTER*( NDF__SZTYP ) ITYPE ! HDS type of output data arrays
      INTEGER J                  ! Loop count
      INTEGER*8 LBND( NDF__MXDIM ) ! Lower pixel index bounds of the
                                 ! output NDF
      INTEGER*8 LBNDO( NDF__MXDIM - 1 ) ! Lower pixel index bounds of the
                                 ! output NDF block
      INTEGER*8 LBNDS( NDF__MXDIM ) ! Lower pixel index bounds of the
                                 ! section of the input NDF
      LOGICAL LOOP               ! Continue to loop through dimensions?
      INTEGER*8 MAXSIZ           ! Maximum size of block along current
                                 ! dimension
      DOUBLE PRECISION MEAN      ! Average value
      DOUBLE PRECISION MED       ! Median value
      INTEGER NBLOCK             ! Number of NDF blocks
      INTEGER NC                 ! Number of characters in COMP
      LOGICAL NDFVAR             ! NDF contains a variance array?
      INTEGER NDIM               ! Number of pixel axes in input NDF
      INTEGER NDIMO              ! Number of pixel axes in output NDF
      INTEGER*8 NFLAG            ! Number of WLIM-flagged o/p values
      INTEGER NGOOD              ! Number of non-bad pixels
      INTEGER NGOOD1             ! Number of pixels used
      INTEGER NNDF               ! The number of input NDFs
      INTEGER*8 NNDF8            ! The number of input NDFs
      INTEGER OBL                ! Identifier for output-NDF block
      INTEGER*8 OBLSIZ( NDF__MXDIM ) ! Output-NDF sizes for processing
                                 ! large datasets in blocks
      INTEGER ONDF               ! NDF identifier of output NDF
      LOGICAL PVAR               ! Process variances?
      LOGICAL SINGLE             ! Are we in SINGLE or ARRAY mode?
      CHARACTER*8 STRIM          ! Trim parameter to pass to NDF_MBNDN
      DOUBLE PRECISION SUM       ! Running total of pixel values
      DOUBLE PRECISION SUM2      ! Running total of pixel values squared
      LOGICAL TRIM               ! Will be trim or pad arrays?
      INTEGER*8 UBND( NDF__MXDIM ) ! Upper pixel index bounds of the
                                 ! output NDF
      INTEGER*8 UBNDO( NDF__MXDIM - 1 )! Upper pixel index bounds of the
                                 ! output NDF block
      INTEGER*8 UBNDS( NDF__MXDIM ) ! Upper pixel index bounds of the
                                 ! section of the input NDF
      LOGICAL USEVAR             ! Allow weights to be derived from the
                                 ! NDF's variance array (if present)
      DOUBLE PRECISION VAR       ! Variance value
      REAL WLIM                  ! Value of WLIM parameter

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Get a group containing the names of the NDFs to be processed.
      NNDF = 0
      CALL KPG1_RGNDF( 'IN', 0, 1, '  Give more NDFs...', IGRP,
     :                 NNDF, STATUS )

*  Determine whether we are in single pixel or array mode.
      CALL PAR_GET0L( 'SINGLE', SINGLE, STATUS )

*  Get the component we require.
      CALL PAR_CHOIC( 'COMP', 'DATA', 'DATA,VARIANCE,QUALITY,ERROR',
     :                .FALSE., COMP, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Single pixel mode.
*  ==================
      IF ( SINGLE ) THEN

*  Allocate some work space.
         CALL PSX_CALLOC( NNDF, '_DOUBLE', IPDAT, STATUS )

*  Get the pixels from which to calculate the statistics.
         CALL KPS1_MSS( IGRP, NNDF, COMP, NGOOD,
     :                  %VAL( CNF_PVAL( IPDAT ) ),
     :                  STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  See if we have found any pixels at the specified point.
         IF ( NGOOD .GT. 0 ) THEN

*  If so, calculate the median.
*  (Note: the first argument should properly be .FALSE., but this
*  currently falls foul of a bug in KPG1_MEDUD.  This version is
*  correct, and is hardly a performance bottleneck).
            CALL KPG1_MEDUD( .TRUE., NGOOD, %VAL( CNF_PVAL( IPDAT ) ),
     :                       MED,
     :                       NGOOD1, STATUS )

*  And calculate the mean and variance.
            SUM = 0D0
            SUM2 = 0D0
            DO I = 1, NGOOD
               CALL KPG1_RETRD( NGOOD, I, %VAL( CNF_PVAL( IPDAT ) ),
     :                          DATUM, STATUS )
               SUM = SUM + DATUM
               SUM2 = SUM2 + DATUM * DATUM
            END DO
            MEAN = SUM / DBLE( NGOOD )
            VAR = SUM2 / DBLE( NGOOD ) - MEAN * MEAN

*  Output the results, if required.
            CALL MSG_SETD( 'MED', MED )
            CALL MSG_OUTIF( MSG__NORM, ' ',
     :                      '    Pixel median:      ^MED', STATUS )
            CALL MSG_SETD( 'MEAN', MEAN )
            CALL MSG_OUTIF( MSG__NORM, ' ',
     :                      '    Pixel mean:        ^MEAN', STATUS )
            CALL MSG_SETD( 'VAR', VAR )
            CALL MSG_OUTIF( MSG__NORM, ' ',
     :                      '    Pixel variance:    ^VAR', STATUS )
            CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Write them to output parameters.
            CALL PAR_PUT0D( 'MEDIAN', MED, STATUS )
            CALL PAR_PUT0D( 'MEAN', MEAN, STATUS )
            CALL PAR_PUT0D( 'VAR', VAR, STATUS )

*  There were no good pixels - write an error message.
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'MSTATS_ERR1', 'There are no good pixels.',
     :                    STATUS )
         END IF

*  Release the work space.
         CALL PSX_FREE( IPDAT, STATUS )

*  Array mode.
*  ===========
      ELSE

*  Weighting by variance?
*  ----------------------
*  See if all of the input NDFs have a VARIANCE component.
         CALL KPS1_MSVP( IGRP, NNDF, NDFVAR, STATUS )

*  Find out whether variances are to be used to define the weights, if
*  the NDF contains any.
         USEVAR = .FALSE.
         IF ( NDFVAR ) CALL PAR_GET0L( 'VARIANCE', USEVAR, STATUS )

*  Weights will be derived from variances only if allowed by USEVAR and
*  if the NDF contains a variance array.
         PVAR = ( USEVAR .AND. NDFVAR )

*  Extend the list of components to be accessed.
         IF ( PVAR ) THEN
            NC = CHR_LEN( COMP )
            CALL CHR_APPND( ',VARIANCE', COMP, NC )
         END IF

*  Estimator
*  ---------
*  Different estimators require different minimum of input values to
*  derive a result.
         IF ( NNDF .GT. 3 ) THEN
            ESTIMO = 'Mean,WMean,Mode,Median,Max,Min,Comax,Comin,'/
     :               /'Absdev,Cmean,Csigma,RMS,Sigma,Sum,Iwc,Iwd,'/
     :               /'Integ,FBad,FGood,NBad,NGood'
         ELSE IF ( NNDF .EQ. 1 ) THEN
            ESTIMO = 'Mean,Max,Min,Comax,Comin,Sum,Iwc,Integ'
         ELSE IF ( NNDF .EQ. 2 ) THEN
            ESTIMO = 'Mean,WMean,Max,Min,Comax,Comin,Absdev,Sum,Iwc,'/
     :               /'Integ,FBad,FGood,NBad,NGood'
         ELSE IF ( NNDF .EQ. 3 ) THEN
            ESTIMO = 'Mean,WMean,Median,Max,Min,Comax,Comin,Absdev,'/
     :               /'Sum,Iwc,Integ,FBad,FGood,NBad,NGood'
         END IF

*  Get the ESTIMATOR and WLIM parameters.
         CALL PAR_CHOIC( 'ESTIMATOR', 'Mean', ESTIMO, .FALSE., ESTIM,
     :                   STATUS )

         CALL PAR_GDR0R( 'WLIM', 0.3, 0.0, 1.0, .FALSE., WLIM, STATUS )

*  See whether the output NDF will be the union or intersection of the
*  inputs.
         CALL PAR_GET0L( 'TRIM', TRIM, STATUS )
         IF ( TRIM ) THEN
            STRIM = 'TRIM'
         ELSE
            STRIM = 'PAD'
         END IF

*  For now obtain just a single number of standard deviations at which
*  to clip.
         IF ( ESTIM .EQ. 'MODE' .OR. ESTIM .EQ. 'CMEAN' .OR.
     :        ESTIM .EQ. 'CSIGMA' ) THEN
            CALL PAR_GDR0R( 'CLIP', CLPDEF, VAL__SMLR, VAL__MAXR,
     :                      .FALSE., CLIP, STATUS )
         ELSE
            CLIP = CLPDEF
         END IF

*  Redefine the data units.
*  ========================

*  There's no guarantee that all the supplied NDFs have the same units,
*  hence there's no revision for COMAX, COMIN, IWC, and IWD estimators.

*  New unit is "pixel".
      IF ( ESTIM .EQ. 'NGOOD' .OR. ESTIM .EQ. 'NBAD' ) THEN
         CALL NDF_CPUT( 'Pixel', ONDF, 'Units', STATUS )

*  Dimensionless...
      ELSE IF ( ESTIM .EQ. 'FGOOD' .OR. ESTIM .EQ. 'FBAD' ) THEN
         CALL NDF_CPUT( ' ', ONDF, 'Units', STATUS )

      END IF

*  Obtain NDF identifiers and bounds.
*  ----------------------------------
*  Allocate some work space.
         CALL PSX_CALLOC( NNDF, '_INTEGER', IPNDF, STATUS )

*  Obtain identifiers and bounds of pixel-matched requested components
*  of all the NDFs.
         CALL KPS1_MSBS( IGRP, NNDF, COMP, STRIM, 'OUT', ITYPE,
     :                   %VAL( CNF_PVAL( IPNDF ) ), ONDF, STATUS )
         CALL NDF_BOUND8( ONDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Get a title for it from the parameter system.
         CALL NDF_CINP( 'TITLE', ONDF, 'Title', STATUS )

*  Adjust output data type if required.
*  ====================================

*  The NBad and NGood estimators always produce _INTEGER output NDFs.
      IF ( ESTIM .EQ. 'NGOOD' .OR. ESTIM .EQ. 'NBAD' ) THEN
         CALL NDF_RESET( ONDF, COMP, STATUS )
         CALL NDF_STYPE( '_INTEGER', ONDF, COMP, STATUS )
         ITYPE = '_INTEGER'
      ELSE
         ITYPE = ITYPE
      END IF

*  Process in blocks.
*  ==================

*  For large datasets, there may be insufficient memory.  Therefore
*  we form blocks of input pixels to process, one at a time.  Now we
*  are going to create an extra dimension in these blocks that stores
*  the values at each input pixel.  That's how the COLLAPSE routines
*  operate.  By definition, therefore we need all the pixels along this
*  pseudo dimension to always be present in full for each output pixel
*  along the other pixel axes.  If this leaves room for a full span of a
*  dimension that becomes the block size along that axis.  Partial fills
*  take the remaining maximum size and subsequent dimensions' block
*  sizes are unity.
         CAXIS = NDIM + 1
         IBLSIZ( CAXIS ) = NNDF
         MAXSIZ = MAX( 1_8, MAXPIX / NNDF )
         LOOP = .TRUE.
         J = 0
         DO I = 1, NDIM
            IF ( LOOP ) THEN
               D = UBND( I ) - LBND( I ) + 1
               IF ( MAXSIZ .GE. D ) THEN
                  IBLSIZ( I ) = D
                  MAXSIZ = MAXSIZ / D
               ELSE
                  IBLSIZ( I ) = MAXSIZ
                  LOOP = .FALSE.
               END IF
            ELSE
               IBLSIZ( I ) = 1
            END IF

*  Copy the output NDF block sizes in sequence omitting the
*  collapse axis.
            J = J + 1
            OBLSIZ( J ) = IBLSIZ( I )
         END DO

*  If the LOW and HIGH limits have reduced the collapsed section from
*  the whole collapsed dimension, then we cannot use the original input
*  NDF to derive the number of blocks.  Instead we create a subsection
*  spanning the actual collapse limits, as if the user had supplied
*  this section with the input NDF.
         DO I = 1, NDIM
            LBNDS( I ) = LBND( I )
            UBNDS( I ) = UBND( I )
         END DO
         LBNDS( CAXIS ) = 1
         UBNDS( CAXIS ) = NNDF

*  Determine the number of blocks.
         CALL NDF_NBLOC8( ONDF, NDIM, IBLSIZ, NBLOCK, STATUS )

*  The total number of elements in the output array is needed for the
*  calculation of the fraction of bad pixels generated by the
*  statistical calculations in conjunction with the WLIM parameter.
*  The count of the bad pixels generated is summed within KPS1_CLPSx.
         NFLAG = 0
         ELO = 0

*  Loop through each block.  Start a new NDF context.
         DO IBLOCK = 1, NBLOCK
            CALL NDF_BEGIN

*  Make blocks from the input NDFs, and concatenate them, so as to
*  make an array one dimension larger, which is then collapsed.
*  The routine returns the pointer to the mapped component.
            IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPS1_MSAGD( NNDF, %VAL( CNF_PVAL( IPNDF ) ), 'Data',
     :                          NDIM, IBLSIZ, IBLOCK, IPIN( 1 ),
     :                          STATUS )
               IF ( PVAR ) THEN
                  CALL KPS1_MSAGD( NNDF, %VAL( CNF_PVAL( IPNDF ) ),
     :                             'Variance', NDIM, IBLSIZ, IBLOCK,
     :                             IPIN( 2 ), STATUS )
               END IF
            ELSE
               CALL KPS1_MSAGR( NNDF, %VAL( CNF_PVAL( IPNDF ) ), 'Data',
     :                          NDIM, IBLSIZ, IBLOCK, IPIN(  1 ),
     :                          STATUS )
               IF ( PVAR ) THEN
                  CALL KPS1_MSAGR( NNDF, %VAL( CNF_PVAL( IPNDF ) ),
     :                             'Variance', NDIM, IBLSIZ, IBLOCK,
     :                             IPIN( 2 ), STATUS )
               END IF
            END IF
            CALL NDF_BLOCK8( ONDF, NDIM, OBLSIZ, IBLOCK, OBL, STATUS )

*  Map the NDF arrays and workspace required.
*  ==========================================

*  Map the full output data and (if needed) variance arrays.  Also set
*  the number of elements in the IPIN work array of the stacked
*  input-NDFs' blocks.
            CALL NDF_MAP8( OBL, COMP, ITYPE, 'WRITE', IPOUT, EL,
     :                     STATUS )
            ELIN = EL * NNDF

            IF ( .NOT. PVAR ) THEN
               IPIN( 2 ) = IPIN( 1 )
               IPOUT( 2 ) = IPOUT( 1 )
            END IF

*  Obtain the bounds of the blocks.
            CALL NDF_BOUND8( OBL, NDF__MXDIM - 1, LBNDO, UBNDO, NDIMO,
     :                       STATUS )

*  Store safe pointer values as the collapse is along the final
*  dimension of the concatenated NDF arrays, and so no additional
*  work space is needed.
            IPW1 = IPIN( 1 )
            IPW2 = IPIN( 1 )
            HIGHER = .FALSE.

*  Associate co-ordinate information.
*  ==================================

*  Obtain co-ordinates along the collapse axis for the following
*  methods.
            IF ( ESTIM .EQ. 'COMAX' .OR. ESTIM .EQ. 'COMIN' .OR.
     :           ESTIM .EQ. 'IWC' .OR. ESTIM .EQ. 'IWD' .OR.
     :           ESTIM .EQ. 'INTEG' ) THEN

*  Since the collapse axis is only notional, being the order of the
*  supplied NDFs, the co-ordinates are the NDF indices equivalent to
*  GRID co-ordinates.

*  Create workspace for the co-ordinates at each pixel in the input
*  array along the notional axis
*  in the correct data type.
               CALL PSX_CALLOC8( ELIN, ITYPE, IPCO, STATUS )

*  Obtain the co-ordinate centres along the collapse axis at every
*  element of the NDF block.
               IF ( ITYPE .EQ. '_REAL' ) THEN
                  CALL KPS1_MSGRR( NNDF, EL, %VAL( CNF_PVAL( IPCO ) ),
     :                             STATUS )

               ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                  CALL KPS1_MSGRD( NNDF, EL, %VAL( CNF_PVAL( IPCO ) ),
     :                             STATUS )
               END IF
            ELSE
               IPCO = IPIN( 1 )
               IPW3 = IPIN( 1 )
            END IF

*  Associate AXIS-width information.
*  =================================

*  Obtain AXIS widths along the collapse axis for the following
*  methods.
            IF ( ESTIM .EQ. 'INTEG' ) THEN

*  Allocate work space for the widths to be derived from the
*  co-ordinates.  These will be 1.0 for all values since the notional
*  collapse axis has GRID co-ordinates.
               CALL PSX_CALLOC8( ELIN, ITYPE, IPWID, STATUS )

*  Store safe pointer value if widths are not needed.
            ELSE
               IPWID = IPIN( 1 )
            END IF

*  Collapse.
*  =========

*  Now do the work, using a routine appropriate to the numeric type.
            NNDF8 = NNDF
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPS1_CLPSR( CAXIS, 1_8, NNDF8, PVAR, ESTIM, WLIM,
     :                          CLIP, EL, NDIM, LBNDS, UBNDS,
     :                          %VAL( CNF_PVAL( IPIN( 1 ) ) ),
     :                          %VAL( CNF_PVAL( IPIN( 2 ) ) ),
     :                          %VAL( CNF_PVAL( IPCO ) ),
     :                          NDIMO, LBNDO, UBNDO, HIGHER, NFLAG,
     :                          %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                          %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                          %VAL( CNF_PVAL( IPWID ) ),
     :                          %VAL( CNF_PVAL( IPW1 ) ),
     :                          %VAL( CNF_PVAL( IPW2 ) ),
     :                          %VAL( CNF_PVAL( IPW3 ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPS1_CLPSD( CAXIS, 1_8, NNDF8, PVAR, ESTIM, WLIM,
     :                          CLIP, EL, NDIM, LBNDS, UBNDS,
     :                          %VAL( CNF_PVAL( IPIN( 1 ) ) ),
     :                          %VAL( CNF_PVAL( IPIN( 2 ) ) ),
     :                          %VAL( CNF_PVAL( IPCO ) ),
     :                          NDIMO, LBNDO, UBNDO, HIGHER, NFLAG,
     :                          %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                          %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                          %VAL( CNF_PVAL( IPWID ) ),
     :                          %VAL( CNF_PVAL( IPW1 ) ),
     :                          %VAL( CNF_PVAL( IPW2 ) ),
     :                          %VAL( CNF_PVAL( IPW3 ) ), STATUS )

            ELSE IF ( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'T', ITYPE )
               CALL ERR_REP( 'MSTATS_ERR5', 'MSTATS: Unsupported '//
     :                       'data type ^T (programming error).',
     :                       STATUS )
            END IF

*  Tidy co-ordinate workspaces.
            IF ( ESTIM .EQ. 'COMAX' .OR. ESTIM .EQ. 'COMIN' .OR.
     :           ESTIM .EQ. 'IWC' .OR. ESTIM .EQ. 'IWD' .OR.
     :           ESTIM .EQ. 'INTEG' ) THEN
                CALL PSX_FREE( IPCO, STATUS )
            END IF

            IF ( ESTIM .EQ. 'INTEG' ) THEN
               CALL PSX_FREE( IPWID, STATUS )
            END IF

*   Add the current block's number of elements to the total.  This
*   is arguably clearer than finding the total using LBND and UBND.
            ELO = ELO + EL

*   Close NDF context.
            CALL NDF_END( STATUS )
         END DO

*  Release workspace.
         CALL PSX_FREE( IPNDF, STATUS )

*  Annul the output NDF.
         CALL NDF_ANNUL( ONDF, STATUS )
      END IF

*  Warn about lost pixels due to insufficient input values.
*  ========================================================

*  For some applications such as spectral-cube analysis where large
*  sections may be flagged, the wrong WLIM may lead to surprising
*  results.  While the WLIM default could be set to zero, this is not
*  the normal default of 0.3 used elsewhere in KAPPA.  At the risk of
*  annoying some users, report the number of output data that were
*  flagged by the WLIM threshold at the normal reporting level.
       IF ( NFLAG .GT. 0 ) THEN
         CALL MSG_FMTR( 'WLIM', 'F6.4', WLIM )
         IF ( NFLAG .EQ. ELO ) THEN
            CALL MSG_OUTIF( MSG__NORM, '',
     :        'WARNING: All of the output pixels are set bad due to '/
     :        /'an excessive number of bad values along the axis of '/
     :        /'axis of collapse.  If this is undesired, decrease the '/
     :        /'fraction of good values required with parameter WLIM '/
     :        /'(currently ^WLIM).', STATUS )

*  The FRAC token is not directly comparable with WLIM.  Report the
*  fraction of bad pixels.  Note this includes cases where all the input
*  pixels along the collapse axis were bad for a given output pixel.
         ELSE IF ( NFLAG .LT. ELO ) THEN
            CALL MSG_FMTR( 'FRAC', 'F6.4', REAL( NFLAG ) / REAL( ELO ) )
            CALL MSG_SETK( 'NF', NFLAG )
            CALL MSG_SETK( 'EL', ELO )
            CALL MSG_OUTIF( MSG__NORM, '',
     :        'WARNING: ^FRAC of the output pixels (^NF of ^EL) are '/
     :        /'set bad due to an excessive number of bad values '/
     :        /'along the collapse axis.  If this is undesired, '/
     :        /'decrease the fraction of good values required with '/
     :        /'Parameter WLIM (currently ^WLIM).', STATUS )
         END IF
      END IF

*  Come here if something has gone wrong.
  999 CONTINUE

*  Release GRP resources.
      CALL GRP_DELET( IGRP, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Report a contextual message if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'MSTATS_ERR2',
     :                 'MSTATS: Unable to calculate statistics.',
     :                 STATUS )
      END IF

      END
