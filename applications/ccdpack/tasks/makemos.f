      SUBROUTINE MAKEMOS( STATUS )
*+
*  Name:
*     MAKEMOS

*  Purpose:
*     Makes a mosaic by combining and normalising a set of NDFs.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MAKEMOS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This is a comprehensive application for combining a set of NDFs
*     (normally representing overlapping coverage of an object) into a
*     single mosaic. It addresses the problems of (a) combining a
*     sequence of separate data sets into a single NDF and (b)
*     optionally normalising each NDF so that they match each other in
*     regions where they overlap. Mutual alignment of the separate NDFs
*     is not performed by this application and must be addressed
*     beforehand (although NDFs may be aligned to the nearest pixel
*     simply by shifting their pixel origin).
*
*     MAKEMOS registers the set of NDFs supplied by matching their
*     pixel indices and then forms a mosaic by combining the separate
*     input pixel values at each location using a nominated
*     data-combination method (by default, it takes the median).  The
*     resulting mosaic is of sufficient extent to accommodate all the
*     input data, with any output data pixels which do not receive
*     values from the input being set to the "bad" pixel value.
*     Account is taken of variance information associated with the
*     input NDFs, and all calculations are optimally weighted to
*     minimise the output noise. Output variance estimates for the
*     final mosaic may also be produced.
*
*     Forming a mosaic in this way will normally be successful only so
*     long as the input data are mutually consistent. Unfortunately,
*     this is often not the case, since data frequently have differing
*     effective exposure times and background levels which give
*     discontinuities in the final mosaic. Thus, MAKEMOS also addresses
*     the problem of normalising the input NDFs to make them mutually
*     consistent. It does this by optionally applying optimised
*     multiplicative and/or additive corrections (termed scale-factor
*     and zero-point corrections) to each NDF before forming the
*     mosaic.  These optimised corrections are determined by
*     inter-comparing the input NDFs in pairs, using the regions where
*     they overlap to determine the relative scale-factor and/or
*     zero-point difference between each pair.  A self-consistent set
*     of corrections is then found which, when applied to each input
*     NDF, will best eliminate these observed differences and give a
*     smooth mosaic.

*  Usage:
*     makemos in out

*  ADAM Parameters:
*     ALPHA = _REAL (Read)
*        The fraction of extreme values to remove before combining
*        input data if the "trimmed mean" data combination method is
*        selected for producing the output mosaic (see the METHOD
*        parameter). A fraction alpha (approximately) of the available
*        values is removed from each extreme. This may take values in
*        the range 0 to 0.5.
*        [0.2]
*     CMPVAR = _LOGICAL (Read)
*        This parameter controls the use of statistical error
*        (variance) information contained in the input NDFs when they
*        are inter-compared in pairs to derive scale-factor or
*        zero-point corrections. It is only used if either SCALE or
*        ZERO is set to TRUE and if two or more of the input NDFs
*        contain variance information (a "reference NDF" also counts,
*        if supplied). In this case, if CMPVAR is set to TRUE, then
*        variance information is used to correctly weight the input
*        data whenever a pair of input NDFs are inter-compared and both
*        have variance information available.
*
*        The default behaviour is to use variance information during
*        inter-comparisons. This may be suppressed by setting CMPVAR to
*        FALSE, which sometimes gives faster execution without greatly
*        affecting the result (also see the "Algorithms Used" section).
*        However, if input data with similar values have widely
*        differing variance values within the same input NDF, then use
*        of input variance information is recommended (this could
*        happen, for instance, if an input NDF is the result of a
*        previous mosaic-ing process).
*        [TRUE]
*     CORRECT = LITERAL (Read)
*        The name of the file used to output the scale and zero-point
*        corrections (see SCALE and ZERO parameters). This file can be
*        read by the DRIZZLE task.  If the file already exists, it is
*        overwritten.  If a null (!) value is supplied, or if SCALE and
*        ZERO are both set to FALSE, no file is written.
*        [!]
*     GENVAR = _LOGICAL (Read)
*        If GENVAR is set to TRUE and all the input NDFs supplied
*        contain statistical error (variance) information, then
*        variance information will also be calculated for the output
*        mosaic NDF, provided that USEVAR is also TRUE.
*
*        Otherwise if GENVAR is TRUE and either USEVAR is FALSE or some
*        of the input NDFs do not contain error information, then output
*        variances will be generated using the natural variations in the
*        input data. Obviously this method should only be used if there
*        are many input datasets, which also provide good coverage of
*        the output area. If this option is chosen any regions of the
*        output image that have only one input value will have their
*        associated variances set bad.
*
*        The default for this parameter depends on the presence of error
*        information in the input NDFs. If all have error information
*        then the default is TRUE, otherwise it is FALSE.
*
*        [DYNAMIC]
*     IN = LITERAL (Read and [optionally] Write)
*        A list of the names of the input NDFs which are to be combined
*        into a mosaic. The NDF names should be separated by commas
*        and may include wildcards.
*
*        The input NDFs are normally accessed only for reading.
*        However, if the MODIFY parameter is set to TRUE (and
*        scale-factor or zero-point corrections are being calculated)
*        then each of the "input" NDFs will be modified by applying the
*        calculated corrections.
*     LISTIN = _LOGICAL (Read)
*        If a TRUE value is given for this parameter (the default),
*        then the names of all the NDFs supplied as input will be
*        listed (and will be recorded in the logfile if this is
*        enabled).  Otherwise, this listing will be omitted.
*        [TRUE]
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter, then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP,
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
*        If the logging system has been initialised using CCDSETUP,
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     MAX = _REAL (Read)
*        Upper limit for input data values which may contribute to the
*        output mosaic if the "threshold" data combination method is
*        selected (see the METHOD parameter). [Maximum real value]
*     MAXIT = _INTEGER (Read)
*        This parameter specifies the maximum number of iterations to
*        be used when inter-comparing pairs of input NDF data arrays to
*        determine their relative scale-factor and/or zero-point. It is
*        only used if (a) both the SCALE and ZERO parameters have been
*        set to TRUE, or (b) SCALE has been set to TRUE and statistical
*        error (variance) information obtained from the input NDFs is
*        being used to weight the data during the inter-comparison. In
*        other cases the inter-comparison operation is not iterative.
*
*        If the specified number of iterations is exceeded without
*        achieving the accuracy required by the settings of the TOLS
*        and TOLZ parameters, then a warning message will be issued,
*        but the results will still be used. The value given for MAXIT
*        must be at least one.
*        [20]
*     METHOD = LITERAL (Read)
*        The method to be used to combine the input NDFs' data values
*        to form the output mosaic. This may be set to any unique
*        abbreviation of the following:
*           -  MEAN      -- Mean of the input data values
*           -  MEDIAN    -- Weighted median of the input data values
*           -  TRIMMED   -- An "alpha trimmed mean" in which a fraction
*                           alpha of the values are removed from
*                           each extreme
*           -  MODE      -- An iteratively "sigma clipped" mean which
*                           approximates to the modal value
*           -  SIGMA     -- A sigma clipped mean
*           -  THRESHOLD -- Mean with values above and below given
*                           limits removed
*           -  MINMAX    -- Mean with the highest and lowest values
*                           removed
*           -  BROADENED -- A broadened median (the mean of a small
*                           number of central values)
*           -  CLIPMED   -- A sigma clipped median (like SIGMA except
*                           that the median of the clipped values is used)
*           -  FASTMED   -- Unweighted median of input data values
*        [MEDIAN]
*     MIN = _REAL (Read)
*        Lower limit for input data values which may contribute to the
*        output mosaic if the "threshold" data combination method is
*        selected (see the METHOD parameter).
*        [Minimum real value]
*     MODIFY = _LOGICAL (Read)
*        By default, the NDFs supplied via the IN parameter are
*        regarded as "input" NDFs and will not be modified. However, if
*        scale-factor or zero-point corrections are being calculated
*        (see the SCALE and ZERO parameters), then giving a TRUE value
*        for MODIFY indicates that these NDFs are themselves to be
*        modified by applying the calculated corrections before the
*        output mosaic is formed.
*
*        This facility provides a means of applying corrections to
*        individual NDFs (e.g. to mutually normalise them) without
*        necessarily also combining them into a mosaic. It may also be
*        useful if several invocations of MAKEMOS are to be made with
*        different parameter settings; by specifying MODIFY=TRUE for
*        the first invocation, scale-factor or zero-point corrections
*        may be applied to normalise the input data so that this need
*        not be repeated on each invocation.
*
*        WARNING: Caution should be exercised if setting MODIFY to
*        TRUE, as information about the uncorrected data values of the
*        "input" NDFs will not be retained.
*        [FALSE]
*     NITER = _REAL (Read)
*        Maximum number of refining iterations used if the "mode" data
*        combination method is selected (see the METHOD parameter).
*        [7]
*     OPTOV = _INTEGER (Read)
*        This parameter specifies the "optimum number of overlaps"
*        which an NDF should have with its neighbours and controls the
*        number of inter-comparisons made between pairs of overlapping
*        NDFs when determining scale-factor or zero-point corrections
*        (see the SCALE and ZERO parameters).
*
*        The need for this parameter arises because when multiple input
*        NDFs are supplied there may be a large number of potential
*        pair-wise overlaps between them.  To prevent them all being
*        used, which may take far longer than is justified, this set of
*        potential overlaps is reduced by elimination, starting with
*        the smallest ones (as measured by the number of overlapping
*        pixels) and continuing until no more overlaps can be removed
*        without reducing the number of overlaps of any NDF below the
*        value given for OPTOV.  In practice, this means that each NDF
*        will end up with about (although not exactly) OPTOV overlaps
*        with its neighbours, with the largest overlaps being
*        preferred.
*
*        Note that although this algorithm is effective in reducing the
*        number of overlaps, it is not guaranteed always to result in a
*        set of overlaps which allow the optimum set of corrections to
*        be calculated. In practice, problems from this cause are
*        unlikely unless unusual patterns of NDF overlap are involved,
*        but they may be solved by increasing the value of OVOPT and/or
*        constructing the required mosaic in pieces by running MAKEMOS
*        several times on different sets of input NDFs.
*
*        In some cases, reducing the value of OVOPT may reduce the
*        number of inter-comparisons made, and hence reduce the
*        execution time, but if too few inter-comparisons are made,
*        there is a risk that the corrections obtained may not be the
*        best possible.
*
*        This parameter is only used if SCALE or ZERO is set to TRUE.
*        [3]
*     OUT = NDF (Write)
*        Name of the NDF to contain the output mosaic. This is normally
*        mandatory. However, if the "input" NDFs are being modified (by
*        setting the MODIFY parameter to TRUE), then it may optionally
*        be omitted by supplying a null value (!). In this case, no
*        output mosaic will be formed.
*     PRESERVE = _LOGICAL (Read)
*        If a TRUE value is given for this parameter (the default),
*        then the data type of the output mosaic NDF will be derived
*        from that of the input NDF with the highest precision, so that
*        the input data type will be "preserved" in the output NDF.
*        Alternatively, if a FALSE value is given, then the output NDF
*        will be given an appropriate floating point data type.
*
*        When using integer input data, the former option is useful for
*        minimising the storage space required for large mosaics, while
*        the latter typically permits a wider output dynamic range when
*        necessary. A wide dynamic range is particularly important if a
*        large range of scale factor corrections are being applied (as
*        when combining images with a wide range of exposure times).
*
*        If a global value has been set up for this parameter using
*        CCDSETUP, then that value will be used.
*        [TRUE]
*     REF = NDF (Read)
*        If scale-factor and/or zero-point corrections are being
*        applied (see the SCALE and ZERO parameters) then, by default,
*        these are normalised so that the median corrections are unity
*        and zero respectively. However, if an NDF is given via the REF
*        parameter (so as to over-ride its default null value), then
*        scale-factor and zero-point corrections will instead be
*        adjusted so that the corrected data are normalised to the
*        "reference NDF" supplied.
*
*        This provides a means of retaining the calibration of a set of
*        data, even when corrections are being applied, by nominating a
*        reference NDF which is to remain unchanged. It also allows the
*        output mosaic to be normalised to any externally-calibrated
*        NDF with which it overlaps, and hence allows a calibration to
*        be transferred from one set of data to another.
*
*        If the NDF supplied via the REF parameter is one of those
*        supplied as input via the IN parameter, then this serves to
*        identify which of the input NDFs should be used as a
*        reference, to which the others will be adjusted. In this case,
*        the scale-factor and/or zero-point corrections applied to the
*        nominated input NDF will be set to one and zero, and the
*        corrections for the others will be adjusted accordingly.
*
*        Alternatively, if the reference NDF does not appear as one of
*        the input NDFs, then it will be included as an additional set
*        of data in the inter-comparisons made between overlapping NDFs
*        and will be used to normalise the corrections obtained (so
*        that the output mosaic is normalised to it). However, it will
*        not itself contribute to the output mosaic in this case.
*        [!]
*     SCALE = _LOGICAL (Read)
*        This parameter specifies whether MAKEMOS should attempt to
*        adjust the input data values by applying scale-factor (i.e.
*        multiplicative) corrections before combining them into a
*        mosaic. This would be appropriate, for instance, if a series
*        of images had been obtained with differing exposure times; to
*        combine them without correction would yield a mosaic with
*        discontinuities at the image edges where the data values
*        differ.
*
*        If SCALE is set to TRUE, then MAKEMOS will inter-compare the
*        NDFs supplied as input and will estimate the relative
*        scale-factor between selected pairs of input data arrays where
*        they overlap.  From this information, a global set of
*        multiplicative corrections will be derived which make the
*        input data as mutually consistent as possible. These
*        corrections will be applied to the input data before combining
*        them into a mosaic.
*
*        Calculation of scale-factor corrections may also be combined
*        with the use of zero-point corrections (see the ZERO
*        parameter). By default, no scale-factor corrections are
*        applied.
*        [FALSE]
*     SIGMAS = _REAL (Read)
*        Number of standard deviations at which to reject values if the
*        "mode", "sigma" or "clipmed" data combination methods are
*        selected (see the METHOD parameter). This value must be
*        positive. [4.0]
*     SKYSUP = _REAL (Read)
*        A positive "sky noise suppression factor" used to control the
*        effects of sky noise when pairs of input NDFs are
*        inter-compared to determine their relative scale-factor. It is
*        intended to prevent the resulting scale-factor estimate being
*        biased by the many similar values present in the "sky
*        background" of typical astronomical data.  SKYSUP controls an
*        algorithm which reduces the weight given to data where there
*        is a high density of points with the same value, in order to
*        suppress this effect. It is only used if a scale factor is
*        being estimated (i.e. if SCALE is TRUE).
*
*        A SKYSUP value of unity can often be effective, but a value
*        set by the approximate ratio of sky pixels to useful object
*        pixels (i.e. those containing non-sky signal) in a "typical"
*        NDF overlap region will usually be better. The precise value
*        is not critical. A value of zero disables the sky noise
*        suppression algorithm completely. The default value for SKYSUP
*        is 10**(n/2.0), where n is the number of significant
*        dimensions in the output mosaic. Hence, for a 2-dimensional
*        image, it will default to 10 which is normally reasonable for
*        CCD frames of extended objects such as galaxies (a larger
*        value, say 100, may give slightly better results for star
*        fields).
*        [10**(n/2.0)]
*     TITLE = LITERAL (Read)
*        Title for the output mosaic NDF. [Output from MAKEMOS]
*     TOLS = _REAL (Read)
*        This parameter defines the accuracy tolerance to be achieved
*        when inter-comparing pairs of input NDF data arrays to
*        determine their relative scale-factor. It is only used if the
*        inter-comparison is to be performed iteratively, which will be
*        the case if (a) both the SCALE and ZERO parameters have been
*        set to TRUE, or (b) SCALE has been set to TRUE and statistical
*        error (variance) information obtained from the input NDFs is
*        being used to weight the data during the inter-comparison.
*
*        The value given for TOLS specifies the tolerable fractional
*        error in the estimation of the relative scale-factor between
*        any pair of input NDFs. This value must be positive.
*        [0.001]
*     TOLZ = _REAL (Read)
*        This parameter defines the accuracy tolerance to be achieved
*        when inter-comparing pairs of input NDF data arrays to
*        determine their relative zero-points. It is only used if the
*        inter-comparison is to be performed iteratively, which will be
*        the case if both the SCALE and ZERO parameters have been set
*        to TRUE.
*
*        The value given for TOLZ specifies the tolerable absolute
*        error in the estimation of the relative zero-point between any
*        pair of input NDFs whose relative scale-factor is unity. If
*        the relative scale-factor is also being estimated, then the
*        value used is multiplied by this relative scale-factor
*        estimate (which reflects the fact that an NDF with a larger
*        data range can tolerate a larger error in estimating its
*        zero-point). The TOLS value supplied must be positive.
*        [0.05]
*     USEVAR = _LOGICAL (Read)
*        The value of this parameter specifies whether statistical
*        error (variance) information contained in the input NDFs
*        should be used to weight the input data when they are combined
*        to produce the output mosaic. This parameter is only used if
*        all the input NDFs contain variance information, in which case
*        the default behaviour is to use this information to correctly
*        weight the data values being combined. If output variances are
*        to be generated (specified by the GENVAR parameter) then this
*        parameter (and GENVAR) should be set TRUE.
*
*        If insufficient input variance information is available, or if
*        USEVAR is set to FALSE, then weights are instead derived from
*        the scale-factor corrections applied to each NDF (see the
*        WEIGHTS parameter for details); unit weight is used if no
*        scale-factor corrections are being applied. Alternatively,
*        explicit weights may be given for each input NDF via the
*        WEIGHTS parameter.
*
*        If you want to add estimated variances to the output image
*        (based on the natural variations of the input images) and all
*        your input images contain variances then you will need to set
*        this parameter FALSE (see GENVAR).
*
*        [TRUE]
*     WEIGHTS( ) = _REAL (Read)
*        A set of positive weighting factors to be used to weight the
*        input NDFs when they are combined. If this parameter is used,
*        then one value should be given for each input NDF and the
*        values should be supplied in the same order as the input NDFs.
*        If a null (!) value is given (the default) then a set of
*        weights will be generated internally - these will normally all
*        be unity unless scale-factor corrections are being applied
*        (see the SCALE parameter), in which case the reciprocal of the
*        scale factor correction for each input NDF is used as its
*        weight. This corresponds to the assumption that variance is
*        proportional to data value in each input NDF.
*
*        This parameter is only used if the USEVAR parameter is set to
*        FALSE or if one or more of the input NDFs does not contain
*        variance information. Otherwise, the input variance values are
*        used to weight the input data when they are combined.
*        [!]
*     ZERO = _LOGICAL (Read)
*        This parameter specifies whether MAKEMOS should attempt to
*        adjust the input data values by applying zero-point (i.e.
*        additive) corrections before combining them into a mosaic.
*        This would be appropriate, for instance, if a series of images
*        had been obtained with differing background (sky) values; to
*        combine them without correction would yield a mosaic with
*        discontinuities at the image edges where the data values
*        differ.
*
*        If ZERO is set to TRUE, then MAKEMOS will inter-compare the
*        NDFs supplied as input and will estimate the relative
*        zero-point difference between selected pairs of input data
*        arrays where they overlap.  From this information, a global
*        set of additive corrections will be derived which make the
*        input data as mutually consistent as possible. These
*        corrections will be applied to the input data before they are
*        combined into a mosaic.
*
*        Calculation of zero-point corrections may also be combined
*        with the use of scale-factor corrections (see the SCALE
*        parameter). By default, no zero-point corrections are applied.
*        [FALSE]

*  Examples:
*     makemos '*' mymos
*        Combines the set of NDFs matching the wild-card "*" into a
*        single mosaic called mymos. By default, no normalisation
*        corrections are applied to the input data, which are combined
*        by taking the median in regions where several input NDFs
*        overlap.
*
*     makemos in='"a,b,c,d"' out=combined zero
*        Combines the four overlapping input NDFs a, b, c and d into a
*        single mosaic called combined. Optimised zero-point
*        corrections are derived and applied to the data before
*        combining them so as to make them as mutually consistent as
*        possible. This helps to eliminate unwanted discontinuities in
*        the output mosaic.
*
*     makemos '"a,b,c,d"' out=combined scale
*        Combines the four NDFs a, b, c and d as above, but makes
*        optimised corrections to the scale factor of each (i.e.
*        multiplies each by an appropriate constant) before they are
*        combined. This would be appropriate if, for instance, the
*        input data were CCD frames acquired using different exposure
*        times and had subsequently had their sky background removed.
*
*     makemos in='frame*' out=result scale zero
*        Combines the set of input NDFs matching the wild-card "frame*"
*        into a single mosaic called result. Optimised scale factor and
*        zero point corrections are applied before combining the data.
*        This would be appropriate if, for instance, the input data had
*        been acquired using different exposure times and also had
*        different levels of sky background.
*
*     makemos in='frame*' out=result scale zero modify
*        This is identical to the previous example, except that in
*        addition to forming the output result, the MODIFY parameter
*        causes all the input NDFs to be modified using the same
*        optimised corrections as are applied when forming the mosaic,
*        thus mutually normalising all the separate NDFs.  Note that
*        this feature should be used with care, as information about
*        the original normalisation of the input data will be lost.
*        When MODIFY is specified, a null value "!" may be given for
*        the OUT parameter if an output mosaic is not actually
*        required.
*
*     makemos '"a,b,c,d"' result scale zero ref=b
*        This example merges the four input NDFs a, b, c and d into a
*        mosaic called result. In calculating the optimised scale
*        factor and zero point corrections to apply, b is regarded as a
*        "reference NDF" and the other NDFs are normalised to it. This
*        means that if b has previously been calibrated, then the
*        output mosaic will inherit this calibration.
*
*     makemos '"a,b,c,d"' result scale zero ref=e
*        This example is identical to that above, except that the
*        "reference NDF" e is not one of the input NDFs and will not
*        form part of the output mosaic. Nevertheless, the scale factor
*        and zero point corrections applied will be such that all the
*        input NDFs are normalised to it (the reference NDF must
*        overlap with at least one of the input NDFs). Thus, if e has
*        been calibrated, this calibration will be transferred to the
*        output mosaic (note that if MODIFY is specified, then the
*        calibration could also be transferred to each of the input
*        NDFs).
*
*     makemos 'frame*' mosaic nopreserve nogenvar method=minmax skysup=0
*        This example illustrates some of the less commonly used
*        MAKEMOS options. nopreserve causes the output data type to be
*        a floating point type rather than preserving the input data
*        type, nogenvar prevents generation of an output variance array
*        (possibly to save space with a large mosaic), method=minmax
*        indicates that output pixels are to be calculated by taking
*        the mean of input pixels after discarding the lowest and
*        highest values, and skysup=0 is used to disable the sky noise
*        suppression algorithm (perhaps for data which contain few sky
*        pixels).

*  Implementation Status:
*     - MAKEMOS supports "bad" pixel values and all non-complex data
*       types, with arithmetic being performed using the appropriate
*       floating point type. It can process NDFs with any number of
*       dimensions. The DATA, TITLE and VARIANCE components of an NDF
*       are directly supported, with the AXIS, HISTORY, LABEL
*       and UNITS components and all extensions being propagated from
*       the first input NDF supplied (note that AXIS values, if
*       present, will normally be extrapolated as a result of
*       propagation to the output mosaic, which will typically have a
*       larger extent than any of the input NDFs).

*  Algorithms Used:
*     Some of the algorithms used by MAKEMOS require a little
*     explanation.  The first of these is used to inter-compare
*     overlapping regions of the input NDFs to determine their relative
*     scale-factor and zero-point difference (in the most general
*     case). In effect, this algorithm has to fit a straight line to a
*     scatter plot representing the pixel values in the two overlapping
*     NDFs.
*
*     Rather than use a conventional least-squares fit for this
*     purpose, which would be sensitive to spurious data, a fit based
*     on minimisation of the sum of the absolute values of the
*     residuals is used instead. This is considerably more robust. It
*     also allows the residuals to be defined by the perpendicular
*     distance of each point from the fitted line, rather than the
*     vertical distance used in conventional least squares. In turn,
*     this removes the distinction between dependent and independent
*     variables and allows the statistical uncertainty on both axes
*     (described by an error ellipse) to be properly taken into account
*     along with other weighting factors used to implement sky noise
*     suppression.
*
*     In general, this fitting algorithm is iterative and is controlled
*     via the MAXIT, TOLS and TOLZ parameters which specify the
*     convergence criteria. However, in some important cases the fit
*     can be obtained in a single pass, with consequent savings in
*     execution time. This occurs if:
*        -  Only zero-point corrections are being determined, or
*        -  Only scale-factor corrections are being determined and no
*           input variance information is being used to weight the
*           inter-comparison process (see the CMPVAR parameter).
*
*     The second stage of normalisation involves a global optimisation
*     process which seeks to determine the best corrections to be
*     applied to each input NDF. The algorithm which performs this task
*     makes a guess at the best corrections to apply and then
*     calculates the scale-factor and/or zero-point differences which
*     would remain between each pair of overlapping NDFs if they were
*     corrected in this way. These corrections are then adjusted until
*     the weighted sum of squares of the remaining differences is
*     minimised. The weights used in this process are derived from
*     error estimates produced by the earlier (inter-comparison)
*     algorithm. This allows information about the required corrections
*     to be optimally combined from many overlaps, even in cases where
*     individual overlaps may be small and contain inadequate
*     information on their own.
*
*     The algorithm used for combining the separate input NDFs into a
*     mosaic requires no special explanation, except to note that it is
*     designed to operate on large mosaics without making excessive
*     demands on system resources such as memory. It does this by
*     partitioning the mosaic into small regions for processing.

*  Behaviour of Parameters:
*     Most parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*     The exceptions to this rule are:
*        - SKYSUP  -- dynamically defaulted
*        - GENVAR  -- dynamically defaulted
*        - SCALE   -- always FALSE
*        - ZERO    -- always FALSE
*        - MODIFY  -- always FALSE
*        - TITLE   -- always "Output from MAKEMOS"
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application but does mean
*     that additional care needs to be taken when using the application
*     on new datasets/different devices, or after a break of sometime.
*     The intrinsic default behaviour of the application may be
*     restored by using the RESET keyword on the command line.
*
*     Certain parameters (LOGTO, LOGFILE and PRESERVE) have global
*     values. These global values will always take precedence, except
*     when an assignment is made on the command line. Global values may
*     be set and reset using the CCDSETUP and CCDCLEAR commands.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council
*     Copyright (C) 1998-1999 Central Laboratory of the Research Councils

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     PDRAPER: Peter Draper (STARLINK)
*     AALLAN: Alasdair Allan (Keele University, STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-AUG-1992 (RFWS):
*        Original version.
*     20-OCT-1992 (PDRAPER):
*        Changed to CCDPACK version 0.1 standards. Modified prologue
*        to explicitly state that output variance generation (from the
*        combination stage) requires that the input variances be used
*        as (scaled) weights.
*     20-JAN-1993 (RFWS):
*        Added further workspace allocation for CCD1_DOMOS (new BADIN
*        argument).
*     13-APR-1993 (RFWS):
*        Added new NNDF argument in call to CCD1_PRUNE.
*     6-OCT-1995 (PDRAPER):
*        Removed VMS references and changed examples to show C-shell
*        usage.
*     18-SEP-1996 (PDRAPER):
*        Removed all NAG calls.
*     3-MAR-1997 (PDRAPER):
*        Removed top-level locator control (foreign data access upgrade).
*     31-JAN-1998 (PDRAPER):
*        Added clipmed combination method.
*     25-JUN-1998 (PDRAPER):
*        Stopped the propagation of quality from the first NDF to the
*        output. This was not the right thing to do when the NDFs are
*        padded to match bounds (regions of BAD quality are introduced).
*     18-NOV-1998 (PDRAPER):
*        Added fastmed combination method.
*     26-NOV-1998 (PDRAPER):
*        Now propagates NDF WCS components.
*     15-JAN-1999 (PDRAPER):
*        Added changed to support estimation of output variances from
*        input data.
*     13-SEP-1999 (AALLAN):
*        Added WRITESZ and CORRECT parameters and associated chaanges
*     31-JAN-2000 (MBT):
*        Removed WRITESZ.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     27-NOV-2000 (MBT):
*        Modified CCD1_DOMOS call due to bug fix.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'CCD1_PAR'         ! General CCDPACK constants
      INCLUDE 'CCD1_MOSPR'       ! Constants specific to MAKEMOS
      INCLUDE 'FIO_PAR'          ! FI/O constants
      INCLUDE 'FIO_ERR'          ! FI/O error codes
      INCLUDE 'PAR_ERR'          ! PAR_ error codes

*  Global Variables:
      INCLUDE 'CCD1_MOSCM'       ! Global variables for MAKEMOS
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
*        CCD1_DDIFS( CCD1__MXCMP ) = DOUBLE PRECISION (Write)
*           Array of standard errors associated with the observed scale
*           factor differences.
*        CCD1_DDIFZ( CCD1__MXCMP ) = DOUBLE PRECISION (Write)
*           Array of standard errors associated with the observed zero
*           point differences.
*        CCD1_DIFS( CCD1__MXCMP + 1 ) = DOUBLE PRECISION (Write)
*           Array of observed scale factor differences.
*        CCD1_DIFZ( CCD1__MXCMP + 1 ) = DOUBLE PRECISION (Write)
*           Array of observed zero point differences.
*        CCD1_IPAIR( 2, CCD1__MXCMP ) = INTEGER (Write)
*           Array of pairs of indices identifying which two data arrays
*           contribute to which observed scale factor and zero point
*           difference.
*        CCD1_IREF = INTEGER (Write)
*           Index of the "reference data array" to which optimised
*           scale factor and zero point corrections should be
*           normalised (set to zero if there is no reference data
*           array).

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER * ( 1 ) REFFLG   ! Character to flag reference NDF
      CHARACTER * ( 13 ) COMP    ! List of NDF components (output)
      CHARACTER * ( 6 ) ACMODE   ! NDF access mode
      CHARACTER * ( 80 ) TXT     ! Output text buffer
      CHARACTER * ( 9 ) EBUFS    ! Buffer for formatting scale error
      CHARACTER * ( 9 ) EBUFZ    ! Buffer for formatting zero error
      CHARACTER * ( 9 ) METH     ! Data combination method
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Storage type (output)
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Processing type (output)
      CHARACTER * ( FIO__SZFNM ) CORFIL ! corrections file
      DOUBLE PRECISION DSCALE( CCD1__MXNDF + 1 ) ! Scale factor error
      DOUBLE PRECISION DZERO( CCD1__MXNDF + 1 ) ! Zero point error
      DOUBLE PRECISION ORIGIN( CCD1__MXNDF + 1 ) ! False origin value
      DOUBLE PRECISION SCALE( CCD1__MXNDF + 1 ) ! Scale factor corrn.
      DOUBLE PRECISION ZERO( CCD1__MXNDF + 1 ) ! Zero point correction
      INTEGER DIMSIZ             ! Output NDF dimension size
      INTEGER FDSZ               ! File descriptor for correction file
      INTEGER I                  ! General loop counter
      INTEGER IDIM               ! Loop counter for NDF dimensions
      INTEGER IGNORE             ! I/O error status (ignored)
      INTEGER IMETH              ! Combination method
      INTEGER INGRP              ! ID for group of input NDFs
      INTEGER IREF               ! Loop counter for reference NDFs
      INTEGER IDUM               ! Dummy integer
      INTEGER LBND( NDF__MXDIM , CCD1__MXNDF ) ! NDF lower bounds
      INTEGER LBNDX( NDF__MXDIM ) ! Minimum (overall) lower bound
      INTEGER LW                 ! Size of workspace
      INTEGER MAXIT              ! Maximum number of iterations
      INTEGER NCMP               ! Number of inter-comparisons
      INTEGER NCMP0              ! Total number of NDF overlaps
      INTEGER NCMP1              ! No. of successful inter-comparisons
      INTEGER NCS                ! No. characters in scale factor error
      INTEGER NCZ                ! No. characters in zero point error
      INTEGER NDF( CCD1__MXNDF + 1 ) ! Array of input NDF identifiers
      INTEGER NDFOUT             ! Output NDF identifier
      INTEGER NDFREF             ! Reference NDF identifier
      INTEGER NDFX               ! Identifier for size of output image
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER NDIMS              ! No. of significant output dimensions
      INTEGER NDIMX              ! Maximum (overall) no. of dimensions
      INTEGER NMAX               ! Maximum possible dimension of workspace
      INTEGER NIN                ! Number of input NDFs
      INTEGER NITER              ! Number of iterations
      INTEGER NNDF               ! Number of NDFs supplied
      INTEGER NPIX( CCD1__MXCMP ) ! Number of intersecting pixels
      INTEGER NPXOUT             ! Number of pixels in output NDF
      INTEGER NVAR               ! Number of input variance arrays
      INTEGER OPTOV              ! Optimum number of NDF overlaps
      INTEGER UBND( NDF__MXDIM, CCD1__MXNDF ) ! NDF upper bounds
      INTEGER UBNDX( NDF__MXDIM ) ! Maximum (overall) upper bound
      INTEGER UNIT               ! free fortran I/O unit number
      INTEGER WRK1               ! Workspace pointer
      INTEGER WRK2               ! Workspace pointer
      INTEGER WRK3               ! Workspace pointer
      INTEGER WRK4               ! Workspace pointer
      INTEGER WRK5               ! Workspace pointer
      INTEGER WRK6               ! Workspace pointer
      INTEGER WRK7               ! Workspace pointer
      LOGICAL ADJUST             ! Apply scale/zero corrections?
      LOGICAL CMPVAR             ! Use variance in inter-comparisons?
      LOGICAL DOITER             ! Inter-comparisins are iterative?
      LOGICAL DOOUT              ! Create an output mosaic?
      LOGICAL GENVAR             ! Generate output variances?
      LOGICAL GETS               ! Make scale factor corrections?
      LOGICAL GETZ               ! Make zero point corrections?
      LOGICAL ISBAS              ! Is NDF a "base NDF"
      LOGICAL ISECT              ! NDFs intersect? (not used)
      LOGICAL LISTIN             ! Display input NDFs?
      LOGICAL MODIFY             ! Modify "input" NDFs?
      LOGICAL OPNSZ              ! Is correction file opened OK?
      LOGICAL PRESRV             ! Preserve input data type?
      LOGICAL SAME               ! NDFs are the same?
      LOGICAL USEVAR             ! Use input variance information?
      LOGICAL USEWT              ! Use user-supplied weights?
      LOGICAL VAR                ! Variance array present?
      REAL ALPHA                 ! Fraction of data to remove
      REAL NSIGMA                ! Clipping limit
      REAL RMAX                  ! Maximum data value
      REAL RMIN                  ! Minimum data value
      REAL SKYSUP                ! Sky noise suppression factor
      REAL TOLS                  ! Fraction scale factor tolerance
      REAL TOLZ                  ! Zero point tolerance
      REAL WEIGHT( CCD1__MXNDF ) ! User-supplied weights

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
*  ==========
*  Start up the logging system.
      CALL CCD1_START( 'MAKEMOS', STATUS )

*  Determine if scale factor and/or zero point corrections are to be
*  made.
      CALL PAR_GET0L( 'SCALE', GETS, STATUS )
      CALL PAR_GET0L( 'ZERO', GETZ, STATUS )

*  If corrections are required, then see whether the input NDFs are to
*  be modified and set an appropriate access mode string.
      MODIFY = .FALSE.
      ACMODE = 'READ'
      IF ( GETS .OR. GETZ ) THEN
         CALL PAR_GET0L( 'MODIFY', MODIFY, STATUS )
         IF ( MODIFY ) ACMODE = 'UPDATE'
      END IF

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDFs.
*  =====================
*  Obtain an NDF group specifying the input NDFs.
      NIN = 0
      CALL CCD1_NDFIN( 'IN', INGRP, NIN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Check that there are not too many input NDFs.
      IF ( NIN .GT. CCD1__MXNDF ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NIN', NIN )
         CALL MSG_SETI( 'MXNDF', CCD1__MXNDF )
         CALL ERR_REP( 'MAKEMOS_2MANY',
     :                 'Too many input NDFs given (^NIN) - maximum ' //
     :                 'number allowed is ^MXNDF.', STATUS )
         GO TO 99
      END IF

*  Loop to obtain an identifier for each NDF.
      NVAR = 0
      DO 3 I = 1, NIN
         CALL NDG_NDFAS( INGRP, I, ACMODE, NDF( I ), STATUS )

*  Obtain the NDF bounds.
         CALL NDF_BOUND( NDF( I ), NDF__MXDIM, LBND( 1, I ),
     :                   UBND( 1, I ), NDIM, STATUS )

*  Test to see whether the NDF contains variance information and count
*  the number which do.
         CALL NDF_STATE( NDF( I ), 'Variance', VAR, STATUS )
         IF ( VAR ) NVAR = NVAR + 1

*  If input NDFs are being modified, then check to see if any of them
*  is a section and report an error if it is.
         IF ( MODIFY ) THEN
            CALL NDF_ISBAS( NDF( I ), ISBAS, STATUS )
            IF ( ( STATUS .EQ. SAI__OK ) .AND. ( .NOT. ISBAS ) ) THEN
               STATUS = SAI__ERROR
               CALL NDF_MSG( 'SECT', NDF( I ) )
               CALL ERR_REP( 'MAKEMOS_SECT',
     :                       'An NDF section (i.e. ''^SECT'') may ' //
     :                       'not be given as input if the %MODIFY ' //
     :                       'parameter is set to TRUE.', STATUS )
               GO TO 99
            END IF
         END IF

*  Initialise scale factor and zero point corrections.
         SCALE( I ) = 1.0D0
         DSCALE( I ) = 0.0D0
         ZERO( I ) = 0.0D0
         DZERO( I ) = 0.0D0

*  Accumulate the minimum lower bound value, the maximum upper bound
*  value and the maximum number of dimensions. These determine the
*  shape of the output mosaic.
         DO 2 IDIM = 1, NDF__MXDIM
            IF ( I .EQ. 1 ) THEN
               LBNDX( IDIM ) = LBND( IDIM, I )
               UBNDX( IDIM ) = UBND( IDIM, I )
               NDIMX = NDIM
            ELSE
               LBNDX( IDIM ) = MIN( LBNDX( IDIM ), LBND( IDIM, I ) )
               UBNDX( IDIM ) = MAX( UBNDX( IDIM ), UBND( IDIM, I ) )
               NDIMX = MAX( NDIMX, NDIM )
            END IF
 2       CONTINUE
         IF ( STATUS .NE. SAI__OK ) GO TO 99
 3    CONTINUE

*  Count the number of NDFs to be considered so far.
      NNDF = NIN
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Obtain an optional reference NDF.
*  ================================
*  If correction of scale factors or zero points is required, then mark
*  the error stack and attempt to obtain a reference NDF to be used to
*  normalise the corrections.
      IF ( GETS .OR. GETZ ) THEN
         CALL ERR_MARK
         CALL CCD1_NDFAC( 'REF', 'READ', 1, 1, IDUM, NDFREF, STATUS )

*  If a null reference NDF is specified, then annul the error and set
*  CCD1_IREF to zero to indicate that there is no reference NDF.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CCD1_IREF = 0

*  Otherwise, compare the reference NDF with each of the input NDFs in
*  turn to see if they are the same. If so, set CCD1_IREF to identify
*  the reference NDF and annul the separate identifier just obtained.
         ELSE
            DO 4 IREF = 1, NIN
               CALL NDF_SAME( NDF( IREF ), NDFREF, SAME, ISECT, STATUS )
               IF ( ( STATUS .EQ. SAI__OK ) .AND. SAME ) THEN
                  CALL NDF_ANNUL( NDFREF, STATUS )
                  GO TO 5
               END IF
 4          CONTINUE
 5          CONTINUE
            CCD1_IREF = IREF

*  If the reference NDF is not amongst the input NDFs, then add it to
*  the end of the list of NDFs.
            IF ( CCD1_IREF .GT. NIN ) THEN
               NNDF = NNDF + 1
               NDF( NNDF ) = NDFREF
            END IF
         END IF

*  Release the error stack and check status.
         CALL ERR_RLSE
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Display statistics derived from the input NDFs.
*  ==============================================
*  See whether the names of the input NDFs are to be displayed.
      CALL PAR_GET0L( 'LISTIN', LISTIN, STATUS )

*  If so, display their names.
      IF ( LISTIN ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '   Input NDFs:', STATUS )
         CALL CCD1_MSG( ' ', '   ----------', STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )
         DO 6 I = 1, NNDF
            CALL MSG_SETI( 'I', I )
            CALL NDF_MSG( 'NDF', NDF( I ) )
            IF ( I .EQ. CCD1_IREF )  THEN
               IF ( NNDF .GT. NIN ) THEN
                  CALL MSG_SETC( 'NDF', ' (additional reference NDF)' )
               ELSE
                  CALL MSG_SETC( 'NDF', ' (reference NDF)' )
               END IF
            END IF
            CALL CCD1_MSG( ' ', '      ^I: ^NDF', STATUS )
 6       CONTINUE
      END IF

*  Format and display the pixel index bounds of the output mosaic.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      DO 7 IDIM = 1, NDIMX
         IF ( IDIM .NE. 1 ) CALL MSG_SETC( 'BOUNDS', ',' )
         CALL MSG_SETI( 'BOUNDS', LBNDX( IDIM ) )
         CALL MSG_SETC( 'BOUNDS', ':' )
         CALL MSG_SETI( 'BOUNDS', UBNDX( IDIM ) )
 7    CONTINUE
      CALL CCD1_MSG( ' ',
     :   '      Pixel bounds of output mosaic:      (^BOUNDS)', STATUS )

*  Also format its dimension sizes, calculating the total number of
*  output pixels in the process.
      NPXOUT = 1
      DO 8 IDIM = 1, NDIMX
         IF ( IDIM .NE. 1 ) CALL MSG_SETC( 'DIMS', 'x' )
         DIMSIZ = UBNDX( IDIM ) - LBNDX( IDIM ) + 1
         CALL MSG_SETI( 'DIMS', DIMSIZ )
         NPXOUT = NPXOUT * DIMSIZ
 8    CONTINUE

*  Display the dimension sizes and total number of output pixels.
      CALL CCD1_MSG( ' ',
     :   '      Dimension size(s) of output mosaic: ^DIMS', STATUS )
      CALL MSG_SETI( 'NPXOUT', NPXOUT )
      CALL CCD1_MSG( ' ',
     :   '      Number of output pixels:            ^NPXOUT', STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Obtain parameters to control processing.
*  =======================================
*  Note if scale factor or zero point adjustments are needed. If so,
*  see if the reference NDF (if supplied) contains a variance
*  component.
      ADJUST = ( ( GETS .OR. GETZ ) .AND. ( NNDF .GT. 1 ) )
      CMPVAR = .FALSE.
      IF ( ADJUST ) THEN
         VAR = .FALSE.
         IF ( CCD1_IREF .NE. 0 ) CALL NDF_STATE( NDF( CCD1_IREF ),
     :                                'Variance', VAR, STATUS )

*  If at least two of the NDFs supplied (including an additional
*  reference NDF, if given) contain variance information, then see if
*  variance values should be used when inter-comparing NDFs. Otherwise,
*  there is no possibility of using variance information.
         IF ( ( NVAR .GE. 2 ) .OR.
     :        ( ( NVAR .EQ. 1 ) .AND. VAR ) ) THEN
            CALL PAR_GET0L( 'CMPVAR', CMPVAR, STATUS )
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  If inter-comparisons will be made, then obtain the optimum number of
*  overlaps to be used per input NDF.
      IF ( ADJUST ) THEN
         CALL PAR_GDR0I( 'OPTOV', 3, 1, CCD1__MXNDF, .FALSE., OPTOV,
     :                   STATUS )
      END IF

*  See if any inter-comparisons to be made between the input NDFs may
*  involve iteration. If so, then obtain the required accuracy
*  tolerances and the maximum number of iterations to allow.
      TOLS = 0.0D0
      TOLZ = 0.0D0
      MAXIT = 1
      DOITER = ( ADJUST .AND. GETS .AND. ( GETZ .OR. CMPVAR ) )
      IF ( DOITER ) THEN
         IF ( GETS ) CALL PAR_GDR0R( 'TOLS', 0.001, 0.0, NUM__MAXR,
     :                               .FALSE., TOLS, STATUS )
         IF ( GETZ ) CALL PAR_GDR0R( 'TOLZ', 0.05, 0.0, NUM__MAXR,
     :                               .FALSE., TOLZ, STATUS )
         CALL PAR_GDR0I( 'MAXIT', 20, 1, NUM__MAXI, .FALSE., MAXIT,
     :                   STATUS )
      END IF

*  If scale factor adjustments will be made, then obtain the sky noise
*  suppression factor, suggesting a suitable default based on the
*  number of output dimensions with a pixel index range exceeding
*  unity.
      IF ( ADJUST .AND. GETS ) THEN
         NDIMS = 0
         DO 9 IDIM = 1, NDIMX
            IF ( UBNDX( IDIM ) .GT. LBNDX( IDIM ) ) NDIMS = NDIMS + 1
 9       CONTINUE
         CALL PAR_GDR0R( 'SKYSUP', 10.0 ** ( 0.5 * NDIMS ), 0.0,
     :                   NUM__MAXR, .FALSE., SKYSUP, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Create the output NDF.
*  =====================
*  (N.B. We do this now, because the output mosaic will often be large
*  and we want to ensure there is sufficient space available before
*  proceeding.) Create a section from the first input NDF the same
*  shape as the required output NDF.
      CALL NDF_SECT( NDF( 1 ), NDIMX, LBNDX, UBNDX, NDFX, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Mark the error stack and attempt to create the output NDF by
*  propagating from this section. Do NOT propagate QUALITY as this may
*  be padded with BAD values and only applies to the first
*  NDF. Propagate the WCS component, this should be correct for all
*  images, assuming they are aligned.
      CALL ERR_MARK
 10   CONTINUE
      DOOUT = .TRUE.
      CALL NDF_PROP( NDFX, 'Units,Axis,WCS', 'OUT', NDFOUT, STATUS )

*  If a null output NDF is given, and the input NDFs are being
*  modified, then note that no output NDF has been given, annul the
*  error and clone the section identifier (this will act as a dummy
*  output NDF, but will not be written to).
      IF ( STATUS .EQ. PAR__NULL ) THEN
         IF ( MODIFY ) THEN
            DOOUT = .FALSE.
            CALL ERR_ANNUL( STATUS )
            CALL NDF_CLONE( NDFX, NDFOUT, STATUS )

*  If the input NDFs are not being modified, then a null output NDF
*  constitutes an error. Inform the user, cancel the parameter and try
*  again to obtain an output NDF.
         ELSE
            CALL ERR_REP( 'MAKEMOS_NOOUT',
     :                    'A null output NDF should only be given ' //
     :                    'if the input NDFs are being modified. ' //
     :                    'Please respecify.', STATUS )
            CALL ERR_FLUSH( STATUS )
            CALL PAR_CANCL( 'OUT', STATUS )
            GO TO 10
         END IF

*  If an output NDF has been supplied, then obtain a title for it.
      ELSE
         CALL NDF_CINP( 'TITLE', NDFOUT, 'Title', STATUS )
      END IF
      CALL ERR_RLSE

*  Annul the temporary section identifier.
      CALL NDF_ANNUL( NDFX, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Obtain further parameters to control processing.
*  ===============================================
*  If an output NDF is to be produced, then see if variances should (a)
*  be used to weight the input NDFs when combining them and (b) be
*  generated for the output mosaic (if not enough input variances are
*  available then they will be generated, if GENVAR is true).
      USEVAR = .FALSE.
      GENVAR = .FALSE.
      IF ( DOOUT ) THEN
         IF ( NVAR .EQ. NIN ) THEN
            CALL PAR_GET0L( 'USEVAR', USEVAR, STATUS )
            GENVAR = .TRUE.
         END IF

*  GENVAR default is TRUE if all variances are available, otherwise it
*  is FALSE.
         CALL PAR_DEF0L( 'GENVAR', GENVAR, STATUS )
         CALL PAR_GET0L( 'GENVAR', GENVAR, STATUS )

*  Set up a list of NDF components for the output NDF.
         IF ( GENVAR ) THEN
            COMP = 'Data,Variance'
         ELSE
            COMP = 'Data'
         END IF

*  See if the data type of the input NDFs is to be preserved in the
*  output.
         CALL PAR_GET0L( 'PRESERVE', PRESRV, STATUS )

*  Match the numeric types of the corresponding components of the input
*  NDFs to determine the type required for the output NDF (DTYPE).
*  Allow all data types if the input type is to be preserved, otherwise
*  restrict the choice to floating point types.  Note we do not include
*  the reference NDF (if given) in this process.
         IF ( PRESRV ) THEN
            CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,' //
     :                      '_REAL,_DOUBLE', NIN, NDF, COMP, ITYPE,
     :                      DTYPE, STATUS )
         ELSE
            CALL NDF_MTYPN( '_REAL,_DOUBLE', NIN, NDF, COMP, ITYPE,
     :                      DTYPE, STATUS )
         END IF

*  Set the output NDF data type.
         CALL NDF_STYPE( DTYPE, NDFOUT, COMP, STATUS )

*  Obtain further parameters to control the algorithm for combining
*  data values from the input NDFs.
         CALL CCD1_GMDCP( USEVAR, NIN, METH, IMETH, USEWT, WEIGHT,
     :                    ALPHA, NSIGMA, NITER, RMIN, RMAX, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Determine which input NDFs to inter-compare.
*  ===========================================
*  If corrections are to be made, then allocate workspace and obtain a
*  list of pair-wise overlaps between the input NDFs.  Include an
*  additional reference NDF, if provided.
      IF ( ADJUST ) THEN
         CALL PSX_CALLOC( NNDF, '_INTEGER', WRK1, STATUS )
         CALL CCD1_GTCMP( NNDF, NDF, CCD1_IPAIR, NPIX, NCMP,
     :                    %VAL( CNF_PVAL( WRK1 ) ), STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Allocate further workspace and prune the list of overlaps to remove
*  unnecessary multiple overlaps. Deallocate the workspace when done.
            NCMP0 = NCMP
            CALL PSX_CALLOC( NCMP0, '_INTEGER', WRK2, STATUS )
            CALL CCD1_PRUNE( OPTOV, NCMP0, CCD1_IPAIR, NPIX, NNDF,
     :                       %VAL( CNF_PVAL( WRK1 ) ), NCMP,
     :                       %VAL( CNF_PVAL( WRK2 ) ), STATUS )
            CALL PSX_FREE( WRK2, STATUS )
         END IF
         CALL PSX_FREE( WRK1, STATUS )

*  Check that there is at least the minimum number of overlaps
*  required.  If not, then report an error and abort.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( NCMP .LT. NNDF - 1 ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'NCMP', NCMP )
               CALL MSG_SETI( 'NNDF', NNDF )
               CALL ERR_REP( 'MAKEMOS_OVLAP',
     :                       '^NNDF NDFs have been supplied with ' //
     :                       'only ^NCMP overlap(s) between them; ' //
     :                       'not adequate to determine the ' //
     :                       'required scale-factor/zero-point ' //
     :                       'corrections.', STATUS )
               GO TO 99
            END IF
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Display general information about the inter-comparisons to be made.
*  ==================================================================
*  Display a heading.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :   '   Parameters for NDF inter-comparisons:', STATUS )
         CALL CCD1_MSG( ' ',
     :   '   ------------------------------------', STATUS )

*  Show how many inter-comparisons will be made.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL MSG_SETI( 'NCMP0', NCMP0 )
         CALL CCD1_MSG( ' ',
     :   '      Number of NDF overlaps available:   ^NCMP0',
     :      STATUS )
         CALL MSG_SETI( 'OPTOV', OPTOV )
         CALL CCD1_MSG( ' ',
     :   '      Optimum no. of overlaps per NDF:    ^OPTOV',
     :      STATUS )
         CALL MSG_SETI( 'NCMP', NCMP )
         CALL CCD1_MSG( ' ',
     :   '      No. inter-comparisons to be made:   ^NCMP',
     :      STATUS )

*  Display the function being fitted and the parameter(s) being
*  estimated.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         IF ( GETS .AND. GETZ ) THEN
            CALL CCD1_MSG( ' ',
     :   '      Function being fitted:              a = b * S + Z',
     :         STATUS )
            CALL CCD1_MSG( ' ',
     :   '      Parameters being estimated:         Scale-factor (S)',
     :         STATUS )
            CALL CCD1_MSG( ' ',
     :   '                                          Zero-point (Z)',
     :         STATUS )
         ELSE IF ( GETS ) THEN
            CALL CCD1_MSG( ' ',
     :   '      Function being fitted:              a = b * S',
     :         STATUS )
            CALL CCD1_MSG( ' ',
     :   '      Parameter being estimated:          Scale-factor (S)',
     :         STATUS )
         ELSE IF ( GETZ ) THEN
            CALL CCD1_MSG( ' ',
     :   '      Function being fitted:              a = b + Z',
     :         STATUS )
            CALL CCD1_MSG( ' ',
     :   '      Parameter being estimated:          Zero-point (Z)',
     :         STATUS )
         END IF

*  If iterations may be needed during the inter-comparison, then show
*  the parameter values controlling the iterations.
         IF ( DOITER ) THEN
            IF ( NVAR .EQ. NNDF ) THEN
               CALL CCD1_MSG( ' ',
     :   '      Estimation will be iterative', STATUS )
            ELSE
               CALL CCD1_MSG( ' ',
     :   '      Estimation may be iterative', STATUS )
            END IF
            CALL CCD1_MSG( ' ',
     :   '         Convergence criteria:', STATUS )
            IF ( GETS ) THEN
               CALL MSG_SETR( 'TOLS', ABS( TOLS ) )
               CALL CCD1_MSG( ' ',
     :   '            Scale-factor tolerance:       ^TOLS * S', STATUS )
            END IF
            IF ( GETZ ) THEN
               CALL MSG_SETR( 'TOLZ', ABS( TOLZ ) )
               CALL CCD1_MSG( ' ',
     :   '            Zero-point tolerance:         ^TOLZ * S', STATUS )
            END IF
            CALL MSG_SETI( 'MAXIT', MAXIT )
            CALL CCD1_MSG( ' ',
     :   '         Maximum number of iterations:    ^MAXIT', STATUS )
         END IF

*  Display the sky noise suppression factor, if appropriate.
         IF ( GETS ) THEN
            IF ( SKYSUP .GT. 0.0 ) THEN
               CALL MSG_SETR( 'SKYSUP', SKYSUP )
            ELSE
               CALL MSG_SETC( 'SKYSUP', 'Not in use' )
            END IF
            CALL CCD1_MSG( ' ',
     :   '      Sky noise suppression factor:       ^SKYSUP', STATUS )
         END IF

*  Display the name of the reference NDF, if supplied.
         IF ( CCD1_IREF .NE. 0 ) THEN
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL NDF_MSG( 'NDFREF', NDF( CCD1_IREF ) )
            IF ( NNDF .GT. NIN ) THEN
               CALL MSG_SETC( 'STATE', 'not an input NDF' )
            ELSE
               CALL MSG_SETC( 'STATE', 'also an input NDF' )
            END IF
            CALL CCD1_MSG( ' ',
     :   '      Reference NDF: ^NDFREF (^STATE)', STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Inter-compare the NDFs in pairs to determine their zero point and/or
*  scale factor differences. Update the number of inter-comparisons to
*  exclude any that failed.
         CALL CCD1_DOCMP( GETS, GETZ, CMPVAR, SKYSUP, TOLS, TOLZ,
     :                    MAXIT, NNDF, NDF, CCD1_IREF, NCMP, CCD1_IPAIR,
     :                    NPIX, CCD1_DIFS, CCD1_DDIFS, CCD1_DIFZ,
     :                    CCD1_DDIFZ, CCD1_ORIG, NCMP1, STATUS )
         NCMP = NCMP1
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Find the optimised corrections.
*  ==============================
*  Display a heading.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '   Solving for optimum corrections:',
     :                 STATUS )
         CALL CCD1_MSG( ' ', '   -------------------------------',
     :                 STATUS )

*  Display the correction formula and headings for the table of
*  results. The form of this depends on which corrections are being
*  applied.
         CALL CCD1_MSG( ' ', ' ', STATUS )

*  Both scale factor and zero point corrections:
         IF ( GETS .AND. GETZ ) THEN
            CALL CCD1_MSG( ' ',
     : '      Correction formula is: new = old * S + Z', STATUS )
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL CCD1_MSG( ' ',
     : '                           Scale-Factor    ' //
     : '           Zero-Point', STATUS )
            CALL CCD1_MSG( ' ',
     : '         NDF             Correction S (dS)' //
     : '         Correction Z (dZ)', STATUS )
            CALL CCD1_MSG( ' ',
     : '         ---             -----------------' //
     : '         -----------------', STATUS )

*  Scale factor corrections only:
         ELSE IF ( GETS ) THEN
            CALL CCD1_MSG( ' ',
     : '      Correction formula is: new = old * S', STATUS )
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL CCD1_MSG( ' ',
     : '                                         Scale-Factor', STATUS )
            CALL CCD1_MSG( ' ',
     : '                   NDF                 Correction S (dS)',
     :         STATUS )
            CALL CCD1_MSG( ' ',
     : '                   ---                 -----------------',
     :         STATUS )

*  Zero point corrections only:
         ELSE IF ( GETZ ) THEN
            CALL CCD1_MSG( ' ',
     : '      Correction formula is: new = old + Z', STATUS )
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL CCD1_MSG( ' ',
     : '                                       Zero-Point', STATUS )
            CALL CCD1_MSG( ' ',
     : '                   NDF              Correction Z (dZ)',
     :         STATUS )
            CALL CCD1_MSG( ' ',
     : '                   ---              -----------------',
     :         STATUS )
         END IF

*  Allocate workspace.
         IF ( GETS .AND. GETZ ) THEN
            NMAX = MAX( 2 * ( NNDF + 1 ), 2 * ( NCMP + 1 ) )
            CALL PSX_CALLOC( NMAX, '_DOUBLE', WRK1, STATUS )
            LW = 17 * NMAX + 40
            CALL PSX_CALLOC( LW, '_INTEGER', WRK2, STATUS )
            LW = ( NMAX + 2 ) * ( NMAX + 2 )
            CALL PSX_CALLOC( LW, '_DOUBLE', WRK3, STATUS )
            LW = 4 * NMAX * NMAX + 86 * NMAX + 141
            CALL PSX_CALLOC( LW, '_DOUBLE', WRK4, STATUS )
            LW = NMAX * NMAX
            CALL PSX_CALLOC( LW, '_DOUBLE', WRK5, STATUS )
         ELSE
            NMAX = 1
            CALL PSX_CALLOC( 2 * ( NNDF + 1 ), '_DOUBLE', WRK1, STATUS )
            WRK2 = 0
            WRK3 = 0
            WRK4 = 0
            WRK5 = 0
         END IF

*  Solve to find globally optimised corrections for each NDF consistent
*  with the inter-comparison results obtained above. Include an
*  additional reference NDF, if provided.
         CALL CCD1_SZSLV( GETS, GETZ, NNDF, NCMP, NMAX, SCALE, DSCALE,
     :                    ZERO, DZERO, ORIGIN, %VAL( CNF_PVAL( WRK1 ) ),
     :                    %VAL( CNF_PVAL( WRK2 ) ),
     :                    %VAL( CNF_PVAL( WRK3 ) ),
     :                    %VAL( CNF_PVAL( WRK4 ) ),
     :                    %VAL( CNF_PVAL( WRK5 ) ), STATUS )

*  Release the workspace.
         CALL PSX_FREE( WRK1, STATUS )
         IF ( GETS .AND. GETZ ) THEN
            CALL PSX_FREE( WRK2, STATUS )
            CALL PSX_FREE( WRK3, STATUS )
            CALL PSX_FREE( WRK4, STATUS )
            CALL PSX_FREE( WRK5, STATUS )
         END IF

*  Loop to display the optimised results. Format the error estimate(s).
         DO 11 I = 1, NNDF
            IF ( GETS ) THEN
               WRITE( EBUFS, '(G9.2)', IOSTAT = IGNORE )
     :            SNGL( DSCALE( I ) )
               CALL CHR_LDBLK( EBUFS )
               NCS = CHR_LEN( EBUFS )
            END IF
            IF ( GETZ ) THEN
               WRITE( EBUFZ, '(G9.2)', IOSTAT = IGNORE )
     :            SNGL( DZERO( I ) )
               CALL CHR_LDBLK( EBUFZ )
               NCZ = CHR_LEN( EBUFZ )
            END IF

*  Note if this is the reference NDF.
            REFFLG = ' '
            IF ( I .EQ. CCD1_IREF ) REFFLG = '*'

*  Display the results using an appropriate format.
            TXT = ' '
            IF ( GETS .AND. GETZ ) THEN
               WRITE( TXT, 1001, IOSTAT = IGNORE ) I, REFFLG,
     :         SNGL( SCALE( I ) ), EBUFS( : NCS ),
     :         SNGL( ZERO( I ) - SCALE( I ) * ORIGIN( I ) ),
     :         EBUFZ( : NCZ )
            ELSE IF ( GETS ) THEN
               WRITE( TXT, 1002, IOSTAT = IGNORE ) I, REFFLG,
     :         SNGL( SCALE( I ) ), EBUFS( : NCS )
            ELSE IF ( GETZ ) THEN
               WRITE( TXT, 1002, IOSTAT = IGNORE ) I, REFFLG,
     :         SNGL( ZERO( I ) - SCALE( I ) * ORIGIN( I ) ),
     :         EBUFZ( : NCZ )
            END IF
            CALL CCD1_MSG( ' ', TXT, STATUS )
 1001       FORMAT( I11, A1, T22, G12.5, ' (', A, ')',
     :                       T51, G12.5, ' (', A, ')' )
 1002       FORMAT( T11, I11, A1, T36, G12.5, ' (', A, ')' )
 11      CONTINUE

*  Add a footnote identifying the reference NDF if necessary.
         IF ( CCD1_IREF .NE. 0 ) THEN
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL CCD1_MSG( ' ',
     : '                              * = Reference NDF', STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 99
      END IF

*  If we want the SCALE and ZERO point information to be written to a
*  file then we do so here -- this file can be input into the DRIZZLE
*  program via the CORRECT parameter.
      IF ( GETS .OR. GETZ ) THEN

*  Get file for output.
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         CALL CCD1_ASFIO( 'CORRECT', 'WRITE', 'LIST', 0, FDSZ, OPNSZ,
     :                    STATUS )
         IF ( STATUS .EQ. PAR__ABORT ) THEN
            GO TO 99
         ELSE IF ( STATUS .NE. SAI__OK ) THEN

*  File open failed - warn user.
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL CCD1_MSG( ' ',
     :      'WARNING: Correction file could not be opened', STATUS )
            CALL CCD1_MSG( ' ',
     :      '         Scale factors and zero points will not be output',
     :                     STATUS )
            CALL ERR_ANNUL( STATUS )

         ELSE IF ( OPNSZ ) THEN

*  File opened successfully.  Get a unit number.
            CALL FIO_UNIT( FDSZ, UNIT, STATUS )

*  Write to file.
            WRITE( UNIT, '(A)' ) '#'
            DO I = 1, NIN
               IF ( GETS .AND. GETZ ) THEN
                  WRITE( UNIT, * ) I, SCALE( I ),
     :                             ZERO( I ) - SCALE( I ) * ORIGIN( I )
               ELSE IF ( .NOT. GETS ) THEN
                  WRITE( UNIT, * ) I, 1.0D0,
     :                             ZERO( I ) - SCALE( I ) * ORIGIN( I )
               ELSE IF( .NOT. GETZ ) THEN
                  WRITE( UNIT, * ) I, SCALE( I ), 0.0D0
               ENDIF
            END DO
            CALL FIO_CLOSE( FDSZ, STATUS )
         END IF

      ENDIF

*  Display information about the output mosaic.
*  ===========================================
*  If an output mosaic is being generated, then display a heading.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      IF ( DOOUT ) THEN
         CALL CCD1_MSG( ' ', '   Generating output mosaic:', STATUS )
         CALL CCD1_MSG( ' ', '   ------------------------', STATUS )

*  Display the output data type and the data combination method.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL MSG_SETC( 'TYPE', DTYPE )
         CALL CCD1_MSG( ' ',
     :   '      Output data type:                   ^TYPE', STATUS )
         CALL MSG_SETC( 'METH', METH )
         CALL CCD1_MSG( ' ',
     :   '      Data combination method:            ^METH', STATUS )

*  Display the parameters which affect the selected data combination
*  method...

*  Trimmed mean: display the trimming fraction.
         IF ( METH .EQ. 'TRIMMED' ) THEN
            CALL MSG_SETR( 'ALPHA', ALPHA )
            CALL CCD1_MSG( ' ',
     :   '         Trimming fraction (alpha):       ^ALPHA', STATUS )

*  Sigma clipping: display the number of standard deviations to clip
*  at.
         ELSE IF ( ( METH .EQ. 'MODE' ) .OR.
     :             ( METH .EQ. 'SIGMA' ) ) THEN
            CALL MSG_SETR( 'NSIGMA', NSIGMA )
            CALL CCD1_MSG( ' ',
     :   '         No. std. deviations to clip at:  ^NSIGMA', STATUS )

*  Mode: display the maximum number of iterations.
            IF ( METH .EQ. 'MODE' ) THEN
               CALL MSG_SETI( 'NITER', NITER )
               CALL CCD1_MSG( ' ',
     :   '         No. of clipping iterations:      ^NITER', STATUS )
            END IF

*  Threshold cuts: display the limits.
         ELSE IF ( METH .EQ. 'THRESHOLD' ) THEN
            CALL MSG_SETR( 'RMIN', RMIN )
            CALL CCD1_MSG( ' ',
     :   '         Minimum data value to use:       ^RMIN', STATUS )
            CALL MSG_SETR( 'RMAX', RMAX )
            CALL CCD1_MSG( ' ',
     :   '         Maximum data value to use:       ^RMAX', STATUS )
         END IF

*  If the data combination method uses weights, then show where they
*  will be obtained from.
         IF ( ( METH .EQ. 'MEAN' ) .OR.
     :        ( METH .EQ. 'MEDIAN' ) .OR.
     :        ( METH .EQ. 'MODE' ) .OR.
     :        ( METH .EQ. 'SIGMA' ) .OR.
     :        ( METH .EQ. 'THRESHOLD' ) .OR.
     :        ( METH .EQ. 'MINMAX' ) ) THEN
            IF ( USEVAR ) THEN
               CALL MSG_SETC( 'WEIGHT', 'Input variance information' )
            ELSE IF ( USEWT ) THEN
               CALL MSG_SETC( 'WEIGHT', 'Explicit weights provided' )
            ELSE IF ( GETS ) THEN
               CALL MSG_SETC( 'WEIGHT', 'Scale factor corrections' )
            ELSE
               CALL MSG_SETC( 'WEIGHT', 'Equal unit weights' )
            END IF
            CALL CCD1_MSG( ' ',
     :   '      Source of data weighting factors:   ^WEIGHT', STATUS )
         END IF

*  Show which corrections are being applied.
         IF ( GETS .AND. GETZ ) THEN
            CALL MSG_SETC( 'CORRNS', 'Scale-factors and zero-points' )
         ELSE IF ( GETS ) THEN
            CALL MSG_SETC( 'CORRNS', 'Scale-factors' )
         ELSE IF ( GETZ ) THEN
            CALL MSG_SETC( 'CORRNS', 'Zero-points' )
         ELSE
            CALL MSG_SETC( 'CORRNS', 'None' )
         END IF
         CALL CCD1_MSG( ' ',
     :   '      Corrections being applied:          ^CORRNS', STATUS )

*  Show whether output variances are being generated.
         CALL MSG_SETL( 'GENVAR', GENVAR )
         CALL CCD1_MSG( ' ',
     :   '      Output variances being generated:   ^GENVAR', STATUS )

*  Say if the input NDFs are being modified.
         CALL MSG_SETL( 'MODIFY', MODIFY )
         CALL CCD1_MSG( ' ',
     :   '      Input NDFs being modified:          ^MODIFY', STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )

*  If no output NDF is being created, then indicate that the input NDFs
*  are being corrected instead.
      ELSE
         CALL CCD1_MSG( ' ', '   Modifying input NDFs:', STATUS )
         CALL CCD1_MSG( ' ', '   --------------------', STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :   '      No output mosaic is being generated', STATUS )
      END IF

*  Generate the output mosaic (and/or modify the input NDFs).
*  ==========================
*  Allocate workspace for forming the output mosaic.
      CALL PSX_CALLOC( 2 * NIN, '_LOGICAL', WRK1, STATUS )
      IF ( MODIFY .AND. ADJUST ) CALL PSX_CALLOC( 2 * NIN, '_LOGICAL',
     :                                            WRK2, STATUS )
      CALL PSX_CALLOC( 2 * ( NIN + 1 ), '_INTEGER', WRK3, STATUS )
      CALL PSX_CALLOC( 2 * NIN, '_INTEGER', WRK4, STATUS )
      CALL PSX_CALLOC( 2 * NIN, '_INTEGER', WRK5, STATUS )
      CALL PSX_CALLOC( NIN, '_DOUBLE', WRK6, STATUS )
      CALL PSX_CALLOC( NIN, '_DOUBLE', WRK7, STATUS )

*  Generate the mosaic and/or correct the input NDFs.
      CALL CCD1_DOMOS( NIN, NDF, LBND, UBND, ADJUST, SCALE, DSCALE,
     :                 ZERO, DZERO, ORIGIN, MODIFY, DOOUT, USEVAR,
     :                 GENVAR, USEWT, WEIGHT, IMETH, ALPHA, NSIGMA,
     :                 NITER, RMIN, RMAX, NDFOUT,
     :                 %VAL( CNF_PVAL( WRK1 ) ),
     :                 %VAL( CNF_PVAL( WRK2 ) ),
     :                 %VAL( CNF_PVAL( WRK3 ) ),
     :                 %VAL( CNF_PVAL( WRK4 ) ),
     :                 %VAL( CNF_PVAL( WRK5 ) ),
     :                 %VAL( CNF_PVAL( WRK6 ) ),
     :                 %VAL( CNF_PVAL( WRK7 ) ),
     :                 STATUS )

*  Release the workspace.
      CALL PSX_FREE( WRK1, STATUS )
      IF ( MODIFY .AND. ADJUST ) CALL PSX_FREE( WRK2, STATUS )
      CALL PSX_FREE( WRK3, STATUS )
      CALL PSX_FREE( WRK4, STATUS )
      CALL PSX_FREE( WRK5, STATUS )
      CALL PSX_FREE( WRK6, STATUS )
      CALL PSX_FREE( WRK7, STATUS )

*  If appropriate, say where the output mosaic has been written.
      IF ( DOOUT ) THEN
         CALL NDF_MSG( 'NDF', NDFOUT )
         CALL CCD1_MSG( ' ',
     :   '      Output mosaic written to NDF structure ^NDF', STATUS )
      END IF

*  Clean up.
*  ========
*  Arrive here if an error occurs. End the outer NDF context, thus
*  cleaning up all the NDF resources used.
 99   CONTINUE
      CALL NDF_END( STATUS )

*  Annul the input NDF group.
      CALL CCD1_GRDEL( INGRP, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'MAKEMOS_ERR',
     :   'MAKEMOS: Error producing a mosaic from a set of NDFs.',
     :      STATUS )
      END IF

*  Close the logging system.
      CALL CCD1_END( STATUS )

      END
* $Id$
