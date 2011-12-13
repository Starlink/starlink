      SUBROUTINE DEBIAS( STATUS )
*+
*  Name:
*     DEBIAS

*  Purpose:
*     Performs the debiassing and initial preparation of CCD
*     data.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DEBIAS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine debiasses CCD frames, masks defects, sets variances,
*     corrects for CCD gain and deferred charge, sets saturated values
*     and extracts the useful portion of the CCD data.
*
*     The debiassing section operates in two basic modes -- with and
*     without a bias frame. If a bias frame is supplied then it is
*     subtracted from the data arrays of the input NDFs. The subtraction
*     can either be direct, or by offsetting the values of the bias by
*     the mean value in the bias-strip region(s). When determining
*     the mean in the bias strips a function of the distance from the
*     edges is used, this reduces the effect of any contamination. If
*     you are offsetting to the bias strip mean then the bias frame
*     should be averaged to zero (MAKEBIAS does this).
*
*     The second debiassing method which DEBIAS supports is the
*     subtraction of interpolated values. The interpolation is
*     performed between the bias strips. If only one strip is given the
*     interpolation is really an extrapolation and is limited to
*     constant values either for each line or for the frame as a whole.
*     Interpolation between bias strips can be as for a single strip or
*     may be a straight line fit for each line, or a fit of a plane to
*     the bias strips (see parameter SMODE). The interpolation uses
*     weighting operations as for bias frame subtraction. Bad values
*     can also be rejected from the strips by sigma clipping, or the
*     noise can be reduced by smoothing the values.
*
*     Additional DEBIAS functionality includes the (optional)
*     production of variance estimates for the input CCD data. It does
*     this by assuming Poissonian statistics for the bias-subtracted
*     data, together with a contribution for the readout noise. The
*     masking of bad data areas is achieved using the transfer of
*     quality information from an NDF, or by using an ASCII Regions
*     Definition (ARD) file. The expansion of the data values into
*     counts and the extraction of the useful area of the CCD are also
*     performed.

*  Usage:
*     debias in out bias [bounds] rnoise adc [mask]

*  ADAM Parameters:
*     ADC = _DOUBLE (Read)
*        The Analogue-to-Digital Conversion factor. This number converts
*        input ADUs to detected electrons. This value is used to
*        estimate the Poissonian noise in the output (debiassed) data
*        values.  If the EXPAND parameter is true, then the output is
*        multiplied by ADC so that the output is in counts (electrons)
*        rather than ADUs.  If variances are not being generated
*        then this value will not be used.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then this will be used.  If USESET is true then a
*        value specific to the Set Index of each image will be sought.
*        [1.0]
*     BADBITS = _INTEGER (Read)
*        If the first input NDF has no quality component, and you have
*        specified the SETBAD= FALSE option, you will be requested to
*        supply a value for BADBITS (SUN/33).  The default for this
*        is 1. BADBITS is a byte value and hence can only be in the
*        range 0-255.
*        [1]
*     BIAS = LITERAL (Read)
*        Name of the NDF which contains the bias calibration data. This
*        parameter may be specified as ! in which case either a
*        constant or values derived from the bias strip(s) are used.
*        The name of this file may be specified using indirection
*        through an ASCII file. The offered default is either the last
*        used master bias name or (if one exists) the name of the NDF
*        produced by the last run of MAKEBIAS.
*
*        If USESET is true and you are using bias calibration data
*        from a file, BIAS should be a group expression referring
*        to one master bias frame matching each of the Set Index
*        attributes represented in the IN list; again the name of
*        the file produced by MAKEBIAS will normally be suitable.
*        [Global master bias or !]
*     BOUNDS( 2 or 4 ) = _INTEGER (Read)
*        The pixel indices (see notes) of the upper and lower bounds of
*        the bias strip(s). These bounds can run in either the
*        horizontal or vertical directions. The direction is controlled
*        by the DIRECTION parameter. The bounds must be supplied in
*        pairs. Pixel indices are the actual number of pixels, starting
*        at 1,1 at the lower left hand corner of the NDF data array,
*        which includes any origin offsets within the input NDFs.
*
*        If global values for these bounds have been set using
*        CCDSETUP then those values will be used.  If USESET is true
*        then a value specific to the Set Index of each image will
*        be sought.
*     BOXSIZE( 2 ) = _INTEGER (Read)
*        The sizes of the sides of the box to be used when smoothing the
*        bias strips. Only used when CMODE="BOX".
*        [15,15]
*     CMODE = LITERAL (Read)
*        The "clean-up" mode for the bias strips. This parameter may
*        take values of "BOX", "SIGMA" or "WEIGHT". If CMODE="BOX" then
*        the bias strips are smoothed with a box filter before being
*        processed. If CMODE="SIGMA" then the bias strips are sigma
*        clipped before being processed. If CMODE="WEIGHT" then only
*        the weighting as indicated by the WMODE parameter is used to
*        attempt to decrease the effects of erroneous pixel values.
*        [BOX]
*     DEFERRED = _DOUBLE (Read)
*        The deferred charge value. This is also often known as the
*        "fat" or "skinny" zero. It represents the amount of charge
*        left behind in a pixel on a readout transfer. This value is
*        subtracted from the data.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then this will be used.  If USESET is true then a
*        value specific to the Set Index of each image will be sought.
*        [0.0]
*     DIRECTION = LITERAL (Read)
*        The readout direction of the CCD. This parameter can take
*        values of "X" or "Y". X indicates that the readout direction is
*        horizontal , Y indicates that the readout direction is
*        vertical. The BOUNDS parameter values are assumed to be values
*        along the readout direction.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then this will be used.  If USESET is true then a
*        value specific to the Set Index of each image will be sought.
*        [X]
*     EXPAND = _LOGICAL (Read)
*        This value controls whether or not the output data should be
*        multiplied by the ADC factor to convert the input ADUs to
*        counts (electrons).  The output variance is affected accordingly
*        (multiplied by ADC**2).  This option is disabled if no variances
*        are generated. Care should be taken when using this option
*        with a large ADC factor and data types of _WORD,_UWORD,_BYTE
*        or _UBYTE as the output data range may exceed that allowed
*        with these types. In this case the best option is to set
*        the PRESERVE parameter FALSE.
*
*        [Default is TRUE if input data is not an unsigned data type
*        otherwise FALSE.]
*     EXTENT(4) = _INTEGER (Read)
*        The extent of the useful CCD area. This should be given in
*        pixel index values (see notes). The extent is restricted to
*        that of the CCD frame, so no padding of the data can occur. If
*        values outside of those permissable are given then they are
*        modified to lie within the CCD frame. The values should be
*        given in the order XMIN,XMAX,YMIN,YMAX.
*
*        Normally the extent should be set so that the bias strips
*        are excluded from the output data, this is essential for
*        flatfields whose normalisation could be adversely biased.
*
*        If global values for these bounds have been set using
*        CCDSETUP then those values will be used.  If USESET is true then a
*        value specific to the Set Index of each image will be sought.
*     FIXORIGIN = _LOGICAL (Read)
*        Whether to fix the origins of the output NDFs to 1,1, rather
*        than the lower corner as defined by the EXTENT parameter.
*        This option is of particular use if the analysis package
*        you are going to use does not support origins.
*        [FALSE]
*     FMODE = LITERAL (Read)
*        The fit mode which will be used when interpolating bias values.
*        May take values of "LINE" or "PLANE". This is used together
*        with the SMODE parameter to define the interpolation method,
*        ie. FMODE="LINE", SMODE="LINEAR", fits each row or column of
*        the bias strips by a straight line; FMODE="PLANE",
*        SMODE="CONSTANT" derives a single constant for the bias value;
*        FMODE="PLANE", SMODE="LINEAR" fits a plane to the bias-strip
*        data.
*        [LINE]
*     GENVAR = _LOGICAL (Read)
*        If variances are to be generated then this value is set
*        TRUE. If variances are not to be generated then this value
*        should be set FALSE. Normally variances should be generated,
*        even though disk and process time savings can be made by their
*        omission.
*
*        If a global value has been set up using CCDSETUP this value
*        will be used.
*        [FALSE]
*     GETBIAS = _LOGICAL (Read)
*        This parameter controls whether or not an attempt is to be made
*        to access a master bias NDF.
*        [TRUE]
*     GETMASK = _LOGICAL (Read)
*        This parameter controls whether or not an attempt is to be made
*        to access a defect mask using the parameter MASK.
*        [TRUE]
*     IN = LITERAL (Read)
*        A list of the names of the NDFs which contain the raw CCD
*        data. Note that at present the input data must have a common
*        processing mode, i.e. have the same ADC factor, readout noise
*        etc. These values are represented by the parameter values of
*        the task.  The input data must also use the same master bias
*        frame except if USESET is true and the input and bias images
*        contain suitable CCDPACK Set header information, in which
*        case each input image will be processed using the bias image
*        with the corresponding Set Index attribute.
*
*        The NDF names should be separated by commas and may include
*        wildcards.
*     KEEPIN = _LOGICAL (Read)
*        Whether to keep (i.e. not delete) the input NDFs (parameter IN)
*        or not. Deleting the input NDFs has the advantage of saving
*        disk space, but should probably only be used if this program
*        is part of a sequence of commands and the intermediary data
*        produced by it are not important.
*
*        The calibration master frames (parameters BIAS and possibly MASK)
*        are never deleted.
*
*        The default for this parameter is TRUE and this cannot be
*        overridden except by assignment on the command line or in
*        response to a forced prompt.
*        [TRUE]
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter then no logfile will be written, regardless of
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
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     MASK = LITERAL (Read)
*        The name of an NDF or ASCII Regions Definition (ARD) file.
*
*        If an NDF is given then any regions of BAD values (set through
*        explicit BAD values or by BADBITS in the quality component)
*        will be transferred to the output NDF.
*
*        If an ARD file is given then its regions will be interpreted
*        and transferred to the output NDF. ARD is described in its own
*        section.
*
*        The regions whose quality is to be set are probably hot spots,
*        line defects etc.  which contain little or no useful
*        information. This parameters may be returned as ! indicating
*        that no mask is to be applied.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then this will be used.   If USESET is true then a
*        value specific to the Set Index of each image will be sought.
*
*        The name of this file may be specified using indirection
*        through a file.
*        [!]
*     NSIGMA = _REAL (Read)
*        The number of standard deviations to clip the bias strips at.
*        This is only used in CMODE="SIGMA". The actual clipping
*        occurs at NSIGMA*RNOISE. If no variances are being generated
*        then the RNOISE value is estimated from the data values in
*        the strips.
*        [4.0]
*     OFFSET = _LOGICAL (Read)
*        If TRUE then the input bias data array is offset
*        by the mean value derived from the bias-strip areas. If FALSE
*        then the bias data is directly subtracted. This parameter is
*        disabled for unsigned data types as the bias data cannot have
*        been previously zeroed.
*        [TRUE]
*     OUT = LITERAL (Write)
*        Names of the output NDFs. These may be specified as list of
*        comma separated names, using indirection if required, OR,
*        as a single modification element (of the input names). The
*        simplest modification element is the asterisk "*" which means
*        call each of the output NDFs the same name as the
*        corresponding input NDFs. So,
*           IN > *
*           OUT > *
*        signifies that all the NDFs in the current directory should be
*        used and the output NDFs should have the same names.
*
*        Other types of modification can also occur, such as,
*           OUT > tmp_*
*        which means call the output NDFs the same as the input NDFs but
*        put tmp_ in front of the names. Replacement of a specified
*        string with another in the output file names can also be used,
*           OUT > tmp_*|debias|flattened|
*        this replaces the string debias with flattened in any of the
*        output names tmp_*.
*     PRESERVE = _LOGICAL (Read)
*        If TRUE then the data type of the input NDFs are used for
*        processing and are preserved on exit from this routine. If
*        FALSE then a suitable floating point type will be chosen for
*        the output type and the processing will be performed using
*        this choice.
*
*        This option should be used when a unacceptable loss of
*        accuracy may occur, or when the data range can no longer be
*        represented in the range of the present data type. The latter
*        effect may occur when expanding input ADU values into
*        electrons, if the ADC factor is large and the input data have
*        types of _WORD,_UWORD,_BYTE or _UBYTE.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then this will be used.
*        [TRUE]
*     RNOISE = _DOUBLE (Read)
*        The readout noise in input data units (ADUs). An estimate of
*        the readout noise is shown for unweighted values in the bias
*        strips, if the bias strips are used. If variances are not
*        generated then this value is not used. If variances are
*        generated then the readout noise is included in the variance
*        estimates.
*
*        If a global value has been set using CCDSETUP then this will
*        be used.   If USESET is true then a value specific to the
*        Set Index of each image will be sought.
*        [Dynamic default or 1.0]
*     SATURATE = _LOGICAL (Read)
*        This parameter controls whether the data are to be processed to
*        detect saturated values or not. The actual saturation value is
*        given using the SATURATION parameter.
*        [FALSE]
*     SATURATION = _DOUBLE (Read)
*        The data saturation value. Only used if SATURATE is TRUE.
*
*        If a global value has been set using CCDSETUP then this will
*        be used.  If USESET is true then a value specific to the
*        Set Index of each image will be sought.
*        [1.0D6]
*     SETBAD = _LOGICAL (Read)
*        If TRUE then the quality information will be transferred
*        from the MASK NDF to the output NDFs in the form of BAD
*        ("flagged") values in the data component. This is the usual
*        method of indicating the presence of pixels with no value. If
*        FALSE then the quality information will be transferred into
*        the quality component, all output quality pixels will have
*        their BADBITS set. (Note that if the input NDF already has a
*        quality component the BADBITS will be set by a logical OR of
*        the current bits with the BADBITS value).
*        [TRUE]
*     SETSAT = _LOGICAL (Read)
*        This parameter controls how saturated data will be flagged.
*        If it is set TRUE then saturated values will be replaced by
*        the value of the parameter SATURATION (which is also the value
*        used to detect saturated data). If it is FALSE then saturated
*        values will be set to BAD (also known as invalid).
*        [FALSE]
*     SMODE = LITERAL (Read)
*        The mode which will be used to perform any interpolation fit
*        between the bias strips. Can take values of "CONSTANT" or
*        "LINEAR". If only one bias strip is given this may only take
*        the value "CONSTANT". This is used together with the FMODE
*        parameter to define the interpolation method, i.e.
*        FMODE="LINE", SMODE="LINEAR", fits each row or column of the
*        bias strips by a straight line; FMODE="PLANE",
*        SMODE="CONSTANT" derives a single constant for the bias value;
*        FMODE="PLANE", SMODE="LINEAR" fits a plane to the bias-strip
*        data.
*        [CONSTANT]
*     TITLE = LITERAL (Read)
*        Title for the output NDF.
*        [Output from DEBIAS]
*     USECON = _LOGICAL (Read)
*        If TRUE then you can supply an estimate for the bias
*        contribution (parameter ZERO). This value is then subtracted
*        from the input NDF. Only use this option if you do not have
*        any bias frames or bias strips and you have good reason to
*        believe that the value you are supplying is accurate enough
*        for your purposes.
*        [FALSE]
*     USEEXT = _LOGICAL (Read)
*        If TRUE then certain of the parameters of this program will not
*        be used and the required values will be obtained from the
*        CCDPACK extensions of the input NDFs instead. This method can
*        only be used if the NDFs have been "imported" using the
*        programs PRESENT or IMPORT. Typically it is used when
*        processing using CCDPACK's "automated" methods (in this case
*        the input NDFs should contain all the information necessary to
*        process them).
*
*        The parameters that this affects are:
*           ADC
*           BOUNDS
*           DEFERRED
*           DIRECTION
*           EXTENT
*           RNOISE
*           SATURATION
*           ZERO
*
*        Values obtained from the CCDPACK extension are identified in
*        the output log by the presence of a trailing asterisk (*).
*        [FALSE]
*     USESET = _LOGICAL (Read)
*        Whether to use Set header information or not.  If USESET is
*        false then any Set header information will be ignored.
*        If USESET is true, then the BIAS parameter is
*        taken to refer to a group of files, and each IN file will
*        be processed using a master bias image with a Set Index
*        attribute which matches its own.  An IN file with no Set
*        header is considered to match a master bias file with
*        no Set header, so USESET can safely be set true
*        when the input files contain no Set header information.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.
*        [FALSE]
*     WMODE = LITERAL (Read)
*        The weighting method which is to be used when deriving means
*        or performing the least squares interpolation fits using any
*        bias strips. Can take the values "LINEAR", "EXP", or "NONE".
*        "LINEAR" and "EXP"-onential produce weights which are maximum
*        in the centre of each bias strip and which fall off towards
*        the edges. "LINEAR" weighting gives zero weighting for the
*        edge lines and so is the more robust.
*        [LINEAR]
*     ZERO = _DOUBLE (Read)
*        If USECON=TRUE then this value is subtracted from the input
*        NDF.

*  Examples:
*     debias r1 r1b bias '[2,10,400,415]' adc=1.1 rnoise=8
*        This example debiasses the data array in NDF r1 writing the
*        result to NDF r1b. It uses the data component of NDF BIAS as
*        the bias estimator. The bias is offset by the mean value found
*        within the ranges 2-10 and 400-415 pixels along the X axis.
*        The data in the bias strips are smoothed by a box filter and
*        weighted linearly from the edges inwards. The output variance
*        is produced by a combination of the Poisson statistics (using
*        an ADC value of 1.1) and readout noise (value 8), together
*        with the variance of the bias NDF (if present).
*
*     debias in=r1 out=r2 bounds='[2,10,401,416]' adc=2.5 rnoise=10
*        This example debiasses the NDF r1 data component writing the
*        result to the NDF r2. The bias is estimated by an interpolation
*        of a constant for each data row. The constant is the result of
*        a linearly weighted average of the bias strip data which has
*        been box filtered.
*
*     debias in=r1 out=r2 bounds='[2,10,401,416]' smode=linear adc=5
*            fmode=plane direct=y wmode=exp cmode=sigma rnoise=10
*            nsigma=4
*        This example debiasses the NDF r1 data component writing the
*        result to the NDF r2. The bias is estimated by the fitting of a
*        plane to the data in the bias strips. The bias-strip data are
*        first sigma clipped at a level RNOISE*NSIGMA. The fit is
*        performed with weighting based on a exponential fall off
*        from the centre of the strips. The bias strips are defined by
*        the bounds applied up the Y axis.
*
*     debias in='*' out='*_debias' bounds='[3,16,912,940]' adc=1 rnoise=4
*            bias=bias/master_bias
*        In this example all the NDFs in the current directory are
*        debiassed. The names of the output NDFs are as those of the
*        corresponding input NDFs, except that they are trailed by the
*        "_debias" string.
*
*  ASCII region definition files:
*      DEBIAS allows regions which are to be defined as having poor
*      quality (either by setting the appropriate pixels BAD or by
*      setting part of the quality component) to be described within an
*      ordinary text file using the ARD (ASCII Region Definition)
*      language. The ARD language is based on a set of keywords that
*      identify simple shapes. Some of the regions which can be defined
*      are:
*
*         - BOX
*         - CIRCLE
*         - COLUMN
*         - ELLIPSE
*         - LINE
*         - PIXEL
*         - POLYGON
*         - RECT
*         - ROTBOX
*         - ROW
*
*      ARD descriptions can be created using the KAPPA application
*      ARDGEN, or you can of course create your own by hand. An example
*      of the contents of an ARD file follows.
*
*         #
*         # ARD description file for bad regions of my CCD.
*
*         COLUMN( 41, 177, 212 )        # Three bad columns
*         PIXEL( 201, 143, 153, 167 )   # Two Bad pixels
*         BOX( 188, 313, 5, 5 )         # One Hot spot centred at 188,313
*         ELLIPSE( 99, 120, 21.2, 5.4, 45.0 )
*
*         # Polygons defining badly vignetted corners
*         POLYGON( 2.2, 96.4, 12.1, 81.5, 26.9, 63.7, 47.7, 41.9,
*                  61.5, 24.1, 84.3, 0.0 , 0.0, 0.0 )
*         POLYGON( 6.2, 294.3, 27.9, 321.0, 52.6, 348.7, 74.4, 371.5,
*                  80.0, 384.0, 0.0, 384.0 )
*         #

*  Implementation Status:
*     - This task supports all components of an NDF. If requested
*       [default] a variance is produced from the bias subtracted
*       values. The task processes BAD pixels. The UNITS of the output
*       NDF are set to ADUs or electrons depending on whether data
*       expansion has occurred or not. Processing is supported for all
*       HDS (non-complex) numeric types.

*  Notes:
*     - If the input NDFs have variance components and no variances
*       are to be generated then they are processed.
*
*     - Pixel indices. The bounds supplied to DEBIAS should be given as
*       pixel indices. These usually start at 1,1 for the pixel at the
*       lower left-hand corner of the data-array component (this may
*       not be true if the NDFs have been sectioned, in which case the
*       lower left hand pixel will have pixel indices equal to the data
*       component origin values). Pixel indices are different from
*       pixel coordinates in that they are non-continuous, i.e. can
*       only have integer values, and start at 1,1 not 0,0. To change
*       from pixel coordinates add 0.5 and round to the nearest integer.

*  Behaviour of Parameters:
*     Most parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*     The exceptions to this rule are:
*        - TITLE   -- always "Output from DEBIAS"
*        - KEEPIN  -- always TRUE
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application but does mean
*     that additional care needs to be taken when using the application
*     on new datasets/different devices, or after a break of sometime.
*     The intrinsic default behaviour of the application may be
*     restored by using the RESET keyword on the command line.
*
*     Certain parameters (ADC, BIAS, BOUNDS, DEFERRED, DIRECTION,
*     EXTENT, GENVAR, LOGFILE, LOGTO, MASK, PRESERVE, RNOISE, SATURATE,
*     SATURATION, SETSAT and USESET) have global values. These global values
*     will always take precedence, except when an assignment is made on
*     the command line.  If USESET is true, then global values of some
*     of these parameters (ADC, BOUNDS, DEFERRED, DIRECTION, EXTENT,
*     MASK, RNOISE, SATURATION) specific to the Set Index of each image
*     will be used if available.  In general global values may be set
*     and reset using the CCDSETUP and CCDCLEAR commands, however,
*     the BIAS parameter may only be set by a run of the application
*     MAKEBIAS.
*
*     If the parameter USEEXT is TRUE then the following parameters
*     are not used: ADC, BOUNDS, DEFERRED, DIRECTION, EXTENT, RNOISE,
*     SATURATION and ZERO. Values are obtained from the input NDF
*     extensions instead.

*  Copyright:
*     Copyright (C) 1991-1994 Science & Engineering Research Council.
*     Copyright (C) 1995-2005 Central Laboratory of the Research Councils.
*     Copyright (C) 2007 Science and Technology Facilities Council.
*     All Rights Reserved.

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
*     26-APR-1991 (PDRAPER):
*        Original version.
*     20-MAR-1992 (PDRAPER):
*        Changed to type cast unsigned data correctly.
*     5-JUN-1992 (PDRAPER):
*        Changed prologue and added new combination routines.
*     28-JAN-1994 (PDRAPER):
*        Now uses extension information. Added parameters GETMASK
*        and GETBIAS to overcome DCL's reluctance to accept !
*        on command line.
*     2-FEB-1994 (PDRAPER):
*        Added option to delete input NDFs when processed.
*     12-MAY-1994 (PDRAPER):
*        Changed ARD ellipse description to +Y through -X not
*        vice-versa.
*     6-OCT-1995 (PDRAPER):
*        Updated for CCDPACK 2.0.
*     3-MAR-1997 (PDRAPER):
*        Removed all top-level locator control (foreign data access
*        changes).
*     23-FEB-1999 (MBT):
*        Modified to propagate WCS component.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     14-FEB-2001 (MBT):
*        Upgraded for use with Sets.
*     15-MAY-2001 (MBT):
*        Added Set-specific global parameter values.
*     31-MAY-2007 (PDRAPER):
*        Remove unnecessary check for expanding output variances. That
*        always happens for GENVAR=TRUE (flawed logic).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'PAR_ERR'          ! Parameter system constants
      INCLUDE 'NDF_PAR'          ! Size of type buffers
      INCLUDE 'PRM_PAR'          ! Primdat constants
      INCLUDE 'GRP_PAR'          ! Standard GRP system constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL VAL_ITOB
      BYTE VAL_ITOB              ! Convert integer to byte with protection
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      BYTE BBYTE                 ! Badbits values
      CHARACTER * ( 7 * NDF__SZTYP ) TYPES ! Permitted types.
      CHARACTER * ( 80 ) MSKNAM  ! Name and type of mask file
      CHARACTER * ( NDF__SZFTP ) DTYPE  ! Full type of input data.
      CHARACTER * ( NDF__SZTYP ) ITYPE  ! Numeric type for processing data.
      CHARACTER * ( NDF__SZTYP ) ITYPEL ! Numeric type for processing data - last loop.
      CHARACTER * ( GRP__SZNAM ) NAME ! Name from a group
      DOUBLE PRECISION ADC       ! ADC factor
      DOUBLE PRECISION DEFER     ! Deferred zero level
      DOUBLE PRECISION SATVAL    ! Saturation value.
      INTEGER BADBIT             ! BADBITS value
      INTEGER BIAGRP             ! NDG identifier for bias NDFs
      INTEGER EL                 ! Number of pixels in Data component
      INTEGER GIDIN              ! Input NDF group identifier
      INTEGER GIDOUT             ! Output NDF group identifier
      INTEGER I                  ! Loop index
      INTEGER IDBIAS             ! " "
      INTEGER IDIM( 2 )          ! Dimensions of Data components
      INTEGER IDIN               ! Identifier
      INTEGER IDMASK             ! " "
      INTEGER IDOUT              ! " "
      INTEGER IDS(4)             ! Identifier stack
      INTEGER IDSO(3)            ! Identifier stack
      INTEGER IDSOUT             ! Sub section of output NDF
      INTEGER IDSUB              ! ""
      INTEGER IDWRK              ! NDF identifier for W/S frame
      INTEGER INDEX              ! Counter for main NDF loop
      INTEGER INGRP              ! NDG group identifier for all input NDFs
      INTEGER IPBIAS             ! Pointer to Data array (BIAS)
      INTEGER IPBVAR             ! Pointer to BIAS variance
      INTEGER IPIN               ! Pointer to Data array (IN)
      INTEGER IPMASK             ! Pointer to MASK data
      INTEGER IPOUT              ! Pointer to Data array (OUT)
      INTEGER IPOVAR             ! Pointer to output variance
      INTEGER IPQUAL             ! Pointer to Quality component
      INTEGER IPWRK              ! Pointer to workspace
      INTEGER ISUB               ! Subgroup loop index
      INTEGER ITOT               ! Total number of NDFs done
      INTEGER IVAL               ! Dummy variable
      INTEGER KEYGRP             ! GRP identifier for subgroup Index values
      INTEGER LBND( 2 )          ! Bounds of Data (lower) input NDF
      INTEGER LBNDB( 2 )         ! Bounds of Data (lower) BIAS
      INTEGER LBNDC( 2 )         ! Bounds of Data (lower) trimmed
      INTEGER LBNDL( 2 )         ! Bounds of Data (lower) last NDF
      INTEGER LBNDM( 2 )         ! Bounds of Data (lower) MASK
      INTEGER LBNDS( 2 )         ! Bounds of Data (lower) output section
      INTEGER NBIAS              ! Pointer to bias NDF ID
      INTEGER NDIM               ! Actual number of dimensions
      INTEGER NERR               ! Number of numeric errors
      INTEGER NID                ! Number of input identifiers (1 or 2)
      INTEGER NNDF               ! Number of data NDFs to process
      INTEGER NSAT               ! Number of saturated values.
      INTEGER NSUB               ! Number of subgroups
      INTEGER NTOT               ! Total number of input NDFs
      INTEGER OUTGRP             ! GRP identifier for all output NDFs
      INTEGER PLACE1             ! Place holder for workspace IDOUT.
      INTEGER PLACE2             ! Place holder for workspace
      INTEGER SHIFT( 2 )         ! Shifts to apply to fix origin
      INTEGER SINDEX             ! Current Set Index value
      INTEGER SUBGRP( CCD1__MXNDF ) ! NDG identifiers for input subgroups
      INTEGER UBND( 2 )          ! Bounds of Data (upper) input NDF
      INTEGER UBNDB( 2 )         ! Bounds of Data (upper) BIAS
      INTEGER UBNDC( 2 )         ! Bounds of Data (upper) trimmed
      INTEGER UBNDL( 2 )         ! Bounds of Data (upper) last NDF
      INTEGER UBNDM( 2 )         ! Bounds of Data (upper) MASK
      INTEGER UBNDS( 2 )         ! Bounds of Data (upper) output section
      LOGICAL BAD                ! Whether there are BAD pixels present or not.
      LOGICAL DELETE             ! Whether to delete input NDFs or not
      LOGICAL EXPAND             ! Whether to expand the data values to electrons or not
      LOGICAL EXTDEF             ! Deferred charge value from NDF extension
      LOGICAL EXTSAT             ! Saturation value from NDF extension
      LOGICAL EXTSEC             ! Out NDF section from NDF extension
      LOGICAL FORI               ! Whether to fix origin of output data to 1,1
      LOGICAL GENVAR             ! Controls whether variances are generated or not.
      LOGICAL GETBIA             ! Will attempt to get a master bias
      LOGICAL GETMSK             ! Will attempt to get a MASK
      LOGICAL GOTBIA             ! Flag for got bias frame
      LOGICAL GOTMSK             ! Flag for have mask frame
      LOGICAL HAVBIT             ! Flag specifying whether BADBITS is set or not.
      LOGICAL HAVBV              ! Flag for got bias variance
      LOGICAL HAVIV              ! Flag for have an unwanted input variance
      LOGICAL HAVQAL             ! state of quality component
      LOGICAL NOQUAL             ! Whether quality is to be used for masking of CCD defects.
      LOGICAL PRESER             ! Flag for preserve input NDF data type.
      LOGICAL REMAP              ! Flag controlling remapping
      LOGICAL SATUR              ! Whether to set saturated values (to BAD)
      LOGICAL SETSAT             ! Whether to set saturated values (to SATVAL)
      LOGICAL USEEXT             ! Get CCD parameters from extensions
      LOGICAL USESET             ! Whether to use Set header information
      LOGICAL VALID              ! Whether NDF identifier is valid
      LOGICAL ZEROED             ! Whether or not input bias master has been zeroed
      LOGICAL ZEROCK             ! Whether or not to check bias master is processed as specified by ZEROED
      REAL FRAC                  ! Fractional numeric error count
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialisations.
      IPWRK = 0
      IPOUT = 0
      IPBIAS = 0
      LBNDL( 1 ) = 1
      LBNDL( 2 ) = 1
      UBNDL( 1 ) = 1
      UBNDL( 2 ) = 1

************************************************************************
*  Open Logfile and write program introduction. Find out if NDF
*  extension items are to be checked.
************************************************************************
*  Open output file for logging parameters (if requested).
      CALL CCD1_START( 'DEBIAS', STATUS )

*  NDF context for whole routine.
      CALL NDF_BEGIN

*  Initialise GRP identifiers, so that a later call of CCD1_GRDEL on
*  an uninitialised group cannot cause trouble.
      GIDIN = GRP__NOID
      GIDOUT = GRP__NOID
      INGRP = GRP__NOID
      OUTGRP = GRP__NOID
      KEYGRP = GRP__NOID
      BIAGRP = GRP__NOID
      DO I = 1, CCD1__MXNDF
         SUBGRP( I ) = GRP__NOID
      END DO

*  See if the user wants to save disk space by deleting the input NDFs
*  when DEBIAS is finished with them. This will use the NDF_DELET
*  call which will delete container files if the NDF is associated with
*  the top-level object, otherwise the NDF itself will just be deleted.
*  In the latter case the space used by the NDF in the container file
*  will be released, the size of the file will probably not reduce.
      CALL PAR_GET0L( 'KEEPIN', DELETE, STATUS )
      DELETE = .NOT. DELETE

*  Are certain values to be obtained from NDF extensions rather than
*  from the environment?
      CALL PAR_GET0L( 'USEEXT', USEEXT, STATUS )

************************************************************************
* INPUT AND OUTPUT NDFs. May have a list of them.
************************************************************************
*  Access an NDG group containing a list of NDF names.
      CALL CCD1_NDFGL( 'IN', 1, CCD1__MXNDF, INGRP, NTOT, STATUS )

*  Find out if we are using Set header information.
      CALL PAR_GET0L( 'USESET', USESET, STATUS )

*  Split the group of input NDFs up by Set Index if necessary.
      NSUB = 1
      IF ( USESET ) THEN
         CALL CCD1_SETSP( INGRP, 'INDEX', CCD1__MXNDF, SUBGRP, NSUB,
     :                    KEYGRP, STATUS )
      ELSE
         SUBGRP( 1 ) = INGRP
         KEYGRP = GRP__NOID
      END IF

*  Access a second list of NDF names for the outputs. Use the input
*  names as a modification list.
      CALL CCD1_NDFPG( 'OUT', INGRP, NTOT, OUTGRP, STATUS )

************************************************************************
* MASK AND BIAS NDF ACCESS --- keep these out of main loop for
* efficiency reasons. Mapping and unmapping of these frames occurs on an
* as necessary basis.
************************************************************************
*  Get the BIAS frame NDF, this may be defaulted to ! null. In which
*  case interpolation will be attempted.
      GETBIA = .TRUE.
      CALL PAR_GET0L( 'GETBIAS', GETBIA, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999
      IF ( GETBIA ) THEN
         IF ( USESET ) THEN
            CALL CCD1_NDFMI( 'BIAS', KEYGRP, BIAGRP, STATUS )
         ELSE
            CALL CCD1_NDFGL( 'BIAS', 1, 1, BIAGRP, IVAL, STATUS )
         END IF
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            GOTBIA = .FALSE.
            HAVBV = .FALSE.
         ELSE
            GOTBIA = .TRUE.
         END IF
      ELSE

*  Do not try for a master bias.
         GOTBIA = .FALSE.
         HAVBV = .FALSE.
      END IF


************************************************************************
*  Loop over subgroups performing calculations separately for each one
************************************************************************
      ITOT = 0
      DO ISUB = 1, NSUB
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Write a header unless this is the only subgroup.
         IF ( NSUB .GT. 1 ) THEN
            CALL CCD1_SETHD( KEYGRP, ISUB, 'Debiassing', 'Index',
     :                       STATUS )
         END IF

*  Get the Set Index value common to this subgroup.
         IF ( USESET ) THEN
            CALL GRP_GET( KEYGRP, ISUB, 1, NAME, STATUS )
            CALL CHR_CTOI( NAME, SINDEX, STATUS )

*  Load in parameter values specific to this subgroup, if they exist.
            CALL CCD1_KPLD( 'ADC', SINDEX, STATUS )
            CALL CCD1_KPLD( 'BOUNDS', SINDEX, STATUS )
            CALL CCD1_KPLD( 'DEFERRED', SINDEX, STATUS )
            CALL CCD1_KPLD( 'DIRECTION', SINDEX, STATUS )
            CALL CCD1_KPLD( 'EXTENT', SINDEX, STATUS )
            CALL CCD1_KPLD( 'MASK', SINDEX, STATUS )
            CALL CCD1_KPLD( 'RNOISE', SINDEX, STATUS )
            CALL CCD1_KPLD( 'SATURATION', SINDEX, STATUS )
         END IF

*  Set up the group of input NDFs for this subgroup.
         GIDIN = SUBGRP( ISUB )
         CALL GRP_GRPSZ( GIDIN, NNDF, STATUS )

*  Set up the group of output NDFs for this subgroup.
         CALL CCD1_ORDG( INGRP, OUTGRP, GIDIN, GIDOUT, STATUS )

*  Set up information about the bias frame if it exists.
         IF ( GOTBIA ) THEN

*  Get the master bias NDF identifier for this subgroup.
            CALL NDG_NDFAS( BIAGRP, ISUB, 'READ', IDBIAS, STATUS )

*  Check the frame type of the BIAS. This should be MASTER_BIAS.
*  If this is wrong this is not a serious problem.
            CALL CCD1_CKTYP( IDBIAS, 1, 'MASTER_BIAS', STATUS )

*  See if it has been zeroed or not, if item exists then no further
*  checking will be performed.
            CALL CCG1_FCH0L( IDBIAS, 'ZEROED', ZEROED, ZEROCK, STATUS )

*  Find out if the bias NDF has a variance component.
            CALL NDF_BOUND( IDBIAS, 2, LBNDB, UBNDB, NDIM, STATUS )
            CALL NDF_STATE( IDBIAS, 'Variance', HAVBV, STATUS )

*  Get the mask frame (if given), set bounds to those of the BIAS NDF.
            LBNDM( 1 ) = LBNDB( 1 )
            LBNDM( 2 ) = LBNDB( 2 )
            UBNDM( 1 ) = UBNDB( 1 )
            UBNDM( 2 ) = UBNDB( 2 )

*  Also get a suitable type for the mask data, ensures that mask data
*  type does not throw the merged type later.
            CALL NDF_TYPE( IDBIAS, 'Data', ITYPE, STATUS )
         ELSE

*  None to supply if the mask file is an ARD description, set LBNDM(1)
*  to VAL__BADI and give GIDIN so that size of first NDF can be used.
            ITYPE = ' '
            LBNDM( 1 ) = VAL__BADI
         END IF

*  Access the MASK, if asked to try.
         GETMSK = .TRUE.
         CALL PAR_GET0L( 'GETMASK', GETMSK, STATUS )
         IF ( GETMSK ) THEN
            CALL CCD1_GTMSK( 'MASK', UBNDM, LBNDM, INGRP, ITYPE, IDMASK,
     :                       MSKNAM, STATUS )
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               GOTMSK= .FALSE.
            ELSE
               GOTMSK = .TRUE.
            END IF
         ELSE

*  No mask.
            GOTMSK = .FALSE.
         END IF

*  Find out whether we need to generate variances.
         GENVAR = .FALSE.
         CALL PAR_GET0L( 'GENVAR', GENVAR, STATUS )

*  Set up original input NDF identifier stack.
         NID = 1
         NBIAS = 1
         IF ( GOTMSK ) THEN
            NID = NID + 1
            IDSO( NID ) = IDMASK
         END IF
         IF ( GOTBIA ) THEN
            NID = NID + 1
            NBIAS = NID
            IDSO( NID ) = IDBIAS
         END IF

************************************************************************
* NDF LIST LOOP
************************************************************************
*  Start looping for all NDFs. On each pass a value for every parameter
*  will either be obtained from the NDF extension (USEEXT=TRUE) or
*  from the environment. PAR_CANCL is not used so each call to the
*  environment for a parameter value will return the same value.
         DO 99999 INDEX = 1, NNDF

*  Increment the number of NDFs tackled in total so far.
            ITOT = ITOT + 1

*  Get the identifier of the input NDF.
            CALL NDG_NDFAS( GIDIN, INDEX, 'READ', IDIN, STATUS )

*  Write out name of this NDF. And which loop this is.
            CALL CCD1_MSG( ' ',  ' ', STATUS )
            CALL NDF_MSG( 'CURRENT_NDF', IDIN )
            CALL CCD1_MSG( ' ', '  +++ Processing NDF: ^CURRENT_NDF',
     :                     STATUS )
            CALL MSG_SETI( 'CURRENT_NUM', ITOT )
            CALL MSG_SETI( 'MAX_NUM', NTOT )
            CALL CCD1_MSG( ' ', '  (Number ^CURRENT_NUM of ^MAX_NUM)',
     :                     STATUS )

*  Find out if the input NDF has a variance component. If it has write
*  out an message warning user that this component will be lost. If not
*  generating variances and one exists already better process it.
            CALL NDF_STATE( IDIN, 'Variance', HAVIV, STATUS )
            IF ( HAVIV .AND. GENVAR ) THEN
               CALL MSG_OUT( 'DEBIAS_GOTVAR',
     :' Warning - input NDF has a variance component this will be'//
     :' superceded', STATUS )
            END IF

*  Check that the frame type of the NDF is reasonable for debiassing.
*  This means that is shouldn't have already been debiassed and it
*  shouldn't have a type of BIAS.
            CALL CCD1_CKCDB( IDIN, STATUS )

************************************************************************
* INPUT NDF merging matching and typing
************************************************************************
* Get the size of the present NDF. If INDEX is equal to one then do not
* compare these values with the last NDF values, just set the REMAP
* flags. If INDEX is not equal to one then compare these bounds with
* those of the last NDF. If they are equal then REMAP is false and no
* padding etc. is required. This saves time remapping every loop.
* Remember the present processing type. Merge the processing types, if a
* new processing type is required set the remap flag for all data to be
* remapped with the new processing type.

*  First find out the bounds of the current NDF.
            CALL NDF_BOUND( IDIN, 2, LBND, UBND, NDIM, STATUS )
            IF ( INDEX .EQ. 1 ) THEN

*  Set the mapping control flag - REMAP - one off initialisation.
               REMAP = .TRUE.
            ELSE

*  Compare these bounds with those of the last NDF.
               IF ( LBND( 1 ) .NE. LBNDL( 1 ) .OR.
     :              UBND( 1 ) .NE. UBNDL( 1 ) .OR.
     :              LBND( 2 ) .NE. LBNDL( 2 ) .OR.
     :              UBND( 2 ) .NE. UBNDL( 2 ) ) THEN

*  Bounds have changed need to remap after trimming to bias,mask frames
*  Get W/S at new size etc.
                  REMAP = .TRUE.
               ELSE

*  No change in bounds, remapping may not be necessary.
                  REMAP = .FALSE.
               END IF
            END IF

*  Check the bounds of the MASK and BIAS frames against those of the
*  input NDF. If the bounds are not the same as the input NDF then
*  trimming will have to be performed, regardless of whether the
*  actual NDF size has changed or not.
            IF ( GOTMSK ) THEN
               IF ( LBND( 1 ) .NE. LBNDM( 1 ) .OR.
     :              UBND( 1 ) .NE. UBNDM( 1 ) .OR.
     :              LBND( 2 ) .NE. LBNDM( 2 ) .OR.
     :              UBND( 2 ) .NE. UBNDM( 2 ) ) THEN

*  Bad array bounds - issue a warning - the NDF sections will be trimmed
*  later.
                  CALL MSG_OUT( 'BAD_BOUNDS',
     :' Warning - MASK bounds do not match input NDF', STATUS )
                  REMAP = .TRUE.
               END IF
            END IF

* Bias frame.
            IF ( GOTBIA ) THEN
               IF ( LBND( 1 ) .NE. LBNDB( 1 ) .OR.
     :              UBND( 1 ) .NE. UBNDB( 1 ) .OR.
     :              LBND( 2 ) .NE. LBNDB( 2 ) .OR.
     :              UBND( 2 ) .NE. UBNDB( 2 ) ) THEN

*  Bad array bounds - issue a warning - the NDF sections will be trimmed
*  later.
                   CALL MSG_OUT( 'BAD_BOUNDS',
     :' Warning - BIAS bounds do not match input NDF', STATUS )
                  REMAP = .TRUE.
               END IF
            END IF

*  See if the user wants to process in a better precision than the
*  present one.
            PRESER = .TRUE.
            CALL PAR_GET0L( 'PRESERVE', PRESER, STATUS )
            IF ( PRESER ) THEN

*  No change to data type. If PRESERVE if false then go up to next
*  floating point precision
               TYPES = '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'//
     :                 '_DOUBLE'
            ELSE

*  Just floating point arithmetic and result
               TYPES = '_REAL,_DOUBLE'
            END IF

*  Set input identifier stack for unmodified (sectioned) INPUT frames.
*  This stack is used for testing the types.
            IDSO( 1 ) = IDIN

* Merge the types of the input data to the lowest representation.
            CALL NDF_MTYPN( TYPES, NID, IDSO, 'Data,Variance', ITYPE,
     :                      DTYPE, STATUS )

*  Compare this with the last type.
            IF ( INDEX .NE. 1 ) THEN
               IF ( ITYPE .NE. ITYPEL ) THEN

*  Processing types have changed - reset everything.
                  REMAP = .TRUE.
               END IF
            END IF

*  Look after sectioned NDF stack. Annul last identifiers if remapping
*  and reestablish the stack as the unsectioned input stack.
            IF ( REMAP ) THEN

*  Annul and unmap previous identifiers. Except the first which is
*  always unmapped and annulled (the last input NDF).
               IF ( INDEX .NE. 1 ) THEN
                  IF ( NID .GT. 1 ) THEN
                     DO 20 IVAL = 2, NID
                        CALL NDF_UNMAP( IDS( IVAL ), '*', STATUS )
                        CALL NDF_ANNUL( IDS( IVAL ), STATUS )
 20                  CONTINUE
                  END IF

*  Unmap any previous workspace as well. All unmapping now performed.
                  CALL NDF_UNMAP( IDWRK, '*', STATUS )
                  CALL NDF_ANNUL( IDWRK, STATUS )
               END IF

*  Make new stack for the sectioning part. Use clones of the originals
*  to stop embarrassing annulments.
               DO 21 IVAL = 1, NID
                  IF ( IVAL .EQ. 1 ) THEN

*  Current input NDF - do not clone this annulment ok.
                     IDS( IVAL ) = IDSO( IVAL )
                  ELSE

*  Bias or mask NDF keep the original identifiers safe.
                     CALL NDF_CLONE( IDSO( IVAL ), IDS( IVAL ), STATUS )
                  END IF
 21            CONTINUE
            ELSE

*  Just using the last sections - copy input NDF identifier to
*  sectioning stack.
               IDS( 1 ) = IDSO( 1 )
            END IF

*  If REMAP is true then all NDFs require may trimmimg to size,
*  perform this task.
            IF ( REMAP ) THEN
               CALL NDF_MBNDN( 'TRIM', NID, IDS, STATUS )

*  Restore convenient labels for the NDF identifiers. The old ones have
*  may have been annulled in the last call.
               NID = 1
               IDIN = IDS( 1 )
               IF ( GOTMSK ) THEN
                  NID = NID + 1
                  IDMASK = IDS( NID )
               END IF
               IF ( GOTBIA ) THEN
                  NID = NID + 1
                  IDBIAS = IDS( NID )
               END IF

*  Find out the size of the merged NDFS. (W/S use)
               CALL NDF_BOUND( IDIN, 2, LBNDC, UBNDC, NDIM, STATUS )
               IDIM( 1 ) = UBNDC( 1 ) - LBNDC( 1 ) + 1
               IDIM( 2 ) = UBNDC( 2 ) - LBNDC( 2 ) + 1
            END IF

*  Check for the presence of BAD pixels in the input data.
            CALL NDF_MBADN( .TRUE., NID, IDS, 'Data,Variance', .FALSE.,
     :                      BAD, STATUS )

************************************************************************
*  MAP CONTROL SECTION -- MAP IN PERMANENT DATA FIRST --
*  workspace array, bias and mask.
************************************************************************
*  Need a temporary array to contain processed data create it.
            IF ( REMAP ) THEN
               CALL NDF_TEMP( PLACE2, STATUS )
               CALL NDF_NEW( ITYPE, 2, LBNDC, UBNDC, PLACE2, IDWRK,
     :                       STATUS )

*  Map in workspace for containing the processed array.
               CALL NDF_MAP( IDWRK, 'Data', ITYPE, 'WRITE/BAD', IPWRK,
     :                       EL, STATUS )
            ELSE

*  Set the array BAD.
               CALL CCD1_STVB( ITYPE, EL, IPWRK, STATUS )
            END IF

*  BIAS NDF.
            IF ( GOTBIA ) THEN
               IF ( REMAP ) THEN
                  CALL NDF_MAP( IDBIAS, 'Data', ITYPE, 'READ', IPBIAS,
     :                          EL, STATUS )
               END IF
               IF ( HAVBV .AND. GENVAR .OR. HAVBV .AND. HAVIV ) THEN

*  Have bias variance and will generate an variance component, or
*  have a bias variance and an input variance - can process these.
*  Either way map in the bias variance for processing.
                  IF ( REMAP ) CALL NDF_MAP( IDBIAS, 'Variance', ITYPE,
     :                                       'READ', IPBVAR, EL,
     :                                       STATUS )
               END IF
            END IF

*  Map in the mask data array if given and requires remapping.
            IF ( GOTMSK ) THEN
               IF ( REMAP ) THEN
                  CALL NDF_MAP( IDMASK, 'Data', ITYPE, 'READ', IPMASK,
     :                          EL, STATUS )
               END IF
            END IF

*  Map in the input and output NDFs these are volatile and should sit
*  "on top" of the virtual address space with the other volatile arrays
*  which usually come and go on each pass (these comments - and similar
*  ones in this routine - were made when having problems with
*  fragmented address space on VMS systems - PDRAPER)

*  Get the output NDF by copying.
*  NOTE THAT THIS VERSION IS A DUMMY - the real output NDFs are created
*  later when the output section size is determined. This NDF is the
*  same size as the merged BIAS, MASK and INPUT NDFs.
            CALL NDF_TEMP( PLACE1, STATUS )
            CALL NDF_COPY( IDIN, PLACE1, IDOUT, STATUS )

*  Force type of array to be that of destination data, this avoids
*  recasting the data into the input type before propagation to the true
*  output NDF and its type. Unsigned data can cause considerable
*  problems if not done this way.
            CALL NDF_STYPE( ITYPE, IDOUT, 'Data,Variance', STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Map in the output NDF.
            CALL NDF_MAP( IDOUT, 'Data', ITYPE, 'WRITE', IPOUT, EL,
     :                    STATUS )

*  Map in the output variance component, set to BAD if generating
*  variances. Map it in only if have a result to enter, i.e. if
*  generating variances or have a variance to propagate.
            IF ( GENVAR ) THEN
               CALL NDF_MAP( IDOUT, 'Variance', ITYPE, 'WRITE/BAD',
     :                       IPOVAR, EL, STATUS )
            ELSE IF ( HAVIV .AND. .NOT. GENVAR ) THEN
               CALL NDF_MAP( IDOUT, 'Variance', ITYPE, 'WRITE',
     :                       IPOVAR, EL, STATUS )
            END IF

*  Input NDF.
            CALL NDF_MAP( IDIN, 'Data', ITYPE, 'READ', IPIN, EL,
     :                    STATUS )

************************************************************************
*  DEBIASSING AND VARIANCE ESTIMATION
************************************************************************
*  Right ready to get on with the first stage main processing.
*  Debias the input frame and generate the variances. If GENVAR is false
*  then do not generate variances. Check that the bias master is
*  processed correctly according to whether it has been zeroed or not.
             CALL CCD1_DEBIA( ITYPE, BAD, GENVAR, USEEXT, IDIN, EL,
     :                        IPIN, IDIM( 1 ), IDIM( 2 ), LBNDC( 1 ),
     :                        LBNDC( 2 ), IPOUT, GOTBIA, ZEROED,
     :                        ZEROCK, IPBIAS, HAVBV, IPBVAR, HAVIV,
     :                        IPOVAR, IPWRK, PRESER, DTYPE,
     :                        IDSO( NBIAS ), ADC, STATUS )

*  Write out the data units, must be ADUs at this stage (unless the user
*  was perverse enough to scale all the data before passing to CCDPACK).
            CALL NDF_CPUT( 'ADUs', IDOUT, 'UNITS', STATUS )

************************************************************************
* MASKING;     input to this section is in the output NDF.
************************************************************************
            HAVBIT = .FALSE.
            IF ( GOTMSK ) THEN

*  Find if the user wants to use the quality component for masking bad
*  pixels
               CALL PAR_GET0L( 'SETBAD', NOQUAL, STATUS )
               IF ( .NOT. NOQUAL ) THEN

*  Have to use quality component.
*  Find out the current value of the badbits mask.
                  CALL NDF_BB( IDOUT, BBYTE, STATUS )

*  If this value is zero then the BADBITS mask has not previously been
*  set. Set it to the value we get from the user.
                  IF( BBYTE .EQ. 0 ) THEN
                     BADBIT = 1
                     CALL PAR_GET0I( 'BADBITS', BADBIT, STATUS )

*  Convert this into a byte value.
                     BBYTE = VAL_ITOB( .TRUE., BADBIT, STATUS )

*  Remember that this has been done.
                     HAVBIT = .TRUE.
                  END IF

*  Map it in. If it is already defined if it is not then we need to
*  initialise it as not all values will be be set in this task.
                  CALL NDF_STATE( IDOUT, 'Quality', HAVQAL, STATUS )
                  IF ( HAVQAL ) THEN
                     CALL NDF_MAP( IDOUT, 'Quality', '_UBYTE', 'WRITE',
     :                             IPQUAL, EL, STATUS )
                  ELSE
                     CALL NDF_MAP( IDOUT, 'Quality', '_UBYTE',
     :                             'WRITE/ZERO', IPQUAL, EL, STATUS )
                  END IF
               ELSE

*  Need to transfer information into output Data array swap pointers
                  IPQUAL = IPOUT
               END IF

*  Right now transfer the 'BAD' pixel data to from the mask NDF to the
*  new NDF. Have two basic situations, setting the quality component to
*  include the BBYTE value, or setting pixels BAD if the mask pixels
*  are BAD.
               CALL CCD1_MASKD( ITYPE, NOQUAL, IPMASK, IPQUAL, EL,
     :                          BBYTE, STATUS )
               CALL CCD1_RMSK( GOTMSK, MSKNAM, NOQUAL, STATUS )
            END IF

************************************************************************
* DATA VALUE EXPANSION AND SATURATING -- DEFERRED CHARGE CORRECTION
************************************************************************
*  Find out if the user wants to expand the data values into counts.
*  Only allow this if generating variances, may not have ADC factor.
*  Set dynamic defaults for value - false if data is unsigned.
            EXPAND = .FALSE.
            IF( GENVAR ) THEN
               IF ( ITYPE( 1 : 2 ) .EQ. '_U' ) THEN
                  CALL PAR_DEF0L( 'EXPAND', .FALSE., STATUS )
               ELSE
                  CALL PAR_DEF0L( 'EXPAND', .TRUE., STATUS )
               END IF
               CALL PAR_GET0L( 'EXPAND', EXPAND, STATUS )
            END IF

*  Does the user want to saturate the data?
            SATUR = .FALSE.
            CALL PAR_GET0L( 'SATURATE', SATUR, STATUS )

*  If saturation is to be allowed then the user has two options. Either
*  setting the saturated data BAD or setting it to a given value.  If
*  the later option is chosen then the saturation value will have to be
*  stored with the data and modified (during global operations which
*  modify the frame similarly i.e. multiplication, addition of
*  constants etc.) as the data is modified, or in the case of a
*  flatfield (a frame-frame operation, during which the saturated
*  values are unlikely to retain the same value), saturated values will
*  have to remain unmodified.
            SETSAT = .FALSE.
            SATVAL = VAL__MAXD
            IF ( SATUR ) THEN

*  Offer the set saturation value option.
               SETSAT = .FALSE.
               CALL PAR_GET0L( 'SETSAT', SETSAT, STATUS )

*  Get the saturation value. This may be available in the NDF extension
*  so have a look if required. If none is found then just prompt the
*  user for the saturation value.
               EXTSAT = .FALSE.
               IF ( USEEXT ) THEN
                  CALL CCG1_FCH0D( IDIN, 'SATURATION', SATVAL, EXTSAT,
     :                             STATUS )
               END IF
               IF ( .NOT. EXTSAT ) THEN
                  CALL PAR_GET0D( 'SATURATION', SATVAL, STATUS )
               END IF

*  Saturate the data either setting values to BAD or the saturation
*  value as requested.
               CALL CCD1_STSAT( ITYPE, BAD, IPOUT, EL, SETSAT, SATVAL,
     :                          NSAT, STATUS )
            END IF

*  Correct for deferred charge (the value given has the sense of
*  subtraction). This value may be found in the NDF extension so look
*  for it there if this is required.
            EXTDEF = .FALSE.
            IF ( USEEXT ) THEN
               CALL CCG1_FCH0D( IDIN, 'DEFERRED', DEFER, EXTDEF,
     :                          STATUS )
            END IF
            IF ( .NOT. EXTDEF ) THEN
               CALL PAR_GET0D( 'DEFERRED', DEFER, STATUS )
            END IF
            IF ( DEFER .NE. 0.0 ) THEN
               CALL CCD1_COSUB( ITYPE, BAD, IPOUT, EL, DEFER, IPWRK,
     :                          STATUS )
            END IF

*  If requested expand data values by multiplication by ADC. Only allow
*  this if variances are being generated.
            IF ( EXPAND .AND. GENVAR ) THEN
               CALL CCD1_CMULT( BAD, ITYPE, IPOUT, EL, ADC, IPWRK, NERR,
     :                          STATUS )

*  Report numeric problems.
               IF ( NERR .GT. 0 ) THEN
                  FRAC = REAL( NERR ) / REAL( EL ) * 100.0
                  CALL MSG_SETR( 'FRAC', FRAC )
                  CALL MSG_SETI( 'NERR', NERR )
                  CALL CCD1_MSG( ' ',
     :' Warning - ^NERR numeric errors (^FRAC%% of total ) occurred'//
     :' when expanding input ADUs to electrons' , STATUS )
                  IF ( FRAC .GT. 0.5 ) THEN

*  Serious warning.
                     CALL MSG_SETC( 'TYPE', ITYPE )
                     CALL CCD1_MSG( ' ',
     :' Warning - large amounts of output data cannot be'//
     :' represented within the range of the chosen data type ^TYPE',
     : STATUS )
                  END IF
               END IF

*  Set the units title
               CALL NDF_CPUT( 'ELECTRONS', IDOUT, 'UNITS', STATUS )

*  Variance needs modification as well.
               CALL CCD1_CMULT( BAD, ITYPE, IPOVAR, EL, ADC * ADC,
     :                          IPWRK, NERR, STATUS )

*  Saturation value requires modification, from the pre-expanded value.
               IF( SETSAT ) SATVAL = ADC * SATVAL
            END IF

*  Report progress in this section.
            CALL CCD1_REXP( EXPAND, SATUR, SETSAT, SATVAL, EXTSAT,
     :                      NSAT, DEFER, EXTDEF, STATUS )

************************************************************************
*  TRIM SECTION
************************************************************************
*  Get the section extents for the useful output area.
            CALL CCD1_GTSEC( USEEXT, IDIN, LBND, UBND, LBNDS, UBNDS,
     :                       EXTSEC, STATUS )

*  Unmap everything in the current output NDF. Some form of conflict
*  occurs on propagation.
            CALL NDF_UNMAP( IDOUT, '*', STATUS )

*  Set a section of this size on the temporary output NDF
            CALL NDF_SECT( IDOUT, 2, LBNDS, UBNDS, IDSUB, STATUS )

*  Propagate this section to the output NDF.
            CALL NDG_NDFPR( IDSUB,
     :                      'Data,Variance,Quality,Axis,Units,WCS',
     :                      GIDOUT, INDEX, IDSOUT, STATUS )

************************************************************************
*  BAD PIXELS, OUTPUT DATA TYPE, TITLE, EXTENSION UPDATE.
************************************************************************
*  Set output arrays components BAD flag
            CALL NDF_SBAD( BAD, IDSOUT, 'Data,Variance', STATUS )

*  Set title of output NDF, propagating it if requested.
            CALL NDF_CINP( 'TITLE', IDSOUT, 'TITLE', STATUS )

*  Set the BADBITS mask value if this option has been used.
            IF ( GOTMSK .AND. HAVBIT ) THEN
               CALL NDF_SBB( BBYTE, IDSOUT, STATUS )
            END IF

*  Record the current saturation value in the CCDPACK extension.
            IF ( SETSAT ) THEN
               CALL CCG1_STO0D( IDSOUT, 'SATVAL', SATVAL, STATUS )
            END IF

*  Offer the option to set the origin of the NDF to 1,1.
            CALL PAR_GET0L( 'FIXORIGIN', FORI, STATUS )
            IF ( FORI ) THEN
               SHIFT( 1 ) = 1 - LBNDS( 1 )
               SHIFT( 2 ) = 1 - LBNDS( 2 )
               CALL NDF_SHIFT( 2, SHIFT, IDSOUT, STATUS )
            END IF

*  Final report on output section, name of output NDF and data type.
            CALL CCD1_REND( LBND, UBND, LBNDS, UBNDS, EXTSEC, ITYPE,
     :                      IDSOUT, FORI, STATUS )

*  Write terminator for Processing NDF: message.
            CALL CCD1_MSG( ' ', '  ---',STATUS )

************************************************************************
*  NDF tidying up close input and output container files.
************************************************************************
*  Touch the output NDF to leave an audit-like trail showing that this
*  NDF has been debiassed.
            CALL CCD1_TOUCH( IDSOUT, 'DEBIAS', STATUS )

*  Release the NDF.
            CALL NDF_UNMAP( IDIN, '*', STATUS )
            CALL NDF_ANNUL( IDIN, STATUS )

*  Release the output NDFs.
            CALL NDF_ANNUL( IDOUT, STATUS )
            CALL NDF_ANNUL( IDSUB, STATUS )
            CALL NDF_ANNUL( IDSOUT, STATUS )

*  Reset the REMAP flag to false, if a condition occurs requiring the
*  trimming etc of the bias and mask frames, this will explicitly change
*  the flag.
            REMAP = .FALSE.
            ITYPEL = ITYPE

*  Store the current NDF bounds.
            LBNDL( 1 ) = LBND( 1 )
            LBNDL( 2 ) = LBND( 2 )
            UBNDL( 1 ) = UBND( 1 )
            UBNDL( 2 ) = UBND( 2 )

************************************************************************
* End of main processing loop. Jump out if BAD status. Saves lots of
* loops.
************************************************************************
            IF ( STATUS .NE. SAI__OK ) GO TO 999
99999 CONTINUE
      END DO

*  Delete input NDFs if so requested to do so.
      IF ( DELETE .AND. STATUS .EQ. SAI__OK ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '   *** Deleting input NDFs.', STATUS )
         DO I = 1, NTOT
            CALL CCD1_NGDEL( INGRP, I, .TRUE., STATUS )
         END DO
      END IF

************************************************************************
*  Tidying up section.
************************************************************************
999   CONTINUE

*  Release workspace NDF if it is still active.
      VALID = .FALSE.
      CALL NDF_VALID( IDWRK, VALID, STATUS )
      IF ( VALID ) CALL NDF_ANNUL( IDWRK, STATUS )

*  Release all NDF left accessed - unmap everything etc.
      CALL NDF_END( STATUS )

*  Release group resources.
      CALL CCD1_GRDEL( GIDIN, STATUS )
      CALL CCD1_GRDEL( GIDOUT, STATUS )
      CALL CCD1_GRDEL( INGRP, STATUS )
      CALL CCD1_GRDEL( OUTGRP, STATUS )
      CALL CCD1_GRDEL( KEYGRP, STATUS )
      CALL CCD1_GRDEL( BIAGRP, STATUS )
      DO I = 1, MIN( NSUB, CCD1__MXNDF )
         CALL CCD1_GRDEL( SUBGRP( I ), STATUS )
      END DO

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'DEBIAS_ERR',
     :   'DEBIAS: Error performing initial preparation of CCD data.',
     :   STATUS )
      END IF

*  Finally close log file.
      CALL CCD1_END( STATUS )

      END
* $Id$
