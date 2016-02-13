/* Defining the FPTRAP macro will cause floating point exceptions to
   occur whenever a NaN, inf or overflow is generated. This can make it
   easier to debug the cause of these values. */
#if defined(FPTRAP)
#define _GNU_SOURCE
#include <fenv.h>
#endif

/*
*+
*  Name:
*     MAKECUBE

*  Purpose:
*     Regrid ACSIS spectra into a data cube.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_makecube( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine converts one or more raw data cubes, spanned by
*     (frequency, detector number, time) axes, into an output cube
*     spanned by (celestial longitude, celestial latitude, frequency) axes.
*
*     Optionally, the output cube can be split up into several separate
*     NDFs, each containing a spatial tile extracted from the full cube
*     (see parameter JSATILES and TILEDIMS). These tiles abut exactly in
*     pixel co-ordinates and can be combined (for example) using KAPPA:PASTE.
*
*     In addition, there is an option to divide the output up into separate
*     polarisation angle bins (see parameter POLBINSIZE). If this option
*     is selected, each tile is split up into several output NDFs (all
*     within the same container file), each one containing the input data
*     relating to a particular range of polarisation angle.
*
*     The full output cube can be either a regularly gridded tangent-plane
*     projection of the sky, or a sparse array (see parameter SPARSE).
*     If a tangent plane projection is selected, the parameters of the
*     projection from sky to pixel grid co-ordinates can be specified using
*     parameters CROTA, PIXSIZE, REFLAT, REFLON. Alternatively, parameter
*     AUTOGRID can be set true, in which case projection parameters are
*     determined automatically in a manner that favours projections that
*     place samples centrally within pixels. Alternatively, a reference
*     NDF can be supplied (see parameter REF), in which case the same pixel
*     grid will be used for the output cube.
*
*     Variance values in the output can be calculated either on the basis
*     of the spread of input dat avalues contributing to each output pixel,
*     or on the basis of the system-noise temperature values supplied in the
*     input NDFs (see parameter GENVAR).

*  ADAM Parameters:
*     ALIGNSYS = _LOGICAL (Read)
*          If TRUE, then the spatial positions of the input data are
*          aligned in the co-ordinate system specified by parameter
*          SYSTEM. Otherwise, they are aligned in the ICRS co-ordinate
*          system. For instance, if the output co-ordinate system is
*          AZEL, then setting ALIGNSYS to TRUE will result in the AZEL
*          values of the input data positions being compared directly,
*          disregarding the fact that a given AZEL will correspond to
*          different positions on the sky at different times. [FALSE]
*     AUTOGRID = _LOGICAL (Read)
*          Only accessed if a null value is supplied for parameter REF.
*          Determines how the dynamic default values should be determined
*          for the projection parameters CROTA, PIXSIZE, REFLAT, REFLON,
*          REFPIX1 and REFPIX2. If TRUE, then default projection parameters
*          are determined by adjusting the grid until as many data samples as
*          possible fall close to the centre of pixels in the output cube. If
*          FALSE, REFLON/REFLAT are set to the first pointing BASE position,
*          CROTA is set to the MAP_PA value in the FITS header (converted
*          to the requested sky co-ordinate system), PIXSIZE is set to 6
*          arcseconds, and REFPIX1/REFPIX2 are both set to zero. [FALSE]
*     REFPIX1 = _DOUBLE (Read)
*          Controls the precise placement of the spatial tangent point on
*          the first pixel axis of the output cube. The position of the
*          tangent point on the sky is specified by REFLON/REFLAT, and
*          this sky position is placed at grid coordinates specified by
*          REFPIX1/REFPIX2. Note, these grid coordinates refer to an
*          interim grid coordinate system that does not depend on the
*          values supplied for LBND, rather than the final grid coordinate
*          system of the output cube. Therefore, if values are supplied for
*          REFPIX1/REFPIX2, they should be copies of the values written to
*          output parameter PIXREF by a previous run of MAKECUBE. The
*          REFPIX and PIXREF parameters allow an initial run of MAKECUBE
*          with AUTOGRID=YES to generate projection parameters that can
*          then be re-used in subsequent runs of MAKECUBE with
*          AUTOGRID=NO in order to force MAKECUBE to use the same pixel
*          grid. If a null (!) value is supplied, default values will be
*          used for REFPIX1/2 - either the autogrid values (if AUTOGRID=YES)
*          or (0,0) (if AUTOGRID=NO). [!]
*     REFPIX2 = _DOUBLE (Read)
*          Controls the precise placement of the spatial tangent point on
*          the second pixel axis of the output cube. See REFPIX1. [!]
*     BADMASK = LITERAL (Read)
*          A string determining the way in which bad pixels are propagated
*          from input to output. The "AND" scheme uses all input data (thus
*          reducing the noise in the output) and also minimises the number of
*          bad pixels in the output. However, the memory requirements of the
*          "AND" scheme can be excessive. For this reason, two other schemes,
*          "FIRST" and "OR", are provided which greatly reduce the memory
*          requirements, at the expense either of introducing more bad pixels
*          into the output ("OR") or producing higher output noise levels
*          ("FIRST"). The value supplied for this parameter is used only if
*          SPREAD is set to "Nearest" (otherwise "AND" is always used):
*
*          - "FIRST" -- The bad-pixel mask in each output spectrum is
*          inherited from the first input spectrum that contributes to the
*          output spectrum. Any subsequent input spectra that contribute
*          to the same output spectrum but which have a different bad-pixel
*          mask are ignored. So an output pixel will be bad if and only if
*          the corresponding pixel in the first input NDF that contributes
*          to it is bad. Since this scheme ignores entire input spectra
*          if they do not conform to the expected bad-pixel mask, the noise
*          in the output can be higher than using the other schemes. However,
*          this scheme has the benefit of using much less memory than the
*          "AND" scheme, and will in general produce fewer bad pixels in
*          the output than the "OR" scheme.
*
*          - "OR" -- The bad pixel mask in each output spectrum is the union
*          (logical OR) of the bad pixel masks for all input spectra that
*          contribute to the output spectrum. So an output pixel will be
*          bad if any of the input pixels that contribute to it are bad.
*          This scheme will in general produce more bad output pixels than
*          the "FIRST" scheme, but the non-bad output pixels will have a
*          lower noise because, unlike "FIRST", all the contributing input
*          data are coadded to produce the good output pixels. Like "FIRST",
*          this scheme uses much less memory than "AND".
*
*          - "AND" -- The bad pixel mask for each output spectrum is the
*          intersection (logical AND) of the bad pixel masks for all input
*          spectra that contribute to the output spectrum. So an output
*          pixel will be bad only if all the input pixels that contribute to
*          it are bad. This scheme will produce fewer bad output pixels
*          and will also give lower output noise levels than "FIRST" or "OR",
*          but at the expense of much greater memory requirements.
*
*          ["OR"]
*     CATFRAME = LITERAL (Read)
*          A string determining the co-ordinate Frame in which positions are
*          to be stored in the output catalogue associated with parameter
*          OUTCAT. The string supplied for CATFRAME can be one of the
*          following:
*
*          - A Domain name such as SKY, AXIS, PIXEL, etc.
*
*          - An integer value giving the index of the required Frame.
*
*          - An IRAS90 Sky Co-ordinate System (SCS) values such as
*          EQUAT(J2000) (see SUN/163).
*
*          If a null (!) value is supplied, the positions will be stored
*          in the current Frame of the output NDF. [!]
*     CATEPOCH = _DOUBLE (Read)
*          The epoch at which the sky positions stored in the output
*          catalogue were determined. It will only be accessed if an epoch
*          value is needed to qualify the co-ordinate Frame specified by
*          COLFRAME. If required, it should be given as a decimal years
*          value, with or without decimal places ("1996.8" for example).
*          Such values are interpreted as a Besselian epoch if less than
*          1984.0 and as a Julian epoch otherwise.
*     CROTA = _REAL (Read)
*          Only accessed if a null value is supplied for parameter REF.
*          The angle, in degrees, from north through east (in the
*          co-ordinate system specified by the SYSTEM parameter) to the second
*          pixel axis in the output cube. The dynamic default value is
*          determined by the AUTOGRID parameter. []
*     DETECTORS = LITERAL (Read)
*          A group of detector names to include in, or exclude from, the
*          output cube. If the first name starts with a minus sign, then
*          the specified detectors are excluded from the output cube (all
*          other detectors are included). Otherwise, the specified detectors
*          are included in the output cube (all other detectors are
*          excluded). If a null (!) value is supplied, data from all detectors
*          will be used. [!]
*     EXTRACOLS = LITERAL (Read)
*          A group of names specifying extra columns to be added to the
*          catalogue specified by parameter OUTCAT. Each name should be
*          the name of a component in the JCMTState extension structure.
*          For each name in the group, an extra column is added to the
*          output catalogue containing the value of the named extension
*          item for every table row (i.e. for each data sample). These
*          extra columns can be viewed and manipulated with general-purpose
*          FITS table tools such as TOPCAT, but will not be displayed by
*          the KAPPA:LISTSHOW command. One use for these extra columns is
*          to allow the catalogue to be filtered (e.g. by TOPCAT) to
*          remove samples that meet (or do not meet) some specified
*          requirement specified by the JCMTState contents. No extra
*          columns are added if a null (!) value is supplied. [!]
*     FBL( ) = _DOUBLE (Write)
*          Sky co-ordinates (radians) of the bottom-left corner of the
*          output cube (the corner with the smallest PIXEL dimension
*          for Axis 1 and the smallest pixel dimension for Axis 2). No
*          check is made that the pixel corresponds to valid data. Note
*          that the position is reported for the centre of the pixel.
*          If SPARSE mode is enabled the positions reported will not
*          be reliable.
*     FBR( ) = _DOUBLE (Write)
*          Sky co-ordinates (radians) of the bottom right corner of the
*          output cube (the corner with the largest PIXEL dimension
*          for Axis 1 and the smallest pixel dimension for Axis 2). No
*          check is made that the pixel corresponds to valid data. Note
*          that the position is reported for the centre of the pixel.
*          If SPARSE mode is enabled the positions reported will not
*          be reliable.
*     FLBND( ) = _DOUBLE (Write)
*          The lower bounds of the bounding box enclosing the output
*          cube in the selected output WCS Frame. The values are
*          calculated even if no output cube is created. Celestial
*          axis values will be in units of radians, spectral-axis
*          units will be in the same units as the input frameset
*          (matching those used in the SPECBOUNDS parameter). The
*          parameter is named to be consistent with KAPPA:NDFTRACE
*          output. Note, the stored values correspond to the outer edges
*          of the first pixel, not to the pixel centre.
*     FUBND( ) = _DOUBLE (Write)
*          The upper bounds of the bounding box enclosing the output
*          cube in the selected output WCS Frame. The values are
*          calculated even if no output cube is created. Celestial
*          axis values will be in units of radians, spectral-axis
*          units will be in the same units of the input frameset
*          (matching those used in the SPECBOUNDS parameter). The
*          parameter is named to be consistent with KAPPA:NDFTRACE
*          output. Note, the stored values correspond to the outer edges
*          of the first pixel, not to the pixel centre.
*     FTL( ) = _DOUBLE (Write)
*          Sky co-ordinates (radians) of the top left corner of the
*          output cube (the corner with the smallest PIXEL dimension
*          for Axis 1 and the largest pixel dimension for Axis 2). No
*          check is made that the pixel corresponds to valid data. Note
*          that the position is reported for the centre of the pixel.
*          If SPARSE mode is enabled the positions reported will not
*          be reliable.
*     FTR( ) = _DOUBLE (Write)
*          Sky co-ordinates (radians) of the top right corner of the
*          output cube (the corner with the largest PIXEL dimension
*          for Axis 1 and the largest pixel dimension for Axis 2). No
*          check is made that the pixel corresponds to valid data. Note
*          that the position is reported for the centre of the pixel.
*          If SPARSE mode is enabled the positions reported will not
*          be reliable.
*     GENVAR = LITERAL (Read)
*          Indicates how the Variance values in the output NDF are to be
*          calculated. It can take any of the following values:
*
*          - "Spread" -- the output Variance values are based on the spread
*          of input data values contributing to each output pixel. This option
*          is not available if parameter SPARSE is set TRUE. If the BADMASK
*          value is "OR" or "FIRST", then a single variance value will be
*          produced for each output spectrum (i.e. all channels in an output
*          spectrum will have the same variance value). If BADMASK is "AND",
*          then an independent variance value will be calculated for each
*          channel in each output spectrum.
*
*          - "Tsys" -- the output Variance values are based on the system
*          noise temperature values supplied in the input NDFs. Since
*          each input spectrum is characterised by a single Tsys value,
*          each output spectrum will have a constant Variance value (i.e.
*          all channels in an output spectrum will have the same variance
*          value).
*
*          - "None" -- no output Variance values are created.
*
*          ["Tsys"]
*
*     IN = NDF (Read)
*          Input raw data file(s)
*     INWEIGHT = _LOGICAL (Read)
*          Indicates if the input spectra should be weighted when combining
*          two or more input spectra together to form an output spectrum.
*          If TRUE, the weights used are the reciprocal of the variances
*          associated with the input spectra, as determined from the Tsys
*          values in the input. [TRUE]
*     JSATILES = _LOGICAL (Read)
*          If TRUE, the output cube is created on the JSA all-sky pixel
*          grid, and is split up into individual JSA tiles. Thus multiple
*          output NDFs may be created, one for each JSA tile that touches
*          the cube. Each of these output NDFs will have the tile index number
*          appended to the end of the path specified by parameter "OUT". If
*          "JSATILES" is TRUE, the "REF" parameter is ignored. [FALSE]
*     JSATILELIST() = _INTEGER (Write)
*          If parameter "JSATILES" is set TRUE, the zero-based indices of
*          the created JSA tiles will be written to this output parameter.
*          The number of such indices is given the "NTILE" parameter
*     LBND( 2 ) = _INTEGER (Read)
*          An array of values giving the lower pixel-index bound on each
*          spatial axis of the output NDF. The suggested default values
*          encompass all the input spatial information. The supplied
*          bounds may be modified if the parameter TRIM takes its default
*          value of TRUE. []
*     LBOUND( 3 ) = _INTEGER (Write)
*          The lower pixel bounds of the output NDF. Note, values will be
*          written to this output parameter even if a null value is supplied
*          for parameter OUT.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     NTILE = _INTEGER (Write)
*          The number of output tiles used to hold the entire output
*          array (see parameter JSATILES and TILEDIMS). If no input data
*          falls within a specified tile, then no output NDF will be created
*          for the tile, but (if JSATILES is FALSE) the tile will still be
*          included in the tile numbering scheme.
*     NPOLBIN = _INTEGER (Write)
*          The number of polarisation angle bins used to hold the entire
*          output data (see parameter POLBINSIZE).
*     OUT = NDF (Write)
*          Output file. If a null (!) value is supplied, the application
*          will terminate early without creating an output cube, but
*          without reporting an error. Note, the pixel bounds which the
*          output cube would have had will still be written to output
*          parameters LBOUND and UBOUND, even if a null value is supplied
*          for OUT. If the output cube is split up into multiple output NDFs
*          (e.g. an NDF for each tile -- see parameter TILEDIMS -- or for each
*          polarisation angle bin -- see parameter POLBINSIZE), then the
*          value supplied for "OUT" will be used as the root name to which
*          other strings are appended to create the name of each output NDF.
*     OUTCAT = FILENAME (Write)
*          An output catalogue in which to store all the spatial detector
*          positions used to make the output cube (i.e. those selected using
*          the DETECTORS parameter). By default, the stored positions are in
*          the same sky co-ordinate system as the current Frame in the output
*          NDF (but see parameter CATFRAME). The label associated with each
*          row in the catalogue is the detector name. The detector positions
*          in the catalogue are ordered as follows: all the positions for
*          the first input NDF come first, followed by those for the second
*          input NDF, etc. Within the group of positions associated with a
*          single input NDF, the positions for the first time slice come
*          first, followed by the positions for the second time slice, etc.
*          If a null value (!) is supplied, no output catalogue is produced.
*          See also parameter CATFRAME. [!]
*     OUTFILES = LITERAL (Write)
*          The name of text file to create, in which to put the names of
*          all the output NDFs created by this application via parameter
*          OUT (one per line). If a null (!) value is supplied no file is
*          created. [!]
*     PARAMS( 2 ) = _DOUBLE (Read)
*          An optional array which consists of additional parameters
*          required by the Sinc, SincSinc, SincCos, SincGauss, Somb,
*          SombCos, and Gauss spreading methods (see parameter SPREAD).
*
*          PARAMS( 1 ) is required by all the above schemes. It is used to
*          specify how many pixels on either side of the output position
*          (that is, the output position corresponding to the centre of the
*          input pixel) are to receive contributions from the input pixel.
*          Typically, a value of 2 is appropriate and the minimum allowed
*          value is 1 (i.e. one pixel on each side). A value of zero or
*          fewer indicates that a suitable number of pixels should be
*          calculated automatically. [0]
*
*          PARAMS( 2 ) is required only by the SombCos, Gauss, SincSinc,
*          SincCos, and SincGauss schemes.  For the SombCos, SincSinc, and
*          SincCos schemes, it specifies the number of pixels at which the
*          envelope of the function goes to zero.  The minimum value is
*          1.0, and the run-time default value is 2.0.  For the Gauss and
*          SincGauss scheme, it specifies the full-width at half-maximum
*          (FWHM) of the Gaussian envelope.  The minimum value is 0.1, and
*          the run-time default is 1.0.  On astronomical images and
*          spectra, good results are often obtained by approximately
*          matching the FWHM of the envelope function, given by PARAMS(2),
*          to the point-spread function of the input data.  []
*     PIXREF( 2 ) = _DOUBLE (Write)
*          The grid coordinates used for the reference pixel, within the
*          interim grid coordinate system. See REFPIX1.
*     PIXSIZE( 2 ) = _REAL (Read)
*          Only accessed if a null value is supplied for parameter REF.
*          Pixel dimensions in the output image, in arcseconds. If only one
*          value is supplied, the same value will be used for both axes. The
*          dynamic default value is determined by the AUTOGRID parameter. []
*     POLBINSIZE = _REAL (Read)
*          This parameter is only prompted for if the input files contain
*          polarisation data. The supplied value is used as the bin size
*          (in degrees) for grouping polarisation analyser angles. The
*          first bin is centred at the angle given by parameter POLBINZERO.
*          The "analyser angle" is the anti-clockwise angle from celestial
*          north (in the system chosen by parameter SYSTEM) to the axis
*          of the "effective analyser" - a rotating analyser that would
*          have the same effect as the combination of fixed analyser and
*          half-wave plate actually present in the polarimeter. The
*          supplied value for POLBINSIZE will be modified if required to
*          ensure that a whole number of bins is used to cover the
*          complete range of analyser angles (0 to 360 degrees). A
*          separate output cube will be created for each bin that is not
*          empty, and each output NDF will contain a POLPACK extension
*          suitable for use with the POLPACK:POLCAL command. These NDFs
*          are all stored in a single HDS container file (one per tile)
*          with the name specified by parameter OUT. Within this
*          container file, each cube will be held in a component with
*          name of the form "P<N>" appended to the end, where "<N>" is an
*          integer bin index. The largest value of N is written to output
*          parameter NPOLBIN. If a null value (!) is supplied, then a
*          single output NDF (without POLPACK extension) is created for
*          each tile, containing all input data.
*     POLBINZERO = _REAL (Read)
*          This parameter is only prompted for if the input files contain
*          polarisation data. It is the analyser angle (in degrees) at the
*          centre of the first analyser angle bin. A value of zero
*          corresponds to north in the celestial co-ordinate system specified
*          by parameter SYSTEM. [0]
*     POSERRFATAL = _LOGICAL (Read)
*          If a true value is supplied, then an error is reported and the
*          application terminates if a significant difference is found
*          between the detector positions array (RECEPPOS) and positions
*          implied by the FPLANEX/Y arrays. If a false value is supplied,
*          a warning is issued but the application proceeds. See also
*          parameter USEDETPOS. [FALSE]
*     REF = NDF (Read)
*          An existing NDF that is to be used to define the output grid,
*          or the string "JSA". If an NDF is supplied, the output grid will
*          be aligned with the supplied reference NDF. The NDF need not be
*          three-dimensional. For instance, a two-dimensional image can be
*          supplied in which case the spatial axes of the output cube will
*          be aligned with the reference image and the spectral axis will be
*          inherited form the first input NDF.  If "JSA" is supplied, the
*          JSA all-sky pixel grid will be used (note, the cube will still be
*          created as a single NDF - if multiple NDFs, one for each JSA tile,
*          are required, the "JSATILES" parameter should beset TRUE instead
*          of using the "REF" parameter). If a null (!) value is supplied
*          then the output grid is determined by parameters AUTOGRID, REFLON,
*          REFLAT, etc. [!]
*     REFLAT = LITERAL (Read)
*          Only accessed if a null value is supplied for parameter REF.
*          The formatted celestial-latitude value at the tangent point of
*          the spatial projection in the output cube. This should be provided
*          in the system specified by parameter SYSTEM. The dynamic-default
*          value is determined by the AUTOGRID parameter. []
*     REFLON = LITERAL (Read)
*          Only accessed if a null value is supplied for parameter REF.
*          The formatted celestial-longitude value at the tangent point of
*          the spatial projection in the output cube. This should be provided
*          in the system specified by parameter SYSTEM. The dynamic-default
*          value is determined by the AUTOGRID parameter. []
*     SPARSE = _LOGICAL (Read)
*          Indicates if the spectra in the output cube should be stored
*          as a sparse array, or as a regularly gridded array. If FALSE,
*          pixel Axes 1 and 2 of the output cube represent a regularly
*          gridded tangent plane projection of the sky, with parameters
*          determined by CROTA, PIXSIZE, REFLON and REFLAT. Each input
*          spectrum is placed at the appropriate pixel position in this
*          three-dimensional projection, as given by the celestial
*          co-ordinates associated with the spectrum. If SPARSE is TRUE, then
*          each input spectrum is given an associated index, starting from 1,
*          and the spectrum with index "I" is stored at pixel position (I,1)
*          in the output cube (pixel Axis 2 will always have the value 1 --
*          that is, Axis 2 is a degenerate axis that spans only a single
*          pixel).
*
*          In both cases, the third pixel axis in the output cube
*          corresponds to spectral position (frequency, velocity, etc).
*
*          Whatever the setting of SPARSE, the output NDF's WCS component
*          can be used to transform pixel position into the corresponding
*          (celestial longitude, celestial latitude, spectral position)
*          values. However, if SPARSE is TRUE, then the inverse transformation
*          (i.e. from (long,lat,spec) to pixel co-ordinates) will not be
*          defined. This means, for instance, that if a sparse array is
*          displayed as a two-dimensional image, then it will not be possible
*          to annotate the axes with WCS values. Also, whilst KAPPA:WCSMOSAIC
*          will succesfully align the data in a sparse array with a
*          regularly gridded cube, KAPPA:WCSALIGN will not, since WCSALIGN
*          needs the inverse transformation to be defined.
*
*          The dynamic default value for SPARSE depends on the value
*          supplied for parameter AUTOGRID. If AUTOGRID is set FALSE,
*          then SPARSE defaults to FALSE. If AUTOGRID is set TRUE, then
*          the default for SPARSE will be TRUE if the algorithm described
*          under the AUTOGRID parameter fails to find useful default grid
*          parameters. If the AUTOGRID algorithm succeeds, the default
*          for SPARSE will be FALSE. []
*     SPECBOUNDS = LITERAL (Read)
*          The bounds of the output cube on the spectral axis. Input data
*          that falls outside the supplied range will not be included in
*          the output cube. The supplied parameter value should be a
*          string containing a pair of axis values separated by white space
*          or commas. The first should be the spectral value corresponding to
*          the lower edge of the first spectral channel in the output cube, and
*          the second should be the spectral value corresponding to the upper
*          edge of the last spectral channel. The supplied values should refer
*          to the spectral system described by the WCS FrameSet of the first
*          input NDF. To see what this is, supply a single colon (":") for the
*          parameter value. This will display a description of the required
*          spectral co-ordinate system, and then re-prompt for a new parameter
*          value. The dynamic default is determined by the SPECUNION
*          parameter. []
*     SPECUNION = _LOGICAL (Read)
*          Determines how the default spectral bounds for the output are
*          chosen. If a TRUE value is supplied, then the defaults for the
*          SPECBOUNDS parameter represent the union of the spectral ranges
*          in the input data. Otherwise, they represent the intersection
*          of the spectral ranges in the input data. This option is
*          only available if parameter BADMASK is set to AND. For any
*          other value of BADMASK, a value of FALSE is always used for
*          SPECUNION. [FALSE]
*     SPREAD = LITERAL (Read)
*          The method to use when spreading each input pixel value out
*          between a group of neighbouring output pixels. If SPARSE is set
*          TRUE, then SPREAD is not accessed and a value of "Nearest" is
*          always assumed. SPREAD can take the following values:
*
*          - "Linear" -- The input pixel value is divided bi-linearly between
*          the four nearest output pixels.  Produces smoother output NDFs than
*          the nearest-neighbour scheme.
*
*          - "Nearest" -- The input pixel value is assigned completely to the
*          single nearest output pixel. This scheme is much faster than any
*          of the others.
*
*          - "Sinc" -- Uses the sinc(pi*x) kernel, where x is the pixel
*          offset from the interpolation point (resampling) or transformed
*          input pixel centre (rebinning), and sinc(z)=sin(z)/z.  Use of
*          this scheme is not recommended.
*
*          - "SincSinc" -- Uses the sinc(pi*x)sinc(k*pi*x) kernel. A
*          valuable general-purpose scheme, intermediate in its visual
*          effect on NDFs between the bi-linear and nearest-neighbour
*          schemes.
*
*          - "SincCos" -- Uses the sinc(pi*x)cos(k*pi*x) kernel.  Gives
*          similar results to the "Sincsinc" scheme.
*
*          - "SincGauss" -- Uses the sinc(pi*x)exp(-k*x*x) kernel.  Good
*          results can be obtained by matching the FWHM of the
*          envelope function to the point-spread function of the
*          input data (see parameter PARAMS).
*
*          - "Somb" -- Uses the somb(pi*x) kernel, where x is the pixel
*          offset from the transformed input pixel centre, and
*          somb(z)=2*J1(z)/z (J1 is the first-order Bessel function of the
*          first kind).  This scheme is similar to the "Sinc" scheme.
*
*          - "SombCos" -- Uses the somb(pi*x)cos(k*pi*x) kernel.  This
*          scheme is similar to the "SincCos" scheme.
*
*          - "Gauss" -- Uses the exp(-k*x*x) kernel. The FWHM of the Gaussian
*          is given by parameter PARAMS(2), and the point at which to truncate
*          the Gaussian to zero is given by parameter PARAMS(1).
*
*          For further details of these schemes, see the descriptions of
*          routine AST_REBINx in SUN/211. ["Nearest"]
*     SYSTEM = LITERAL (Read)
*          The celestial co-ordinate system for the output cube. One of
*          ICRS, GAPPT, FK5, FK4, FK4-NO-E, AZEL, GALACTIC, ECLIPTIC. It
*          can also be given the value "TRACKING", in which case the
*          system used will be which ever system was used as the tracking
*          system during in the observation. The value supplied for the
*          CROTA parameter should refer to the co-ordinate system specified
*          by this parameter.
*
*          The choice of system also determines if the telescope is
*          considered to be tracking a moving object such as a planet or
*          asteroid. If system is GAPPT or AZEL, then each time slice in
*          the input data will be shifted in order to put the base
*          telescope position (given by TCS_AZ_BC1/2 in the JCMTSTATE
*          extension of the input NDF) at the same pixel position that it
*          had for the first time slice. For any other system, no such
*          shifts are applied, even if the base telescope position is
*          changing through the observation. [TRACKING]
*     TILEBORDER = _INTEGER (Read)
*          Only accessed if a non-null value is supplied for parameter
*          TILEDIMS. It gives the width, in pixels, of a border to add to
*          each output tile. These borders contain data from the adjacent
*          tile. This results in an overlap between adjacent tiles equal to
*          twice the supplied border width. If the default value of zero
*          is accepted, then output tiles will abut each other in pixel
*          space without any overlap. If a non-zero value is supplied,
*          then each pair of adjacent tiles will overlap by twice the
*          given number of pixels. Pixels within the overlap border will
*          be given a quality name of "BORDER" (see KAPPA:SHOWQUAL). [0]
*     TILEDIMS( 2 ) = _INTEGER (Read)
*          This parameter is ignored if parameter "JSATILES" is set TRUE.
*
*          For large data sets, it may sometimes be beneficial to break
*          the output array up into a number of smaller rectangular tiles,
*          each created separately and stored in a separate output NDF. This
*          can be accomplished by supplying non-null values for the TILEDIMS
*          parameter. If supplied, these values give the nominal spatial size
*          of each output tile, in pixels. Edge tiles may be thinner if the
*          TRIMTILES parameter is set TRUE. In order to avoid creating very thin
*          tiles around the edges, the actual tile size used for the edge tiles
*          may be up to 10 % larger than the supplied value. This creation of
*          "fat" edge tiles may be prevented by supplying a negative value for
*          the tile size, in which case edge tiles will never be wider than
*          the supplied absolute value.
*
*          If only one value is supplied, the supplied value is duplicated to
*          create square tiles. Tiles are created in a raster fashion, from
*          bottom left to top right of the spatial extent. The NDF file name
*          specified by "out" is modified for each tile by appending "_<N>"
*          to the end of it, where <N> is the integer tile index (starting at
*          1). The number of tiles used to cover the entire output cube is
*          written to output parameter NTILES. The tiles all share the same
*          projection and so can be simply pasted together in pixel
*          co-ordinates to reconstruct the full size output array. The tiles
*          are centred so that the reference position (given by REFLON and
*          REFLAT) falls at the centre of a tile. If a tile receives no
*          input data, then no corresponding output NDF is created, but
*          the tile is still included in the tile numbering scheme. If a
*          null (!) value is supplied for TILEDIMS, then the
*          entire output array is created as a single tile and stored in
*          a single output NDF with the name given by parameter OUT
*          (without any "_<N>" appendage). [!]
*     TRIM = _LOGICAL (Read)
*          If TRUE, then the output cube will be trimmed to exclude any
*          borders filled with bad values. Such borders can be caused, for
*          instance, by one or more detectors having been excluded (see
*          parameter DETECTORS), or by the supplied LBND and/or UBND parameter
*          values extending beyond the available data. [TRUE]
*     TRIMTILES = _LOGICAL (Read)
*          Only accessed if the output is being split up into more than
*          one spatial tile (see parameter TILEDIMS and JSATILES). If TRUE,
*          then the tiles around the border will be trimmed to exclude areas
*          that fall outside the bounds of the full sized output array. This
*          will result in the border tiles being smaller than the central
*          tiles. [FALSE]
*     UBND( 2 ) = _INTEGER (Read)
*          An array of values giving the upper pixel-index bound on each
*          spatial axis of the output NDF. The suggested default values
*          encompass all the input spatial information. The supplied
*          bounds may be modified if the parameter TRIM takes its default
*          value of TRUE. []
*     UBOUND( 3 ) = _INTEGER (Write)
*          The upper pixel bounds of the output NDF. Note, values will be
*          written to this output parameter even if a null value is supplied
*          for parameter OUT.
*     USEDETPOS = _LOGICAL (Read)
*          If a true value is supplied, then the detector positions are
*          read from the detector position arrays in each input NDF.
*          Otherwise, the detector positions are calculated on the basis
*          of the FPLANEX/Y arrays. Both methods should (in the absence
*          of bugs) result in identical cubes. See also parameter
*          POSERRFATAL. [TRUE]
*     WEIGHTS = _LOGICAL (Read)
*          If TRUE, then the weights associated with the array of output
*          pixels are stored in an extension named ACSISRED, within the output
*          NDF. If FALSE the weights are discarded once they have been
*          used. These weights record the relative weight of the input
*          data associated with each output pixel. If SPARSE is set TRUE,
*          then WEIGHTS is not accessed and a FALSE value is assumed. [FALSE]

*  Notes:
*     - A FITS extension is added to the output NDF containing any keywords
*     that are common to all input NDFs. To be included in the output
*     FITS extension, a FITS keyword must be present in the NDF extension
*     of every input NDF, and it must have the same value in all input
*     NDFs. In addition, certain headers that relate to start and end
*     events are propagated from the oldest and newest files respectively.
*     - The output NDF will contain an extension named "SMURF" containing
*     two NDFs named "EXP_TIME" and "EFF_TIME". In addition, if parameter
*     SPREAD is set to "Nearest", a third NDF called "TSYS" will be created.
*     Each of these NDFs is 2-dimensional, with the same pixel bounds as the
*     spatial axes of the main output NDF, so that a pixel in one of these
*     NDFs corresponds to a spectrum in the main output NDF. EXP_TIME holds
*     the sum of the total exposure times (Ton + Toff) for the input spectra
*     that contributed to each output spectrum. EFF_TIME holds the sum of the
*     effective integration times (Teff) for the input spectra that contributed
*     to each output spectrum, scaled up by a factor of 4 in order to normalise
*     it to the reported exposure times in EXP_TIME. TSYS holds the effective
*     system temperature for each output spectrum. The TSYS array is not
*     created if GENVAR is "None" or if SPREAD is not "Nearest".
*     - FITS keywords EXP_TIME, EFF_TIME and MEDTSYS are added to the output
*     FITS extension. The EXP_TIME and EFF_TIME keywords hold the median
*     values of the EXP_TIME and EFF_TIME arrays (stored in the SMURF extension
*     of the output NDF). The MEDTSYS keyword holds the median value of the
*     TSYS array (also stored in the SMURF extension of the output NDF). If
*     any of these values cannot be calculated for any reason, the
*     corresponding FITS keyword is assigned a blank value.
*     - FITS keywords NUMTILES and TILENUM are added to the output FITS
*     header. These are the number of tiles used to hold the output data,
*     and the index of the NDF containing the header, in the range 1 to
*     NUMTILES. See parameter TILEDIMS.

*  Related Applications:
*     SMURF: TIMESORT

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     David Berry (JAC, UCLan)
*     Brad Cavanagh (JAC, Hawaii)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     14-SEP-2006 (TIMJ):
*        Clone from smurf_makemap
*     18-SEP-2006 (DSB):
*        MAKECUBE code added.
*     6-NOV-2006 (DSB):
*        Added parameter DETECTORS.
*     10-NOV-2006 (DSB):
*        Added HISTORY component to output NDF.
*     14-NOV-2006 (DSB):
*        Added AUTOGRID parameter.
*     20-NOV-2006 (DSB):
*        - Make the DETECTORS parameter case insensitive.
*        - Document label column in OUTCAT.
*     21-NOV-2006 (DSB):
*        AUTOGRID now supplies the dynamic defaults for the projection
*        parametersm which are now acquired after AUTOGRID.
*     23-NOV-2006 (DSB):
*        - SYSTEM can now accept any AST celestial System name.
*        - Fix incorrect indices for "pixsize" array when checking pixel
*        sizes.
*     24-NOV-2006 (DSB):
*        Added PARAMS and SPREAD parameters.
*     28-NOV-2006 (DSB):
*        - Propagate Label and Unit components from first input NDF to the
*        output NDF.
*        - Added WEIGHTS parameter.
*        - Added GENVAR parameter.
*     29-NOV-2006 (DSB):
*        We do not need a double size weights array if GENVAR is FALSE.
*     30-NOV-2006 (DSB):
*        Added parameter SPARSE.
*     6-DEC-2006 (DSB):
*        Add detgrp to the smf_cubegrid argument list.
*     13-DEC-2006 (DSB):
*        Allow output variances to be caclulated on the basis of the system
*        noise temperature values in the input NDFs.
*     21-DEC-2006 (DSB):
*        Set the spatial output FrameSet to represent offsets from first
*        base pointing position if the target is moving.
*     11-JAN-2007 (DSB):
*        Aded parameters LBOUND and UBOUND, and allowed a null value to
*        be supplied for OUT.
*     11-JAN-2007 (TIMJ):
*        Added FLBND and FUBND. Add FTL, FTR, FBL, FBR parameters.
*     12-JAN-2007 (DSB):
*        Add reporting of axis labels.
*     16-JAN-2007 (DSB):
*        Use 2D variance and weights arrays where possible.
*     22-JAN-2007 (DSB):
*        Modified to accomodate changes to argument lists for smf_cubegrid,
*        smf_cubebounds and smf_rebincube, which provide better handling
*        of moving sources.
*     25-JAN-2007 (DSB):
*        Remove duplicated code for getting parameter "SPARSE".
*     7-FEB-2007 (DSB):
*        Store median exposure time int he output NDF FITS extension.
*     8-FEB-2007 (DSB):
*        - Create a SMURF extension in the output holding arrays EXP_TIME,
*        ON_TIME and TSYS.
*        - Store the median output TSYS value in the output FITS extension.
*        - Find FITS headers that are present and have the same value in all
*        input NDFs, and add them to the output NDF's FITS extension.
*     12-FEB-2007 (DSB):
*        Added parameter INWEIGHT.
*     21-FEB-2007 (DSB):
*        - Changed ON_TIME to EFF_TIME.
*        - Added EFF_TIME FITS header to output.
*     7-MAR-2007 (BC):
*        - Added input OBSID FITS header tracking through PROVCNT and
*        OBSnnnnn output FITS headers.
*     16-MAR-2007 (DSB):
*        Extend use of INWEIGHT to all spreading schemes.
*     20-MAR-2007 (TIMJ):
*        Factor out output FITS header code.
*     28-MAR-2007 (DSB):
*        - Expand documentation for INWEIGHT, and warn user if the supplied
*        INWEIGHT value cannot be used.
*        - Set the pixel origin of the weights NDF to be the same as the
*        pixel origin of the main output NDF.
*        - Erase the output NDF variance array if less than 10% of the
*        good output data values have good output variances.
*     14-APR-2007 (DSB):
*        Warn user about rejected input spectra.
*     24-APR-2007 (DSB):
*        Add parameter BADMASK.
*     2-MAY-2007 (DSB):
*        Modify the message about rejected spectra to indicate how many
*        input spectra there were in total.
*     8-MAY-2007 (DSB):
*        Change default BADMASK to "OR".
*     22-JUN-2007 (TIMJ):
*        Rework to handle PRV* as well as OBS*
*     06-JUL-2007 (TIMJ):
*        Use smf_accumulate_prov
*     21-SEP-2007 (DSB):
*        Move reporting of WCS bounds to be before the prompt for OUT.
*     11-OCT-2007 (DSB):
*        Create a separate output cube for each polarisation angle bin.
*     19-OCT-2007 (DSB):
*        - Added NUMTILES and TILENUM FITS headers.
*        - Added SPECUNION parameter.
*     25-OCT-2007 (DSB):
*        Modify description of DETECTORS parameter.
*     29-OCT-2007 (DSB):
*        Added Label and Unit values to the extension NDFs.
*     29-OCT-2007 (EC):
*        Modified interface to smf_open_file.
*     2-NOV-2007 (DSB):
*        Added provenance information to output NDFs.
*     7-NOV-2007 (DSB):
*        Correct size to tile->size in call to smf_rebincube.
*     26-NOV-2007 (DSB):
*        Pass output FrameSet to smf_choosetiles so that tiles can be
*        centred on ref point.
*     4-DEC-2007 (DSB):
*        Added parameter TRIMTILES.
*     6-DEC-2007 (DSB):
*        Clarify docs for BADMASK.
*     18-DEC-2007 (DSB):
*        Added parameter REF.
*     18-DEC-2007 (AGG):
*        Update to use new smf_free behaviour
*     19-DEC-2007 (DSB):
*        Correct the way reference WCS is handled.
*     7-JAN-2008 (DSB):
*        Added LBND and UBND.
*     9-JAN-2008 (DSB):
*        Do not create empty output tiles.
*     15-JAN-2008 (DSB):
*        Ensure that the output tile names are in line with the ITILE value
*        stored in the SMURF extension even when some output tiles are
*        skipped due to being empty.
*     17-JAN-2008 (DSB):
*        - Only create the output Tsys array if SPREAD is Nearest.
*        - Added parameter ALIGNSYS.
*     6-FEB-2008 (DSB):
*        Write all output polarisation cubes to a single container file.
*     12-FEB-2008 (DSB):
*        - Take account of the fact that the first input file to be pasted
*        into the output cube is not necessarily the first input file
*        supplied. The same applies to the last input file pasted into the
*        output cube.
*        - Add parameter POLBINZERO.
*     13-FEB-2008 (DSB):
*        Move the inversion of "tskymap" (the output GRID<->SKY Mapping)
*        outside the polarisation bin loop.
*     13-FEB-2008 (AGG):
*        Factor out code for setting pixel-spreading parameters into new routine
*     15-FEB-2008 (DSB):
*        - Expand the GENVAR documentation.
*        - Display the bin angle for each polarisation bin.
*     10-APR-2008 (DSB):
*        Add parameter OUTFILES.
*     25-APR-2008 (DSB):
*        Do not put OBSxxx and PRVxxx keywords into the output NDF FITS
*        extensions any more since ndf2fits now creates these on the basis
*        of the PROVENANCE extenson.
*     26-MAY-2008 (EC):
*        Added is2d parameter to smf_choosetiles
*     04-JUN-2008 (TIMJ):
*        Factor out WCS bounds reporting into separate function.
*     05-JUN-2008 (EC):
*        Removed is2d from smf_choosetiles interface
*     06-JUN-2008 (TIMJ):
*        Change smf_open_ndfname API.
*        Use smf_expand_tilegroup
*     24-APR-2009 (TIMJ):
*        Now summarizes the input observations.
*     3-JUL-2009 (DSB):
*        Added EXTRACOLS parameter.
*     3-SEP-2009 (DSB):
*        If the target is moving, set AlignOffset non-zero in the output
*        current Frame.
*     4-OCT-2009 (DSB)
*        Allow the supplied TIMEDIMS value to be changed by up to 10% to
*        avoid creating thin tiles around the edges.
*     14-MAR-2011 (DSB)
*        If poarameter TRIM is true, then trim bad borders no matter what
*        causes them (previously only bad borders caused by excluded
*        detectors were trimmed).
*     26-MAR-2011 (DSB)
*        Added parameters REFPIX1, REFPIX2 and PIXREF.
*     20-MAY-2011 (DSB)
*        If TRIM is TRUE, annul error caused by empty output tiles, and
*        proceed to produce remaining tiles.
*     11-APR-2012 (DSB):
*        If an output NDF is not created because it contains no good
*        data, then remove it's name from the group written to the text file
*        specified by the OUTFILES parameter.
*     7-MAY-2013 (DSB):
*        Report an error if no NDFs are created.
*     21-AUG-2013 (AGG):
*        Do not call grpList if no output files are generated. This
*        avoids a GRP__INVID error in such cases.
*     11-NOV-2013 (DSB):
*        Added the "JSATILES" parameter, and made other changes to allow
*        the output cubew to be split up into JSA tiles.
*     27-NOV-2013 (DSB):
*        - Ensure the NTILE parameter is written before the OUT parameter is
*        accessed (unless JSA tiles are being created).
*        - Added parameter POSERRFATAL.
*     14-OCT-2014 (DSB):
*        Handle cases where the target is moving but the output cube has
*        absolute sky coords (e.g. when creating JSA tiles for moving targets).

*  Copyright:
*     Copyright (C) 2007-2014 Science and Technology Facilities Council.
*     Copyright (C) 2006-2007 Particle Physics and Astronomy Research
*     Council. Copyright (C) 2006-2008,2013 University of British Columbia.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/


#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>
#include <stdint.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/atl.h"
#include "star/kaplibs.h"
#include "star/thr.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"
#include "libsmf/smf_err.h"
#include "libsmf/jsatiles.h"

#define FUNC_NAME "smurf_makecube"
#define TASK_NAME "MAKECUBE"
#define LEN__METHOD 20

void smurf_makecube( int *status ) {

/* Local Variables */
   AstFitsChan *fchan = NULL; /* FitsChan holding output NDF FITS extension */
   AstFrame *ospecfrm = NULL; /* SpecFrame from the output WCS Frameset */
   AstFrame *tfrm = NULL;     /* Current Frame from output WCS */
   AstFrameSet *spacerefwcs = NULL;/* WCS Frameset for spatial reference axes */
   AstFrameSet *specrefwcs = NULL;/* WCS Frameset for spectral reference axis */
   AstFrameSet *wcsout = NULL;/* WCS Frameset for output cube */
   AstFrameSet *wcsout2d = NULL;/* WCS Frameset describing 2D spatial axes */
   AstFrameSet *wcstile = NULL;/* WCS Frameset for output tile */
   AstFrameSet *wcstile2d = NULL;/* WCS Frameset describing 2D spatial axes */
   AstMapping *oskymap = NULL;/* GRID->SkyFrame Mapping from output WCS */
   AstMapping *ospecmap = NULL;/* GRID->SpecFrame Mapping from output WCS */
   AstMapping *tmap = NULL;   /* Base->current Mapping from output WCS */
   AstMapping *tskymap = NULL;/* GRID->SkyFrame Mapping from output tile WCS */
   AstSkyFrame *abskyfrm = NULL;/* Output SkyFrame (always absolute) */
   AstSkyFrame *oskyfrm = NULL;/* SkyFrame from the output WCS Frameset */
   Grp *detgrp = NULL;        /* Group of detector names */
   Grp *igrp = NULL;          /* Group of input files */
   Grp *igrp4 = NULL;         /* Group holding output NDF names */
   Grp *ogrp = NULL;          /* Group containing output file */
   HDSLoc *smurf_xloc = NULL; /* HDS locator for output SMURF extension */
   HDSLoc *weightsloc = NULL; /* HDS locator of weights array */
   ThrWorkForce *wf = NULL;   /* Pointer to a pool of worker threads */
   char *pname = NULL;        /* Name of currently opened data file */
   char basename[ GRP__SZNAM + 1 ]; /* Output base file name */
   char oname[SMF_PATH_MAX+1];/* Name of output NDF */
   char pabuf[ 10 ];          /* Text buffer for parameter value */
   char system[ 10 ];         /* Celestial coord system for output cube */
   double *pangle;            /* Ptr to array holding angle for each pol bin */
   double aref = AST__BAD;    /* Absolute sky longitude of target */
   double bref = AST__BAD;    /* Absolute sky latitude of target */
   double fcon;               /* Tsys factor for file */
   double par[ 7 ];           /* Projection parameter */
   double params[ 4 ];        /* astRebinSeq parameters */
   float *eff_array = NULL;   /* Pointer to array of eff times  */
   float *exp_array = NULL;   /* Pointer to array of exp times */
   float *ipd = NULL;         /* Pointer to the next output data value */
   float *ipt = NULL;         /* Pointer to the next Tsys value */
   float *ipv = NULL;         /* Pointer to the next output variance value */
   float *ipw = NULL;         /* Pointer to the next work value */
   float *tsys_array = NULL;  /* Pointer to array of tsys values */
   float *var_array = NULL;   /* Pointer to temporary variance array */
   float *var_out = NULL;     /* Pointer to the output variance array */
   float *work2_array = NULL; /* Pointer to temporary work array */
   float median;              /* Median data value */
   float polbinsize;          /* Angular size of polarisation bins */
   float polbinzero;          /* Angle at centre of first polarisation bin */
   float teff;                /* Effective integration time */
   float var;                 /* Variance value */
   int ***ptime;              /* Holds time slice indices for each bol bin */
   int *hist = NULL;          /* Histogram array */
   int *pt;                   /* Holds time slice indices for each bol bin */
   int alignsys;              /* Align data in the output system? */
   int autogrid;              /* Determine projection parameters automatically? */
   int axes[ 2 ];             /* Indices of selected axes */
   int badmask;               /* How is the output bad pixel mask chosen? */
   int blank;                 /* Was a blank line just output? */
   int delete;                /* Delete the output cube? */
   int el0;                   /* Index of 2D array element */
   int el;                    /* Index of 3D array element */
   int first;                 /* Is this the first input file? */
   int flag;                  /* Is group expression to be continued? */
   int genvar;                /* How to create output Variances */
   int hasoffexp;             /* Any ACS_OFFEXPOSURE values found in the i/p? */
   int hastsys;               /* Have some good Tsys values been found? */
   int ifile;                 /* Input file index */
   int ilast;                 /* Index of the last input file */
   int iout;                  /* Index of next output NDF name */
   int ipbin;                 /* Index of current polarisation angle bin */
   int is2d;                  /* Is the weights array 2-dimensional? */
   int isjsa;                 /* Is the JSA all-sky pixel grid being used? */
   int ispec;                 /* Index of next spectrum within output NDF */
   int jin;                   /* Input NDF index within igrp */
   int jsatiles;              /* Create JSA tiles? */
   int junk;                  /* Unused integer */
   int lbnd_out[ 3 ];         /* Lower pixel bounds for full output map */
   int lbnd_wgt[ 4 ];         /* Lower pixel bounds for weight array */
   int moving;                /* Is the telescope base position changing? */
   int naccept;               /* Number of accepted input spectra */
   int nbad;                  /* No. of o/p pixels with good data but bad variance */
   int nel;                   /* Number of elements in 3D array */
   int ngood;                 /* No. of o/p pixels with good data */
   int nparam = 0;            /* No. of parameters required for spreading scheme */
   int npbin;                 /* No. of polarisation angle bins */
   int npos;                  /* Number of samples included in output NDF */
   int nreject;               /* Number of rejected input spectra */
   int nval;                  /* Number of parameter values supplied */
   int nwgtdim;               /* No. of axes in the weights array */
   int nxy;                   /* Number of elements in a 2D output tile */
   int ondf = NDF__NOID;      /* Output NDF identifier */
   int outax[ 2 ];            /* Indices of corresponding output axes */
   int polobs;                /* Do the input files contain polarisation data? */
   int poserrfatal;           /* Report an error if RECEPPOS and FPLANEX/Y disagree? */
   int savewgt;               /* Should weights be saved in the output NDF? */
   int smfflags;              /* Flags for smfData */
   int sparse;                /* Create a sparse output array? */
   int specunion;             /* O/p spec range = union of i/p spec ranges? */
   int spread = 0;            /* Pixel spreading method */
   int tileborder;            /* Dimensions (in pixels) of tile overlap */
   int tiledims[2];           /* Dimensions (in pixels) of each output tile */
   int tndf = NDF__NOID;      /* Temporary NDF identifier */
   int trim;                  /* Trim the output cube to exclude bad pixels? */
   int trimtiles;             /* Trim the border tiles to exclude bad pixels? */
   int ubnd_out[ 3 ];         /* Upper pixel bounds for full output map */
   int ubnd_wgt[ 4 ];         /* Upper pixel bounds for weight array */
   int use_wgt;               /* Use input variance to weight input data? */
   int usedetpos;             /* Should the detpos array be used? */
   int64_t wgtsize;           /* No. of elements in the weights array */
   int64_t nused;             /* No. of input samples pasted into output cube */
   size_t itile;              /* Output tile index */
   size_t ndet;               /* Number of detectors supplied for "DETECTORS" */
   size_t njsatile;           /* Number of output JSA tiles */
   size_t ntile;              /* Number of output tiles */
   size_t outsize;            /* Number of files in output group */
   size_t size;               /* Number of files in input group */
   smfBox *boxes = NULL;      /* Pointer to array of i/p file bounding boxes */
   smfData *data = NULL;      /* Pointer to data struct */
   smfData *effdata = NULL;   /* Pointer to o/p struct holding eff time array */
   smfData *expdata = NULL;   /* Pointer to o/p struct holding exp time array */
   smfData *odata = NULL;     /* Pointer to o/p struct holding data array */
   smfData *tsysdata = NULL;  /* Pointer to o/p struct holding tsys array */
   smfData *wdata = NULL;     /* Pointer to o/p struct holding weights array */
   smfFile *file = NULL;      /* Pointer to data file struct */
   smfTile *tile = NULL;      /* Pointer to next output tile description */
   smfTile *tiles = NULL;     /* Pointer to array of output tile descriptions */
   void *data_array = NULL;   /* Pointer to the rebinned map data */
   void *wgt_array = NULL;    /* Pointer to the weights map */


#if defined(FPTRAP)
   feenableexcept(FE_DIVBYZERO|FE_INVALID|FE_OVERFLOW);
#endif

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* We have not yet displayed a blank line on stdout. */
   blank = 0;

/* Begin an NDF context (we do not begin an AST context since this is
   done within the calling monolith routine). */
   ndfBegin();

/* Find the number of cores/processors available and create a pool of
   threads of the same size. */
   wf = thrGetWorkforce( thrGetNThread( SMF__THREADS, status ), status );

/* Get a group of input files */
   ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

/* Report observation details early */
   smf_summarize_obs( igrp, status );

/* Get the celestial co-ordinate system for the output cube. */
   parChoic( "SYSTEM", "TRACKING", "TRACKING,FK5,ICRS,AZEL,GALACTIC,"
             "GAPPT,FK4,FK4-NO-E,ECLIPTIC", 1, system, 10, status );

/* See of the detector positions are to be read from the RECEPPOS array.
   Otherwise, they are calculated on the basis of the FPLANEX/Y arrays. */
   parGet0l( "USEDETPOS", &usedetpos, status );

/* Get the detectors to use. If a null value is supplied, annul the
   error. Otherwise, make the group case insensitive. */
   detgrp = NULL;
   if( *status == SAI__OK ) {
      kpg1Gtgrp( "DETECTORS", &detgrp, &ndet, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
	 if (detgrp) {
	   grpDelet( &detgrp, status );
	 }
      } else {
         grpSetcs( detgrp, 0, status );
      }
   }

/* See how the bad pixel mask in each output spectrum is to be determined. */
   parChoic( "BADMASK", "OR", "AND,OR,FIRST", 1, pabuf, 10, status );
   if( !strcmp( pabuf, "AND" ) ) {
      badmask = 2;
   } else if( !strcmp( pabuf, "OR" ) ) {
      badmask = 1;
   } else {
      badmask = 0;
   }

/* Indicate we have no projection parameters as yet. */
   par[ 0 ] = AST__BAD;
   par[ 1 ] = AST__BAD;
   par[ 2 ] = AST__BAD;
   par[ 3 ] = AST__BAD;
   par[ 4 ] = AST__BAD;
   par[ 5 ] = AST__BAD;
   par[ 6 ] = AST__BAD;

/* See if the output grp should be trimmed to exclude missing data (e.g.
   caused by detectors not being selected for inclusion via parameter
   DETECTORS). */
   parGet0l( "TRIM", &trim, status );

/* Attempt to get WCS information from a reference NDF. */
   smf_getrefwcs( "REF", igrp, &specrefwcs, &spacerefwcs, &isjsa, status );

/* If no spatial reference WCS was obtained, see if any unspecified
   projection parameters are to be determined using an optimal fitting
   process. */
   if( !spacerefwcs ) {
      parGet0l( "AUTOGRID", &autogrid, status );
   } else {
      autogrid = 0;
   }

/* See if the input data is to be aligned in the output co-ordinate system
   rather than teh default of ICRS. */
   parGet0l( "ALIGNSYS", &alignsys, status );

/* See whether any significant discrepancy between RECEPPOS and FPLANEX/Y
   should trigger a fatal error. */
   parGet0l( "POSERRFATAL", &poserrfatal, status );

/* Calculate the default grid parameters (these are only used if no
   reference spatial WCS was obtained). This also modifies the contents
   of "detgrp" if needed so that it always holds a list of detectors to be
   included (not excluded). */
   smf_cubegrid( igrp,  size, system, usedetpos, autogrid, alignsys,
                 detgrp, spacerefwcs ? NULL : par, poserrfatal, &moving,
                 &oskyfrm, &sparse, &hastsys, status );

/* If we have spatial reference WCS, use the SkyFrame from the spatial
   reference WCS. */
   if( spacerefwcs ) oskyfrm = astGetFrame( spacerefwcs, AST__CURRENT );

/* Get the pixel spreading scheme to use. */
   if( !sparse ) {
      parChoic( "SPREAD", "NEAREST", "NEAREST,LINEAR,SINC,"
                "SINCSINC,SINCCOS,SINCGAUSS,SOMB,SOMBCOS,GAUSS",
                1, pabuf, 10, status );

      smf_get_spread( pabuf, &spread, &nparam, status );
   } else {
      spread = AST__NEAREST;
      nparam = 0;
   }

/* Get an additional parameter vector if required. */
   if( nparam > 0 ) parExacd( "PARAMS", nparam, params, status );

/* See how the bad pixel mask in each output spectrum is to be determined.
   Also choose whether to use the 2D or the 3D weighting system. The 2D
   system assumes that all pixels in a given output spectrum have the
   same weight and variance, and requires much less memory than the 3D
   system. Do not bother asking if we are using a 3D spread function by
   definition. */
   if (spread == AST__NEAREST) {
     parChoic( "BADMASK", "OR", "AND,OR,FIRST", 1, pabuf, 10, status );

     if( !strcmp( pabuf, "AND" ) ) {
       badmask = 2;
       is2d = 0;

     } else if( !strcmp( pabuf, "OR" ) ) {
       badmask = 1;
       is2d = 1;

     } else {
       badmask = 0;
       is2d = 1;

     }
   } else {
     badmask = 2;
     is2d = 0;
   }

/* BADMASK = OR and FIRST can only be used with SPREAD = Nearest. Report an
   error for any other combination. */
   if( badmask != 2 && spread != AST__NEAREST && *status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( "", "Incompatible values supplied for parameters BADMASK "
              "and SPREAD.", status );
      errRep( "", "BADMASK values of 'OR' and 'FIRST' can only be used if "
              "SPREAD is 'Nearest'.", status );
   }

/* If we are producing an output cube with the XY plane being a spatial
   projection... */
   if( !sparse && *status == SAI__OK ) {

/* See if the output spectral range is to be the intersection or union of
   the input spectral ramges. */
      if( badmask != 2 ) {
         specunion = 1;
      } else {
         parGet0l( "SPECUNION", &specunion, status );
      }

/* Validate the input files, create the WCS FrameSet to store in the
   output cube, and get the pixel index bounds of the output cube. */
      smf_cubebounds( igrp, size, oskyfrm, autogrid, usedetpos,
                      spacerefwcs, specrefwcs, par,
                      ( trim ? detgrp : NULL ), moving, specunion, lbnd_out,
                      ubnd_out, &wcsout, &npos, &hasoffexp, &boxes,
                      &polobs, &aref, &bref, status );

/* See if the input data should be weighted according to the reciprocal
   of the input variances. This required ACS_OFFEXPOSURE values in the
   input JCMTSTATE, so warn the user if this cannot be done and continue
   without using weights. */
      parGet0l( "INWEIGHT", &use_wgt, status );
      if( use_wgt && ( !hasoffexp || !hastsys ) ) {
         if( !blank) msgBlank( status );
         if( !hasoffexp ) {
            msgOutif( MSG__NORM, "INW_MSG1A", "   ACS_OFFEXPOSURE not found "
                      "in JCMTSTATE extension.", status );
         } else {
            msgOutif( MSG__NORM, "INW_MSG1B", "   No good TSYS values found "
                      "in ACSIS extension.", status );
         }
         msgOutif( MSG__NORM, "INW_MSG1C", "   Weights cannot be determined "
                   "for the input spectra.", status );
         msgOutif( MSG__NORM, "INW_MSG2", "   Each output spectrum will be "
                   "an unweighted sum of the input spectra.", status );
         msgBlank( status );
         blank = 1;
         use_wgt = 0;
      }

/* See how the output Variances are to be created. */
      parChoic( "GENVAR", "TSYS", "SPREAD,TSYS,NONE", 1, pabuf, 10, status );

      if( !strcmp( pabuf, "SPREAD" ) ) {
         genvar = 1;

      } else if( !strcmp( pabuf, "TSYS" ) ) {
         genvar = 2;

      } else {
         genvar = 0;

      }

/* Now deal with sparse output cubes. */
   } else {

/* Validate the input files, create the WCS FrameSet to store in the
   output cube, and get the pixel index bounds of the output cube. */
      smf_sparsebounds( wf, igrp, size, oskyfrm, usedetpos, detgrp, lbnd_out,
                        ubnd_out, &wcsout, &hasoffexp, &polobs, status );

/* See how the output Variances are to be created (the "Spread" option is
   not available in sparse mode). */
      parChoic( "GENVAR", "TSYS", "TSYS,NONE", 1, pabuf, 10, status );

      if( !strcmp( pabuf, "TSYS" ) ) {
         genvar = 2;

      } else {
         genvar = 0;

      }

   }

/* We need some good TSYS valaues and some ACS_OFFEXPOSURE values to
   create Tsys output variances. Report an error if these are not
   available. */
   if( genvar == 2 && ( !hasoffexp || !hastsys) ) {
      msgOutif( MSG__NORM, "GNV_MSG1", "   Variances cannot be determined "
                "for the input spectra.", status );
      msgOutif( MSG__NORM, "GNV_MSG2", "   The output file will not contain "
                "a Variance array.", status );
      msgBlank( status );
      blank = 1;
      genvar = 0;
   }

/* See if the output is to be split up into a number of separate tiles,
   each one being stored in a separate output NDF. If a null value is
   supplied for TILEDIMS, annul the error and retain the original NULL
   pointer for the array of tile structures (this is used as a flag that
   the entire output grid should be stored in a single output NDF). Note,
   tiling cannot be used with sparse output NDFs. If we are producing JSA
   tiles, do not access the parameters for user-defined tiles. */
   parGet0l( "JSATILES", &jsatiles, status );
   if( !jsatiles && !sparse && *status == SAI__OK ) {
      parGet1i( "TILEDIMS", 2, tiledims, &nval, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
      } else {
         parGet0l( "TRIMTILES", &trimtiles, status );
         parGet0i( "TILEBORDER", &tileborder, status );
         if( nval == 1 ) tiledims[ 1 ] = tiledims[ 0 ];
         tiles = smf_choosetiles( igrp, size, lbnd_out, ubnd_out, boxes,
                                  spread, params, wcsout, tiledims,
                                  trimtiles, tileborder, &ntile, status );
      }
   }

/* If we are not splitting the output up into user-defined tiles, then create
   an array containing a single tile description that encompasses the entire
   full size output grid. */
   if( !tiles ) {
      tiledims[ 0 ] = 0;
      tiles = smf_choosetiles( igrp, size, lbnd_out, ubnd_out, boxes,
                               spread, params, wcsout, tiledims,
                               0, 0, &ntile, status );
   }

/* Output the pixel bounds of the full size output array (not of an
   individual tile). */
   parPut1i( "LBOUND", 3, lbnd_out, status );
   parPut1i( "UBOUND", 3, ubnd_out, status );

/* Get the base->current Mapping from the output WCS FrameSet, and split it
   into two Mappings; one (oskymap) that maps the first 2 GRID axes into
   celestial sky co-ordinates, and one (ospecmap) that maps the third GRID
   axis into a spectral co-ordinate. Also extract the SpecFrame and
   SkyFrame from the current Frame. */
   tmap = astGetMapping( wcsout, AST__BASE, AST__CURRENT );
   tfrm = astGetFrame( wcsout, AST__CURRENT );

   axes[ 0 ] = 1;
   axes[ 1 ] = 2;
   astMapSplit( tmap, 2, axes, outax, &oskymap );
   oskyfrm = astPickAxes( tfrm, 2, outax, NULL );

   axes[ 0 ] = 3;
   astMapSplit( tmap, 1, axes, outax, &ospecmap );
   ospecfrm = astPickAxes( tfrm, 1, outax, NULL );

/* Create a copy of "oskyfrm" representing absolute coords rather than
   offsets. */
   abskyfrm = astCopy( oskyfrm );
   astClear( abskyfrm, "SkyRefIs" );

/* Create a FrameSet describing 2D GRID to spatial sky coords. This will
   be used in the extra 2D images stored in the output SMURF extension. */
   wcsout2d = astFrameSet( astFrame( 2, "Domain=GRID" ), " " );
   astAddFrame( wcsout2d, AST__BASE, oskymap, oskyfrm );

/* Invert the spectral Mapping (for the convenience of smf_rebincube), so that
   it goes from current Frame to output grid axis. */
   astInvert( ospecmap );

/* See if weights are to be saved in the output NDFs. */
   parGet0l( "WEIGHTS", &savewgt, status );

/* Report the WCS bounds and store the WCS bounds parameters */
   smf_store_outputbounds( 1, lbnd_out, ubnd_out, wcsout, oskyfrm,
                           oskymap, status);
   msgBlank( status );
   blank = 1;

/* Get the polarisation analyser angular bin size, and convert from
   degrees to radians. Watch for null values, using zero bin size to flag
   that polarisation analyser angle should be ignored. Also get the
   analyser angle at the centre of the first bin. */
   if( *status == SAI__OK && polobs ) {

      if( !blank ) msgBlank( status );
      msgOutif( MSG__NORM, "POL_MSG1", "   The input files contain "
                "polarisation data.", status );
      msgBlank( status );
      blank = 1;

      parGet0r( "POLBINSIZE", &polbinsize, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
         polbinsize = 0.0;
      } else {
         polbinsize *= AST__DD2R;
      }

      parGet0r( "POLBINZERO", &polbinzero, status );
      polbinzero *= AST__DD2R;

   } else {
      polbinsize = 0.0;
      polbinzero = 0.0;
   }

/* Choose a set of polarisation angle bins, and assign each time slice in
   each input NDF to one of these bins. */
   ptime = smf_choosepolbins( igrp, size, polbinsize, polbinzero, wcsout2d,
                              &npbin, &pangle, status );

/* Write the number of polarisation angle bins being created to an output
   parameter. */
   parPut0i( "NPOLBIN", npbin, status );

/* If known, write the number of tiles being created to an output
   parameter. We do it here if possible so that a valid value is
   available to subsequent commands even if a null value is supplied
   for "OUT". But we cannot do it here if we are creating JSA tiles
   since we only know how many JSA tiles are being created once the
   cube has been created. */
   if( !jsatiles ) parPut0i( "NTILE", ntile, status );

/* Create a new group to hold the names of the output NDFs that have been
   created. This group does not include any NDFs that correspond to tiles
   that contain no input data. */
   igrp4 = grpNew( "", status );

/* Create a group holding the names of the output NDFs. Abort without error
   if a null value is supplied. We first get the base name. If only 1
   tile is being created, we just use the base name as the output NDF name.
   Otherwise, we create a new group holding a name for each tile which is
   the basename with "_1", "_2", etc, appended to it. */
   if( *status == SAI__OK ) {
      ndgCreat ( "OUT", NULL, &ogrp, &outsize, &flag, status );

      if( *status == PAR__NULL ) {
         errAnnul( status );
         goto L998;
      }

/* Expand the group to hold an output NDF name for each tile and/or pol bin. */
      smf_expand_tilegroup( ogrp, ntile, npbin, &outsize, status );
   }

/* Initialise the index of the next output NDF name to use in "ogrp". */
   iout = 1;

/* Initialise the factor needed for calculating the variances from the
   Tsys value, to indicate that no factor has yet been calculated. */
   fcon = -1.0;

/* Loop round, creating each tile of the output array. Each tile is
   initially made a little larger than required so that edge effects
   (caused by the width of the spreading kernel) are avoided. The NDF
   containing the tile is eventually reshaped to exclude the extra
   boundary, resulting in a set of tiles that can be assembled edge-to-edge
   to form the full output array. */
   tile = tiles;
   for( itile = 1; itile <= ntile && *status == SAI__OK; itile++, tile++ ) {

/* Tell the user which tile is being produced. */
      if( ntile > 1 ) {
         if( !blank ) msgBlank( status );
         msgSeti( "I", itile );
         msgSeti( "N", ntile );
         msgSeti( "XLO", (int) tile->lbnd[ 0 ] );
         msgSeti( "XHI", (int) tile->ubnd[ 0 ] );
         msgSeti( "YLO", (int) tile->lbnd[ 1 ] );
         msgSeti( "YHI", (int) tile->ubnd[ 1 ] );
         msgOutif( MSG__NORM, "TILE_MSG1", "   Creating output tile ^I of "
                   "^N (pixel bounds ^XLO:^XHI, ^YLO:^YHI)...", status );
         msgOutif( MSG__NORM, "TILE_MSG3", "   -----------------------------------------------------------", status );
         msgBlank( status );
         blank = 1;
      }

/* If the tile is empty, do not create it. */
      if( tile->size == 0 ) {

/* Issue a warning. */
         msgOutif( MSG__NORM, "TILE_MSG2", "      No input data "
                   "contributes to this output tile. The tile "
                   "will not be created.", status );
         msgBlank( status );
         blank = 1;

/* Skip over the unused output file names. */
         iout += npbin;

/* Proceed to the next tile. */
         continue;
      }

/* Begin an AST context for the current tile. */
      astBegin;

/* Begin an NDF context for the current tile. */
      ndfBegin();

/* Create FrameSets that are appropriate for this tile. This involves
   remapping the base (GRID) Frame of the full size output WCS so that
   GRID position (1,1) corresponds to the centre of the first pixel int he
   tile. */
      wcstile = astCopy( wcsout );
      if( tile->map3d ) astRemapFrame( wcstile, AST__BASE, tile->map3d );

      wcstile2d = astCopy( wcsout2d );
      if( tile->map2d ) astRemapFrame( wcstile2d, AST__BASE, tile->map2d );

/* Get the Mapping from 2D GRID to SKY coords (the tiles equivalent of
   "oskymap"). If the target is moving, but an absolute skyframe is used
   by the output, then we need to change the skyframe to use offsets
   before getting the mapping (as required by smf_rebin_totmap). We set
   the correct target position first. */
      if( moving && astChrMatch( astGetC( wcstile2d, "SkyRefIs" ),
                                 "Ignored" ) ){
         AstFrameSet *temp = astCopy( wcstile2d );
         astSetD( temp, "SkyRef(1)", aref );
         astSetD( temp, "SkyRef(2)", bref );
         astSetC( temp, "SkyRefIs", "Origin" );
         tskymap = astGetMapping( temp, AST__BASE, AST__CURRENT );
         temp = astAnnul( temp );

/* If the target is not moving, but an offset skyframe is used by the
   output, then we need to change the skyframe to use absolute coords
   before getting the mapping (as required by smf_rebin_totmap). */
      } else if( !moving && astChrMatch( astGetC( wcstile2d, "SkyRefIs" ),
                                         "Origin" ) ){
         astClear( wcstile2d, "SkyRefIs" );
         tskymap = astGetMapping( wcstile2d, AST__BASE, AST__CURRENT );
         astSetC( wcstile2d, "SkyRefIs", "Origin" );

/* Otherwise just use the mapping as it is. */
      } else {
         tskymap = astGetMapping( wcstile2d, AST__BASE, AST__CURRENT );
      }

/* Invert the output sky mapping so that it goes from sky to pixel
   coords. */
      astInvert( tskymap );

/* Store the initial number of pixels per spatial plane in the output tile. */
      nxy = ( tile->eubnd[ 0 ] - tile->elbnd[ 0 ] + 1 )*
            ( tile->eubnd[ 1 ] - tile->elbnd[ 1 ] + 1 );

/* Loop to create a separate output cube for each polarisation bin. */
      for( ipbin = 0; ipbin < npbin && *status == SAI__OK; ipbin++ ) {
         if( npbin > 1 ) {
            if( !blank ) msgBlank( status );
            msgSeti( "I", ipbin + 1 );
            msgSeti( "N", npbin );
            msgSetr( "A", (float) ( pangle[ ipbin ]*AST__DR2D ) );
            msgOutif( MSG__NORM, "PBIN_MSG1", "      Polarisation bin ^I of "
                      "^N (^A degrees)...", status );
            msgBlank( status );
            blank = 1;
         }

/* Add the name of this output NDF to the group holding the names of the
   output NDFs that have actually been created. */
         pname = basename;
         grpGet( ogrp, iout, 1, &pname, GRP__SZNAM, status );
         grpPut1( igrp4, basename, 0, status );

/* Create the output NDF to hold the tile data relating to the current
   polarisation angle bin. */
         smfflags = 0;
         if( genvar && !is2d ) smfflags |= SMF__MAP_VAR;
         smf_open_newfile( wf, ogrp, iout++, SMF__FLOAT, 3, tile->elbnd,
                           tile->eubnd, smfflags, &odata, status );

/* Abort if an error has occurred. */
         if( *status != SAI__OK ) goto L999;

/* Save some useful pointers. */
         file = odata->file;
         ondf = file->ndfid;

/* Save the output file name. */
         strcpy( oname, file->name );

/* Create a history component in the output NDF by copying the History
   component from the first input file. */
         smf_history_copy( igrp, 1, ondf, status );

/* Copy the Label and Unit strings from the first input NDF, and check
   that all input NDFs have the same Label and Unit strings. */
         smf_labelunit( igrp, size, odata, status );

/* Get a pointer to the mapped output data array. */
         data_array = (odata->pntr)[ 0 ];

/* If a 3D weights array is being used, the variance will be evaluated for
   each individual pixel in the output cube. In this case we will have
   mapped the Variance component in the output cube, so store a pointer to
   it. */
         if( !is2d ) {
            var_array = (odata->pntr)[ 1 ];

/* Otherwise, the variance is assumed to be the same in every spatial
   slice, so we only need memory to hold one spatial slice (this slice is
   later copied to all slices in the output cube Variance component).
   Also allocate some work arrays of the same size. */
         } else if( genvar ) {
            var_array = (float *) astMalloc( nxy*sizeof( float ) );
         }

/* If we are producing a regularly gridded output NDF, we need to
   allocate a work array. */
         if( !sparse ) {

/* Assume for the moment that the weights array is 2-dimensional (i.e. a
   single spatial plane of the output). Store its bounds and calculate its
   total size in pixels. */
            nwgtdim = 2;
            lbnd_wgt[ 0 ] = tile->elbnd[ 0 ];
            lbnd_wgt[ 1 ] = tile->elbnd[ 1 ];
            ubnd_wgt[ 0 ] = tile->eubnd[ 0 ];
            ubnd_wgt[ 1 ] = tile->eubnd[ 1 ];
            wgtsize = ubnd_wgt[ 0 ] - lbnd_wgt[ 0 ] + 1;
            wgtsize *= ubnd_wgt[ 1 ] - lbnd_wgt[ 1 ] + 1;

/* If the weights array is in fact 3D, increase its total size and
   increment the number of axes in the weights array. */
            if( !is2d ) {
               nwgtdim = 3;
               lbnd_wgt[ 2 ] = tile->elbnd[ 2 ];
               ubnd_wgt[ 2 ] = tile->eubnd[ 2 ];
               wgtsize *= ubnd_wgt[ 2 ] - lbnd_wgt[ 2 ] + 1;
            }

/* If output variances are being created from the spread of input values,
   the weights array needs to be twice the size determined above.
   Implement this as an extra trailing axis with bounds [1:2]. */
            if( genvar == 1 ) {
               lbnd_wgt[ nwgtdim ] = 1;
               ubnd_wgt[ nwgtdim ] = 2;
               nwgtdim++;
               wgtsize *= 2;
            }

/* Report an error if the weights array is too big for the current
   version of HDS. */
            if( wgtsize >= VAL__MAXI && *status == SAI__OK ) {
               *status = SMF__TOOBIG;
               errRepf( "", "Output cube is too big to create. The weights "
                        "array would contain %lld elements.", status,
                        (long long int) wgtsize );
            }

/* Create the NDF extension, or allocate the work space, as required. */
            if( savewgt ) {
               weightsloc = smf_get_xloc ( odata, "ACSISRED", "WT_ARR", "WRITE",
                                           0, 0, status );
               smf_open_ndfname ( weightsloc, "WRITE", "WEIGHTS", "NEW",
                                  "_DOUBLE", nwgtdim, (int *) lbnd_wgt,
                                  (int *) ubnd_wgt, NULL, NULL, NULL,  &wdata, status );
               if( wdata ) wgt_array = (wdata->pntr)[ 0 ];

            } else {
               wgt_array = astMalloc( sizeof( double )*(size_t)wgtsize );
            }
         }

/* Create a SMURF extension in the output NDF and create two or three 2D NDFs
   in the extension; one for the total exposure time ("on+off"), one for the
   "on" time, and one for the Tsys values. Each of these 2D NDFs inherits
   the spatial bounds of the main output NDF. Note, the Tsys array also
   needs variances to be calculated. Include spatial WCS in each NDF. */
         smurf_xloc = smf_get_smurfloc ( odata, "WRITE", status );

         smf_open_ndfname ( smurf_xloc, "WRITE", "EXP_TIME", "NEW",
                            "_REAL", 2, (int *) tile->elbnd,
                            (int *) tile->eubnd, "Total exposure time",
                            "s", wcstile2d, &expdata, status );
         if( expdata ) {
            exp_array = (expdata->pntr)[ 0 ];
         }

         smf_open_ndfname ( smurf_xloc, "WRITE", "EFF_TIME", "NEW",
                            "_REAL", 2, (int *) tile->elbnd,
                            (int *) tile->eubnd, "Effective integration time",
                            "s", wcstile2d, &effdata, status );
         if( effdata ) {
            eff_array = (effdata->pntr)[ 0 ];
         }

         if( genvar && spread == AST__NEAREST ) {
            smf_open_ndfname ( smurf_xloc, "WRITE", "TSYS", "NEW",
                               "_REAL", 2, (int *) tile->elbnd,
                               (int *) tile->eubnd,
                               "Effective system temperature", "K", wcstile2d,
                               &tsysdata, status );
            if( tsysdata ) {
               tsys_array = (tsysdata->pntr)[ 0 ];
            }
         }

/* Find the last input file that contributes to the current output tile
   and polarisation bin. */
         ilast = 0;
         for( ifile = tile->size; ifile >= 1 && *status == SAI__OK; ifile-- ) {
            jin = ( tile->jndf ) ? tile->jndf[ ifile - 1 ] : ifile - 1;
            pt = ptime ?  ptime[ jin ][ ipbin ] : NULL;
            if( !pt || pt[ 0 ] < VAL__MAXI ) {
               ilast = ifile;
               break;
            }
         }

/* Loop round all the input files that overlap this tile, pasting each one
   into the output NDF. */
         naccept = 0;
         nreject = 0;
         nused = 0;
         ispec = 0;
         first = 1;
         for( ifile = 1; ifile <= tile->size && *status == SAI__OK; ifile++ ) {

/* Get the zero-based index of the current input file (ifile) within the
   group of input NDFs (igrp). */
            jin = ( tile->jndf ) ? tile->jndf[ ifile - 1 ] : ifile - 1;

/* Does this input NDF have any time slices that fall within the current
   polarisation bin? Look at the first used time slice index for this
   input NDF and polarisation angle bin. Only proceed if it is legal.
   Otherwise, pass on to the next input NDF. */
            pt = ptime ?  ptime[ jin ][ ipbin ] : NULL;
            if( !pt || pt[ 0 ] < VAL__MAXI ) {

/* Obtain information about the current input NDF. */
               smf_open_file( wf, tile->grp, ifile, "READ", 0, &data, status );

/* Issue a suitable message and abort if anything went wrong. */
               if( *status != SAI__OK ) {
                  errRep( FUNC_NAME, "Could not open input data file.", status );
                  break;

               } else {
                  if( data->file == NULL ) {
                     *status = SAI__ERROR;
                     errRep( FUNC_NAME, "No smfFile associated with smfData.",
                             status );
                     break;

                  } else if( data->hdr == NULL ) {
                     *status = SAI__ERROR;
                     errRep( FUNC_NAME, "No smfHead associated with smfData.",
                             status );
                     break;

                  }
               }

/* Report the name of the input file. */
               smf_smfFile_msg( data->file, "FILE", 1, "<unknown>" );
               msgSeti( "THISFILE", ifile );
               msgSeti( "NUMFILES", tile->size );
               msgOutif( MSG__VERB, " ", "Processing ^FILE (^THISFILE/^NUMFILES)",
                         status );

/* Update the provenance for the output NDF to include the input NDF as
   an ancestor. */
               smf_updateprov( ondf, data, NDF__NOID, "SMURF:MAKECUBE",
                               NULL, status );

/* Check that the input data type is single precision. */
               if( data->dtype != SMF__FLOAT ) {
                  if( *status == SAI__OK ) {
                     smf_smfFile_msg( data->file, "FILE", 1, "<unknown>" );
                     msgSetc( "DTYPE", smf_dtype_string( data, status ) );
                     *status = SAI__ERROR;
                     errRep( FUNC_NAME, "^FILE has ^DTYPE data type, should "
                             "be REAL.",  status );
                  }
                  break;
               }

/* If the detector positions are to calculated on the basis of FPLANEX/Y
   rather than detpos, then free the detpos array in the smfHead
   structure. This will cause smf_tslice_ast to use the fplanex/y values. */
               if( !usedetpos && data->hdr->detpos ) {
                  data->hdr->detpos = astFree( (double *) data->hdr->detpos );
               }

/* Handle output FITS header creation/manipulation */
               smf_fits_outhdr( data->hdr->fitshdr, &fchan, status );

/* Rebin the data into the output grid. */
               if( !sparse ) {
                  smf_rebincube( wf, data, first, (ifile == ilast ), pt, badmask,
                                 is2d, abskyfrm, tskymap, ospecfrm, ospecmap,
                                 detgrp, moving, use_wgt, tile->elbnd,
                                 tile->eubnd, spread, params, genvar, data_array,
                                 var_array, wgt_array, exp_array, eff_array, &fcon,
                                 &nused, &nreject, &naccept, status );

               } else {
                  smf_rebinsparse( data, first, pt, ospecfrm, ospecmap,
                                   abskyfrm, detgrp, tile->elbnd, tile->eubnd,
                                   genvar, data_array, var_array, &ispec,
                                   exp_array, eff_array, &fcon, status );
               }

               blank = 0;
               first = 0;

/* Close the input data file. */
               if( data != NULL ) {
                  smf_close_file( wf, &data, status );
                  data = NULL;
               }
            }
         }

/* Tell the user how many input spectra were rejected. */
         if( nreject > 0 ) {
            if( !blank ) msgBlank( status );
            msgSeti( "N", nreject );
            msgSeti( "T", naccept + nreject );
            msgOutif( MSG__NORM, " ", "WARNING: ^N out of the ^T input spectra "
                      "were ignored because they included unexpected bad pixel "
                      "values.", status );
            msgBlank( status );
            blank = 1;
         }

/* Arrive here if an error occurs. */
   L999:;

/* Close the input data file that remains open due to an early exit from
   the above loop. */
         if( data != NULL ) {
            smf_close_file( wf, &data, status );
            data = NULL;
         }

/* Store the WCS FrameSet in the output NDF (if any). If the target is
   moving, set the AlignOffset attribute non-zero in the current Frame.
   Use the Frame pointer rather than the FrameSet pointer to avoid
   re-mapping the Frame within the FrameSet. */
         if( wcstile ) {
            if( moving ) {
               tfrm = astGetFrame( wcstile, AST__CURRENT );
               astSetI( tfrm, "AlignOffset", 1 );
               tfrm = astAnnul( tfrm );
            }
            ndfPtwcs( wcstile, ondf, status );
         }

/* If we are creating an output Variance component... */
         if( genvar && *status == SAI__OK) {
            msgOutif( MSG__VERB, " ", "Creating output variances",
                      status );

/* Count the number of pixel which have a good data value but a bad
   variance value, and count the number which have a good data value.
   If the weights array is 2-dimensional, cycle through the 2D variance
   array as we move through the entire 3D output data array. */
            ngood = 0;
            nbad = 0;
            ipd = (float *) data_array;
            ipv = (float *) var_array;
            nel = nxy*( tile->eubnd[ 2 ] - tile->elbnd[ 2 ] + 1 );

            if( !is2d ) {

               for( el = 0; el < nel; el++, ipd++,ipv++ ) {
                  if( *ipd != VAL__BADR ) {
                     ngood++;
                     if( *ipv == VAL__BADR ) nbad++;
                  }
               }

            } else {

               for( el = 0; el < nel; el++, ipd++,ipv++ ) {
                  if( el % nxy == 0 ) ipv = (float *) var_array;
                  if( *ipd != VAL__BADR ) {
                     ngood++;
                     if( *ipv == VAL__BADR ) nbad++;
                  }
               }
            }

/* If more than 50% of the good data values have bad variance values,
   we will erase the variance component. */
            if( nbad > 0.5*ngood ) {
               if( !blank ) msgBlank( status );
               msgOutif( MSG__NORM, " ", "WARNING: More than 50% of the good "
                         "output data values have bad variances. The output "
                         "NDF will not contain a Variance array.", status );
               msgBlank( status );
               blank = 1;

               if( !is2d ) {
                  ndfUnmap( ondf, "Variance", status );
                  ndfReset( ondf, "Variance", status );
                  (odata->pntr)[ 1 ] = NULL;
                  var_array = NULL;

               } else {
                  var_array = (float *) astFree( var_array );
               }

               genvar = 0;

/* Otherwise, if the output variances are the same for every spatial slice, the
   "var_array" used above will be a 2D array holding a single slice of the 3D
   Variance array. In this case we now copy this slice to the output cube. */
            } else if( is2d ) {
               ndfMap( ondf, "Variance", "_REAL", "WRITE", (void *) &var_out, &nel,
                       status );
               if( var_out && *status == SAI__OK ) {
                  ipd = (float *) data_array;
                  ipv = (float *) var_out;
                  el0 = 0;
                  for( el = 0; el < nel; el++, el0++, ipd++, ipv++ ) {
                     if( el0 == nxy ) el0 = 0;
                     if( *ipd != VAL__BADR ) {
                        *ipv = var_array[ el0 ];
                     } else {
                        *ipv = VAL__BADR;
                     }
                  }
               }

/* If all the input files had the same backend degradation factor and
   channel width, calculate a 2D array of Tsys values for the output
   cube. */
               if( tsys_array ) {
                  if( fcon != VAL__BADD ) {
                     for( el0 = 0; el0 < nxy; el0++ ) {
                        teff = eff_array[ el0 ];
                        var = var_array[ el0 ];
                        if( teff != VAL__BADR && teff > 0.0 &&
                            var != VAL__BADR && var > 0.0 ) {
                           tsys_array[ el0 ] = sqrt( 0.25*var*teff/fcon );
                        } else {
                           tsys_array[ el0 ] = VAL__BADR;
                        }
                     }

                  } else {
                     for( el0 = 0; el0 < nxy; el0++ ) {
                        tsys_array[ el0 ] = VAL__BADR;
                     }
                  }
               }

/* Free the memory used to store the 2D variance information. */
               var_array = astFree( var_array );

/* For 3D variances, the output Tsys values are based on the mean
   variance in every output spectrum. */
            } else if( fcon != VAL__BADD && tsys_array ) {

               work2_array = astMalloc( nxy*sizeof( float ) );
               if( work2_array ) {
                  ipw = work2_array;
                  ipt = tsys_array;
                  for( el = 0; el < nxy; el++ ) {
                     *(ipw++) = 0.0;
                     *(ipt++) = 0.0;
                  }

                  ipv = var_array;
                  ipt = tsys_array;
                  ipw = work2_array;

                  for( el = 0; el < nel; el++, ipv++, ipt++, ipw++ ) {
                     if( el % nxy == 0 ) {
                        ipt = tsys_array;
                        ipw = work2_array;
                     }

                     if( *ipv != VAL__BADR ) {
                        *(ipw) += 1.0;
                        *(ipt) += *ipv;
                     }
                  }

                  for( el0 = 0; el0 < nxy; el0++ ) {
                     teff = eff_array[ el0 ];
                     var = tsys_array[ el0 ];
                     if( teff != VAL__BADR && teff > 0.0 &&
                         work2_array[ el0 ] > 0.0 && var > 0.0 ) {
                        var /= work2_array[ el0 ];
                        tsys_array[ el0 ] = sqrt( 0.25*var*teff/fcon );
                     } else {
                        tsys_array[ el0 ] = VAL__BADR;
                     }
                  }
               }

/* For 3D weights and no Variance->Tsys conversion factor, fill the Tsys
   array with bad values. */
            } else {

               if( !blank ) msgBlank( status );
               msgOutif( MSG__NORM, " ", "WARNING: Cannot create output Tsys "
                         "values.", status );
               msgBlank( status );
               blank = 1;

               if( tsys_array ) {
                  for( el0 = 0; el0 < nxy; el0++ ) {
                     tsys_array[ el0 ] = VAL__BADR;
                  }
               }

            }
         }

/* Put a separator in the output fits header to make it clear which headers
   have been added by the data processing.
   Wind to end of the fitschan first. */
         astSetI( fchan, "CARD", astGetI( fchan, "NCard" ) + 1 );
         astSetFitsCM( fchan, " ", 0 );
         astSetFitsCM( fchan, "---- Data Processing ----", 0 );

/* If we created an output Variance component, store the median system
   temperature as keyword TSYS in the FitsChan. */
         if( tsys_array ) {
            msgOutif( MSG__VERB, " ", "Calculating median output Tsys value",
                      status );
            if( genvar ) {
               hist = smf_find_median( tsys_array, NULL, nxy, hist, &median,
                                       status );
               if( median != VAL__BADR ) {
                  atlPtftr( fchan, "MEDTSYS", median,
                            "[K] Median MAKECUBE system temperature", status );
               }

            } else {
               if( !blank ) msgBlank( status );
               msgOutif( MSG__NORM, " ", "WARNING: Cannot create output Tsys "
                         "values since no output variances have been created.",
                         status );
               msgBlank( status );
               blank = 1;
            }
         }

/* Store the median exposure time as keyword EXP_TIME in the FitsChan.
   Since kpg1Medur partially sorts the array, we need to take a copy of it
   first. */
         msgOutif( MSG__VERB, " ", "Calculating median output exposure time",
                   status );
         hist = smf_find_median( exp_array, NULL, nxy, hist, &median, status );
         if( median != VAL__BADR ) {
            atlPtftr( fchan, "EXP_TIME", median,
                      "[s] Median MAKECUBE exposure time", status );
         }

/* Store the median effective integration time as keyword EFF_TIME in the
   FitsChan. Since kpg1Medur partially sorts the array, we need to take a
   copy of it first. */
         msgOutif( MSG__VERB, " ", "Calculating median output effective exposure time",
                   status );

         hist = smf_find_median( eff_array, NULL, nxy, hist, &median, status );
         if( median != VAL__BADR ) {
            atlPtftr( fchan, "EFF_TIME", median,
                      "[s] Median MAKECUBE effective integration time", status );
         }

/* Free the second work array and histogram array. */
         work2_array = astFree( work2_array );
         hist = astFree( hist );

/* Store the keywords holding the number of user-defined tiles generated and
   the index of the current tile. */
         if( !jsatiles ) {
            atlPtfti( fchan, "NUMTILES", ntile,
                      "No. of tiles covering the field", status );
            atlPtfti( fchan, "TILENUM", itile,
                      "Index of this tile (1->NUMTILES)", status );
         }

/* If the FitsChan is not empty, store it in the FITS extension of the
   output NDF (any existing FITS extension is deleted). */
         if( fchan ){
            if( astGetI( fchan, "NCard" ) > 0 ) kpgPtfts( ondf, fchan, status );
            fchan = astAnnul( fchan );
         }

/* If required ensure that the output NDF has a POLPACK extension holding
   the polarisation angle for the data in the output cube. */
         if( npbin > 1 ) smf_polext( ondf, 1, pangle[ ipbin ], NULL, 0, status );

/* The following calls to smf_reshapendf close the open NDFs. So if we
   will need access to the NDF afterwards, clone the NDF identifier now. */
         if( jsatiles || trim ) ndfClone( ondf, &tndf, status );

/* For each open output NDF (the main tile NDF, and any extension NDFs),
   first clone the NDF identifier, then close the file (which will unmap
   the NDF arrays), and then reshape the NDF to exclude the boundary
   that was added to the tile to avoid edge effects. */
         msgOutif( MSG__VERB, " ", "Reshaping output NDFs", status );
         smf_reshapendf( &expdata, tile, status );
         smf_reshapendf( &effdata, tile, status );
         smf_reshapendf( &tsysdata, tile, status );
         smf_reshapendf( &wdata, tile, status );
         smf_reshapendf( &odata, tile, status );

/* If required trim any remaining bad borders, and annul the cloned output
   NDF identifier. Catch NDFs that have no good data values. Annul the
   error, flag that the output NDF should be deleted and issue a warning. */
         delete = 0;
         if( trim ) {
            if( trim && *status == SAI__OK ) {
               kpg1Badbx( tndf, 2, &junk, &junk, status );
               if( *status == SAI__ERROR ) {
                  errAnnul( status );
                  delete = 1;
                  msgOutif( MSG__NORM, " ", "      No usable input data "
                            "falls within this output tile.", status );
                  msgOutif( MSG__NORM, " ", "      The tile will not be "
                            "created.", status );
                  msgBlank( status );
                  blank = 1;
               }
            }
         }

/* If required, split the output cube up into JSA tiles. */
         if( jsatiles ) {
            parGet0l( "TRIMTILES", &trimtiles, status );
            grpSetsz( igrp4, 0, status );
            smf_jsadicer( tndf, oname, trimtiles, SMF__INST_NONE,
                          SMF__JSA_HPX, &njsatile, igrp4, status );
            delete = -1;
         }

/* Delete (and if required remove the name from the group of output NDF
   names) or annul the NDF as required. */
         if( tndf != NDF__NOID ) {
            if( delete ) {
               ndfDelet( &tndf, status );
               if( delete > 0 ) {
                  Grp *tgrp = grpRemov( igrp4, basename, status );
                  grpDelet( &igrp4, status);
                  igrp4 = tgrp;
               }
            } else {
               ndfAnnul( &tndf, status );
            }
         }

/* Free other resources related to the current tile. */
         if( wgt_array && !savewgt ) wgt_array = astFree( wgt_array );

/* Next polarisation angle bin. */
      }

/* End the tile's NDF context. */
      ndfEnd( status );

/* End the tile's AST context. */
      astEnd;
   }

/* Write the number of tiles being created to an output parameter,
   unless it was written earlier. */
   if( jsatiles ) parPut0i( "NTILE", njsatile, status );

/* Report an error if no output NDFs were created. */
   if( grpGrpsz( igrp4, status ) == 0 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "", "No output NDFs could be created.", status );
   }

/* Write out the list of output NDF names, annulling the error if a null
   parameter value is supplied. */
   if( *status == SAI__OK && igrp4 ) {
      grpList( "OUTFILES", 0, 0, NULL, igrp4, status );
      if( *status == PAR__NULL ) errAnnul( status );
   }

/* Arrive here if no output NDF is being created. */
L998:;

/* Free remaining resources. */
   if( detgrp != NULL) grpDelet( &detgrp, status);
   if( igrp4 != NULL) grpDelet( &igrp4, status);
   if( igrp != NULL) grpDelet( &igrp, status);
   if( ogrp != NULL) grpDelet( &ogrp, status);
   boxes = astFree( boxes );
   if( tiles ) tiles = smf_freetiles( tiles, ntile, status );
   ptime = smf_freepolbins( size, npbin, &pangle, ptime, status );

/* End the NDF context. */
   ndfEnd( status );

/* Issue a status indication.*/
   if( *status == SAI__OK ) {
      msgOutif(MSG__VERB," ",TASK_NAME " succeeded, cube written.", status);
   } else {
      msgOutif(MSG__VERB," ",TASK_NAME " failed.", status);
   }
}
