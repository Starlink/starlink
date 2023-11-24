/*
*+
*  Name:
*     MAKEMAP

*  Purpose:
*     Make a map from SCUBA-2 data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_makemap( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This command is used to create a map from SCUBA-2 data. Two techniques
*     are provided and can be selected using the METHOD parameter.
*
*     The "REBIN" method can be used to make a map by coadding all the samples
*     in the correct location using a number of different convolution
*     techniques. This is useful when the time series has been processed
*     independently of the map-maker and the map should be made "as-is".
*     Raw data will be flatfielded but this method will not apply any
*     extinction correction, sky removal or filtering. It is assumed that
*     this has been handled by other tasks prior to making the map.
*
*     The default "ITERATE" method takes a more holistic approach to map
*     making using an iterative technique to fit for a number of
*     models for noise and instrumental behaviour, one of which is the
*     underlying astronomical image. Details of the map making process
*     can be controlled using the CONFIG parameter.

*  ADAM Parameters:
*     ABORTEDAT = _INTEGER (Write)
*          Set to a non-zero value on exit if the iterative process was
*          aborted because of the ABORTSOON parameter being set TRUE.
*          The specific non-zero value returned is the number of iterations
*          that had been completed when the iterative process was aborted.
*          Always set to zero if ABORTSOON is FALSE.
*     ABORTSOON = _LOGICAL (Read)
*          If TRUE, then the iterative process will exit as soon as it
*          becomes likely that the convergence criterion (specified by
*          configuration parameter MAPTOL) will not be reached within
*          the number of iterations allowed by configuration parameter
*          NUMITER.  [FALSE]
*     ALIGNSYS = _LOGICAL (Read)
*          If TRUE, then the spatial positions of the input data are
*          aligned in the co-ordinate system specified by parameter
*          SYSTEM. Otherwise, they are aligned in the ICRS co-ordinate
*          system. For instance, if the output co-ordinate system is
*          AZEL, then setting ALIGNSYS to TRUE will result in the AZEL
*          values of the input data positions being compared directly,
*          disregarding the fact that a given AZEL will correspond to
*          different positions on the sky at different times. [FALSE]
*     BBM = NDF (Read)
*          Group of files to be used as bad bolometer masks. Each data file
*          specified with the IN parameter will be masked. The
*          corresponding previous mask for a subarray will be used. If
*          there is no previous mask the closest following will be
*          used. It is not an error for no mask to match. A NULL
*          parameter indicates no mask files to be supplied. [!]
*     CHUNKCHANGE( ) = _DOUBLE (Write)
*          An output array holding the final normalised map change value
*          for each chunk.
*     CONFIG = GROUP (Read)
*          Specifies values for the configuration parameters used by the
*          iterative map maker (METHOD=ITERATE). If the string "def"
*          (case-insensitive) or a null (!) value is supplied, a set of
*          default configuration parameter values will be used. A full
*          list of the available configuration parameters is available
*          in the appendix of SUN/258. A smaller list of the more
*          commonly used configuration parameters is available in SC/21.
*
*          The supplied value should be either a comma-separated list of
*          strings or the name of a text file preceded by an up-arrow
*          character "^", containing one or more comma-separated lists of
*          strings. Each string is either a "keyword=value" setting, or
*          the name of a text file preceded by an up-arrow character
*          "^". Such text files should contain further comma-separated
*          lists which will be read and interpreted in the same manner
*          (any blank lines or lines beginning with "#" are
*          ignored). Within a text file, newlines can be used as
*          delimiters, as well as commas. Settings are applied in the
*          order in which they occur within the list, with later
*          settings over-riding any earlier settings given for the same
*          keyword.
*
*          Each individual setting should be of the form:
*
*             <keyword>=<value>
*
*          The parameters available are listed in the "Configuration
*          Parameters" appendix of SUN/258. Default values will be used for
*          any unspecified parameters. Assigning the value "<def>" (case
*          insensitive) to a keyword has the effect of reseting it to its
*          default value. Unrecognised options will result in an error
*          condition. This is done to help find spelling mistakes.
*          [current value]
*     CROTA = _REAL (Read)
*          The angle, in degrees, from the second pixel axis in the output cube
*          to north (in the coordinate system specified by the SYSTEM
*          parameter), measured north through east.  Only accessed if a
*          null value is supplied for parameter REF.
*     FBL( ) = _DOUBLE (Write)
*          Sky coordinates (radians) of the bottom left corner of the
*          output map (the corner with the smallest PIXEL dimension for
*          axis 1 and the smallest PIXEL dimension for axis 2). No check
*          is made that the pixel corresponds to valid data. Note that the
*          position is reported for the centre of the pixel.
*     FBR( ) = _DOUBLE (Write)
*          Sky coordinates (radians) of the bottom right corner of the
*          output map (the corner with the largest PIXEL dimension for
*          axis 1 and the smallest PIXEL dimension for axis 2). No check
*          is made that the pixel corresponds to valid data. Note that the
*          position is reported for the centre of the pixel.
*     FLBND( ) = _DOUBLE (Write)
*          The lower bounds of the bounding box enclosing the output map
*          in the selected output WCS Frame. The values are calculated
*          even if no output cube is created. Celestial axis values will
*          be in units of radians.  The parameter is named to be
*          consistent with KAPPA:NDFTRACE output.
*     FLATMETH = _CHAR (Read)
*          Method to use to calculate the flatfield solution. Options
*          are POLYNOMIAL and TABLE. Polynomial fits a polynomial to
*          the measured signal. Table uses an interpolation scheme
*          between the measurements to determine the power. [POLYNOMIAL]
*     FLATORDER = _INTEGER (Read)
*          The order of polynomial to use when choosing POLYNOMIAL method.
*          [1]
*     FLATSNR = _DOUBLE (Read)
*          Signal-to-noise ratio threshold to use when filtering the
*          responsivity data to determine valid bolometers for the
*          flatfield. [3.0]
*     FLATUSENEXT = _LOGICAL (Read)
*          If true the previous and following flatfield will be used to
*          determine the overall flatfield to apply to a sequence. If false
*          only the previous flatfield will be used. A null default will
*          use both flatfields for data when we did not heater track
*          at the end, and will use a single flatfield when we did heater
*          track. The parameter value is not sticky and will revert to
*          the default unless explicitly over-ridden. [!]
*     FTL( ) = _DOUBLE (Write)
*          Sky coordinates (radians) of the top left corner of the
*          output map (the corner with the smallest PIXEL dimension for
*          axis 1 and the largest PIXEL dimension for axis 2). No check
*          is made that the pixel corresponds to valid data. Note that the
*          position is reported for the centre of the pixel.
*     FTR( ) = _DOUBLE (Write)
*          Sky coordinates (radians) of the top right corner of the
*          output map (the corner with the largest PIXEL dimension for
*          axis 1 and the largest PIXEL dimension for axis 2). No check
*          is made that the pixel corresponds to valid data. Note that the
*          position is reported for the centre of the pixel.
*     FUBND( ) = _DOUBLE (Write)
*          The upper bounds of the bounding box enclosing the output map
*          in the selected output WCS Frame. The values are calculated
*          even if no output cube is created. Celestial axis values will
*          be in units of radians.  The parameter is named to be
*          consistent with KAPPA:NDFTRACE output.
*     FTSPORT = _CHAR (Read)
*          The FTS-2 port to use in calculating the mapping to sky
*          coordinates, or null if FTS-2 was not in the beam.
*          If set, this parameter should be "tracking" or "image". [!]
*     IN = NDF (Read)
*          Input file(s).
*     IPREF = NDF (Read)
*          An existing NDF that is to be used to define the correction
*          to be made for instrumental polarisation (IP). It is only
*          accessed if the input data contains POL2 Q or U time-series
*          values, as created by SMURF:CALCQU. No IP correction is made
*          if a null (!) value is supplied. If a non-null value is supplied,
*          it should be an NDF that holds the total intensity (in pW)
*          within the area of sky covered by the output map. The supplied
*          NDF need not be pre-aligned with the output map - the WCS
*          information in the NDF will be used to aligned them. For each Q
*          or U value in the input time-streams, the corresponding total
*          intensity (I) value is found by sampling the supplied IPREF map
*          at the sky position of the Q/U value. This I value is multipled
*          by a factor that depends on elevation and focal plane position,
*          to get the IP correction. These Q and U corrections are
*          rotated so that they use the same reference direction as the input
*          Q/U data, corrected for extinction, and are then subtracted from
*          the input Q or U value before going on to make a map from the
*          corrected values. [!]
*     ITERMAPS = LITERAL (Read)
*          Specifies the name of a file in which to place a copy of the
*          current map at the end of each iteration. If a null (!) value is
*          supplied, they are placed in the MORE.SMURF.ITERMAPS component of
*          the main output NDF (see parameter OUT).  See configuration
*          parameter "Itermap". [!]
*     JSATILES = _LOGICAL (Read)
*          If TRUE, the output map is created on the JSA all-sky pixel
*          grid, and is split up into individual JSA tiles. Thus multiple
*          output NDFs may be created, one for each JSA tile that touches
*          the map. Each of these output NDFs will have the tile index number
*          appended to the end of the path specified by parameter "OUT". If
*          "JSATILES" is TRUE, the "REF" parameter is ignored. [FALSE]
*     JSATILELIST() = _INTEGER (Write)
*          If parameter "JSATILES" is set TRUE, the zero-based indicies of
*          the created JSA tiles will be written to this output parameter.
*          The number of such indices is given the "NTILE" parameter
*     LBND( 2 ) = _INTEGER (Read)
*          An array of values giving the lower pixel index bound on each
*          spatial axis of the output NDF. The suggested default values
*          encompass all the input spatial information. The supplied
*          values may be modified if TRIM is set TRUE. []
*     LBOUND( 2 ) = _INTEGER (Write)
*          The lower pixel bounds of the output NDF. Note, values will be
*          written to this output parameter even if a null value is supplied
*          for parameter OUT.
*     MASK2 = NDF (Read)
*          An existing NDF that can be used to specify a second external mask
*          for use with either the AST, FLT or COM model. See configuration
*          parameters AST.ZERO_MASK, FLT.ZERO_MASK and COM.ZERO_MASK. Note,
*          it is assumed that this image is aligned in pixel coordinate with
*          the output map. [!]
*     MASK3 = NDF (Read)
*          An existing NDF that can be used to specify a third external mask
*          for use with either the AST, FLT or COM model. See configuration
*          parameters AST.ZERO_MASK, FLT.ZERO_MASK and COM.ZERO_MASK. Note,
*          it is assumed that this image is aligned in pixel coordinate with
*          the output map. [!]
*     MAXMEM = _INTEGER (Read)
*          Maximum memory available for map-making in MiB (mebibytes).
*          For machines with more than 20 GB or memory, the default is to
*          leave 4 GB free for other processes. For machines with less
*          than than 20 GB or memory, the default is to leave 20% of the
*          total memory free for other processes. []
*     METHOD = LITERAL (Read)
*          Specify which map-maker should be used to construct the
*          map. The parameter can take the following values:
*          - "REBIN" -- Use a single pass rebinning algorithm. This
*          technique assumes that the data have previously had
*          atmosphere and instrument signatures removed. It makes use
*          of the standard AST library rebinning algorithms (see also
*          KAPPA:WCSMOSAIC). It is an excellent choice for obtaining an
*          image quickly, especially of a bright source.
*          - "ITERATE" -- Use the iterative map maker. This map maker
*          is much slower than the REBIN algorithm because it
*          continually makes a map, constructs models for different
*          data components (common-mode, spikes etc.). See CONFIG for
*          parameters controlling the iterative map maker.
*          [ITERATE]
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     NBOLOEFF = _DOUBLE (Write)
*          Effective number of bolometers in the output map when
*          METHOD=iterate. [!]
*     NCONTCHUNK = _INTEGER (Write)
*          Total number of continuous data chunks processed by makemap
*          when METHOD=iterate. [!]
*     NMCNVG = _INTEGER (Write)
*          Total number of continuous data chunks processed by makemap
*          when METHOD=iterate that failed to converge. [!]
*     NMINSMP = _INTEGER (Write)
*          Total number of continuous data chunks processed by makemap
*          when METHOD=iterate that failed due to insufficient samples. [!]
*     NOMFCF = _DOUBLE (Write)
*          The nominal beam FCF for the output map (Jy/beam/pW). This will
*          depend on the observation dates of the input time-seroes files
*          and the filter (450 or 850). If the input data spans a critical
*          date at which the FCF changed, the stored value will be weighted
*          by the amount of data included in the map that was taken before
*          and after the critical date.
*     NTILE = _INTEGER (Write)
*          The number of output tiles used to hold the entire output
*          array (see parameters JSATILES and TILEDIMS). If no input data
*          fall within a specified tile, then no output NDF will be created
*          for the tile, but (if JSATILES is FALSE) the tile will still be
*          included in the tile numbering.
*     OUT = NDF (Write)
*          Output file.
*     OUTFILES = LITERAL (Write)
*          The name of a text file to create, in which to put the names of
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
*          to the point-spread function of the input data.
*     PIXSIZE( 2 ) = _REAL (Read)
*          Pixel dimensions in the output image, in arcsec. If only one value
*          is supplied, the same value will be used for both axes. The default
*          depends on the wavelength of the input data.
*     POINTING = LITERAL (Read)
*          The name of a text file containing corrections to the pointing
*          read from the input data files. If null (!) is supplied, no
*          corrections are used. If a file is supplied, it should start
*          with one or more lines containing "#" in column one. These are
*          comment lines, but if any comment line has the form "# SYSTEM=AZEL"
*          or "# SYSTEM=TRACKING" then it determines the system in which the
*          pointing correction are specified (SYSTEM defaults to AZEL). The
*          last comment line should be a space-separated list of column names,
*          including "TAI", "DLON" and "DLAT". Each remaining line should
*          contain numerical values for each column, separated by white space.
*          The TAI column should contain the TAI time given as an MJD. The
*          DLON and DLAT columns should give arc-distance offsets parallel
*          to the longitude and latitude axes, in arc-seconds. The TAI values
*          should be monotonic increasing with row number. The longitude and
*          latitude axes are either AZEL or TRACKING as determined by the
*          SYSTEM value in the header comments. Blank lines are ignored.
*          The DLON and DLAT values are added onto the SMU jiggle positions
*          stored in the JCMTSTATE extension of the input NDFs. DLON and DLAT
*          values for non-tabulated times are determined by interpolation.
*
*          If you need to apply two sets of pointing corrections, one in
*          TRACKING and one in AZEL, you can include two tables (one for
*          each system) in a single text file. Both tables should use the
*          format described above. The two tables must be separated by a line
*          containing two or more minus signs with no leading spaces. [!]
*     RATE_LIMITED = _LOGICAL (Write)
*          Set TRUE on exit if the iterative loop was terminated because
*          the mean normalised change in the map does not seem to be
*          falling (see config parameter "MAPTOL_RATE").
*     REF = NDF (Read)
*          An existing NDF that is to be used to define the output grid,
*          or the string "JSA". If an NDF is supplied, the output grid will
*          be aligned with the supplied reference NDF. The reference can be
*          either 2D or 3D and the spatial frame will be extracted. If "JSA"
*          is supplied, the JSA all-sky pixel grid will be used (note,
*          the map will still be created as a single NDF - if multiple NDFs,
*          one for each JSA tile, are required, the "JSATILES" parameter
*          should beset TRUE instead of using the "REF" parameter). If a
*          null (!) value is supplied then the output grid is determined
*          by parameters REFLON, REFLAT, etc. In addition, this NDF can
*          be used to mask the AST, FLT or COM model. See configuration
*          parameters AST.ZERO_MASK, FLT.ZERO_MASK and COM.ZERO_MASK. [!]
*     REFLAT = LITERAL (Read)
*          The formatted celestial latitude value at the tangent point of
*          the spatial projection in the output cube. This should be provided
*          in the coordinate system specified by parameter SYSTEM.
*     REFLON = LITERAL (Read)
*          The formatted celestial longitude value at the tangent point of
*          the spatial projection in the output cube. This should be provided
*          in the system specified by parameter SYSTEM.
*     RESIST = GROUP (Read)
*          A group expression containing the resistor settings for
*          each bolometer.  Usually specified as a text file using "^"
*          syntax. An example can be found in
*          $STARLINK_DIR/share/smurf/resist.cfg
*          [$STARLINK_DIR/share/smurf/resist.cfg]
*     RESPMASK = _LOGICAL (Read)
*          If true, responsivity data will be used to mask bolometer data
*          when calculating the flatfield. [TRUE]
*     SPREAD = LITERAL (Read)
*          The method to use when spreading each input pixel value out
*          between a group of neighbouring output pixels if using
*          METHOD=REBIN (for METHOD=ITERATE nearest-neighbour
*          resampling is always used). If SPARSE is set TRUE, then
*          SPREAD is not accessed and a value of "Nearest" is always
*          assumed. SPREAD can take the following values:

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
*          The celestial coordinate system for the output cube. One of
*          ICRS, GAPPT, FK5, FK4, FK4-NO-E, AZEL, GALACTIC, ECLIPTIC. It
*          can also be given the value "TRACKING", in which case the
*          system used will be which ever system was used as the tracking
*          system during the observation. The supplied value is ignored
*          if a value is supplied for parameter "REF".
*
*          The choice of system also determines if the telescope is
*          considered to be tracking a moving object such as a planet or
*          asteroid. If the system is GAPPT or AZEL, then each time slice in
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
*          1). The number of tiles used to cover the entire output array is
*          written to output parameter NTILES. The tiles all share the same
*          projection and so can be simply pasted together in pixel
*          coordinates to reconstruct the full size output array. The tiles
*          are centred so that the reference position (given by REFLON and
*          REFLAT) falls at the centre of a tile. If a tile receives no
*          input data, then no corresponding output NDF is created, but
*          the tile is still included in the tile numbering scheme. If a
*          null (!) value is supplied for TILEDIMS, then the
*          entire output array is created as a single tile and stored in
*          a single output NDF with the name given by parameter OUT
*          (without any "_<N>" appendix). [!]
*     TRIM = _LOGICAL (Read)
*          If TRUE, then the output image is trimmed to remove any border
*          of bad pixels. [FALSE]
*     TRIMTILES = _LOGICAL (Read)
*          Only accessed if the output is being split up into more than
*          one spatial tile (see parameter TILEDIMS and JSATILES). If TRUE,
*          then the tiles around the border will be trimmed to exclude areas
*          that fall outside the bounds of the full sized output array. This
*          will result in the border tiles being smaller than the central
*          tiles. [FALSE]
*     UBND( 2 ) = _INTEGER (Read)
*          An array of values giving the upper pixel index bound on each
*          spatial axis of the output NDF. The suggested default values
*          encompass all the input spatial information. The supplied
*          values may be modified if TRIM is set TRUE. []
*     UBOUND( 2 ) = _INTEGER (Write)
*          The upper pixel bounds of the output NDF. Note, values will be
*          written to this output parameter even if a null value is supplied
*          for parameter OUT.
*     WVMLOG = FILENAME (Write)
*          Name of a text file into which to write raw WVM data with
*          columns for each stage of processing.  Multiple blocks of data
*          may be written to the file if the WVM data are processed in
*          chunks.  New data are appended to the file if it already exists.
*          If a null (!) value is supplied, no file is written. [!]

*  Related Applications:
*     SMURF: QLMAKEMAP

*  Notes:
*     - If multiple masks are specified for a single model component,
*     then the source areas of the individual masks are combined together
*     to form the total mask. For instance, if values are supplied for
*     both AST.ZERO_MASK and AST.ZERO_LOWHITS, then a pixel in the total
*     mask will be considered to be a "source" pixel if it is a source
*     pixel in either the external mask specified by AST.ZERO_MASK, or in
*     the "low hits" mask.
*     - The iterative algorithm can be terminated prematurely by pressing
*     control-C at any time. If this is done, the current iteration will
*     complete and the user will then be asked how to continue. Options
*     include: 1) abort immediately without an output map, 2) close
*     retaining the current unfinalised output map, and 3) perform one
*     more iteration to finalise the map and then close. Note, if control-C
*     is pressed a second time, the application will abort immediately,
*     potentially leaving files in an unclean state.
*     - A FITS extension is added to the output NDF containing any keywords
*     that are common to all input NDFs. To be included in the output
*     FITS extension, a FITS keyword must be present in the NDF extension
*     of every input NDF, and it must have the same value in all input
*     NDFs. In addition, certain headers that relate to start and end
*     events are propogated from the oldest and newest files respectively.
*     - The output NDF will contain an extension named "SMURF"
*     containing an NDF named "EXP_TIME", which contains the exposure
*     time associated with each pixel.
*     - The FITS keyword EXP_TIME is added to the output FITS
*     extension. This header contains the median value of the EXP_TIME
*     array (stored in the SMURF extension of the output NDF).If this
*     value cannot be calculated for any reason, the corresponding
*     FITS keyword is assigned a blank value.
*     - If parameter TILEDIMS is assigned a value, FITS keywords NUMTILES
*     and TILENUM are added to the output FITS header. These are the number
*     of tiles used to hold the output data, and the index of the NDF
*     containing the header, in the range 1 to NUMTILES, but if
*     JSATILES is TRUE then FITS keyword TILENUM is also added
*     but is instead used for the JSA tile number in the range 0 to
*     12 * Nside ^ 2 - 1.
*     - The model configuration parameters can be sub-instrument dependent.
*     For example, 850.flt.edgelow will copy the edgelow value into the flt
*     section only for 850 micron data. Similarly for 450.flt.edgelow.
*     - Default values can be read from the $SMURF_DIR/smurf_makemap.def file.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     Edward Chapin (UBC)
*     David Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     2005-09-27 (EC):
*        Clone from smurf_extinction
*     2005-12-16 (EC):
*        Working for simple test case with astRebinSeq
*     2006-01-04 (EC):
*        Properly setting rebinflags
*     2006-01-13 (EC):
*        Automatically determine map size
*        Use VAL__BADD for pixels with no data in output map
*     2006-01-25 (TIMJ):
*        Replace malloc with smf_malloc.
*     2006-01-25 (TIMJ):
*        sc2head is now embedded in smfHead.
*     2006-01-27 (TIMJ):
*        - Try to jump out of loop if status bad.
*        - sc2head is now a pointer again
*     2006-02-02 (EC):
*        - Broke up mapbounds/regridding into subroutines smf_mapbounds and
*          smf_rebinmap
*        - fits header written to output using ndfputwcs
*     2006-03-23 (AGG):
*        Update to take account of new API for rebinmap
*     2006-03-23 (DSB):
*        Guard against null pointer when reporting error.
*     2006-04-21 (AGG):
*        Now calls sky removal and extinction correction routines.
*     2006-05-24 (AGG):
*        Check that the weights array pointer is not NULL
*     2006-05-25 (EC):
*        Add iterative map-maker + associated command line parameters
*     2006-06-24 (EC):
*        Iterative map-maker parameters given in CONFIG file
*     2006-08-07 (TIMJ):
*        GRP__NOID is not a Fortran concept.
*     2006-08-21 (JB):
*        Write data, variance, and weights using smfData structures
*     2006-08-22 (JB):
*        Add odata for output, add smf_close_file for odata.
*     2006-10-11 (AGG):
*        - Update to new API for smf_open_newfile, remove need for dims array
*        - Remove calls to subtract sky and correct for extinction
*     2006-10-12 (JB):
*        Use bad bolometer mask if supplied; add usebad flag
*     2006-12-18 (AGG):
*        Fix incorrect indf declaration, delete ogrp if it exists
*     2007-01-12 (AGG):
*        Add SYSTEM parameter for specifying output coordinate system
*     2007-01-25 (AGG):
*        Update API in calls to smf_mapbounds and smf_rebinmap
*     2007-02-06 (AGG):
*        Add uselonlat flag rather that specify hard-wired value in
*        smf_mapbounds
*     2007-03-05 (EC):
*        Changed smf_correct_extinction interface
*     2007-03-20 (TIMJ):
*        Write an output FITS header
*     2007-06-22 (TIMJ):
*        Rework to handle PRV* as well as OBS*
*     2007-07-05 (TIMJ):
*        Fix provenance file name handling.
*     2007-07-12 (EC):
*        Add moving to smf_bbrebinmap interface
*        Add moving to smf_calc_mapcoord interface
*     2007-10-29 (EC):
*        Modified interface to smf_open_file.
*     2007-11-15 (EC):
*        Modified smf_iteratemap interface.
*     2007-11-28 (EC):
*        Fixed flag in smf_open_file
*     2008-01-22 (EC):
*        Added hitsmap to smf_iteratemap interface
*     2008-02-12 (AGG):
*        - Update to reflect new API for smf_rebinmap
*        - Note smf_bbrebinmap is now deprecated
*        - Remove sky subtraction and extinction calls
*     2008-02-13 (AGG):
*        Add SPREAD and PARAMS parameters to allow choice of
*        pixel-spreading scheme, update call to smf_rebinmap
*     2008-02-15 (AGG):
*        Expand number of dimensions for weights array if using REBIN
*     2008-02-18 (AGG):
*        - Check for all ADAM parameters before call to smf_mapbounds
*        - Change weightsloc to smurfloc
*        - Add EXP_TIME component to output file
*     2008-02-19 (AGG):
*        - Add status check before attempting to access hitsmap pointer
*        - Set exp_time values to BAD if no data exists for that pixel
*     2008-02-20 (AGG):
*        Calculate median exposure time and write FITS entry
*     2008-03-11 (AGG):
*        Update call to smf_rebinmap
*     2008-04-01 (AGG):
*        Write WCS to EXP_TIME component in output file
*     2008-04-02 (AGG):
*        Write 2-D WEIGHTS component + WCS in output file, protect
*        against attempting to access NULL smfFile pointer
*     2008-04-22 (AGG):
*        Use faster histogram-based method for calculating median
*        exposure time
*     2008-04-23 (DSB):
*        Modify call to kpg1Ghstd to pass max and min values by reference
*        rather than by value.
*     2008-04-24 (EC):
*        Added MAXMEM parameter, memory checking for output map
*     2008-05-01 (TIMJ):
*        - Use BAD in EXP_TIME when no integration time.
*        - Tidy up some status logic.
*        - Add units and label to output file.
*     2008-05-02 (EC):
*        - Added mapbounds timing message
*     2008-05-03 (EC):
*        - In provenance loop for iterate use READ instead of UPDATE
*     2008-05-14 (EC):
*        Added projection functionality cloned from smurf_makecube. See
*        ADAM parameters: PIXSIZE, REFLAT, REFLON, [L/U]BND/BOUND.
*     2008-05-15 (EC):
*        - Trap SMF__NOMEM from smf_checkmem_map; request new [L/U]BND
*        - Set [L/U]BOUND
*        - Fix read error caused by status checking in for loop
*     2008-05-26 (EC):
*        - changed default map size to use ~50% of memory if too big
*        - handle OUT=! case to test map size
*        - started adding tiling infrastructure: NTILE + TILEDIMS
*        - check for minimum numbin for histogram
*     2008-05-28 (TIMJ):
*        "Proper" provenance propagation.
*     2008-05-29 (EC):
*        Don't call smf_checkmem_map if OUT=!
*     2008-06-02 (EC):
*        Handle 1-element TILEDIMS
*     2008-06-04 (TIMJ):
*        - Calculate and free smfBox in smf_mapbounds
*        - Handle pixel bounds in smf_mapbounds.
*        - Use smf_store_outputbounds and document new F* parameters.
*     2008-06-05 (TIMJ):
*        - Add ALIGNSYS parameter.
*        - Add REF parameter.
*     2008-06-06 (TIMJ):
*        - support TILES in REBIN mode
*        - use smf_find_median
*        - new interface to smf_open_ndfname
*     2008-07-11 (TIMJ):
*        use strlcat
*     2008-07-22 (AGG):
*        Refactor provenance handling loop, initialize spread
*        parameter to AST__NEAREST
*     2008-07-25 (TIMJ):
*        Filter out darks. Use kaplibs.
*     2008-07-29 (TIMJ):
*        Steptime is now in smfHead.
*     2008-08-22 (AGG):
*        Check coordinate system before writing frameset to output
*        file and set attributes for moving sources accordingly
*     2008-08-26 (AGG):
*        Check for non-NULL return value from astGetC before
*        attempting string comparison
*     2008-08-27 (AGG):
*        Factor out WCS check for moving sources to smf_set_moving
*     2008-12-12 (TIMJ):
*        Add BPM parameter.
*     2009-07-21 (EC)
*        Updated header comments:
*        - re-tabbed some sections to fit into 80 columns
*        - define MAXMEM ADAM parameter
*        - Added "Configuration Parameters" section
*     2009-10-07 (DSB):
*        Update to reflect new smf_choosetiles behaviour.
*     2009-10-08 (EC):
*        Calculate dynamic default value for MAXMEM using sysconf call
*     2009-10-27 (DSB):
*        Add a spectral axis to the output NDF.
*     2010-01-08 (AGG):
*        Change BPM to BBM.
*     2010-03-11 (TIMJ):
*        Support flatfield ramps.
*     2010-04-13 (EC):
*        Support short maps with smf_iteratemap.
*     2010-04-20 (EC):
*        iteratemaps now have quality
*     2010-05-04 (TIMJ):
*        Use kpg1Config for reading config parameters including defaults values.
*        This also required some code to determine the current sub-instrument.
*     2010-05-07 (TIMJ):
*        Set bad-bits mask so that QUALITY has an effect.
*     2010-05-27 (TIMJ):
*        Write out NBOLOEFF information.
*     2010-06-28 (TIMJ):
*        Properly support varying step time.
*     2010-10-15 (TIMJ):
*        Slight speed up in provenance loop.
*     2011-01-12 (TIMJ):
*        Trigger an error if there are no science files. This makes it easier
*        for the pipeline to know something has gone wrong.
*     2011-03-11 (DSB):
*        Add TRIMBAD parameter (also accessed in smf_mapbounds).
*     2011-03-14 (DSB):
*        Rename TRIMBAD parameter as TRIM (for consistency with MAKECUBE).
*     2011-05-17 (DSB):
*        Added parameter POINTING.
*     2011-06-27 (EC):
*        Added NCONTCHUNK, NMINSMP, NMCNVG
*     2011-06-29 (EC):
*        Added sampcubes extensions to iterative map-maker
*     2012-01-31 (DSB):
*        Run smf_iteratemap twice if ast.zero_snr_fwhm is set. The second
*        run uses a static zero mask produced by smoothing the final mask
*        in use at the end of the first run.
*     2012-05-01 (DSB):
*        Allow premature termination of iterative algorithm by pressing
*        control-C.
*     2012-18-10 (DSB):
*        Store provenance after the map has been made (rather than before)
*        so that all parameter values recorded are sure to be the ones which
*        were actually used rather than ones from the previous invocation
*        that were stored in the parameter file on start-up.
*     2013-02-25 (DSB):
*        Correct calculation of default MAXMEM value. Previously, the
*        default was to leave 4GB spare for machines with 4GB or more of
*        memory, and to leave 20% spare for machines with less than 4GB.
*        This was bad news for machines with 4GB of memory!
*     2013-07-03 (DSB):
*        If makemap is interupted with control-C and the user chooses to
*        save the current map, add an extension item to the map saying
*        how many iterations were completed.
*     2013-07-04 (DSB):
*        Added parameter ITERMAPS.
*     2013-08-21 (AGG):
*        Do not call grpList if no output files are generated. This
*        avoids a GRP__INVID error in such cases.
*     2013-11-08 (DSB):
*        Added the "JSATILES" parameter, and made other changes to allow
*        the output map to be split up into JSA tiles.
*     2013-11-25 (DSB):
*        smf_mapbounds fast mode does not work for the JSA all-sky pixel
*        grid since the maps are rotated with respect to north.
*     2013-11-27 (DSB):
*        Ensure the NTILE parameter is written before the OUT parameter is
*        accessed (unless JSA tiles are being created).
*     2014-03-31 (DSB):
*        smf_mapbounds fast mode has been fixed so that it now works for
*        the JSA all-sky pixel grid.
*     2016-01-13 (DSB):
*        Store total expsoure time in map FITS extension.
*     2017-03-2 (DSB):
*        Added parameters ABORTSOON and ABORTEDAT.
*     2019-03-19 (GSB):
*        Mention WVMLOG parameter in the documentation.
*     2019-06-19 (DSB):
*        Add FITS header NCONTNCV to the output map, holding the number of
*        contiguous chunks that failed to converge.
*     2019-07-9 (DSB):
*        Ensure no output NDF is created if an error occurs.
*     2022-1-19 (DSB):
*        Added output parameter NOMFCF and FITS header NOMFCF
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2005-2010,2013 University of British Columbia.
*     Copyright (C) 2007-2012 Science and Technology Facilities Council.
*     Copyright (C) 2017-2019 East Asian Observatory.
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
#include <sys/time.h>
#include <time.h>
#include <math.h>
#include <unistd.h>

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
#include "star/kaplibs.h"
#include "star/atl.h"
#include "star/one.h"
#include "star/thr.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"
#include "libsmf/jsatiles.h"
#include "libsmf/smf_err.h"
#include "smurf_typ.h"

#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"

#include "libsc2sim/sc2sim.h"

#define FUNC_NAME "smurf_makemap"
#define TASK_NAME "MAKEMAP"
#define LEN__METHOD 20

void smurf_makemap( int *status ) {

  /* Local Variables */
  int abortedat;             /* Value for parameter ABORTEDAT */
  int abortsoon;             /* Value of parameter ABORTSOON */
  int alignsys;              /* Align data in the output system? */
  char basename[ GRP__SZNAM + 1 ]; /* Output base file name */
  int blank=0;                 /* Was a blank line just output? */
  smfBox *boxes = NULL;      /* Pointer to array of i/p file bounding boxes */
  smfArray *bbms = NULL;     /* Bad bolometer masks */
  smfArray *darks = NULL;   /* Dark data */
  smfData *data=NULL;        /* Pointer to SCUBA2 data struct */
  char data_label[SMF__CHARLABEL+1]; /* NDF label for output map */
  char data_units[SMF__CHARLABEL+1]; /* Units string */
  double *exp_time = NULL;    /* Exposure time array written to output file */
  AstFitsChan *fchan = NULL; /* FitsChan holding output NDF FITS extension */
  Grp * fgrp = NULL;         /* Filtered group, no darks */
  smfFile *file=NULL;        /* Pointer to SCUBA2 data file struct */
  int first;                 /* Is this the first input file? */
  smfArray *flatramps = NULL;/* Flatfield ramps */
  fts2Port fts_port = NO_FTS;/* FTS-2 port */
  char fts_port_name[10];    /* FTS-2 port name */
  int gotbadflat = 0;        /* Was one of the required flats bad? */
  AstKeyMap *heateffmap = NULL;    /* Heater efficiency data */
  dim_t *histogram = NULL;   /* Histogram for calculating exposure statistics */
  int *hitsmap;              /* Hitsmap array calculated in ITERATE method */
  int i;                     /* Loop counter */
  dim_t ii;                  /* Loop counter */
  int ifile;                 /* Input file index */
  Grp *igrp = NULL;          /* Group of input files */
  Grp *igrp4 = NULL;         /* Group holding output NDF names */
  int ilast;                 /* Index of the last input file */
  int iout;                  /* Index of next output NDF name */
  int ipbin=0;               /* Index of current polarisation angle bin */
  int isjsa;                 /* Are we making a map on the JSA all-sky pixel grid? */
  int iterate=0;             /* Flag to denote ITERATE method */
  int iters;                 /* If interupted, the no. of completed iterations */
  dim_t itile;               /* Output tile index */
  int ival;                  /* Integer parameter value */
  int jin;                   /* Input NDF index within igrp */
  int jsatiles;              /* Create JSA tiles? */
  int junk;                  /* Unused integer */
  AstKeyMap *keymap=NULL;    /* Pointer to keymap of config settings */
  dim_t lbnd_out[2];         /* Lower pixel bounds for output map */
  double *map=NULL;          /* Pointer to the rebinned map data */
  size_t mapmem=0;           /* Memory needed for output map */
  smf_qual_t *mapqual=NULL;  /* Map quality */
  size_t maxmem=0;           /* Max memory usage in bytes */
  int maxmem_mib;            /* Max memory usage in MiB */
  int maxmem_default = 1000; /* Default value for maxmem */
  double maxtexp = 0.0;      /* Maximum exposure time */
  double meanstep = 0.0;     /* Mean steptime for output map */
  float medtexp = 0.0;       /* Median exposure time */
  char method[LEN__METHOD];  /* String for map-making method */
  int moving = 0;            /* Is the telescope base position changing? */
  int nparam = 0;            /* Number of extra parameters for pixel spreading*/
  int njsatile;              /* Number of output JSA tiles */
  dim_t ntile;               /* Number of output tiles */
  int64_t nused;             /* No. of used input samples */
  int nval;                  /* Number of parameter values supplied */
  dim_t nxy;                 /* Number of pixels in output image */
  smfData *odata=NULL;       /* Pointer to output SCUBA2 data struct */
  Grp *ogrp = NULL;          /* Group containing output file */
  char oname[SMF_PATH_MAX+1];/* Name of output NDF */
  int ondf = NDF__NOID;      /* output NDF identifier */
  dim_t outsize;             /* Number of files in output group */
  size_t temp;               /* Temp size_t value */
  AstFrameSet *outfset=NULL; /* Frameset containing sky->output mapping */
  char pabuf[ 10 ];          /* Text buffer for parameter value */
  double params[ 4 ];        /* astRebinSeq parameters */
  char *pname = NULL;        /* Name of currently opened data file */
  int ***ptime = NULL;       /* Holds time slice indices for each pol bin */
  int *pt = NULL;            /* Holds time slice indices for each pol bin */
  int rebin=1;               /* Flag to denote whether to use the REBIN method*/
  int size;                  /* Number of files in input group */
  int smfflags=0;            /* Flags for smfData */
  HDSLoc *smurfloc=NULL;     /* HDS locator of SMURF extension */
  AstFrameSet *spacerefwcs = NULL;/* WCS Frameset for spatial reference axes */
  AstFrameSet *specrefwcs = NULL;/* WCS Frameset for spectral reference axis */
  int spread = AST__NEAREST; /* Code for pixel spreading scheme */
  char system[10];           /* Celestial coordinate system for output image */
  smfData *tdata=NULL;       /* Exposure time data */
  int tileborder;            /* Dimensions (in pixels) of tile overlap */
  int tiledims[2];           /* Dimensions (in pixels) of each output tile */
  smfTile *tile = NULL;      /* Pointer to next output tile description */
  smfTile *tiles=NULL;       /* Pointer to array of output tile descriptions */
  int tndf = NDF__NOID;      /* NDF identifier for 3D output NDF */
  int trim;                  /* Trim the output image to exclude bad pixels? */
  int trimtiles;             /* Trim the border tiles to exclude bad pixels? */
  AstMapping *tskymap = NULL;/* GRID->SkyFrame Mapping from output tile WCS */
  struct timeval tv1, tv2;   /* Timers */
  dim_t ubnd_out[2];         /* Upper pixel bounds for output map */
  void *variance=NULL;       /* Pointer to the variance map */
  smfData *wdata=NULL;       /* Pointer to SCUBA2 data struct for weights */
  double *weights=NULL;      /* Pointer to the weights map */
  double *weights3d = NULL;  /* Pointer to 3-D weights array */
  AstFrameSet *wcstile2d = NULL;/* WCS Frameset describing 2D spatial axes */
  int wndf = NDF__NOID;      /* NDF identifier for WEIGHTS */
  ThrWorkForce *wf = NULL;   /* Pointer to a pool of worker threads */

  if (*status != SAI__OK) return;

  /* initialisation */
  data_units[0] = '\0';

  /*** TIMER ***/
  smf_timerinit( &tv1, &tv2, status );

  /* Main routine */
  ndfBegin();

  /* Find the number of cores/processors available and create a pool of
     threads of the same size. */
  wf = thrGetWorkforce( thrGetNThread( SMF__THREADS, status ), status );

  /* Get group of input files */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &temp, status );
  size = (int) temp;

  /* Filter out darks */
  smf_find_science( wf, igrp, &fgrp, 0, NULL, NULL, 1, 1, SMF__NULL, &darks,
                    &flatramps, &heateffmap, &meanstep, status );

  /*** TIMER ***/
  msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME ": %f s finding science obs",
             status, smf_timerupdate(&tv1,&tv2,status) );

  /* input group is now the filtered group so we can use that and
     free the old input group */
  size = (int) grpGrpsz( fgrp, status );
  grpDelet( &igrp, status);
  igrp = fgrp;
  fgrp = NULL;

  if (size == 0) {
    if (*status == SAI__OK) {
      *status = SMF__NOSCI;
      errRep( "", "No science frames supplied. Unable to make a map.",
              status );
    }
    goto L998;
  }

  /* if we ended up with bad flat field data we may still want to
     calculate the bounds of the output map using OUT=!. */
  if (*status == SMF__BADFLAT) {
    gotbadflat = 1;
    errMark();
    errAnnul(status);
  }

  /* Get group of bolometer masks and read them into a smfArray */
  smf_request_mask( wf, "BBM", &bbms, status );

  /* Get the celestial coordinate system for the output map */
  parChoic( "SYSTEM", "TRACKING", "TRACKING,FK5,ICRS,AZEL,GALACTIC,"
            "GAPPT,FK4,FK4-NO-E,ECLIPTIC", 1, system, 10, status );

  /* Get the maximum amount of memory that we can use */

  if( *status == SAI__OK ) {
    double freemem = 0.0;
    int64_t physsize = 0;

    (void)smf_get_freemem( NULL, NULL, &physsize, status );

    /* Convert physical memory to MB */
    freemem = physsize / SMF__MIB;

    if (physsize > 0 ) {
      const int min_mem = 512;
      const int max_freemem = 4096;

      /* Set default memory as 80% of physical memory, or leaving max_freemem
         MB available for other processes, whichever is larger. */
      maxmem_default = (int) (freemem*0.80);
      if( (freemem - maxmem_default) > max_freemem ) {
        maxmem_default = freemem - max_freemem;
      }

      /* Generate warning if this seems too small... */
      if( maxmem_default <= min_mem ) {
        *status = SAI__ERROR;
        errRepf( "", TASK_NAME ": Available memory estimated to be %d MiB but recommend at least %d MiB! You may wish to set MAXMEM manually.",
                 status, maxmem_default, min_mem );
      }

      msgOutiff( MSG__VERB, "", TASK_NAME
                 ": Default MAXMEM is %i MiB (largest of 80%% of physical "
                 "memory, or leaving 4096 MiB free)",
                 status, maxmem_default );
    } else {
      /* If smf_get_freemem can't tell us, try something safe...*/
      maxmem_default = 1000;

      msgOutiff( MSG__VERB, "", TASK_NAME
                 ": Can't determine memory, setting to %i MiB. You may wish to set MAXMEM manually.",
                 status, maxmem_default );
    }
  }

  /* Set dynamic default before querying the parameter */
  parDef0i( "MAXMEM", maxmem_default, status );
  parGdr0i( "MAXMEM", maxmem_default, 1, VAL__MAXI, 1, &maxmem_mib, status );
  if( *status==SAI__OK ) {
    maxmem = (size_t) maxmem_mib * SMF__MIB;
  }
  msgOutiff( MSG__NORM, "", TASK_NAME
             ": Map-maker will use no more than %i MiB of memory",
             status, maxmem_mib );

  /* Get METHOD - set rebin/iterate flags */
  parChoic( "METHOD", "REBIN", "REBIN, ITERATE.", 1,
            method, LEN__METHOD, status);

  if( *status == SAI__OK ) {
    if( strncmp( method, "REBIN", 5 ) == 0 ) {
      rebin = 1;
      iterate = 0;
    } else if ( strncmp( method, "ITERATE", 7 ) == 0 ) {
      rebin = 0;
      iterate = 1;
    }


    /* Get remaining parameters so errors are caught early */
    if( rebin ) {
      /* Obtain desired pixel-spreading scheme */
      parChoic( "SPREAD", "NEAREST", "NEAREST,LINEAR,SINC,"
                "SINCSINC,SINCCOS,SINCGAUSS,SOMB,SOMBCOS,GAUSS",
                1, pabuf, 10, status );

      smf_get_spread( pabuf, &spread, &nparam, status );

      /* Get an additional parameter vector if required. */
      if( nparam>0 ) {
        parExacd( "PARAMS", nparam, params, status );
      }

    } else if ( iterate ) {
      /* Read a group of configuration settings into keymap using
         defaults and typo checking. Also need to know which
         subinstrument we are actively interested in for merging
         purposes. We need to open a representative file for that. */

      AstKeyMap * sub_instruments = smf_subinst_keymap( SMF__SUBINST_NONE,
                                                        NULL, igrp, 1, status );
      keymap = kpg1Config( "CONFIG", "$SMURF_DIR/smurf_makemap.def",
                           sub_instruments, 1, status );
      sub_instruments = astAnnul( sub_instruments );

      /* Set global values to reflect the contents of the above config keymap.
         These global values are stored in another KeyMap created in
         smurf_mon and accessed via the smurf_global_keymap pointer
         declared within smf.h. This provides a mechanism for getting
         config values to low level functions that do not have access to
         the config keymap (e.g. smf_fix_metadata_scuba2). It can also be
         used to communicate any other required global values (i.e. it's
         not restricted to config parameters). */
       astMapGet0I( keymap, "VALIDATE_SCANS", &ival );
       smf_put_global0I( "VALIDATE_SCANS", ival, status );

       astMapGet0I( keymap, "FILLGAPS_NOISE", &ival );
       smf_put_global0I( "FILLGAPS_NOISE", ival, status );
    }

    parChoic("FTSPORT", "", "TRACKING,IMAGE", 0, fts_port_name, 10, status);
    if (*status == PAR__NULL) {
        errAnnul(status);
        fts_port = NO_FTS;
    }
    else {
        if (! strcmp("TRACKING", fts_port_name)) {
            fts_port = FTS_TRACKING;
        }
        else {
            fts_port = FTS_IMAGE;
        }
    }
  }

  /* Allow the user to specify a text file containing a table of pointing
     corrections. Corresponding Mappings are created from the column data
     and stored in the "igrp" group as items of metadata. */
  smf_pread( igrp, "POINTING", status );

  /* Calculate the map bounds */

  smf_getrefwcs( "REF", igrp, &specrefwcs, &spacerefwcs, &isjsa, status );
  if( specrefwcs ) specrefwcs = astAnnul( specrefwcs );

  /* See if the input data is to be aligned in the output coordinate system
     rather than the default of ICRS. */
  parGet0l( "ALIGNSYS", &alignsys, status );

  msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Determine map bounds", status);

  smf_mapbounds( 1, igrp, size, system, spacerefwcs, alignsys,
                 lbnd_out, ubnd_out, &outfset, &moving, &boxes, fts_port,
                 keymap, status );

  msgBlank( status );

  /*** TIMER ***/
  msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME ": Mapbounds took %f s",
             status, smf_timerupdate(&tv1,&tv2,status) );

  /* Write WCS bounds */
  smf_store_outputbounds(1, lbnd_out, ubnd_out, outfset, NULL, NULL,
                         status);
  msgBlank( status );

  /* See if the output is to be split up into a number of separate tiles,
     each one being stored in a separate output NDF. If a null value is
     supplied for TILEDIMS, annul the error and retain the original NULL
     pointer for the array of tile structures (this is used as a flag that
     the entire output grid should be stored in a single output NDF). If
     we are producing JSA tiles, do not access the parameters for
     user-defined tiles. */
  parGet0l( "JSATILES", &jsatiles, status );
  if( !jsatiles && *status == SAI__OK ) {
    parGet1i( "TILEDIMS", 2, tiledims, &nval, status );
    if( *status == PAR__NULL ) {
      errAnnul( status );
    } else {
      parGet0l( "TRIMTILES", &trimtiles, status );
      parGet0i( "TILEBORDER", &tileborder, status );
      if( nval == 1 ) tiledims[ 1 ] = tiledims[ 0 ];
      tiles = smf_choosetiles( igrp, size, lbnd_out, ubnd_out, boxes,
                               spread, params, outfset, tiledims,
                               trimtiles, tileborder, &ntile, status );
    }
  }

  /* If we are not splitting the output up into user-defined tiles, then
     create an array containing a single tile description that encompasses
     the entire full size output grid. */

  if( !tiles ) {
    tiledims[ 0 ] = 0;
    tiles = smf_choosetiles( igrp, size, lbnd_out, ubnd_out, boxes,
                             spread, params, outfset, tiledims,
                             0, 0, &ntile, status );
  }

  /* Output the pixel bounds of the full size output array (not of an
     individual tile). */
  parPut1k( "LBOUND", 2, lbnd_out, status );
  parPut1k( "UBOUND", 2, ubnd_out, status );

  if ( moving ) {
    msgOutif(MSG__VERB, " ", "Tracking a moving object", status);
  } else {
    msgOutif(MSG__VERB, " ", "Tracking a stationary object", status);
  }

  /* See if output NDFs are to be trimmed to exclude bad borders. */
  parGet0l( "TRIM", &trim, status );

  /* If known, write the number of tiles being created to an output
     parameter. We do it here if possible so that a valid value is
     available to subsequent commands even if a null value is supplied
     for "OUT". But we cannot do it here if we are creating JSA tiles
     since we only know how many JSA tiles are being created once the
     cube has been created. */
  if( !jsatiles ) parPut0k( "NTILE", ntile, status );

  /* Create a new group to hold the names of the output NDFs that have been
     created. This group does not include any NDFs that correspond to tiles
     that contain no input data. */
  igrp4 = grpNew( "", status );

  /* Create an output smfData */
  if (*status == SAI__OK) {
    kpg1Wgndf( "OUT", NULL, 1, 1, NULL, &ogrp, &temp, status );
    outsize = temp;
    /* If OUT is NULL annul the bad status but set a flag so that we
       know to skip memory checks and actual map-making */
    if( *status == PAR__NULL ) {
      if (gotbadflat) errRlse();
      errAnnul( status );
      goto L998;
    }

    /* Expand the group to hold an output NDF name for each tile. */
    smf_expand_tilegroup( ogrp, ntile, 0, &outsize, status );
  }

  /* now we want that bad status from a bad flat to trigger */
  if (gotbadflat) {
    errRlse();
    *status = SMF__BADFLAT;
    errRep("", "Flatfield data not trusted so can not proceed to map-making",
           status );
  }

  /* Create the output map using the chosen METHOD */
  if ( rebin ) {

    /************************* R E B I N *************************************/

    /* Initialise the index of the next output NDF name to use in "ogrp". */
    iout = 1;

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
        msgSetk( "I", itile );
        msgSetk( "N", ntile );
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
        iout++;

        /* Proceed to the next tile. */
        continue;
      }

      /* Begin an AST context for the current tile. */
      astBegin;

      /* Begin an NDF context for the current tile. */
      ndfBegin();

      /* Create FrameSets that are appropriate for this tile. This involves
         remapping the base (GRID) Frame of the full size output WCS so that
         GRID position (1,1) corresponds to the centre of the first pixel in the
         tile. */
      wcstile2d = astCopy( outfset );
      if( tile->map2d ) astRemapFrame( wcstile2d, AST__BASE, tile->map2d );

      /* Get the Mapping from 2D GRID to SKY coords (the tiles equivalent of
         "oskymap"). */
      tskymap = astGetMapping( wcstile2d, AST__BASE, AST__CURRENT );

      /* Invert the output sky mapping so that it goes from sky to pixel
         coords. */
      astInvert( tskymap );

      /* Store the initial number of pixels per spatial plane in the output tile. */
      nxy = ( tile->eubnd[ 0 ] - tile->elbnd[ 0 ] + 1 )*
            ( tile->eubnd[ 1 ] - tile->elbnd[ 1 ] + 1 );

      /* Add the name of this output NDF to the group holding the names of the
         output NDFs that have actually been created. */
      pname = basename;
      grpGet( ogrp, iout, 1, &pname, GRP__SZNAM, status );
      grpPut1( igrp4, basename, 0, status );

      /* Create the 2D output NDF for this tile. */
      smfflags = 0;
      smfflags |= SMF__MAP_VAR;
      smf_open_newfile ( NULL, ogrp, iout++, SMF__DOUBLE, 2, tile->elbnd, tile->eubnd,
                         smfflags, &odata, status );

      /* Abort if an error has occurred. */
      if( *status != SAI__OK ) goto L999;

      /* we know this is a map */
      odata->qfamily = SMF__QFAM_MAP;

      /* Convenience pointers */
      file = odata->file;
      ondf = file->ndfid;

      /* Create SMURF extension in the output file and map pointers to
         WEIGHTS and EXP_TIME arrays */
      smurfloc = smf_get_xloc ( odata, SMURF__EXTNAME, SMURF__EXTTYPE, "WRITE", 0, 0, status );

      /* Create WEIGHTS component in output file */
      smf_open_ndfname ( smurfloc, "WRITE", "WEIGHTS", "NEW", "_DOUBLE",
                         2, tile->elbnd, tile->eubnd, "Weight", NULL, wcstile2d, &wdata, status );
      if ( wdata ) {
        weights = (wdata->pntr)[0];
        wndf = wdata->file->ndfid;
      }

      /* Create EXP_TIME component in output file */
      smf_open_ndfname ( smurfloc, "WRITE", "EXP_TIME", "NEW", "_DOUBLE",
                         2, tile->elbnd, tile->eubnd, "Total exposure time","s", wcstile2d,
                         &tdata, status );
      if ( tdata ) {
        exp_time = (tdata->pntr)[0];
      }

      /* Free the extension locator */
      datAnnul( &smurfloc, status );

      /* Now allocate memory for 3-d work array used by smf_rebinmap -
         plane 2 of this 3-D array is stored in the weights component
         later. Initialize to zero. We need one such weights array for
         each thread, so we actually allocate a 4D array. */
      weights3d = astCalloc( 2*nxy*wf->nworker, sizeof(double) );

      /* Each worker thread needs its own output array. This is needed since
         otherwise different threads may attempt to write to the same output
         pixel at the same time. We create a 3D array now in which the
         first 2 axes match the 2D output NDF dimensions, and the third axis
         has dimension equal to the number of worker threads. Once the
         rebinning is complete, these multiple output arrays are combined
         into one, and copied into the output NDF. */
      if( wf->nworker > 1 ) {
         map = astMalloc( (nxy*wf->nworker)*sizeof(double) );
         variance = astMalloc( (nxy*wf->nworker)*sizeof(double) );
      } else {
         map = (odata->pntr)[0];
         variance = (odata->pntr)[1];
      }

      /* Simple Regrid of the data */
      msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Make map using REBIN method",
               status);

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

          /* Read data from the ith input file in the group */
          smf_open_and_flatfield( wf, tile->grp, NULL, ifile, darks, flatramps,
                                  heateffmap, &data,status );

          /* Check that the data dimensions are 3 (for time ordered data) */
          if( *status == SAI__OK ) {
            if( data->ndims != 3 ) {
              msgSeti("I",ifile);
              msgSeti("THEDIMS", data->ndims);
              *status = SAI__ERROR;
              errRep(FUNC_NAME,
                     "File ^I data has ^THEDIMS dimensions, should be 3.",
                     status);
              break;
            }
          }

          /* Check that the input data type is double precision */
          if( *status == SAI__OK ) {
            if( data->dtype != SMF__DOUBLE) {
              msgSeti("I",ifile);
              msgSetc("DTYPE", smf_dtype_string( data, status ));
              *status = SAI__ERROR;
              errRep(FUNC_NAME,
                     "File ^I has ^DTYPE data type, should be DOUBLE.",
                     status);
              break;
            }
          }

          /* Check units are consistent */
          smf_check_units( ifile, data_units, data->hdr, status);

          /* Propagate provenance to the output file */
          smf_accumulate_prov( data, tile->grp, ifile, ondf, "SMURF:MAKEMAP(REBIN)",
                               NULL, status);

          /* Handle output FITS header creation */
          if (*status == SAI__OK)
            smf_fits_outhdr( data->hdr->fitshdr, &fchan, status );

          /* Report the name of the input file. */
          if (data->file && data->file->name) {
            smf_smfFile_msg( data->file, "FILE", 1, "<unknown>" );
            msgSeti( "THISFILE", ifile );
            msgSeti( "NUMFILES", tile->size );
            msgOutif( MSG__VERB, " ", "Processing ^FILE (^THISFILE/^NUMFILES)",
                      status );
          }

          /* Apply quality mask */
          smf_update_valbad( data, SMF__NUL, NULL, 0, 0, SMF__Q_GOOD, status );

          /* Mask out bad bolometers - mask data array not quality array */
          smf_apply_mask( wf, data, bbms, SMF__BBM_DATA, 0, status );

          /* Rebin the data onto the output grid. This also closes the
             data file.  */
          smf_rebinmap( wf, data, NULL, (first ? 1 : ifile), ilast, wcstile2d,
                        spread, params, moving, 1, tile->elbnd, tile->eubnd,
                        map, variance, weights3d, &nused, fts_port, status );
          first = 0;
          blank = 0;

          /* Break out of loop over data files if bad status */
          if (*status != SAI__OK) {
            errRep(FUNC_NAME, "Rebinning step failed", status);
            break;
          }
        }
      }
    L999:

   /* Wait untill all jobs have finished. If an error has occurred we may
      have aborted the above loop early leaving some threads still running.
      If we close down all NDFs now, etc, we may pull the carpet out from
      underneath these running threds, resulting in them suffering a
      segmentation fault. */
      thrWait( wf, status );

      /* If required, copy the data and variance arrays from the 3D work
         arrays into the output NDF, free the work arrays, and use the
         NDF arrays from here on. */
      if( wf->nworker > 1 ) {
         if( *status == SAI__OK ) {
            memcpy( (odata->pntr)[0], map, sizeof(double)*nxy );
            memcpy( (odata->pntr)[1], variance, sizeof(double)*nxy );
         }
         (void) astFree( map );
         (void) astFree( variance );
         map = (odata->pntr)[0];
         variance = (odata->pntr)[1];
      }

      /* Calculate exposure time per output pixel from weights array -
         note even if weights is a 3-D array we only use the first
         mapsize number of values which represent the `hits' per
         pixel */
      for (ii=0; (ii<nxy) && (*status == SAI__OK); ii++) {
        if ( map[ii] == VAL__BADD) {
          exp_time[ii] = VAL__BADD;
          weights[ii] = VAL__BADD;
        } else {
          exp_time[ii] = meanstep * weights3d[ii];
          weights[ii] = weights3d[ii+nxy];
          if ( exp_time[ii] > maxtexp ) {
            maxtexp = exp_time[ii];
          }
        }
      }
      weights3d = astFree( weights3d );

      /* Write WCS */
      if (wcstile2d) {
	smf_set_moving( (AstFrame *) wcstile2d, fchan, status );
	ndfPtwcs( wcstile2d, ondf, status );
      }

      /* write units - hack we do not have a smfHead */
      if (strlen(data_units)) ndfCput( data_units, ondf, "UNITS", status);
      ndfCput("Flux Density", ondf, "LABEL", status);

      /* Weights are related to data_units */
      one_strlcat(data_units, "**-2", sizeof(data_units), status);
      ndfCput(data_units, wndf, "UNITS", status);

      /* Put a separator in the output fits header to make it clear which headers
         have been added by the data processing.
         Wind to end of the fitschan first. */
      astSetI( fchan, "CARD", astGetI( fchan, "NCard" ) + 1 );
      astSetFitsCM( fchan, " ", 0 );
      astSetFitsCM( fchan, "---- Data Processing ----", 0 );

      /* Store the mean step time (overwriting an existing value if required) */
      atlPtftd( fchan, "STEPTIME", meanstep,
                "RTS step time during an RTS sequence", status );

      /* Calculate median exposure time - use faster histogram-based
         method which should be accurate enough for our purposes */
      msgOutif( MSG__VERB, " ", "Calculating median output exposure time",
                status );
      histogram = smf_find_median( NULL, exp_time, nxy, NULL, &medtexp, status );
      if ( medtexp != VAL__BADR ) {
        atlPtftr(fchan, "EXP_TIME", medtexp, "[s] Median MAKEMAP exposure time", status);
      }
      histogram = astFree( histogram );

      /* Store the keywords holding the number of user-defined tiles generated
         and the index of the current tile. */
      if( !jsatiles ) {
         atlPtfti( fchan, "NUMTILES", (int) ntile,
                   "No. of tiles covering the field", status );
         atlPtfti( fchan, "TILENUM", (int) itile,
                   "Index of this tile (1->NUMTILES)", status );
      }

      /* If the FitsChan is not empty, store it in the FITS extension of the
         output NDF (any existing FITS extension is deleted). Do not annul
         it as it is needed by smf_add_spectral_axis below, and will be
         annulled automatically by astEnd anyway. */
      if( astGetI( fchan, "NCard" ) > 0 ) kpgPtfts( ondf, fchan, status );

      /* Clone the main output NDF identifier so it will be available to
         smf_add_spectral_axis below. */
      ndfClone( ondf, &tndf, status );

      /* For each open output NDF (the main tile NDF, and any extension NDFs),
         first clone the NDF identifier, then close the file (which will unmap
         the NDF arrays), and then reshape the NDF to exclude the boundary
         that was added to the tile to avoid edge effects. */
      msgOutif( MSG__VERB, " ", "Reshaping output NDFs", status );
      smf_reshapendf( &tdata, tile, status );
      smf_reshapendf( &wdata, tile, status );
      smf_reshapendf( &odata, tile, status );

      /* If required trim any remaining bad borders. Note, this will
         unmap the NDF, but we do not need access to the data arrays
         anymore so that's OK. */
      if( trim && tndf != NDF__NOID ) {
         kpg1Badbx( tndf, 2, &junk, &junk, status );
      }

      /* Add a spectral axis to the main output NDF for this tile. */
      if( tndf != NDF__NOID ) smf_add_spectral_axis( tndf, fchan, status );

      /* End contexts for current tile*/
      ndfEnd(status);
      astEnd;
    }



















  } else if ( iterate ) {
    Grp *bolrootgrp = NULL;
    Grp *flagrootgrp = NULL;
    Grp *iterrootgrp = NULL;
    Grp *samprootgrp = NULL;
    Grp *shortrootgrp = NULL;
    Grp *cyclerootgrp = NULL;
    char tempfile[GRP__SZNAM+1];
    double nboloeff = 0.0;
    dim_t ncontchunks=0;
    dim_t ninsmp=0;
    dim_t ncnvg=0;
    dim_t ncontig=0;
    int memlow=0;
    NdgProvenance * oprov = NULL;
    double totexp;
    double nomfcf;

    /************************* I T E R A T E *************************************/

    smfflags = SMF__MAP_VAR | SMF__MAP_QUAL;
    smf_open_newfile ( wf, ogrp, 1, SMF__DOUBLE, 2, lbnd_out, ubnd_out, smfflags,
                       &odata, status );

    if ( *status == SAI__OK ) {
      file = odata->file;
      ondf = file->ndfid;
      strcpy( oname, file->name );
      odata->qfamily = SMF__QFAM_MAP;
      /* Map the data and variance arrays */
      map = (odata->pntr)[0];
      variance = (odata->pntr)[1];
      mapqual = odata->qual;
    }

    /* Work out the name for the root file path if bolomaps are being
       created */
    pname = tempfile;
    grpGet( ogrp, 1, 1, &pname, sizeof(tempfile), status );
    one_strlcat( tempfile, ".MORE.SMURF.BOLOMAPS", sizeof(tempfile), status);
    bolrootgrp = grpNew( "bolomap root", status );
    grpPut1( bolrootgrp, tempfile, 0, status );

    /* Similarly, work out the name for the root file path if itermaps
       are being created, allowing the user to specify an alternative
       destination other than the SMURF extension of the main output NDF. */
    iterrootgrp = grpNew( "itermap root", status );
    if( *status == SAI__OK ) {
       parGet0c( "ITERMAPS", tempfile, sizeof(tempfile), status );
       if( *status == PAR__NULL ) {
          errAnnul( status );
          grpGet( ogrp, 1, 1, &pname, sizeof(tempfile), status );
          one_strlcat( tempfile, ".MORE.SMURF.ITERMAPS", sizeof(tempfile), status);
       }
       grpPut1( iterrootgrp, tempfile, 0, status );
    }

    /* Work out the name for the root file path if shortmaps are being created*/
    grpGet( ogrp, 1, 1, &pname, sizeof(tempfile), status );
    one_strlcat( tempfile, ".MORE.SMURF.SHORTMAPS", sizeof(tempfile), status);
    shortrootgrp = grpNew( "shortmap root", status );
    grpPut1( shortrootgrp, tempfile, 0, status );

    /* Work out the name for the root file path if cyclemaps are being created */
    grpGet( ogrp, 1, 1, &pname, sizeof(tempfile), status );
    one_strlcat( tempfile, ".MORE.SMURF.CYCLEMAPS", sizeof(tempfile), status);
    cyclerootgrp = grpNew( "shortmap root", status );
    grpPut1( cyclerootgrp, tempfile, 0, status );

    /* Work out the name for the root file path if flagmaps are being created*/
    grpGet( ogrp, 1, 1, &pname, sizeof(tempfile), status );
    one_strlcat( tempfile, ".MORE.SMURF.FLAGMAPS", sizeof(tempfile), status);
    flagrootgrp = grpNew( "flagmap root", status );
    grpPut1( flagrootgrp, tempfile, 0, status );

    /* Work out the name for the sample cube file path if sampcubes are being
       created*/
    grpGet( ogrp, 1, 1, &pname, sizeof(tempfile), status );
    one_strlcat( tempfile, ".MORE.SMURF.SAMPCUBES", sizeof(tempfile), status);
    samprootgrp = grpNew( "sampcube root", status );
    grpPut1( samprootgrp, tempfile, 0, status );

    /* Compute number of pixels in output map */
    nxy = (ubnd_out[0] - lbnd_out[0] + 1) * (ubnd_out[1] - lbnd_out[1] + 1);

    /* Create SMURF extension in the output file and map pointers to
       WEIGHTS and EXP_TIME arrays */
    smurfloc = smf_get_smurfloc ( odata, "WRITE", status );

    /* Create WEIGHTS component in output file */
    smf_open_ndfname ( smurfloc, "WRITE", "WEIGHTS", "NEW", "_DOUBLE",
                       2, lbnd_out, ubnd_out, "Weight", NULL, outfset, &wdata, status );
    if ( wdata ) {
      weights = (wdata->pntr)[0];
      wndf = wdata->file->ndfid;
    }

    /* Create EXP_TIME component in output file */
    smf_open_ndfname ( smurfloc, "WRITE", "EXP_TIME", "NEW", "_DOUBLE",
                       2, lbnd_out, ubnd_out, "Total exposure time","s", outfset,
                       &tdata, status );
    if ( tdata ) {
      exp_time = (tdata->pntr)[0];
      /* initialise with zero exposure time */
      memset( exp_time, 0, nxy*sizeof(*exp_time) );
    }

    /* Free the extension locator */
    datAnnul( &smurfloc, status );

    /*** TIMER ***/
    msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME ": %f s on initial setup",
               status, smf_timerupdate(&tv1,&tv2,status) );

    /* Iterative map-maker */
    msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Make map using ITERATE method",
             status);

    /* Allocate space for hitsmap */
    hitsmap = astCalloc( nxy, sizeof (*hitsmap) );

    /* We can unlock the config KeyMap now since we have got the user's
       configuration (the point of locking it was to allow the user to
       get error messages describing any unknown parameters the supplied
       config). Unlocking it allows us to use the KeyMap to communicate
       values into the model calculators. */
    astSetI( keymap, "MapLocked", 0 );

    /* See if we should re-run iteratemap a second time using the mask
       determined from the first run. This is the case if AST.ZERO_SNR_FWHM
       and AST.ZERO_SNR are both non-zero. */
    AstObject *obj = NULL;
    astMapGet0A( keymap, "AST", &obj );
    AstKeyMap *astkmap = (AstKeyMap *) obj;
    double zero_snr_fwhm = -1.0;
    double zero_snr = 0.0;
    double zero_snr_low = -1.1;
    astMapGet0D( astkmap, "ZERO_SNR_FWHM", &zero_snr_fwhm );
    astMapGet0D( astkmap, "ZERO_SNR", &zero_snr );
    astMapGet0D( astkmap, "ZERO_SNR_LOW", &zero_snr_low );

    /* See if the iterative process should be aborted as soon as it
       becomes likely that convergence will not be achieved within the
       allowed number of iterations. */
    parGet0l( "ABORTSOON", &abortsoon, status );

    /* Perform the required number of calls to smf_iteratemap - one
    normally, but two if we want to re-run with a smoothed SNR mask. */
    int nrun = ( zero_snr > 0.0 && zero_snr_fwhm > 0.0 ) ? 2 : 1;
    double *snr_mask = NULL;
    int irun;
    for( irun = 0; irun < nrun; irun++ ) {

      /* Call the low-level iterative map-maker */
      smf_iteratemap( wf, igrp, iterrootgrp, bolrootgrp, shortrootgrp, cyclerootgrp,
                      flagrootgrp, samprootgrp, keymap, NULL, bbms, flatramps,
                      heateffmap, outfset, moving, lbnd_out, ubnd_out,
                      fts_port, maxmem-mapmem, abortsoon, &abortedat,
                      map, hitsmap, exp_time, variance, mapqual, weights, data_units,
                      data_label, &nboloeff, &ncontchunks, &ncontig, &memlow, &ninsmp,
                      &ncnvg, &iters, &totexp, &nomfcf, status );

      /* If we have just run smf_iteratemap for the second time, free the
         snr mask allocated after the first run. */
      if( irun == 1 ) {
         snr_mask = astFree( snr_mask );

      /* Otherwise, if we will be running smf_iteratemap again, create a
         smoothed copy of the SNR mask. */
      } else if( nrun == 2 ) {
         snr_mask = smf_smoothmask( wf, SMF__MAPQ_AST, mapqual,
                                    (int)(ubnd_out[0] - lbnd_out[0] + 1),
                                    (int)(ubnd_out[1] - lbnd_out[1] + 1),
                                    outfset, zero_snr_fwhm, zero_snr_low,
                                    status );

        /* Store a pointer to the smoothed mask in the KeyMap so that
           smf_calcmodel_ast can pick it up and use it in the same way
           that a  user-supplied mask is used. */
        astMapPut0P( astkmap, "ZERO_MASK_POINTER", snr_mask, NULL );

        /* Re-initialise all the output arrays. */
        memset( map, 0, nxy*sizeof(*map) );
        memset( hitsmap, 0, nxy*sizeof(*hitsmap) );
        memset( exp_time, 0, nxy*sizeof(*exp_time) );
        memset( variance, 0, nxy*sizeof(*map) );
        memset( mapqual, 0, nxy*sizeof(*mapqual) );
        memset( weights, 0, nxy*sizeof(*weights) );

        /* Tell the user we are about to start all over again... */
        msgOutiff( MSG__DEBUG, "", "\n\n" FUNC_NAME ": making a new "
                   "map using the smoothed SNR mask from the first "
                   "map.\n\n", status );
      }
    }

    if( astkmap ) astkmap = astAnnul( astkmap );

    /* Now that the map is created and all parameters have been accessed,
       history information and provenance can now be stored in the output.
       Loop over all input data files to setup provenance handling */
    for(i=1; (i<=size) && ( *status == SAI__OK ); i++ ) {
      smf_open_file( wf, igrp, i, "READ", SMF__NOCREATE_DATA, &data, status );
      if( *status != SAI__OK) {
        msgSetk("I",i);
        msgSeti("S",size);
        errRep(FUNC_NAME, "Error opening input file ^I of ^S for provenance tracking", status);
      }

      /* Propagate provenance to the output file */
      smf_accumulate_prov( data, igrp, i, ondf, "SMURF:MAKEMAP(ITER)",
                           &oprov, status);

      /* Handle output FITS header creation (since the file is open and
         the header is available) */
      smf_fits_outhdr( data->hdr->fitshdr, &fchan, status );

      /* close the input file */
      smf_close_file( wf, &data, status );
    }

    /* Flush the provenance */
    if (oprov) {
      ndgWriteProv( oprov, ondf, 1, status );
      oprov = ndgFreeProv( oprov, status );
    }

    /*** TIMER ***/
    msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME ": %f s setting up provenance",
               status, smf_timerupdate(&tv1,&tv2,status) );

    /* Write ADAM parameters */
    parPut0i( "ABORTEDAT", abortedat, status );
    parPut0d( "NBOLOEFF", nboloeff, status );
    parPut0i( "NCONTCHUNK", (int) ncontchunks, status );
    parPut0i( "NMINSMP", (int) ninsmp, status );
    parPut0i( "NMCNVG", (int) ncnvg, status );

    if( bolrootgrp ) grpDelet( &bolrootgrp, status );
    if( iterrootgrp ) grpDelet( &iterrootgrp, status );
    if( shortrootgrp ) grpDelet( &shortrootgrp, status );
    if( cyclerootgrp ) grpDelet( &cyclerootgrp, status );
    if( flagrootgrp ) grpDelet( &flagrootgrp, status );
    if( samprootgrp ) grpDelet( &samprootgrp, status );

    /* free the hits map */
    hitsmap = astFree( hitsmap );

    /* Set array to BAD where we have no integration time (otherwise the
     median calculation will not work properly) */
    for (ii=0; (ii<nxy) && (*status == SAI__OK); ii++) {
      if ( map[ii] == VAL__BADD ) exp_time[ii] = VAL__BADD;
    }

    /* Write WCS */
    smf_set_moving( (AstFrame *) outfset, fchan, status );
    ndfPtwcs( outfset, ondf, status );

    /* If the map was completed prematurely due to an interupt, add an
       item to the smurf extension of the output map indicating how many
       iterations were completed. This is only useful if the data was
       processed in a single chunk. */
    if( iters != -1 && ncontchunks == 1 ) {
       ndfXpt0i( iters, ondf, SMURF__EXTNAME, "NUMITER", status );
    }

    /* Set bad bits mask to enable QUALITY. We can have quality bits set
       indicating where a boundary condition to zero the map has been used.
       If ast.zero_notlast is set the boundary condition was not applied on
       the last iteration so don't enable QUALITY bits for this case. */

    if (mapqual) {
      AstKeyMap *kmap=NULL; /* Local AST keymap */
      int zero_notlast=0;   /* Don't zero boundary pixels on last iteration */

      astMapGet0A( keymap, "AST", &kmap );
      astMapGet0I( kmap, "ZERO_NOTLAST", &zero_notlast );
      if( !zero_notlast ) ndfSbb( 255, ondf, status );

      if( kmap ) kmap = astAnnul( kmap );
    }

    /* write units - hack we do not have a smfHead */
    if (strlen(data_units)) ndfCput( data_units, ondf, "UNITS", status);
    ndfCput(data_label, ondf, "LABEL", status);

    /* Weights are related to data_units */
    one_strlcat(data_units, "**-2", sizeof(data_units), status);
    ndfCput(data_units, wndf, "UNITS", status);

    /* Put a separator in the output fits header to make it clear which headers
       have been added by the data processing.
       Wind to end of the fitschan first. */
    astSetI( fchan, "CARD", astGetI( fchan, "NCard" ) + 1 );
    astSetFitsCM( fchan, " ", 0 );
    astSetFitsCM( fchan, "---- Data Processing ----", 0 );

    /* Store the mean step time (overwriting an existing value if required) */
    atlPtftd( fchan, "STEPTIME", meanstep,
              "RTS step time during an RTS sequence", status );

    /* Calculate median exposure time - use faster histogram-based
       method which should be accurate enough for our purposes */
    msgOutif( MSG__VERB, " ", "Calculating median output exposure time",
              status );
    histogram = smf_find_median( NULL, exp_time, nxy, NULL, &medtexp, status );
    if ( medtexp != VAL__BADR ) {
      atlPtftr(fchan, "EXP_TIME", medtexp, "[s] Median MAKEMAP exposure time", status);
    }
    histogram = astFree( histogram );

    /* Store the keywords holding the number of tiles generated and the index
       of the current tile. For the iterative mapmaker these are currently
       always 1 (unless JSATILES is set). */
    if( !jsatiles ) {
       atlPtfti( fchan, "NUMTILES", 1,
                 "No. of tiles covering the field", status );
       atlPtfti( fchan, "TILENUM", 1,
                 "Index of this tile (1->NUMTILES)", status );
    }

    /* Store the number of contiguous chunks of time-series data
       supplied. */
    atlPtfti( fchan, "NCONTIG", (int) ncontig, "No. of contig. chunks "
              "within supplied data ", status );

    /* Store the number of contiguous chunks of time-series data that
       did not converged. */
    atlPtfti( fchan, "NCONTNCV", (int) ncnvg, "No. of chunks that did not "
              "converge", status );

    /* Store a flag indicating if the data was chunked due to lack of
       memory. */
    atlPtftl( fchan, "MEMLOW", memlow, "Was data chunked due to insufficient "
              "memory?", status );

    /* Store the effective bolometer count */
    atlPtftd( fchan, "NBOLOEFF", nboloeff,
              "Effective bolometer count", status );

    /* Store the total exposure time. */
    atlPtftd( fchan, "ELAPTIME", totexp,
              "[s] Total duration of all observations in map", status );

    /* Store the nominal beam FCF. */
    atlPtftd( fchan, "NOMFCF", nomfcf,
              "[Jy/beam/pW] Nominal beam FCF for map", status );

    /* If the FitsChan is not empty, store it in the FITS extension of the
       output NDF (any existing FITS extension is deleted). No need to
       annul the FitsChan as it will be annulled when astEnd is called in
       the monolith function (there should really be an all-encompassing
       astBegin/astEnd block in this function too). Also, the FitsChan is
       needed below by smf_add_spectral_axis. */
    if( astGetI( fchan, "NCard" ) > 0 ) kpgPtfts( ondf, fchan, status );

    /* Before closing the output file, clone the NDF identifier so that
       we can pass it to smf_add_spectral_axis later (smf_close_file
       annuls the NDF identifier from which "ondf" was copied). Do it
       this way, rather than calling smf_add_spectral_axis now, before
       closing the file, to avoid any chance of the changes introduced by
       smf_add_spectral_axis upsetting the behaviour of smf_close_file. */
    ndfClone( ondf, &tndf, status );
    smf_close_file ( wf, &tdata, status );
    smf_close_file ( wf, &wdata, status );
    smf_close_file ( wf, &odata, status );

    /* If required trim any remaining bad borders. Note, this will
       unmap the NDF, but we do not need access to the data arrays
       anymore so that's OK. */
    if( trim ) kpg1Badbx( tndf, 2, &junk, &junk, status );

    /* Convert the output NDF form 2D to 3D by adding a spectral axis
       spanning a single pixel. Then the output NDF identifier. */
    smf_add_spectral_axis( tndf, fchan, status );

    /* If required, split the output map up into JSA tiles. Delete the
       original output NDF afterwards. Always delete the output NDF if
       an error has occurred. */
    if( jsatiles ) {
       parGet0l( "TRIMTILES", &trimtiles, status );
       grpSetsz( igrp4, 0, status );
       smf_jsadicer( tndf, oname, trimtiles, SMF__INST_NONE, SMF__JSA_HPX,
                     &njsatile, igrp4, status );
       ndfDelet( &tndf, status );

    } else if( *status == SAI__OK ){
       grpPut1( igrp4, oname, 0, status );
       ndfAnnul( &tndf, status );

    } else {
       ndfDelet( &tndf, status );
    }

  } else {
    /* no idea what mode */
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( " ", "Map maker mode not understood. Should not be possible",
              status );
    }
  }

  /* Write out the list of output NDF names, annulling the error if a null
     parameter value is supplied. */
  if( *status == SAI__OK && igrp4 ) {
    grpList( "OUTFILES", 0, 0, NULL, igrp4, status );
    if( *status == PAR__NULL ) errAnnul( status );
  }

  /* Write the number of tiles being created to an output parameter,
     unless it was written earlier. */
  if( jsatiles ) parPut0k( "NTILE", njsatile, status );

  /* Arrive here if no output NDF is being created. */
 L998:;
  if( spacerefwcs ) spacerefwcs = astAnnul( spacerefwcs );
  if( outfset != NULL ) outfset = astAnnul( outfset );
  if( igrp != NULL ) {
    smf_pread( igrp, NULL, status );
    grpDelet( &igrp, status);
  }
  if( igrp4 != NULL) grpDelet( &igrp4, status);
  if( ogrp != NULL ) grpDelet( &ogrp, status);
  boxes = astFree( boxes );
  if( tiles ) tiles = smf_freetiles( tiles, (int) ntile, status );
  if( darks ) smf_close_related( wf, &darks, status );
  if( flatramps ) smf_close_related( wf, &flatramps, status );
  if (heateffmap) heateffmap = smf_free_effmap( heateffmap, status );
  if( bbms ) smf_close_related( wf, &bbms, status );
  if( keymap ) keymap = astAnnul( keymap );

  ndfEnd( status );

  if( *status == SAI__OK ) {
    msgOutif(MSG__VERB," ","MAKEMAP succeeded, map written.", status);
  } else {
    msgOutif(MSG__VERB," ","MAKEMAP failed.", status);
  }

}
