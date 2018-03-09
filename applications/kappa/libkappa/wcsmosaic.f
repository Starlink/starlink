      SUBROUTINE WCSMOSAIC( STATUS )
*+
*  Name:
*     WCSMOSAIC

*  Purpose:
*     Tiles a group of NDFs using World Co-ordinate System information.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL WCSMOSAIC( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application aligns and rebins a group of input NDFs into a
*     single output NDF.  It differs from WCSALIGN in both the algorithm
*     used, and in the requirements placed on the input NDFs.  WCSMOSAIC
*     requires that the transformation from pixel to WCS co-ordinates
*     be defined in each input NDF, but (unlike WCSALIGN) the inverse
*     transformation from WCS to pixel co-ordinates need not be defined.
*     For instance, this means that WCSMOSAIC can process data in which
*     the WCS position of each input pixel is defined via a look-up
*     table rather than an analytical expression.  Note however, that
*     the WCS information in the reference NDF (see Parameter REF) must
*     have a defined inverse transformation.
*
*     The WCSMOSAIC algorithm proceeds as follows.  First, the output
*     NDF is filled with zeros.  An associated array of weights (one
*     weight for each output pixel) is created and is also filled with
*     zeros.  Each input NDF is then processed in turn.  For each pixel
*     in the current input NDF, the corresponding transformed position
*     in the output NDF is found (based on the WCS information in both
*     NDFs).  The input pixel value is then divided up between a small
*     group of output pixels centred on this central output position.
*     The method used for choosing the fraction of the input pixel
*     value assigned to each output pixel is determined by the METHOD
*     and PARAMS parameters.  Each of the affected output pixel values
*     is then incremented by its allocated fraction of the input pixel
*     value.  The corresponding weight values are incremented by the
*     fractions used (that is, if 0.25 of an input pixel is assigned to
*     an output pixel, the weight for the output pixel is incremented by
*     0.25).  Once all pixels in the current input NDF have been
*     rebinned into the output NDF in this way, the algorithm proceeds
*     to rebin the next input NDF in the same way.  Once all input NDFs
*     have been processed, output pixels which have a weight less than
*     the value given by Parameter WLIM are set bad.  The output NDF may
*     then optionally (see Parameter NORM) be normalised by dividing it
*     by the weights array.  This normalisation of the output NDF takes
*     account of any difference in the number of pixels contributing to
*     each output pixel, and also removes artefacts which may be
*     produced by aliasing between the input and output pixel grids.
*     Thus each output pixel value is a weighted mean of the input pixel
*     values from which it receives contributions.  This means that the
*     units of the output NDF are the same as the input NDF.  In
*     particular, any difference between the input and output pixel
*     sizes is ignored, resulting in the total input data sum being
*     preserved only if the input and output NDFs have equal pixel\
*     sizes.  However, an option exists to scale the input values before
*     use so that the total data sum in each input NDF is preserved even
*     if the input and output pixel sizes differ (see Parameter
*     CONSERVE).
*
*     If the input NDFs contain variances, then these are propagated to
*     the output.  Alternatively, output variances can be generated from
*     the spread of input values contributing to each output pixel (see
*     Parameter GENVAR). Any input variances can also be used to weight
*     the input data (see Parameter VARIANCE).  By default, all input
*     data is given equal weight. An additional weight for each NDF can be
*     specified using parameter WEIGHTS.
*
*     The transformations needed to produce alignment are derived from
*     the co-ordinate system information stored in the WCS components of
*     the supplied NDFs.  For each input NDF, alignment is first
*     attempted in the current co-ordinate Frame of the reference NDF.
*     If this fails, alignment is attempted in the current co-ordinate
*     Frame of the input NDF.  If this fails, alignment occurs in the
*     pixel co-ordinate Frame.  A message indicating which Frame
*     alignment was achieved in is displayed.

*  Usage:
*     wcsmosaic in out lbnd ubnd ref

*  ADAM Parameters:
*     ACC = _REAL (Read)
*        The positional accuracy required, as a number of pixels.  For
*        highly non-linear projections, a recursive algorithm is used in
*        which successively smaller regions of the projection are
*        fitted with a least-squares linear transformation.  If such a
*        transformation results in a maximum positional error greater
*        than the value supplied for ACC (in pixels), then a smaller
*        region is used.  High accuracy is paid for by longer run times.
*        [0.05]
*     ALIGNREF = _LOGICAL (Read)
*        Determines the coordinate system in which each input NDF is
*        aligned with the reference NDF. If TRUE, alignment is performed
*        in the coordinate system described by the current Frame of the WCS
*        FrameSet in the reference NDF. If FALSE, alignment is performed
*        in the coordinate system specified by the following set of WCS
*        attributes in the reference NDF: AlignSystem AlignStdOfRest,
*        AlignOffset, AlignSpecOffset, AlignSideBand, AlignTimeScale. The
*        AST library provides fixed defaults for all these. So for
*        instance, AlignSystem defaults to ICRS for celestial axes and
*        Wavelength for spectral axes, meaning that celestial axes will
*        be aligned in ICRS and spectral axes in wavelength, by default.
*        Similarly, AlignStdOfRest defaults to Heliocentric, meaning that
*        by default spectral axes will be aligned in the Heliocentric rest
*        frame.
*
*        As an example, if you are mosaicing two spectra which both use
*        radio velocity as the current WCS, but which have different rest
*        frequencies, then setting ALIGNREF to TRUE will cause alignment
*        to be performed in radio velocity, meaning that the differences
*        in rest frequency are ignored. That is, a channel with 10 Km/s
*        in the input is mapping onto the channel with 10 km/s in the output.
*        If ALIGNREF is FALSE (and no value has been set for the AlignSystem
*        attribute in the reference WCS), then alignment will be performed
*        in wavelength, meaning that the different rest frequencies cause
*        an additional shift. That is, a channel with 10 Km/s in the input
*        will be mapping onto which ever output channel has the same
*        wavelength, taking into account the different rest frequencies.
*
*        As another example, consider mosaicing two maps which both have
*        (azimuth,elevation) axes. If ALIGNREF is TRUE, then any given
*        (az,el) values in one image will be mapped onto the exact same
*        (az,el) values in the other image, regardless of whether the
*        two images were taken at the same time. But if ALIGNREF is FALSE,
*        then a given (az,el) value in one image will be mapped onto
*        pixel that has the same ICRS coordinates in the other image
*        (since AlignSystem default to ICRS for celestial axes). Thus any
*        different in the observation time of the two images will result
*        in an additional shift.
*
*        As yet another example, consider mosaicing two spectra which are
*        both in frequency with respect to the LSRK, but which refer to
*        different points on the sky. If ALIGNREF is TRUE, then a given
*        LSRK frequency in one spectra will be mapped onto the exact same
*        LSRK frequency in the other image, regardless of the different sky
*        positions. But if ALIGNREF is FALSE, then a given input frequency
*        will first be converted to Heliocentric frequency (the default
*        value for AlignStdOfRest is "Heliocentric"), and will be mapped
*        onto the output channel that has the same Heliocentric frequency.
*        Thus the differecen in sky positions will result in an additional
*        shift.   [FALSE]
*     CONSERVE = _LOGICAL (Read)
*        If set TRUE, then the output pixel values will be scaled in
*        such a way as to preserve the total data value in a feature on
*        the sky.  The scaling factor is the ratio of the output pixel
*        size to the input pixel size.  This option can only be used if
*        the Mapping is successfully approximated by one or more linear
*        transformations.  Thus an error will be reported if it used
*        when the ACC parameter is set to zero (which stops the use of
*        linear approximations), or if the Mapping is too non-linear to
*        be approximated by a piece-wise linear transformation.  The
*        ratio of output to input pixel size is evaluated once for each
*        panel of the piece-wise linear approximation to the Mapping,
*        and is assumed to be constant for all output pixels in the
*        panel.  This parameter is ignored if the NORM parameter is set
*        FALSE.  [TRUE]
*     FLBND( ) = _DOUBLE (Write)
*        The lower bounds of the bounding box enclosing the output NDF
*        in the current WCS Frame.  The number of elements in this
*        parameter is equal to the number of axes in the current WCS
*        Frame.  Celestial axis values will be in units of radians.
*     FUBND( ) = _DOUBLE (Write)
*        The upper bounds of the bounding box enclosing the output NDF
*        in the current WCS Frame.  The number of elements in this
*        parameter is equal to the number of axes in the current WCS
*        Frame.  Celestial axis values will be in units of radians.
*     GENVAR = _LOGICAL (Read)
*        If TRUE, output variances are generated based on the spread of
*        input pixel values contributing to each output pixel.  Any
*        input variances then have no effect on the output variances
*        (although input variances will still be used to weight the
*        input data if the VARIANCE parameter is set TRUE).  If GENVAR
*        is set FALSE, the output variances are based on the variances
*        in the input NDFs, so long as all input NDFs contain variances
*        (otherwise the output NDF will not contain any Variances). If a
*        null (!) value is supplied, then a value of FALSE is adopted if
*        and only if all the input NDFs have variance components (TRUE
*        is used otherwise).  [FALSE]
*     IN = NDF (Read)
*        A group of input NDFs (of any dimensionality).  This should be
*        given as a comma-separated list, in which each list element
*        can be one of the following options.
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
*        If the value supplied for this parameter ends with a hyphen,
*        then you are re-prompted for further input until a value is
*        given which does not end with a hyphen.  All the NDFs given in
*        this way are concatenated into a single group.
*     LBND() = _INTEGER (Read)
*        An array of values giving the lower pixel-index bound on each
*        axis for the output NDF.  The suggested default values just
*        encompass all the input data.  A null value (!) also results in
*        these same defaults being used.  [!]
*     LBOUND() = _INTEGER (Write)
*        The lower pixel bounds of the output NDF. Note, values will be
*        written to this output parameter even if a null value is
*        supplied for Parameter OUT.
*     MAXPIX = _INTEGER (Read)
*        A value which specifies an initial scale size in pixels for the
*        adaptive algorithm which approximates non-linear Mappings with
*        piece-wise linear transformations.  If MAXPIX is larger than
*        any dimension of the region of the output grid being used, a
*        first attempt will be made to approximate the Mapping by a
*        linear transformation over the entire output region.  If a
*        smaller value is used, the output region will first be divided
*        into subregions whose size does not exceed MAXPIX pixels in any
*        dimension, and then attempts will be made at approximation.
*        [1000]
*     METHOD = LITERAL (Read)
*        The method to use when dividing an input pixel value between a
*        group of neighbouring output pixels.  For details on these
*        schemes, see the description of AST_REBINx in SUN/210.  METHOD
*        can take the following values.
*
*        - "Bilinear" -- The input pixel value is divided bi-linearly
*        between  the four nearest output pixels.  This produces
*        smoother output NDFs than the nearest-neighbour scheme, but is
*        marginally slower.
*
*        - "Nearest" -- The input pixel value is assigned completely to
*        the single nearest output pixel.
*
*        - "Sinc" -- Uses the sinc(pi*x) kernel, where x is the pixel
*        offset from the transformed input pixel centre, and
*        sinc(z)=sin(z)/z.  Use of this scheme is not recommended.
*
*        - "SincSinc" -- Uses the sinc(pi*x)sinc(k*pi*x) kernel.  This
*        is a valuable general-purpose scheme, intermediate in its
*        visual effect on NDFs between the bilinear and
*        nearest-neighbour schemes.
*
*        - "SincCos" -- Uses the sinc(pi*x)cos(k*pi*x) kernel.  It gives
*        similar results to the "Sincsinc" scheme.
*
*        - "SincGauss" -- Uses the sinc(pi*x)exp(-k*x*x) kernel.  Good
*        results can be obtained by matching the FWHM of the
*        envelope function to the point-spread function of the
*        input data (see Parameter PARAMS).
*
*        - "Somb" -- Uses the somb(pi*x) kernel, where
*        somb(z)=2*J1(z)/z  (J1 is the first-order Bessel function of
*        the first kind).  This scheme is similar to the "Sinc" scheme.
*
*        - "SombCos" -- Uses the somb(pi*x)cos(k*pi*x) kernel.  This
*        scheme is similar to the "SincCos" scheme.
*
*        - "Gauss" -- Uses the exp(-k*x*x) kernel.  The FWHM of the
*        Gaussian is given by Parameter PARAMS(2), and the point at
*        which to truncate the Gaussian to zero is given by Parameter
*        PARAMS(1).
*
*        All methods propagate variances from input to output, but the
*        variance estimates produced by schemes other than
*        nearest neighbour need to be treated with care since the
*        spatial smoothing produced by these methods introduces
*        correlations in the variance estimates.  Also, the degree of
*        smoothing produced varies across the NDF.  This is because a
*        sample taken at a pixel centre will have no contributions from
*        the neighbouring pixels, whereas a sample taken at the corner
*        of a pixel will have equal contributions from all four
*        neighbouring pixels, resulting in greater smoothing and lower
*        noise.  This effect can produce complex Moire patterns in the
*        output variance estimates, resulting from the interference of
*        the spatial frequencies in the sample positions and in the
*        pixel-centre positions.  For these reasons, if you want to use
*        the output variances, you are generally safer using
*        nearest-neighbour interpolation.  The initial default is
*        "SincSinc".  [current value]
*     NORM = _LOGICAL (Read)
*        In general, each output pixel contains contributions from
*        multiple input pixel values, and the number of input pixels
*        contributing to each output pixel will vary from pixel to
*        pixel.  If NORM is set TRUE (the default), then each output
*        value is normalised by dividing it by the number of
*        contributing input pixels, resulting in each output value being
*        the weighted mean of the contibuting input values.  However, if
*        NORM is set FALSE, this normalisation is not applied.  See also
*        Parameter CONSERVE.  Setting NORM to FALSE and VARIANCE to TRUE
*        results in an error being reported.  [TRUE]
*     OUT = NDF (Write)
*        The output NDF.  If a null (!) value is supplied, WCSMOSAIC
*        will terminate early without creating an output cube, but
*        without reporting an error. Note, the pixel bounds which the
*        output cube would have had will still be written to output
*        Parameters LBOUND and UBOUND, even if a null value is supplied
*        for OUT.
*     PARAMS( 2 ) = _DOUBLE (Read)
*        An optional array which consists of additional parameters
*        required by the Sinc, SincSinc, SincCos, SincGauss, Somb,
*        SombCos and Gauss methods.
*
*        PARAMS( 1 ) is required by all the above schemes.  It is used
*        to specify how many output pixels on either side of the central
*        output pixel are to receive contribution from the corresponding
*        input pixel.  Typically, a value of 2 is appropriate and the
*        minimum allowed value is 1 (i.e. one pixel on each side).  A
*        value of zero or fewer indicates that a suitable number of
*        pixels should be calculated  automatically.  [0]
*
*        PARAMS( 2 ) is required only by the Gauss, SombCos, SincSinc,
*        SincCos, and SincGauss schemes. For the SombCos, SincSinc and
*        SincCos schemes, it specifies the number of output pixels at
*        which the envelope of the function goes to zero.  The minimum
*        value is 1.0, and the run-time default value is 2.0.  For the
*        Gauss and SincGauss scheme, it specifies the full-width at
*        half-maximum (FWHM) of the Gaussian envelope measured in
*        output pixels.  The minimum value is 0.1, and the run-time
*        default is 1.0.  []
*     REF = NDF (Read)
*        The NDF to which all the input NDFs are to be aligned.  If a
*        null value is supplied for this parameter, the first NDF
*        supplied for Parameter IN is used.  The WCS information in this
*        NDF must have a defined inverse transformation (from WCS
*        co-ordinates to pixel co-ordinates).  [!]
*     UBND() = _INTEGER (Read)
*        An array of values giving the upper pixel-index bound on each
*        axis for the output NDF.  The suggested default values just
*        encompass all the input data.  A null value (!) also results in
*        these same defaults being used.  [!]
*     UBOUND() = _INTEGER (Write)
*        The upper pixel bounds of the output NDF. Note, values will be
*        written to this output parameter even if a null value is
*        supplied for Parameter OUT.
*     VARIANCE = _LOGICAL (Read)
*        If TRUE, then any input VARIANCE components in the input NDFs
*        are used to weight the input data (the weight used for each
*        data value is the reciprocal of the variance).  If FALSE, all
*        input data is given equal weight.  Note, some applications
*        (such as CCDPACK:MAKEMOS) use a parameter named USEVAR to
*        determine both whether input variances are used to weights
*        input data values, and also how to calculate output variances.
*        However, WCSMOSAIC uses the VARIANCE parameter only for the
*        first of these purposes (determining whether to weight the
*        input data).  The second purpose (determining how to create
*        output variances) is fulfilled by the GENVAR parameter. [FALSE]
*     WEIGHTS = LITERAL (Read)
*        An optional group of numerical weights, one for each of the input
*        NDFs specified by parameter IN. If VARIANCE is TRUE, the weight
*        assigned to each input pixel is the value supplied in this group
*        correspoinding to the appropriate input NDF, divided by the variance
*        of the pixel value. An error is reported if the number of supplied
*        weights does not equal the number of supplied input NDFs. [!]
*     WLIM = _REAL (Read)
*        This parameter specifies the minimum number of good pixels
*        that must contribute to an output pixel for the output pixel
*        to be valid.  Note, fractional values are allowed.  If a value
*        less than 1.0E-10 is supplied, a value of 1.0E-10 is used.
*        [1.0E-10]

*  Examples:
*     wcsmosaic m51* mosaic lbnd=! accept
*        This example rebins all the NDFs with names starting with
*        the string "m51" in the current directory so that they are
*        aligned with the first input NDF, and combines them all into a
*        single output NDF called mosaic.  The output NDF is just big
*        enough to contain all the pixels in all the input NDFs.

*  Notes:
*     -  WCS information (including the current co-ordinate Frame) is
*     propagated from the reference NDF to the output NDF. All other
*     information is propagated form the first input NDF.
*     -  The QUALITY and AXIS components are not propagated from input
*     to output.
*     -  There are different facts reported, their verbosity depending
*     on the current message-reporting level set by environment variable
*     MSG_FILTER.  If this is set to QUIET, no information will be
*     displayed while the command is executing.  When the filtering
*     level is at least as verbose as NORMAL, the interpolation method
*     being used will be displayed.  If set to VERBOSE, the name of each
*     input NDF will also be displayed as it is processed.

*  Related Applications:
*     KAPPA: WCSFRAME, WCSALIGN, REGRID; CCDPACK: TRANNDF.

*  Implementation Status:
*     -  This routine correctly processes the DATA, VARIANCE, LABEL,
*     TITLE, UNITS, WCS, and HISTORY components of the input NDFs (see
*     the METHOD parameter for notes on the interpretation of output
*     variances).
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled, but the data
*     type will be converted to one of _INTEGER, _DOUBLE or _REAL for
*     processing.

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics & Astronomy Research
*     Council.
*     Copyright (C) 2007-2009 Science & Technology Facilities Council.
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
*     {enter_new_authors_here}

*  History:
*     14-SEP-2005 (DSB):
*        Original version, based on WCSALIGN.
*     30-MAR-2006 (DSB):
*        Added ILEVEL.
*     2006 April 12 (MJC):
*        Remove unused variables.
*     8-JAN-2007 (DSB):
*        Modified the prologue to clarify the need for an inverse
*        WCS transformation in the reference NDF.
*     8-FEB-2007 (DSB):
*        Added parameters LBOUND and UBOUND.
*     13-FEB-2007 (DSB):
*        Remove C-like semi-colon line terminators.
*     13-MAR-2007 (DSB):
*        Add Parameter VARWGT.
*     29-MAR-2007 (DSB):
*        Renamed Parameter VARWGT as USEVAR. Internally, the flag is
*        still refered to as varwgt for consistency with AST (which has
*        both AST__USEVAR and AST__VARWGT flags).
*     2007 April 4 (MJC):
*        Renamed USEVAR to the KAPPA standard of VARIANCE.
*     4-MAY-2007 (DSB):
*        Add new argument NUSED to calls to AST_REBINSEQ.
*     19-JUN-2007 (DSB):
*        New output Parameters FLBND and FUBND.
*     4-FEB-2008 (DSB):
*        Allow a null value to be supplied for Parameter GENVAR.
*     23-JUN-2008 (DSB):
*        Propagate from the first input NDF rather than the reference
*        NDF.
*     2009 July 22 (MJC):
*        Remove ILEVEL parameter and use the current reporting level
*        instead (set by the global MSG_FILTER environment variable).
*     30-AUG-2012 (DSB):
*        Added Parameters CONSERVE and NORM.
*     1-DEC-2014 (DSB):
*        Added parameter ALIGNREF.
*     9-MAR-2018 (DSB):
*        Added parameter WEIGHTS.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE          ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'      ! Standard SAE constants
      INCLUDE 'PRM_PAR'      ! VAL__ constants
      INCLUDE 'PAR_ERR'      ! PAR error constants
      INCLUDE 'GRP_PAR'      ! GRP constants
      INCLUDE 'NDF_PAR'      ! NDF constants
      INCLUDE 'CNF_PAR'      ! CNF constants
      INCLUDE 'AST_PAR'      ! AST constants
      INCLUDE 'MSG_PAR'      ! Message-system constants

*  Status:
      INTEGER STATUS         ! Global status

*  Local Variables:
      CHARACTER DTYPE*(NDF__SZFTP) ! Data type
      CHARACTER MESS*60      ! Message text
      CHARACTER METHOD*13    ! Interpolation method to use.
      CHARACTER TEXT*(GRP__SZNAM)! Text of current element
      CHARACTER TY_IN*(NDF__SZTYP) ! Numeric type for processing
      DOUBLE PRECISION FLBND( NDF__MXDIM ) ! Lower WCS bounds of output
      DOUBLE PRECISION FUBND( NDF__MXDIM ) ! Upper WCS bounds of output
      DOUBLE PRECISION GLBND( NDF__MXDIM ) ! Lower GRID bounds of output
      DOUBLE PRECISION GUBND( NDF__MXDIM ) ! Upper GRID bounds of output
      DOUBLE PRECISION PARAMS( 3 )! Param values passed to AST_RESAMPLE
      DOUBLE PRECISION XL( NDF__MXDIM ) ! GRID position at lower limit
      DOUBLE PRECISION XU( NDF__MXDIM ) ! GRID position at upper limit
      INTEGER DLBND( NDF__MXDIM )! Defaults for LBND
      INTEGER DUBND( NDF__MXDIM )! Defaults for UBND
      INTEGER EL             ! Number of array elements mapped
      INTEGER FLAGS          ! Flags for AST_REBINSEQ
      INTEGER I              ! Index into input and output groups
      INTEGER IGRP1          ! GRP id. for group holding input NDFs
      INTEGER IGRP2          ! GRP id. for group holding input weights
      INTEGER INDF0          ! NDF id. for the first input NDF
      INTEGER INDF1          ! NDF id. for the input NDF
      INTEGER INDF2          ! NDF id. for the output NDF
      INTEGER INDFR          ! NDF id. for the reference NDF
      INTEGER IPD1           ! Pntr. to input data array
      INTEGER IPD2           ! Pntr. to output data array
      INTEGER IPIX2          ! Index of PIXEL Frame in o/p FrameSet
      INTEGER IPIXR          ! Index of PIXEL Frame in ref. FrameSet
      INTEGER IPMAP          ! Pntr. to array of pix_in->pix_out Mappings
      INTEGER IPV1           ! Pntr. to input variance array
      INTEGER IPV2           ! Pntr. to output variance array
      INTEGER IPW            ! Pntr. to work array
      INTEGER ISTAT          ! Local status value
      INTEGER IWCS2          ! Original output WCS FrameSet
      INTEGER IWCSR          ! WCS FrameSet for reference NDF
      INTEGER IWCSR2         ! New output WCS FrameSet
      INTEGER LBND( NDF__MXDIM ) ! Indices of lower-left corner of o/p
      INTEGER LBND1( NDF__MXDIM )! Indices of lower-left corner of input
      INTEGER MAP            ! AST id for (pix_in->pix_out) Mapping
      INTEGER MAP2           ! Mapping from PIXEL to output GRID Frame
      INTEGER MAP3           ! AST Mapping (ref. GRID -> o/p GRID)
      INTEGER MAPR           ! AST Mapping (ref. GRID -> ref. PIXEL)
      INTEGER MAXPIX         ! Initial scale size in pixels
      INTEGER METHOD_CODE    ! Integer identifier for spreading method
      INTEGER NAX            ! No. of axes in reference WCS Frame
      INTEGER NDIM           ! Number of pixel axes in output NDF
      INTEGER NDIM1          ! Number of pixel axes in input NDF
      INTEGER NPAR           ! No. of required interpolation parameters
      INTEGER SIZE           ! Total size of the input group
      INTEGER UBND( NDF__MXDIM ) ! Indices of upper-right corner of o/p
      INTEGER UBND1( NDF__MXDIM )! Indices of upper-right corner of i/p
      INTEGER WSIZE          ! Number of NDF weights given
      INTEGER*8 NUSED        ! No. of input values used so far
      LOGICAL BAD_DV         ! Any bad data/variance values in input?
      LOGICAL CONSRV         ! Conserve flux in each input NDF?
      LOGICAL GENVAR         ! Use i/p spread to create o/p variance?
      LOGICAL HASVAR         ! Do all i/p NDFs have variances?
      LOGICAL NDFWGT         ! Has a weight been supplied for each NDF?
      LOGICAL NORM           ! Normalise the o/p values?
      LOGICAL REFALN         ! Use ref. to define alignment properties?
      LOGICAL USEVAR         ! Use i/p variances to create o/p variance?
      LOGICAL VARWGT         ! Use i/p variances as weights?
      REAL ERRLIM            ! Positional accuracy in pixels
      REAL WLIM              ! Minimum good output weight
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get a group containing the names of the NDFs to be processed.
      CALL KPG1_RGNDF( 'IN', 0, 1, '  Give more NDFs...',
     :                 IGRP1, SIZE, STATUS )

*  Get an identifier for the first input NDF.
      CALL NDG_NDFAS( IGRP1, 1, 'READ', INDF0, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the reference NDF.
      CALL LPG_ASSOC( 'REF', 'READ', INDFR, STATUS )

*  If a null value was supplied, annul the error and use the first NDF
*  supplied for IN.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         INDFR = INDF0
      END IF

*  Get the number of pixel axes in the reference NDF.
      CALL NDF_BOUND( INDFR, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  See if the reference NDF is to be used to define the cordinate system
*  in which alignment will occur.
      CALL PAR_GET0L( 'ALIGNREF', REFALN, STATUS )

*  Extract required global information describing the group of input
*  NDF.  This includes the default values for LBND and UBND, and the
*  Mappings from the input PIXEL Frames to the output PIXEL Frame.
      CALL PSX_CALLOC( SIZE, '_INTEGER', IPMAP, STATUS )
      CALL KPS1_WMOS0( INDFR, IGRP1, NDIM, REFALN, DLBND, DUBND,
     :                 HASVAR, %VAL( CNF_PVAL( IPMAP ) ), IWCSR,
     :                 STATUS )

*  Set the default bounds for the output NDF.
      CALL PAR_DEF1I( 'LBND', NDIM, DLBND, STATUS )
      CALL PAR_DEF1I( 'UBND', NDIM, DUBND, STATUS )

*  Get the bounds required for the output NDF.  Use the above defaults
*  if a null value is supplied.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the bounds required for the output NDF.
      CALL PAR_EXACI( 'LBND', NDIM, LBND, STATUS )
      CALL PAR_EXACI( 'UBND', NDIM, UBND, STATUS )

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         DO I = 1, NDIM
            LBND( I ) = DLBND( I )
            UBND( I ) = DUBND( I )
         END DO
      END IF

*  Write the output pixel bounds to output Parameters LBOUND and UBOUND.
      CALL PAR_PUT1I( 'LBOUND', NDIM, LBND, STATUS )
      CALL PAR_PUT1I( 'UBOUND', NDIM, UBND, STATUS )

*  Convert these pixel index bounds to WCS bounds, and write them out to
*  the FLBND and FUBND output parameters.
      DO I = 1, NDIM
         GLBND( I ) = 0.5D0
         GUBND( I ) = DBLE( UBND( I ) - LBND( I ) ) + 1.5D0
      END DO

      NAX = AST_GETI( IWCSR, 'Naxes', STATUS )
      DO I = 1, NAX
         CALL AST_MAPBOX( IWCSR, GLBND, GUBND, .TRUE., I, FLBND( I ),
     :                    FUBND( I ), XL, XU, STATUS )
      END DO

      CALL AST_NORM( IWCSR, FLBND, STATUS )
      CALL AST_NORM( IWCSR, FUBND, STATUS )
      CALL PAR_PUT1D( 'FLBND', NAX, FLBND, STATUS )
      CALL PAR_PUT1D( 'FUBND', NAX, FUBND, STATUS )

*  Get the pixel spreading method to be used.
      CALL PAR_CHOIC( 'METHOD', 'SincSinc', 'Nearest,Bilinear,'//
     :                'Sinc,Gauss,SincSinc,SincCos,SincGauss,'//
     :                'Somb,SombCos', .TRUE., METHOD, STATUS )

*  Tell the user what method is being used, and convert value of
*  METHOD to one of the values expected by AST_REBINSEQ<x>.
      NPAR = 0
      IF( METHOD( 1 : 1 ) .EQ. 'N' ) THEN
         METHOD_CODE = AST__NEAREST
         MESS = '  Using nearest neighbour binning.'

      ELSE IF( METHOD( 1 : 2 ) .EQ. 'BI' ) THEN
         METHOD_CODE = AST__LINEAR
         MESS = '  Using bi-linear binning.'

      ELSE IF( METHOD( 1 : 1 ) .EQ. 'G' ) THEN
         NPAR = 2
         PARAMS( 1 ) = 0.0
         PARAMS( 2 ) = 2.0
         METHOD_CODE = AST__GAUSS
         MESS = '  Using a Gaussian binning kernel.'

      ELSE IF ( METHOD( 1 : 4 ) .EQ. 'SINC' ) THEN
         NPAR = 2
         PARAMS( 1 ) = 0.0
         PARAMS( 2 ) = 2.0

         IF ( METHOD( 5 : 5 ) .EQ. 'S' ) THEN
            METHOD_CODE = AST__SINCSINC
            MESS = '  Using sincsinc binning kernel.'

         ELSE IF( METHOD( 5 : 5 ) .EQ. 'C' ) THEN
            METHOD_CODE = AST__SINCCOS
            MESS = '  Using sinccos binning kernel.'

         ELSE IF( METHOD( 5 : 5 ) .EQ. 'G' ) THEN
            METHOD_CODE = AST__SINCGAUSS
            PARAMS( 2 ) = 1.0
            MESS = '  Using sincgauss binning kernel.'

         ELSE
            NPAR = 1
            METHOD_CODE = AST__SINC
            MESS = '  Using sinc binning kernel.'

         END IF

      ELSE IF ( METHOD( 1 : 4 ) .EQ. 'SOMB' ) THEN
         NPAR = 2
         PARAMS( 1 ) = 0.0
         PARAMS( 2 ) = 2.0

         IF( METHOD( 5 : 5 ) .EQ. 'C' ) THEN
            METHOD_CODE = AST__SOMBCOS
            MESS = '  Using sombcos binning kernel.'

         ELSE
            NPAR = 1
            METHOD_CODE = AST__SOMB
            MESS = '  Using somb binning kernel.'

         END IF

      END IF

      CALL MSG_OUTIF( MSG__NORM, 'WCSMOSAIC_MSG1', MESS, STATUS )

*  If required, set the dynamic defaults for PARAMS, then get new
*  values.
      IF( NPAR .GT. 0 ) THEN
         CALL PAR_DEF1D( 'PARAMS', NPAR, PARAMS, STATUS )
         CALL PAR_EXACD( 'PARAMS', NPAR, PARAMS, STATUS )
      END IF

*  Get the positional accuracy required.
      CALL PAR_GET0R( 'ACC', ERRLIM, STATUS )
      ERRLIM = MAX( 0.0001, ERRLIM )

*  Get the minimum acceptable output weight.
      CALL PAR_GET0R( 'WLIM', WLIM, STATUS )

*  Get a value for MAXPIX.
      CALL PAR_GET0I( 'MAXPIX', MAXPIX, STATUS )
      MAXPIX = MAX( 1, MAXPIX )

*  See if the normalisation of the output values is to be skipped.
      CALL PAR_GET0L( 'NORM', NORM, STATUS )

*  If not, see if total flux in each input NDF is to be preserved.
      IF( NORM ) THEN
         CALL PAR_GET0L( 'CONSERVE', CONSRV, STATUS )
      ELSE
         CONSRV = .FALSE.
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  See if output variances are to be generated on the basis of the
*  spread of input data values.
      CALL PAR_GET0L( 'GENVAR', GENVAR, STATUS )

*  If a null value was supplied, annul the error and use FALSE if all the
*  input NDFs have a variance component, and TRUE otherwise.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GENVAR = .NOT. HASVAR
      END IF

*  If we are creating output variances from the spread of input data
*  values, then we do not use the input variances to calculate the output
*  variances.
      IF( GENVAR ) THEN
         USEVAR = .FALSE.

*  If output variances are not being created on the basis of the spread
*  in input values, then thet are created on the basis of input variances,
*  if all input NDF have defined Variance components.
      ELSE
         USEVAR = HASVAR
      END IF

*  If all input have variance components, see if input variances are to
*  be used as weights.
      IF( HASVAR ) THEN
         CALL PAR_GET0L( 'VARIANCE', VARWGT, STATUS )
      ELSE
         VARWGT = .FALSE.
      END IF

*  Get any per-NDF weights.
      NDFWGT = .FALSE.
      IGRP2 = GRP__NOID
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL KPG1_GTGRP( 'WEIGHTS', IGRP2, WSIZE, STATUS )

         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

         ELSE IF( WSIZE .NE. SIZE .AND. STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETI( 'N', SIZE )
            CALL MSG_SETI( 'W', WSIZE )
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'No. of values supplied for parameter '//
     :                    'WEIGHTS (^W) is not the same as the number'//
     :                    ' of input NDFs (^N).', STATUS )

         ELSE
            NDFWGT = .TRUE.

*  Shuffle the values in the PARAMS array down one element to leave room
*  for the NDF weight in the first element.
            PARAMS( 3 ) = PARAMS( 2 )
            PARAMS( 2 ) = PARAMS( 1 )

         END IF
      END IF

*  Cannot skip the normalisation if we are weighting input data.
      IF( VARWGT .AND. .NOT. NORM .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'The NORM parameter is FALSE while '/
     :                 /'the VARIANCE parameter is TRUE; this '/
     :                 /'is not allowed.', STATUS )
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Create the output NDF by propagation from the first input NDF.  The
*  default components HISTORY, TITLE, LABEL and all extensions are
*  propagated, together with the UNITS component.  The NDF is
*  initially created with the same bounds as the first input NDF.
      CALL NDF_PROP( INDF0, 'UNITS', 'OUT', INDF2, STATUS )

*  If a null value was supplied for OUT, annul the error and abort.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GO TO 999
      END IF

*  Change the bounds of the output NDF to the required values.
      CALL NDF_SBND( NDIM, LBND, UBND, INDF2, STATUS )

*  We now create the WCS FrameSet for the output NDF.  This will be a
*  copy of the reference FrameSet, modified to take account of any
*  difference in the pixel origins between the reference and output
*  NDFs.  We do this by taking a copy of the reference WCS FrameSet and
*  then re-mapping the GRID Frame in the copy.  The Mapping used is the
*  mapping from reference GRID Frame to output GRID Frame, going via the
*  common PIXEL Frame.  Get the default WCS FrameSet for the output NDF.
      CALL NDF_GTWCS( INDF2, IWCS2, STATUS )

*  Find the PIXEL Frame.
      CALL KPG1_ASFFR( IWCS2, 'PIXEL', IPIX2, STATUS )

*  Get the Mapping from the PIXEL Frame to the output GRID Frame.
      MAP2 = AST_GETMAPPING( IWCS2, IPIX2, AST__BASE, STATUS )

*  Take a copy of the reference FrameSet.
      IWCSR2 = AST_COPY( IWCSR, STATUS )

*  Find the PIXEL Frame.
      CALL KPG1_ASFFR( IWCSR2, 'PIXEL', IPIXR, STATUS )

*  Get the Mapping from the reference GRID Frame to the PIXEL Frame.
      MAPR = AST_GETMAPPING( IWCSR2, AST__BASE, IPIXR, STATUS )

*  Concatenate and simplify MAPR and MAP2 to get the Mapping from
*  reference GRID Frame to output GRID Frame.
      MAP3 = AST_SIMPLIFY( AST_CMPMAP( MAPR, MAP2, .TRUE., ' ',
     :                                 STATUS ), STATUS )

*  Re-map the GRID Frame in the copy of the reference WCS FrameSet so
*  that it corresponds to the GRID Frame in the output NDF.
      CALL AST_REMAPFRAME( IWCSR2, AST__BASE, MAP3, STATUS )

*  Store this FrameSet in the output NDF.
      CALL NDF_PTWCS( IWCSR2, INDF2, STATUS )

*  Choose the data type to use.
      CALL NDF_MTYPE( '_INTEGER,_REAL,_DOUBLE', INDF2, INDF2, 'DATA',
     :                TY_IN, DTYPE, STATUS )

*  Map the output Data array.
      CALL NDF_MAP( INDF2, 'DATA', TY_IN, 'WRITE', IPD2, EL, STATUS )

*  If an output Variance component is to be created, map it, else assign
*  a value of the corresponding DATA component (safe value).
      IF( USEVAR .OR. GENVAR ) THEN
         CALL NDF_MAP( INDF2, 'VAR', TY_IN, 'WRITE', IPV2, EL,
     :                 STATUS )
      ELSE
         IPV2 = IPD2
      END IF

*  Create a work array to hold the weight for each output pixel.
      IF( GENVAR ) THEN
         CALL PSX_CALLOC( 2*EL, '_DOUBLE', IPW, STATUS )
      ELSE
         CALL PSX_CALLOC( EL, '_DOUBLE', IPW, STATUS )
      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Initialise to zero the number of input values pasted into the
*  output array.
      NUSED = 0

*  Loop round each NDF to be processed.
      DO I = 1, SIZE

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRP1, I, 'Read', INDF1, STATUS )

*  If required, tell the user which input NDF is currently being
*  processed.
         CALL MSG_BLANKIF( MSG__VERB, STATUS )
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL MSG_OUTIF( MSG__VERB, 'WCSMOSAIC_MSG2',
     :                   '  Processing ^NDF...', STATUS )

*  Set the AST_REBINSEQ flags for this input.
         FLAGS = 0
         IF( I .EQ. 1 ) FLAGS = FLAGS + AST__REBININIT
         IF( I .EQ. SIZE ) FLAGS = FLAGS + AST__REBINEND
         IF( CONSRV ) FLAGS = FLAGS + AST__CONSERVEFLUX
         IF( .NOT. NORM ) FLAGS = FLAGS + AST__NONORM
         IF( GENVAR ) FLAGS = FLAGS + AST__GENVAR
         IF( USEVAR ) FLAGS = FLAGS + AST__USEVAR
         IF( VARWGT ) FLAGS = FLAGS + AST__VARWGT
         IF( NDFWGT ) THEN
            FLAGS = FLAGS + AST__PARWGT

*  Copy the NDF's initial weight to the first element of the PARAMS array,
*  checking it for validity.
            CALL GRP_GET( IGRP2, I, 1, TEXT, STATUS )
            ISTAT = SAI__OK
            CALL CHR_CTOD( TEXT, PARAMS( 1 ), ISTAT )
            IF( ISTAT .NE. SAI__OK .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = ISTAT
               CALL MSG_SETC( 'V', TEXT )
               CALL ERR_REP( ' ', 'Invalid numerical value ''^V'' '//
     :                       'supplied for parameter WEIGHTS.',
     :                       STATUS )
            ELSE IF( PARAMS( 1 ) .LE. 0.0 .AND.
     :               STATUS .EQ. SAI__OK ) THEN
               STATUS = ISTAT
               CALL MSG_SETC( 'V', TEXT )
               CALL ERR_REP( ' ', 'Zero or negative value ''^V'' '//
     :                       'supplied for parameter WEIGHTS.',
     :                       STATUS )
            END IF
         END IF

         CALL NDF_BAD( INDF1, 'DATA,VARIANCE', .FALSE., BAD_DV, STATUS )
         IF( BAD_DV ) FLAGS = FLAGS + AST__USEBAD

*  Get the pixel bounds of the input NDF.
         CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND1, UBND1, NDIM1,
     :                   STATUS )

*  Map the required components of the input.
         CALL NDF_MAP( INDF1, 'DATA', TY_IN, 'READ', IPD1, EL, STATUS )
         IF ( USEVAR .OR. VARWGT  ) THEN
            CALL NDF_MAP( INDF1, 'VAR', TY_IN, 'READ', IPV1, EL,
     :                    STATUS )
         ELSE
            IPV1 = IPD1
         END IF

*  Get a pointer to the Mapping from input to output pixel co-ordinates.
         CALL KPG1_RETRI( SIZE, I, %VAL( CNF_PVAL( IPMAP ) ), MAP,
     :                    STATUS )

*  Call the appropriate rebinning routine.
         IF ( TY_IN .EQ. '_INTEGER' ) THEN
            CALL AST_REBINSEQI( MAP, DBLE( WLIM ), NDIM1, LBND1, UBND1,
     :                          %VAL( CNF_PVAL( IPD1 ) ),
     :                          %VAL( CNF_PVAL( IPV1 ) ), METHOD_CODE,
     :                          PARAMS, FLAGS, DBLE( ERRLIM ), MAXPIX,
     :                          VAL__BADI, NDIM, LBND, UBND, LBND1,
     :                          UBND1, %VAL( CNF_PVAL( IPD2 ) ),
     :                          %VAL( CNF_PVAL( IPV2 ) ),
     :                          %VAL( CNF_PVAL( IPW ) ), NUSED,
     :                          STATUS )

         ELSE IF ( TY_IN .EQ. '_REAL' ) THEN
            CALL AST_REBINSEQR( MAP, DBLE( WLIM ), NDIM1, LBND1, UBND1,
     :                          %VAL( CNF_PVAL( IPD1 ) ),
     :                          %VAL( CNF_PVAL( IPV1 ) ), METHOD_CODE,
     :                          PARAMS, FLAGS, DBLE( ERRLIM ), MAXPIX,
     :                          VAL__BADR, NDIM, LBND, UBND, LBND1,
     :                          UBND1, %VAL( CNF_PVAL( IPD2 ) ),
     :                          %VAL( CNF_PVAL( IPV2 ) ),
     :                          %VAL( CNF_PVAL( IPW ) ), NUSED,
     :                          STATUS )

         ELSE IF ( TY_IN .EQ. '_DOUBLE' ) THEN
            CALL AST_REBINSEQD( MAP, DBLE( WLIM ), NDIM1, LBND1, UBND1,
     :                          %VAL( CNF_PVAL( IPD1 ) ),
     :                          %VAL( CNF_PVAL( IPV1 ) ), METHOD_CODE,
     :                          PARAMS, FLAGS, DBLE( ERRLIM ), MAXPIX,
     :                          VAL__BADD, NDIM, LBND, UBND, LBND1,
     :                          UBND1, %VAL( CNF_PVAL( IPD2 ) ),
     :                          %VAL( CNF_PVAL( IPV2 ) ),
     :                          %VAL( CNF_PVAL( IPW ) ), NUSED,
     :                          STATUS )

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'TY', TY_IN )
            CALL ERR_REP( 'KPS1_WMOS2_ERR1', 'KPS1_WMOS2: Unsupported'//
     :             ' rebinning data type ''^TY'' (programming error).',
     :             STATUS )
         END IF

*  Annul the input NDF identifier.
         CALL NDF_ANNUL( INDF1, STATUS )

*  If an error occurred processing the current input NDF, abort.
         IF( STATUS .NE. SAI__OK  ) GO TO 999

*  Process the next input NDF.
      END DO

*  Display a blank line.
      CALL MSG_BLANKIF( MSG__VERB, STATUS )

*  Set the bad pixel flags for the output DATA and VARIANCE arrays.
      CALL NDF_SBAD( .TRUE., INDF2, 'DATA', STATUS )
      CALL NDF_SBAD( .TRUE., INDF2, 'VARIANCE', STATUS )

*  Tidy up.
*  ========
 999  CONTINUE

*  Free resourcee.
      CALL GRP_DELET( IGRP1, STATUS )
      IF( IGRP2 .NE. GRP__NOID ) CALL GRP_DELET( IGRP2, STATUS )
      CALL PSX_FREE( IPW, STATUS )
      CALL PSX_FREE( IPMAP, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Add a context report if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'WCSMOSAIC_ERR', 'WCSMOSAIC: Failed to mosaic '//
     :                'a group of NDFs using WCS information.', STATUS )
      END IF

      END
