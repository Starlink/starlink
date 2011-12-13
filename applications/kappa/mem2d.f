      SUBROUTINE MEM2D( STATUS )
*+
*  Name:
*     MEM2D

*  Purpose:
*     Performs a Maximum-Entropy deconvolution of a 2-dimensional NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MEM2D( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     MEM2D is based on the Gull and Skilling Maximum-Entropy package
*     MEMSYS3.  It takes an image and a Point-Spread Function as input
*     and produces an equal-sized image as output with higher
*     resolution.  Facilities are provided to `analyse' the resulting
*     deconvolved image, i.e. to calculate an integrated flux in some
*     area of the deconvolved image and also an estimate of the
*     uncertainty in the integrated flux.  This allows the significance
*     of structure visible in the deconvolution to be checked.
*
*     For a detailed description of the algorithm, and further
*     references, see the MEMSYS Users' Manual, and SUN/117.

*  Usage:
*     mem2d in out mask=? { fwhmpsf=?
*                         { psf=?
*                         psftype

*  ADAM Parameters:
*     ANALYSE = _LOGICAL (Read)
*        ANALYSE should be given a TRUE value if an analysis of a
*        previously generated deconvolution is to be performed, instead
*        of a whole new deconvolution being started.  An analysis
*        returns the integrated flux in some area of the deconvolved
*        image you specify, together with the standard deviation on the
*        integrated flux value.  The area to be integrated over is
*        specified by an image associated with parameter MASK.  This
*        facility can, for instance, be used to assess the significance
*        of structure seen in the deconvolution.  An analysis can only
*        be performed if the input NDF (see parameter IN) contains a
*        MEM2D extension (see parameter EXTEND).  If the input does
*        contain such an extension, and if the extension shows that the
*        deconvolution was completed, then ANALYSE is defaulted to
*        TRUE, otherwise it is defaulted to FALSE. []
*     DEF = _REAL (Read)
*        This is the value to which the output image will default in
*        areas for which there is no valid data in the input.  The `zero
*        entropy' image is defined to be a flat surface with value
*        given by parameter DEF.  Any deviation of the output image away
*        from this image will cause its entropy to become negative.
*        Thus a maximum-entropy criterion causes the output image to be
*        as similar as possible to a flat surface with value DEF
*        (within the constraints of the data).  DEF is defaulted to the
*        mean data value in the input image and must always be strictly
*        positive. []
*     DSUM = _REAL (Write)
*        This is an output parameter to which is written the standard
*        deviation of the integrated-flux value calculated if an
*        analysis is performed (see parameter ANALYSE).
*     EXTEND = _LOGICAL (Read)
*        If EXTEND has a TRUE value, then the output NDF will contain
*        an extension called MEM2D which will contain all the
*        information required to either restart or analyse the
*        deconvolution.  Note, including this extension makes the output
*        file much bigger (by about a factor of seven).  [TRUE]
*     FWHMICF = _REAL (Read)
*        This is the Full Width at Half Maximum (in pixels) of a
*        Gaussian Intrinsic Correlation Function (ICF) to be used in
*        the deconvolution.  The ICF can be used to encode prior
*        knowledge of pixel-to-pixel correlations in the output image.
*        A value of zero for FWHMICF causes no ICF to be used, and so
*        no correlations are expected in the output.  Larger values
*        encourage smoothness in the output on the scale of the ICF.  If
*        a non-zero ICF is used, the image entropy which is maximised
*        is not the output image, but a `hidden' image.  This hidden
*        image is the deconvolution of the output image with the ICF,
*        and is assumed to have no pixel-to-pixel correlations. [2]
*     FWHMPSF = _REAL (Read)
*        This is the Full Width at Half Maximum (in pixels) of a
*        Gaussian Point-Spread Function (PSF).  This PSF is used to
*        deconvolve the input only if parameter PSFTYPE has the value
*        "Gaussian".
*     ILEVEL = _INTEGER (Read)
*        ILEVEL controls the amount of information displayed as MEM2D
*        runs.  If set to zero then no information is displayed.  Larger
*        values up to a maximum of 3, give larger amounts of
*        information.  A value of 3 gives full MEMSYS3 diagnostics
*        after each iteration. [1]
*     IN = NDF (Read)
*        The input NDF.  This can either contain an image to be
*        deconvolved, or the output from a previous run of MEM2D.  The
*        NDF is considered to be an output from MEM2D if it contains an
*        extension called MEM2D (see parameter EXTEND).  If such an
*        extension is found, a check is made to see if the NDF contains
*        a completed deconvolution or a partial deconvolution.  If the
*        deconvolution is complete, the ANALYSE parameter is defaulted
*        to TRUE, and unless you override this default, an analysis of
*        the deconvolution contained in the input NDF is performed.  If
*        the input deconvolution is not complete, then the
*        deconvolution process is restarted from where it left off.  If
*        no MEM2D extension is found, then a new deconvolution is
*        started from scratch.
*     MASK = NDF (Read)
*        An image to use as a mask to define the areas to be integrated
*        when performing an analysis (see parameter ANALYSE).  The
*        integrated-flux value calculated by the analysis is actually
*        the total data sum in the product of the mask and the
*        deconvolved image.  Mask pixel values can be positive or
*        negative (or zero) and so, for instance, masks can be arranged
*        which subtract off a background brightness from a source
*        before returning the integrated source flux.
*     MODEL = NDF (Read)
*        An image to use as the default model for the reconstruction.
*        If a null value is given, then a constant value given by the
*        parameter DEF is used to define a flat default model.  The
*        section of the given image which matches the bounds of the
*        input image is used.  Any bad pixels in the image cause the
*        corresponding pixels in the input image to be ignored.  Such
*        pixels are set bad in the output.  The model image should
*        contain no pixels with a value of zero or less.  The default
*        model is defined to have zero entropy.  The hidden image will
*        tend to the default model in the absence of data.  It should be
*        noted that this model applies to the `hidden' image, not the
*        actually required reconstructed image.  The reconstructed image
*        is obtained from the hidden image by blurring the hidden image
*        with the ICF. [!]
*     MODELOUT = NDF (Write)
*        An image which can be used for the default model in a further
*        run of MEM2D.  Each pixel value in the created image is a
*        linear combination of the model value at the corresponding
*        pixel in the current reconstruction, and the hidden image
*        pixel value.  Pixels for which the hidden image is well away
*        from the current model, tend towards the value of the hidden
*        image; pixels for which the hidden image is close to the
*        current model tend towards the model.  Running MEM2D several
*        times, using the new model created on the previous run as the
*        model for the current run, can reduce the `mottling' often
*        seen in MEM2D reconstructions. [!]
*     NITER = _INTEGER (Read)
*        The maximum number of maximum-entropy iterations to perform.
*        MEM2D continues the deconvolution until either MEMSYS3
*        indicates that the termination criterion (omega=1.0) has been
*        reached, or the maximum number of iterations is reached.  If a
*        deconvolution requires more iterations than was allowed by
*        NITER, then you can choose to continue the deconvolution by
*        giving the prematurely terminated output from MEM2D as the
*        input to another run of MEM2D, specifying a larger value for
*        NITER. [50]
*     NOISE = LITERAL (Read)
*        NOISE defines the noise statistics within the input image.  It
*        can take the value "Gaussian" or "Poisson".  If Gaussian noise
*        is selected, the data variances are set initially to the
*        values stored in the VARIANCE component of the input NDF.  If
*        no such component exists, then the data variances are set to a
*        constant value equal to the RMS difference between adjacent
*        pixels in the x direction.  MEMSYS3 scales these initial noise
*        estimates to maximise the data `evidence'.  The evidence is
*        displayed as "LOG(PROB)" and the noise scaling factor as
*        "SIGMA", if parameter ILEVEL is set to 2 or more.  If Poisson
*        statistics are selected the uncertainty in each data value is,
*        as usual, of the order of the square root of the data value.
*        When using Poisson statistics, there is no equivalent to the
*        noise scaling performed when using Gaussian statistics.  Any
*        input VARIANCE component is ignored. ["Gaussian"]
*     OUT = NDF (Write)
*        The output image in a `primitive' NDF.  The output is the same
*        size as the input.  Any pixels which were flagged as bad in
*        the input will also be bad in the output.  If parameter EXTEND
*        is TRUE, then the output NDF contains an extension called
*        MEM2D containing information which allows the deconvolution to
*        be either continued or analysed.  There is no VARIANCE
*        component in the output, but any QUALITY values are propagated
*        from the input to the output.  If parameter UPDATE is TRUE
*        then the output NDF is created after the first iteration and
*        is updated after each subsequent iteration.
*     PSF = NDF (Read)
*        An NDF holding an estimate of the Point Spread Function (PSF)
*        of the input image.  This PSF is used to deconvolve the input
*        only if parameter PSFTYPE has the value "NDF".  The PSF can be
*        centred anywhere within the image, the location of the centre
*        is specified using parameters XCENTRE and YCENTRE.  The
*        extent of the PSF actually used is controlled by parameter
*        THRESH.
*     PSFTYPE = LITERAL (Read)
*        PSFTYPE determines if the Point Spread Function used in the
*        deconvolution is to be Gaussian (if PSFTYPE = "Gaussian"), or
*        is to be defined by an image that you supply (if PSFTYPE
*        = "NDF").  ["NDF"]
*     RATE = _REAL (Read)
*        This is the value to use for the MEMSYS3 RATE parameter.  It
*        determines the rate at which the convergence is allowed to
*        proceed.  If RATE is high, each maximum-entropy iteration is
*        allowed to make a big change to the current reconstruction.
*        This can cause numeric problems within MEMSYS3 resulting in
*        MEM2D crashing with a "floating overflow" error.  If this
*        happens, try reducing RATE.  Useful values will normally be of
*        the order of unity, and must lie in the interval 0.0001 to
*        100.  [0.5]
*     SUM = _REAL (Write)
*        This is an output parameter to which is written the
*        integrated-flux value calculated if an analysis is performed
*        (see parameter ANALYSE).
*     THRESH = _REAL (Read)
*        The fraction of the PSF peak amplitude at which the extents of
*        the NDF PSF are determined.  It must be positive and less than
*        0.5.  This parameter is only used when PSFTYPE = "NDF".  An
*        error will result if the input PSF is truncated above this
*        threshold. [0.0625]
*     TITLE = LITERAL (Read)
*        A title for the output NDF.  A null (!) value means using the
*        title of the input NDF.  [!]
*     UPDATE = _LOGICAL (Read)
*        If UPDATE is given a TRUE value, then the output NDF will be
*        created after the first iteration, and will then be updated
*        after each subsequent iteration.  This means that the current
*        reconstruction can be examined without aborting the
*        application.  Also, if parameter EXTEND is TRUE, then if the
*        job aborts for any reason, it can be restarted from the last
*        completed iteration (see parameter IN). [TRUE]
*     XCENTRE = _INTEGER (Read)
*        The x pixel index of the centre of the PSF within the supplied
*        PSF image.  This is only required if PSFTYPE is NDF.  XCENTRE
*        is defaulted to the middle pixel (rounded down if there are an
*        even number of pixels per line). []
*     YCENTRE = _INTEGER (Read)
*        The y pixel index (line number) of the centre of the PSF
*        within the supplied PSF image.  This is only required if
*        PSFTYPE is NDF.  YCENTRE is defaulted to the middle line
*        (rounded down if there are an even number of lines). []

*  Examples:
*     mem2d m51 m51_hires psftype=gaussian fwhmpsf=3
*        This example deconvolves the data array in the NDF called m51,
*        putting the resulting image in the data array of the NDF called
*        m51_hires.  A circular Gaussian Point-Spread Function is used
*        with a Full Width at Half Maximum of 3 pixels.
*     mem2d m51 m51_hires psf=star xcentre=20 ycentre=20
*        This example performs the same function as the previous
*        example, but the PSF is defined by the data array of the NDF
*        called star, instead of being defined to be Gaussian.  This
*        allows the PSF to be any arbitrary 2-dimensional function.
*        NDF star could be produced for example, by the KAPPA
*        application called PSF.  Parameters XCENTRE and YCENTRE give
*        the pixel indices of the centre of the beam defined by the PSF
*        in star.  The PSF is truncated to one sixteenth of its peak
*        amplitude.
*     mem2d m51_hires m51_hires niter=70 psf=star
*        If the previous example failed to converge within the default
*        50 iterations, the deconvolution can be started again from
*        its current state, rather than having to start again from
*        scratch.  Here NITER gives the upper limit on the total number
*        of iterations which can be performed (including those performed
*        in the previous run of MEM2D), NOT just the number performed in
*        this single run of MEM2D.  This facility can also be used if a
*        MEM2D run is interrupted for any reason, such as the host
*        computer going down, or a batch-queue CPU limit being reached.
*        To use this facility the parameters EXTEND and UPDATE should
*        have the default values of TRUE.
*     mem2d m51_hires mask=nucleus
*        Once a deconvolved image has been produced, the significance
*        of features seen in the deconvolution can be assessed.  This
*        example takes in the NDF m51_hires produced by a previous run
*        of MEM2D.  If this is a completed deconvolution then the
*        parameter ANALYSE will be defaulted to TRUE, and an analysis
*        will be performed.  This effectively results in the
*        deconvolution being multiplied by the data array of the NDF
*        called nucleus, and the total data sum in the resulting image
*        being displayed, together with the standard deviation on the
*        total data sum.  The image in m51_hires is the most probable
*        deconvolution, but there may be other deconvolutions only
*        slightly less probable than m51_hires.  The standard deviation
*        produced by an analysis takes account of the spread between
*        such deconvolutions.  If the total data sum is not
*        significantly greater than the standard deviation, then the
*        feature selected by the mask image (called nucleus in this
*        case) may well be spurious.  The mask image itself may for
*        instance consist of an area of uniform value +1 covering some
*        feature of interest, and the bad value (or equivalently the
*        value zero) everywhere else.  The analysis would then give the
*        integrated flux in the feature, assuming that the background
*        is known to be zero.  If the background is not zero, then the
*        mask may contain a background region containing the value -1,
*        of equal area to the region containing the value +1.  The
*        resulting integrated flux would then be the total flux in the
*        source minus the flux in a background region of equal area.

*  Related Applications:
*     KAPPA: FOURIER, LUCY, WIENER.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of an NDF
*     data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported, though only to remove them by the DEF value.
*     -  All non-complex numeric data types can be handled.  Arithmetic is
*     performed using single-precision floating point.

*  Notes:
*     -  MEM2D requires a large quantity of memory--- almost as much as
*     the rest of KAPPA.  In order for the KAPPA monolith to load
*     without you having to increase your memory or datasize resources,
*     and because MEM2D is batch oriented (see Timing) it is only
*     available as a separate application.
*     -  Memory is required to store several intermediate images while
*     the deconvolution is in progress.  If the input image is small
*     enough, these images are stored in a statically declared, internal
*     array.  Otherwise, they are stored in dynamically mapped external
*     arrays.  There is no limit on the size of image which can be
*     processed by MEM2D (other than those imposed by limited resources
*     on the host computer).
*     -  It is sometimes desirable for the pixels in the output image
*     to be smaller than those in the input image.  For instance, if
*     the input data are critically sampled (two samples per PSF), the
*     output image may not be a very good deconvolution.  In such cases
*     sub-dividing the output pixels would give better results.  At the
*     moment MEM2D cannot do this.  Be warned that sub-dividing the
*     input pixels and then running the current version of MEM2D will
*     not have the same effect, since the noise in the input image will
*     then have pixel-to-pixel correlations, and be interpreted as real
*     structure.

*  Timing:
*     MEM deconvolution is extremely CPU intensive.  The total CPU time
*     taken depends partly on the size of the image, and partly on the
*     complexity of the structure within the image.  As a typical
*     example, a 100x100 image containing 20 Gaussians on a flat
*     background took about 34 minutes of elapsed time on an unloaded
*     DEC Alpha 2000.  Deconvolution jobs should therefore always be
*     done in batch.  To perform an analysis on a deconvolution takes
*     about the same length of time as a single deconvolution
*     iteration.

*  Copyright:
*     Copyright (C) 1990-1992 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998, 2001, 2004 Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1990 (DSB):
*        Original version.
*     26-FEB-1991 (DSB):
*        MODEL and MODELOUT parameters added.
*     27-FEB-1991 (DSB):
*        KAPPA subroutine prefixes added (KPS_ and KPG_)
*     1991 July 4 (MJC):
*        Propagated AXIS and UNITS.  Added Usage and Implementation
*        Status to the documentation.  Made error reports more helpful
*        and added a missing bad status assignment.
*     1991 July 18 (MJC):
*        Called renamed routines---KPS1_MEM*.
*     1991 October 18 (MJC):
*        Added the THRESH parameter.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 13 (MJC):
*        Made to handle significant dimensions for user-defined
*        sections.
*     1992 October 15 (MJC):
*        Increased the permitted length of the image name stored in the
*        MEM2D extension from 40 to 132.
*     22-FEB-1995 (DSB):
*        Comments re-formatted to edstar style.  Replace AIF VM routines
*        with PSX.  Replaced calls to LPG_ASSOC, KPG1_SGDIM, etc. with a
*        single call to KPG1_GTNDF.  "Usage" and "Examples" commands
*        in the prologue converted to lower case.  Removed calls to
*        FTSIZE since the FFTPACK routines can handled images of any
*        size.  Increased the size of each image until one image will
*        provide enough workspace for the FFT routines.
*     20-MAR-1995 (DSB):
*        Modified to allow use of external storage for intermediate
*        files.  This removes the restriction on the size of image
*        which can be deconvolved using MEM2D.
*     1995 April 10 (MJC):
*        Corrected typo's, and made minor stylistic changes and
*        improvements to the documentation including a Related
*        Applications section.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     29-NOV-2001 (DSB):
*        Corrected argument list for KPS1_MEMNM.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Data constants.
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'NDF_ERR'          ! NDF error definitions
      INCLUDE 'PAR_ERR'          ! PAR_ errors.

*  Global Variables:
      INCLUDE 'C1_COM'           ! Used to communicate with OPUS and
                                 ! TROPUS.
*        C1_NPX = INTEGER (Read/Write)
*           The X dimension of all internal images (including margin).
*        C1_NLN = INTEGER (Read/Write)
*           The Y dimension of all internal images (including margin).
*        C1_DIM(2) = INTEGER (Read/Write)
*           The dimensions of the input image.
*        C1_XMG = INTEGER (Read/Write)
*           The width of the margin along the X axis in pixels.
*        C1_YMG = INTEGER (Read/Write)
*           The width of the margin along the Y axis in pixels.
*        C1_ICF = REAL (Read/Write)
*           The FWHM of the Gaussian ICF
*        C1_IP( 40 ) = INTEGER (Read/Write)
*           Pointers to the mapped external work arrays. Zero if no
*           external work array is needed.

      INCLUDE 'ME_COM'           ! Common blocks etc required by MEMSYS3
*        ME_ST( ME_MEM ) = REAL (Write)
*           The array in which internal files are stored.
*        ME_KB( 40 ) = INTEGER (Read)
*           Pointers to the start of each internal file.


*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Dimensionality required
      PARAMETER( NDIM = 2 )

*  Local Variables:
      REAL     ALPHA		 ! Previous value of MEM3 argument ALPHA
      LOGICAL  ANALYS            ! True if analysis is being performed
      LOGICAL  BAD               ! True if bad pixels are present
      REAL     BETA		 ! Previous value of MEM3 argument BETA
      REAL     DEF               ! Default value in output image
      LOGICAL  EXTEND            ! True if output is to have analysis
                                 ! info appended in an extension
      REAL     FWHM              ! FWHM of gaussian PSF in pixels
      INTEGER  ILEVEL            ! Information level
      INTEGER  ILEVD             ! Information level default
      CHARACTER * ( 132 ) IMAGE  ! Name of original input image
      INTEGER  INDF              ! NDF identifier for the input image
      INTEGER  IPIN              ! Pointer to the input image
      INTEGER  IPMASK            ! Pointer to the mapped mask section
      INTEGER  IPMOD             ! Pointer to the mapped model image
      INTEGER  IPNMOD            ! Pointer to the mapped new model image
      INTEGER  IPPSF             ! Pointer to the supplied PSF image
      INTEGER  IPVAR             ! Pointer to the variance array of the
                                 ! input NDF
      INTEGER  IPW0              ! Pointer to work array
      INTEGER  ISTAT             ! MEM3 argument ISTAT from previous run
      INTEGER  ITER              ! No. of iterations completed at the
                                 ! end ! of the previous run of MEM2D
      INTEGER  LBNDIN( NDIM )    ! Lower bounds of the input NDF
      REAL     MEAN              ! Mean data value in the input image
      LOGICAL  MEM2DX            ! True if the input NDF has a MEM2D
				 ! extension
      INTEGER  METHOD            ! Previous value of MEM3 argument
                                 ! METHOD
      INTEGER  MNDF              ! NDF identifier for analysis mask
                                 ! image
      CHARACTER * ( 8 ) MODEL    ! Specifies if a CONSTANT model value
                                 ! is being used, or if the model is
                                 ! defined by an NDF
      INTEGER  MODNDF            ! NDF identifier for model image
      INTEGER  MODSEC            ! NDF identifier for a section of MODEL
                                 ! which matches the input image
      INTEGER  MSNDF		 ! NDF section identifier for analysis
				 ! mask which matches the i/p image
      INTEGER  NDIMS             ! No. of dimensions in the input NDF
      INTEGER  NEL               ! No. of elements in the input NDF
      INTEGER  NELPSF            ! No. of elements in the supplied PSF
                                 ! NDF
      INTEGER  NITER             ! Max. no. of MEMSYS3 iterations to
                                 ! perform
      INTEGER  NITERD            ! Max. no. of MEMSYS3 iterations to
                                 ! perform default
      INTEGER  NMDNDF            ! NDF identifier for a new default
                                 ! model image
      CHARACTER * ( 8 ) NOISE    ! Type of noise to use; GAUSSIAN or
                                 ! POISSON
      INTEGER  PDIMS( NDIM )     ! Dimensions of the PSF
      REAL     PSFFRA            ! Truncation threshold of input PSF
      INTEGER  PSFNDF            ! NDF identifier for the supplied PSF
      CHARACTER * ( 8 ) PSFTYP   ! Type of PSF to use; GAUSSIAN or NDF
      INTEGER  PSFXSZ            ! Approximate size of the PSF along x
      INTEGER  PSFYSZ            ! Approximate size of the PSF along y
      REAL     RATE              ! MEMSYS3 convergence rate limit
      REAL     RATED             ! MEMSYS3 convergence rate limit
                                 ! default
      INTEGER  SDIM( NDF__MXDIM ) ! Significant NDF dimensions of the
                                 ! PSF
      INTEGER  SDIMIN( NDF__MXDIM ) ! Significant NDF dimensions of the
                                 ! input image
      REAL     SIGMA		 ! Previous value of MEM3 argument SIGMA
      INTEGER  SLBND( NDIM )     ! Significant lower bounds of the PSF
      INTEGER  SLBNDI( NDIM )    ! Significant lower bounds of the input
                                 ! image
      REAL     STDEV             ! Approximate noise level in input
                                 ! image
      INTEGER  SUBND( NDIM )     ! Significant upper bounds of the PSF
      INTEGER  SUBNDI( NDIM )    ! Significant upper bounds of the input
                                 ! image
      LOGICAL  UPDATE		 ! True if the output NDF is to be
                                 ! updated at the end of each iteration
      INTEGER  UBNDIN( NDIM )    ! Upper bounds of the input NDF
      LOGICAL  VAR               ! True if the input NDF has a VARIANCE
                                 ! component
      INTEGER  WDIMS( 2 )        ! Dimensions of work array
      INTEGER  XCEN              ! X co-ordinate of PSF centre
      INTEGER  XSIZE             ! Size of internal file in X before
                                 ! correction
      INTEGER  YCEN              ! Y co-ordinate of PSF centre
      INTEGER  YSIZE             ! Size of internal file in Y before
                                 ! correction
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the NDF containing the input image, ensuring that it is
*  2-dimensional.  Also find the significant axes and their bounds
*  (i.e. axes with dimensions greater than 1).
      CALL KPG1_GTNDF( 'IN', 2, .TRUE., 'READ', INDF, SDIMIN, SLBNDI,
     :                 SUBNDI, STATUS )
      CALL NDF_BOUND( INDF, 2, LBNDIN, UBNDIN, NDIMS, STATUS )

*  If the input NDF contains analysis information then it is the result
*  of a previous run of MEM2D.  If termination has been reached
*  (indicated by the MEM3 argument ISTAT), then assume that the user
*  wants to do an analysis of the deconvolution using the MEMSYS3
*  inference routine.  If termination has not been reached, assume that
*  the deconvolution is to be continued from where the previous run
*  left off.  First see if there is an MEM2D extension in the input
*  NDF, and initialise the flag.
      ANALYS = .FALSE.
      CALL NDF_XSTAT( INDF, 'MEM2D', MEM2DX, STATUS )

*  If it does contain a MEM2D extension get the information out of the
*  extension and put it in the relevant common blocks or into the
*  arguments of KPS1_MEMGA.
      IF ( MEM2DX ) THEN

         CALL KPS1_MEMGA( INDF, RATE, ALPHA, BETA, DEF, SIGMA, STDEV,
     :                    NITER, ITER, ILEVEL, METHOD, IMAGE, ISTAT,
     :                    STATUS )

*  Set the default for parameter ANALYSE based on the value of ISTAT.
*  N.B. the conjugate gradient status digits (5th,6th and 7th) can be
*  ignored.
         ANALYS = MOD( ISTAT, 10000 ) .EQ. 0

      END IF

*  See if user wants to perform a deconvolution, or to analyse the
*  results of a previous deconvolution.
      CALL PAR_DEF0L( 'ANALYSE', ANALYS, STATUS )
      CALL PAR_GET0L( 'ANALYSE', ANALYS, STATUS )

*  If an anlysis is to be performed ..............................
      IF ( ANALYS ) THEN

*  If no MEM2D extension was found in the input NDF, abort.
         IF ( .NOT. MEM2DX ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'MEM2D_ERR0',
     :        'MEM2D: No analysis information found in input NDF ^NDF.',
     :        STATUS )
            GO TO 999
         END IF

*  Give a warning if the user is trying to analyse an unfinished
*  deconvolution.
         IF ( MOD( ISTAT, 10000 ) .NE. 0 ) THEN
            CALL MSG_OUT( 'REPORT', ' ', STATUS )
            CALL MSG_OUT( 'REPORT',
     :        'WARNING: You are trying to analyse an unfinished '/
     :        /'deconvolution.', STATUS )
         END IF

*  Tell the user the name of the NDF being analysed
         CALL MSG_OUT( 'REPORT', ' ', STATUS )
         CALL MSG_SETC( 'IMAGE', IMAGE )
         CALL MSG_OUT( 'REPORT',
     :     '  Analysing the deconvolution of ^IMAGE.', STATUS )
         CALL MSG_OUT( 'REPORT', ' ', STATUS )

*  Get an NDF holding the analysis mask, and create a section from it
*  matching the input NDF. Map the data array of the section.
         CALL LPG_ASSOC( 'MASK', 'READ', MNDF, STATUS )
         CALL NDF_SECT( MNDF, NDIMS, LBNDIN, UBNDIN, MSNDF, STATUS )
         CALL KPG1_MAP( MSNDF, 'DATA', '_REAL', 'READ', IPMASK, NEL,
     :                 STATUS )

         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  See how much run-time information is to be given to the user.
         ILEVD = ILEVEL
         CALL PAR_GDR0I( 'ILEVEL', ILEVD, 0, 3, .TRUE., ILEVEL, STATUS )

*  Call MEMIN to call the MEMSYS3 inference routine, to do the
*  analysis (file <2> is used as work space).
         IF ( C1_WEXT ) THEN
            CALL KPS1_MEMIN( 'SUM', 'DSUM', %VAL( IPMASK ), C1_DIM( 1 ),
     :                       C1_DIM( 2 ), ALPHA, DEF, SIGMA, ILEVEL,
     :                       %VAL( C1_IP( 2 ) ), STATUS )
         ELSE
            CALL KPS1_MEMIN( 'SUM', 'DSUM', %VAL( IPMASK ), C1_DIM( 1 ),
     :                       C1_DIM( 2 ), ALPHA, DEF, SIGMA, ILEVEL,
     :                       ME_ST( ME_KB( 2 ) ), STATUS )
         END IF

*  Finish.
         GO TO 999

      END IF

*  Now deal with cases where a convolution is to be performed...........

*  If the input NDF contains a MEM2D extension, tell the user that
*  this is a continuation of a previous run of MEM2D.  Give a warning
*  message if the deconvolution is already complete.
      IF ( MEM2DX ) THEN

         CALL MSG_OUT( 'REPORT', ' ', STATUS )
         CALL MSG_SETC( 'IMAGE', IMAGE )
         CALL MSG_OUT( 'REPORT', '  Continuing deconvolution of ^IMAGE',
     :                  STATUS )
         CALL MSG_OUT( 'REPORT', ' ', STATUS )

         IF ( MOD( ISTAT, 10000 ) .EQ. 0 ) THEN
            CALL MSG_OUT( 'REPORT', ' ', STATUS )
            CALL MSG_OUT( 'REPORT',
     :        'WARNING: The deconvolution is already finished.',
     :        STATUS )
         END IF

*  Allow the user to give new values for certain `safe' parameters.
         ILEVD = ILEVEL
         CALL PAR_GDR0I( 'ILEVEL', ILEVD, 0, 3, .TRUE., ILEVEL, STATUS )
         RATED = RATE
         CALL PAR_GDR0R( 'RATE', RATED, 0.0001, 100.0, .TRUE., RATE,
     :                   STATUS )
         NITERD = NITER
         CALL PAR_GDR0I( 'NITER', NITERD, 0, VAL__MAXI, .TRUE., NITER,
     :                   STATUS )
         CALL PAR_GET0L( 'UPDATE', UPDATE, STATUS )
         CALL PAR_GET0L( 'EXTEND', EXTEND, STATUS )

*  Call MEMCO to continue the deconvolution.
         CALL KPS1_MEMCO( 'OUT', UPDATE, EXTEND, INDF, RATE, DEF,
     :                    NITER, NOISE, STDEV, ILEVEL, METHOD, ALPHA,
     :                    BETA, ITER, STATUS )

*  Finish.
         GO TO 999
      END IF


*  Now deal with deconvolutions starting from scratch...................

*  See how much run-time information is to be given to the user.
      CALL PAR_GDR0I( 'ILEVEL', 2, 0, 3, .TRUE., ILEVEL, STATUS )

*  Find the dimensions of the input image.
      C1_DIM( 1 ) = SUBNDI( 1 ) - SLBNDI( 1 ) + 1
      C1_DIM( 2 ) = SUBNDI( 2 ) - SLBNDI( 2 ) + 1

*  Map the data array.
      CALL KPG1_MAP( INDF, 'DATA', '_REAL', 'READ', IPIN, NEL, STATUS )

*  Abort if an error occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get an image is to use for the default model.
      CALL ERR_MARK

      CALL LPG_ASSOC( 'MODEL', 'READ', MODNDF, STATUS )

*  If a null value was given, use a constant default model.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MODEL = 'CONSTANT'

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         MODEL = 'NDF'

      END IF

      CALL ERR_RLSE

*  See if the user wants to use a Gaussian PSF of specified FWHM, or
*  wants to give an image of the PSF to use.
      CALL PAR_CHOIC( 'PSFTYPE', 'GAUSSIAN', 'GAUSSIAN,NDF', .FALSE.,
     :                PSFTYP, STATUS )

*  Deal first with the case where the PSF is given in an NDF.
      IF ( PSFTYP .EQ. 'NDF' ) THEN

*  Get the truncation threshold as fraction of the peak amplitude of the
*  PSF.
         CALL PAR_GDR0R( 'THRESH', 0.0625, VAL__SMLR, 0.5, .FALSE.,
     :                   PSFFRA, STATUS )

*  Get the NDF containing the PSF image, ensuring that it is
*  2-dimensional.  Also find the significant axes and their bounds
*  (i.e. axes with dimensions greater than 1).
         CALL KPG1_GTNDF( 'PSF', 2, .TRUE., 'READ', PSFNDF, SDIM, SLBND,
     :                    SUBND, STATUS )
         PDIMS( 1 ) = SUBND( 1 ) - SLBND( 1 ) + 1
         PDIMS( 2 ) = SUBND( 2 ) - SLBND( 2 ) + 1

*  See if the PSF contains any bad pixels. If it does then abort.
         CALL NDF_BAD( PSFNDF, 'Data', .TRUE., BAD, STATUS )

         IF ( BAD ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'PSFNDF', PSFNDF )
            CALL ERR_REP('MEM2D_ERR1',
     :        'MEM2D: PSF ^PSFNDF contains some bad pixels.', STATUS )
            GO TO 999
         END IF

*  Map the PSF.
         CALL KPG1_MAP( PSFNDF, 'DATA', '_REAL', 'READ', IPPSF, NELPSF,
     :                 STATUS )

*  Get work space for use in routine for finding PSF size.
         WDIMS( 1 ) = MAX( PDIMS( 2 ), PDIMS( 1 ) )
         WDIMS( 2 ) = 2
         CALL PSX_CALLOC( WDIMS( 1 ) * WDIMS( 2 ), '_REAL', IPW0,
     :                    STATUS )

*  Abort if an error occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get approximate width of the PSF along both image axes.  The
*  internal image files will be padded with a blank margin of this size
*  to reduce edge effects caused by wrap-around in the convolution
*  routines.
         CALL KPG1_PSFSR( %VAL( IPPSF ), PDIMS( 1 ), PDIMS( 2 ),
     :                    %VAL( IPW0 ), WDIMS( 1 ), WDIMS( 2 ), PSFFRA,
     :                    ILEVEL, PSFXSZ, PSFYSZ, STATUS )

*  Release work space used by PSFSIZ.
         CALL PSX_FREE( IPW0, STATUS )

*  Add this margin onto the input image dimensions to get the
*  dimensions of the internal images.
         C1_XMG = PSFXSZ
         C1_YMG = PSFYSZ
         XSIZE = C1_DIM(1) + 2 * C1_XMG
         YSIZE = C1_DIM(2) + 2 * C1_YMG

*  The 2-d Hermitian FFT routines (KPG1_FFTFR and KPG1_FFTBR) require
*  a work array containing at least ( 3*MAX(M,N)+15 ) elements, where
*  M and N are the dimensions of the array being FFTed.  Ensure that
*  the internal image size is large enough to provide such a work array.
         C1_NPX = MAX( 6, XSIZE )
         C1_NLN = MAX( 6, YSIZE )

*  Store the resulting size in C1_COM and update the margin size
*  accordingly.
         C1_XMG = ( C1_NPX - C1_DIM( 1 ) ) / 2
         C1_YMG = ( C1_NLN - C1_DIM( 2 ) ) / 2

*  Set up the common block /MECOMP/.
         CALL KPS1_MEMCP( ILEVEL, MODEL, STATUS )

*  Abort if an error occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the co-ordinates of the centre of the PSF within the image.  The
*  default is the centre of the PSF image.
         CALL PAR_GDR0I( 'XCENTRE', SLBND( 1 ) + PDIMS( 1 ) / 2,
     :                   SLBND( 1 ), SUBND( 1 ), .FALSE., XCEN, STATUS )
         CALL PAR_GDR0I( 'YCENTRE', SLBND( 2 ) + PDIMS( 2 ) / 2,
     :                   SLBND( 2 ), SUBND( 2 ), .FALSE., YCEN, STATUS )

*  Store the Fourier transform of the PSF in the internal file <3>.
*  Internal file <2> is used as work space.  If external storage is
*  being used, use the poitners in C1_IP to find the internal files.
*  Otherwise find them in the ME_ST array.
         IF ( C1_WEXT ) THEN
            CALL KPS1_MEMFP( %VAL( IPPSF ), %VAL( C1_IP( 2 ) ),
     :                       PDIMS( 1 ), PDIMS( 2 ),
     :                       XCEN - SLBND( 1 ) + 1,
     :                       YCEN - SLBND( 2 ) + 1, %VAL( C1_IP( 3 ) ),
     :                       STATUS )
         ELSE
            CALL KPS1_MEMFP( %VAL( IPPSF ), ME_ST( ME_KB( 2 ) ),
     :                       PDIMS( 1 ), PDIMS( 2 ),
     :                       XCEN - SLBND( 1 ) + 1,
     :                       YCEN - SLBND( 2 ) + 1, ME_ST( ME_KB( 3 ) ),
     :                       STATUS )
         END IF

*  Now deal with the case where the user wants to use a Gaussian PSF.
      ELSE

*  Get the Full Width at Half Maximum of the Gaussian.
         CALL PAR_GDR0R( 'FWHMPSF', 10.0, 0.0,
     :                   REAL( MIN( C1_DIM(1), C1_DIM(2) ) ), .FALSE.,
     :                   FWHM, STATUS )

*  Set the internal image dimensions to include a blank margin twice
*  the width of the Gaussian, to reduce edge effects caused by
*  wrap-around in the convolution routines.
         C1_XMG = INT( 2.0 * FWHM )
         C1_YMG = INT( 2.0 * FWHM )
         XSIZE = C1_DIM( 1 ) + 2 * C1_XMG
         YSIZE = C1_DIM( 2 ) + 2 * C1_YMG

*  Check that this image size can be handled by the FFT routines.
*  The 2-d Hermitian FFT routines (KPG1_FFTFR and KPG1_FFTBR) require
*  a work array containing at least ( 3*MAX(M,N)+15 ) elements, where
*  M and N are the dimensions of the array being FFTed.  Ensure that
*  the internal image size is large enough to provide such a work array.
         C1_NPX = MAX( 6, XSIZE )
         C1_NLN = MAX( 6, YSIZE )

*  Store the resulting size in C1_COM and update the margin size
*  accordingly.
         C1_XMG = ( C1_NPX - C1_DIM(1) )/2
         C1_YMG = ( C1_NLN - C1_DIM(2) )/2

*  Set up the common block /MECOMP/.
         CALL KPS1_MEMCP( ILEVEL, MODEL, STATUS )

*  Abort if an error occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Calculate the Gaussian and stores its FFT in internal file <3>.
*  File <2> is used as work space.  If external storage is being used,
*  use the pointers in C1_IP to find the internal files.  Otherwise
*  find them in the ME_ST array.
         IF ( C1_WEXT ) THEN
            CALL KPS1_GAUPS( FWHM, %VAL( C1_IP( 2 ) ),
     :                       %VAL( C1_IP( 3 ) ), STATUS )
         ELSE
            CALL KPS1_GAUPS( FWHM, ME_ST( ME_KB( 2 ) ),
     :                       ME_ST( ME_KB( 3 ) ), STATUS )
         END IF

      END IF

*  If required, tell the user the size of the margins and the size of
*  the internal data files.
      IF ( ILEVEL .GE. 3 ) THEN
         CALL MSG_OUT( 'REPORT', ' ', STATUS )
         CALL MSG_SETI( 'C1_XMG', C1_XMG )
         CALL MSG_OUT( 'REPORT',
     :     '  X axis margin is ^C1_XMG pixels wide.', STATUS )
         CALL MSG_SETI( 'C1_YMG', C1_YMG )
         CALL MSG_OUT( 'REPORT',
     :     '  Y axis margin is ^C1_YMG lines wide.', STATUS )
         CALL MSG_SETI( 'XSIZE', C1_NPX )
         CALL MSG_SETI( 'YSIZE', C1_NLN )
         CALL MSG_OUT( 'REPORT',
     :     '  Internal file size is ^XSIZE by ^YSIZE.', STATUS )
         CALL MSG_OUT( 'REPORT', ' ', STATUS )
      END IF

*  Get the width of the Gaussian Intrinsic Correlation Function to use.
      CALL PAR_GDR0R( 'FWHMICF', 2.0, 0.0,
     :                REAL( MIN( C1_DIM(1), C1_DIM(2) ) ), .FALSE.,
     :                C1_ICF, STATUS )

*  Blur the PSF in file <3> with the ICF.  Internal file <1> and
*  <2> are used as work space.
      IF ( C1_WEXT ) THEN
         CALL KPS1_ICBLU( C1_ICF, .TRUE., %VAL( C1_IP( 1 ) ),
     :                    %VAL( C1_IP( 2 ) ), %VAL( C1_IP( 3 ) ),
     :                    STATUS )
      ELSE
         CALL KPS1_ICBLU( C1_ICF, .TRUE., ME_ST( ME_KB( 1 ) ),
     :                    ME_ST( ME_KB( 2 ) ), ME_ST( ME_KB( 3 ) ),
     :                    STATUS )
      END IF

*  If the NDF has a variance component, map it.
      CALL NDF_STATE( INDF, 'VARIANCE', VAR, STATUS )

      IF ( VAR ) THEN
         CALL KPG1_MAP( INDF, 'VARIANCE', '_REAL', 'READ', IPVAR, NEL,
     :                 STATUS)
      ELSE
         IPVAR = IPIN
      ENDIF

*  Abort if an error occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  See what sort of noise to use.
      CALL PAR_CHOIC( 'NOISE', 'GAUSSIAN', 'GAUSSIAN,POISSON', .FALSE.,
     :                NOISE, STATUS )

*  Set up internal files <21> and <22>, and find the mean input data
*  value and a rough estimate of the noise in the input.
      IF ( C1_WEXT ) THEN
         CALL KPS1_MEMCS( %VAL( IPIN ), %VAL( IPVAR ), C1_DIM( 1 ),
     :                    C1_DIM( 2 ), VAR, ILEVEL, C1_XMG, C1_YMG,
     :                    %VAL( C1_IP( 21 ) ), %VAL( C1_IP( 22 ) ),
     :                    STDEV, MEAN, STATUS)
      ELSE
         CALL KPS1_MEMCS( %VAL( IPIN ), %VAL( IPVAR ), C1_DIM( 1 ),
     :                    C1_DIM( 2 ), VAR, ILEVEL, C1_XMG, C1_YMG,
     :                    ME_ST( ME_KB( 21 ) ), ME_ST( ME_KB( 22 ) ),
     :                    STDEV, MEAN, STATUS)
      END IF

*  If the mean data value is not positive, MEMSYS3 will have a hard
*  time (in fact it will probably take an infinite amount of CPU to get
*  no where).  Abort.
      IF ( MEAN .LE. 0.0 ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'INDF', INDF )
         CALL ERR_REP( 'MEM2D_ERR2',
     :     'MEM2D: Mean data value in input NDF ^INDF is negative.',
     :     STATUS )
         GO TO 999
      END IF

*  If necessary, get a value for the constant model.
      IF ( MODEL .EQ. 'CONSTANT' ) THEN
         CALL PAR_GDR0R( 'DEF', MEAN, VAL__SMLR, VAL__MAXR, .FALSE.,
     :                   DEF, STATUS )

*  Otherwise get a section of the model NDF which matches the bounds of
*  the input image, and map the data array.
      ELSE
         CALL NDF_SECT( MODNDF, NDIMS, LBNDIN, UBNDIN, MODSEC, STATUS )
         CALL KPG1_MAP( MODSEC, 'DATA', '_REAL', 'READ', IPMOD, NEL,
     :                 STATUS )

*  Store the model in internal file <20>, replacing bad pixels with the
*  value zero, to exclude them from the reconstruction.
         IF ( C1_WEXT ) THEN
            CALL KPS1_MEM20( %VAL( IPMOD ), C1_DIM( 1 ), C1_DIM( 2 ),
     :                       %VAL( C1_IP( 20 ) ), STATUS )
         ELSE
            CALL KPS1_MEM20( %VAL( IPMOD ), C1_DIM( 1 ), C1_DIM( 2 ),
     :                       ME_ST( ME_KB( 20 ) ), STATUS )
         END IF

      END IF

*  Get values for the other arguments required to use MEMSYS3.
      CALL PAR_GDR0R( 'RATE', 0.5, 0.0001, 100.0, .TRUE., RATE, STATUS )
      CALL PAR_GDR0I( 'NITER', 30, 0, VAL__MAXI, .TRUE., NITER, STATUS )
      CALL PAR_GET0L( 'UPDATE', UPDATE, STATUS )
      CALL PAR_GET0L( 'EXTEND', EXTEND, STATUS )

*  Activate MEMSYS3 to do the convolution and produce the output NDF.
      CALL KPS1_MEMSY( 'OUT', MODEL, UPDATE, EXTEND, INDF,
     :                  RATE, DEF, NITER, NOISE, STDEV, ILEVEL, STATUS )

 999  CONTINUE

*  Attempt to get an NDF to store the updated default model in. The new
*  NDF is based on the input NDF.
      CALL ERR_MARK

      CALL LPG_PROP( INDF, 'WCS,AXIS,QUALITY,UNITS', 'MODELOUT', NMDNDF,
     :               STATUS )

*  If an NDF was given, store the new model in it.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         CALL KPG1_MAP( NMDNDF, 'DATA', '_REAL', 'WRITE/BAD', IPNMOD,
     :                 NEL, STATUS )

         CALL KPS1_MEMNM( DEF, C1_DIM( 1 ), C1_DIM( 2 ),
     :                    ME_ST( ME_KB( 1 ) ), ME_ST( ME_KB( 20 ) ),
     :                    ME_ST( ME_KB( 22 ) ), ME_ST( ME_KB( 2 ) ),
     :                    %VAL( IPNMOD ), STATUS )

      END IF

      CALL ERR_RLSE

*  Free the external work array if one has been used.
      IF ( C1_WEXT ) CALL PSX_FREE( C1_IP0, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

      END
