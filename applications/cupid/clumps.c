#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/ndg.h"
#include "ast.h"
#include "star/kaplibs.h"
#include "star/grp.h"
#include "par.h"
#include "prm_par.h"
#include "cupid.h"
#include <math.h>
#include <string.h>
#include <stdio.h>


/* If the FPTRAP macros is defined, then the fptrapfunction defined here
   will be called in order to cause floating point exceptions to be
   generated when a NaN value is returned from a calculation. This can be
   useful when debugging since otherwise it can be difficult to determine
   where the NaN values are coming from. */

#if defined(FPTRAP)
#   include <fpu_control.h>
#     if defined(__i386__)
#       if !defined(_FPU_GETCW)
#         define _FPU_GETCW(cw) (cw=__getfpucw())
#       endif
#       if !defined(_FPU_SETCW)
#         define _FPU_SETCW(cw) (__setfpucw(cw))
#       endif
void
fptrap (int i)
{
    unsigned int cw;
    _FPU_GETCW(cw);
    _FPU_SETCW(i==0 ? cw | _FPU_MASK_ZM | _FPU_MASK_IM | _FPU_MASK_OM :
                       cw & ~(_FPU_MASK_ZM | _FPU_MASK_IM | _FPU_MASK_OM));
}

#  endif 
#endif


void clumps() {
/*
*+
*  Name:
*     CLUMPS

*  Purpose:
*     Identify clumps of emission within a 1, 2 or 3 dimensional NDF.

*  Language:
*     C

*  Type of Module:
*     ADAM A-task

*  Synopsis:
*     void clumps();

*  Description:
*     This application identifies clumps of emission within a 1, 2 or 3 
*     dimensional NDF. It is assumed that any background has already been
*     removed form the data array. Information about the clumps is returned 
*     in several different ways:
*
*     - A pixel mask identifying pixels as background pixels or clump
*     pixels can be written to the Quality array of the input NDF (see 
*     parameter MASK).
*
*     - An output catalogue containing clump parameters can be created (see
*     parameter OUTCAT).
*
*     - Information about each clump, including a minimal cut-out image
*     of the clump and the clump parameters, is written to the CUPID 
*     extension of the input NDF (see the section "Use of CUPID Extension" 
*     below). 
*
*     The algorithm used to identify the clumps (GaussCLumps, ClumpFind,
*     etc) can be specified (see parameter METHOD).

*  Usage:
*     clumps in outcat method mask

*  ADAM Parameters:
*     CONFIG = GROUP (Read)
*        Specifies values for the configuration parameters used by the
*        clump finding algorithms. If the string "def" (case-insensitive)
*        or a null (!) value is supplied, a set of default configuration 
*        parameter values will be used.
*
*        A comma-separated list of strings should be given in which each
*        string is either a "keyword=value" setting, or the name of a text 
*        file preceded by an up-arrow character "^". Such text files should
*        contain further comma-separated lists which will be read and 
*        interpreted in the same manner (any blank lines or lines beginning 
*        with "#"). Settings are applied in the order in which they occur 
*        within the list, with later settings over-riding any earlier 
*        settings given for the same keyword.
*
*        Each individual setting should be of the form:
*
*           <keyword>=<value>
*        
*        where <keyword> has the form "algorithm.param"; that is, the name
*        of the algorithm, followed by a dot, followed by the name of the
*        parameter to be set. If the algorithm name is omitted, the current
*        algorithm given by parameter METHOD is assumed. The parameters 
*        available for each algorithm are listed in the "Configuration 
*        Parameters" sections below. Default values will be used for any 
*        unspecified parameters. Unrecognised options are ignored (that is, 
*        no error is reported). [current value]
*     ILEVEL = _INTEGER (Read)
*        Controls the amount of diagnostic information reported. It
*        should be in the range 0 to 6. A value of zero will suppress all 
*        screen output. Larger values give more information (the precise 
*        information displayed depends on the algorithm being used). [1]
*     IN = NDF (Update)
*        The 1, 2 or 3 dimensional NDF to be analysed. Information about
*        the identified clumps and the configuration parameters used will 
*        be stored in the CUPID extension of the supplied NDF, and so the 
*        NDF must not be write protected. See "Use of CUPID Extension"
*        below for further details about the information stored in the CUPID
*        extension. Other applications within the CUPID package can be used 
*        to display this information in various ways.
*     MASK = _LOGICAL (Read)
*        If true, then a Quality component is added to the supplied NDF
*        (replacing any existing Quality component) indicating if each pixel 
*        is inside or outside a clump. Two quality bits will be used; one is 
*        set if and only if the pixel is contained within one or more clumps,
*        the other is set if and only if the pixel is not contained within 
*        any clump. These two quality bits have names associated with
*        them which can be used with the KAPPA applications SETQUAL, 
*        QUALTOBAD, REMQUAL, SHOWQUAL. The names used are "CLUMP" and
*        "BACKGROUND". [current value]
*     METHOD = LITERAL (Read)
*        The algorithm to use. Each algorithm is described in more detail
*        in the "Algorithms:" section below. Can be one of:
*
*        - GaussClumps
*        - ClumpFind
*        - Reinhold
*        - FellWalker
*
*        Each algorithm has a collection of extra tuning values which are
*        set via the CONFIG parameter.   [current value]
*     OUT = NDF (Write)
*        An optional output NDF which has the same shape and size as the
*        input NDF. The information written to this NDF depends on the value 
*        of the METHOD parameter. If METHOD is GaussClumps, the output NDF 
*        receives the sum of all the fitted Gaussian clump models including
*        a global background level chosen to make the mean output value
*        equal to the mean input value. If METHOD is ClumpFind, FellWalker or 
*        Reinhold, each pixel in 
*        the output is the integer index of clump to which the pixel has been 
*        assigned. Bad values are stored for pixels which are not part of
*        any clump. No output NDF will be produced if a null (!) value is 
*        supplied. Otherwise, the output NDF will inherit the AXIS, WCS and 
*        QUALITY components (plus any extensions) from the input NDF. [!]
*     OUTCAT = FILENAME (Write)
*        An optional output catalogue in which to store the clump parameters.
*        No catalogue will be produced if a null (!) value is supplied. The
*        following columns are stored in the catalogue: 
*
*        - PeakX: The PIXEL X coordinates of the clump peak value.
*        - PeakY: The PIXEL Y coordinates of the clump peak value.
*        - PeakZ: The PIXEL Z coordinates of the clump peak value.
*        - CenX: The PIXEL X coordinates of the clump centroid.
*        - CenY: The PIXEL Y coordinates of the clump centroid.
*        - CenZ: The PIXEL Z coordinates of the clump centroid.
*        - SizeX: The size of the clump along the X axis, in pixels.
*        - SizeY: The size of the clump along the Y axis, in pixels.
*        - SizeZ: The size of the clump along the Z axis, in pixels.
*        - Sum: The total data sum in the clump.
*        - Peak: The peak value in the clump.
*        - Area: The total number of pixels falling within the clump.
*
*        If the data has less than 3 pixel axes, then the columns
*        describing the missing axes will not be present in the catalogue.
*
*        The "size" of the clump on an axis is the RMS deviation of each 
*        pixel centre from the clump centroid, where each pixel is
*        weighted by the correspinding pixel data value.
*
*        For the GaussClump algorithm, the Sum and Area values refer 
*        to the part of the Gaussian within the level defined by the
*        GaussClump.ModelLim configuration parameter.
*
*        The KAPPA command "listshow" can be used to draw markers at the 
*        central positions of the clumps described in a catalogue. For
*        instance, the command "listshow fred plot=mark" will draw
*        markers identifying the positions of the clumps described in 
*        file fred.FIT, overlaying the markers on top of the currently
*        displayed image. [!]
*     REPCONF = _LOGICAL (Read)
*        If a TRUE value is supplied, then the configuration parameters
*        supplied by the CONFIG parameter will be listed to standard 
*        output. [current value]
*     RMS = _DOUBLE (Read)
*        Specifies a value to use as the global RMS noise level in the 
*        supplied data array. The suggested defaukt value is the square root 
*        of the mean of the values in the input NDF's Variance component is 
*        used. If the NDF has no Variance component, the suggested default 
*        is based on the differences between neighbouring pixel values. Any 
*        pixel-to-pixel correlation in the noise can result in this estimate 
*        being too low. 

*  Use of CUPID Extension:
*     This application will create an NDF extension called "CUPID" in the 
*     input NDF (unless there is already one there), and add the following 
*     components to it, erasing any of the same name which already exist:
* 
*     - CLUMPS: This a an array of CLUMP structures, one for each clump
*     identified by the selected algorithm. Each such structure contains 
*     the same clump parameters that are written to the catalogue via
*     parameter OUTCAT. It also contains a component called MODEL which
*     is an NDF containing a section of the main input NDF which is just
*     large enough to encompass the clump. Any pixels within this section 
*     which are not contained within the clump are set bad. So for instance,
*     if the input array "fred.sdf" is 2-dimensional, and an image of it has 
*     been displayed using KAPPA:DISPLAY, then the outline of clump number 9
*     (say) can be overlayed on the image by doing 
*
*     contour noclear "fred.more.cupid.clumps(9).model" mode=good
*
*     - CONFIG: Lists the algorithm configuration parameters used to
*     identify the clumps (see parameter CONFIG).
*
*     - QUALITY_NAMES: Defines the textual names used to identify background 
*     and clump pixels within the Quality mask. See parameter MASK.

*  Algorithms:
*     - GaussClumps: Based on the algorithm described by Stutski & Gusten 
*     (1990, ApJ 356, 513). This algorithm proceeds by fitting a Gaussian 
*     profile to the brightest peak in the data. It then subtracts the fit 
*     from the data and iterates, fitting a new ellipse to the brightest peak 
*     in the residuals. This continues until a series of consecutive fits
*     are made which have peak values below a given multiple of the noise
*     level. Each fitted ellipse is taken to be a single clump and is added 
*     to the output catalogue. In this algorithm, clumps may overlap. Any 
*     input variance component is used to scale the weight associated with 
*     each pixel value when performing the Gaussian fit. The most significant 
*     configuration parameters for this algorithm are: GaussClumps.FwhmBeam 
*     and GaussClumps.VeloRes which determine the minimum clump size, and 
*     GaussClumps.Thresh which (together with the ADAM paramater RMS) 
*     determine the termination criterion.

*     - ClumpFind: Described by Williams et al (1994, ApJ 428, 693). This 
*     algorithm works by first contouring the data at a multiple of the
*     noise, then searches for peaks of emission which locate the clumps,
*     and then follows them down to lower intensities. No a priori clump
*     profile is assumed. In this algorithm, clumps never overlap. Clumps
*     which touch an edge of the data array are not included in the final
*     list of clumps.

*     - Reinhold: Based on an algorithm developed by Kim Reinhold at JAC.
*     See SUN/255 for more information on this algorithm. The edges of the 
*     clumps are first found by searching for peaks within a set of 1D 
*     profiles running through the data, and then following the wings of 
*     each peak down to the noise level or to a local minimum. A mask is 
*     thus produced in which the edges of the clumps are marked. These edges 
*     however tend to be quite noisey, and so need to be cleaned up before 
*     further use. This is done using a pair of cellular automata which 
*     first dilate the edge regions and then erode them. The volume between 
*     the edges are then filled with an index value associated with the 
*     peak position. Another cellular automata is used to removed noise 
*     from the filled clumps.
*
*     - FellWalker: Based on an algorithm which walks up hill along the
*     line of greatest gradient until a significant peak is reached. It then
*     assigns all pixels visited along the route to the clump associated
*     with the peak. Such a walk is performed for every pixel in the data
*     array which is above a specified background level. See SUN/255 for more 
*     information on this algorithm. 

*  GaussClumps Configuration Parameters:
*     The GaussClumps algorithm uses the following configuration parameters. 
*     Values for these parameters can be specified using the CONFIG parameter. 
*     Default values are shown in square brackets:
*
*     - GaussClumps.FwhmBeam: The FWHM of the instrument beam, in
*     pixels. The fitted Gaussians are not allowed to be smaller than the
*     instrument beam. This prevents noise spikes being fitted. [3.0]
*     - GaussClumps.FwhmStart: An initial guess at the ratio of the typical 
*     observed clump size to the instrument beam width. This is used to
*     determine the starting point for the algorithm which finds the best
*     fitting Gaussian for each clump. If no value is supplied, the
*     initial guess at the clump size is based on the local profile
*     around the pixel with peak value. []
*     - GaussClumps.MaxClumps: Specifies a termination criterion for
*     the GaussClumps algorithm. The algorithm will terminate when
*     "MaxClumps" clumps have been identified, or when one of the other 
*     termination criteria is met. [unlimited]
*     - GaussClumps.MaxNF: The maximum number of evaluations of the
*     objective function allowed when fitting an individual clump. Here,
*     the objective function evaluates the chi-squared between the
*     current gaussian model and the data being fitted. [100]
*     - GaussClumps.MaxSkip: The maximum number of consecutive failures 
*     which are allowed when fitting Guassians. If more than "MaxSkip" 
*     consecutive clumps cannot be fitted, the iterative fitting
*     process is terminated. [10]
*     - GaussClumps.ModelLim: Determines the value at which each Gaussian
*     model is truncated to zero. Model values below ModelLim times the RMS
*     noise are treated as zero. [3.0]
*     - GaussClumps.NPad: Specifies a termination criterion for the 
*     GaussClumps algorithm. The algorithm will terminate when "Npad" 
*     consecutive clumps have been fitted all of which have peak values less 
*     than the threshold value specified by the "Thresh" parameter. [10]
*     - GaussClumps.S0: The Chi-square stiffness parameter "S0" which 
*     encourages the peak amplitude of each fitted gaussian to be below 
*     the corresponding maximum value in the observed data (see the Stutski 
*     & Gusten paper). [1.0]
*     - GaussClumps.Sa: The Chi-square stiffness parameter "Sa" which 
*     encourages the peak amplitude of each fitted gaussian to be close to 
*     the corresponding maximum value in the observed data (see the Stutski 
*     & Gusten paper). [1.0]
*     - GaussClumps.Sb: An additional Chi-square stiffness parameter which
*     encourages the background value to stay close to its initial value.
*     This stiffness is not present in the Stutzki & Gusten paper but is
*     added because the background value is usually determined by data
*     points which have very low weight and is thus poorly constrained. It
*     would thus be possibly to get erroneous background values without
*     this extra stiffness. [0.1]
*     - GaussClumps.Sc: The Chi-square stiffness parameter "Sc" which 
*     encourages the peak position of each fitted gaussian to be close to 
*     the corresponding peak position in the observed data (see the Stutski 
*     & Gusten paper). [1.0]
*     - GaussClumps.Thresh: Gives the minimum peak amplitude of clumps to
*     be fitted by the GaussClumps algorithm (see also GaussClumps.NPad).
*     The value should be supplied as a multiple of the RMS noise level.
*     (see ADAM parameter RMS). [20.0]
*     - GaussClumps.VeloRes: The velocity resolution of the instrument, in
*     channels. The velocity FWHM of each clump is not allowed to be
*     smaller than this value. Only used for 3D data. [3.0]
*     - GaussClumps.VeloStart: An initial guess at the ratio of the typical 
*     observed clump velocity width to the velocity resolution. This is used to
*     determine the starting point for the algorithm which finds the best
*     fitting Gaussian for each clump. If no value is supplied, the
*     initial guess at the clump velocity width is based on the local profile
*     around the pixel with peak value. Only used for 3D data. []
*     - GaussClumps.Wmin: This parameter, together with GaussClumps.Wwidth, 
*     determines which input data values are used when fitting a Gaussian to 
*     a given peak in the data array. It specifies the minimum weight
*     which is to be used (normalised to a maximum weight value of 1.0). 
*     Pixels with weight smaller than this value are not included in the 
*     fitting process. [0.05]
*     - GaussClumps.Wwidth: This parameter, together with GaussClumps.Wmin, 
*     determines which input data values are used when fitting a Gaussian to 
*     a given peak in the data array. It is the ratio of the width of the
*     Gaussian weighting function (used to weight the data around each clump 
*     during the fitting process), to the width of the initial guess Guassian 
*     used as the starting point for the Gaussian fitting process. The
*     Gaussian weighting function has the same centre as the initial guess 
*     Gaussian. [2.0]

*  ClumpFind Configuration Parameters:
*     The ClumpFind algorithm uses the following configuration parameters. 
*     Values for these parameters can be specified using the CONFIG parameter. 
*     Default values are shown in square brackets:
*
*     - ClumpFind.DeltaT: The gap between the contour levels. Only accessed 
*     if no value is supplied for "Level1", in which case the contour levels 
*     are linearly spaced, starting at a lowest level given by "Tlow" and 
*     spaced by "DeltaT". Note, small values of DeltaT can result in noise 
*     spikes being interpreted as real peaks, whilst large values can result 
*     in some real peaks being missed and merged in with neighbouring peaks. 
*     The default value of two times the RMS noise level (as specified by
*     the ADAM parameter RMS) is usually considered to be optimal, 
*     although this obviously depends on the RMS noise level being correct. 
*     If the default value of twice the RMS would produce more than 100 
*     contours, the default value is increased so that it would produce 100 
*     contours. []
*     - ClumpFind.Level<n>: The n'th data value at which to contour the
*     data array (where <n> is an integer). Values should be given for 
*     "Level1", "Level2", "Level3", etc. Any number of contours can be 
*     supplied, but there must be no gaps in the progression of values for 
*     <n>. The values will be sorted into descending order before being 
*     used. If "Level1" is not supplied, the contour levels are instead 
*     determined automatically using parameters "Tlow" and "DeltaT". Note 
*     clumps found at higher contour levels are traced down to the lowest 
*     supplied contour level, but any new clumps which are initially found 
*     at the lowest contour level are ignored. That is, clumps must have 
*     peaks which exceed the second lowest contour level to be included in 
*     the returned catalogue. []
*     - ClumpFind.MinPix: The lowest number of pixel which a clump can
*     contain. If a candidate clump has fewer than this number of pixels, 
*     it will be ignored. This prevents noise spikes from being interpreted 
*     as real clumps. [16]
*     - ClumpFind.Naxis: Controls the way in which contiguous areas of
*     pixels are located when contouring the data. When a pixel is found
*     to be at or above a contour level, the adjacent pixels are also checked.
*     "Naxis" defines what is meant by an "adjacent" pixel in this sense. 
*     The supplied value must be at least 1 and must not exceed the number 
*     of pixel axes in the data. The default value equals the number of
*     pixel axes in the data. If the data is 3-dimensional, any given pixel 
*     can be considered to be at the centre of a cube of neighbouring pixels. 
*     If "Naxis" is 1 only those pixels which are at the centres of the cube 
*     faces are considered to be adjacent to the central pixel. If "Naxis" 
*     is 2, pixels which are at the centre of any edge of the cube are also 
*     considered to be adjacent to the central pixel. If "Naxis" is 3, pixels
*     which are at the corners of the cube are also considered to be adjacent 
*     to the central pixel. If the data is 2-dimensional, any given pixel can 
*     be considered to be at the centre of a square of neighbouring pixels. 
*     If "Naxis" is 1 only those pixels which are at the centres of the
*     square edges are considered to be adjacent to the central pixel. If 
*     "Naxis" is 2, pixels which are at square corners are also considered 
*     to be adjacent to the central pixel. For one dimensional data, a
*     value of 1 is always used for "Naxis", and each pixel simply has 2 
*     adjacent pixels, one on either side. []
*     - ClumpFind.Tlow: The lowest level at which to contour the data
*     array. Only accessed if no value is supplied for "Level1". See also
*     "DeltaT". [2*RMS]

*  Reinhold Configuration Parameters:
*     The Reinhold algorithm uses the following configuration parameters. 
*     Values for these parameters can be specified using the CONFIG parameter. 
*     Default values are shown in square brackets:
*
*     - Reinhold.MinLen: The minimum number of pixels spanned by a peak
*     along any one dimension in order for the peak to be considered
*     significant. If a peak is spanned by fewer than this number of pixels 
*     on any axis, then it is ignored. [4]
*     - Reinhold.MinPix: The lowest number of pixel which a clump can
*     contain. If a candidate clump has fewer than this number of pixels, 
*     it will be ignored. This prevents noise spikes from being interpreted 
*     as real clumps. [16]
*     - Reinhold.Noise: Defines the data value below which pixels are 
*     considered to be in the noise. A peak is considered to end when the 
*     peak value dips below the "noise" value. [2*RMS]
*     - Reinhold.Thresh: The smallest significant peak height. Peaks which 
*     have a maximum data value less than this value are ignored. ["noise"+RMS]
*     - Reinhold.FlatSlope: A peak is considered to end when the slope of a
*     profile through the peak drops below this value. The value should be 
*     given as a change in data value between adjacent pixels. [1.0*RMS]
*     - Reinhold.CAThresh: Controls the operation of the cellular automata 
*     which is used to erode the (previously dilated) edges regions prior to 
*     filling them with clump indicies. If the number of edge pixels in
*     the 3x3x3 pixel cube (or 2x2 pixel square for 2D data) surrounding 
*     any pixel is greater than CAThresh, then the central pixel is
*     considered to be an edge pixel. Otherwise it is not considered to be 
*     an edge pixel. The default value is one less than the total number
*     of pixels in the square or cube (i.e. 8 for 2D data and 26 for 3D 
*     data). []
*     - Reinhold.CAIterations: This gives the number of times to apply
*     the cellular automata which is used to erode the edges regions prior
*     to filling them with clump indicies. [1]
*     - Reinhold.FixClumpsIterations: This gives the number of times to
*     apply the cellular automata which cleans up the filled clumps. This
*     cellular automata replaces each output pixel by the most commonly 
*     occuring value within a 3x3x3 cube (or 2x2 square for 2D data) of input 
*     pixels centred on the output pixel. [1]

*  FellWalker Configuration Parameters:
*     The FellWalker algorithm uses the following configuration parameters. 
*     Values for these parameters can be specified using the CONFIG parameter. 
*     Default values are shown in square brackets:
*
*     - FellWalker.CleanIter: This gives the number of times to apply the 
*     cellular automata which cleans up the filled clumps. This cellular 
*     automata replaces each clump index by the most commonly occuring 
*     value within a 3x3x3 cube (or 2x2 square for 2D data) of neighbours. [3]
*     - FellWalker.FlatSlope: Any initial section to a walk which has an
*     average gradient (measured over 4 steps) less than this value will not 
*     be included in the clump. [1.0*RMS]
*     - FellWalker.MinPix: The lowest number of pixel which a clump can
*     contain. If a candidate clump has fewer than this number of pixels, 
*     it will be ignored. This prevents noise spikes from being interpreted 
*     as real clumps. [4]
*     - FellWalker.MaxJump: Defines the extent of the neighbourhood about a
*     local maximum which is checked for higher pixel values. The
*     neighbourhood checked is  square or cube with side equal to twice the
*     supplied value, in pixels. [4]
*     - FellWalker.Noise: Defines the data value below which pixels are 
*     considered to be in the noise. No walk will start from a pixel with 
*     data value less than this value. [2*RMS]

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     28-SEP-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/      

/* Local Variables: */
   AstFrameSet *iwcs;           /* Pointer to the WCS FrameSet */
   AstKeyMap *config;           /* Pointer to KeyMap holding used config settings */
   AstKeyMap *config2;          /* Pointer to KeyMap holding used config settings */
   AstKeyMap *keymap;           /* Pointer to KeyMap holding all config settings */
   AstMapping *map;             /* Current->base Mapping from WCS FrameSet */
   AstMapping *tmap;            /* Unused Mapping */
   Grp *grp;                    /* GRP identifier for configuration settings */
   int *clist;                  /* List of NDF identifiers for individual clump */
   HDSLoc *xloc;                /* HDS locator for CUPID extension */
   IRQLocs *qlocs;              /* HDS locators for quality name information */
   char *value;                 /* Pointer to GRP element buffer */
   char attr[ 30 ];             /* AST attribute name */
   char buffer[ GRP__SZNAM ];   /* Buffer for GRP element */
   char dtype[ 20 ];            /* NDF data type */
   char itype[ 20 ];            /* NDF data type */
   char method[ 15 ];           /* Algorithm string supplied by user */
   const char *lab;             /* AST Label attribute for an axis */
   const char *sys;             /* AST System attribute for an axis */
   double *ipv;                 /* Pointer to Variance array */
   double bg;                   /* Background level */
   double rms;                  /* Global rms error in data */
   double sum;                  /* Sum of variances */
   float *rmask;                /* Pointer to cump mask array */
   int dim[ NDF__MXDIM ];       /* Pixel axis dimensions */
   int el;                      /* Number of array elements mapped */
   int i;                       /* Loop count */
   int ifr;                     /* Index of Frame within WCS FrameSet */
   int ilevel;                  /* Interaction level */
   int indf2;                   /* Identifier for output NDF */
   int indf;                    /* Identifier for input NDF */
   int mask;                    /* Write a mask to the supplied NDF? */
   int n;                       /* Number of values summed in "sum" */
   int nclump;                  /* Number of elements in "clist" */
   int ndim;                    /* Total number of pixel axes */
   int nfr;                     /* Number of Frames within WCS FrameSet */
   int nsig;                    /* Number of significant pixel axes */
   int repconf;                 /* Report configuration? */
   int sdim[ NDF__MXDIM ];      /* The indices of the significant pixel axes */
   int size;                    /* Size of the "grp" group */
   int slbnd[ NDF__MXDIM ];     /* The lower bounds of the significant pixel axes */
   int subnd[ NDF__MXDIM ];     /* The upper bounds of the significant pixel axes */
   int there;                   /* Extension exists? */
   int type;                    /* Integer identifier for data type */
   int var;                     /* Does the i/p NDF have a Variance component? */
   int vax;                     /* Index of the velocity WCS axis (if any) */
   int velax;                   /* Index of the velocity pixel axis (if any) */
   void *ipd;                   /* Pointer to Data array */
   void *ipo;                   /* Pointer to output Data array */
   
#if defined(FPTRAP)
   fptrap(1);
#endif

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Initialise things to safe values. */
   mask = 0;
   rmask = NULL;
   clist = NULL;
   xloc = NULL;

/* Start an AST context */
   astBegin;

/* Start an NDF context */
   ndfBegin();

/* Get an identifier for the input NDF. We use NDG (via kpg1_Rgndf)
   instead of calling ndfAssoc directly since NDF/HDS has problems with
   file names containing spaces, which NDG does not have. */
   kpg1Rgndf( "IN", 1, 1, "", &grp, &size, status );
   ndgNdfas( grp, 1, "UPDATE", &indf, status );
   grpDelet( &grp, status );

/* Get the dimensions of the NDF, and count the significant ones. */
   ndfDim( indf, NDF__MXDIM, dim, &ndim, status );
   nsig = 0;
   for( i = 0; i < ndim; i++ ) {
      if( dim[ i ] > 1 ) nsig++;
   }

/* Abort if the NDF is not 1-, 2- or 3- dimensional. */
   if( nsig > 3 ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         ndfMsg( "NDF", indf );
         msgSeti( "NSIG", nsig );
         errRep( "CLUMPS_ERR2", "\"^NDF\" has ^NSIG significant "
                 "pixel axes", status );
         errRep( "CLUMPS_ERR4", "This application requires 1, 2 or 3 "
                 "significant pixel axes", status );
      }
      goto L999;
   }          

/* Get the WCS FrameSet and the significant axis bounds. */
   kpg1Asget( indf, nsig, 1, 0, 0, sdim, slbnd, subnd, &iwcs, status );

/* If the NDF has 3 pixel axes, identify the velocity axis. */
   if( nsig == 3 && astGetI( iwcs, "Naxes" ) == 3 ) {

/* Search all Frames in the FrameSet, starting with the current Frame. */
      vax = 0;
      nfr = astGetI( iwcs, "Nframe" );
      for( ifr = 0; ifr <= nfr && vax == 0; ifr++ ) {
         if( ifr > 0 ) astSetI( iwcs, "Current", ifr );

/* Check the AST System attribute associated with each axis of the
   current WCS Frame, looking for an axis with a known velocity system. 
   Note the one-based index of the axis when and if found. */
         for( i = 1; i <= 3; i++ ) {
            sprintf(attr, "System(%d)", i );
            sys = astGetC( iwcs, attr );
            if( sys ) {
               if( !strcmp( "VRAD", sys ) || 
                   !strcmp( "VOPT", sys ) ||
                   !strcmp( "VELO", sys ) ) {
                  vax = i;
                  break;
               }
            }
         }

/* If no SpecFrame was found, look for an axis labelcontaining a letter
   "V". */
         if( vax == 0 ) {
            for( i = 1; i <= 3; i++ ) {
               sprintf(attr, "Label(%d)", i );
               lab = astGetC( iwcs, attr );
               if( lab && strpbrk( lab, "Vv" ) ){
                  vax = i;
                  break;
               }
            }
         }
      }

/* Identify the pixel axis corresponding to the velocity WCS axis.
   astMapSplit uses one-based axis indices, so we need to convert to and
   from zero-based for further use. */
      velax = -1;
      if( vax != 0 ) {
         map = astGetMapping( iwcs, AST__CURRENT, AST__BASE );
         astMapSplit( map, 1, &vax, &velax, &tmap );
         if( tmap ) {
            velax--;
         } else {
            velax = -1;
         }         
      }

/* Issue a warnining if no velocity axis was found, and use pixel axis 3. */
      if( velax == -1 ) {
         velax = 2;
         msgOut( "", "WARNING: Cannot identify a velocity axis within the "
                 "supplied NDF. Assuming pixel axis 3 is the velocity axis.", 
                 status );
      }
   }         

/* Choose the data type to use when mapping the NDF Data array. */
   ndfMtype( "_REAL,_DOUBLE", indf, indf, "DATA", itype, 20, dtype, 20,
             status );
   if( !strcmp( itype, "_DOUBLE" ) ) {
      type = CUPID__DOUBLE;
   } else {
      type = CUPID__FLOAT;
   }

/* Map the Data array. */
   ndfMap( indf, "DATA", itype, "READ", &ipd, &el, status );

/* Get the interaction level. */
   parGdr0i( "ILEVEL", 1, 0, 6, 1, &ilevel, status );
   if( ilevel > 0 ) msgBlank( status );

/* Calculate the default RMS value. If the NDF has a Variance component
   it is the square root of the mean Variance value. Otherwise, it is found
   by looking at differences between adjacent pixel values in the Data 
   component. */
   ndfState( indf, "VARIANCE", &var, status );
   if( *status == SAI__OK && var ) {   
      ndfMap( indf, "VARIANCE", "_DOUBLE", "READ", (void *) &ipv, &el, status );

      sum = 0.0;
      n = 0;
      for( i = 0; i < el; i++ ) {
         if( ipv[ i ] != VAL__BADD ) {
            sum += ipv[ i ];
            n++;
         }
      }
      
      if( n > 0 ) {
         rms = sqrtf( sum/n );

      } else {
         *status = SAI__ERROR;
         errRep( "CLUMPS_ERR1", "The supplied data contains insufficient "
                 "good Variance values to continue.", status );
      }         

   } else {
      ipv = NULL;
      rms = cupidRms( type, ipd, el, subnd[ 0 ] - slbnd[ 0 ] + 1 );
   }   


/* Get the RMS noise level. */
   parDef0d( "RMS", rms, status );
   parGet0d( "RMS", &rms, status );

/* Determine which algorithm to use. */
   parChoic( "METHOD", "GAUSSCLUMPS", "GAUSSCLUMPS,CLUMPFIND,REINHOLD,"
             "FELLWALKER", 1, method, 15,  status );

/* Abort if an error has occurred. */
   if( *status != SAI__OK ) goto L999;

/* Read a group of configuration setting. */
   grp = NULL;
   kpg1Gtgrp( "CONFIG", &grp, &size, status );

/* If no group was supplied, annul the error and create an empty KeyMap. */
   if( *status == PAR__NULL || size == 0 ) {
      if( *status != SAI__OK ) errAnnul( status );
      keymap = astKeyMap( "" );

/* If a group was supplied, see if it consists of the single value "def".
   If so, create an empty KeyMap. */
   } else {
      keymap = NULL;
      if( size == 1 ) {
         value = buffer;
         grpGet( grp, 1, 1, &value, GRP__SZNAM, status );
         if( astChrMatch( value, "DEF" ) ) {
            keymap = astKeyMap( "" );
         }
      }

/* Otherwise, create an AST KeyMap holding the value for each configuration 
   setting, indexed using its name, display the config file if needed. */
      if( !keymap ) kpg1Kymap( grp, &keymap, status );
   }

/* Delete the group, if any. */
   if( grp ) grpDelet( &grp, status );      

/* Switch for each method */
   if( !strcmp( method, "GAUSSCLUMPS" ) ) {
      clist = cupidGaussClumps( type, nsig, slbnd, subnd, ipd, ipv, rms, 
                                keymap, velax, ilevel, &nclump ); 

   } else if( !strcmp( method, "CLUMPFIND" ) ) {
      clist = cupidClumpFind( type, nsig, slbnd, subnd, ipd, ipv, rms,
                              keymap, velax, ilevel, &nclump ); 
      
   } else if( !strcmp( method, "REINHOLD" ) ) {
      clist = cupidReinhold( type, nsig, slbnd, subnd, ipd, ipv, rms,
                              keymap, velax, ilevel, &nclump ); 
      
   } else if( !strcmp( method, "FELLWALKER" ) ) {
      clist = cupidFellWalker( type, nsig, slbnd, subnd, ipd, ipv, rms,
                              keymap, velax, ilevel, &nclump ); 
      
   } else if( *status == SAI__OK ) {
      msgSetc( "METH", method );
      errRep( "CLUMPS_ERR1", "Requested Method ^METH has not yet been "
              "implemented.", status );
   }

/* Report the configuration (if any). */
   errBegin( status );
   parGet0l( "REPCONF", &repconf, status );
   if( repconf ) {

/* Create a GRP group containing the required text. */
      grp = NULL;
      kpg1Kygrp( keymap, &grp, status );
      grpGrpsz( grp, &size, status );

      msgBlank( status );
      msgOut( "", "Current configuration:", status );
      value = buffer;
      for( i = 1; i <= size; i++ ) {
         grpGet( grp, i, 1, &value, GRP__SZNAM, status );
         msgSetc( "V", value );
         msgOut( "", "   ^V", status );
      }
      msgBlank( status );

      if( grp ) grpDelet( &grp, status );      

   }    
   errEnd( status );

/* Skip the rest if no clumps were found. */
   if( nclump ) {

/* See if an output NDF is to be created. If not, annull the error. If so,
   map the data array. */
      ipo = NULL;
      ndfProp( indf, "AXIS,WCS,QUALITY", "OUT", &indf2, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );

      } else if( method ){
         if( !strcmp( method, "GAUSSCLUMPS" ) ) {
            ndfMap( indf2, "DATA", itype, "WRITE", &ipo, &el, status );
            ndfSbad( 1, indf2, "DATA", status );

         } else if( !strcmp( method, "CLUMPFIND" ) ||
                    !strcmp( method, "REINHOLD" ) ||
                    !strcmp( method, "FELLWALKER" ) ) {
            ndfStype( "_INTEGER", indf2, "DATA", status );
            ndfMap( indf2, "DATA", "_INTEGER", "WRITE", &ipo, &el, status );
            ndfSbad( 1, indf2, "DATA", status );

         } else {
            ndfDelet( &indf2, status );
         }
      }

/* If required allocate room for a mask holding bad values for points which 
   are not inside any clump. */
      parGet0l( "MASK", &mask, status );
      if( *status == SAI__OK && mask ) {
         rmask = astMalloc( sizeof( float )*(size_t) el );
      } else {
         mask = 0;
      }

/* Create any output NDF by summing the contents of the NDFs describing the 
   found clumps, and then adding on a background level (for GaussClumps). This 
   also fills the above mask array if required. */
      cupidSumClumps( type, ipd, ilevel, nsig, slbnd, subnd, el, clist, 
                         nclump, rmask, ipo, method, &bg );

/* Create an CUPID extension in the NDF if none already exists, or get a
   locator for the existing extension otherwise. */
      there = 0;
      ndfXstat( indf, "CUPID", &there, status );
      if( there ) {
         ndfXloc( indf, "CUPID", "UPDATE", &xloc, status );  
      } else {
         ndfXnew( indf, "CUPID", "CUPID_EXT", 0, NULL, &xloc, status );
      }

/* If a quality mask is being added to the input NDF... */
      if( mask ) {

/* Delete any existing quality name information from the supplied NDF, and 
   create a structure to hold new quality name info. */
         irqDelet( indf, status ); 
         irqNew( indf, "CUPID", &qlocs, status );

/* Add in two quality names; "CLUMP"and "BACKGROUND". */
         irqAddqn( qlocs, "CLUMP", 0, "set iff a pixel is within a clump", 
                   status );
         irqAddqn( qlocs, "BACKGROUND", 0, "set iff a pixel is not within a clump", 
                   status );

/* Transfer the pixel mask to the NDF quality array. */
         irqSetqm( qlocs, 1, "BACKGROUND", el, rmask, &n, status );
         irqSetqm( qlocs, 0, "CLUMP", el, rmask, &n, status );
      }

/* Store the configuration parameters relating to the used algorithm in the 
   CUPID extension. We put them into a new KeyMap so that the CUPID NDF
   extension gets names of the form "method.name" rather than just "name". */
      if( astMapGet0A( keymap, method, &config ) ) {     
         config2 = astKeyMap( "" );
         astMapPut0A( config2, method, config, NULL );
         cupidStoreConfig( xloc, config2 );
         astAnnul( config2 );
         astAnnul( config );
      }

/* Store the clump properties in the CUPID extension and output catalogue
   (if needed). */
      cupidStoreClumps( "OUTCAT", xloc, clist, nclump, nsig, 
                        "Output from CUPID:CLUMPS" );

/* Release the quality name information. */
      if( mask ) {
         rmask = astFree( rmask );
         irqRlse( &qlocs, status );
      }

/* Relase the extension locator.*/
      datAnnul( &xloc, status );
   }

/* Tidy up */
L999:

/* Release the memory containing the list of NDF identifiers describing the 
   clumps. The actual identifiers should already have been freed in
   cupidStoreClumps. */
   clist = astFree( clist );

/* If an error has occurred, delete the Quality component if a mask was
   being created, and also delete the CUPID extension. */
   if( *status != SAI__OK ) {
      errBegin( status );
      if( mask ) ndfReset( indf, "QUALITY", status );
      ndfXdel( indf, "CUPID", status );
      errEnd( status );
   }

/* End the NDF context */
   ndfEnd( status );

/* End the AST context */
   astEnd;

/* If an error has occurred, issue another error report identifying the 
   program which has failed (i.e. this one). */
   if( *status != SAI__OK ) {
      errRep( "CLUMPS_ERR", "CLUMPS: Failed to identify clumps of emission "
              "within a 1, 2 or 3-D NDF.", status );
   }

}

