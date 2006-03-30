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
*     single output NDF.
*
*     The algorithm proceeds as follows.  First, the output NDF is
*     filled with zeros.  An associated array of weights (one weight for
*     each output pixel) is created and is also filled with zeros.  Each
*     input NDF is then processed in turn.  For each pixel in the
*     current input NDF, the corresponding transformed position in the
*     output NDF is found (based on the WCS information in both NDFs).
*     The input pixel value is then divided up between a small group of
*     output pixels centred on this central output position.  The method
*     used for choosing the fraction of the input pixel value assigned
*     to each output pixel is determined by the METHOD and PARAMS 
*     parameters.  Each of the affected output pixel values is then
*     incremented by its allocated fraction of the input pixel value. 
*     The corresponding weight values are incremented by the fractions 
*     used (that is, if 0.25 of an input pixel is assigned to an output 
*     pixel, the weight for the output pixel is incremented by 0.25).
*     Once all pixels in the current input NDF have been rebinned into
*     the output NDF in this way, the algorithm proceeds to rebin the
*     next input NDF in the same way.  Once all input NDFs have been
*     processed, output pixels which have a weight less than the value
*     given by parameter WLIM are set bad.  The mean value in the
*     weights array (excluding those values less than WLIM) is then
*     found.  The output NDF is then normalised by dividing it by the
*     weights array.  This normalisation of the output NDF takes account
*     of any difference in the number of pixels contributing to each
*     output pixel, and also removes artifacts which may be produced by
*     aliasing between the input and output pixel grids. 
*
*     If the input NDFs contain variances, then these are propagated to
*     the output.  Alternatively, output variances can be generated from
*     the spread of input values contributing to each output pixel (see
*     parameter GENVAR).
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
*     GENVAR = _LOGICAL (Read)
*        If TRUE, any input variances are ignored and output variances
*        are generated based on the spread of input pixel values
*        contributing to each output pixel.  If FALSE, the output 
*        variances are based on the variances in the input NDFs, and the
*        output NDF will contain variances only if all input NDFs
*        contain variances. [FALSE]
*     ILEVEL = _INTEGER (Read)
*        Controls the amount of information displayed on the screen. If
*        set to 1, no information will be displayed while the command
*        is executing. If set to 2, the interpolation method being used 
*        will be displayed. If set to 3, the name of each input NDF will
*        also be displayed as it is processed. [2]
*     IN = NDF (Read)
*        A group of input NDFs (of any dimensionality).  This should be
*        given as a comma-separated list, in which each list element
*        can be:
*
*        - an NDF name, optionally containing wild-cards and/or regular 
*        expressions ("*", "?", "[a-z]" etc.). 
*
*        - the name of a text file, preceded by an up-arrow character
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
*        these same defaults being used. [!]
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
*        between  the four nearest output pixels.  This produces smoother
*        output NDFs than the nearest-neighbour scheme, but is
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
*        input data (see parameter PARAMS).
*
*        - "Somb" -- Uses the somb(pi*x) kernel, where
*        somb(z)=2*J1(z)/z  (J1 is the first-order Bessel function of
*        the first kind).  This scheme is similar to the "Sinc" scheme.
*
*        - "SombCos" -- Uses the somb(pi*x)cos(k*pi*x) kernel.  This
*        scheme is similar to the "SincCos" scheme.
*
*        - "Gauss" -- Uses the exp(-k*x*x) kernel.  The FWHM of the
*        Gaussian is given by parameter PARAMS(2), and the point at
*        which to truncate the Gaussian to zero is given by parameter
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
*        nearest-neighbour interpolation. [current value]
*     OUT = NDF (Write)
*        The output NDF.
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
*        pixels should be calculated  automatically. [0]
*
*        PARAMS( 2 ) is required only by the Gauss, SombCos, SincSinc, 
*        SincCos, and SincGauss schemes. For the SombCos, SincSinc and
*        SincCos schemes, it specifies the number of output pixels at
*        which the envelope of the function goes to zero.  The minimum 
*        value is 1.0, and the run-time default value is 2.0.  For the 
*        Gauss and SincGauss scheme, it specifies the full-width at 
*        half-maximum (FWHM) of the Gaussian envelope.  The minimum 
*        value is 0.1, and the run-time default is 1.0. []
*     REF = NDF (Read)
*        The NDF to which all the input NDFs are to be aligned.  If a
*        null value is supplied for this parameter, the first NDF
*        supplied for parameter IN is used.
*     UBND() = _INTEGER (Read)
*        An array of values giving the upper pixel-index bound on each
*        axis for the output NDF.  The suggested default values just
*        encompass all the input data.  A null value (!) also results in
*        these same defaults being used. [!]
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
*     propagated from the reference NDF to the output NDF. 
*     -  QUALITY is not propagated from input to output.

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

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-SEP-2005 (DSB):
*        Original version, based on WCSALIGN.
*     30-MAR-2006 (DSB):
*        Added ILEVEL.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'CNF_PAR'          ! CNF constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DTYPE*(NDF__SZFTP) ! Data type
      CHARACTER MESS*60          ! Message text
      CHARACTER METHOD*13        ! Interpolation method to use.
      CHARACTER TY_IN*(NDF__SZTYP) ! Numeric type for processing
      DOUBLE PRECISION PARAMS( 2 ) ! Param. values passed to 
                                 ! AST_RESAMPLE<x>
      INTEGER DLBND( NDF__MXDIM )! Defaults for LBND
      INTEGER DUBND( NDF__MXDIM )! Defaults for UBND
      INTEGER EL                 ! Number of array elements mapped
      INTEGER FLAGS              ! Flags for AST_REBINSEQ
      INTEGER I                  ! Index into input and output groups
      INTEGER IGRP1              ! GRP id. for group holding input NDFs
      INTEGER ILEVEL             ! Information level
      INTEGER INDF1              ! NDF id. for the input NDF
      INTEGER INDF2              ! NDF id. for the output NDF
      INTEGER INDFR              ! NDF id. for the reference NDF
      INTEGER IPD1               ! Pointer to input data array
      INTEGER IPD2               ! Pointer to output data array
      INTEGER IPMAP              ! Pointer to array of pix_in->pix_out
                                 ! Mappings
      INTEGER IPV1               ! Pointer to input variance array
      INTEGER IPV2               ! Pointer to output variance array
      INTEGER IPW                ! Pointer to work array
      INTEGER LBND( NDF__MXDIM ) ! Indices of lower-left corner of
                                 ! output
      INTEGER LBND1( NDF__MXDIM )! Indices of lower-left corner of input
      INTEGER MAP                ! AST id for (pix_in->pix_out) Mapping
      INTEGER MAXPIX             ! Initial scale size in pixels
      INTEGER METHOD_CODE        ! Integer corresponding to spreading 
                                 ! method 
      INTEGER NDIM               ! Number of pixel axes in output NDF
      INTEGER NDIM1              ! Number of pixel axes in input NDF
      INTEGER NPAR               ! No. of required interpolation
                                 ! parameters
      INTEGER SIZE               ! Total size of the input group
      INTEGER UBND( NDF__MXDIM ) ! Indices of upper-right corner of
                                 ! output
      INTEGER UBND1( NDF__MXDIM )! Indices of upper-right corner of 
                                 ! input
      LOGICAL BAD_DV             ! Any bad data/variance values in
                                 ! input?
      LOGICAL GENVAR             ! Use input spread to create output
                                 ! variance?
      LOGICAL USEVAR             ! Use input variances to create output
                                 ! variance?
      REAL ERRLIM                ! Positional accuracy in pixels
      REAL WLIM                  ! Minimum good output weight
      REAL D,V
      REAL*8 W
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

*  Get the level of screen information to display.
      CALL PAR_GDR0I( 'ILEVEL', 2, 1, 3, .TRUE., ILEVEL, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the reference NDF.
      CALL LPG_ASSOC( 'REF', 'READ', INDFR, STATUS )

*  If a null value was supplied, annul the error and use the first NDF
*  supplied for IN.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL NDG_NDFAS( IGRP1, 1, 'READ', INDFR, STATUS )
      END IF

*  Get the number of pixel axes in the reference NDF.
      CALL NDF_BOUND( INDFR, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Extract required global information describing the group of input
*  NDF.  This includes the default values for LBND and UBND, and the
*  Mappings from the input PIXEL Frames to the output PIXEL Frame.
      CALL PSX_CALLOC( SIZE, '_INTEGER', IPMAP, STATUS )
      CALL KPS1_WMOS0( INDFR, IGRP1, NDIM, DLBND, DUBND, USEVAR,
     :                 %VAL( CNF_PVAL( IPMAP ) ), STATUS )

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

      IF( ILEVEL .GE. 2 ) THEN 
         CALL MSG_OUT( 'WCSMOSAIC_MSG1', MESS, STATUS )
      END IF

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

*  See if output variances are to be generated on the basis of the
*  spread of input data values.  If so, we do not use input variances.
      CALL PAR_GET0L( 'GENVAR', GENVAR, STATUS )
      IF( GENVAR ) USEVAR = .FALSE.

*  Create the output NDF by propagation from the reference NDF.  The
*  default components HISTORY, TITLE, LABEL and all extensions are
*  propagated, together with the UNITS and WCS component.  The NDF is
*  initially created with the same bounds as the reference NDF.
      CALL NDF_PROP( INDFR, 'WCS,UNITS', 'OUT', INDF2, STATUS )

*  Change the bounds of the output NDF to the required values.
      CALL NDF_SBND( NDIM, LBND, UBND, INDF2, STATUS )

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

*  Loop round each NDF to be processed.
      DO I = 1, SIZE

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRP1, I, 'Read', INDF1, STATUS )

*  If required, tell the user which input NDF is currently being
*  processed.
         IF( ILEVEL .GE. 3 ) THEN
            CALL MSG_BLANK( STATUS )
            CALL NDF_MSG( 'NDF', INDF1 )
            CALL MSG_OUT( 'WCSMOSAIC_MSG2', '  Processing ^NDF...', 
     :                    STATUS )
         END IF

*  Set the AST_REBINSEQ flags for this input.
         FLAGS = 0
         IF( I .EQ. 1 ) FLAGS = FLAGS + AST__REBININIT
         IF( I .EQ. SIZE ) FLAGS = FLAGS + AST__REBINEND
   
         IF( GENVAR ) THEN
            FLAGS = FLAGS + AST__GENVAR
         ELSE IF( USEVAR ) THEN
            FLAGS = FLAGS + AST__USEVAR
         END IF
   
         CALL NDF_BAD( INDF1, 'DATA,VARIANCE', .FALSE., BAD_DV, STATUS )
         IF( BAD_DV ) FLAGS = FLAGS + AST__USEBAD 

*  Get the pixel bounds of the input NDF.
         CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND1, UBND1, NDIM1, 
     :                   STATUS )

*  Map the required components of the input.
         CALL NDF_MAP( INDF1, 'DATA', TY_IN, 'READ', IPD1, EL, STATUS )
         IF ( USEVAR ) THEN
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
     :                          %VAL( CNF_PVAL( IPW ) ),
     :                          STATUS )

         ELSE IF ( TY_IN .EQ. '_REAL' ) THEN
            CALL AST_REBINSEQR( MAP, DBLE( WLIM ), NDIM1, LBND1, UBND1,
     :                          %VAL( CNF_PVAL( IPD1 ) ), 
     :                          %VAL( CNF_PVAL( IPV1 ) ), METHOD_CODE,
     :                          PARAMS, FLAGS, DBLE( ERRLIM ), MAXPIX, 
     :                          VAL__BADR, NDIM, LBND, UBND, LBND1, 
     :                          UBND1, %VAL( CNF_PVAL( IPD2 ) ), 
     :                          %VAL( CNF_PVAL( IPV2 ) ),
     :                          %VAL( CNF_PVAL( IPW ) ),
     :                          STATUS )
         
         ELSE IF ( TY_IN .EQ. '_DOUBLE' ) THEN
            CALL AST_REBINSEQD( MAP, DBLE( WLIM ), NDIM1, LBND1, UBND1,
     :                          %VAL( CNF_PVAL( IPD1 ) ), 
     :                          %VAL( CNF_PVAL( IPV1 ) ), METHOD_CODE,
     :                          PARAMS, FLAGS, DBLE( ERRLIM ), MAXPIX, 
     :                          VAL__BADD, NDIM, LBND, UBND, LBND1, 
     :                          UBND1, %VAL( CNF_PVAL( IPD2 ) ), 
     :                          %VAL( CNF_PVAL( IPV2 ) ),
     :                          %VAL( CNF_PVAL( IPW ) ),
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
      IF( ILEVEL .GE. 3 ) CALL MSG_BLANK( STATUS )

*  Set the bad pixel flags for the output DATA and VARIANCE arrays.
      CALL NDF_SBAD( .TRUE., INDF2, 'DATA', STATUS )
      CALL NDF_SBAD( .TRUE., INDF2, 'VARIANCE', STATUS )

*  Tidy up.
*  ========
 999  CONTINUE

*  Free resourcee.
      CALL GRP_DELET( IGRP1, STATUS )
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
