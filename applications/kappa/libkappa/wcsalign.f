      SUBROUTINE WCSALIGN( STATUS )
*+
*  Name:
*     WCSALIGN

*  Purpose:
*     Aligns a group of NDFs using World Co-ordinate System information.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL WCSALIGN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application resamples a group of input NDFs, producing 
*     corresponding output NDFs which are aligned pixel-for-pixel
*     with a specified reference NDF. 
*
*     The transformations needed to produce alignment are derived from the 
*     co-ordinate system information stored in the WCS components of the
*     supplied NDFs. For each input NDF, alignment is first attempted in 
*     the current co-ordinate Frame of the reference NDF. If this fails,
*     alignment is attempted in the current co-ordinate Frame of the input
*     NDF. If this fails, alignment occurs in the pixel co-ordinate Frame.
*     A message indicating which Frame alignment was achieved in is
*     displayed.
*     
*     The output pixel values are formed by re-sampling the input pixel
*     values using nearest neighbour, bi-linear, sinc, sincsinc, sinccos,
*     or sincgauss interpolation (see parameter METHOD). 
*
*     Two methods exist for determining the bounds of the output NDFs.
*     Firstly, the user can give values for parameters LBND and UBND
*     which are then used as the pixel index bounds for all output
*     NDFs. Secondly, if a null value is given for LBND or UBND,
*     default values are generated separately for each output NDF so
*     that the output NDF just encloses the entire area covered by the
*     corresponding input NDF. Using the first method will ensure that
*     all output NDFs have the same pixel origin, and so the resulting
*     NDFs can be directly compared. However, this may result in the
*     output NDFs being larger than necessary. In general, the second
*     method results in smaller NDFs being produced, in less time.
*     However, the output NDFs will have differing pixel origins which
*     need to be taken into account when comparing the aligned NDFs.

*  Usage:
*     wcsalign in out lbnd ubnd ref 

*  ADAM Parameters:
*     ACC = _REAL (Read)
*        The positional accuracy required, as a a number of pixels. For
*        highly non-linear projections, a recursive algorithm is used in
*        which successively smaller regions of the projection are fitted 
*        with a least squares linear transformation. If such a transformation 
*        results in a maximum positional error greater than the value 
*        supplied for ACC (in pixels), then a smaller region is used. High 
*        accuracy is paid for by larger run times. [0.5]
*     IN = NDF (Read)
*        A group of input NDFs (of any dimensionality). This should be given 
*        as  a comma separated list, in which each list element can be:
*
*        - an NDF name, optionally containing wild-cards and/or regular 
*        expressions ("*", "?", "[a-z]" etc.). 
*
*        - the name of a text file, preceded by an up-arrow character "^".
*        Each line in the text file should contain a comma separated list
*        of elements, each of which can in turn be an NDF name (with
*        optional wild-cards, etc), or another file specification
*        (preceded by an up-arrow). Comments can be included in the file 
*        by commencing lines with a hash character "#".
*
*        If the value supplied for this parameter ends with a minus
*        sign "-", then the user is re-prompted for further input until
*        a value is given which does not end with a minus sign. All the
*        NDFs given in this way are concatenated into a single group.
*     LBND() = _INTEGER (Read)
*        An array of values giving the lower pixel index bound on each axis 
*        for the output NDFs. The number of values supplied should equal
*        the number of axes in the reference NDF. The given values are used 
*        for all output NDFs.  If a null value (!) is given for this parameter 
*        or for parameter UBND, then separate default values are calculated for
*        each output NDF which result in the output NDF just encompassing 
*        the corresponding input NDF. The suggested defaults are the 
*        lower pixel index bounds from the reference NDF (see parameter REF).
*     MAXPIX = _INTEGER (Read)
*        A value which specifies an initial scale size in pixels for the
*        adaptive algorithm which approximates non-linear Mappings with
*        piece-wise linear transformations. If MAXPIX is larger than any
*        dimension of the region of the output grid being used, a first
*        attempt will be made to approximate the Mapping by a linear
*        transformation over the entire output region. If a smaller value
*        is used, the output region will first be divided into subregions
*        whose size does not exceed MAXPIX pixels in any dimension, and then
*        attempts will be made at approximation. [1000]
*     METHOD = LITERAL (Read)
*        The method to use when sampling the input pixel values, in
*        order to find the corresponding output pixel value. For details 
*        on these sub-pixel interpolation schemes, see the description of 
*        routine AST_RESAMPLEx in SUN/210. METHOD can take the following 
*        values:
*
*        - "Bilinear" -- the output pixel values are calculated by 
*        bi-linear interpolation among the four nearest pixels values 
*        in the input NDF. Produces smoother output NDFs than
*        the nearest neighbour scheme, but is marginally slower.
*
*        - "Nearest" -- the output pixel values are assigned the value 
*        of the single nearest input pixel.
*
*        - "Sinc" -- use the sinc(pi*x) kernel, where x is the pixel
*        offset from the interpolation point, and sinc(z)=sin(z)/z.
*        Use of this scheme is not recommended.
*
*        - "SincSinc" -- uses the sinc(pi*x)sinc(k*pi*x) kernel. A
*        valuable general-purpose interpolation scheme, intermediate
*        in its visual effect on NDFs between the bilinear and
*        nearest neighbour schemes. 
*         
*        - "SincCos" -- uses the sinc(pi*x)cos(k*pi*x) kernal. Gives
*        similar results to the sincsinc scheme.
*
*        - "SincGauss" -- uses the sinc(pi*x)exp(-k*x*x). Good 
*        results can be obtained by matching the FWHM of the
*        envelope function to the point spread function of the
*        input data (see parameter PARAMS).
*
*        All methods propagate variances from input to output, but the
*        variance estimates produced by interpolation schemes other than
*        nearest neighbour need to be treated with care since the spatial 
*        smoothing produced by this interpolation methods introduces 
*        correlations in the variance estimates. Also, the degree of 
*        smoothing produced varies across the NDF. This is because a 
*        sample taken at a pixel centre will have no contributions from the 
*        neighbouring pixels, whereas a sample taken at the corner of a 
*        pixel will have equal contributions from all four neighbouring 
*        pixels, resulting in greater smoothing and lower noise. This 
*        effect can produce complex Moire patterns in the output 
*        variance estimates, resulting from the interference of the 
*        spatial frequencies in the sample positions and in the pixel 
*        centre positions. For these reasons, if you want to use the 
*        output variances, you are generally safer using nearest neighbour
*        interpolation. [current value]
*     OUT = NDF (Write)
*        A group of output NDFs corresponding one-for-one with the list
*        of input NDFs given for parameter IN. This should be given as 
*        a comma separated list, in which each list element can be:
*        - an NDF name. If the name contains an asterisk character "*",
*        the name of the corresponding input NDF (without directory or
*        file suffix) is substituted for the asterisk (for instance, "*_al" 
*        causes the output NDF name to be formed by appending the string 
*        "_al" to the corresponding input NDF name). Input NDF names
*        can also be edited by including original and replacement strings 
*        between vertical bars after the NDF name (for instance,
*        *_al|b4|B1| causes any occurrence of the string "B4" in the input 
*        NDF name to be replaced by the string "B1" before appending the
*        string "_al" to the result).
*
*        - the name of a text file, preceded by an up-arrow character "^".
*        Each line in the text file should contain a comma separated list
*        of elements, each of which can in turn be an NDF name (with
*        optional editing, etc), or another file specification
*        (preceded by an up-arrow). Comments can be included in the file 
*        by commencing lines with a hash character "#".
*
*        If the value supplied for this parameter ends with a minus
*        sign "-", then the user is re-prompted for further input until
*        a value is given which does not end with a minus sign. All the
*        NDFs given in this way are concatenated into a single group.
*     PARAMS( 2 ) = _DOUBLE (Read)
*        An optional array which consists of additional parameters
*        required by the Sinc, SincSinc, SincCos, and SincGauss
*        interpolation schemes. 
*
*        PARAMS( 1 ) is required by all the above interpolation schemes.
*        It is used to specify how many pixels are to contribute to the 
*        interpolated result on either side of the interpolation point 
*        in each dimension. Typically, a value of 2 is appropriate and the 
*        minimum allowed value is 1 ( i.e. one pixel on each side ). A value 
*        of zero or less indicates that a suitable number of pixels should be
*        calculated automatically. [0]
*
*        PARAMS( 2 ) is required only by the SincSinc, SincCos, and 
*        SincGauss interpolation schemes. For the SincSinc and SincCos 
*        schemes, it specifies the number of pixels at which the envelope
*        of the function goes to zero. The minimum value is 1.0, and the
*        run-time default value is 2.0. For the SincGauss scheme, it
*        specifies the full-width at half-maximum (FWHM) of the Gaussian 
*        envelope. The minimum value is 0.1, and the run-time default is
*        1.0. On astronomical NDFs and spectra, good results are often 
*        obtained by approximately matching the FWHM of the envelope 
*        function, given by PARAMS(2), to the point spread function of the 
*        input data. []
*     REF = NDF (Read)
*        The NDF to which all the input NDFs are to be aligned. If a null 
*        value is supplied for this parameter, the first NDF supplied for 
*        parameter IN is used. 
*     UBND() = _INTEGER (Read)
*        An array of values giving the upper pixel index bound on each axis 
*        for the output NDFs. The number of values supplied should equal
*        the number of axes in the reference NDF. The given values are used 
*        for all output NDFs.  If a null value (!) is given for this parameter 
*        or for parameter LBND, then separate default values are calculated for
*        each output NDF which result in the output NDF just encompassing 
*        the corresponding input NDF. The suggested defaults are the 
*        upper pixel index bounds from the reference NDF (see parameter REF).

*  Examples:
*     wcsalign image1 image1_al ref=image2 accept
*        This example resamples the NDF called image1 so that it is aligned 
*        with the NDF call image2, putting the output in image1_al. The output 
*        image has the same pixel index bounds as image2 and inherits WCS 
*        information from image2.
*     wcsalign m51* *_al lbnd=! accept
*        This example resamples all the NDFs with names starting with the 
*        string "m51" in the current directory so that 
*        they are aligned with the first input NDF. The output NDFs
*        have the same names as the input NDFs, but extended with the
*        string "_al". Each output NDF is just big enough to contain all 
*        the pixels in the corresponding input NDF.
*     wcsalign ^in.lis ^out.lis lbnd=! accept
*        This example is like the previous example, except that the names
*        of the input NDFs are read from the text file in.lis, and the
*        names of the corresponding output NDFs are read from text file
*        out.lis.

*  Notes:
*     -  WCS information (including the current co-ordinate Frame) is 
*     propagated from the reference NDF to all output NDFs. 
*     -  QUALITY is propagated from input to output only if parameter
*     METHOD is set to Nearest.

*  Related Applications:
*     KAPPA: WCSFRAME, RESAMPLE; CCDPACK: TRANNDF.

*  Implementation Status:
*     -  This routine correctly processes the DATA, VARIANCE, LABEL, 
*     TITLE, UNITS, WCS and HISTORY components of the input NDFs (see the
*     METHOD parameter for notes on the interpretation of output variances).
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.

*  Authors:
*     DSB: David Berry (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1998 (DSB):
*        Original version, based on IRAS90:SKYALIGN. 
*     8-JUL-1999 (TDCA):
*        Modified to use AST_RESAMPLE
*     5-AUG-1999 (DSB):
*        Tidied up.
*     19-SEP-2001 (DSB):
*        Allow use with 1-dimensional NDFs by changing kpg1_asget EXACT
*        argument to .false.
*     31-OCT-2002 (DSB):
*        Make N-dimensional.
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
      INCLUDE 'AST_PAR'          ! AST constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER METHOD*13           ! Interpolation method to use.
      CHARACTER NDFNAM*(GRP__SZNAM) ! The name of an NDF.
      DOUBLE PRECISION PARAMS( 2 ) ! Param. values passed to AST_RESAMPLE<x>
      INTEGER I                  ! Index into input and output groups
      INTEGER IGRP1              ! GRP id. for group holding input NDFs
      INTEGER IGRP2              ! GRP id. for group holding output NDFs
      INTEGER INDF1              ! NDF id. for the input NDF
      INTEGER INDF2              ! NDF id. for the output NDF
      INTEGER INDFR              ! NDF id. for the reference NDF
      INTEGER IWCSR              ! WCS FrameSet for reference NDF
      INTEGER LBND( NDF__MXDIM ) ! Indices of lower left corner of outputs
      INTEGER MAXPIX             ! Initial scale size in pixels
      INTEGER METHOD_CODE        ! Integer corresponding to interp. method 
      INTEGER NDIMR              ! Number of pixel axes in reference NDF
      INTEGER NPAR               ! No. of required interpolation parameters
      INTEGER SIZE               ! Total size of the input group
      INTEGER SIZEO              ! Total size of the output group
      INTEGER LBNDR( NDF__MXDIM )! Lower pixel bounds of reference NDF
      INTEGER UBNDR( NDF__MXDIM )! Upper pixel bounds of reference NDF
      INTEGER UBND( NDF__MXDIM ) ! Indices of upper right corner of outputs
      REAL ERRLIM                ! Positional accuracy in pixels
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get a group containing the names of the NDFs to be processed.
      CALL KPG1_RGNDF( 'IN', 0, 1, '  Give more NDFs...', 
     :                 IGRP1, SIZE, STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

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

*  Get the associated WCS FrameSet. 
      CALL KPG1_GTWCS( INDFR, IWCSR, STATUS )

*  Get the dimensionality and pixel bounds of the reference NDF.
      CALL NDF_BOUND( INDFR, NDF__MXDIM, LBNDR, UBNDR, NDIMR, STATUS ) 

*  Set the suggested default for LBND and UBND.
      CALL PAR_DEF1I( 'LBND', NDIMR, LBNDR, STATUS )
      CALL PAR_DEF1I( 'UBND', NDIMR, UBNDR, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the bounds required for the output NDFs.
      CALL PAR_EXACI( 'LBND', NDIMR, LBND, STATUS )
      CALL PAR_EXACI( 'UBND', NDIMR, UBND, STATUS )

*  If a null value was supplied for LBND or UBND, annul the error and
*  put bad values in them.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         DO I = 1, NDIMR 
            LBND( I ) = VAL__BADI
            UBND( I ) = VAL__BADI
         END DO
      END IF

*  Get a group containing the names of the output NDFs.  Base
*  modification elements on the group containing the input NDFs.
      CALL KPG1_WGNDF( 'OUT', IGRP1, SIZE, SIZE,
     :                 '  Give more NDFs...',
     :                  IGRP2, SIZEO, STATUS )

*  Get the interpolation method to be used.
      CALL PAR_CHOIC( 'METHOD', 'SincSinc', 'Nearest,Bilinear,Sinc,'//
     :                'SincSinc,SincCos,SincGauss', .TRUE., METHOD, 
     :                STATUS )
      CALL MSG_BLANK( STATUS )

*  Tell the user what method is being used, and convert value of
*  METHOD to one of the values expected by AST_RESAMPLE<x>. 
      IF( METHOD( 1 : 1 ) .EQ. 'N' ) THEN
         METHOD_CODE = AST__NEAREST
         CALL MSG_OUT( 'WCSALIGN_MSG1', 
     :                 '  Using nearest neighbour interpolation.', 
     :                 STATUS ) 

      ELSE IF( METHOD( 1 : 1 ) .EQ. 'B' ) THEN
         METHOD_CODE = AST__LINEAR
         CALL MSG_OUT( 'WCSALIGN_MSG2', 
     :                 '  Using bi-linear interpolation.', STATUS ) 

      ELSE IF ( METHOD( 1 : 4 ) .EQ. 'SINC' ) THEN
         NPAR = 2
         PARAMS( 1 ) = 0.0
         PARAMS( 2 ) = 2.0

         IF ( METHOD( 5 : 5 ) .EQ. 'S' ) THEN
            METHOD_CODE = AST__SINCSINC
            CALL MSG_OUT( 'WCSALIGN_MSG3', 
     :                    '  Using sincsinc interpolation.', STATUS ) 

         ELSE IF( METHOD( 5 : 5 ) .EQ. 'C' ) THEN
            METHOD_CODE = AST__SINCCOS
            CALL MSG_OUT( 'WCSALIGN_MSG4', 
     :                    '  Using sinccos interpolation.', STATUS ) 

         ELSE IF( METHOD( 5 : 5 ) .EQ. 'G' ) THEN
            METHOD_CODE = AST__SINCGAUSS
            PARAMS( 2 ) = 1.0
            CALL MSG_OUT( 'WCSALIGN_MSG5', 
     :                    '  Using sincgauss interpolation.', STATUS ) 

         ELSE
            NPAR = 1
            METHOD_CODE = AST__SINC
            CALL MSG_OUT( 'WCSALIGN_MSG6', 
     :                    '  Using sinc interpolation.', STATUS ) 

         END IF

*  Set the dynamic defaults for PARAMS.
         CALL PAR_DEF1D( 'PARAMS', NPAR, PARAMS, STATUS )

*  Get the required number of interpolation parameters.
         CALL PAR_EXACD( 'PARAMS', NPAR, PARAMS, STATUS ) 

      END IF

*  Get the positional accuracy required.
      CALL PAR_GET0R( 'ACC', ERRLIM, STATUS )      
      ERRLIM = MAX( 0.0001, ERRLIM )

*  Get a value for MAXPIX.
      CALL PAR_GET0I( 'MAXPIX', MAXPIX, STATUS )
      MAXPIX = MAX( 1, MAXPIX )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each NDF to be processed.
      DO I = 1, SIZE
         CALL MSG_BLANK( STATUS )

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRP1, I, 'READ', INDF1, STATUS )

*  Tell the user which input NDF is currently being procesed.
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL MSG_OUT( 'WCSALIGN_MSG7', '  Processing ^NDF...', STATUS )

*  Create the output NDF by propagation from the input NDF. The default
*  components HISTORY, TITLE, LABEL and all extensions are propagated,
*  together with the UNITS component. The NDF is initially created with
*  the same bounds as the input NDF.
         CALL NDG_NDFPR( INDF1, 'UNITS', IGRP2, I, INDF2, STATUS )

*  Process this pair of input and output NDFs.
         CALL KPS1_WALA0( NDIMR, INDF1, INDF2, IWCSR, METHOD_CODE, 
     :                    PARAMS, LBND, UBND, ERRLIM, MAXPIX, STATUS )

*  Annul the input NDF identifier.
         CALL NDF_ANNUL( INDF1, STATUS )

*  If an error has occurred, delete the output NDF, otherwise just 
*  annul its identifier.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL NDF_DELET( INDF2, STATUS )
         ELSE
            CALL NDF_ANNUL( INDF2, STATUS )
         END IF

*  If an error occurred processing the current input NDF...
         IF( STATUS .NE. SAI__OK ) THEN

*  Flush the error.
            CALL ERR_FLUSH( STATUS )

*  Give a warning telling the user that no output NDF will be created 
*  for the current input NDF.
            CALL GRP_GET( IGRP2, I, 1, NDFNAM, STATUS )
            CALL MSG_SETC( 'NDF', NDFNAM )
            CALL MSG_OUT( 'WCSALIGN_MSG8', '  WARNING: ^NDF cannot be'//
     :                    ' produced', STATUS )
         END IF

*  Process the next input NDF.
      END DO

*  Display a blank line.
      CALL MSG_BLANK( STATUS )

*  Tidy up.
*  ========
 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Delete all groups.
      CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Add a context report if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'WCSALIGN_ERR', 'WCSALIGN: Failed to align a '//
     :                 'group of NDFs using WCS information.', STATUS )
      END IF

      END
