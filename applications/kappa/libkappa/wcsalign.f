      SUBROUTINE WCSALIGN( STATUS )
*+
*  Name:
*     WCSALIGN

*  Purpose:
*     Aligns a group of 2-dimensional NDFs using World Co-ordinate System 
*     information.

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
*     This application resamples a group of 2-dimensional input NDFs,
*     producing corresponding output NDFs which are aligned pixel-for-pixel
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
*     The output image values are formed by re-sampling the input image
*     values using either nearest neighbour or bi-linear interpolation
*     (see parameter METHOD). 
*
*     Two methods exist for determining the bounds of the output images.
*     Firstly, the user can give values for parameters LBND and UBND
*     which are then used as the pixel index bounds for all output
*     images. Secondly, if a null value is given for LBND or UBND,
*     default values are generated separately for each output image so
*     that the output image just encloses the entire area covered by the
*     corresponding input image. Using the first method will ensure that
*     all output images have the same pixel origin, and so the resulting
*     images can be directly compared. However, this may result in the
*     output images being larger than necessary. In general, the second
*     method results in smaller images being produced, in less time.
*     However, the output images will have differing pixel origins which
*     need to be taken into account when comparing the aligned images.

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
*        A group of 2-dimensional input images. This should be given as 
*        a comma separated list, in which each list element can be:
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
*        images given in this way are concatenated into a single group.
*     LBND(2) = _INTEGER (Read)
*        A pair of values giving the lower pixel index bound on each axis 
*        for the output images. The given values are used for all output 
*        images.  If a null value (!) is given for this parameter or for 
*        parameter UBND, then separate default values are calculated for 
*        each output image which result in the output image just encompassing 
*        the corresponding input image. The suggested defaults are the 
*        lower pixel index bounds from the reference image (see parameter REF).
*     METHOD = LITERAL (Read)
*        The method to use when sampling the input pixel values, in
*        order to find the corresponding output pixel value. It can
*        take the following values:
*        - "Bilinear" -- the output pixel values are calculated by 
*        bi-linear interpolation among the four nearest pixels values 
*        in the input image.
*
*        - "Nearest" -- the output pixel values are assigned the value 
*        of the single nearest input pixel.
*
*        Bi-linear interpolation generally produces smoother output
*        images with lower noise, but is marginally slower.
*
*        Both methods propagate variances from input to output, but the
*        variance estimates produced using bi-linear interpolation need
*        to be treated with care since the spatial smoothing produced by 
*        this interpolation method introduces correlations in the
*        variances estimates. Also, the degree of smoothing produced varies
*        across the image. This is because a sample taken at a pixel centre 
*        will have no contributions from the neighbouring pixels, whereas a 
*        sample taken at the corner of a pixel will have equal contributions
*        from all four neighbouring pixels, resulting in greater smoothing 
*        and lower noise. This effect can produce complex Moire patterns
*        in the output variance estimates, resulting from the
*        interference of the spatial frequencies in the sample positions 
*        and in the pixel centre positions. For these reasons, if you
*        want to use the output variances, you are generally safer using 
*        nearest neighbour interpolation. [current value]
*     OUT = NDF (Write)
*        A group of output NDFs corresponding one-for-one with the list
*        of input NDFs given for parameter IN. This should be given as 
*        a comma separated list, in which each list element can be:
*        - an NDF name. If the name contains an asterisk character "*",
*        the name of the corresponding input image (without directory or
*        file suffix) is substituted for the asterisk (for instance, "*_al" 
*        causes the output image name to be formed by appending the string 
*        "_al" to the corresponding input image name). Input image names
*        can also be edited by including original and replacement strings 
*        between vertical bars after the NDF name (for instance,
*        *_al|b4|B1| causes any occurrence of the string "B4" in the input 
*        image name to be replaced by the string "B1" before appending the
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
*        images given in this way are concatenated into a single group.
*     REF = NDF (Read)
*        A 2-dimensional NDF containing the image to which all the input
*        images are to be aligned. If a null value is supplied for this 
*        parameter, the first image supplied for parameter IN is used. 
*     UBND(2) = _INTEGER (Read)
*        A pair of values giving the upper pixel index bound on each axis 
*        for the output images. The given values are used for all output 
*        images.  If a null value (!) is given for this parameter or for 
*        parameter UBND, then separate default values are calculated for 
*        each output image which result in the output image just encompassing 
*        the corresponding input image. The suggested defaults are the 
*        lower pixel index bounds from the reference image (see parameter REF).

*  Examples:
*     wcsalign image1 image1_al ref=image2 accept
*        This example resamples the 2-dimensional NDF called image1 so that
*        it is aligned with the 2-dimensional NDF call image2, putting the
*        output in image1_al. The output image has the same pixel index
*        bounds as image2 and inherits WCS information from image2.
*     wcsalign m51* *_al lbnd=! accept
*        This example resamples all the 2-dimensional NDFs with names 
*        starting with the string "m51" in the current directory so that 
*        they are aligned with the first input NDF. The output images
*        have the same names as the input images, but extended with the
*        string "_al". Each output image is just big enough to contain all 
*        the pixels in the corresponding input image.
*     wcsalign ^in.lis ^out.lis lbnd=! accept
*        This example is like the previous example, except that the names
*        of the input images are read from the text file in.lis, and the
*        names of the corresponding output images are read from text file
*        out.lis.

*  Notes:
*     -  WCS information (including the current co-ordinate Frame) is 
*     propagated from the reference NDF to all output NDFs. 
*     -  QUALITY is propagated from input to output only if parameter
*     METHOD is set to Nearest.

*  Related Applications:
*     KAPPA: WCSFRAME, TRANMAKE, TRANSFORMER; CCDPACK: TRANNDF

*  Implementation Status:
*     -  Only real data can be processed directly.  Other non-complex
*     numeric data types will undergo a type conversion before the
*     resampling takes place.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1998 (DSB):
*        Original version, based on IRAS90:SKYALIGN. 
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

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER METHOD*8            ! Interpolation method to use.
      CHARACTER NDFNAM*(GRP__SZNAM) ! The name of an NDF.
      INTEGER I                  ! Index into input and output groups
      INTEGER IGRP1              ! GRP id. for group holding input NDFs
      INTEGER IGRP2              ! GRP id. for group holding output NDFs
      INTEGER INDF1              ! NDF id. for the input NDF
      INTEGER INDF2              ! NDF id. for the output NDF
      INTEGER INDFR              ! NDF id. for the reference NDF
      INTEGER IWCSR              ! WCS FrameSet for reference image
      INTEGER LBND( 2 )          ! Indices of lower left corner of outputs
      INTEGER SDIM( 2 )          ! Indices of significant axis
      INTEGER SIZE               ! Total size of the input group
      INTEGER SIZEO              ! Total size of the output group
      INTEGER SLBND( 2 )         ! Lower pixel bounds on significant axis
      INTEGER SUBND( 2 )         ! Upper pixel bounds on significant axis
      INTEGER UBND( 2 )          ! Indices of upper right corner of outputs
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

*  Get the reference image.
      CALL NDF_ASSOC( 'REF', 'READ', INDFR, STATUS )

*  If a null value was supplied, annul the error and use the first NDF
*  supplied for IN.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL NDG_NDFAS( IGRP1, 1, 'READ', INDFR, STATUS )
      END IF

*  Get the associated WCS FrameSet. Report an error if the NDF is not
*  2-dimensional.
      CALL KPG1_ASGET( INDFR, 2, .TRUE., .FALSE., .TRUE., SDIM, 
     :                 SLBND, SUBND, IWCSR, STATUS )

*  Set the suggested default for LBND and UBND.
      CALL PAR_DEF1I( 'LBND', 2, SLBND, STATUS )
      CALL PAR_DEF1I( 'UBND', 2, SUBND, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the bounds required for the output images.
      CALL PAR_EXACI( 'LBND', 2, LBND, STATUS )
      CALL PAR_EXACI( 'UBND', 2, UBND, STATUS )

*  If a null value was supplied for LBND or UBND, annul the error and
*  put bad values in them.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         LBND( 1 ) = VAL__BADI
         LBND( 2 ) = VAL__BADI
         UBND( 1 ) = VAL__BADI
         UBND( 2 ) = VAL__BADI
      END IF

*  Get a group containing the names of the output NDFs.  Base
*  modification elements on the group containing the input NDFs.
      CALL KPG1_WGNDF( 'OUT', IGRP1, SIZE, SIZE,
     :                 '  Give more NDFs...',
     :                  IGRP2, SIZEO, STATUS )

*  Get the interpolation method to be used.
      CALL PAR_CHOIC( 'METHOD', 'Nearest',  'Nearest,Bilinear', .TRUE.,
     :                METHOD, STATUS )

*  Tell the user what method is being used.
      IF( METHOD( 1 : 1 ) .EQ. 'N' ) THEN
         CALL MSG_SETC( 'METH', 'nearest neighbour' )
      ELSE
         CALL MSG_SETC( 'METH', 'bi-linear' )
      END IF
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( 'WCSALIGN_MSG0', '  Using ^METH interpolation.',
     :              STATUS )    

*  Get the positional accuracy required.
      CALL PAR_GET0R( 'ACC', ERRLIM, STATUS )      
      ERRLIM = MAX( 0.0001, ERRLIM )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each NDF to be processed.
      DO I = 1, SIZE
         CALL MSG_BLANK( STATUS )

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRP1, I, 'READ', INDF1, STATUS )

*  Tell the user which input NDF is currently being procesed.
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL MSG_OUT( 'WCSALIGN_MSG1', '  Processing ^NDF...', STATUS )

*  Create the output NDF by propagation from the input NDF. The default
*  components HISTORY, TITLE, LABEL and all extensions are propagated,
*  together with the UNITS component. The NDF is initially created with
*  the same bounds as the input NDF.
         CALL NDG_NDFPR( INDF1, 'UNITS', IGRP2, I, INDF2, STATUS )

*  Process this pair of input and output NDFs.
         CALL KPS1_WALA0( INDF1, INDF2, IWCSR, METHOD, LBND, UBND,
     :                    ERRLIM, STATUS )

*  Annul the input NDF identifier.
         CALL NDG_ANNUL( INDF1, STATUS )

*  If an error has occurred, delete the output NDF, otherwise just 
*  annul its identifier.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL NDG_DELET( INDF2, STATUS )
         ELSE
            CALL NDG_ANNUL( INDF2, STATUS )
         END IF

*  If an error occurred processing the current input NDF...
         IF( STATUS .NE. SAI__OK ) THEN

*  Flush the error.
            CALL ERR_FLUSH( STATUS )

*  Give a warning telling the user that no output NDF will be created 
*  for the current input NDF.
            CALL GRP_GET( IGRP2, I, 1, NDFNAM, STATUS )
            CALL MSG_SETC( 'NDF', NDFNAM )
            CALL MSG_OUT( 'WCSALIGN_MSG2', '  WARNING: ^NDF cannot be'//
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
     :                 'group of 2-dimensional NDFs using WCS '//
     :                 'information.', STATUS )
      END IF

      END
