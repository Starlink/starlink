*+  RAPI2D - top level Rapi2d subroutine for A-task monolith

      SUBROUTINE RAPI2D( STATUS )

*    Description :
*
*     This is the top level monolith subroutine for the Rapi2d suite
*     of A-tasks. The value of NAME is input from the interface and
*     parsed, the requested A-task being called on successful matching
*     of the input string with a valid task name.
*
*    Invocation :
*
*     CALL RAPI2D( NAME, STATUS )
*
*    Method :
*
*     The input string NAME is tested against all the valid A-task
*     names after having been forced to upper-case. If a valid test
*     is made, the relevant A-task is called. If not, an error message
*     is output to the environment.
*
*    Deficiencies :
*
*     The input string has to be forced to upper-case (I think).
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     02-01-1986 : First documented implementation (REVA::MJM)
*     18-03-1986 : Updated list of included A-tasks (REVA::MJM)
*     15-06-1986 : Added LOOK option (REVA::MJM)
*     20-06-1986 : Removed BCAL and included MSTATS (REVA::MJM)
*     27-06-1986 : Added CADD, CDIV, CMULT, CSUB (REVA::MJM)
*     03-07-1986 : Changed EXP to EXPE (REVA::MJM)
*     26-11-1986 : Added PICKIM (UKTH::MJM)
*     25-10-1987 : Added QUILT (UKTH::CAA)
*     12-Apr-1994  Tidied calls to identical subroutines (SKL@JACH)
*                  removed MSTATS for KAPPA replacement (SKL@JACH)
*     19-AUG-1994  Changed style to new UNIX/VMS Atask monolith style(SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'PAR_PAR'          ! Necessary for non-VMS
      INCLUDE  'NDF_PAR'
      INCLUDE  'NDF_ERR'


      INTEGER  STATUS             ! global status parameter

      CHARACTER*(PAR__SZNAM)    NAME    ! action name

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN

         RETURN

      ENDIF

* start NDF context

      CALL NDF_BEGIN


*    get action name

      CALL TASK_GET_NAME( NAME, STATUS )

*    force the input string to upper-case before testing

      CALL UPCASE( NAME, NAME, STATUS )

*    check the string against valid A-task names - if matched then
*    call the relevant A-task

      IF ( NAME .EQ. 'ADD' ) THEN

*       add image+image into new output image

         CALL ADD ( STATUS )

      ELSE IF ( NAME .EQ. 'APERADD' ) THEN

*       bins up pixels in a circular aperture and gives statistics

         CALL APERADD ( STATUS )

      ELSE IF ( NAME .EQ. 'BLOCK' ) THEN

*       perform a block smooth on a 2-D image

         CALL BLOCK ( STATUS )

      ELSE IF ( NAME .EQ. 'CADD' ) THEN

*       add image+scalar into new output image

         CALL CADD ( STATUS )

      ELSE IF ( NAME .EQ. 'CDIV' ) THEN

*       divide image/scalar into new output image

         CALL CDIV ( STATUS )

      ELSE IF ( NAME .EQ. 'CENTROID' ) THEN

*       locates star centres by centroiding technique

         CALL CENTROID ( STATUS )

      ELSE IF ( NAME .EQ. 'CHPIX' ) THEN

*       replace 'bad' pixels with specified values

         CALL CHPIX ( STATUS )

      ELSE IF ( NAME .EQ. 'CMULT' ) THEN

*       multiply image*scalar into new output image

         CALL CMULT ( STATUS )

      ELSE IF ( NAME .EQ. 'COMPADD' ) THEN

*       compresses image by adding together a specified number of pixels

         CALL COMPADD ( STATUS )

      ELSE IF ( NAME .EQ. 'COMPAVE' ) THEN

*       compresses image by averaging over specified number of pixels

         CALL COMPAVE ( STATUS )

      ELSE IF ( NAME .EQ. 'COMPICK' ) THEN

*       compresses image by selecting pixels from the input image

         CALL COMPICK ( STATUS )

      ELSE IF ( NAME .EQ. 'COMPRESS' ) THEN

*       compresses image by different amounts in x and y dimensions

         CALL COMPRESS ( STATUS )

      ELSE IF ( NAME .EQ. 'CREFRAME' ) THEN

*       allows user to generate specific forms of test data

         CALL CREFRAME ( STATUS )

      ELSE IF ( NAME .EQ. 'CSUB' ) THEN

*       subtract image-scalar into new output image

         CALL CSUB ( STATUS )

      ELSE IF ( NAME .EQ. 'DIV' ) THEN

*       divide image/image into new output image

         CALL DIV2D ( STATUS )

      ELSE IF ( NAME .EQ. 'EXPE' ) THEN

*       takes the exponential of an image or scalar

         CALL EXPON ( STATUS )

      ELSE IF ( NAME .EQ. 'EXP10' ) THEN

*       takes the exponential of an image or scalar ( base 10 )

         CALL EXPON ( STATUS )

      ELSE IF ( NAME .EQ. 'EXPON' ) THEN

*       takes the exponential of an image or scalar ( prompted for base )

         CALL EXPON ( STATUS )

      ELSE IF ( NAME .EQ. 'FLIP' ) THEN

*       reverse a 2-D image about either the horizontal or vertical axis

         CALL FLIP ( STATUS )

      ELSE IF ( NAME .EQ. 'GAUSS' ) THEN

*       perform a gaussian smooth on a 2-D image

         CALL GAUSS ( STATUS )

      ELSE IF ( NAME .EQ. 'GLITCH' ) THEN

*       replace 'bad' pixels with local mean

         CALL GLITCH ( STATUS )

      ELSE IF ( NAME .EQ. 'HISTEQ' ) THEN

*       performs histogram equalisation on an image

         CALL HISTEQ ( STATUS )

      ELSE IF ( NAME .EQ. 'HISTO' ) THEN

*       calculates histogram of an image sub-array and certain
*       statistical parameters from the histogram

         CALL HISTO ( STATUS )

      ELSE IF ( NAME .EQ. 'LAPLACE' ) THEN

*       subtract an image's Laplacian from itself

         CALL LAPLACE ( STATUS )

      ELSE IF ( NAME .EQ. 'LOG10' ) THEN

*       takes the logarithm of an image or scalar ( base 10 )

         CALL LOGAR ( STATUS )

      ELSE IF ( NAME .EQ. 'LOGAR' ) THEN

*       takes the logarithm of an image or scalar ( prompted for base )

         CALL LOGAR ( STATUS )

      ELSE IF ( NAME .EQ. 'LOGE' ) THEN

*       takes the logarithm of an image or scalar ( base e )

         CALL LOGAR ( STATUS )

      ELSE IF ( NAME .EQ. 'LOOK' ) THEN

*       allows inspection of image values to screen or listing file

         CALL LOOK ( STATUS )

      ELSE IF ( NAME .EQ. 'MANIC' ) THEN

*       1,2 or 3-D image to 1,2 or 3-D image reshaping program

         CALL MANIC ( STATUS )

      ELSE IF ( NAME .EQ. 'MEDIAN' ) THEN

*       perform weighted median filtering on a 2-D image

         CALL MEDIAN ( STATUS )

      ELSE IF ( NAME .EQ. 'MOFF' ) THEN

*       calculates spatial and intensity offsets from 2 images

         CALL MOFF ( STATUS )

      ELSE IF ( NAME .EQ. 'MOSAIC' ) THEN

*       merges several non-congruent images into one big image

         CALL MOSAIC ( STATUS )

      ELSE IF ( NAME .EQ. 'MULT' ) THEN

*       multiply image*image into new output image

         CALL MULT ( STATUS )

      ELSE IF ( NAME .EQ. 'NUMB' ) THEN

*       count how many pixels in a 2-D image with absolute values > value

         CALL NUMB ( STATUS )

      ELSE IF ( NAME .EQ. 'OUTSETC' ) THEN

*       replaces pixels outside specified circle with specified value

         CALL OUTSET ( STATUS )

      ELSE IF ( NAME .EQ. 'PICKIM' ) THEN

*       creates a new image from a subset of another image

         CALL PICKIM ( STATUS )

      ELSE IF ( NAME .EQ. 'PIXDUPE' ) THEN

*       expands an input image by pixel duplication

         CALL PIXDUPE ( STATUS )

      ELSE IF ( NAME .EQ. 'POW' ) THEN

*       calculates the power of an image or scalar ( prompted for power )

         CALL POW2D ( STATUS )

      ELSE IF ( NAME .EQ. 'QUILT' ) THEN

*       auto mosaicing from an input file of images/offsets

         CALL QUILT ( STATUS )

      ELSE IF ( NAME .EQ. 'ROTATE' ) THEN

*       rotate a 2-D image through any number of degrees

         CALL ROTATE ( STATUS )

      ELSE IF ( NAME .EQ. 'SHADOW' ) THEN

*       enhances image by simulating side-lighting effect

         CALL SHADOW ( STATUS )

      ELSE IF ( NAME .EQ. 'SHIFT' ) THEN

*       realign a 2-D image

         CALL SHIFT ( STATUS )

      ELSE IF ( NAME .EQ. 'SHSIZE' ) THEN

*       output the dimensions of an image to the environment

         CALL SHSIZE ( STATUS )

      ELSE IF ( NAME .EQ. 'SQORST' ) THEN

*       change the dimensions of a 2-D image

         CALL SQORST ( STATUS )

      ELSE IF ( NAME .EQ. 'STATS' ) THEN

*       works out image statistics

         CALL STATS ( STATUS )

      ELSE IF ( NAME .EQ. 'SUB' ) THEN

*       subtract image-image into new output image

         CALL SUB ( STATUS )

      ELSE IF ( NAME .EQ. 'THRESH' ) THEN

*       for a 2-D image set all values outside a given range to user
*       defined values both above and below limits
         CALL THRESH ( STATUS )

      ELSE IF ( NAME .EQ. 'THRESH0' ) THEN

*       for a 2-D image set all values outside a given range to zero

         CALL THRESH ( STATUS )

      ELSE IF ( NAME .EQ. 'TRACE' ) THEN

*       trace through contents of a data structure

         CALL TRACE ( STATUS )

      ELSE IF ( NAME .EQ. 'TRANDAT' ) THEN

*       converts free-format x,y,i data into .SDF images

         CALL TRANDAT ( STATUS )

      ELSE IF ( NAME .EQ. 'TRIG' ) THEN

*       performs trigonometrical transformations on images

         CALL TRIG ( STATUS )

      ELSE IF ( NAME .EQ. 'WELCOME_RAPI2D' ) THEN

*       writes up welcome info for auto loading

         CALL WELCOME_RAPI2D ( STATUS )

      ELSE IF ( NAME .EQ. 'ZAPLIN' ) THEN

*       interpolates over bad rows or columns

         CALL ZAPLIN ( STATUS )

      ELSE

*        no such option exists

         CALL MSG_OUT( 'RAP_ERR',
     :                  'No such option honey, hard cheese', STATUS)

	 CALL MSG_SETC( 'NAM', NAME)
	 CALL MSG_OUT( 'RAP_ERR', 'Option specified was ^NAM', STATUS)

      END IF

* make sure all NDF locators are released

      CALL NDF_END( STATUS )

      END
