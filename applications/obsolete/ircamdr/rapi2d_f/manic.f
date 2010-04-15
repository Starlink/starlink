
*+  MANIC - Conversion of STARLINK images from one format to another
      SUBROUTINE MANIC( STATUS )
*    Description :
*     To write all or part of a 1, 2 or 3 dimensional STARLINK image to an
*     output image of 1, 2 or 3 dimensions. Windows may be set in any of the
*     dimensions of the input image. All or part of the input image may be
*     projected on to any of the rectangular planes or axes of the input before
*     being written to an output image; or a 1 or 2 dimensional image may be
*     grown to more dimensions to fill an output image. Many output images,
*     each of a different configuration if required, may be extracted from a
*     single input image with one call to the routine.
*    Parameters :
*     INPUT      = IMAGE( READ )
*           Data structure holding the input image
*     ONDIM      = INTEGER( READ )
*           Dimensionality of the output image
*     XLIMITS(2) = INTEGER( READ )
*           The X-axis window on the input image to be used in forming the
*           output image
*     YLIMITS(2) = INTEGER( READ )
*           The Y-axis window on the input image to be used in forming the
*           output image
*     ZLIMITS(2) = INTEGER( READ )
*           The Z-axis window on the input image to be used in forming the
*           output image
*     XRANGE(2)  = INTEGER( READ )
*           The X-axis range for summation in the input image in forming the
*           output image
*     YRANGE(2)  = INTEGER( READ )
*           The Y-axis range for summation in the input image in forming the
*           output image
*     ZRANGE(2)  = INTEGER( READ )
*           The Z-axis range for summation in the input image in forming the
*           output image
*     EPLANE     = CHARACTER( READ )
*           Plane to be extracted from the input 3-D image.
*     GPLANE     = CHARACTER( READ )
*           Input 2-D image forms this plane when being grown into 3-D image
*     ELINE1     = CHARACTER( READ )
*           Axis of input 2-D image to be extracted to form output 1-D image
*     ELINE2     = CHARACTER( READ )
*           Axis of input 3-D image to be extracted to form output 1-D image
*     GLINE1     = CHARACTER( READ )
*           Input 1-D image will form this axis of an output 2-D image
*     GLINE2     = CHARACTER( READ )
*           Input 1-D image will form this axis of an output 3-D image
*     XDIM       = _INTEGER(READ)
*           X-dimension of output 2-D or 3-D image grown from input 1-D or 2-D
*           image
*     YDIM       = _INTEGER(READ)
*           Y-dimension of output 2-D or 3-D image grown from input 1-D or 2-D
*           image
*     ZDIM       = _INTEGER(READ)
*           Z-dimension of output 2-D or 3-D image grown from input 1-D or 2-D
*           image
*     OUTPUT     = IMAGE( WRITE )
*           Data structure to hold output image
*     OTITLE     = CHARACTER( WRITE )
*           Title for data structure to hold output image
*     LOOP       = LOGICAL( READ )
*       Extract or grow further output images from the same input image.
*    Method :
*     Get input IMAGE type data structure
*     Find out shape of input DATA_ARRAY component
*     If image is 1 to 3-dimensional then
*        Initialise LOOP to .TRUE. to start off the loop
*        Do while LOOP = .TRUE. and no errors occur
*           Tell user shape of input image
*           Get dimensionality of output image
*           If output image has more dimensions the input image then
*              Call MAMORE to set up output image dimensions and input image
*                slice limits.
*           Else if output image has same no. of dimensions as input image then
*              Call MASAME to set up output image dimensions and input image
*                slice limits.
*           Else
*              Output image must have fewer dimensions than input image so
*                call MALESS to set up output image dimensions and input image
*                slice limits.
*           Endif
*           Get locator to and then map input image slice
*           Create output data structure containing a DATA_ARRAY component
*             of appropriate dimensionality and dimensions also create
*             and get a value for a TITLE component
*           Map output DATA_ARRAY component
*           Perform input to output transformation according to value of CASE
*           Ask user if to repeat process for same input image
*           Unmap images and cancel parameters if necessary
*        Enddo
*     Endif
*     Tidy up input data structure
*    Authors :
*     C D Pike    (RGO::CDP)
*     Roger Wood  (RGO::RW)
*     Dave Baines (ROE::ASOC5)
*    History :
*     15/08/1981 : Original version    (RGO::CDP)
*     30/03/1983 : Amended  version    (RGO::RW)
*     21/02/1984 : Revised SSE version (ROE::ASOC5)
*     11-MAR-94    Changed DAT_, CMP_ to NDF_ (SKL@JACH)
*     15-Jul-1994  Changed feeding of dimensions to MAxTOx routines
*                  so that routines will still compile (SKL@JACH)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'
*    Status :
      INTEGER STATUS
*    External references :

*    Local constants :
      INTEGER MXSIZE
      PARAMETER ( MXSIZE = 1000 )
      INTEGER MXDIM
      PARAMETER ( MXDIM = 3 )
*    Local variables :
      INTEGER                  ! locators to :
     :  LOCI,                  ! input data structure
     :  LOCO,                  ! output data structure
     :  SLICE                  ! slice of input data_array
      INTEGER
     :  NELEMENTS,           ! number of elements mapped
     :  IDIMS( MXDIM ), ! input image dimensions
     :  ODIMS( MXDIM ), ! output  "        "
     :  SDIMS( MXDIM ), ! input image slice dimensions
     :  UPPER( 3 ), ! upper bounds for slice
     :  LOWER( 3 ), ! lower    "    "    "
     :  PNTRI, ! pointer to input data_array component
     :  PNTRO, !    "     " output    "          "
     :  INDIM, ! dimensionality of input image
     :  ONDIM, !        "        " output  "
     :  MODE,  ! decision parameter for subroutines
     :  CASE   ! type of extraction/projection to be performed
      LOGICAL
     :  LOOP ! will be .true. if further output images to be created
*-

*    get a locator to input IMAGE type data structure
      CALL GETINP( 'INPUT', LOCI, STATUS )

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

*       enquire shape of input DATA_ARRAY component
          CALL NDF_DIM( LOCI, NDF__MXDIM, IDIMS, INDIM, STATUS )

*       check that dimensionality is within allowed range
         IF( ( INDIM .GT. 0 ) .AND. ( INDIM .LT. 4 ) ) THEN

*          initialise LOOP to .TRUE. to start off the loop
            LOOP = .TRUE.

*          repeat the loop as long as LOOP is 'Y' and no errors occur
            DO WHILE ( LOOP .AND. (STATUS .EQ. SAI__OK) )

*             tell user dimensionality and dimensions of input image
               CALL MSG_SETI( 'INDIM', INDIM )
               CALL MSG_OUT( 'INPUT_INDIM',
     :           'Image is ^INDIM dimensional', STATUS )
               CALL MSG_SETI( 'XDIM', IDIMS(1) )
               CALL MSG_OUT( 'INPUT_XDIM',
     :           'First dimension = ^XDIM', STATUS )
               IF( INDIM .GT. 1 ) THEN

                  CALL MSG_SETI( 'YDIM', IDIMS(2) )
                  CALL MSG_OUT( 'INPUT_YDIM',
     :              'Second dimension = ^YDIM', STATUS )
                  IF( INDIM .GT. 2 ) THEN

                     CALL MSG_SETI( 'ZDIM', IDIMS(3) )
                     CALL MSG_OUT( 'INPUT_ZDIM',
     :                 'Third dimension = ^ZDIM', STATUS )
                  ENDIF
               ENDIF

*             enquire number of dimensions for output image
               CALL APPG0I( 'ONDIM', INDIM, 1, 3, ONDIM, STATUS )

*             deal with the three basic cases, output array with more,
*             same or less dimensions than the input array
               IF( ONDIM .GT. INDIM ) THEN

                  CALL MAMORE( 'XLIMITS', 'YLIMITS', 'ZLIMITS', 'XDIM',
     :              'YDIM', 'ZDIM', 'GLINE1', 'GLINE2', 'GPLANE',
     :              MXSIZE, INDIM, IDIMS, ONDIM, ODIMS, LOWER, UPPER,
     :              CASE, MODE, STATUS )

               ELSEIF( ONDIM .EQ. INDIM ) THEN

*                here CASE is the same as the dimensionality of the arrays
                  CASE = INDIM
                  CALL MASAME( 'XLIMITS', 'YLIMITS', 'ZLIMITS', INDIM,
     :              IDIMS, ODIMS, LOWER, UPPER, STATUS )
               ELSE

                 CALL MALESS( 'XLIMITS', 'YLIMITS', 'ZLIMITS', 'XRANGE',
     :             'YRANGE', 'ZRANGE', 'ELINE1', 'ELINE2', 'EPLANE',
     :             INDIM, IDIMS, ONDIM, ODIMS, LOWER, UPPER, CASE, MODE,
     :             STATUS )
               ENDIF

*             get a locator to the required slice of the input image
               CALL NDF_SECT( LOCI, INDIM, LOWER, UPPER, SLICE,
     :           STATUS )

*             map the input image slice
               CALL NDF_MAP( SLICE, 'DATA', '_REAL', 'READ', PNTRI,
     :           NELEMENTS, STATUS )

               CALL NDF_DIM( SLICE, NDF__MXDIM, SDIMS, INDIM, STATUS )

*             create the output IMAGE type data structure, create and get a
*             value for a TITLE component and create a DATA_ARRAY component
*             of dimensionality ONDIM and dimensions DIMS(ONDIM)
               CALL CREOUT( 'OUTPUT', 'OTITLE', ONDIM, ODIMS, LOCO,
     :           STATUS )

*             check for error
               IF( STATUS .EQ. SAI__OK ) THEN

*                map the output DATA_ARRAY component
                  CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :              PNTRO, NELEMENTS, STATUS )

*                check for error
                  IF( STATUS .EQ. SAI__OK ) THEN

*                   perform the reshaping of the image
                     IF( CASE .EQ. 1 ) THEN

                        CALL COPY1D( SDIMS(1), %VAL( PNTRI ),
     :                    %VAL( PNTRO ), STATUS )

                     ELSEIF( CASE .EQ. 2 ) THEN

                        CALL COPY2D( SDIMS(1), SDIMS(2),
     :                    %VAL( PNTRI ), %VAL( PNTRO ), STATUS )

                     ELSEIF( CASE .EQ. 3 ) THEN

                        CALL COPY3D( SDIMS(1), SDIMS(2), SDIMS(3),
     :                    %VAL( PNTRI ), %VAL( PNTRO ), STATUS )

                     ELSEIF( CASE .EQ. 4 ) THEN

                        CALL MA3TO2( MODE, SDIMS(1), SDIMS(2),
     :                    SDIMS(3), %VAL( PNTRI ), ODIMS(1), ODIMS(2),
     :                    %VAL( PNTRO ), STATUS )

                     ELSEIF( CASE .EQ. 5 ) THEN

                        CALL MA3TO1( MODE, SDIMS(1), SDIMS(2),
     :                    SDIMS(3), %VAL( PNTRI ), ODIMS(1),
     :                    %VAL( PNTRO ), STATUS )

                     ELSEIF( CASE .EQ. 6 ) THEN

                        CALL MA2TO3( MODE, SDIMS(1), SDIMS(2),
     :                    %VAL( PNTRI ), ODIMS(1), ODIMS(2), ODIMS(3),
     :                    %VAL( PNTRO ), STATUS )

                     ELSEIF( CASE .EQ. 7 ) THEN

                        CALL MA2TO1( MODE, SDIMS(1), SDIMS(2),
     :                    %VAL( PNTRI ), ODIMS(1),
     :                    %VAL( PNTRO ), STATUS )

                     ELSEIF( CASE .EQ. 8 ) THEN

                        CALL MA1TO3( MODE, SDIMS(1), %VAL( PNTRI ),
     :                    ODIMS(1), ODIMS(2), ODIMS(3),
     :                    %VAL( PNTRO ), STATUS )

                     ELSEIF( CASE .EQ. 9 ) THEN

                        CALL MA1TO2( MODE, SDIMS(1), %VAL( PNTRI ),
     :                    ODIMS(1), ODIMS(2),
     :                    %VAL( PNTRO ), STATUS )
                     ENDIF
                  ENDIF

*                tidy up output structure
                  CALL NDF_ANNUL(  LOCO, STATUS )
               ENDIF

*             tidy up the input slice
               CALL NDF_ANNUL( SLICE, STATUS )

*             enquire if user wishes to make another image from same input
               CALL PAR_GET0L( 'LOOP', LOOP, STATUS )

*             if reply was Yes then cancel all parameters except INPUT
               IF( LOOP .AND. ( STATUS .EQ.SAI__OK ) ) THEN

                  CALL PAR_CANCL(  'OUTPUT', STATUS )
                  CALL PAR_CANCL(   'ONDIM', STATUS )
                  CALL PAR_CANCL(  'OTITLE', STATUS )
                  CALL PAR_CANCL( 'XLIMITS', STATUS )
                  CALL PAR_CANCL( 'YLIMITS', STATUS )
                  CALL PAR_CANCL( 'ZLIMITS', STATUS )
                  CALL PAR_CANCL(  'XRANGE', STATUS )
                  CALL PAR_CANCL(  'YRANGE', STATUS )
                  CALL PAR_CANCL(  'ZRANGE', STATUS )
                  CALL PAR_CANCL(  'EPLANE', STATUS )
                  CALL PAR_CANCL(  'GPLANE', STATUS )
                  CALL PAR_CANCL(  'ELINE1', STATUS )
                  CALL PAR_CANCL(  'ELINE2', STATUS )
                  CALL PAR_CANCL(  'GLINE1', STATUS )
                  CALL PAR_CANCL(  'GLINE2', STATUS )
                  CALL PAR_CANCL(    'XDIM', STATUS )
                  CALL PAR_CANCL(    'YDIM', STATUS )
                  CALL PAR_CANCL(    'ZDIM', STATUS )
                  CALL PAR_CANCL(    'LOOP', STATUS )
               ENDIF
            ENDDO
         ELSE

*          input image has wrong dimensionality
            CALL MSG_SETI( 'INDIM', INDIM )
            CALL MSG_OUT( 'ERR_MANIC',
     :        'Image is ^INDIM dimensional, not 1,2 or 3-D', STATUS )
         ENDIF

*       tidy up the input structure
         CALL NDF_ANNUL(  LOCI, STATUS )
      ENDIF

      END
