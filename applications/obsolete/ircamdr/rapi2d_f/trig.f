
*+  TRIG - performs trigonometric transformations on images

      SUBROUTINE TRIG ( STATUS )

*    Description :
*
*     This routine allows the user to select one of a set of several
*     basic trigonometrical functions (sine, cosine, tangent, arcsine,
*     etc..) and operate on each pixel of an input image with this
*     function, and then to output a transformed version of the image.
*     The trigonometric functions can be selected to act as if the
*     input data is to be treated as radians or degrees. If a scalar
*     value rather than a data array is input, the program acts purely
*     on that scalar value.
*
*    Invocation :
*
*     CALL TRIG( STATUS )
*
*    Parameters :
*
*     INPIC = IMAGE( READ )
*            Input image to be transformed
*     TRIGFUNC = CHARACTER( READ )
*            Trigonometrical function to be applied
*     OUTPIC = IMAGE( WRITE )
*            Output transformed image
*     OTITLE = CHARACTER( READ )
*            Title string for output image
*
*    Method :
*
*     The user is prompted for the location of the input image
*     structure and its DATA_ARRAY component is mapped in.
*     From a defined set of possible options, a string is
*     obtained which corresponds to one of the permissible
*     trigonometrical functions.
*     An output image stucture name is obtained and a DATA_ARRAY
*     component created and mapped. The OTITLE title component
*     parameter is obtained also.
*     The subroutine TRIGSUB is called to perform the requested
*     transformation on the input data, returning the output
*     transformed array.
*     In the unlikely event of a bad function being passed to
*     TRIGSUB, it will just copy the input array into the output
*     and return the string 'BAD' in TRIGFUNC.
*     If TRIGSUB has been asked to do arcsine or arccos, and it
*     finds pixels outwith the range -1 to 1 in the input, the
*     output pixels are set to the corresponding range limits
*     and TRIGFUNC is returned with the string 'OUT'
*     On return, the input and output image structures are tidied.
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
*     10-12-1985 :  First implementation
*                :  (UKTH::MARK)
*     08-01-1986 :  Allowed option of scalar processing
*                :  (REVA::MJM)
*     12-Apr-1994   Changed DAT and CMP calls to NDF (SKL@JACH)
*     15-AUG-1994   Changed input DIM arguments for TRIGSUB (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'PAR_ERR'          ! parameter system error definitions
      INCLUDE  'NDF_PAR'
      INCLUDE  'NDF_ERR'

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local Constants :

      INTEGER  NDIMS              ! dimensionality of input data
      PARAMETER( NDIMS = 2 )      ! 2d images only

*    Local variables :

      INTEGER
     :     DIMS( NDIMS ),      ! input and output array dimensions
     :     NDIM,               ! number of dimensions from NDF_DIM
     :     NELEMENTS,          ! number of elements mapped by NDF_MAP
     :     PNTRI,              ! pointer to input image
     :     PNTRO,              !    "     " output  "
     :     LOCI,               ! locator to input image structure
     :     LOCO                !    "     " output  "       "

      REAL
     :     SCALAR,             ! input scalar value for processing
     :     RESULT              ! result of processing scalar value

      CHARACTER*5
     :     TRIGFUNC            ! requested trigonometrical function

      CHARACTER*80
     :     OPTIONS             ! list of acceptable trig options

      LOGICAL
     :     VALID,              ! whether a valid input has been given
     :     FSCAL               ! whether a scalar was given

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*    initialise the valid and scalar input flags
      VALID  =  .FALSE.
      FSCAL  =  .FALSE.

*    loop until a valid input of some sort is obtained, aborting if
*    a null is input
      DO WHILE( .NOT. VALID )

*       start by trying to obtain an input image structure locator
         CALL GETINP( 'INPUT', LOCI, STATUS )

*       map its DATA_ARRAY component onto a pointer
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )

         CALL NDF_DIM( LOCI, NDIMS, DIMS, NDIM, STATUS)

*       check the status - if it is bad, then something went wrong with
*       the processing of the input image. Depending on the value of
*       the status, either return, or attempt to get a scalar value as
*       input
         IF ( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN

*          null parameter specified or abort requested - return
            RETURN

         ELSEIF( STATUS .NE. SAI__OK ) THEN

*          annul the error and try to get a scalar
            CALL ERR_ANNUL( STATUS )
            CALL DAT_GET0R( LOCI, SCALAR, STATUS )

*          if this succeeds, set the valid and scalar flags
*          accordingly
            IF( STATUS .EQ. SAI__OK ) THEN
               VALID  =  .TRUE.
               FSCAL  =  .TRUE.

            ELSE

*             a valid scalar was not given either - annul errors and
*             try to get another input
               CALL ERR_ANNUL( STATUS )
               CALL PAR_CANCL( 'INPUT', STATUS )
               CALL MSG_OUT( 'TRIG_BADINP',
     :          'Failed to make sense of input - try again', STATUS )

            END IF

         ELSE

*          the input image was o.k. so set the valid flag accordingly
            VALID  =  .TRUE.

         ENDIF

      END DO

*    now get the desired trigonometrical function from the interface
*    - permissible functions are :
*        SIN  -  sine (radians)       ASIN  -  arcsine (radians)
*        COS  -  cosine (radians)     ACOS  -  arccosine (radians)
*        TAN  -  tangent (radians)    ATAN  -  arctangent (radians)
*        SIND -  sine (degrees)       ASIND -  arcsine (degrees)
*        COSD -  cosine (degrees)     ACOSD -  arccosine (degrees)
*        TAND -  tangent (degrees)    ATAND -  arctangent (degrees)

      OPTIONS  =
     : 'SIN,COS,TAN,SIND,COSD,TAND,ASIN,ACOS,ATAN,ASIND,ACOSD,ATAND'

      CALL AIF_CHOIC( 'TRIGFUNC', OPTIONS, TRIGFUNC, STATUS )

*    process the input accordingly depending on whether or not
*    a scalar was input
      IF ( FSCAL ) THEN

*       call the subroutine that does trig on a scalar
         CALL TRIGSCAL( SCALAR, TRIGFUNC, RESULT, STATUS )

*       on return check for errors in TRIGSCAL

         IF ( STATUS .EQ. SAI__OK ) THEN

            IF( TRIGFUNC .EQ. 'BAD' ) THEN
*             bad function requested
               CALL MSG_OUT( 'TRIGS_BAD',
     :          'Bad trig fn. passed - no result available',
     :                        STATUS )

            ELSEIF( TRIGFUNC .EQ. 'OUT' ) THEN
               CALL MSG_OUT( 'TRIGS_OUT',
     :  'Input scalar was outside function range - no result available',
     :                       STATUS )

            ELSE

*             a valid result was returned, so output
               CALL MSG_OUT( 'BLANK', ' ', STATUS )
               CALL MSG_SETR( 'SCALAR', SCALAR, STATUS )
               CALL MSG_SETR( 'RESULT', RESULT, STATUS )
               CALL MSG_OUT( 'TRIG_RES',
     :          TRIGFUNC//' of ^SCALAR is ^RESULT', STATUS )
               CALL MSG_OUT( 'BLANK', ' ', STATUS )
            END IF

         ELSE
            CALL ERR_REP('ERR', 'Bad status after TRIGSCAL',
     :                   STATUS)
         END IF

*       clean up
         CALL NDF_ANNUL( LOCI, STATUS )

      ELSE

*       an image was input, so get an output image structure
         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, DIMS, LOCO, STATUS )

*       map its DATA_ARRAY component onto a pointer
         CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                  PNTRO, NELEMENTS, STATUS )


*       now call the subroutine that does the actual work
         CALL TRIGSUB( %VAL( PNTRI ), DIMS(1), DIMS(2), TRIGFUNC,
     :                    %VAL( PNTRO ), STATUS )

*       on return, check for errors in TRIGSUB

         IF (STATUS .EQ. SAI__OK) THEN

            IF( TRIGFUNC .EQ. 'BAD' ) THEN     ! bad function requested
               CALL MSG_OUT( 'TRIG_BAD',
     :       'Bad trig fn. passed - output array set equal to input',
     :                        STATUS )

            ELSEIF( TRIGFUNC .EQ. 'OUT' ) THEN
               CALL MSG_OUT( 'TRIG_OUT',
     :       'At least one input pixel was outside function range',
     :                       STATUS )

            END IF
         ELSE
            CALL ERR_REP('ERR', 'Bad status after TRIGSUB',
     :                   STATUS)
         END IF

*       tidy up input and output data structures
         CALL NDF_ANNUL( LOCI, STATUS )

         CALL NDF_ANNUL( LOCO, STATUS )


      END IF

*    end

      END
