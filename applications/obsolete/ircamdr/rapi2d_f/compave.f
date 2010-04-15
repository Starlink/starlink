
*+  COMPAVE - reduces the size of an array by averaging pixels

      SUBROUTINE COMPAVE ( STATUS )

*    Description :
*
*     This routine takes an input image and reduces it in size by
*     averaging together a number of pixels in the input array to
*     form one new pixel in the output array.
*
*    Invocation :
*
*     CALL COMPAVE( STATUS )
*
*    Parameters :
*
*     INPIC  = IMAGE( READ )
*           Image to be reduced in size
*     OUTPIC = IMAGE( WRITE )
*           Modified, smaller version of the image
*     OTITLE = CHARACTER( READ )
*           Label for the output image
*     COMPRESS = INTEGER( READ )
*           Linear compression factor to be used to create new image
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get input image structure from environment
*     If no error then
*        Map DATA_ARRAY component onto pointer
*        Output input image dimensions to user
*        Get compression factor from interface
*        Create output structure to hold processed image
*        If no error so far then
*           Map DATA_ARRAY component in
*           If no error so far then
*              Call COMPAVESUB to do the work
*           Endif
*           Unmap and tidy output structure
*        Endif
*        Unmap and tidy input structure
*     Endif
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     18-09-1985 : First implementation (REVA::MJM)
*     20-01-1986 : More error checking and tidying (REVA::MJM)
*     10-Mar-94    Changed DAT_, CMP_ calls to NDF_ (SKL@JACH)
*     15-AUG-1994  Changed input DIM arguments for COMPAVESUB (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE           ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'       ! global SSE definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*    Status :

      INTEGER STATUS          ! global status parameter

*    Local Constants :

      INTEGER NDIMS           ! input/output image dimensionality
      PARAMETER ( NDIMS = 2 ) ! 2-d images only

*    Local variables :

      INTEGER
     :  IDIMS( NDIMS ),       ! dimensions of input DATA_ARRAY
     :  NELEMENTS,            ! Number of elements mapped
     :  NDIM,                 ! Number of dimensions from NDF_DIM
     :  PNTRI,                ! pointer to input DATA_ARRAY component
     :  ODIMS( NDIMS ),       ! dimensions of output DATA_ARRAY
     :  PNTRO,                ! pointer to output DATA_ARRAY component
     :  COMPRESS,             ! one-dimensional compression factor
     :  MAXCOM                ! maximum compression factor allowed

      INTEGER                ! locators for :
     :  LOCI,                 ! input data structure
     :  LOCO                  ! output data structure

*-
*    check status on entry - if not ok then return
      IF( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get a locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    if no error getting input structure then continue
      IF ( STATUS .EQ. SAI__OK ) THEN

*       map the DATA_ARRAY component of the input data structure
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )

*       get dimensions of array
         CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS )

*       tell user dimensions of input array
         CALL MSG_SETI( 'XDIM', IDIMS(1) )
         CALL MSG_SETI( 'YDIM', IDIMS(2) )
         CALL MSG_OUT( 'INPUT_DIMS',
     :        'Image is ^XDIM by ^YDIM pixels', STATUS )

*       work out maximum compression factor allowable as input
         MAXCOM  =  MIN( IDIMS( 1 ), IDIMS( 2 ) )

*       get one-dimensional integer compression factor ( i.e. factor by
*       which the image is to be compressed in each direction ) - set
*       default to be 2, minimum 2 (or it's not a compression), and
*       maximum to be the minor axis of the input array (such that
*       the smallest dimension of the output image is at least 1).
*       Also initialise compress variable in case of null parameter
*       input, which could cause divide by zero error.
         COMPRESS  =  1
         CALL AIF_GET0I( 'COMPRESS', 2, 2, MAXCOM, COMPRESS, STATUS )

*       now work out the size of the output array from the input image
*       dimensions and the compression factor - relies on integer
*       arithmetic truncation
         ODIMS( 1 )  =  IDIMS( 1 ) / COMPRESS
         ODIMS( 2 )  =  IDIMS( 2 ) / COMPRESS

*       tell user dimensions of output array
         CALL MSG_SETI( 'NEWXDIM', ODIMS(1) )
         CALL MSG_OUT( 'OUTPUT_XDIM',
     :        'x dimension of output image  =  ^NEWXDIM', STATUS )

         CALL MSG_SETI( 'NEWYDIM', ODIMS(2) )
         CALL MSG_OUT( 'OUTPUT_YDIM',
     :        'y dimension of output image  =  ^NEWYDIM', STATUS )

*       now get an output array to contain averaged data
         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO, STATUS )

*       if no error creating output structure then continue
         IF ( STATUS .EQ. SAI__OK ) THEN

*          map output DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :               PNTRO, NELEMENTS, STATUS )

*          check for error before calling working subroutine
            IF ( STATUS .EQ. SAI__OK ) THEN

*             now call the subroutine that does the actual work
               CALL COMPAVESUB( %VAL( PNTRI ), IDIMS(1), IDIMS(2),
     :                 %VAL( PNTRO ), ODIMS(1), ODIMS(2), COMPRESS,
     :                 STATUS )

            END IF

*          tidy up the output data structure
            CALL NDF_ANNUL( LOCO, STATUS )

*       end of if-no-error-after-creating-output-structure check
         END IF

*       tidy up the input data structure
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-no-error-after-getting-input-stucture check
      END IF


*    end

      END
