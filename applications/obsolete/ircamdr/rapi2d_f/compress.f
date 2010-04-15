
*+  COMPRESS - reduces the size of an array by different amounts in x and y

      SUBROUTINE COMPRESS ( STATUS )

*    Description :
*
*     This routine takes an input image and reduces it in size by
*     averaging together a number of pixels in the input array to
*     form one new pixel in the output array. The integer compression
*     factor may be different in both the x and y dimensions.
*
*    Invocation :
*
*     CALL COMPRESS( STATUS )
*
*    Parameters :
*
*     INPIC  = IMAGE( READ )
*           Image to be reduced in size
*     OUTPIC = IMAGE( WRITE )
*           Modified, smaller version of the image
*     OTITLE = CHARACTER( READ )
*           Label for the output image
*     XCMPRS = INTEGER( READ )
*           Linear compression factor to be used in x dimension
*     YCMPRS = INTEGER( READ )
*           Linear compression factor to be used in y dimension
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get input image structure from environment
*     If no error so far then
*        Map DATA_ARRAY component onto pointer
*        Output input image dimensions to the user
*        Get x and y compression factors from interface
*        Create output image structure and title component
*        If no error so far then
*           Map a DATA_ARRAY component onto a pointer
*           If no error so far then
*              Call COMPRESSSUB to do the actual compression
*           Endif
*           Unmap and tidy output data structure
*        Endif
*        Unmap and tidy input data structure
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
*     21-10-1985 : First implementation (REVA::MJM)
*     20-01-1986 : More error checking and tidying (REVA::MJM)
*     10-MAR=94    Changed DAT_, CMP_ calls to NDF_ (SKL@JACH)
*     15-Aug-1994  Changed input DIM arguments for COMPRESSSUB (SKL@JACH)
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
     :  XCMPRS,               ! compression factor in x dimension
     :  YCMPRS,               ! compression factor in y dimension
     :  MAXXCOM,              ! maximum compression factor allowed in x
     :  MAXYCOM               ! maximum compression factor allowed in y

      INTEGER                 ! locators for :
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

*       get sixe of array
         CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS)

*       tell user dimensions of input array
         CALL MSG_SETI( 'XDIM', IDIMS(1) )
         CALL MSG_SETI( 'YDIM', IDIMS(2) )
         CALL MSG_OUT( 'INPUT_DIMS',
     :        'Image is ^XDIM by ^YDIM pixels', STATUS )

*       work out maximum compression factors allowable as input
         MAXXCOM  =  IDIMS( 1 )
         MAXYCOM  =  IDIMS( 2 )

*       get the two integer compression factors ( i.e. factor by
*       which the image is to be compressed in each direction ) - set
*       default to be 2, minimum 1, and the maximum amount of
*       compression in each direction to be equal to the input array
*       dimension for that axis. Thus the minimum possible output
*       array size would be 1x1. Also, initialise the two compress
*       variables in case of null parameters being given, which could
*       lead to divide by zero errors.
         XCMPRS  =  1
         YCMPRS  =  1
         CALL AIF_GET0I( 'XCMPRS', 2, 1, MAXXCOM, XCMPRS, STATUS )
         CALL AIF_GET0I( 'YCMPRS', 2, 1, MAXYCOM, YCMPRS, STATUS )

*       now work out the size of the output array from the input image
*       dimensions and the compression factors - relies on integer
*       arithmetic truncation
         ODIMS( 1 )  =  IDIMS( 1 ) / XCMPRS
         ODIMS( 2 )  =  IDIMS( 2 ) / YCMPRS

*       tell user dimensions of output array
         CALL MSG_SETI( 'NEWXDIM', ODIMS(1) )
         CALL MSG_OUT( 'OUTPUT_XDIM',
     :        'x dimension of output image  =  ^NEWXDIM', STATUS )

         CALL MSG_SETI( 'NEWYDIM', ODIMS(2) )
         CALL MSG_OUT( 'OUTPUT_YDIM',
     :        'y dimension of output image  =  ^NEWYDIM', STATUS )

*       now get an output array to contain averaged data
         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS,
     :                 LOCO, STATUS )

*       if no error in creating output structure then continue
         IF ( STATUS .EQ. SAI__OK ) THEN

*          map output DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :               PNTRO, NELEMENTS, STATUS )

*          check for error before calling working subroutine
            IF ( STATUS .EQ. SAI__OK ) THEN

*             now call the subroutine that does the actual work
               CALL COMPRESSSUB( %VAL( PNTRI ), IDIMS(1), IDIMS(2),
     :                  %VAL( PNTRO ), ODIMS(1), ODIMS(2), XCMPRS,
     :                  YCMPRS, STATUS )

            END IF

*          tidy up the output data structure
            CALL NDF_ANNUL( LOCO, STATUS )

*       end of if-no-error-creating-output-structure check
         END IF

*       tidy up the input data structure
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-no-error-getting-input-structure check
      END IF


*    end

      END
