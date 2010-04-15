
*+  PIXDUPE - expands an input image by pixel duplication

      SUBROUTINE PIXDUPE ( STATUS )

*    Description :
*
*     This routine takes an input image and expands it in size by
*     duplicating the input pixels a calculated number of times to
*     form the required number of pixels in the output image.
*
*    Invocation :
*
*     CALL PIXDUPE( STATUS )
*
*    Parameters :
*
*     INPIC  = IMAGE( READ )
*           Image to be expanded
*     OUTPIC = IMAGE( WRITE )
*           Modified, larger version of the image
*     OTITLE = CHARACTER( READ )
*           Label for the output image
*     EXPAND = INTEGER( READ )
*           Linear expansion factor to be used to create new image
*
*    Method :
*
*     A DATA_ARRAY is input from the interface, and the dimensions of
*     the input array are reported to the user, who is then prompted
*     for an integer expansion to be applied to the input image. The
*     size of the output image is then calculated and reported to the
*     user, and a new data array of this size is created after the user
*     is prompted for the name and title for this output. The subroutine
*     PIXDUPESUB is then called to do the actual pixel duplication, and
*     then the data arrays are unmapped and tidied.
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
*     19-09-1985 : First implementation
*                : (REVA::MJM)
*     15-AUG-1994  Changed input DIM arguments for PIXDUPESUB (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE

*    Global constants :

      INCLUDE 'SAE_PAR'       ! global SSE definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*    Status :

      INTEGER STATUS          ! global status parameter

*    Local Constants :

      INTEGER NDIMS
      PARAMETER ( NDIMS = 2 )

*    Local variables :

      INTEGER
     :  IDIMS( NDIMS ),       ! dimensions of input DATA_ARRAY
     :  NELEMENTS,            ! number of elements mapped by NDF_MAP
     :  NDIM,                 ! number of dimensions from NDF_DIM
     :  PNTRI,                ! pointer to input DATA_ARRAY component
     :  ODIMS( NDIMS ),       ! dimensions of output DATA_ARRAY
     :  PNTRO,                ! pointer to output DATA_ARRAY component
     :  EXPAND,               ! one-dimensional compression factor
     :  MAXEXP,               ! maximum expansion factor allowed
     :  LOCI,                 ! input data structure
     :  LOCO                  ! output data structure

*-
*
*    check status on entry - if not ok then return

      IF( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get a locator to input IMAGE type data structure

      CALL GETINP( 'INPIC', LOCI, STATUS )

      IF( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    map the DATA_ARRAY component of the input data structure

      CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :               PNTRI, NELEMENTS, STATUS )

      CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS)

*    tell user dimensions of input array

      CALL MSG_SETI( 'XDIM', IDIMS(1) )
      CALL MSG_OUT( 'INPUT_XDIM',
     :        'x dimension of input image  =  ^XDIM', STATUS )

      CALL MSG_SETI( 'YDIM', IDIMS(2) )
      CALL MSG_OUT( 'INPUT_YDIM',
     :        'y dimension of input image  =  ^YDIM', STATUS )


*    work out maximum expansion factor allowable as input - make
*    this such that the output image would be no larger than
*    2048 x 2048 pixels ( uses integer arithmetic truncation )

      MAXEXP  =  2048 / ( MAX( IDIMS( 1 ), IDIMS( 2 ) ) )

*    get one-dimensional integer expansion factor ( i.e. factor by
*    which the image is to be pixel duplicated in each direction )
*    - set default to be 2, minimum 2 (or it's not a expansion),
*    and maximum to be the value calculated such that the output
*    image size does not exceed 2048 x 2048

      CALL AIF_GET0I( 'EXPAND', 2, 2, MAXEXP, EXPAND, STATUS )

*    now work out the size of the output array from the input image
*    dimensions and the expansion factor

      ODIMS( 1 )  =  IDIMS( 1 ) * EXPAND
      ODIMS( 2 )  =  IDIMS( 2 ) * EXPAND

*    tell user dimensions of output array

      CALL MSG_SETI( 'NEWXDIM', ODIMS(1) )
      CALL MSG_OUT( 'OUTPUT_XDIM',
     :        'x dimension of output image  =  ^NEWXDIM', STATUS )

      CALL MSG_SETI( 'NEWYDIM', ODIMS(2) )
      CALL MSG_OUT( 'OUTPUT_YDIM',
     :        'y dimension of output image  =  ^NEWYDIM', STATUS )


*    now get an output array to contain duplicated input pixels

      CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO, STATUS )

*    map output DATA_ARRAY component

      CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :               PNTRO, NELEMENTS, STATUS )


*    now call the subroutine that does the actual work

      CALL PIXDUPESUB( %VAL( PNTRI ), IDIMS(1), IDIMS(2),
     :                 %VAL( PNTRO ), ODIMS(1), ODIMS(2),
     :                 EXPAND, STATUS )


*    tidy up the output data structure

      CALL NDF_ANNUL( LOCO, STATUS )

*    tidy up the input data structure

      CALL NDF_ANNUL( LOCI, STATUS )

*    end

      END
