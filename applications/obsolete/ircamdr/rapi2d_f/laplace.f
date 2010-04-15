
*+  LAPLACE - performs a Laplacian convolution as an edge detector

      SUBROUTINE LAPLACE ( STATUS )

*    Description :
*
*     This routine works out the Laplacian of an input image and
*     subtracts it from the original image to create a new output
*     image. This can be done an integer number of times, as given
*     by an input parameter.
*     This operation can be thought of as a convolution by
*
*                           -N   -N   -N
*                           -N   +8N  -N
*                           -N   -N   -N
*
*     where N is the integer number of times the Laplacian is
*     subtracted. This convolution is used as a unidirectional
*     edge detector. Areas where the input image is flat come
*     out as zero in the output.
*
*    Invocation :
*
*     CALL LAPLACE( STATUS )
*
*    Parameters :
*
*     INPIC = IMAGE( READ )
*            Input image to be processed
*     NUMBER = INTEGER( READ )
*            Number of times Laplacian is to be subtracted
*     OUTPIC = IMAGE( WRITE )
*            Output processed image
*     OTITLE = CHARACTER( READ )
*            Title string for output image
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get an input image structure
*     If no error so far then
*        Map its DATA_ARRAY component
*        Get a value for N, the number of Laplacian subtractions
*        Create an output image structure and title string
*        If no error so far then
*           Map a DATA_ARRAY component to hold convolved image
*           If no error so far then
*              Call LAPLACESUB to perform the convolution
*           Endif
*           Tidy up output image structure
*        Endif
*        Tidy up input image structure
*     Endif
*     End
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
*     10-12-1985 : First implementation (UKTH::MARK)
*     17-04-1986 : Tidied and more error checking (REVA::MJM)
*     11-MAR--94   Changed CMP_, DAT_ to NDF_ (SKL@JACH)
*     12-AUG-1994  Changed input DIM arguments for LAPLACESUB (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! global SSE definitions
      INCLUDE  'NDF_PAR'
      INCLUDE  'NDF_ERR'

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local Constants :

      INTEGER  NDIMS              ! dimensionality of input data
      PARAMETER( NDIMS = 2 )      ! 2d images only

*    Local variables :

      INTEGER
     :     NELEMENTS,             ! number of elements mapped
     :     NDIM,                  ! dimensions from NDF_DIM
     :     DIMS( NDIMS ),         ! input and output array dimensions
     :     PNTRI,                 ! pointer to input image
     :     PNTRO,                 !    "     " output  "
     :     NUMBER                 ! number of times Laplacian is removed

      INTEGER                     ! locators to :
     :     LOCI,                  ! input image structure
     :     LOCO                   ! output  "       "

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    start by obtaining the input image structure locator
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    if no error so far then continue
      IF ( STATUS .EQ. SAI__OK ) THEN

*       map its DATA_ARRAY component onto a pointer
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )

*       get dimesnions
         CALL NDF_DIM( LOCI, NDIMS, DIMS, NDIM, STATUS )

*       get the number of times Laplacian is to be removed
         CALL AIF_GET0I( 'NUMBER', 1, 1, 100, NUMBER, STATUS )

*       get an output image structure
         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, DIMS, LOCO, STATUS )

*       if no error so far then continue
         IF ( STATUS .EQ. SAI__OK ) THEN

*          map its DATA_ARRAY component onto a pointer
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :               PNTRO, NELEMENTS, STATUS )

*          check status before accessing pointers
            IF ( STATUS .EQ. SAI__OK ) THEN

*             now call the subroutine that does the actual work
               CALL LAPLACESUB( %VAL( PNTRI ), DIMS(1), DIMS(2),
     :                 NUMBER, %VAL( PNTRO ), STATUS )

            END IF

*          tidy up output data structures
            CALL NDF_ANNUL( LOCO, STATUS )

*       end of if-no-error-after-getting-output-structure check
         END IF

*       tidy up input image structure
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-no-error-after-getting-input-structure check
      END IF


*    end
      END
