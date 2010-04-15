
*+  CSUB - subtracts a scalar from each pixel of an image

      SUBROUTINE CSUB ( STATUS )

*    Description :
*
*     This routine subtracts an input scalar from each pixel of an
*     input image. The result goes into a new output image.
*
*    Invocation :
*
*     CALL CSUB( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*         Input image
*     SCALAR  =  REAL( READ )
*         Scalar to be subtracted from every pixel of input image
*     OUTPIC  =  IMAGE( WRITE )
*         Output image holding result of image minus scalar
*     OTITLE  =  CHAR( READ )
*         Title string for output image structure
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get locator for input structure
*     If no error then
*        Map its data array component
*        If no error then
*           Get scalar to be subtracted from image
*           If no error then
*              Create output image structure
*              If no error then
*                 Map a new data array component in output
*                 If no error then
*                    Call subroutine to subtract scalar from
*                     image, results going into output image
*                 Endif
*                 Tidy up output image structure
*              Endif
*           Endif
*        Endif
*        Tidy up input structure
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
*     26-06-1986 : First implementation (REVA::MJM)
*     10-Mar-94    Changed DAT_, CMP_ calls to NDF_ (SKL@JACH)
*     12-AUG-1994  Changed input DIM arguments for subsca2d (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'NDF_PAR'
      INCLUDE  'NDF_ERR'

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local Constants :

      INTEGER
     :    NDIMS                   ! dimensionality of images
      PARAMETER( NDIMS  =  2 )    ! 2-d only

      REAL
     :    MAXSCAL,                ! maximum scalar allowed in subtraction
     :    MINSCAL                 ! minimum    "      "     "      "
      PARAMETER( MAXSCAL  =  1.0E30 )
      PARAMETER( MINSCAL  = -1.0E30 )

*    Local variables :

      INTEGER
     :    IDIMS( NDIMS ),         ! dimensions of input image
     :    NELEMENTS,              ! number of elements mapped
     :    NDIM,                   ! number of dimensions from NDF_DIM
     :    PNTRI,                  ! pointer to input data
     :    PNTRO                   !    "     " output data

      INTEGER                     ! locators for :
     :    LOCI,                   ! input image structure
     :    LOCO                    ! output  "       "

      REAL
     :    SCALAR                  ! scalar to be subtracted from each pixel
                                  ! of input image

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    try to get the input image structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    if no error then continue
      IF ( STATUS .EQ. SAI__OK ) THEN

*       map the data array component of the input structure
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )

*       get array dimensions
         CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS )

*       if no error then continue
         IF ( STATUS .EQ. SAI__OK ) THEN

*          try to get the scalar to be subtracted from each input image pixel
            CALL AIF_GET0R( 'SCALAR', 0.0, MINSCAL, MAXSCAL, SCALAR,
     :                       STATUS )

*          if no error then continue
            IF ( STATUS .EQ. SAI__OK ) THEN

*             create an output image structure
               CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, IDIMS, LOCO,
     :                       STATUS )

*             if no error then continue
               IF ( STATUS .EQ. SAI__OK ) THEN

*                map a data array component
                  CALL NDF_MAP( LOCO, 'DATA', '_REAL',
     :                        'WRITE', PNTRO, NELEMENTS, STATUS )

*                check status before accessing pointers
                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   call working subroutine to subtract scalar value from
*                   each pixel of input image, result going into output
                     CALL SUBSCA2D( %VAL( PNTRI ), IDIMS( 1), IDIMS( 2),
     :	                            SCALAR, %VAL( PNTRO ), STATUS )

*                end of if-no-error-before-accessing-pointers check
                  END IF

*                tidy up output structure
                  CALL NDF_ANNUL( LOCO, STATUS )

*             end of if-no-error-after-creating-output check
               END IF

*          end of if-no-error-after-getting-scalar check
            END IF

*       end of if-no-error-after-mapping-input-image check
         END IF

*       tidy up the input structure
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-no-error-after-getting-input-structure check
      END IF


*    return and end
      END
