*+  CADD - adds a scalar to each pixel of an image

      SUBROUTINE CADD ( STATUS )

*    Description :
*
*     This routine adds an input scalar to each pixel of an
*     input image. The result goes into a new output image.
*
*    Invocation :
*
*     CALL CADD( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*         Input image
*     SCALAR  =  REAL( READ )
*         Scalar to be added to every pixel of input image
*     OUTPIC  =  IMAGE( WRITE )
*         Output image holding result of image plus scalar
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
*           Get scalar to be added to image
*           If no error then
*              Create output image structure
*              If no error then
*                 Map a new data array component in output
*                 If no error then
*                    Call subroutine to add scalar to image,
*                     results going into output image
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
*     12-Aug-1994  Changed input DIM arguments for ADDSCA2D (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'NDF_PAR'
      INCLUDE  'NDF_ERR'          ! SSE global definitions

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local Constants :

      INTEGER
     :    NDIMS                   ! dimensionality of images
      PARAMETER( NDIMS  =  2 )    ! 2-d only

      REAL
     :    MAXSCAL,                ! maximum scalar allowed in addition
     :    MINSCAL                 ! minimum    "      "     "    "
      PARAMETER( MAXSCAL  =  1.0E30 )
      PARAMETER( MINSCAL  = -1.0E30 )

*    Local variables :

      INTEGER
     :    IDIMS( NDIMS ),         ! dimensions of input image
     :    NELEMENTS,              ! Number elements mapped
     :    NDIM,                   ! Number dimensions from NDF_DIM
     :    PNTRI,                  ! pointer to input data
     :    PNTRO                   !    "     " output data

      REAL
     :    SCALAR                  ! scalar to be added to each pixel
                                  ! of input image

      INTEGER                     ! locators for :
     :    LOCI,                   ! input image structure
     :    LOCO                    ! output  "       "

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

*       get image dimensions
         CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS)

*       if no error then continue
         IF ( STATUS .EQ. SAI__OK ) THEN

*          try to get the scalar to be added to each input image pixel
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

*                   call working subroutine to add scalar value to each
*                   pixel of input image, result going into output image
                     CALL ADDSCA2D( %VAL( PNTRI ), IDIMS(1), IDIMS(2),
     :                                SCALAR, %VAL( PNTRO ), STATUS )

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

*       tidy up the input image structure
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-no-error-after-getting-input-image check
      END IF


*    return and end
      END
