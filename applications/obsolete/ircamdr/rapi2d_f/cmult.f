
*+  CMULT - multiplies an image by a scalar pixel by pixel

      SUBROUTINE CMULT ( STATUS )

*    Description :
*
*     This routine multiplies each pixel of an input image
*     by a scalar. The result goes into a new output image.
*
*    Invocation :
*
*     CALL CMULT( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*         Input image
*     SCALAR  =  REAL( READ )
*         Scalar that each input image pixel is to be multiplied by
*     OUTPIC  =  IMAGE( WRITE )
*         Output image holding result of image times scalar
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
*           Get scalar to be used in multiplication
*           If no error then
*              Create output image structure
*              If no error then
*                 Map a new data array component in output
*                 If no error then
*                    Call subroutine to multiply image by scalar,
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
*     10-MAR-94    Changed DAT_, CMP_ calls to NDF (SKL@JACH)
*     12-Aug-1994  Changed input DIM arguments for MULTSCA2D (SKL@JACH)
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
     :    MAXSCAL,                ! maximum scalar allowed in multiplication
     :    MINSCAL                 ! minimum    "      "     "        "
      PARAMETER( MAXSCAL  =  1.0E20 )
      PARAMETER( MINSCAL  = -1.0E20 )

*    Local variables :

      INTEGER
     :    IDIMS( NDIMS ),         ! dimensions of input image
     :    NELEMENTS,              ! Number elements mapped
     :    NDIM,                   ! Number of dimensions from NDF_DIM
     :    PNTRI,                  ! pointer to input data
     :    PNTRO                   !    "     " output data

      REAL
     :    SCALAR                  ! scalar to multiply each pixel
                                  ! of input image by

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
         CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS )

*       if no error then continue
         IF ( STATUS .EQ. SAI__OK ) THEN

*          try to get the scalar to be used in pixel multiplication
            CALL AIF_GET0R( 'SCALAR', 1.0, MINSCAL, MAXSCAL, SCALAR,
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

*                   call working subroutine to multiply each pixel of
*                   input image by scalar, result going into output image
                     CALL MULTSCA2D( %VAL( PNTRI ), IDIMS(1), IDIMS(2),
     :                                 SCALAR, %VAL( PNTRO ), STATUS )

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
