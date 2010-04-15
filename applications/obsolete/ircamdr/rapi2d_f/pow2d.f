
*+  POW2D - takes the power of each pixel of an image (specified power)

      SUBROUTINE POW2D ( STATUS )

*    Description :
*
*     This routine takes the specified power of each pixel
*     of an input image. The result goes into a new output
*     image.
*
*    Invocation :
*
*     CALL POW2D( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*         Input image
*     POWER  =  REAL( READ )
*         Power to be taken of each input image pixel
*     OUTPIC  =  IMAGE( WRITE )
*         Output image holding result of processed image
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
*           Get power to be used
*           If no error then
*              Create output image structure
*              If no error then
*                 Map a new data array component in output
*                 If no error then
*                    Call subroutine to take power of image,
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
*     03-07-1986 : First implementation (REVA::MJM)
*     12-APR-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     14-Jul-1994  Changed arguments of POWARR2D (SKL@JACH)
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
     :    MAXPOW,                 ! maximum power allowed
     :    MINPOW                  ! minimum   "      "
      PARAMETER( MAXPOW  =  100 )
      PARAMETER( MINPOW  = -100 )

*    Local variables :

      INTEGER
     :    IDIMS( NDIMS ),         ! dimensions of input image
     :    NELEMENTS,              ! number of elements mapped by NDF_MAP
     :    NDIM,                   ! number of dimensions from NDF_DIM
     :    PNTRI,                  ! pointer to input data
     :    PNTRO,                  !    "     " output data
     :    LOCI,                   ! input image structure
     :    LOCO                    ! output  "       "

      REAL
     :    POWER                   ! power to be taken of each pixel
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

         CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS)

*       if no error then continue
         IF ( STATUS .EQ. SAI__OK ) THEN

*          try to get the power to apply to each input image pixel
            CALL AIF_GET0R( 'POWER', 2.0, MINPOW, MAXPOW, POWER,
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

*                   call working subroutine to take power of each pixel
*                   of input image, result going into output image
                     CALL POWARR2D( %VAL( PNTRI ), IDIMS(1), IDIMS(2),
     :                                POWER, %VAL( PNTRO ), STATUS )

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
