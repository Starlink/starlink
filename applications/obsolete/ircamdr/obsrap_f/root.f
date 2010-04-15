*+  ROOT - takes the square root of each pixel of an image

      SUBROUTINE ROOT ( STATUS )

*    Description :
*
*     This routine takes the square root of each pixel
*     of an input image. The result goes into a new output
*     image.
*
*    Invocation :
*
*     CALL ROOT( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*         Input image
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
*              Create output image structure
*              If no error then
*                 Map a new data array component in output
*                 If no error then
*                    Call subroutine to take square root of image,
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
*     Colin Aspin (UKIRT)
*
*    History :
*
*     23-01-1987 : First implementation (REVA::MJM)
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     11-OCT-1994  Changed ROOT2D arguments for UNIX compiler (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local Constants :

      INTEGER
     :    NDIMS                   ! dimensionality of images
      PARAMETER( NDIMS  =  2 )    ! 2-d only

*    Local variables :

      INTEGER
     :    IDIMS( NDIMS ),         ! dimensions of input image
     :    ACTDIM,                 ! actual dimensions from NDF_DIM
     :    NELEMENTS,              ! number of elements mapped by NDF_MAP
     :    PNTRI,                  ! pointer to input data
     :    PNTRO,                  !    "     " output data
     :    LOCI,                   ! locator for input image structure
     :    LOCO                    ! locator for output  "       "

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    try to get the input image structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    if no error then continue
      IF ( STATUS .EQ. SAI__OK ) THEN

*       map the input structure and get dimensions
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                 PNTRI, NELEMENTS, STATUS )
         CALL NDF_DIM( LOCI, NDIMS, IDIMS, ACTDIM, STATUS )

*       if no error then continue
         IF ( STATUS .EQ. SAI__OK ) THEN

*          if no error then continue
            IF ( STATUS .EQ. SAI__OK ) THEN

*             create an output image structure
               CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, IDIMS, LOCO,
     :                       STATUS )

*             if no error then continue
               IF ( STATUS .EQ. SAI__OK ) THEN

*                map a data array component
                  CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                          PNTRO, NELEMENTS, STATUS )

*                check status before accessing pointers
                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   call working subroutine to take root of each pixel
*                   of input image, result going into output image
                     CALL ROOT2D( %VAL( PNTRI ), IDIMS(1), IDIMS(2),
     :                            %VAL( PNTRO ), STATUS )

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

