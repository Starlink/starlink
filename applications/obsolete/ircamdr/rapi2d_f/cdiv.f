
*+  CDIV - divides an image by a scalar pixel by pixel

      SUBROUTINE CDIV ( STATUS )

*    Description :
*
*     This routine divides each pixel of an input image
*     by a scalar. The result goes into a new output image.
*
*    Invocation :
*
*     CALL CDIV( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*         Input image
*     SCALAR  =  REAL( READ )
*         Scalar that each input image pixel is to be divided by
*     OUTPIC  =  IMAGE( WRITE )
*         Output image holding result of image divided by scalar
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
*           Get scalar to be used in division, making sure it is between
*            specified limits
*           If no error then
*              Create output image structure
*              If no error then
*                 Map a new data array component in output
*                 If no error then
*                    Call subroutine to divide image by scalar,
*                     results going into output image
*                    If status bad on return then
*                       Scalar passed was too small - output image is
*                       just copy of input - report the error
*                    Endif
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
*     27-06-1986 : First implementation (REVA::MJM)
*     10-Mar-94    Changed DAT_, CMP_ calls to NDF_
*     12-AUG-1994  Changed input DIM arguments for DIVSCA2D (SKL@JACH)
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
     :    MAXSCAL,                ! maximum scalar allowed in division
     :    MINSCAL,                ! minimum    "      "     "     "
     :    SMALLEST                ! smallest   "      "     "     "
      PARAMETER( MAXSCAL   =  1.0E20  )
      PARAMETER( MINSCAL   = -1.0E20  )
      PARAMETER( SMALLEST  =  1.0E-20 )

*    Local variables :

      INTEGER
     :    NELEMENTS,              ! Number elements mapped
     :    NDIM,                   ! Number dimensions fom NDF_DIM
     :    IDIMS( NDIMS ),         ! dimensions of input image
     :    PNTRI,                  ! pointer to input data
     :    PNTRO                   !    "     " output data

      REAL
     :    SCALAR                  ! scalar to divide each pixel
                                  ! of input image by

      LOGICAL                     ! true if :
     :    VALID                   ! a valid scalar divisor has been got

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

*       get dimensions image
         CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS)

*       if no error then continue
         IF ( STATUS .EQ. SAI__OK ) THEN

*          set valid scalar flag false to start with
            VALID  =  .FALSE.

*          loop until a valid scalar has been got
            DO WHILE ( STATUS .EQ. SAI__OK .AND. .NOT. VALID )

*             try to get the scalar to be used in pixel division
               CALL AIF_GET0R( 'SCALAR', 1.0, MINSCAL, MAXSCAL, SCALAR,
     :                          STATUS )

*             the AIF_ call has made sure the number is between defined
*             large positive and large negative bounds, but division is
*             special, as we must not divide by too small a number, else
*             we will get an arithmentic overflow, and if we try to divide
*             by zero, we will get a divide by zero error (naturally enough)
               IF ( STATUS .EQ. SAI__OK .AND.
     :              ABS( SCALAR ) .GE. SMALLEST ) THEN

*                number is not too small - set valid flag
                  VALID  =  .TRUE.

               ELSE

*                number is too small - output message, cancel parameter
*                and loop again
                  CALL MSG_SETR( 'SCALAR', SCALAR )
                  IF (STATUS .NE. SAI__OK) THEN
                     CALL ERR_REP( 'TOO_SMALL',
     : 'Input scalar ^SCALAR is too small for division - try again',
     :  STATUS )
                     CALL ERR_FLUSH( STATUS )
                     CALL ERR_ANNUL( STATUS )
                  ELSE
                     CALL MSG_OUT( 'TOO_SMALL',
     : 'Input scalar ^SCALAR is too small for division - try again',
     :  STATUS )
                  END IF
                  CALL PAR_CANCL( 'SCALAR', STATUS )

*             end of if-scalar-is-not-too-small check
               END IF

*          end of loop until valid scalar found
            END DO

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

*                   call working subroutine to divide each pixel of
*                   input image by scalar, result going into output image
                     CALL DIVSCA2D( %VAL( PNTRI ), IDIMS(1), IDIMS(2),
     :                                 SCALAR, %VAL( PNTRO ), STATUS )

*                   check status on return from subroutine
                     IF ( STATUS .EQ. SAI__ERROR ) THEN

*                      the passed scalar was too small - output image
*                      just contains copy of input - output error
*                      message
                        CALL ERR_REP( 'ERR1',
     : ' Scalar passed to DIVSCALAR was outside limits - output',
     :   STATUS )
                        CALL ERR_REP( 'ERR2',
     : ' image is just a copy of the input image - no division done',
     :   STATUS )
                        CALL ERR_FLUSH( STATUS )

*                   end of if-error-on-return-from-subroutine check
                     END IF

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
