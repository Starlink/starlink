

*+  DIV2D - divides one image by another to form a new output image

      SUBROUTINE DIV2D ( STATUS )

*    Description :
*
*     This routine divides one image by another, pixel by pixel,
*     placing the result in a new output image. The two input images
*     must have the same dimensions.
*
*    Invocation :
*
*     CALL DIV2D( STATUS )
*
*    Parameters :
*
*     INPIC1  =  IMAGE( READ )
*         First input image
*     INPIC2  =  IMAGE( READ )
*         Second input image
*     OUTPIC  =  IMAGE( WRITE )
*         Output image holding result of first image divided by second
*     OTITLE  =  CHAR( READ )
*         Title string for output image structure
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get locator for first input structure
*     If no error then
*        Map its data array component
*        If no error then
*           Get locator for second input structure
*           If no error then
*              Map its data array component
*              If no error then
*                 If images are same size then
*                    Create output image structure
*                    If no error then
*                       Map a new data array component in output
*                       If no error then
*                          Call subroutine to divide first image by
*                           second, results going into output image
*                       Endif
*                       Tidy up output image structure
*                    Endif
*                 Else images are not same size
*                    Output error message to user
*                 Endif
*              Endif
*              Tidy up second input structure
*           Endif
*        Endif
*        Tidy up first input structure
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
*     10-Mar-94    Changed DAT_, CMP_ calls to NDF_ (SKL@JACH)
*     12-AUG-1994  Changed input DIM arguments for DIVARR2D (SKL@JACH)
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
     :    NDIMS                   ! dimensionality of input images
      PARAMETER( NDIMS  =  2 )    ! 2-d only

*    Local variables :

      INTEGER
     :    DIMS1( NDIMS ),         ! dimensions of first input image
     :    DIMS2( NDIMS ),         !      "      " second  "     "
     :    NELEMENTS,              ! number of elements mapped
     :    NDIM,                   ! number of dimensions from NDF_DIM
     :    PNTRI1,                 ! pointer to first input data
     :    PNTRI2,                 !    "     " second  "     "
     :    PNTRO                   !    "     " output data

      INTEGER                     ! locators for :
     :    LOCI1,                  ! first input image structure
     :    LOCI2,                  ! second  "     "       "
     :    LOCO                    ! output image structure

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    try to get the first input image structure
      CALL GETINP( 'INPIC1', LOCI1, STATUS )

*    if no error then continue
      IF ( STATUS .EQ. SAI__OK ) THEN

*       map the data array component of the first structure
         CALL NDF_MAP( LOCI1, 'DATA', '_REAL', 'READ',
     :                  PNTRI1, NELEMENTS, STATUS )

*       get dimensions of array
         CALL NDF_DIM( LOCI1, NDIMS, DIMS1, NDIM, STATUS)

*       if no error then continue
         IF ( STATUS .EQ. SAI__OK ) THEN

*          try to get the second input image structure
            CALL GETINP( 'INPIC2', LOCI2, STATUS )

*          if no error then continue
            IF ( STATUS .EQ. SAI__OK ) THEN

*             map the data array component of the second structure
               CALL NDF_MAP( LOCI2, 'DATA', '_REAL', 'READ',
     :                        PNTRI2, NELEMENTS, STATUS )

*             get dimensions of array
               CALL NDF_DIM( LOCI2, NDIMS, DIMS2, NDIM, STATUS)

*             if no error then continue
               IF ( STATUS .EQ. SAI__OK ) THEN

*                check to see if image dimensions match up
                  IF ( DIMS1( 1 ) .EQ. DIMS2( 1 ) .AND.
     :                 DIMS1( 2 ) .EQ. DIMS2( 2 ) ) THEN

*                   images are same size - try to get an ouput image
*                   for the results of division
                     CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, DIMS1,
     :                             LOCO, STATUS )

*                   if no error then continue
                     IF ( STATUS .EQ. SAI__OK ) THEN

*                      map a data array component
                        CALL NDF_MAP( LOCO, 'DATA', '_REAL',
     :                        'WRITE', PNTRO, NELEMENTS, STATUS )

*                      check status before accessing pointers
                        IF ( STATUS .EQ. SAI__OK ) THEN

*                         call working subroutine to divide first data
*                         array by second, result going into output
                           CALL DIVARR2D( %VAL(PNTRI1), %VAL(PNTRI2),
     :                            %VAL(PNTRO), DIMS1(1), DIMS1(2),
     :                            STATUS )

*                      end of if-no-error-before-accessing-pointers check
                        END IF

*                      tidy up output structure
                        CALL NDF_ANNUL( LOCO, STATUS )

*                   end of if-no-error-after-creating-output check
                     END IF

*                else the image dimensions are different
                  ELSE

*                   output error message
                     CALL MSG_OUT( 'BLANK', ' ', STATUS )
                     CALL MSG_OUT( 'ERR1',
     : ' Images do not have same dimensions - ', STATUS )
                     CALL MSG_SETI( 'XDIM1', DIMS1( 1 ) )
                     CALL MSG_SETI( 'YDIM1', DIMS1( 2 ) )
                     CALL MSG_OUT( 'ERR2',
     : ' first  input image is ^XDIM1 by ^YDIM1 pixels', STATUS )
                     CALL MSG_SETI( 'XDIM2', DIMS2( 1 ) )
                     CALL MSG_SETI( 'YDIM2', DIMS2( 2 ) )
                     CALL MSG_OUT( 'ERR3',
     : ' second input image is ^XDIM2 by ^YDIM2 pixels', STATUS )
                     CALL MSG_OUT( 'ERR4', ' - aborting ...', STATUS )
                     CALL MSG_OUT( 'BLANK', ' ', STATUS )

*                end of if-images-are-both-same-dimensions check
                  END IF

*             end of if-no-error-after-mapping-second-input check
               END IF

*             tidy up second input structure
               CALL NDF_ANNUL( LOCI2, STATUS )

*          end of if-no-error-after-getting-second-input check
            END IF

*       end of if-no-error-after-mapping-first-input check
         END IF

*       tidy up the first input structure
         CALL NDF_ANNUL( LOCI1, STATUS )

*    end of if-no-error-after-getting-first-input check
      END IF


*    return and end
      END
