*+  SQSHY - squashes the input array into the output array in the
*           Y-direction

      SUBROUTINE SQSHY( IDIM1, IDIM2, ARRIN, ODIM1, ODIM2, ARROUT,
     :                  YPIX, YWT, STATUS )
*
*    Description :
*
*     The input array, ARRIN, is squashed in the Y (second dimension)
*     direction into the output array, ARROUT, by calculating each pixel
*     in ARROUT as the mean of the corresponding pixels in ARRIN.
*     An immediate return will occur if STATUS has an error value on
*     entry.
*
*    Invocation :
*
*     CALL SQSHY( IDIM1, IDIM2, ARRIN, ODIM1, ODIM2, ARROUT, YPIX, YWT,
*    :            STATUS )
*
*    Arguments :
*
*     IDIM1 = INTEGER( READ )
*         The first dimension of the input 2-d array.
*     IDIM2 = INTEGER( READ )
*         The second dimension of the input 2-d array.
*     ARRIN( IDIM1, IDIM2 ) = REAL( READ )
*         Array of data that will be averaged to produced squashed
*           array.
*     ODIM1 = INTEGER( READ )
*         The first dimension of the output 2-d array.
*     ODIM2 = INTEGER( READ )
*         The second dimension of the output 2-d array.
*     ARROUT( ODIM1, ODIM2 ) = REAL( WRITE )
*         Array to contain the squashed data.
*     YPIX( ODIM2, 2 ) = INTEGER( WRITE )
*         Workspace array of pixel limits for averaging input array
*           data (second dimension is for left (1) and right (2)).
*     YWT( ODIM2, 2 ) = REAL( WRITE )
*         Workspace array of weights corresponding to pixel limits
*           (second dimension is for left (1) and right (2)).
*     STATUS = INTEGER( READ )
*         This is the global status, if this variable has an error
*           value on entry then an immediate return will occur.
*
*    Method :
*
*     If no error on entry then
*        If input image second dimension is not an integral multiple of
*          the output image second dimension then
*           Call SQSHS to set up pixel limits and weights
*           For all lines of output image
*              For all points in line
*                 Initialise sum and total weight to zero
*                 If pixel is valid then
*                    Add first pixel for averaging times its weight to
*                      sum
*                    Increment weight
*                 Endif
*                 Add in valid unit-weighted pixels, if any, to sum and
*                   increment weight
*                 If pixel is valid then
*                    Add in last pixel for averaging times its weight
*                    Increment weight
*                 Endif
*                 If total weight is greater than one pixel then
*                    Output image pixel is sum divided by total weight
*                 Else
*                    Output image pixel is invalid
*                 Endif
*              Endfor
*           Endfor
*        Else
*           Set up integer and real values of ratio of input/output
*             image second dimensions
*           Calculate pixel limits
*           For all lines of output image
*              For all points of line
*                 Initialise sum and weight to zero
*                 For all valid input image pixels between pixel limits
*                    Add input image pixel into the sum
*                    Increment weights
*                 Endfor
*                 If total weight is greater than 1.0 then
*                    Output image pixel is sum divided by ratio of
*                      dimensions
*                 Else
*                    Output image pixel is invalid
*                 Endif
*              Endfor
*           Endfor
*        Endif
*     Endif
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     18/08/1983 : Original version                     (ROE::ASOC5)
*     19/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     1986 Sep 10: Renamed parameters section to arguments, added
*                  invalid-pixel handling and tidied (RL.STAR::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*     1990 Jul 11: Corrected bug if there is no squashing (weight is
*                  one) (RL.STAR::CUR).
*
*    Type Definitions :

      IMPLICIT NONE

*    Global constants :

      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*    Import :

      INTEGER
     :  IDIM1, IDIM2,
     :  ODIM1, ODIM2,
     :  YPIX( ODIM2, 2 )

      REAL
     :  ARRIN( IDIM1, IDIM2 ),
     :  YWT( ODIM2, 2 )

*    Export :

      REAL
     :  ARROUT( ODIM1, ODIM2 )

*    Status :

      INTEGER STATUS

*    Local variables :

      INTEGER
     :  INTRAT,                ! Integer truncated ratio ODIM2/IDIM2
     :  CURPIX,                ! Index to pixel currently being accessed
     :  INY,                   !   "    "   "   from input image
     :  X,                     !   "    "   "   in output image, 1st dim
     :  Y                      !   "    "   "   in input/output images,
                               ! 2nd dimension

      REAL
     :  YRATIO,                ! Ratio ODIM2/IDIM2
     :  INARR,                 ! Temporary storage of input pixel (for
                               ! efficiency)
     :  WEIGHT,                ! Running total of weight for calculation
                               ! of mean
     :  SUM                    ! Running total for calculation of mean
*-

*    check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

         IF ( MOD( IDIM2, ODIM2 ) .NE. 0 ) THEN

*          input image second dimension is not an integral multiple of
*          the output image second dimension, set up pixel limits for
*          averaging, weighting values at the pixel limits and ratio of
*          dimensions

            CALL SQSHS( IDIM2, ODIM2, YRATIO, YPIX, YWT, STATUS )

*          do all lines of output image

            DO  Y = 1, ODIM2

*             do all points in line

               DO  X = 1, ODIM1

*                initialise sum and weight

                  SUM = 0.0
                  WEIGHT = 0.0

                  INARR = ARRIN ( X, YPIX( Y, 1 ) )

*                test if weighted first pixel is valid, and if it is
*                increment sum and weight using left-hand limiting
*                pixel's weighting factor

                  IF ( INARR .NE. VAL__BADR ) THEN
                     SUM = YWT( Y, 1 ) * INARR
                     WEIGHT = WEIGHT + YWT( Y, 1 )
                  END IF

*                point to the next pixel along

                  CURPIX = YPIX( Y, 1 ) + 1

*                add in valid unit-weighted pixels, if any

                  DO WHILE ( CURPIX .LT. YPIX( Y, 2 ) )

                     IF ( ARRIN( X, CURPIX ) .NE. VAL__BADR ) THEN
                        SUM = SUM + ARRIN( X, CURPIX )
                        WEIGHT = WEIGHT + 1.0
                     END IF

                     CURPIX = CURPIX + 1
                  END DO

                  INARR = ARRIN ( X, YPIX( Y, 2 ) )

                  IF ( INARR .NE. VAL__BADR ) THEN

*                    add in weighted last pixel for averaging and
*                    increment weight

                      SUM = SUM + ( YWT( Y, 2 ) * INARR )
                      WEIGHT = WEIGHT + YWT( Y, 2 )
                   END IF

*                if at least one valid pixel has been used (allow for
*                possible rounding by the two factor)

                  IF ( WEIGHT .GT. 1.0 - 2*VAL__EPSR ) THEN

*                output pixel value is weighted sum divided by total
*                weight

                     ARROUT( X, Y ) = SUM / WEIGHT
                  ELSE
                     ARROUT( X, Y ) = VAL__BADR
                  END IF
               END DO
            END DO
         ELSE

*          input image second dimension is an integral multiple of
*          output-image second dimension. calculate integer and real
*          values of the ratio

            INTRAT = IDIM2 / ODIM2
            YRATIO = REAL( INTRAT )

*          set up the pixel limits for averaging

            DO  Y = 1, ODIM2
               YPIX( Y, 1 ) = (( Y - 1 ) * INTRAT ) + 1
               YPIX( Y, 2 ) = YPIX( Y, 1 ) + INTRAT - 1
            END DO

*         do all lines of the output image

            DO  Y = 1, ODIM2

*             do all points of line

               DO  X = 1, ODIM1

*                initialise sum and weight to zero

                  SUM = 0.0
                  WEIGHT = 0.0

*                add in all valid pixels between pixel limits for
*                averaging

                  DO  INY = YPIX( Y, 1 ), YPIX( Y, 2 )

                     IF ( ARRIN ( X, INY ) .NE. VAL__BADR ) THEN
                        SUM = SUM + ARRIN( X, INY )
                        WEIGHT = WEIGHT + 1.0
                     END IF

                  END DO

*                if at least one valid pixel has been used (allow for
*                possible rounding by the two factor)

                  IF ( WEIGHT .GT. 1.0 - 2*VAL__EPSR ) THEN

*                output pixel value is sum of input pixel values divided
*                by the ratio of dimensions

                     ARROUT( X, Y ) = SUM / WEIGHT
                  ELSE

*                output pixel is invalid

                     ARROUT( X, Y ) = VAL__BADR
                  END IF
               END DO
            END DO
         END IF
      END IF

      END
