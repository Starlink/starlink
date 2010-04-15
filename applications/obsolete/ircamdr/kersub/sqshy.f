*+  SQSHY - squashes the input array into the output array in the Y-direction
      SUBROUTINE SQSHY( IDIMS1, IDIMS2, ARRIN, ODIMS1, ODIMS2, ARROUT,
     :                  YPIX, YWT, STATUS )
*    Description :
*     The input array, ARRIN, is squashed in the Y ( second dimension )
*     direction into the output array, ARROUT, by calculating each pixel
*     in ARROUT as the mean of the corresponding pixels in ARRIN.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL SQSHY( IDIMS, ARRIN, ODIMS, ARROUT, YPIX, YWT, STATUS )
*    Parameters :
*     IDIMS( 2 ) = INTEGER( READ )
*           Dimensions of the input data array ARRIN.
*     ARRIN( IDIMS(1), IDIMS(2) ) = REAL( READ )
*           Data to be averaged to produce squashed array.
*     ODIMS( 2 ) = INTEGER( READ )
*           Dimensions of the output data array ARROUT.
*     ARROUT( ODIMS(1), ODIMS(2) ) = REAL( WRITE )
*           Will contain the averaged input data array.
*     YPIX( ODIMS(2), 2 ) = INTEGER( WRITE )
*           Workspace array containing pixel limits for averaging.
*     YWT( ODIMS(2), 2 ) = REAL( WRITE )
*           Workspace array containing weights corresponding to pixel limits.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur.
*    Method :
*     If no error on entry then
*        If input image second dimension is not an integral multiple of
*          output image second dimension then
*           Call SQSHS to set up pixel limits and weights
*           For all rows of output image
*              For all points in row
*                 Initialise sum as first pixel for averaging times its weight
*                 Add in unit weighted pixels, if any
*                 Add in last pixel for averaging times its weight
*                 Output image pixel is sum divided by total weight
*              Endfor
*           Endfor
*        Else
*           Set up integer and real values of ratio of input/output image
*             first dimensions
*           Set up the pixel limits
*           For all rows of output image
*              For all points of row
*                 Initialise sum to zero
*                 For all pixels in input image between pixel limits
*                    Add input image pixel into sum
*                 Endfor
*                 Output image pixel is sum divided by ratio of dimensions
*              Endfor
*           Endfor
*        Endif
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     18/08/1983 : Original version                     (ROE::ASOC5)
*     19/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     12-AUG-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  IDIMS1, IDIMS2, ! dimensions of input array
     :  ODIMS1, ODIMS2, !      "      " output array
     :  YPIX( ODIMS2, 2 ) ! array of pixel limits for averaging
      REAL
     :  ARRIN( IDIMS1, IDIMS2 ), ! input array
     :  YWT( ODIMS2, 2 )           ! array of weights for averaging
*    Export :
      REAL
     :  ARROUT( ODIMS1, ODIMS2 ) ! output array
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  INTRAT, ! integer truncated ratio IDIMS1/ODIMS1
     :  CURPIX, ! pixel currently being averaged
     :  INY,    ! pixel being accessed in input image, 2nd dim.
     :  X,      ! index to output array element, 1st dimension
     :  Y       !   "    "    "     "       "  , 2nd     "
      REAL
     :  YRATIO, ! ratio IDIMS1/ODIMS1
     :  SUM     ! running total for calculating means
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

         IF( MOD( IDIMS2, ODIMS2 ) .NE. 0 ) THEN

*          input image second dimension is not an integral multiple of
*          output image second dimension.
*          set up pixel limits for averaging, weighting values at pixel
*          limits and ratio of dimensions
            CALL SQSHS( IDIMS2, ODIMS2, YRATIO, YPIX, YWT,
     :        STATUS )

*          do all rows of output image
            DO Y = 1, ODIMS2

*             do all points in row
               DO X = 1, ODIMS1

*                initialise sum as weighted first pixel for averaging
                  SUM = YWT( Y, 1 ) * ARRIN( X, YPIX( Y, 1 ) )

*                point to next pixel along
                  CURPIX = YPIX( Y, 1 ) + 1

*                add in unit weighted pixels, if any
                  DO WHILE ( CURPIX .LT. YPIX( Y, 2 ) )

                     SUM = SUM + ARRIN( X, CURPIX )
                     CURPIX = CURPIX + 1
                  ENDDO

*                add in weighted last pixel for averaging
                  SUM = SUM + ( YWT( Y, 2 ) * ARRIN( X, YPIX( Y, 2 ) ) )

*                output pixel value is sum of weighted input pixels divided
*                by the total weight
                  ARROUT( X, Y ) = SUM / YRATIO
               ENDDO
            ENDDO
         ELSE

*          input image second dimension is integral multiple of output
*          image second dimension
*          calculate integer and real values of ratio
            INTRAT = IDIMS2 / ODIMS2
            YRATIO = REAL( INTRAT )

*          do all rows of output image
            DO Y = 1, ODIMS2

*             set up pixel limits
               YPIX( Y, 1 ) = ( ( Y - 1 ) * INTRAT ) + 1
               YPIX( Y, 2 ) = YPIX( Y, 1 ) + INTRAT - 1

*             do all points in row
               DO X = 1, ODIMS1

*                initialise sum to zero
                  SUM = 0.0

*                add in all pixels between the limits
                  DO INY = YPIX( Y, 1 ), YPIX( Y, 2 )

                     SUM = SUM + ARRIN( X, INY )
                  ENDDO

*                output pixel is sum of input pixel values divided by
*                ratio of dimensions
                  ARROUT( X, Y ) = SUM / YRATIO
               ENDDO
            ENDDO
         ENDIF
      ENDIF

      END
