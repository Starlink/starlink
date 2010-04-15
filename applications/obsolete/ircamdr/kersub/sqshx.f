*+  SQSHX - squashes the input array into the output array in the X-direction
      SUBROUTINE SQSHX( IDIMS1, IDIMS2, ARRIN, ODIMS1, ODIMS2, ARROUT,
     :                  XPIX, XWT, STATUS )
*    Description :
*     The input array, ARRIN, is squashed in the X ( first dimension )
*     direction into the output array, ARROUT, by calculating each pixel
*     in ARROUT as the mean of the corresponding pixels in ARRIN.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL SQSHX( IDIMS, ARRIN, ODIMS, ARROUT, XPIX, XWT, STATUS )
*    Parameters :
*     IDIMS( 2 ) = INTEGER( READ )
*           Dimensions of input array ARRIN.
*     ARRIN( IDIMS(1), IDIMS(2) ) = REAL( READ )
*           Array of data that will be averaged to produced squashed array.
*     ODIMS( 2 ) = INTEGER( READ )
*           Dimensions of output array ARROUT.
*     ARROUT( ODIMS(1), ODIMS(2) ) = REAL( WRITE )
*           Array to contain the squashed data.
*     XPIX( ODIMS(1), 2 ) = INTEGER( WRITE )
*           Workspace array of pixel limits for averaging input array data.
*     XWT( ODIMS(1), 2 ) = REAL( WRITE )
*           Workspace array of weights corresponding to pixel limits.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur.
*    Method :
*     If no error on entry then
*        If input image first dimension is not an integral multiple of
*          the output image first dimension then
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
*           Calculate pixel limits
*           For all rows of output image
*              For all points of row
*                 Initialise sum to zero
*                 For all input image pixels between pixel limits
*                    Add input image pixel into the sum
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
*     12-Aug-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  IDIMS1, IDIMS2, ! dimensions of input array
     :  ODIMS1, ODIMS2, !      "      " output  "
     :  XPIX( ODIMS1, 2 ) ! array of pixel limits for averaging
      REAL
     :  ARRIN( IDIMS1, IDIMS2 ), ! input array
     :  XWT( ODIMS1, 2 )           ! array of weights for averaging
*    Export :
      REAL
     :  ARROUT( ODIMS1, ODIMS2 ) ! output array
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  INTRAT, ! integer truncated ratio ODIMS1/IDIMS1
     :  CURPIX, ! index to pixel currently being accessed
     :  INX,    !   "    "   "   from input image
     :  X,      !   "    "   "   in output image, 1st dimension
     :  Y       !   "    "   "   in input/output images, 2nd dim
      REAL
     :  XRATIO, ! ratio ODIMS1/IDIMS1
     :  SUM     ! running total for calculation of mean
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

         IF( MOD( IDIMS1, ODIMS1 ) .NE. 0 ) THEN

*          input image first dimension is not an integral multiple of the
*          output image first dimension, set up pixel limits for averaging,
*          weighting values at the pixel limits and ratio of dimensions
            CALL SQSHS( IDIMS1, ODIMS1, XRATIO, XPIX, XWT,
     :        STATUS )

*          do all rows of output image
            DO Y = 1, ODIMS2

*             do all points in row
               DO X = 1, ODIMS1

*                initialise sum as weighted first pixel for averaging
                  SUM = XWT( X, 1 ) * ARRIN( XPIX( X, 1 ), Y )

*                point to the next pixel along
                  CURPIX = XPIX( X, 1 ) + 1

*                add in unit weighted pixels, if any
                  DO WHILE ( CURPIX .LT. XPIX( X, 2 ) )

                     SUM = SUM + ARRIN( CURPIX, Y )
                     CURPIX = CURPIX + 1
                  ENDDO

*                add in weighted last pixel for averaging
                  SUM = SUM + ( XWT( X, 2 ) * ARRIN( XPIX( X, 2 ), Y ) )

*                output pixel value is weighted sum divided by total weight
                  ARROUT( X, Y ) = SUM / XRATIO
               ENDDO
            ENDDO
         ELSE

*          input image first dimension is an integral multiple of output
*          image first dimension. calculate integer and real values of
*          the ratio
            INTRAT = IDIMS1 / ODIMS1
            XRATIO = REAL( INTRAT )

*          set up the pixel limits for averaging
            DO X = 1, ODIMS1

               XPIX( X, 1 ) = (( X - 1 ) * INTRAT ) + 1
               XPIX( X, 2 ) = XPIX( X, 1 ) + INTRAT - 1
            ENDDO

*         do all rows of the output image
            DO Y = 1, ODIMS2

*             do all points of row
               DO X = 1, ODIMS1

*                initialise sum to zero
                  SUM = 0.0

*                add in all pixels between pixel limits for averaging
                  DO INX = XPIX( X, 1 ), XPIX( X, 2 )

                     SUM = SUM + ARRIN( INX, Y )
                  ENDDO

*                output pixel value is sum of input pixel values divided by
*                ratio of dimensions
                  ARROUT( X, Y ) = SUM / XRATIO
               ENDDO
            ENDDO
         ENDIF
      ENDIF

      END
