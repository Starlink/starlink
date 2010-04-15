*+  RAPGAU - performs a rapid 2-D gaussian smooth on a 2-D array
      SUBROUTINE RAPGAU( SIGMA, NPIX, DIMS1, DIMS2, ARRAY, ROLL, RBUF,
     :  WEIGHT, RMARGE, STATUS )
*    Description :
*     Replaces each pixel of the 2-D array, ARRAY, by the weighted mean of
*     the NPIX by NPIX pixels centred on it. NPIX must be odd. The weighting
*     function is a gaussian of standard deviation SIGMA pixels.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*      CALL RAPGAU( SIGMA, NPIX, DIMS, ARRAY, ROLL, RBUF, WEIGHT, RMARGE,
*     :  STATUS )
*    Parameters :
*     SIGMA = REAL( READ )
*           Standard deviation of the gaussian to be used for smoothing.
*     NPIX = INTEGER( READ )
*           Size, in pixels, of the box over which the gaussian smoothing
*           profile will be applied.
*     DIMS( 2 ) = INTEGER( READ )
*           Dimensions of the data array holding the data to be smoothed.
*     ARRAY( DIMS(1), DIMS(2) ) = REAL( UPDATE )
*           Array which contains data to be smoothed on entry and contains the
*           smoothed data on exit.
*     ROLL( DIMS(1), NPIX ) = REAL( WRITE )
*           Workspace for the 2-D rolling buffer.
*     RBUF( NPIX ) = REAL( WRITE )
*           Workspace for the 1-D rolling buffer.
*     WEIGHT( NPIX ) = REAL( WRITE )
*           Workspace for the gaussian weighting function.
*     RMARGE( DIMS(1) ) = REAL( WRITE )
*           Workspace for the marginal sum of ROLL.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur.
*    Method :
*     If no error on entry then
*        Initialize 2-D rolling buffer as the bottom part of the data array,
*        centred on the row 'below' the bottom of the data array, padded by
*        duplicating the bottom row of the array.
*        For each row of the data array
*           If clear of top edge
*              Update 2-D rolling buffer to contain the NPIX original data
*              rows centred on the current row.
*           Else
*              Update 2-D rolling buffer by replicating last row of data array
*           Endif
*           Put the column weighted means of the 2-D rolling buffer into a 1-D
*           array.
*           For each point along the current row
*              New value for point is the weighted mean of the NPIX column
*              weighted means centred on the current point.
*              If the current point is within NPIX/2 points of the start or end
*              of the row then the first or last column weighted mean for the
*              row is replicated.
*           Endfor
*        Endfor
*     Endif
*    Authors :
*     B.D.Kelly   (ROE::BDK)
*     Dave Baines (ROE::ASOC5)
*    History :
*     05/08/1981 : Original version                     (ROE::BDK)
*     21/11/1983 : Conversion to SSE                    (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     11-Aug-1994  Changed input DIMS so that routine will compile (SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  DIMS1,   ! dimensions of the data array
     :  DIMS2,   ! dimensions of the data array
     :  NPIX     ! size of square smoothing area; odd number
      REAL
     :  SIGMA ! sigma of gaussian profile for smooth, in pixels
*    Import-Export :
      REAL
     :  ARRAY( DIMS1, DIMS2 ) ! data array to be smoothed
*    Export :
      REAL
     :  ROLL( DIMS1, NPIX ), ! workspace for 2-D rolling buffer
     :  RMARGE( DIMS1 ),     ! workspace for marginal sum of ROLL
     :  WEIGHT( NPIX ),        ! workspace for weights
     :  RBUF( NPIX )           ! workspace for 1-D rolling buffer
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  HNPIX,  ! half size of smoothing area rounded down
     :  HNPIX1, !  "    "    "     "       "     "    up
     :  X,      ! index to first dimension of arrays
     :  Y,      !   "    " second    "      "   "
     :  FIRST,  ! position of first row of array in 2-D buffer
     :  YROLL,  ! index to oldest row in 2-D buffer
     :  YPOS,   ! index into rolling buffer
     :  WT      ! index to elements of weight buffer
      REAL
     :  SUM,    ! sum of weights used in normalisation
     :  CDIST   ! distance from centre of gaussian
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       calculate half smoothing box size rounded down and up
         HNPIX = NPIX / 2
         HNPIX1 = ( NPIX + 1 ) / 2

*       initialize weighting function
         SUM = 0.0
         DO X = 1, NPIX

*          calculate X, the + or - distance from centre of gaussian
            CDIST = REAL( X - HNPIX - 1 )

*          weight is the value of the gaussian at position X
            WEIGHT( X ) = EXP( ( -0.5 * CDIST * CDIST ) /
     :        ( SIGMA * SIGMA ) )

*          update the sum of the weights
            SUM = SUM + WEIGHT( X )
         ENDDO

*       normalize weights
         DO X = 1, NPIX

            WEIGHT( X ) = WEIGHT( X ) / SUM
         ENDDO

*       initialize 2-D rolling buffer such that it is centred on the row
*       "before" the first row of the image

*       calculate the position of the first row of the image in the 2-D buffer
         FIRST = HNPIX1 + 1

*       for all rows of 2-D buffer up to FIRST copy in first row of image
         DO Y = 1, FIRST
            DO X = 1, DIMS1

               ROLL( X, Y ) = ARRAY( X, 1 )
            ENDDO
         ENDDO

*       for remaining rows of 2-D buffer copy in 2nd to HNPIXth rows of image
         DO Y = FIRST+1, NPIX
            DO X = 1, DIMS1

               ROLL( X, Y ) = ARRAY( X, 1+Y-FIRST )
            ENDDO
         ENDDO

*       calculate the means row-by-row
         DO Y = 1, DIMS2

*          calculate index of oldest row in 2-D buffer
            YROLL = MOD( Y-1, NPIX ) + 1

*          roll the 2-D buffer
            IF( Y .LE. ( DIMS2-HNPIX ) ) THEN

*             buffer is still in main body of the image so oldest row is
*             replaced by the next row of image
               DO X = 1, DIMS1

                  ROLL( X, YROLL ) = ARRAY( X, Y+HNPIX )
               ENDDO
            ELSE

*             part of buffer is now past last row of image so replicate
*             last row of image
               DO X = 1, DIMS1

                  ROLL( X, YROLL ) = ARRAY( X, DIMS2 )
               ENDDO
            ENDIF

*          calculate weighted means in Y-direction

*          initialize array to hold the sums of weighted Y values
            DO X = 1, DIMS1

               RMARGE( X ) = 0.0
            ENDDO

*          apply the weighting function to the 2-D rolling buffer
            DO WT = 1, NPIX

*             calculate position in rolling buffer corresponding to weight WT
               YPOS = MOD( YROLL+WT-1, NPIX ) + 1

*             move along 2-D buffer updating the sums of weighted values
               DO X = 1, DIMS1

                  RMARGE( X ) = RMARGE( X ) +
     :              ( WEIGHT( WT ) * ROLL( X, YPOS ) )
               ENDDO
            ENDDO

*          do 1-D smooth in X-direction. the ends of the data are padded
*          by pixel replication

*          for the first NPIX/2 pixels, when applying weighting function
*          to pixels "before" the first pixel in the row then replicate
*          the first pixel
            DO X = 1, HNPIX

               ARRAY( X, Y ) = 0.0
               DO WT = 1, HNPIX-X+1

                  ARRAY( X, Y ) = ARRAY( X, Y) +
     :              ( WEIGHT( WT ) * RMARGE( 1 ) )
               ENDDO

               DO WT = HNPIX+2-X, NPIX

                  ARRAY( X, Y ) = ARRAY( X, Y ) +
     :              ( WEIGHT( WT ) * RMARGE( X-HNPIX-1+WT ) )
               ENDDO
            ENDDO

*          while clear of the ends of the row, no pixel replication is needed
            DO X = HNPIX+1, DIMS1-HNPIX

               ARRAY( X, Y ) = 0.0
               DO WT = 1, NPIX

                  ARRAY( X, Y ) = ARRAY( X, Y ) +
     :              ( WEIGHT( WT ) * RMARGE( X-HNPIX-1+WT ) )
               ENDDO
            ENDDO

*          for the last NPIX/2 pixels, when applying the weighting function
*          to pixels "after" the last pixel in the row then replicate the
*          last pixel
            DO X = DIMS1-HNPIX+1, DIMS1

               ARRAY( X, Y ) = 0.0
               DO WT = HNPIX+2+DIMS1-X, NPIX

                  ARRAY( X, Y ) = ARRAY( X, Y ) +
     :              ( WEIGHT( WT ) * RMARGE( DIMS1 ) )
               ENDDO
               DO WT = 1, HNPIX+1+DIMS1-X

                  ARRAY( X, Y ) = ARRAY( X, Y ) +
     :              ( WEIGHT( WT ) * RMARGE( X-HNPIX-1+WT ) )
               ENDDO
            ENDDO
         ENDDO
      ENDIF

      END
