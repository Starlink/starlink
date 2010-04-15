
*+  RAPBLO - performs a rapid 2-D block smooth on a 2-D array
      SUBROUTINE RAPBLO( NPIX, DIMS1, DIMS2, ARRIN, ARROUT, ROLL,
     :  RMARGE, STATUS )
*    Description :
*     Each pixel in the 2-D output array ARROUT is the mean of the NPIX by NPIX
*     pixels centred on the corresponding pixel in the 2-D input array ARRIN.
*     NPIX must be odd. Pixel replication is used to allow the smoothing to be
*     performed right up to the edge of the array.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL RAPBLO( NPIX, DIMS, ARRIN, ARROUT, ROLL, RMARGE, STATUS )
*    Parameters :
*     NPIX = INTEGER( READ )
*           Size of the smoothing box in pixels.
*     DIMS( 2 ) = INTEGER( READ )
*           Dimensions of the input and output arrays.
*     ARRIN( DIMS(1), DIMS(2) ) = REAL( READ )
*           Array of data to be smoothed.
*     ARROUT( DIMS(1), DIMS(2) ) = REAL( WRITE )
*           Array to hold the smoothed data.
*     ROLL( DIMS(1), NPIX ) = REAL( WRITE )
*           Workspace array to contain the 2-D rolling buffer.
*     RMARGE( DIMS(1) ) = REAL( WRITE )
*           Workspace array to contain the marginal sums.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur.
*    Method :
*      Initialize 2-D rolling buffer as the bottom part of the data array,
*      centred on the row 'below' the bottom row of the data array, padded
*      by duplicating the bottom row of the array.
*      For each row of the data array
*        If clear of top edge
*          Update 2-D rolling buffer to contain the NPIX original data rows
*            centred on the current row.
*        Else
*          Propogate latest row added to 2-D rolling buffer by 1 row.
*        Endif
*        Put the column sum of the 2-D rolling buffer into a 1-D array
*        Initialize 1-D rolling buffer
*        For each point in the 1-D array
*          If clear of end
*            Update 1-D rolling buffer to contain the NPIX elements centred
*              on the current element.
*          Else
*            Propogate latest element added to 1-D rolling buffer by 1 element.
*          Endif
*          Replace the corresponding point in the output data array
*          by (sum of 1-D rolling buffer)/(NPIX**2)
*        Endfor
*      Endfor
*    Authors :
*     B.D.Kelly   (ROE::BDK)
*     Dave Baines (ROE::ASOC5)
*    History :
*     05/08/1981 : Original version                     (ROE::BDK)
*     21/11/1983 : Conversion to SSE                    (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     12-Aug-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  DIMS1, DIMS2, ! dimensions of input/output arrays
     :  NPIX       ! size of square smoothing area; odd no.
      REAL
     :  ARRIN( DIMS1, DIMS2 ) ! data array
*    Import-Export :
      REAL
     :  ROLL( DIMS1, NPIX ), ! 2-D rolling buffer
     :  RMARGE( DIMS1 )      ! marginal sum of ROLL
*    Export :
      REAL
     :  ARROUT( DIMS1, DIMS2 ) ! smoothed data array
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  HNPIX,  ! half size of smoothing area, rounded down
     :  HNPIX1, !  "    "    "     "       "      "    up
     :  X,      ! index to first dimension of arrays
     :  Y,      !   "    " second    "      "    "
     :  FIRST,  ! initial position of first row of data array in 2-D buffer
     :  YROLL   ! index of oldest row in 2-D buffer
      REAL
     :  AREA, ! area of the smoothing box ( square of NPIX )
     :  SUM   ! sum used in calculating mean values
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       calculate half size of smoothing area, both rounded down and up
         HNPIX = NPIX / 2
         HNPIX1 = ( NPIX + 1 ) / 2

*       calculate area of smoothing box
         AREA = REAL( NPIX * NPIX )

*       initialize 2-D rolling buffer, centre it on row "below" first row of
*       data array by replicating first row of data array

*       calculate the position of first row of the data array in the 2-D buffer
         FIRST = ( HNPIX1 ) + 1

*       replicate first row of data array into 1 to FIRST rows of 2-D buffer
         DO Y = 1, FIRST
            DO X = 1, DIMS1

               ROLL( X, Y ) = ARRIN( X, 1 )
            ENDDO
         ENDDO

*       put the 2nd to ( NPIX-FIRST ) rows of the data array into the
*       ( FIRST+1 ) to NPIX rows of the 2-D buffer
         DO Y = FIRST+1, NPIX
            DO X = 1, DIMS1

               ROLL( X, Y ) = ARRIN( X, 1+Y-FIRST )
            ENDDO
         ENDDO

*       initialize the sum of the columns of the rolling buffer.
*       first set all the totals to zero
         DO X = 1, DIMS1

            RMARGE( X ) = 0.0
         ENDDO

*       calculate the totals for each column
         DO Y = 1, NPIX
            DO X = 1, DIMS1

               RMARGE( X ) = RMARGE( X ) + ROLL( X, Y )
            ENDDO
         ENDDO

*       calculate the means row-by-row
         DO Y = 1, DIMS2

*          calculate index of oldest row in 2-D buffer
            YROLL = MOD( ( Y-1 ), NPIX ) + 1

*          subtract the oldest row of the buffer from the column sum.
            DO X = 1, DIMS1

               RMARGE( X ) = RMARGE( X ) - ROLL( X, YROLL )
            ENDDO

*          roll the 2-D buffer
            IF( Y .LE. ( DIMS2 - HNPIX ) ) THEN

               DO X = 1, DIMS1

                  ROLL( X, YROLL ) = ARRIN( X, Y+HNPIX )
               ENDDO
            ELSE

               DO X = 1, DIMS1

                  ROLL( X, YROLL ) = ARRIN( X, DIMS2 )
               ENDDO
            ENDIF

*          add the newest row of the buffer to the column sum
            DO X = 1, DIMS1

               RMARGE( X ) = RMARGE( X ) + ROLL( X, YROLL )
            ENDDO

*          calculate means from 1-D column totals array
*          first initialize SUM such that it represents the sum of NPIX
*          columns centred on the column immediatly before the first column
*          this is done by replicating the first column total
            SUM = RMARGE( 1 ) * FLOAT( HNPIX1 )
            DO X = 1, HNPIX

               SUM = SUM + RMARGE( X )
            ENDDO

*          move through data updating sum and calculating smoothed values
            DO X = 1, HNPIX1

*             lefthand edge of data array so add in column total
*             from the right and drop replicated first column total
               SUM = SUM + RMARGE( X+HNPIX ) - RMARGE( 1 )
               ARROUT( X, Y ) = SUM / AREA
            ENDDO

            DO X = HNPIX1+1, DIMS1-HNPIX

*             in main body of data so drop column total from lefthand
*             side and pick up column total from righthand side
               SUM = SUM + RMARGE( X+HNPIX ) - RMARGE( X-HNPIX1 )
               ARROUT( X, Y ) = SUM / AREA
            ENDDO

            DO X = DIMS1-HNPIX+1, DIMS1

*             righthand edge of data array so replicate last column
*             total and drop column total from the left
               SUM = SUM + RMARGE( DIMS1 ) - RMARGE( X-HNPIX1 )
               ARROUT( X, Y ) = SUM / AREA
            ENDDO
         ENDDO
      ENDIF

      END
