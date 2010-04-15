*+  GEN_MEND1F - Interpolate across bad pixels in 1-D real array.
      SUBROUTINE GEN_MEND1F( DIM, INPUT, INVAR, INQUAL, QUALITY,
     :  FLAGGED, FBAD, VARIANCE, OUTPUT, OUTVAR, OUTQUAL )
*    Description :
*     This routine "mends" a 1-D array by interpolating the data
*     values across bad pixel regions. A linear interpolation is used.
*     New variance values are calculated.
*
*     Note that the interpolated values will no longer have independent
*     variances.
*
*     Although this routine is designed to process 1-D arrays, it may
*     also work with 2-D arrays, as long as interpolation across columns
*     and not rows is acceptable.
*    Invocation :
*     CALL GEN_MEND1F( DIM, INPUT, INVAR, INQUAL, QUALITY,
*     :  FLAGGED, FBAD, VARIANCE, OUTPUT, OUTVAR, OUTQUAL )
*    Parameters
*     DIM           = INTEGER( READ )
*           Dimension of the arrays
*     INPUT( DIM )  = REAL( READ )
*           Input data array.
*     INVAR( DIM )  = REAL( READ )
*           Input variance array. (Only used if VARIANCE = .TRUE.)
*     INQUAL( DIM ) = BYTE( READ )
*           Input quality array. (Only used if QUALITY = .TRUE.)
*     QUALITY       = LOGICAL( READ )
*           .TRUE. if a data quality array is being used.
*     FLAGGED       = LOGICAL( READ )
*           .TRUE. if "magic" flagged values are being used.
*     FBAD          = REAL( READ )
*           If FLAGGED is .TRUE., the magic value being used.
*     VARIANCE      = LOGICAL( READ )
*           .TRUE. if a variance array is being used.
*     OUTPUT( DIM )  = REAL( WRITE )
*           Output data array.
*           This can be the same as the input array.
*     OUTVAR( DIM )  = REAL( WRITE )
*           Output variance array. (Only used if VARIANCE = .TRUE.)
*           This can be the same as the input array.
*     OUTQUAL( DIM ) = BYTE( WRITE )
*           Output quality array. (Only used if QUALITY = .TRUE.)
*           This can be the same as the input array.
*    Method :
*    Deficiencies :
*     As this is a GEN routine, it has no STATUS argument.
*    Bugs :
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*    History :
*      2-Jan-1991: Original version.                         (SMB)
*      3-Jan-1991: Typing mistakes fixed.                    (SMB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  DIM                  ! Dimension of arrays
      REAL
     :  INPUT( DIM ),        ! Input data array
     :  INVAR( DIM )         ! Input variance array
      BYTE
     :  INQUAL( DIM )        ! Input data quality array
      LOGICAL
     :  QUALITY,             ! T if data quality array is being used
     :  FLAGGED,             ! T if flagged values are being used
     :  VARIANCE             ! T if a variance array is being used
      REAL
     :  FBAD                 ! The flagged value
*    Export :
      REAL
     :  OUTPUT( DIM ),       ! Output data array
     :  OUTVAR( DIM )        ! Output variance array
      BYTE
     :  OUTQUAL( DIM )       ! Output data quality array
*    Status :
*    Global variables :
*    Local constants :
      BYTE
     :  GOOD,                ! Quality value meaning "good"
     :  BAD                  ! Quality value meaning "bad"
      PARAMETER ( GOOD = 0,
     :            BAD  = 1 )
*    Local variables :
      INTEGER
     :  I1,                  ! Position of last detected good pixel
     :  I2,                  ! Position of next good pixel
     :  I, J                 ! Loop counters
      REAL
     :  A, B,                ! Distances used for interpolation
     :  AB,                  ! A + B
     :  ABSQ                 ! (A + B)**2
*-

*   Check which data quality method is being used

      IF ( QUALITY ) THEN

*      A data quality array is being used.
*      Scan the elements of the input array, starting at element 1.

         I = 1

         DO WHILE ( I .LE. DIM )

*         Search for the next region of bad pixels, copying the
*         good pixels at the same time.

            DO WHILE ( ( I .LE. DIM ) .AND.
     :                 ( INQUAL(I) .EQ. GOOD ) )

               OUTPUT(I) = INPUT(I)
               IF ( VARIANCE ) OUTVAR(I) = INVAR(I)
               OUTQUAL(I) = INQUAL(I)

               I = I + 1
            END DO

*         Check that the end of the array has not been encountered

            IF ( I .LE. DIM ) THEN

*            A bad pixel region has been detected. Remember the location
*            of the last good pixel in I1 and locate the next good pixel.

               I1 = I - 1

               DO WHILE ( ( I .LE. DIM ) .AND.
     :                    ( INQUAL(I) .NE. GOOD ) )

                  I = I + 1
               END DO

               I2 = I

*            I1 and I2 now delimit the bad pixel region. Check whether
*            the region touches the left or right hand edges.

               IF ( I1 .EQ. 0 ) THEN

*               The region touches the left hand edge. If it touches
*               the right hand edge as well, give up! Otherwise
*               duplicate the value found in the good pixel at I2.

                  IF ( I2 .LE. DIM ) THEN

                     DO J = 1, I2-1

                        OUTPUT(J) = INPUT(I2)
                        IF ( VARIANCE ) OUTVAR(J) = INVAR(I2)
                        OUTQUAL(J) = GOOD
                     END DO
                  END IF

               ELSE IF ( I2 .GT. DIM ) THEN

*               The region touches the right hand edge.
*               Duplicate the value found in the good pixel at I1.

                  DO J = I1+1, DIM

                     OUTPUT(J) = INPUT(I1)
                     IF ( VARIANCE ) OUTVAR(J) = INVAR(I1)
                     OUTQUAL(J) = GOOD
                  END DO
               ELSE

*               The region is delimited by good pixels on both sides.
*               For each bad pixel in this region, interpolate between
*               the good values either side.

                  AB = REAL( I2 - I1 )
                  ABSQ = AB * AB

                  DO J = I1+1, I2-1

                     A = REAL( J - I1 )
                     B = REAL( I2 - J )

                     OUTPUT(J) = ( B*INPUT(I1) + A*INPUT(I2) ) / AB

                     IF ( VARIANCE ) THEN

                        OUTVAR(J) = 4.0
     :                              * ( B*B*INVAR(I1) + A*A*INVAR(I2) )
     :                              / ABSQ
                     END IF

                     OUTQUAL(J) = GOOD
                  END DO
               END IF

*            Reset the value of I to the next good pixel.
*            Then go back and search for the next bad pixel region.

               I = I2
            END IF
         END DO

      ELSE IF ( FLAGGED ) THEN

*      Flagged values are being used.
*      Scan the elements of the input array, starting at element 1.

         I = 1

         DO WHILE ( I .LE. DIM )

*         Search for the next region of bad pixels, copying the
*         good pixels at the same time.

            DO WHILE ( ( I .LE. DIM ) .AND.
     :                 ( INPUT(I) .NE. FBAD ) )

               OUTPUT(I) = INPUT(I)
               IF ( VARIANCE ) OUTVAR(I) = INVAR(I)

               I = I + 1
            END DO

*         Check that the end of the array has not been encountered

            IF ( I .LE. DIM ) THEN

*            A bad pixel region has been detected. Remember the location
*            of the last good pixel in I1 and locate the next good pixel.

               I1 = I - 1

               DO WHILE ( ( I .LE. DIM ) .AND.
     :                    ( INPUT(I) .EQ. FBAD ) )

                  I = I + 1
               END DO

               I2 = I

*            I1 and I2 now delimit the bad pixel region. Check whether
*            the region touches the left or right hand edges.

               IF ( I1 .EQ. 0 ) THEN

*               The region touches the left hand edge. If it touches
*               the right hand edge as well, give up! Otherwise
*               duplicate the value found in the good pixel at I2.

                  IF ( I2 .LE. DIM ) THEN

                     DO J = 1, I2-1

                        OUTPUT(J) = INPUT(I2)
                        IF ( VARIANCE ) OUTVAR(J) = INVAR(I2)
                     END DO
                  END IF

               ELSE IF ( I2 .GT. DIM ) THEN

*               The region touches the right hand edge.
*               Duplicate the value found in the good pixel at I1.

                  DO J = I1+1, DIM

                     OUTPUT(J) = INPUT(I1)
                     IF ( VARIANCE ) OUTVAR(J) = INVAR(I1)
                  END DO
               ELSE

*               The region is delimited by good pixels on both sides.
*               For each bad pixel in this region, interpolate between
*               the good values either side.

                  AB = REAL( I2 - I1 )
                  ABSQ = AB * AB

                  DO J = I1+1, I2-1

                     A = REAL( J - I1 )
                     B = REAL( I2 - J )

                     OUTPUT(J) = ( B*INPUT(I1) + A*INPUT(I2) ) / AB

                     IF ( VARIANCE ) THEN

                        OUTVAR(J) = 4.0
     :                              * ( B*B*INVAR(I1) + A*A*INVAR(I2) )
     :                              / ABSQ
                     END IF
                  END DO
               END IF

*            Reset the value of I to the next good pixel.
*            Then go back and search for the next bad pixel region.

               I = I2
            END IF
         END DO

      ELSE

*      Neither data quality nor flagged values are being used.
*      The routine can do nothing other than copy the input
*      to the output.

         DO I = 1, DIM

            OUTPUT(I) = INPUT(I)
            IF ( VARIANCE ) OUTVAR(I) = INVAR(I)
         END DO
      END IF

      END
