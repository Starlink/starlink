*+  P4_BSMOTHQ - Smooth a 1-D REAL array with a simple moving box filter.
      SUBROUTINE P4_BSMOTHQ( NELM, INPUT, INQUAL, BOXSIZE, OUTPUT,
     :  OUTQUAL, QUALITY, FLAGGED, FBAD, STATUS )
*    Description :
*     This routine smooths a 1-D REAL array with a moving box filter
*     with the given box size. The filter uses a very simple top-hat
*     function. If data quality information is provided, bad values in
*     the input array are ignored. The output array will contain a bad
*     value whenever a smoothed value cannot be calculated.
*    Invocation :
*      CALL P4_BSMOTHQ( NELM, INPUT, INQUAL, BOXSIZE, OUTPUT,
*     :  OUTQUAL, QUALITY, FLAGGED, FBAD, STATUS )
*    Parameters :
*     NELM                 = INTEGER( READ )
*        The dimension of the array.
*     INPUT( NELM )        = REAL( READ )
*        The input array to be smoothed.
*     INQUAL( NELM )       = BYTE( READ )
*        The quality of the input array. This is only used if
*        QUALITY is .TRUE.
*     BOXSIZE              = INTEGER( UPDATE )
*        On input this should be the size of box required.
*        On output it will contain the size of the box actually used.
*        BOXSIZE should be an odd number but if an even number is given
*        it will be rounded up to the nearest odd number (BOXSIZE+1).
*        The maximum BOXSIZE allowed is NELM/2, and larger values will
*        be truncated.
*     OUTPUT( NELM )       = REAL( WRITE )
*        The smoothed version of the array.
*        NOTE THE OUTPUT ARRAY CANNOT BE THE SAME AS THE INPUT ARRAY.
*     OUTQUAL( NELM )      = BYTE( WRITE )
*        The quality of the output array. This is only used if
*        QUALITY is .TRUE.
*     QUALITY              = LOGICAL( READ )
*        Indicates if a data quality array is to be used.
*     FLAGGED              = LOGICAL( READ )
*        Indicates if magic, flagged values are to be used.
*     FBAD                 = REAL( READ )
*        The magic bad value used. This is only used if FLAGGED is .TRUE.
*     STATUS               = INTEGER( UPDATE )
*        Global ADAM status.
*    Method :
*    Deficiencies :
*     This routine uses a very naive and inefficient algorithm.
*     It would be quicker to use a geniune moving box, subtracting
*     values from the left hand side and adding them to the right.
*     However, the routine will work reasonably well for small
*     data sets.
*     Figaro provides some much better smoothing routines, but this one
*     was produced because it provides a very simple way of smoothing a
*     1-D histogram.
*     Variances are not handled.
*    Bugs :
*    Authors :
*     S.M.Beard (REVAD::SMB)
*    History :
*     27-Nov-1990: Original version as P4_BSMOTHQ.                   (SMB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  NELM                ! Dimensions of the data arrays
      REAL
     :  INPUT( NELM )       ! Input data array
      BYTE
     :  INQUAL( NELM )      ! Input data quality array (optional).
      LOGICAL
     :  QUALITY,            ! T if a data quality array has been provided
     :  FLAGGED             ! T if flagged values are being used
      REAL
     :  FBAD                ! Magic bad value
*    Import-export
      INTEGER
     :  BOXSIZE             ! The size of the moving box.
*    Export :
      REAL
     :  OUTPUT( NELM )      ! Output data array
*    Status :
      INTEGER
     :  STATUS              ! Global status
      BYTE
     :  OUTQUAL( NELM )     ! Output data quality array (optional).
*    External references :
*    Global variables :
*    Local Constants :
      BYTE
     :  GOOD,               ! Quality value meaning good data
     :  BAD                 ! Quality value meaning bad data
      PARAMETER ( GOOD = 0,
     :            BAD = 1 )
*    Local variables :
      INTEGER
     :  BOX2,               ! Half the box size
     :  I, J,               ! Loop counters
     :  J1, J2,             ! Range for J values
     :  NPTS                ! Number of points making up the sum
      REAL
     :  SUM                 ! Sum of values
*    Internal References :
*    Local data :
*-

*   Check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*      Initialise the half box size. (Note this is integer arithmetic,
*      so the value will be rounded down. 2/2=1 and 3/2=1). Ensure that
*      BOX is always less than NELM/4. Reset BOXSIZE to the actual size
*      used.

         BOX2 = MIN( NELM/4, BOXSIZE/2 )
         BOXSIZE = 2 * BOX2 + 1

*      The procedure which follows depends on whether a data quality
*      array has been provided or flagged values are being used.

         IF ( QUALITY ) THEN

*         A data quality array has been provided.

*         First loop through the elements in the output array which are
*         affected by edge effects at the left hand edge.

            DO I = 1, BOX2

*            Set the output array to be the average of all the values
*            up to BOX2 elements either side of the current value in the
*            input array, ensuring the bounds of the input array are not
*            violated at the left hand edge.

               SUM  = 0.0
               NPTS = 0

               J1 = MAX( 1, I - BOX2 )
               J2 = I + BOX2

               DO J = J1, J2

                  IF ( INQUAL(J) .EQ. GOOD ) THEN

                     SUM = SUM + INPUT(J)
                     NPTS = NPTS + 1
                  END IF
               END DO

               IF ( NPTS .GT. 0 ) THEN

                  OUTPUT(I) = SUM / REAL(NPTS)
                  OUTQUAL(I) = GOOD
               ELSE

                  OUTQUAL(I) = BAD
               END IF
            END DO

*         Loop through the central elements in the output array which are
*         not affected by edge effects.

            DO I = BOX2+1, NELM-BOX2

*            Set the output array to be the average of all the values
*            up to BOX2 elements either side of the current value in the
*            input array. No bounds violation check is needed.

               SUM  = 0.0
               NPTS = 0

               J1 = I - BOX2
               J2 = I + BOX2

               DO J = J1, J2

                  IF ( INQUAL(I) .EQ. GOOD ) THEN

                     SUM = SUM + INPUT(J)
                     NPTS = NPTS + 1
                  END IF
               END DO

               IF ( NPTS .GT. 0 ) THEN

                  OUTPUT(I) = SUM / REAL(NPTS)
                  OUTQUAL(I) = GOOD
               ELSE

                  OUTQUAL(I) = BAD
               END IF
            END DO

*         Finally loop through the elements in the output array which are
*         affected by edge effects at the right hand edge.

            DO I = NELM-BOX2+1, NELM

*            Set the output array to be the average of all the values
*            up to BOX2 elements either side of the current value in the
*            input array, ensuring the bounds of the input array are not
*            violated at the right hand edge.

               SUM  = 0.0
               NPTS = 0

               J1 = I - BOX2
               J2 = MIN( NELM, I + BOX2 )

               DO J = J1, J2

                  IF ( INQUAL(I) .EQ. GOOD ) THEN
                     SUM = SUM + INPUT(J)
                     NPTS = NPTS + 1
                  END IF
               END DO

               IF ( NPTS .GT. 0 ) THEN

                  OUTPUT(I) = SUM / REAL(NPTS)
                  OUTQUAL(I) = GOOD
               ELSE

                  OUTQUAL(I) = BAD
               END IF
            END DO

         ELSE IF ( FLAGGED ) THEN

*         "Magic" flagged values are being used to identify bad points.

*         First loop through the elements in the output array which are
*         affected by edge effects at the left hand edge.

            DO I = 1, BOX2

*            Set the output array to be the average of all the values
*            up to BOX2 elements either side of the current value in the
*            input array, ensuring the bounds of the input array are not
*            violated at the left hand edge.

               SUM  = 0.0
               NPTS = 0

               J1 = MAX( 1, I - BOX2 )
               J2 = I + BOX2

               DO J = J1, J2

                  IF ( INPUT(J) .NE. FBAD ) THEN

                     SUM = SUM + INPUT(J)
                     NPTS = NPTS + 1
                  END IF
               END DO

               IF ( NPTS .GT. 0 ) THEN

                  OUTPUT(I) = SUM / REAL(NPTS)
               ELSE

                  OUTPUT(I) = FBAD
               END IF
            END DO

*         Loop through the central elements in the output array which are
*         not affected by edge effects.

            DO I = BOX2+1, NELM-BOX2

*            Set the output array to be the average of all the values
*            up to BOX2 elements either side of the current value in the
*            input array. No bounds violation check is needed.

               SUM  = 0.0
               NPTS = 0

               J1 = I - BOX2
               J2 = I + BOX2

               DO J = J1, J2

                  IF ( INPUT(I) .NE. FBAD ) THEN

                     SUM = SUM + INPUT(J)
                     NPTS = NPTS + 1
                  END IF
               END DO

               IF ( NPTS .GT. 0 ) THEN

                  OUTPUT(I) = SUM / REAL(NPTS)
               ELSE

                  OUTPUT(I) = FBAD
               END IF
            END DO

*         Finally loop through the elements in the output array which are
*         affected by edge effects at the right hand edge.

            DO I = NELM-BOX2+1, NELM

*            Set the output array to be the average of all the values
*            up to BOX2 elements either side of the current value in the
*            input array, ensuring the bounds of the input array are not
*            violated at the right hand edge.

               SUM  = 0.0
               NPTS = 0

               J1 = I - BOX2
               J2 = MIN( NELM, I + BOX2 )

               DO J = J1, J2

                  IF ( INPUT(I) .NE. FBAD ) THEN
                     SUM = SUM + INPUT(J)
                     NPTS = NPTS + 1
                  END IF
               END DO

               IF ( NPTS .GT. 0 ) THEN

                  OUTPUT(I) = SUM / REAL(NPTS)
               ELSE

                  OUTPUT(I) = FBAD
               END IF
            END DO

         ELSE

*         There is no data quality information.

*         First loop through the elements in the output array which are
*         affected by edge effects at the left hand edge.

            DO I = 1, BOX2

*            Set the output array to be the average of all the values
*            up to BOX2 elements either side of the current value in the
*            input array, ensuring the bounds of the input array are not
*            violated at the left hand edge.

               SUM  = 0.0
               NPTS = 0

               J1 = MAX( 1, I - BOX2 )
               J2 = I + BOX2

               DO J = J1, J2

                  SUM = SUM + INPUT(J)
                  NPTS = NPTS + 1
               END DO

               OUTPUT(I) = SUM / REAL(NPTS)
            END DO

*         Loop through the central elements in the output array which are
*         not affected by edge effects.

            DO I = BOX2+1, NELM-BOX2

*            Set the output array to be the average of all the values
*            up to BOX2 elements either side of the current value in the
*            input array. No bounds violation check is needed.

               SUM  = 0.0

               J1 = I - BOX2
               J2 = I + BOX2

               DO J = J1, J2

                  SUM = SUM + INPUT(J)
               END DO

               OUTPUT(I) = SUM / REAL(BOXSIZE)
            END DO

*         Finally loop through the elements in the output array which are
*         affected by edge effects at the right hand edge.

            DO I = NELM-BOX2+1, NELM

*            Set the output array to be the average of all the values
*            up to BOX2 elements either side of the current value in the
*            input array, ensuring the bounds of the input array are not
*            violated at the right hand edge.

               SUM  = 0.0
               NPTS = 0

               J1 = I - BOX2
               J2 = MIN( NELM, I + BOX2 )

               DO J = J1, J2

                  SUM = SUM + INPUT(J)
                  NPTS = NPTS + 1
               END DO

               OUTPUT(I) = SUM / REAL(NPTS)
            END DO
         END IF
      END IF

      END
