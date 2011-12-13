      SUBROUTINE KPS1_BAFIR( NPIX, NLINES, INARR, VAR, INVAR, NITER,
     :                         SIZE, CNGMAX, CNGRMS, NBAD, OUTARR,
     :                         OUTVAR, DSUM, WTSUM, DLAST, WTLAST,
     :                         STATUS )
*+
*  Name:
*     KPS1_BAFIx

*  Purpose:
*     Fills bad-pixel regions in a 2-dimensional image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_BAFIx( NPIX, NLINES, INARR, VAR, INVAR, NITER, SIZE,
*                      CNGMAX, CNGRMS, NBAD, OUTARR, OUTVAR, DSUM,
*                      WTSUM, DLAST, WTLAST, STATUS )

*  Description:
*     This routine replaces all the bad pixels in an image with a
*     solution of Laplace's equation that matches the valid data in
*     the image at the edges of the invalid regions.  This solution
*     has zero gradient normal to any image edges which it meets.

*  Arguments:
*     NPIX = INTEGER (Given)
*        The number of pixels per line in the input data and variance
*        arrays.
*     NLINES = INTEGER (Given)
*        The number of lines in the input data and variance arrays.
*     INARR( NPIX, NLINES ) = ? (Given)
*        The input data array.
*     VAR = LOGICAL (Given)
*        Whether or not associated variance information is to be
*        processed and included in the weighting.
*     INVAR( NPIX, NLINES ) = ? (Given)
*        The input variance array.
*     NITER = INTEGER (Given)
*        The number of iterations required.
*     SIZE = REAL (Given and Returned)
*        On entry it is the initial smoothing size in pixels.  On return
*        it is the final smoothing size.
*     CNGMAX = DOUBLE PRECISION (Returned)
*        The maximum absolute change in output values which occurred in
*        the final iteration.
*     CNGRMS = DOUBLE PRECISION (Returned)
*        The RMS change in output values which occurred in the last
*        iteration.
*     NBAD = INTEGER (Returned)
*        The number of invalid pixels replaced.
*     OUTARR( NPIX, NLINES ) = ? (Returned)
*        The output data array free of bad values.
*     OUTVAR( NPIX, NLINES ) = ? (Returned)
*        The output variance array free of bad values.
*     DSUM( NPIX, NLINES ) = ? (Given)
*        Work array to sum the values.
*     WTSUM( NPIX, NLINES ) = ? (Given)
*        Work array to store the weights.
*     DLAST( NPIX ) = ? (Given)
*        Work array for the last summed values.
*     WTLAST( NPIX, NLINES ) = ? (Given)
*        Work array for the last set of weights.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B or UB as appropriate.  The
*     data and variance arrays supplied to this routine must have the
*     data type specified.
*     -  The state of the replacements (iteration, smoothing length,
*     maximum change, rms change) is tabulated for verbose reporting.

*  Algorithm:
*     Iterate, replacing each bad pixel with a weighted mean of its
*     valid neighbours in the same row and column.  The weights
*     decrease exponentially with a scale length SIZE and go to zero
*     after the first valid pixel is encountered.  When there is
*     variance information this is included in the weighting.  The
*     length SIZE is reduced by a factor 2 whenever the maximum absolute
*     change in an iteration is at least a factor 4 less than the
*     maximum absolute change obtained since the current scale length
*     was first used.  Iterations stop after NITER have been performed.
*
*     When there is variance processing the output variance is
*     reassigned if either the input variance or data value was bad.
*     Where the input value is good but its associated variance is bad,
*     the calculation proceeds as if the data value were bad, except
*     that only the variance is substituted in the output.  The new
*     variance is the inverse of the sum of the weights.
*
*     See the Notes in FILLBAD for additional details and background to
*     the algorithm.

*  Prior Requirements:
*     -  The input variance array should be propagated from the input
*     dataset.  This was done to reduce memory requirements, which are
*     already large.

*  Copyright:
*     Copyright (C) 1995, 1998 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 April 16 (MJC):
*        Original version based on R.F. Warren-Smith's FILLIN.
*     21-MAY-1998 (DSB):
*        Added protection against division by zero if there are any
*        zero variance values.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Arguments Given:
      INTEGER NPIX
      INTEGER NLINES
      REAL INARR( NPIX, NLINES )
      LOGICAL VAR
      REAL INVAR( NPIX, NLINES )
      INTEGER NITER

*  Arguments Given and Returned:
      REAL SIZE

*  Arguments Returned:
      DOUBLE PRECISION CNGMAX
      DOUBLE PRECISION CNGRMS
      INTEGER NBAD
      REAL OUTARR( NPIX, NLINES )
      REAL OUTVAR( NPIX, NLINES )
      REAL DSUM( NPIX, NLINES )
      REAL WTSUM( NPIX, NLINES )
      REAL DLAST( NPIX )
      REAL WTLAST( NPIX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL DEC                ! Weighting decrement factor
      DOUBLE PRECISION DIFF      ! Absolute change in new value
      INTEGER I                  ! Loop counter
      INTEGER IDIRN              ! Direction through row
      INTEGER IFIRST             ! First row
      INTEGER ILAST              ! Final row
      INTEGER ITER               ! Iteration loop counter
      INTEGER J                  ! Loop counter
      INTEGER JDIRN              ! Direction through line
      INTEGER JFIRST             ! First line
      INTEGER JLAST              ! Final line
      DOUBLE PRECISION LASTMX    ! Last maximum change
      REAL NEWVAL             ! New value
      INTEGER NRMS               ! Number of values used to compute the
                                 ! rms change
      CHARACTER * ( 80 ) PRBUF   ! Message buffer
      DOUBLE PRECISION RMS       ! RMS

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      NBAD=0

*  Copy the input to the output data array and count the bad values.
      DO J = 1, NLINES
         DO I = 1, NPIX
            OUTARR( I, J ) = INARR( I, J )
            IF ( INARR( I, J ) .EQ. VAL__BADR ) NBAD = NBAD + 1
         END DO
      END DO

*  If the progress of the iterations is to be printed (verbose message
*  reporting), print some headings.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      CALL MSG_OUTIF( MSG__NORM, 'HEADING', '      iteration    '/
     :  /'smoothing length    max. change    rms change', STATUS )
      CALL MSG_OUTIF( MSG__NORM, 'HEADING2', '      ---------    '/
     :  /'----------------    -----------    ----------', STATUS )

*  There are two processing paths depending whether or not variance is
*  present.
      IF ( .NOT. VAR ) THEN

*  Perform the required number of relaxation iterations.
         LASTMX = 0.0D0
         CNGMAX = 0.0D0

         DO ITER = 1, NITER

*  Set the maximum absolute change so far.
            LASTMX = MAX( LASTMX, CNGMAX, 0.0D0 )

*  If the max change last iteration was less than 0.25 of the maximum
*  change so far, reduce the scale size by a factor 2 and reset the
*  maximum change so far.
            IF ( CNGMAX * 4.0D0 .LE. LASTMX .AND. ITER .NE. 1 ) THEN
               SIZE = SIZE * 0.5
               LASTMX = CNGMAX
            END IF

*  Initialise the maximum absolute change and number of values used to
*  compute the rms change for this iteration.
            CNGMAX = 0.0D0
            NRMS = 0

*  Calculate the logarithmic decrement for the weights in going from one
*  pixel to the next.
            DEC = EXP( -1.0E0 / NUM_RTOR( SIZE ) )

*  Initialise the storage for forming the weighted means.
            DO J = 1, NLINES
               DO  I = 1, NPIX
                  DSUM( I, J ) = 0.0E0
                  WTSUM( I, J ) = 0.0E0
               END DO
            END DO

*  First work through the image lines, scanning each line in both
*  directions.
            DO J = 1, NLINES
               DO IDIRN = -1, 1, 2

                  IF ( IDIRN .GE. 0 ) THEN
                     IFIRST = 1
                     ILAST = NPIX
                  ELSE
                     IFIRST = NPIX
                     ILAST = 1
                  END IF

*  Initialise the stores.  DLAST is the weighted sum of previous data
*  values; WTLAST is the sum of previous weights.
                  DLAST( 1 ) = 0.0E0
                  WTLAST( 1 ) = 0.0E0

*  Process a line.
                  DO I = IFIRST, ILAST, IDIRN

*  If the input pixel is valid, reset the weighted sums.
                     IF ( INARR( I, J ) .NE. VAL__BADR ) THEN
                        DLAST( 1 ) = OUTARR( I, J )
                        WTLAST( 1 ) = 1.0E0

*  For invalid locations, form the sums for the weighted mean.
                     ELSE

*  Decrement the previous weight.
                        WTLAST( 1 ) = WTLAST( 1 ) * DEC
                        DLAST( 1 ) = DLAST( 1 ) * DEC

*  Form sums for the replacement value.
                        DSUM( I, J ) = DSUM( I, J ) + DLAST( 1 )
                        WTSUM( I, J ) = WTSUM( I, J ) + WTLAST( 1 )

*  If this pixel has been replaced before, add it into the current
*  weighted sums for this line.
                        IF ( OUTARR( I, J ) .NE. VAL__BADR ) THEN
                           WTLAST( 1 ) = WTLAST( 1 ) + 1.0E0
                           DLAST( 1 ) = DLAST( 1 ) + OUTARR( I, J )
                        END IF
                     END IF

                  END DO
               END DO
            END DO

*  Now perform the same process down the image columns, but processing
*  a whole line of data at once.
            DO JDIRN = -1, 1, 2

               IF ( JDIRN .GE. 0 ) THEN
                  JFIRST = 1
                  JLAST = NLINES
               ELSE
                  JFIRST = NLINES
                  JLAST = 1
               END IF

*  Initialise the stores for a whole line.
               DO I = 1, NPIX
                  DLAST( I ) = 0.0E0
                  WTLAST( I ) = 0.0E0
               END DO

*  Process columns, as above, but using a whole line of data.
               DO J = JFIRST, JLAST, JDIRN
                  DO I  = 1, NPIX

*  If the input pixel is valid, reset the weighted sums.
                     IF ( INARR( I, J ) .NE. VAL__BADR ) THEN
                        DLAST( I ) = OUTARR( I, J )
                        WTLAST( I ) = 1.0E0

                     ELSE

*  Decrement the previous weight.
                        WTLAST( I ) = WTLAST( I ) * DEC
                        DLAST( I ) = DLAST( I ) * DEC

*  Form sums for the replacement value.
                        DSUM( I, J ) = DSUM( I, J ) + DLAST( I )
                        WTSUM( I, J ) = WTSUM( I, J ) + WTLAST( I )

*  If this pixel has been replaced before, add it into the current
*  weighted sums for this line.
                        IF ( OUTARR( I, J ) .NE. VAL__BADR ) THEN
                           WTLAST( I ) = WTLAST( I ) + 1.0E0
                           DLAST( I ) = DLAST( I ) + OUTARR( I, J )
                        END IF

                     END IF
                  END DO
               END DO
            END DO

*  Scan the invalid pixels, replacing those for which a new weighted
*  mean can be formed.
            RMS = 0.0D0

            DO J = 1, NLINES
               DO I = 1, NPIX

*  If the input pixel was invalid, and a replacement value can be
*  found, calculate the replacement value.
                  IF ( INARR( I, J ) .EQ. VAL__BADR .AND.
     :                 WTSUM( I, J ) .GT. 0.0E0 ) THEN
                     NEWVAL = DSUM( I, J ) / WTSUM( I, J )

*  Cannot compute differences when the output value is still bad.
                     IF ( OUTARR( I, J ) .NE. VAL__BADR ) THEN

*  Find the maximum absolute change this iteration.
                        DIFF = ABS( NUM_RTOD( NEWVAL -
     :                         OUTARR( I, J ) ) )
                        CNGMAX = MAX( CNGMAX, DIFF )

*  Form the sums for the rms change.
                        RMS = RMS + DIFF * DIFF
                        NRMS = NRMS + 1
                     END IF

*  Assign the new output value.
                     OUTARR( I, J ) = NEWVAL
                  END IF
               END DO
            END DO

*  Print the progress of each iteration, if required.  Calculate the
*  rms and maximum change this iteration.  The first iteration change
*  values are undefined because the difference is with respect to an
*  undefined value.
            IF ( ITER .GT. 1 ) THEN
               CNGRMS = SQRT( RMS / DBLE( MAX( 1, NRMS ) ) )

               WRITE( PRBUF, '(6X,I6,9X,G13.6,4X,G13.6,2X,G13.6)' )
     :           ITER, SIZE, CNGMAX, CNGRMS

            ELSE
               WRITE( PRBUF, '(6X,I6,9X,G13.6,4X)' ) ITER, SIZE

            END IF
            CALL MSG_OUTIF( MSG__NORM, 'PROGRESS', PRBUF, STATUS )

         END DO

*  Variance weighting and processing.
      ELSE

*  Copy the input to the output variance array.  Add to the number of
*  bad values if the data was good, but the variance was bad.
         DO J = 1, NLINES
            DO I = 1, NPIX
               OUTVAR( I, J ) = INVAR( I, J )
               IF ( INARR( I, J ) .NE. VAL__BADR .AND.
     :              INVAR( I, J ) .EQ. VAL__BADR ) NBAD = NBAD + 1
            END DO
         END DO

*  Perform the required number of relaxation iterations.
         LASTMX = 0.0D0
         CNGMAX = 0.0D0

         DO ITER = 1, NITER

*  Set the maximum absolute change so far.
            LASTMX = MAX( LASTMX, CNGMAX, 0.0D0 )

*  If the max change last iteration was less than 0.25 of the maximum
*  change so far, reduce the scale size by a factor 2 and reset the
*  maximum change so far.
            IF ( CNGMAX * 4.0D0 .LE. LASTMX .AND. ITER .NE. 1 ) THEN
               SIZE = SIZE * 0.5
               LASTMX = CNGMAX
            END IF

*  Initialise the maximum absolute change and number of values used to
*  compute the rms change for this iteration.
            CNGMAX = 0.0D0
            NRMS = 0

*  Calculate the logarithmic decrement for the weights in going from one
*  pixel to the next.
            DEC = EXP( -1.0E0 / NUM_RTOR( SIZE ) )

*  Initialise the storage for forming the weighted means.
            DO J = 1, NLINES
               DO  I = 1, NPIX
                  DSUM( I, J ) = 0.0E0
                  WTSUM( I, J ) = 0.0E0
               END DO
            END DO

*  First work through the image lines, scanning each line in both
*  directions.
            DO J = 1, NLINES
               DO IDIRN = -1, 1, 2

                  IF ( IDIRN .GE. 0 ) THEN
                     IFIRST = 1
                     ILAST = NPIX
                  ELSE
                     IFIRST = NPIX
                     ILAST = 1
                  END IF

*  Initialise the stores.  DLAST is the weighted sum of previous data
*  values; WTLAST is the sum of previous weights.
                  DLAST( 1 ) = 0.0E0
                  WTLAST( 1 ) = 0.0E0

*  Process a line.
                  DO I = IFIRST, ILAST, IDIRN

*  If the input pixel and variance are valid, reset the weighted sums.
                     IF ( INARR( I, J ) .NE. VAL__BADR .AND.
     :                    INVAR( I, J ) .NE. VAL__BADR .AND.
     :                    OUTVAR( I, J ) .GT. 0.0 ) THEN
                        DLAST( 1 ) = OUTARR( I, J ) / OUTVAR( I, J )
                        WTLAST( 1 ) = 1.0E0 / OUTVAR( I, J )

*  For invalid locations, form the sums for the weighted mean.
                     ELSE

*  Decrement the weight given by the variance.
                        WTLAST( 1 ) = WTLAST( 1 ) * DEC
                        DLAST( 1 ) = DLAST( 1 ) * DEC

*  Form sums for the replacement value.
                        DSUM( I, J ) = DSUM( I, J ) + DLAST( 1 )
                        WTSUM( I, J ) = WTSUM( I, J ) + WTLAST( 1 )

*  If this pixel or variance has been replaced before, add it into the
*  current weighted sums for this line.
                        IF ( OUTARR( I, J ) .NE. VAL__BADR .AND.
     :                       OUTVAR( I, J ) .NE. VAL__BADR .AND.
     :                       OUTVAR( I, J ) .GT. 0.0 ) THEN
                           WTLAST( 1 ) = WTLAST( 1 ) + 1.0E0 /
     :                                   OUTVAR( I, J )
                           DLAST( 1 ) = DLAST( 1 ) +
     :                                  OUTARR( I, J ) / OUTVAR( I, J )
                        END IF
                     END IF

                  END DO
               END DO
            END DO

*  Now perform the same process down the image columns, but processing
*  a whole line of data at once.
            DO JDIRN = -1, 1, 2

               IF ( JDIRN .GE. 0 ) THEN
                  JFIRST = 1
                  JLAST = NLINES
               ELSE
                  JFIRST = NLINES
                  JLAST = 1
               END IF

*  Initialise the stores for a whole line.
               DO I = 1, NPIX
                  DLAST( I ) = 0.0E0
                  WTLAST( I ) = 0.0E0
               END DO

*  Process columns, as above, but using a whole line of data.
               DO J = JFIRST, JLAST, JDIRN
                  DO I  = 1, NPIX

*  If the input pixel and variance are valid, reset the weighted sums.
                     IF ( OUTARR( I, J ) .NE. VAL__BADR .AND.
     :                    OUTVAR( I, J ) .NE. VAL__BADR .AND.
     :                    OUTVAR( I, J ) .GT. 0.0 ) THEN
                        DLAST( I ) = OUTARR( I, J ) / OUTVAR( I, J )
                        WTLAST( I ) = 1.0E0 / OUTVAR( I, J )

                     ELSE

*  Decrement the previous weight.
                        WTLAST( I ) = WTLAST( I ) * DEC
                        DLAST( I ) = DLAST( I ) * DEC

*  Form sums for the replacement value.
                        DSUM( I, J ) = DSUM( I, J ) + DLAST( I )
                        WTSUM( I, J ) = WTSUM( I, J ) + WTLAST( I )

*  If this pixel or variance has been replaced before, add it into the
*  current weighted sums for this line.
                        IF ( OUTARR( I, J ) .NE. VAL__BADR .AND.
     :                       OUTVAR( I, J ) .NE. VAL__BADR .AND.
     :                       OUTVAR( I, J ) .GT. 0.0 ) THEN
                           WTLAST( I ) = WTLAST( I ) + 1.0E0 /
     :                                   OUTVAR( I, J )
                           DLAST( I ) = DLAST( I ) +
     :                                  OUTARR( I, J ) / OUTVAR( I, J )
                        END IF

                     END IF
                  END DO
               END DO
            END DO

*  Scan the invalid pixels, replacing those for which a new weighted
*  mean can be formed.
            RMS = 0.0D0

            DO J = 1, NLINES
               DO I = 1, NPIX

*  If the input pixel or variance was invalid, and a replacement value
*  can be found, calculate the replacement value.
                  IF ( ( INARR( I, J ) .EQ. VAL__BADR .OR.
     :                   INVAR( I, J ) .EQ. VAL__BADR ) .AND.
     :                 WTSUM( I, J ) .GT. 0.0E0 ) THEN
                     NEWVAL = DSUM( I, J ) / WTSUM( I, J )

*  Cannot compute differences when the output value is still bad.
                     IF ( OUTARR( I, J ) .NE. VAL__BADR ) THEN

*  Find the maximum absolute change this iteration.
                        DIFF = ABS( NUM_RTOD( NEWVAL -
     :                         OUTARR( I, J ) ) )
                        CNGMAX = MAX( CNGMAX, DIFF )

*  Form the sums for the rms change.
                        RMS = RMS + DIFF * DIFF
                        NRMS = NRMS + 1
                     END IF

*  Assign the new output values.  Only assign a new output value when
*  the input was bad, but change the variance if it was bad or there
*  is a new value.
                     IF ( INARR( I, J ) .EQ. VAL__BADR )
     :                 OUTARR( I, J ) = NEWVAL
                     OUTVAR( I, J ) = 2.0E0 / WTSUM( I, J )
                  END IF
               END DO
            END DO

*  Print the progress of each iteration, if required.  Calculate the
*  rms and maximum change this iteration.  The first iteration change
*  values are undefined because the difference is with respect to an
*  undefined value.
            IF ( ITER .GT. 1 ) THEN
               CNGRMS = SQRT( RMS / DBLE( MAX( 1, NRMS ) ) )

               WRITE( PRBUF, '(6X,I6,9X,G13.6,4X,G13.6,2X,G13.6)' )
     :           ITER, SIZE, CNGMAX, CNGRMS

            ELSE
               WRITE( PRBUF, '(6X,I6,9X,G13.6,4X)' ) ITER, SIZE

            END IF
            CALL MSG_OUTIF( MSG__NORM, 'PROGRESS', PRBUF, STATUS )

         END DO

      END IF

  999 CONTINUE

      END
