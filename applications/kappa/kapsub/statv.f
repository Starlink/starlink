      SUBROUTINE STATV ( ARRAY, DIM, NCLIP, CLIP, MAXMUM, MINMUM, TOTAL,
     :                   MEAN, STDDEV, NINVAL, MAXPOS, MINPOS, MAXMCL,
     :                   MINMCL, TOTLCL, MEANCL, STDVCL, NPIXCL, MXPSCL,
     :                   MNPSCL, STATUS )
*+
*  Name:
*     STATV

*  Purpose:
*     Gives simple statistics of an array

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     SUBROUTINE

*  Invocation:
*     CALL STATV ( ARRAY, DIM, NCLIP, CLIP, MAXMUM, MINMUM, TOTAL, MEAN,
*    :             STDDEV, NINVAL, MAXPOS, MINPOS, MAXMCL, MINMCL,
*    :             TOTLCL, MEANCL, STDVCL, NPIXCL, MXPSCL, MNPSCL,
*    :             STATUS )

*  Description:
*     This routine returns simples statistics of an array.  The
*     statistics are the maximum and minimum values and their
*     co-ordinates, the total value, the mean, the standard deviation
*     and the number of bad pixels within the array. Kappa-sigma
*     clipping is optional.
*
*     The magic-value method is used for bad pixels.

*  Arguments:
*     ARRAY( DIM ) = REAL( READ )
*         The input data array for which statistical parameters are
*           required.
*     DIM = INTEGER( READ )
*         The dimension of the array.
*     NCLIP = INTEGER( READ )
*         The number of clipping cycles.
*     CLIP( * ) = REAL( READ )
*         The array of standard deviation thresholds to define the
*           progressive clipping of the distribution.  Thus a value of
*           3.0 would eliminate points outside the range mean-3.*sigma
*           to mean+3.*sigma.
*     MAXMUM = REAL( WRITE )
*         Maximum pixel value in the array.
*     MINMUM = REAL( WRITE )
*         Minimum pixel value in the array.
*     TOTAL = REAL( WRITE )
*         Total pixel value in the array.
*     MEAN = REAL( WRITE )
*         Mean pixel value in the array.
*     STDDEV = REAL( WRITE )
*         Standard deviation of the pixel values in the array.
*     NINVAL = INTEGER( WRITE )
*         Number of invalid pixels in the array.
*     MAXPOS  =  INTEGER ( WRITE )
*         Index of the pixel where the maximum value is (first) found.
*     MINPOS  =  INTEGER ( WRITE )
*         Index of the pixel where the minimum value is (first) found.
*     MAXMCL = REAL( WRITE )
*         Maximum pixel value in the array after clipping.
*     MINMCL = REAL( WRITE )
*         Minimum pixel value in the array after clipping.
*     TOTLCL = REAL( WRITE )
*         Total pixel value in the array after clipping.
*     MEANCL = REAL( WRITE )
*         Mean pixel value in the array after clipping.
*     STDVCL = REAL( WRITE )
*         Standard deviation of the pixel values in the array after
*           clipping.
*     NPIXCL = INTEGER( WRITE )
*         Number of pixels remaining in the array after clipping.
*     MXPSCL  =  INTEGER ( WRITE )
*         Index of the pixel where the maximum value is (first) found
*           after clipping.
*     MNPSCL  =  INTEGER ( WRITE )
*         Index of the pixel where the minimum value is (first) found
*           after clipping.
*     STATUS  =  INTEGER( READ )
*         Global status value.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     Looping around the requested array (including only valid
*     pixels) just once, the maximum and minimum pixel values are
*     found, along with the total of all the pixel values, and the
*     total of all the squared pixel values. On exit from the loop,
*     the quantities mean and standard deviation are formed from
*     algebraic manipulations.
*     All internal arithmetic is done in with double-precision reals
*     for accuracy, although the input and output remain in single
*     precision.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     Malcolm J. Currie  STARLINK ( RAL::CUR )
*     {enter_new_authors_here}

*  History:
*     1990 Jan 16: Second implementation (RAL::CUR).
*     1990 Mar  8: Added arguments for the position of maximum and
*                  minimum pixels (RAL::CUR).
*     {enter_further_changes_here}

*  Bugs:
*     None known.
*     {note_new_bugs_here}

*-

*  Type Definitions:

      IMPLICIT  NONE              ! no implicit typing allowed

*  Global Constants:

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'PRM_PAR'          ! Magic-value definitions

*  Arguments Given:

      INTEGER
     :  DIM,
     :  NCLIP

      REAL
     :  ARRAY( DIM ),
     :  CLIP( * )

*  Arguments Returned:

      INTEGER
     :  NINVAL,
     :  NPIXCL,
     :  MAXPOS,
     :  MINPOS,
     :  MXPSCL,
     :  MNPSCL

      REAL
     :  MAXMUM,
     :  MINMUM,
     :  TOTAL,
     :  MAXMCL,
     :  MINMCL,
     :  TOTLCL,
     :  MEAN,
     :  STDDEV,
     :  MEANCL,
     :  STDVCL

*  Status:

      INTEGER  STATUS

*  Local Variables:

      DOUBLE PRECISION
     :  VALUE,                 ! Current array pixel value
     :  DMEAN,                 ! Double precision mean
     :  DTOTAL,                !    "       "     total
     :  DNMPIX,                !    "       "     number of pixels
     :  DMAXIM,                !    "       "     maximum
     :  DMINIM,                !    "       "     minimum
     :  DTOTSQ,                !    "       "     sum of pixels squared
     :  DSTDDV,                !    "       "     standard deviation
     :  VARNCE                 !    "       "     variance

      REAL
     :  LOWER,                 ! Lower clipping bound
     :  UPPER,                 ! Upper clipping bound
     :  TMEAN,                 ! Current mean in clipping loop
     :  TMAX,                  ! Current maximum in clipping loop
     :  TMIN,                  ! Current minimum in clipping loop
     :  TSTDEV,                ! Current standard deviation in clipping
                               ! loop
     :  TTOTAL                 ! Current total in clipping loop

      INTEGER
     :  I, J,                  ! Array counter variables
     :  NOCL,                  ! Number of times to loop less one
     :  NVALPX                 ! Number of valid pixels in array

*.
*    Check status on entry - return if not o.k..

      IF ( STATUS .NE. SAI__OK ) GOTO 999

      NOCL = MAX( 0, NCLIP )
      DO  J = 0, NOCL

*       Initialise variables.

         DTOTAL  =  0.0D0
         DTOTSQ  =  0.0D0
         DMAXIM  =  VAL__MINR
         DMINIM  =  VAL__MAXR

*       Deal with no clipping case first.  This code could have been
*       merged, but with some loss of clarity and a reduction in
*       efficiency when no clipping was required.

         IF ( J .EQ. 0 ) THEN
            NVALPX  =  0

*          Positions are undefined because any other value could be a
*          co-ordinate.

            MAXPOS = VAL__BADI
            MINPOS = VAL__BADI

*          Loop for all pixels in the array.

            DO  I  =  1, DIM

*             Only include valid pixels in the statistics.

               IF ( ARRAY( I ) .NE. VAL__BADR ) THEN

                  NVALPX  =  NVALPX  +  1
                  VALUE   =  DBLE( ARRAY( I ) )
                  DTOTAL  =  DTOTAL + VALUE
                  DTOTSQ  =  DTOTSQ + VALUE * VALUE

*                Check current maximum against current pixel value.

                  IF ( VALUE .GT. DMAXIM ) THEN
                     DMAXIM  =  VALUE
                     MAXPOS  =  I
                  END IF

*                Check current minimum against current pixel value.

                  IF ( VALUE .LT. DMINIM ) THEN
                     DMINIM  =  VALUE
                     MINPOS  =  I
                  END IF

               END IF
            END DO

*       Now for the clipping case.

         ELSE

*          Initialise some statistics.  Positions are undefined because
*          any other value could be a co-ordinate.

            NVALPX =  0
            MXPSCL = VAL__BADI
            MNPSCL = VAL__BADI

*          Define the bounds for a value to be included in the
*          statistics.

            LOWER = TMEAN - CLIP( J ) * TSTDEV
            UPPER = TMEAN + CLIP( J ) * TSTDEV

*          Loop for all pixels in the array.

            DO  I  =  1, DIM

*             Only include valid pixels in the statistics.

               IF ( ARRAY( I ) .NE. VAL__BADR ) THEN
                  IF ( ARRAY( I ) .GT. LOWER .AND.
     :                 ARRAY( I ) .LT. UPPER ) THEN

                     NVALPX  =  NVALPX  +  1
                     VALUE   =  DBLE( ARRAY( I ) )
                     DTOTAL  =  DTOTAL + VALUE
                     DTOTSQ  =  DTOTSQ + VALUE * VALUE

*                   Check current maximum against current pixel value.

                     IF ( VALUE .GT. DMAXIM ) THEN
                        DMAXIM  =  VALUE
                        MXPSCL  =  I
                     END IF

*                   Check current minimum against current pixel value.

                     IF ( VALUE .LT. DMINIM ) THEN
                        DMINIM  =  VALUE
                        MNPSCL  =  I
                     END IF

                  END IF
               END IF

*          End of the loop for all pixels.

            END DO
         END IF

*       Now form returned quantities.  The standard deviation (sigma)
*       is formed from the equation for the variance (sigma squared) :
*                                               _
*        variance  =   Sum over n pixels ( x   -   x )**2
*                                           i
*                      ----------------------------------
*
*                                  ( n - 1 )
*
*       This can be algebraically manipulated to the following :
*                                          _
*        variance  =   Sum(( x )**2)  -  n.x**2
*                             i
*                      ------------------------
*
*                              ( n - 1 )
*
*       and then standard deviation equals sqrt( variance ).
*
*       First check that there is at least one valid pixel.

         IF ( NVALPX .GT. 0 ) THEN

*          Compute the mean.

            DNMPIX  =  DBLE( NVALPX )
            DMEAN  =  DTOTAL / DNMPIX

*          Now the variance.

            VARNCE  =  ( DTOTSQ - DNMPIX * DMEAN * DMEAN )

*          Error check for case of 1x1 box---will result in a divide by
*          zero---and for rounding errors. Non-zero variances can be
*          generated by rounding errors even when all the pixels are
*          identical.  If this is so then the variance should be zero.
*          Also check for negative variances which can arise through
*          rounding errors.

            IF ( DIM .EQ. 1 .OR. DMAXIM .EQ. DMINIM .OR.
     :           VARNCE .LT. 0.0D0 ) THEN
               VARNCE  =  0.0D0
            ELSE
              VARNCE  =  VARNCE / ( DNMPIX - 1.0D0 )
            ENDIF

*          Calculate the standard deviation = sqrt( variance ).

            DSTDDV  =  SQRT( VARNCE )

*          Convert returned values back to single precision.

            TTOTAL  =  REAL( DTOTAL )
            TMEAN   =  REAL( DMEAN )
            TMAX  =  REAL( DMAXIM )
            TMIN  =  REAL( DMINIM )
            TSTDEV  =  REAL( DSTDDV )

         ELSE

*          Statistics are undefined.

            TTOTAL =  0
            TMEAN  = VAL__BADR
            TMAX = VAL__BADR
            TMIN = VAL__BADR
            TSTDEV = VAL__BADR
         END IF

         IF ( J .EQ. 0 ) THEN

*         Copy the temporary statistics as the unclipped values for
*         return.

            TOTAL = TTOTAL
            MAXMUM = TMAX
            MINMUM = TMIN
            MEAN = TMEAN
            STDDEV = TSTDEV

*          The number of invalid pixels is number in array less the
*          valid ones.

            NINVAL = DIM - NVALPX

         ELSE IF ( J .EQ. NOCL .OR. NVALPX .EQ. 0 ) THEN

*          Copy the temporary statistics as the clipped values for
*          return.

            TOTLCL = TTOTAL
            MAXMCL = TMAX
            MINMCL = TMIN
            MEANCL = TMEAN
            STDVCL = TSTDEV
            NPIXCL = NVALPX
        END IF

*      No point in clipping further when there are no pixels left.

        IF ( NVALPX .EQ. 0 ) GOTO 999
      END DO

 999  CONTINUE

*    That's it - return.

      END
