      SUBROUTINE CAP_GNHST (XMIN, XMAX, BINWID, NORML, PTS, XVAL, BINS,
     :  IHISTY, HISTX, HISTY, YMIN, YMAX, NUMINC, NUMEXC, STATUS)
*+
*  Name:
*     CAP_GNHST
*  Purpose:
*     Generate a histogram.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GNHST (XMIN, XMAX, BINWID, NORML, PTS, XVAL, BINS;
*      IHISTY; HISTX, HISTY, YMIN, YMAX, NUMINC, NUMEXC; STATUS)
*  Description:
*     Generate a histogram.  Given an array of values bin them into
*     a histogram.  Arrays are returned containing the central value
*     of each histogram bin and the number of points in each bin.
*  Arguments:
*     XMIN  =  REAL (Given)
*        X minimum of the range to be histogrammed.
*     XMAX  =  REAL (Given)
*        Y minimum of the range to be histogrammed.
*     BINWID  =  REAL (Given)
*        Width of each histogram bin.
*     NORML  =  LOGICAL (Given)
*        Flag indicating whether the histogram is to be normalised or
*        not, coded as follows:
*        .TRUE.  - normalise the histogram,
*        .FALSE. - do not normalise the histogram.
*     PTS  =  INTEGER (Given)
*        Number of points in the array to be histogrammed.
*     XVAL(PTS)  =  REAL (Given)
*        Array of points to be histogrammed.
*     BINS  =  INTEGER (Given)
*        Number of histogram bins.
*     IHISTY(BINS)  =  INTEGER (Used)
*        Work array containing the INTEGER number of points in each
*        histogram bin.
*     HISTX(BINS)  =  REAL (Returned)
*        X coordinates of the histogram: the mid-position of each bin.
*     HISTY(BINS)  =  REAL (Returned)
*        X coordinates of the histogram: the number of points in each
*        bin (normalised to the total number of points, if required).
*     YMIN  =  REAL (Returned)
*        Minimum number of points in any bin (normalised to the total
*        number of points, if required).
*     YMAX  =  REAL (Returned)
*        Maximum number of points in any bin (normalised to the total
*        number of points, if required).
*     NUMINC  =  INTEGER (Returned)
*        Number of points included in the histogram.
*     NUMEXC  =  INTEGER (Returned)
*        Number of points excluded from the histogram (because they
*        lie outside the specified range).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Generate the central X values of each histogram bin and also
*     initialise the histogram array.
*     For every point
*       If it is inside the histogram range then
*         Calculate the bin the point falls in.
*         Increment the count for this bin.
*       else
*         Increment the number of points outside the range.
*       end if
*     end for
*     If normalisation is required then
*       Normalise the histrogram.
*     else
*       Copy the INTEGER histogram to the REAL return array.
*     end if
*     Find the minimum and maximum Y values.
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/9/99 (ACD): Original version.
*     1/12/99 (ACD): Changed SNGL to REAL for Linux.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      INTEGER
     :  PTS,
     :  BINS
      REAL
     :  XMIN,
     :  XMAX,
     :  BINWID,
     :  XVAL(PTS)
      LOGICAL
     :  NORML
*  Arguments Returned:
      INTEGER
     :  IHISTY(BINS),
     :  NUMINC,
     :  NUMEXC
      REAL
     :  HISTX(BINS),
     :  HISTY(BINS),
     :  YMIN,
     :  YMAX
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  CURBIN,  ! Current histogram bin.
     :  CURPT    ! Current point in array to be histogrammed.
      REAL
     :  XSTART   ! Start value for computing histogram bin centres.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Generate the central X values of each histogram bin and also
*       initialise the histogram array.

         XSTART = XMIN - (BINWID * 5.0E-1)

         DO CURBIN = 1, BINS
            HISTX(CURBIN) = XSTART + (BINWID * REAL(CURBIN))
            IHISTY(CURBIN) = 0
         END DO

*
*       Generate the histogram.

         NUMINC = 0
         NUMEXC = 0

         DO CURPT = 1, PTS

*
*          Check whether the point lies in the permitted range.

            IF (XVAL(CURPT) .GE. XMIN  .AND.  XVAL(CURPT) .LE. XMAX)
     :        THEN
               NUMINC = NUMINC + 1

*
*             Calculate the bin the point falls in.

               CURBIN = INT((XVAL(CURPT) - XMIN) / BINWID) + 1

               CURBIN = MAX(CURBIN, 1)
               CURBIN = MIN(CURBIN, BINS)

*
*             Increment the count for this bin.  Note that the histogram
*             is accummulated in an INTEGER array to avoid rounding
*             errors.

               IHISTY(CURBIN) = IHISTY(CURBIN) + 1

            ELSE

*
*             Increment the number of excluded points which fall outside
*             the range.

               NUMEXC = NUMEXC + 1

            END IF
         END DO

*
*       Either normalise the histrogram or copy it to the REAL return
*       array.

         IF (NORML) THEN
            DO CURBIN = 1, BINS
               HISTY(CURBIN) = REAL(IHISTY(CURBIN) ) / REAL(NUMINC)
            END DO
         ELSE
            DO CURBIN = 1, BINS
               HISTY(CURBIN) = REAL(IHISTY(CURBIN) )
            END DO
         END IF

*
*       Find the minimum and maximum Y values.

         YMIN = HISTY(1)
         YMAX = HISTY(1)

         DO CURBIN = 2, BINS
            IF (HISTY(CURBIN) .LT. YMIN) THEN
               YMIN = HISTY(CURBIN)
            END IF

            IF (HISTY(CURBIN) .GT. YMAX) THEN
               YMAX = HISTY(CURBIN)
            END IF
         END DO

      END IF

      END
