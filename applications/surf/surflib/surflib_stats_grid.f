      SUBROUTINE SURFLIB_STATS_GRID(SMODE, NX, NY, NMAX, NSIGMA,
     :     IPOS, JPOS, BINS, PNTS, STATS, STATUS)
*+
*  Name:
*     SURFLIB_STATS_GRID

*  Purpose:
*     Calculate statistics of binned data

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SURFLIB_STATS_GRID(SMODE, NX, NY, NMAX, NSIGMA,
*    :     IPOS, JPOS, BINS, PNTS, STATS, STATUS)

*  Description:
*     Calculate the statistics of the binned data and return the
*     median and upper and lower limits of acceptable data.

*  Arguments:
*     SMODE = CHARACTER (Given)
*       Smoothing mode. The statistics can be smoothed by adjacent pixels
*       in order to remove spikes from the clipping envelope.
*       Options are:
*          NONE - No smoothing (ie the statistics of each bin)
*          HANN - Hanning smoothing. Triangular envolope across 3 points.
*       Note that smoothing depends on the way the grid was unwrapped.
*       since that determines which pixels are adjacent (except for
*       SMODE=NONE.
*     NX = INTEGER (Given)
*       Size of X dimension
*     NY = INTEGER (Given)
*       Size of Y dimension
*     NMAX = INTEGER (Given)
*       Maximum value allowed for third dimension of BINS
*     NSIGMA = REAL (Given)
*       Standard deviation limit.
*     IPOS(NX * NY) = INTEGER (Given)
*       I coordinate for each pixel
*     JPOS(NX * NY) = INTEGER (Given)
*       J coordinate for each pixel
*     BINS(NX, NY, NMAX) = REAL (Given)
*       The data stored in relation to its position
*     PNTS(NMAX) = REAL (Given)
*       Scratch space for copying in the data from each I,J
*     STATS(NX, NY, 3) = REAL (Returned)
*       Statistics of each bin. 3 components are: Median, Upper limit
*       lower limit.
*     STATUS = INTEGER (Given & Returned)
*       Global status.

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)

*  Notes:


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.3  2004/09/01 01:02:03  timj
*     use CNF_PVAL
*
*     Revision 1.2  1999/08/03 19:32:54  timj
*     Add copyright message to header.
*
*     Revision 1.1  1997/11/12 00:13:48  timj
*     Initial revision
*

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'                          ! Bad values
      INCLUDE 'CNF_PAR'                          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER NMAX
      REAL    NSIGMA
      REAL    BINS(NX, NY, NMAX)
      REAL    PNTS(NMAX)
      INTEGER IPOS(NX * NY)
      INTEGER JPOS(NX * NY)
      CHARACTER *(*) SMODE

*  Arguments Returned:
      REAL    STATS(NX, NY, 3)


*     Status
      INTEGER STATUS

*  Local Constants:
      INTEGER MAX__LEN        ! Maximum number of pixels to smooth over
      PARAMETER (MAX__LEN = 3)

*  Local Variables:
      BYTE    BTEMP           ! Temporary byte (for Badbit mask)
      INTEGER COUNT           ! Loop counter
      INTEGER I               ! Loop variable
      INTEGER IERR            ! For VEC_
      INTEGER ITEMP           ! Temporary integer
      INTEGER J               ! J coordinate
      DOUBLE PRECISION MEAN   ! Mean
      DOUBLE PRECISION MEDIAN ! Median
      INTEGER N               ! Loop counter
      INTEGER NERR            ! For VEC_
      INTEGER NI              ! I position for pixel in smooth
      INTEGER NJ              ! J position for pixel in smooth
      INTEGER P               ! High or low counter
      INTEGER QUAL_END        ! end of dummy quality array
      INTEGER QUAL_PTR        ! Dummy quality array
      INTEGER QSORT_END       ! End of sorted array
      INTEGER QSORT_PTR       ! Sort array for statr
      INTEGER RANGE           ! Number of pixels to smooth over
      REAL    RUNN            ! Running total
      INTEGER SCRATCH_END     ! End of scratch space
      INTEGER SCRATCH_PTR     ! Scratch space
      DOUBLE PRECISION STDEV  ! Standard deviation
      DOUBLE PRECISION SUM    ! Sum of data
      DOUBLE PRECISION SUMSQ  ! Sum of squares
      REAL    WEIGHT          ! Total weight
      REAL    WEIGHTS(MAX__LEN)! Weights for each pixel in smoothing
      INTEGER XPOS            ! Position in smoothing array


*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Initialise pointers
      QUAL_PTR = 0
      QUAL_END = 0
      QSORT_PTR = 0
      QSORT_END = 0

*     Get some memory for the stats routine

      CALL SCULIB_MALLOC(NMAX * VAL__NBR, QSORT_PTR, QSORT_END,
     :     STATUS)
      CALL SCULIB_MALLOC(NMAX * VAL__NBUB, QUAL_PTR, QUAL_END,
     :     STATUS)

*     Fill this dummy quality array with 0
      BTEMP = 0
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_CFILLB(NMAX, BTEMP, %VAL(CNF_PVAL(QUAL_PTR)))
      END IF



*     Loop over each position in the grid

      DO COUNT = 1, NX*NY

*     Find the indices
         I = IPOS(COUNT)
         J = JPOS(COUNT)

*     Copy the data to a work array

         DO N = 1, NMAX

            PNTS(N) = BINS(I,J,N)

         END DO

*     Calculate the statistics of this data

         CALL SCULIB_STATR(NMAX, NSIGMA, PNTS, %VAL(CNF_PVAL(QUAL_PTR)),
     :        BTEMP, ITEMP, MEAN, MEDIAN, SUM, SUMSQ, STDEV,
     :        %VAL(CNF_PVAL(QSORT_PTR)), STATUS)

*     Store the required values taking care of bad values
*     This does not check for overflow conditions.

         STATS(I,J,1) = VAL__BADR
         STATS(I,J,2) = VAL__BADR
         STATS(I,J,3) = VAL__BADR

         IF (MEDIAN .NE. VAL__BADD) STATS(I,J,1) = REAL(MEDIAN)

         IF (MEAN .NE. VAL__BADD) THEN
            IF (STDEV .NE. VAL__BADD) THEN

               STATS(I,J,2) = REAL(MEAN + (STDEV * DBLE(NSIGMA)))
               STATS(I,J,3) = REAL(MEAN - (STDEV * DBLE(NSIGMA)))

            END IF
         END IF

      END DO

*     Free the scratch space
      CALL SCULIB_FREE('QSORT', QSORT_PTR, QSORT_END, STATUS)
      CALL SCULIB_FREE('QUAL', QUAL_PTR, QUAL_END, STATUS)

*     Smoothing

      IF (SMODE .EQ. 'HANN') THEN

*     Implement Hanning smoothing of the limits.
*     This is the sum of (0.25*surround) + (0.5*middle)
*     average across 3 points

*     First need some scratch space for the copy of the data
         SCRATCH_PTR = 0
         SCRATCH_END = 0
         CALL SCULIB_MALLOC(2 * NX * NY * VAL__NBR, SCRATCH_PTR,
     :        SCRATCH_END, STATUS)

*     Loop through all pixels
*     except the end pixels which I won't change

         RANGE = 3
         WEIGHTS(1) = 0.25
         WEIGHTS(2) = 0.5
         WEIGHTS(3) = 0.25

*     Loop over each branch (ie high and low stats)
*     Include MEDIAN in the smoothing

         DO P = 1, 3

*     Loop through each pixel
            DO COUNT = 1, NX * NY

*     Find the position
               I = IPOS(COUNT)
               J = JPOS(COUNT)

*     Reset the weight
               WEIGHT = 0.0
               RUNN = 0.0

*     Calculate the sum and weight

               DO N = 1, RANGE

*     Find out the I,J of this particular pixel
*     Making sure that we have not gone out of array bounds

                  ITEMP = INT((REAL(RANGE)+1.0)/2.0)
                  XPOS = COUNT + N - ITEMP

                  IF ((XPOS .GE. 1) .AND. (XPOS .LE. NX*NY)) THEN

                     NI = IPOS(XPOS)
                     NJ = JPOS(XPOS)

                     IF (STATS(NI,NJ,P) .NE. VAL__BADR) THEN
                        RUNN = RUNN + WEIGHTS(N) * STATS(NI,NJ,P)
                        WEIGHT = WEIGHT + WEIGHTS(N)
                     END IF
                  END IF

               END DO

*     Save the value if there were any points
               IF (WEIGHT .GT. 0.0) THEN

                  RUNN = RUNN / WEIGHT

               ELSE

                  RUNN = VAL__BADR

               END IF

*       Copy the value in
               CALL VEC_RTOR(.FALSE., 1, RUNN,
     :   %VAL(CNF_PVAL(SCRATCH_PTR) + ((COUNT-1) * VAL__NBR)),
     :              IERR, NERR, STATUS)

            END DO

*     Copy this data back into the median data

            DO COUNT = 1, NX*NY

               I = IPOS(COUNT)
               J = JPOS(COUNT)

               CALL VEC_RTOR(.FALSE., 1,
     :              %VAL(CNF_PVAL(SCRATCH_PTR) + ((COUNT-1)*VAL__NBR)),
     :              STATS(I,J,P), IERR, NERR, STATUS)

            END DO


         END DO

*     Free the memory
         CALL SCULIB_FREE('SCRT', SCRATCH_PTR, SCRATCH_END, STATUS)

      END IF


      END
