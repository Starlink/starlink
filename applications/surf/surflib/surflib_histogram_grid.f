      SUBROUTINE SURFLIB_HISTOGRAM_GRID (N_PTS, NX, NY, USEDATA,
     :     IN_DATA, IN_QUALITY, BADBIT, IJ, GRID, IMAX, JMAX, NMAX,
     :     STATUS )

*+
*  Name:
*     SURFLIB_HISTOGRAM_GRID

*  Purpose:
*     Calculate a 2-D histogram of I,J coordinates on a grid

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL SURFLIB_HISTOGRAM_GRID (N_PTS, NX, NY, USEDATA,
*     :     IN_DATA, IN_QUALITY, BADBIT, IJ, GRID, IMAX, JMAX, NMAX,
*     :     STATUS )


*  Description:
*     Given an array of I,J coordinates calculate the histogram
*     of all points onto this grid of size NX * NY. Just tells you
*     how many data points there are per cell. Returns the histogram
*     and the highest value in the histogram

*  Arguments:
*     N_PTS = INTEGER (Given)
*       Number of I,J pairs supplied
*     NX = INTEGER (Given)
*       Size of X dimension
*     NY = INTEGER (Given)
*       Size of Y dimension
*     USEDATA = LOGICAL (Given)
*       If TRUE the histogram takes bad data points into account by not
*       including them in the histogram. If false the straight histogram
*       of the I,J's is constructed.
*     IN_DATA(N_PTS) = REAL (Given)
*       Input data
*     IN_QUALITY(N_PTS) = BYTE (Given)
*       Input quality
*     BADBIT = BYTE (Given)
*       Bad bits mask for quality array
*     IJ ( 2, N_PTS ) = INTEGER (Given)
*       Positions of each point (I,J) in output grid
*     GRID (NX, NY) = INTEGER (Given & Returned)
*       Histogram. Note that array is not cleared by this routine leaving
*       the possibility that the routine can be called multiple times
*       to include more than one data set.
*     IMAX = INTEGER (Returned)
*       I position of maximum [first one encountered]
*     JMAX = INTEGER (Returned)
*       J position of maximum [first one encountered]
*     NMAX = INTEGER (Returned)
*       Maximum value in histogram
*     STATUS = INTEGER (Given & Returned)
*       Global Status

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)
*     John Lightfoot (jfl@roe.ac.uk)

*  Notes:
*     - If USEDATA is TRUE the data and quality array are used so that bad
*       pixels can be identified and not included into the histogram of
*       positions.
*     - GRID is not initialised by this routine
*     - A warning error is raised if the bounds of the histogram
*       are exceeded. It is up to the caller to decide whether this
*       is a fatal error.


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     Original version: Timj, 1997 Oct 21
*     $Log$
*     Revision 1.5  1999/08/06 02:29:06  timj
*     Tweak headers for use with PROLAT.
*
*     Revision 1.4  1999/08/03 19:32:50  timj
*     Add copyright message to header.
*
*     Revision 1.3  1999/07/17 02:49:49  timj
*     Check for a bad value in the index array
*
*     Revision 1.2  1997/10/22 02:02:21  timj
*     Add check for index range.
*     Really do the check for good data.
*
*     Revision 1.1  1997/10/22 01:39:16  timj
*     Initial revision
*
*
*

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'                          ! Bad values

*  Arguments Given:
      INTEGER N_PTS
      INTEGER NX
      INTEGER NY
      LOGICAL USEDATA
      INTEGER IJ(2, N_PTS)
      REAL    IN_DATA(N_PTS)
      BYTE    IN_QUALITY(N_PTS)
      BYTE    BADBIT

*  Arguments Given & Returned:
      INTEGER GRID(NX, NY)

*  Arguments Returned:
      INTEGER NMAX
      INTEGER JMAX
      INTEGER IMAX

*     Status
      INTEGER STATUS

*     Local Variables:
      INTEGER I               ! Loop variable
      INTEGER J               ! Loop index
      LOGICAL WARNING         ! Flag to indicate a warning should be issued

*    External functions:
      INCLUDE 'NDF_FUNC'

*.
      IF (STATUS .NE. SAI__OK) RETURN

*     Set no warning
      WARNING = .FALSE.


*     Loop through all points in the input data
*     adding into the histogram.
*     Should make sure that the I,Js are valid

*     Implement one loop that checks for data and one that just uses IJ
*     More efficient than putting the IF inside the loop.

      IF (USEDATA) THEN

         DO I = 1, N_PTS

*     Check that data has a good value and that the index is not
*     a bad value
            IF ((IN_DATA(I) .NE. VAL__BADR) .AND.
     :           (NDF_QMASK(IN_QUALITY(I), BADBIT)) .AND.
     :           IJ(1,I) .NE. VAL__BADI .AND.
     :           IJ(2,I) .NE. VAL__BADI ) THEN

*     Check bounds
               IF ((IJ(1,I) .LE. NX) .AND. (IJ(1,I) .GT. 1) .AND.
     :              (IJ(2,I) .LE. NY) .AND. (IJ(2,I) .GT. 1)) THEN

*     Place in grid
                  GRID(IJ(1,I), IJ(2,I)) = GRID(IJ(1,I), IJ(2,I)) + 1

               ELSE

                  WARNING = .TRUE.

               END IF

            END IF

         END DO

      ELSE

         DO I = 1, N_PTS

*     Ignore bad pixels
            IF (IJ(1,I) .NE. VAL__BADI .AND.
     :           IJ(2,I) .NE. VAL__BADI) THEN

*     Check bounds
               IF ((IJ(1,I) .LE. NX) .AND. (IJ(1,I) .GT. 1) .AND.
     :              (IJ(2,I) .LE. NY) .AND. (IJ(2,I) .GT. 1)) THEN

*     Place in grid
                  GRID(IJ(1,I), IJ(2,I)) = GRID(IJ(1,I), IJ(2,I)) + 1

               ELSE

                  WARNING = .TRUE.

               END IF

            END IF

         END DO

      END IF


*     Find the maximum position

      NMAX = 0
      IMAX = 0
      JMAX = 0

      DO I = 1, NX
         DO J = 1, NY

            IF (GRID(I,J) .GT. NMAX) THEN
               NMAX = GRID(I,J)
               JMAX = J
               IMAX = I
            END IF

         END DO
      END DO

*     Issue a warning if necessary. The WARNING flag will only
*     be set if an index was found to be out of range in array IJ

      IF (WARNING) THEN
         STATUS = SAI__WARN
         CALL ERR_REP(' ', 'SURFLIB_HISTOGRAM_GRID:'//
     :        ' The supplied indices lie outside the histogram'//
     :        'grid', STATUS)

      END IF


      END
