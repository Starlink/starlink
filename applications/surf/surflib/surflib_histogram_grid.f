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
*      SUBROUTINE SURFLIB_HISTOGRAM_GRID (N_PTS, NX, NY, USEDATA,
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

*  History:
*     Original version: Timj, 1997 Oct 21
*     $Log$
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

*    External functions:
      INCLUDE 'NDF_FUNC'
 
*.
      IF (STATUS .NE. SAI__OK) RETURN

*     Loop through all points in the input data
*     adding into the histogram

      DO I = 1, N_PTS

         IF ((IN_DATA(I) .NE. VAL__BADR) .AND.
     :        (NDF_QMASK(IN_QUALITY(I), BADBIT))) THEN

            GRID(IJ(1,I), IJ(2,I)) = GRID(IJ(1,I), IJ(2,I)) + 1

         END IF

      END DO

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

      END
