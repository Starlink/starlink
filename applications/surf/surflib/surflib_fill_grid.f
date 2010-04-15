      SUBROUTINE SURFLIB_FILL_GRID (N_PTS, NX, NY, NMAX, OFFSET,
     :     IN_DATA, IN_QUALITY, BADBIT, IJ, GRID, BINS, BIN_POS,
     :     STATUS )

*+
*  Name:
*     SURFLIB_FILL_GRID

*  Purpose:
*     Populate grid with data points corresponding to position

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL SURFLIB_FILL_GRID (N_PTS, NX, NY, NMAX, OFFSET,
*     :     IN_DATA, IN_QUALITY, BADBIT, IJ, GRID, BINS, BIN_POS,
*     :     STATUS )

*  Description:
*     Given an array of I,J coordinates associated with data values,
*     this routine places each data point onto the rectangular grid
*     and the position in the data array into a corresponding grid.
*     Grid is of size (NX,NY,NMAX) and each data point is placed into
*     a different layer (up to NMAX) for each I,J. We already know the
*     value of NMAX since the output array has been pre-allocated.

*  Arguments:
*     N_PTS = INTEGER (Given)
*       Number of I,J pairs supplied
*     NX = INTEGER (Given)
*       Size of X dimension
*     NY = INTEGER (Given)
*       Size of Y dimension
*     NMAX = INTEGER (Given)
*       Maximum value allowed for third dimension of BINS
*     OFFSET = INTEGER (Given)
*       This is the offset in the IJ data array. Not used for IJ itself
*       (Since this is added to the pointer of the input array if needed)
*       in this array but is added onto each of the values placed into
*       the BIN_POS array so that we can keep track of the input data
*       without using a further two pieces of information.
*     IN_DATA(N_PTS) = REAL (Given)
*       Input data
*     IN_QUALITY(N_PTS) = BYTE (Given)
*       Input quality
*     BADBIT = BYTE (Given)
*       Bad bits mask for quality array
*     IJ ( 2, N_PTS ) = INTEGER (Given)
*       Positions of each point (I,J) in output grid for each N_PTS
*     GRID (NX, NY) = INTEGER (Given & Returned)
*       Scratch space for keeping track of the number of points added
*       into each I,J
*     BINS(NX, NY, NMAX) = REAL (Given & Returned)
*       The data stored in relation to its position
*     BIN_POS (NX, NY, NMAX) = INTEGER (Given & Returned)
*       The position in the input data arrays associated with each data point
*       in BINS
*     STATUS = INTEGER (Given & Returned)
*       Global Status

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)

*  Notes:
*     - GRID is not initialised by this routine since it is incremented
*       each time a point is placed in BINS
*     - BINS and BIN_POS are not initialised here but should be filled
*       with bad before this routine is first called.
*     - a status of SAI__WARN is returned if the indices lie outside
*       the model area. It is up to the calling routine to decide
*       whether to halt.


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     Original version: Timj, 1997 Oct 21
*     $Log$
*     Revision 1.5  1999/08/03 19:32:49  timj
*     Add copyright message to header.
*
*     Revision 1.4  1999/07/17 02:49:48  timj
*     Check for a bad value in the index array
*
*     Revision 1.3  1998/05/20 22:21:17  timj
*     Make sure index range checking is correct (GE 1 instead of .GT.1)
*
*     Revision 1.2  1998/05/20 19:18:36  timj
*     Distinguish not having enough memory from not having a large
*     enough grid.
*
*     Revision 1.1  1997/10/28 19:06:45  timj
*     Initial revision
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
      INTEGER NMAX
      INTEGER OFFSET
      INTEGER IJ(2, N_PTS)
      REAL    IN_DATA(N_PTS)
      BYTE    IN_QUALITY(N_PTS)
      BYTE    BADBIT

*  Arguments Given & Returned:
      INTEGER GRID(NX, NY)
      REAL    BINS(NX, NY, NMAX)
      INTEGER BIN_POS(NX, NY, NMAX)

*  Arguments Returned:

*     Status
      INTEGER STATUS

*     Local Variables:
      INTEGER I               ! Loop variable
      INTEGER IFAIL           ! Error status of routine
      INTEGER NMAX_EXCEED     ! Required value of NMAX

*    External functions:
      INCLUDE 'NDF_FUNC'

*.
      IF (STATUS .NE. SAI__OK) RETURN

*     Set IFAIL to keep track of problems
      IFAIL = 0

*     Loop through all points in the input data
*     and place in the output in the correct place

      DO I = 1, N_PTS

*     Check the data and check that the indices do not have bad values
         IF ((IN_DATA(I) .NE. VAL__BADR) .AND.
     :        (NDF_QMASK(IN_QUALITY(I), BADBIT)) .AND.
     :        IJ(1,I) .NE. VAL__BADI .AND.
     :        IJ(2,I) .NE. VAL__BADI) THEN

*     Check that the index is within range
            IF ((IJ(1,I) .LE. NX) .AND. (IJ(1,I) .GE. 1) .AND.
     :           (IJ(2,I) .LE. NY) .AND. (IJ(2,I) .GE. 1)) THEN

*     GRID tells us the 'Z' position in the output array
               GRID(IJ(1,I), IJ(2,I)) = GRID(IJ(1,I), IJ(2,I)) + 1

*     Now put in the value (assuming Z index is not exceeded

               IF (GRID(IJ(1,I), IJ(2,I)) .LE. NMAX) THEN

                  BINS(IJ(1,I), IJ(2,I), GRID(IJ(1,I), IJ(2,I))) =
     :                 IN_DATA(I)
                  BIN_POS(IJ(1,I), IJ(2,I), GRID(IJ(1,I), IJ(2,I))) =
     :                 OFFSET + I


               ELSE
*     Oh dear - array not big enough

                  IFAIL = 1
                  NMAX_EXCEED = GRID(IJ(1,I), IJ(2,I))

               END IF

            ELSE
*     Oh dear - index was not within our grid

               IFAIL = 2

            END IF

         END IF

      END DO

*     Report on the status of IFAIL if this all went horribly wrong

      IF (IFAIL .EQ. 1) THEN

         STATUS = SAI__ERROR
         CALL MSG_SETI('NMAX', NMAX)
         CALL MSG_SETI('XC', NMAX_EXCEED)
         CALL ERR_REP(' ','SURFLIB_FILL_GRID: Work array too small.'//
     :        ' Needed Z=^XC but got Z=^NMAX', STATUS)

      ELSE IF (IFAIL .EQ. 2) THEN

         STATUS = SAI__WARN
         CALL ERR_REP(' ','SURFLIB_FILL_GRID: Index out of range',
     :        STATUS)

      END IF


      END
