      SUBROUTINE SURFLIB_REM_GRID ( N_PTS, NX, NY,
     :     IJ, GRID, IN_DATA, STATUS)
*+
*  Name:
*     SURFLIB_REM_GRID

*  Purpose:
*     Remove an image grid of data from demodulated data

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SURFLIB_REM_GRID( N_POS, N_BOLS, NX, NY,
*    :     IJ, GRID, IN_DATA, STATUS)

*  Description:
*     Removes a gridded image from demodulated data using a
*     lookup table containing the (I,J) coordinates (in the grid)
*     of each input data point.
*     Can be used to remove the source from an input data set.

*  Arguments:
*     N_PTS = INTEGER (Given)
*        Number of data points in input data
*     NX = INTEGER (Given)
*        X dimension of input grid
*     NY = INTEGER (Given)
*        Y dimension of input grid
*     IJ (2, N_PTS)  INTEGER (Given)
*        Positions of each point (I,J) in input grid
*     GRID ( NX, NY) = REAL (Given)
*        Image grid to be subtracted from data
*     IN_DATA( N_PTS ) = REAL (Given & Returned)
*        Input data to be modified
*     STATUS = INTEGER (Given & Returned)
*        Global Status

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)

*  Notes:


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.4  1999/08/03 19:32:52  timj
*     Add copyright message to header.
*
*     Revision 1.3  1999/07/17 02:50:15  timj
*     Check for a bad value in the index array
*
*     Revision 1.2  1999/05/15 01:43:47  timj
*     Remove unused variables
*
*     Revision 1.1  1998/06/17 07:42:07  timj
*     Initial revision
*

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'                          ! For VAL__BAD

*  Arguments Given:
      INTEGER N_PTS
      INTEGER NX
      INTEGER NY
      INTEGER IJ ( 2, N_PTS )
      REAL    GRID ( NX, NY )

*  Arguments Given & Returned:
      REAL    IN_DATA ( N_PTS )

*  Status
      INTEGER STATUS

*  Local Variables:
      INTEGER I                       ! Loop counter
      INTEGER IPOS                    ! I position in grid
      INTEGER JPOS                    ! J position in grid

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Loop over input points

      DO I = 1, N_PTS

*     Find position in grid
         IPOS = IJ(1, I)
         JPOS = IJ(2, I)

         IF (IPOS .NE. VAL__BADI .AND. JPOS .NE. VAL__BADI) THEN

            IF ((IPOS .GT. 1) .AND. (IPOS .LE. NX) .AND. (JPOS.GT.1)
     :           .AND. (JPOS .LE. NY)) THEN

*     Remove grid
*     If input data is bad do nothing

               IF (IN_DATA(I) .NE. VAL__BADR) THEN

*     If grid value is bad (should not happen in the case this routine
*     was written for but check for it anyway) then data value is set to bad

*               if (in_data(I) .gt.0.015) then
*                  npos = int(i / 37) + 1
*                  nbol = i - ((npos-1) * 37)
*                  if (nbol.eq.0) then
*                     nbol = 37
*                     npos = npos - 1
*                  end if
*                  print *,i,nbol,npos,in_data(i),ipos,jpos,
*     :                 grid(ipos,jpos),in_data(i)-grid(ipos,jpos)
*               end if

                  IF (GRID(IPOS, JPOS) .EQ. VAL__BADR) THEN
                     IN_DATA ( I ) = VAL__BADR
                  ELSE
                     IN_DATA ( I ) = IN_DATA ( I ) - GRID (IPOS, JPOS)
                  END IF

               END IF

            END IF

         END IF

      END DO

      END

