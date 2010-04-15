      SUBROUTINE SURFLIB_CALC_GRIDIJ (TYPE, NX, NY, ICEN, JCEN,
     :     I, J, STATUS)
*+
*  Name:
*     SURFLIB_CALC_GRIDIJ

*  Purpose:
*     Calculate the position of a pixel number in an IJ grid

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SURFLIB_CALC_GRIDIJ( TYPE, NX, NY, ICEN, JCEN, I, J, STATUS)

*  Description:
*     Calculates the I,J grid positions related to pixel numbers
*     1 to NX*NY.
*     Must calculate all positions in one go since it is very inefficient
*     to calculate the spiral positions on demand
*     Three modes types are currently available.

*  Arguments:
*     TYPE = CHAR (Given)
*       Type of unwrapping. Can be one of:
*       XLINEAR - unfold each X strip in turn for each Y
*       YLINEAR - unfold each Y strip in turn for each X
*       SPIRAL - unfold in a spiral from the reference pixels (ICEN,JCEN)
*       DIAG1  - Diagonal starting at 1,1
*       DIAG2  - Diagonal starting at NX,1
*     NX = INTEGER (Given)
*       Number of pixels in X
*     NY = INTEGER (Given)
*       Number of pixels in Y
*     ICEN = INTEGER (Given)
*       Location of reference pixel in X
*     JCEN = INTEGER (Given)
*       Location of reference pixel in Y
*     I ( NX * NY ) = INTEGER (Returned)
*       X Positions in grid, indexed by pixel number
*     J ( NX * NY ) = INTEGER (Returned)
*       Y Positions in grid, indexed by pixel number
*     STATUS = INTEGER (Given & Returned)
*       Global Status

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     Original version: Timj, 1997 Oct 23
*     $Log$
*     Revision 1.4  2004/09/01 01:06:58  timj
*     fix uninitialised warnings
*
*     Revision 1.3  1999/08/06 02:29:04  timj
*     Tweak headers for use with PROLAT.
*
*     Revision 1.2  1999/08/03 19:32:47  timj
*     Add copyright message to header.
*
*     Revision 1.1  1997/11/12 00:11:59  timj
*     Initial revision
*
*

*  Notes:

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'                          ! Bad values

*  Arguments Given:
      INTEGER ICEN
      INTEGER JCEN
      INTEGER NX
      INTEGER NY
      CHARACTER * (*) TYPE

*  Arguments Returned:
      INTEGER I(NX*NY)
      INTEGER J(NX*NY)

*     Status
      INTEGER STATUS

*     Local Variables:
      INTEGER CURR_I               ! Current I position
      INTEGER CURR_J               ! Current J position
      INTEGER CURR_LEG             ! Current leg of spiral (1..4)
      INTEGER CURR_LENGTH          ! Current distance along leg
      INTEGER CURR_POS             ! Current position in loop
      INTEGER DX                   ! Change in I this time round loop
      INTEGER DY                   ! Change in J this time round loop
      INTEGER NSTEPS               ! Number of steps for this leg
      INTEGER POS                  ! Loop counter
      INTEGER STARTX               ! Start x pos of strip
      INTEGER STARTY               ! Start y pos of strip


*.
      IF (STATUS .NE. SAI__OK) RETURN

      DX = 0
      DY = 0

      IF (TYPE .EQ. 'XLINEAR') THEN

*     Just a simple strip through x at each y

         DO POS = 1, NX * NY

            J(POS) = INT((POS - 1) / NX) + 1
            I(POS) = POS - ((J(POS)-1) * NX)

         END DO

      ELSE IF (TYPE .EQ. 'YLINEAR') THEN

*     Just a simple strip through y at each x

         DO POS = 1, NX * NY

            I(POS) = INT((POS - 1) / NY) + 1
            J(POS) = POS - ((I(POS)-1) * NY)

         END DO

      ELSE IF (TYPE .EQ. 'SPIRAL') THEN

*     Start at the reference pixels and spiral out
*     Must take care of going over the edge.
*     Can't think of any trick for doing this without going
*     through every position of the spiral to get to the required position.

         I(1) = ICEN
         J(1) = JCEN

         CURR_POS = 2     ! The CURR_POS = 1 is a special case
         CURR_I = ICEN
         CURR_J = JCEN
         CURR_LEG = 4     ! ie just finished leg 4
         CURR_LENGTH = 0  ! Number of times left round for this leg
         NSTEPS = 0

         DO WHILE (CURR_POS .LE. NX * NY)

*     I change leg if I have been round the loop the required number of
*     times for this leg (ie no length left)

            IF (CURR_LENGTH .EQ. 0) THEN

               CURR_LEG = CURR_LEG + 1
               IF (CURR_LEG .GT. 4) CURR_LEG = 1

*     Every two legs the number of steps per leg goes up by one Every
*     2nd leg we have to increment steps

               IF (CURR_LEG .EQ. 1 .OR. CURR_LEG .EQ. 3) THEN
                  NSTEPS = NSTEPS + 1
               END IF

               CURR_LENGTH = NSTEPS
            END IF

*     There are four 'legs' involved in the spiral. Need to decide which
*     one I am on.

            IF (CURR_LEG .EQ. 1) THEN
               DX = 0
               DY = -1
            ELSE IF (CURR_LEG .EQ. 2) THEN
               DX = 1
               DY = 0
            ELSE IF (CURR_LEG .EQ. 3) THEN
               DX = 0
               DY = 1
            ELSE IF (CURR_LEG .EQ. 4) THEN
               DX = -1
               DY = 0
            END IF

*     Now add on the increment (if we are allowed)

            CURR_I = CURR_I + DX
            CURR_J = CURR_J + DY

*     Decrement the step
            CURR_LENGTH = CURR_LENGTH - 1

*     Incerement the current position if this is inside the grid
*     otherwise we just loop round again until we get back onto the
*     grid.

            IF (CURR_I .LE. NX .AND. CURR_J .LE. NY
     :           .AND. CURR_I .GT. 0 .AND. CURR_J .GT. 0) THEN
               I(CURR_POS) = CURR_I
               J(CURR_POS) = CURR_J
*               PRINT *,CURR_POS, I(CURR_POS), J(CURR_POS)
               CURR_POS = CURR_POS + 1
            END IF

         END DO

      ELSE IF (TYPE .EQ. 'DIAG1') THEN
*     Go through grid diagonally starting at (1,1)

         STARTX = 0
         STARTY = 1
         CURR_I = 0
         CURR_J = 0

         DO POS = 1, NX * NY

*     Go along the edge of the grid

*     If we have reached the end of a diagonal then we need to
*     start on a new strip

            IF (CURR_I .EQ. 0 .OR. CURR_J .GT. NY) THEN

*     This is the starting coordinate of each diagonal
               IF (STARTX .LT. NX) THEN
                  STARTX = STARTX + 1
               ELSE
                  STARTY = STARTY + 1
               END IF

               CURR_I = STARTX
               CURR_J = STARTY

            END IF

*     Now set IJ

            I(POS) = CURR_I
            J(POS) = CURR_J

*            print *,POS, CURR_I, CURR_J

*     and change the current position

            CURR_I = CURR_I - 1
            CURR_J = CURR_J + 1


         END DO

      ELSE IF (TYPE .EQ. 'DIAG2') THEN
*     Go through grid diagonally starting at (1,NX)

         STARTX = NX + 1
         STARTY = 1
         CURR_I = STARTX
         CURR_J = 0

         DO POS = 1, NX * NY

*     Go along the edge of the grid

*     If we have reached the end of a diagonal then we need to
*     start on a new strip

            IF (CURR_I .GT. NX .OR. CURR_J .GT. NY) THEN

*     This is the starting coordinate of each diagonal
               IF (STARTX .GT. 1) THEN
                  STARTX = STARTX - 1
               ELSE
                  STARTY = STARTY + 1
               END IF

               CURR_I = STARTX
               CURR_J = STARTY

            END IF

*     Now set IJ

            I(POS) = CURR_I
            J(POS) = CURR_J

*            print *,POS, CURR_I, CURR_J

*     and change the current position

            CURR_I = CURR_I + 1
            CURR_J = CURR_J + 1


         END DO



      ELSE

         CALL MSG_SETC('TY', TYPE)
         STATUS = SAI__ERROR

         CALL ERR_REP(' ','SURFLIB_CALC_GRIDIJ: ^TY is not a valid '//
     :        ' grid type', STATUS)


      END IF

      END
