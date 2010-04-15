      SUBROUTINE CAP_PLTGS (XPOSD, YPOSD, SYMBOL, COLOUR, SUNITS,
     :  LABEL, SIZE1, SIZE2, SIZE3, SIZE4, STATUS)
*+
*  Name:
*     CAP_PLTGS
*  Purpose:
*     Plot a single object from a grahics attributes list.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PLTGS (XPOSD, YPOSD, SYMBOL, COLOUR, SUNITS, LABEL,
*       SIZE1, SIZE2, SIZE3, SIZE4; STATUS)
*  Description:
*     Plot a single object from a grahics attributes list.
*  Arguments:
*     XPOSD  =  DOUBLE PRECISION (Given)
*        X position of the point in standard coordinates.
*     YPOSD  =  DOUBLE PRECISION (Given)
*        Y position of the point in standard coordinates.
*     SYMBOL  =  INTEGER (Given)
*        Plotting symbol.
*     COLOUR  =  INTEGER (Given)
*        Colour of the symbol.
*     SUNITS  =  INTEGER (Given)
*        Code for the units of the symbol size.
*     LABEL  =  CHARACTER*(*) (Given)
*        Label for the symbol.
*     SIZE1  =  REAL (Given)
*        First size attribute.
*     SIZE2  =  REAL (Given)
*        Second size attribute.
*     SIZE3  =  REAL (Given)
*        Third size attribute.
*     SIZE4  =  REAL (Given)
*        Fourth size attribute.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Convert the position to single precision.
*     Convert the colour to the PGPLOT colour index.
*     Set the PGPLOT colour index.
*     If the symbol is a dot then
*       Plot the object as a dot.
*     else for the appropriate symbol
*       Generate the symbol.
*       Scale and translate the symbol.
*       Plot the symbol
*     else (repeat for all symbols)
*        .
*        .
*     end if
*     If a label is required then
*       Set the text height.
*       Compute the label position.
*       Plot the label.
*     end if
*  Implementation Deficiencies:
*     The following plotting symbols listed in IOFC/ACD/3.2 are not
*     implemented: x error bar, y error bar, xy error bar, error
*     lozenges.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     20/8/96 (ACD): Original version.
*     3/6/97  (ACD): Converted for CURSA.
*     5/6/97  (ACD): First stable version.
*     18/4/01 (ACD): Implemented ellipses and labels.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'CIO_PAR'           ! CIO parametric constants.
      INCLUDE 'CHART_PAR'         ! CATCHART constants.
*  Global Variables:
      INCLUDE 'CHART_CMN'         ! CATCHART common block.
*  Arguments Given:
      DOUBLE PRECISION
     :  XPOSD,
     :  YPOSD
      INTEGER
     :  SYMBOL,
     :  COLOUR,
     :  SUNITS
      CHARACTER
     :  LABEL*(*)
      REAL
     :  SIZE1,
     :  SIZE2,
     :  SIZE3,
     :  SIZE4
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER MPTS ! Maximum no. of points to define a symbol.
      PARAMETER (MPTS = 50)
*  Local Variables:
      INTEGER
     :  PGCLIX,    ! PGPLOT colour index.
     :  PTS,       ! Number of points plotted in each symbol.
     :  LABLEN     ! Length of LABEL (excl. trail. blanks).
      REAL
     :  XPOS,      ! X position.
     :  YPOS,      ! Y    "    .
     :  XDOT(1),   ! X position for drawing a dot.
     :  YDOT(1),   ! Y    "      "     "    "  " .
     :  X(MPTS),   ! Array to hold X corners of the symbol.
     :  Y(MPTS),   !   "   "   "   Y    "    "   "    "   .
     :  XCRNR1,    ! First  X coordinate of a rectangle.
     :  YCRNR1,    !   "    Y     "      "  "     "    .
     :  XCRNR2,    ! Second X     "      "  "     "    .
     :  YCRNR2     !   "    Y     "      "  "     "    .
      REAL
     :  XLINE(2),  ! X coordinates of a single straight line.
     :  YLINE(2),  ! Y      "      "  "   "       "      "  .
     :  XTEXT,     ! X text position.
     :  YTEXT      ! Y  "      "    .
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Convert the position to single precision.

         XPOS = REAL(XPOSD)
         YPOS = REAL(YPOSD)

*
*       Convert the colour to the PGPLOT colour index and then set the
*       PGPLOT colour index.

         IF (COLOUR .EQ. CIO__CRED) THEN
            PGCLIX = 2
         ELSE IF (COLOUR .EQ. CIO__CGRN) THEN
            PGCLIX = 3
         ELSE IF (COLOUR .EQ. CIO__CBLUE) THEN
            PGCLIX = 4
         ELSE IF (COLOUR .EQ. CIO__CCYAN) THEN
            PGCLIX = 5
         ELSE IF (COLOUR .EQ. CIO__CMAGN) THEN
            PGCLIX = 6
         ELSE IF (COLOUR .EQ. CIO__CYELL) THEN
            PGCLIX = 7
         ELSE
            PGCLIX = 1
         END IF

         CALL PGSCI (PGCLIX)

*
*       Check for each of the various plotting symbols.

         IF (SYMBOL .EQ. CIO__SDOT) THEN

*
*          First a dot, which has no size parameters.  Note that the
*          smallest dot available on the device is drawn.

            XDOT(1) = XPOS
            YDOT(1) = XPOS
            CALL PGPOINT (1, XDOT, YDOT, -1)

         ELSE IF (SYMBOL .EQ. CIO__SOPCR) THEN

*
*          Open circle.

            CALL CAP_CIRCL (MPTS, X, Y, STATUS)
            CALL CAP_STSYM (SIZE1, SUNITS, XPOS, YPOS, MPTS, X, Y,
     :        STATUS)
            CALL PGLINE (MPTS, X, Y)

         ELSE IF (SYMBOL .EQ. CIO__SFLCR) THEN

*
*          Filled circle.

            CALL CAP_CIRCL (MPTS, X, Y, STATUS)
            PTS = MPTS - 1
            CALL CAP_STSYM (SIZE1, SUNITS, XPOS, YPOS, PTS, X, Y,
     :        STATUS)
            CALL PGPOLY (PTS, X, Y)

         ELSE IF (SYMBOL .EQ. CIO__SOPSQ) THEN

*
*          Open square.

            X(1) = -1.0E0
            Y(1) =  1.0E0

            X(2) =  1.0E0
            Y(2) =  1.0E0

            X(3) =  1.0E0
            Y(3) = -1.0E0

            X(4) = -1.0E0
            Y(4) = -1.0E0

            X(5) = -1.0E0
            Y(5) =  1.0E0

            CALL CAP_STSYM (SIZE1, SUNITS, XPOS, YPOS, 5, X, Y,
     :        STATUS)
            CALL PGLINE (5, X, Y)

         ELSE IF (SYMBOL .EQ. CIO__SFLSQ) THEN

*
*          Filled square.

            X(1) = -1.0E0
            Y(1) = -1.0E0

            X(2) =  1.0E0
            Y(2) =  1.0E0

            CALL CAP_STSYM (SIZE1, SUNITS, XPOS, YPOS, 2, X, Y,
     :        STATUS)

            XCRNR1 = X(1)
            YCRNR1 = Y(1)

            XCRNR2 = X(2)
            YCRNR2 = Y(2)

            CALL PGRECT (XCRNR1, XCRNR2, YCRNR1, YCRNR2)

         ELSE IF (SYMBOL .EQ. CIO__SOPTR) THEN

*
*          Open triangle.

            X(1) =  0.0E0
            Y(1) =  1.0E0

            X(2) =  9.0E-1
            Y(2) = -5.0E-1

            X(3) = -9.0E-1
            Y(3) = -5.0E-1

            X(4) =  0.0E0
            Y(4) =  1.0E0

            CALL CAP_STSYM (SIZE1, SUNITS, XPOS, YPOS, 4, X, Y,
     :        STATUS)
            CALL PGLINE (4, X, Y)

         ELSE IF (SYMBOL .EQ. CIO__SFLTR) THEN

*
*          Filled triangle.

            X(1) =  0.0E0
            Y(1) =  1.0E0

            X(2) =  9.0E-1
            Y(2) = -5.0E-1

            X(3) = -9.0E-1
            Y(3) = -5.0E-1

            CALL CAP_STSYM (SIZE1, SUNITS, XPOS, YPOS, 3, X, Y,
     :        STATUS)
            CALL PGPOLY (3, X, Y)

         ELSE IF (SYMBOL .EQ. CIO__SOPSR) THEN

*
*          Open star.

            X(1) =  0.0E0
            Y(1) =  1.0E0

            X(2) =  2.5E-1
            Y(2) =  3.5E-1

            X(3) =  9.0E-1
            Y(3) =  3.5E-1

            X(4) =  4.0E-1
            Y(4) = -1.0E-1

            X(5) =  6.0E-1
            Y(5) = -8.0E-1

            X(6) =  0.0E0
            Y(6) = -4.0E-1

            X(7) = -6.0E-1
            Y(7) = -8.0E-1

            X(8) = -4.0E-1
            Y(8) = -1.0E-1

            X(9) = -9.0E-1
            Y(9) =  3.5E-1

            X(10) = -2.5E-1
            Y(10) =  3.5E-1

            X(11) =  0.0E0
            Y(11) =  1.0E0

            CALL CAP_STSYM (SIZE1, SUNITS, XPOS, YPOS, 11, X, Y,
     :        STATUS)
            CALL PGLINE (11, X, Y)

         ELSE IF (SYMBOL .EQ. CIO__SFLSR) THEN


*
*          Filled star.

            X(1) =  0.0E0
            Y(1) =  1.0E0

            X(2) =  2.5E-1
            Y(2) =  3.5E-1

            X(3) =  9.0E-1
            Y(3) =  3.5E-1

            X(4) =  4.0E-1
            Y(4) = -1.0E-1

            X(5) =  6.0E-1
            Y(5) = -8.0E-1

            X(6) =  0.0E0
            Y(6) = -4.0E-1

            X(7) = -6.0E-1
            Y(7) = -8.0E-1

            X(8) = -4.0E-1
            Y(8) = -1.0E-1

            X(9) = -9.0E-1
            Y(9) =  3.5E-1

            X(10) = -2.5E-1
            Y(10) =  3.5E-1

            CALL CAP_STSYM (SIZE1, SUNITS, XPOS, YPOS, 10, X, Y,
     :        STATUS)
            CALL PGPOLY (10, X, Y)

         ELSE IF (SYMBOL .EQ. CIO__SPLUS) THEN

*
*          Plus sign.

            X(1) =  0.0E0
            Y(1) =  1.0E0

            X(2) =  0.0E0
            Y(2) = -1.0E0

            X(3) = -1.0E0
            Y(3) =  0.0E0

            X(4) =  1.0E0
            Y(4) =  0.0E0

            CALL CAP_STSYM (SIZE1, SUNITS, XPOS, YPOS, 4, X, Y,
     :        STATUS)

            XLINE(1) = X(1)
            YLINE(1) = Y(1)

            XLINE(2) = X(2)
            YLINE(2) = Y(2)

            CALL PGLINE (2, XLINE, YLINE)

            XLINE(1) = X(3)
            YLINE(1) = Y(3)

            XLINE(2) = X(4)
            YLINE(2) = Y(4)

            CALL PGLINE (2, XLINE, YLINE)

         ELSE IF (SYMBOL .EQ. CIO__SMULT) THEN

*
*          Multiplication sign.

            X(1) = -7.0E-1
            Y(1) =  7.0E-1

            X(2) =  7.0E-1
            Y(2) = -7.0E-1

            X(3) =  7.0E-1
            Y(3) =  7.0E-1

            X(4) = -7.0E-1
            Y(4) = -7.0E-1

            CALL CAP_STSYM (SIZE1, SUNITS, XPOS, YPOS, 4, X, Y,
     :        STATUS)

            XLINE(1) = X(1)
            YLINE(1) = Y(1)

            XLINE(2) = X(2)
            YLINE(2) = Y(2)

            CALL PGLINE (2, XLINE, YLINE)

            XLINE(1) = X(3)
            YLINE(1) = Y(3)

            XLINE(2) = X(4)
            YLINE(2) = Y(4)

            CALL PGLINE (2, XLINE, YLINE)

         ELSE IF (SYMBOL .EQ. CIO__SAST) THEN

*
*          Asterisk.

            X(1) =  0.0E0
            Y(1) =  1.0E0

            X(2) =  0.0E0
            Y(2) = -1.0E0

            X(3) = -1.0E0
            Y(3) =  0.0E0

            X(4) =  1.0E0
            Y(4) =  0.0E0

            X(5) = -7.0E-1
            Y(5) =  7.0E-1

            X(6) =  7.0E-1
            Y(6) = -7.0E-1

            X(7) =  7.0E-1
            Y(7) =  7.0E-1

            X(8) = -7.0E-1
            Y(8) = -7.0E-1

            CALL CAP_STSYM (SIZE1, SUNITS, XPOS, YPOS, 8, X, Y,
     :        STATUS)

            XLINE(1) = X(1)
            YLINE(1) = Y(1)

            XLINE(2) = X(2)
            YLINE(2) = Y(2)

            CALL PGLINE (2, XLINE, YLINE)

            XLINE(1) = X(3)
            YLINE(1) = Y(3)

            XLINE(2) = X(4)
            YLINE(2) = Y(4)

            CALL PGLINE (2, XLINE, YLINE)

            XLINE(1) = X(5)
            YLINE(1) = Y(5)

            XLINE(2) = X(6)
            YLINE(2) = Y(6)

            CALL PGLINE (2, XLINE, YLINE)

            XLINE(1) = X(7)
            YLINE(1) = Y(7)

            XLINE(2) = X(8)
            YLINE(2) = Y(8)

            CALL PGLINE (2, XLINE, YLINE)

         ELSE IF (SYMBOL .EQ. CIO__SOELP) THEN

*
*          Open ellipse.

            CALL CAP_ELLIPS (SIZE1, SIZE2, SIZE3, MPTS, X, Y, STATUS)
            CALL CAP_STSYM (SIZE1, SUNITS, XPOS, YPOS, MPTS, X, Y,
     :        STATUS)
            CALL PGLINE (MPTS, X, Y)

         ELSE IF (SYMBOL .EQ. CIO__SFELP) THEN

*
*          Filled ellipse.

            CALL CAP_ELLIPS (SIZE1, SIZE2, SIZE3, MPTS, X, Y, STATUS)
            PTS = MPTS - 1
            CALL CAP_STSYM (SIZE1, SUNITS, XPOS, YPOS, PTS, X, Y,
     :        STATUS)
            CALL PGPOLY (PTS, X, Y)

         END IF

*
*       Plot a label if one is required.

         IF (LABEL .NE. ' ') THEN

*
*          Set the text height.

C           XXXXX

*
*          Compute the label aposition.

            XTEXT = -1.1E0
            YTEXT = 1.1E0

            CALL CAP_STSYM (SIZE1, SUNITS, XPOS, YPOS, 1, XTEXT,
     :        YTEXT, STATUS)

*
*          Plot the label.

            CALL CHR_LDBLK (LABEL)
            LABLEN = CHR_LEN(LABEL)
            CALL PGPTEXT (XTEXT, YTEXT, 0.0E0, 0.0E0,
     :        LABEL(1 : LABLEN) )

*
*          Reset the text height.

C           XXXX

         END IF

      END IF

      END
