      SUBROUTINE CAP_STSYM (SIZE, SUNITS, XPOS, YPOS, PTS, X, Y,
     :  STATUS)
*+
*  Name:
*     CAP_STSYM
*  Purpose:
*     Scale and translate a plotting symbol.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_STSYM (SIZE, SUNITS, XPOS, YPOS, PTS; X, Y; STATUS)
*  Description:
*     Scale and translate a plotting symbol.
*  Arguments:
*     SIZE  =  REAL (Given)
*        Size of the symbol.
*     SUNITS  =  INTEGER (Given)
*        Code representing the units of the size attribute.
*     XPOS  =  REAL (Given)
*        X coordinate of the point to be plotted.
*     YPOS  =  REAL (Given)
*        Y coordinate of the point to be plotted.
*     PTS  =  INTEGER (Given)
*        Number of points in the symbol.
*     X(PTS)  =  REAL (Given and Returned)
*        X coordinates of the symbol corners.
*     Y(PTS)  =  REAL (Given and Returned)
*        Y coordinates of the symbol corners.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Scale the symbol size to radians.
*     For each point
*       Scale and translate the point.
*     end for
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     5/6/97 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'CAP_PAR'           ! CURSA parametric constants.
      INCLUDE 'CIO_PAR'           ! CIO parametric constants.
*  Global Variables:
      INCLUDE 'CHART_CMN'         ! CATCHART common block.
*  Arguments Given:
      REAL
     :  SIZE,
     :  XPOS,
     :  YPOS
      INTEGER
     :  SUNITS,
     :  PTS
*  Arguments Given and Returned:
      REAL
     :  X(PTS),
     :  Y(PTS)
*  Status:
      INTEGER STATUS    ! Global status.
*  Local Variables:
      INTEGER
     :  LOOP            ! Loop index.
      REAL
     :  SIZER           ! Scaled size in radians.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Scale the symbol size to radians.  For each of the permitted
*       units for a size attribute convert the value to radians.  Note
*       that if the given code is not recognised then a size in radians
*       corresponding to a fractional size of 0.05 is returned.

         IF (SUNITS .EQ. CIO__UFRAC) THEN
            SIZER = (CWYMX__CIO - CWYMN__CIO) * SIZE

         ELSE IF (SUNITS .EQ. CIO__UASEC) THEN
            SIZER = SIZE * CAP__SPI / (1.8E2 * 6.0E1 * 6.0E1)

         ELSE IF (SUNITS .EQ. CIO__UAMIN) THEN
            SIZER = SIZE * CAP__SPI / (1.8E2 * 6.0E1)

         ELSE IF (SUNITS .EQ. CIO__UDEGR) THEN
            SIZER = SIZE * CAP__SPI / 1.8E2

         ELSE IF (SUNITS .EQ. CIO__UHOUR) THEN
            SIZER = SIZE * CAP__SPI / 1.2E1

         ELSE IF (SUNITS .EQ. CIO__URADN) THEN
            SIZER = SIZE

         ELSE
            SIZER = (CWYMX__CIO - CWYMN__CIO) * 5.0E-2

         END IF

*
*       Scale and translate every point.

         DO LOOP = 1, PTS
            X(LOOP) = XPOS +(X(LOOP) * SIZER)
            Y(LOOP) = YPOS +(Y(LOOP) * SIZER)
         END DO

      END IF

      END
