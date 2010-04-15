      SUBROUTINE SCULIB_CALC_GRID (N, X, Y, XMIN, XMAX, XSPACE, NX,
     :  YMIN, YMAX, YSPACE, NY, IPOS, JPOS, STATUS)
*+
*  Name:
*     SCULIB_CALC_GRID

*  Purpose:
*     calculate the minimum rectangular grid that would
*     contain the input jiggle pattern

*  Description:
*     This routine takes the x,y coords of the mapped points and attempts to
*     fit them onto a rectangular grid. The grid contains the minimum number
*     of points required to hold the measured positions.

*  Invocation:
*     CALL SCULIB_CALC_GRID (N, X, Y, XMIN, XMAX, XSPACE, NX,
*    :  YMIN, YMAX, YSPACE, NY, IPOS, JPOS, STATUS)

*  Arguments:
*     N                                     = INTEGER (Given)
*           the number of positions in the jiggle pattern
*     X (N)                                 = REAL (Given)
*           the x offsets of the jiggle
*     Y (N)                                 = REAL (Given)
*           the y offsets
*     XMIN                                  = REAL (Returned)
*           the minimum of the output map's x-axis
*     XMAX                                  = REAL (Returned)
*           the maximum of the x-axis
*     XSPACE                                = REAL (Returned)
*           the pixel spacing in x
*     NX                                    = INTEGER (Returned)
*           the map dimension in x
*     YMIN                                  = REAL (Returned)
*           the minimum of the output map's y-axis
*     YMAX                                  = REAL (Returned)
*           the maximum of the y-axis
*     YSPACE                                = REAL (Returned)
*           the pixel spacing in y
*     NY                                    = INTEGER (Returned)
*           the map dimension in y
*     IPOS (N)                              = INTEGER (Returned)
*           the I index of each jiggle position in the map
*     JPOS (N)                              = INTEGER (Returned)
*           the J index of each jiggle position in the map
*     STATUS                                = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     9-JUN-1993: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER N
      REAL X (N), Y (N)

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL XMIN, XMAX, XSPACE
      INTEGER NX
      REAL YMIN, YMAX, YSPACE
      INTEGER NY
      INTEGER IPOS (N), JPOS (N)

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I
      INTEGER IR
      LOGICAL OK
      REAL    R

*  Internal References:

*  Local data:

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF (N .LE. 0) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_CALC_GRID: jiggle pattern is empty',
     :     STATUS)

      ELSE IF (N .EQ. 1) THEN

         XMAX = X (1)
         XMIN = X (1)
         XSPACE = 0.0
         NX = 1
         IPOS (1) = 1
         YMAX = Y (1)
         YMIN = Y (1)
         YSPACE = 0.0
         NY = 1
         JPOS (1) = 1

      ELSE

*  find maximum and minimum values

         XMAX = X (1)
         XMIN = X (1)
         YMAX = Y (1)
         YMIN = Y (1)

         DO I = 2, N
            XMAX = MAX (XMAX, X(I))
            XMIN = MIN (XMIN, X(I))
            YMAX = MAX (YMAX, Y(I))
            YMIN = MIN (YMIN, Y(I))
         END DO

*  find smallest non-zero spaces between pixels. Use only 2 reference pixels
*  to stop the process taking too long. This means that the routine could fail
*  on a valid dataset but?

         XSPACE = XMAX - XMIN
         YSPACE = YMAX - YMIN

         DO I = 2, N
            IF (ABS(X(I)-X(1)) .GT. 0.001) THEN
               XSPACE = MIN (XSPACE, ABS(X(I)-X(1)))
            END IF
            IF (ABS(Y(I)-Y(1)) .GT. 0.001) THEN
               YSPACE = MIN (YSPACE, ABS(Y(I)-Y(1)))
            END IF
         END DO

         DO I = 1, N - 1
            IF (ABS(X(I)-X(N)) .GT. 0.001) THEN
               XSPACE = MIN (XSPACE, ABS(X(I)-X(N)))
            END IF
            IF (ABS(Y(I)-Y(N)) .GT. 0.001) THEN
               YSPACE = MIN (YSPACE, ABS(Y(I)-Y(N)))
            END IF
         END DO

*  check that all pixels fit onto a regular grid with the limits and pixel
*  spacings derived

         OK = .TRUE.

         DO I = 1, N
            IF (XMAX .NE. XMIN) THEN
               R = (X(I) - XMIN) / XSPACE
               IR = NINT (R)
               IF (ABS(REAL(IR)-R) .GT. 0.001) THEN
                  OK = .FALSE.
               END IF
            END IF
            IF (YMAX .NE. YMIN) THEN
               R = (Y(I) - YMIN) / YSPACE
               IR = NINT (R)
               IF (ABS(REAL(IR)-R) .GT. 0.001) THEN
                  OK = .FALSE.
               END IF
            END IF
         END DO

*  warn if jiggle pattern does not fit derived mesh

         IF (.NOT. OK) THEN

            STATUS = SAI__WARN
            CALL ERR_REP (' ', 'SCULIB_CALC_GRID: jiggle does not '//
     :        'fit regular sample grid', STATUS)
            XMIN = 0.0
            YMIN = 0.0
            XSPACE = 0.0
            YSPACE = 0.0

*     Setup a 1D grid if irregular - one point at each position
*     This is not the observed mesh but it allows for the maps to be made
            NX = N
            NY = 1
            DO I = 1, N
               IPOS (I) = I
               JPOS (I) = 1
            END DO

         ELSE

*  calculate other mesh parameters and pointers connecting jiggle index to
*  mesh coordinates
*     It is possible to get to this point by using a zero offset jiggle
*     ub X or Y. We need to trap for that to prevent division by zero

            IF (XMAX .EQ. XMIN) THEN
               NX = 1
            ELSE
               NX = NINT((XMAX - XMIN)/XSPACE) + 1
            END IF
            IF (YMAX .EQ. YMIN) THEN
               NY = 1
            ELSE
               NY = NINT((YMAX - YMIN)/YSPACE) + 1
            END IF

            IF (YSPACE .GT. 1.0E-10) THEN
               DO I = 1, N
                  IPOS (I) = NINT((X(I) - XMIN)/XSPACE) + 1
               END DO
            ELSE
               DO I = 1, N
                  IPOS (I) = 0
               END DO
            END IF

            IF (YSPACE .GT. 1.0E-10) THEN
               DO I = 1, N
                  JPOS (I) = NINT((Y(I) - YMIN)/YSPACE) + 1
               END DO
            ELSE
               DO I = 1, N
                  JPOS (I) = 0
               END DO
            END IF

         END IF
      END IF


      END
