      SUBROUTINE JCMT_CALCULATE_GRID (N, X, Y, XMIN, XMAX, XSPACE,
     :  NX, YMIN, YMAX, YSPACE, NY, STATUS)
*+
*  Name:
*     JCMT_CALCULATE_GRID

*  Purpose:
*     Calculate extent and pixel size of the rectangular mesh holding the
*     observed map

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     SUBROUTINE JCMT_CALCULATE_GRID (N, X, Y, XMIN, XMAX, XSPACE,
*    :  NX, YMIN, YMAX, YSPACE, NY, STATUS)

*  Description:
*   This routine takes the x,y coords of the mapped points and attempts to
*   fit them onto a rectangular grid. The grid contains the minimum number
*   of points required to hold the measured positions.

*  Arguments:
*     N       = INTEGER (Given)
*        The number of points in the map
*     X (N)   = REAL (Given)
*        the x coordinates in cell units of the measured points
*     Y (N)   = REAL (Given)
*        the y coordinates in cell units of the measured points
*     XMIN    = REAL (Returned)
*        the lowest value of x
*     XMAX    = REAL (Returned)
*        the highest value of x
*     XSPACE  = REAL (Returned)
*        the x spacing of the map grid
*     NX      = REAL (Returned)
*        the x dimension of the map grid
*     YMIN    = REAL (Returned)
*        the lowest value of y
*     YMAX    = REAL (Returned)
*        the highest value of y
*     YSPACE  = REAL (Returned)
*        the y spacing of the map grid
*     NY      = REAL (Returned)
*        the y dimension of the map grid

*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL: J.Lightfoot
*     {enter_new_authors_here}

*  History:
*     5-FEB-1991 (REVAD::JFL)
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER N
      REAL X (N)
      REAL Y (N)

*  Arguments Returned:
      REAL XMIN, XMAX, XSPACE
      REAL YMIN, YMAX, YSPACE
      INTEGER NX, NY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:

*  Local Variables:
      LOGICAL OK
      INTEGER I, J, II
      INTEGER IPOS
      INTEGER IGNORE
      REAL INDEX_SPACE
      REAL RPOS
*.

*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF (N .EQ. 1) THEN
         XMAX = X (1)
         XMIN = X (1)
         XSPACE = 0.0
         NX = 1
         YMAX = Y (1)
         YMIN = Y (1)
         YSPACE = 0.0
         NY = 1
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

*  find smallest non-zero spaces between pixels. The nested DO-loop section
*  would be very time consuming for large datasets unless only a few pixels
*  were picked out for checking. This means that the routine could fail on
*  a valid dataset but?

         XSPACE = XMAX - XMIN
         YSPACE = YMAX - YMIN

         IF (N .LE. 30) THEN

            DO I = 1, N-1
               DO J = I+1, N
                  IF (ABS(X(I)-X(J)) .GT. 0.001) THEN
                     XSPACE = MIN (XSPACE, ABS(X(I)-X(J)))
                  END IF
                  IF (ABS(Y(I)-Y(J)) .GT. 0.001) THEN
                     YSPACE = MIN (YSPACE, ABS(Y(I)-Y(J)))
                  END IF
               END DO
            END DO

         ELSE

            INDEX_SPACE = N / 20.0
            DO II = 1, 20
               I = (II-1) * INDEX_SPACE + 1
               I = MAX (I, 1)
               I = MIN (I, N)
               DO J = 1, N
                  IF (ABS(X(I)-X(J)) .GT. 0.001) THEN
                     XSPACE = MIN (XSPACE, ABS(X(I)-X(J)))
                  END IF
                  IF (ABS(Y(I)-Y(J)) .GT. 0.001) THEN
                     YSPACE = MIN (YSPACE, ABS(Y(I)-Y(J)))
                  END IF
               END DO
            END DO

         END IF


*  check that all pixels fit onto a regular grid with the limits and pixel
*  spacings derived

         OK = .TRUE.

         DO I = 1, N
            IF (XMAX .NE. XMIN) THEN
               RPOS = (X(I) - XMIN) / XSPACE
               IPOS = NINT (RPOS)
               IF (ABS(REAL(IPOS)-RPOS) .GT. 0.01) THEN
                  OK = .FALSE.
               END IF
            END IF
            IF (YMAX .NE. YMIN) THEN
               RPOS = (Y(I) - YMIN) / YSPACE
               IPOS = NINT (RPOS)
               IF (ABS(REAL(IPOS)-RPOS) .GT. 0.01) THEN
                  OK = .FALSE.
               END IF
            END IF
         END DO

         IF (.NOT. OK) THEN
            STATUS = SAI__ERROR
            IGNORE = 0
            CALL PAR_WRUSER ('MAKEMAP - Data does not fit regular '//
     :        'sample grid, fatal error', IGNORE)
            XMIN = 0.0
            YMIN = 0.0
            XSPACE = 0.0
            YSPACE = 0.0
            NX = 0
            NY = 0
         ELSE
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
         END IF

      END IF


      END


