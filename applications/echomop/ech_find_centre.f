      SUBROUTINE ECH_FIND_CENTRE( MODE, N_POINTS, DATA, CENTRE,
     :           STATUS )
*+
*  Name:
*     ECHOMOP - ECH_FIND_CENTRE

*  Purpose:
*     Calculates central position of peaked dataset (1-D).

*  Description:
*     This routine attempts to calculate the central peak of a set of
*     data.  The method used varies according to the 'mode' passed.
*
*       C  -  Centroiding (default).
*       E  -  Edge location and interpolation.
*       G  -  Gaussian fitting.

*  Invocation:
*     CALL ECH_FIND_CENTRE( MODE, N_POINTS, DATA, CENTRE, STATUS )

*  Arguments:
*     DATA = REAL (Given)
*        Input 1-D data array.
*     N_POINTS = INTEGER (Given)
*        Number of points in data array
*     MODE = CHAR (Given)
*        Tracing mode selected.
*        May be: E - c.o.g. between edges,
*                G - Gaussian fit to profile,
*                C - centroid profile.
*     CENTRE = REAL (Given and Returned)
*        Passed in as initial estimate.
*        Returned as better estimate (hopefully!).
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Setup mode variables
*     If edge mode is selected then
*        If failed then set status
*        Endif
*     Else if centroid mode selected then
*        Calculate centroid of data
*        If failed then set status
*     Else if gaussian mode selected then
*     Else if balancing (centre-of-gravity) mode selected then
*        Sum intensity values in data array
*        Find entry where sum-so-far exceeds half total sum
*        Calulate simple minded cog
*     Endif

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'

*  Arguments Given:
      CHARACTER*( * ) MODE
      INTEGER N_POINTS
      REAL DATA( N_POINTS )

*  Arguments Given and Returned:
      REAL CENTRE

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAX_POINTS
      PARAMETER ( MAX_POINTS = 512 )

*  Local Variables:
      REAL FIT( MAX_POINTS )
      REAL FIT_PARS( 3 )
      REAL FIT_ERR( 3 )
      REAL INPUT_CENTRE
      REAL LEFT
      REAL RIGHT
      REAL WIDTH
      REAL PEAK
      REAL SUM
      REAL OFFS
      REAL HALF_SUM

      INTEGER I
      INTEGER INDEX

      LOGICAL GAUSSIAN
      LOGICAL CENTROID
      LOGICAL EDGE
      LOGICAL BALANCE

*  Functions called:
      REAL FIG_EDGES
      EXTERNAL FIG_EDGES
*.

*  Find selected mode.
      CENTROID = .FALSE.
      GAUSSIAN = .FALSE.
      EDGE = .FALSE.
      IF ( MODE( :1 ) .EQ. 'G' .OR. MODE( :1 ) .EQ. 'g' ) THEN
         GAUSSIAN = .TRUE.

      ELSE IF ( MODE( :1 ) .EQ. 'E' .OR. MODE( :1 ) .EQ. 'e' ) THEN
         EDGE = .TRUE.

      ELSE IF ( MODE( :1 ) .EQ. 'B' .OR. MODE( :1 ) .EQ. 'b' ) THEN
         BALANCE = .TRUE.

      ELSE
         CENTROID = .TRUE.
      ENDIF

*  Edge-mode estimate.
      IF ( EDGE ) THEN

*     Save starting estimate.
         INPUT_CENTRE = CENTRE

*     Find new centre.
         CENTRE = FIG_EDGES( DATA, N_POINTS, INPUT_CENTRE, LEFT,
     :                       RIGHT )
         IF ( CENTRE .EQ. 0.0 ) THEN
            STATUS = ECH__NO_CENTRE

         ELSE
            STATUS = 0
         ENDIF

*  Calculate centroid.
      ELSE IF ( centroid ) THEN
         WIDTH = FLOAT( N_POINTS ) / 10.0

*     Check for negative values - fold them to zero.
         INDEX = MIN( N_POINTS, MAX_POINTS )
         DO I = 1, INDEX
            IF ( DATA( I ) .LT. 0.0 ) THEN
               FIT( I ) = 0.0

            ELSE
               FIT( I ) = DATA( I )
            END IF
         END DO

*     Try to centroid - up to three times.
         DO I = 1, 3
            CALL GEN_CENTROID( FIT, INDEX, WIDTH, CENTRE, PEAK,
     :                         STATUS )
            IF ( STATUS .EQ. 1 ) THEN
               WIDTH = WIDTH / 2.0

            ELSE
               GO TO 100
            END IF
         END DO
  100    CONTINUE

*     If failed then set status.
         IF ( STATUS .NE. 0 )  THEN
            CENTRE = 0.0
            STATUS = ECH__NO_CENTRE
         END IF

*  Gaussian mode estimate.
      ELSE IF ( GAUSSIAN ) THEN
         STATUS = 0
         FIT_PARS( 1 ) = DATA( INT( N_POINTS + 1 ) / 2 )
         FIT_PARS( 2 ) = FLOAT( N_POINTS + 1 ) / 2.0
         FIT_PARS( 3 ) = FLOAT( N_POINTS ) / 2.0 / 2.35
         DO I = 1, N_POINTS
            IF ( DATA( I ) .GT. 0.0 ) THEN
               FIT( I ) = 1.0 / DATA( I )

            ELSE
               FIT( I ) = 1.0
            END IF
         END DO
         CALL ECH_FIT_GAUSSIAN( DATA, FIT, N_POINTS, FIT_PARS,
     :                          FIT_ERR, 3, STATUS )
         IF ( STATUS .NE. 0 )  THEN
            CENTRE = 0.0
            STATUS = ECH__NO_CENTRE

         ELSE
            CENTRE = FIT_PARS( 2 )
         ENDIF

*  Balancing (centre-of-gravity) mode estimate.
      ELSE IF ( BALANCE ) THEN
         OFFS = DATA( 1 )
         IF ( DATA( N_POINTS ) .LT. OFFS ) OFFS = DATA( N_POINTS )

*     Sum intensity values in data array.
         SUM = 0.0
         DO I = 1, N_POINTS
            SUM = SUM + MAX( 0.0, DATA( I ) - OFFS )
         END DO

*     Find entry where sum-so-far exceeds half total sum.
         INDEX = 0
         HALF_SUM = 0.0
         DO I = 1, N_POINTS
            IF ( INDEX .EQ. 0 ) THEN
               HALF_SUM = HALF_SUM + MAX( 0.0, DATA( I ) - OFFS )
               IF ( HALF_SUM .GT. SUM / 2.0 ) THEN
                  INDEX = I
                  GO TO 500
               END IF
            END IF
         END DO
  500    CONTINUE

*      Calulate simple C-o-G.
         IF ( INDEX .GT. 0 )  THEN
            CENTRE = FLOAT( INDEX ) - ( HALF_SUM / SUM - 0.5 ) /
     :               ( MAX( 0.0, DATA( INDEX ) - OFFS ) / SUM ) + 0.5
            STATUS = 0

         ELSE
            CENTRE = 0.0
            STATUS = ECH__NO_CENTRE
         END IF
      END IF

      END
