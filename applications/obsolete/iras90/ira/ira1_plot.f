      SUBROUTINE IRA1_PLOT( INK, XLO, YLO, XHI, YHI, XA, YA, XB, YB,
     :                      MAXBRK, OUT, BREAK, VBREAK, NBREAK, LNF,
     :                      STATUS )
*+
*  Name:
*     IRA1_PLOT

*  Purpose:
*     Plot a line between two points with windowing.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_PLOT( INK, XLO, YLO, XHI, YHI, XA, YA, XB, YB, MAXBRK,
*                     OUT, BREAK, VBREAK, NBREAK, LNF, STATUS )

*  Description:
*     This routine plots a straight line from (XA,YA) to (XB,YB),
*     blabking the line at the edges of the zone defined by XLO, XHI,
*     YLO and YHI. If the line is not continuous with the previous line
*     plotted by this routine, then a break is reported by returning the
*     position and unit vector of the break. A flag is also maintained
*     indicating if any lines have yet been drawn which intersect the
*     plotting zone. A factor giving the fraction of the line length
*     which lay within the plotting zone is returned.

*  Arguments:
*     INK = LOGICAL (Given)
*        If false then no line is drawn even if the line intersects the
*        plotting space, but break information (etc) is still returned.
*     XLO = REAL (Given)
*        Lower X limit of the plotting space.
*     XHI = REAL (Given)
*        Upper X limit of the plotting space.
*     YLO = REAL (Given)
*        Lower Y limit of the plotting space.
*     YHI = REAL (Given)
*        Upper Y limit of the plotting space.
*     XA = REAL (Given)
*        X coordinate of start point.
*     YA = REAL (Given)
*        Y coordinate of start point.
*     XB = REAL (Given)
*        X coordinate of end point.
*     YB = REAL (Given)
*        Y coordinate of end point.
*     MAXBRK = INTEGER (Given)
*        Size of arrays BREAK and VBREAK.
*     OUT = LOGICAL (Given and Returned)
*        Returned false if the line intersects the plottiong space.
*        Unchanged otherwise.
*     BREAK( 2, MAXBRK ) = REAL (Returned)
*        Contains the world coordinates at which each break in the
*        plotted curve occurred. A break is recorded if the starting
*        point of the current line is not the same as the end point of
*        the previous line.
*     VBREAK( 2, MAXBRK ) = REAL (Returned)
*        Contains the unit vector (within the world coordinate system)
*        parallel to the tangent to the curve at each break. The sense
*        is such that the vector always points back along the plotted
*        section of the curve.
*     NBREAK = INTEGER (Returned)
*        The number of breaks for which information is returned in BREAK
*        and VBREAK.
*     LNF = REAL (Returned)
*        The fraction of the line length which fell within the plotting
*        space.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAR-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.

*  Arguments Given:
      LOGICAL INK
      REAL XLO
      REAL YLO
      REAL XHI
      REAL YHI
      REAL XA
      REAL YA
      REAL XB
      REAL YB
      INTEGER MAXBRK

*  Arguments Given and Returned:
      LOGICAL OUT
      REAL BREAK( 2, MAXBRK )
      REAL VBREAK( 2, MAXBRK )
      INTEGER NBREAK

*  Arguments Returned:
      REAL LNF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL AAMAX                 ! Displacement from supplied point B to
                                 ! the plotable point A.
      REAL AAMIN                 ! Displacement from supplied point B to
                                 ! the plotable point B.
      REAL A1                    ! Displacement from point B to the
                                 ! point of intersection of the line
                                 ! with lower X boundary.
      REAL A2                    ! Displacement from point B to the
                                 ! point of intersection of the line
                                 ! with upper X boundary.
      REAL A3                    ! Displacement from point B to the
                                 ! point of intersection of the line
                                 ! with lower Y boundary.
      REAL A4                    ! Displacement from point B to the
                                 ! point of intersection of the line
                                 ! with upper Y boundary.
      REAL DX                    ! Difference in X between supplied
                                 ! points.
      REAL DY                    ! Difference in Y between supplied
                                 ! points.
      LOGICAL PLOT               ! True if a line can be plotted.
      REAL T                     ! Temporary storage.
      REAL XAM                   ! Modified XA position.
      REAL XBM                   ! Modified XB position.
      REAL YAM                   ! Modified YA position.
      REAL YBM                   ! Modified YB position.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the shifts in X and Y.
      DX = XB - XA
      DY = YB - YA

* If either end is outside the zone, replace the given coordinates with
* the end coordinates of the section of the line which lies within the
* zone.
      IF( XA .LT. XLO .OR. XA .GT. XHI .OR.
     :    XB .LT. XLO .OR. XB .GT. XHI .OR.
     :    YA .LT. YLO .OR. YA .GT. YHI .OR.
     :    YB .LT. YLO .OR. YB .GT. YHI ) THEN


*  Find the displacement from point B towards point A at which the
*  line cuts the two X bounds of the zone (displacement at point B is
*  0.0, and at point A is 1.0)
         IF( DX .NE. 0.0 ) THEN

            A1 = ( XB - XLO )/DX
            A2 = ( XB - XHI )/DX

*  Ensure that A1 represents the highest plottable offset, and A2 the
*  lowest.
            IF( A1 .LT. A2 ) THEN
               T = A1
               A1 = A2
               A2 = T
            END IF

*  If the line joining A and B is vertical...
         ELSE

*  If the line is within the plottable X range, indicate that all
*  offsets are plottable (as far as the X range is concerned at least).
            IF( XA .GE. XLO .AND. XA .LE. XHI ) THEN
               A1 = VAL__MAXR
               A2 = VAL__MINR

*  If the line is ouside the plottable X range, indicate that no
*  offsets are plottable.
            ELSE
               A1 = 0.0
               A2 = 0.0
            END IF

         END IF

*  Find the fractional displacement from point A to point B at which the
*  line cuts the two Y bounds of the zone.
         IF( DY .NE. 0.0 ) THEN

            A3 = ( YB - YLO )/DY
            A4 = ( YB - YHI )/DY

*  Ensure that A3 represents the highest plottable offset, and A4 the
*  lowest.
            IF( A3 .LT. A4 ) THEN
               T = A3
               A3 = A4
               A4 = T
            END IF

*  If the line joining A and B is horizontal...
         ELSE

*  If the line is within the plottable Y range, indicate that all
*  offsets are plottable (as far as the Y range is concerned at least).
            IF( YA .GE. YLO .AND. YA .LE. YHI ) THEN
               A3 = VAL__MAXR
               A4 = VAL__MINR

*  If the line is ouside the plottable Y range, indicate that no
*  offsets are plottable.
            ELSE
               A3 = 0.0
               A4 = 0.0
            END IF

         END IF

*  Find the fractional displacements from point A to point B at the ends
*  of the plotable line.
         AAMIN = MIN( 1.0, MAX( 0.0, A2, A4 ) )
         AAMAX = MAX( 0.0, MIN( 1.0, A1, A3 ) )

*  Store the end coordinates of the line joining the plotable points.
         IF( AAMAX .GT. AAMIN ) THEN
            XAM = XB - AAMAX*DX
            YAM = YB - AAMAX*DY
            XBM = XB - AAMIN*DX
            YBM = YB - AAMIN*DY
            PLOT = .TRUE.
            LNF = AAMAX - AAMIN

         ELSE
            PLOT = .FALSE.
            LNF = 0.0

         END IF

*  If both ends of the line are within the plotting zone, use the
*  supplied end points.
      ELSE
         XAM = XA
         YAM = YA
         XBM = XB
         YBM = YB
         PLOT = .TRUE.
         LNF = 1.0
      END IF

*  If a line is to be plotted...
      IF( PLOT ) THEN

*  Do nothing if the line has zero length.
         IF( ABS( XAM - XBM ) .GT. VAL__SMLR .OR.
     :       ABS( YAM - YBM ) .GT. VAL__SMLR ) THEN

*  Plot the line.
            IF( INK ) CALL SGS_LINE( XAM,  YAM, XBM, YBM )

*  If this is the first line to be plotted in the current curve, save
*  the start of the line as a break.
            IF( OUT ) THEN
               NBREAK = 1
               BREAK( 1, 1 ) = XAM
               BREAK( 2, 1 ) = YAM
               VBREAK( 1, 1 ) = DX
               VBREAK( 2, 1 ) = DY
               OUT = .FALSE.

*  The end of the current line will be saved as a potential break.
*  Reserve a slot in the break arrays for it.
               NBREAK = 2

*  If this is not the first line to be plotted...
            ELSE

*  ... and if the start of this line is not coincident with the end
*  of the previous line, save the starting position of this line as a
*  break in the curve. The position and vector at the end of the
*  previous line were stored in the arrays as a potential break when the
*  line was plotted.
               IF( ABS( XAM - BREAK( 1, NBREAK ) ) .GT. VAL__SMLR .OR.
     :             ABS( YAM - BREAK( 2, NBREAK ) ) .GT. VAL__SMLR ) THEN

*  Increment NBREAK so that the information about the end of the
*  previous line is kept as a valid break.
                  NBREAK = NBREAK + 1
                  IF( NBREAK .LE. MAXBRK ) THEN

                     BREAK( 1, NBREAK ) = XAM
                     BREAK( 2, NBREAK ) = YAM
                     VBREAK( 1, NBREAK ) = DX
                     VBREAK( 2, NBREAK ) = DY

                  END IF

*  The end of the current line will be saved as a potential break.
*  Reserve a slot in the break arrays for it.
                  NBREAK = NBREAK + 1

               END IF

            END IF

*  Save the position and vector at the end of the current line as a
*  potential break. If a slot has not been reserved for it in the break
*  arrays then the information describing the end of the previous line
*  will be overwritten.
            IF( NBREAK .LE. MAXBRK ) THEN
               BREAK( 1, NBREAK ) = XBM
               BREAK( 2, NBREAK ) = YBM
               VBREAK( 1, NBREAK ) = -DX
               VBREAK( 2, NBREAK ) = -DY
            END IF

*  If the arrays are full, report an error.
            IF( NBREAK .GT. MAXBRK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'MX', MAXBRK )
               CALL ERR_REP( 'IRA1_PLOT_ERR1',
     :                'IRA1_PLOT: No. of breaks in curve exceeds ^MX.',
     :                       STATUS )
            END IF

         END IF

      END IF

      END
