      SUBROUTINE SPD_WAAL( PMASK, PMODEL, PERROR, ZONID, PLABEL,
     :   RMIN, RMAX, NELM1, X1, Y1, NELM2, NUSE2, X2,
     :   NELM3, X3, Y3, NELM4, X4, Y4, YL, YU, PMIN, PMAX, STATUS )
*+
*  Name:
*     SPD_WAAL

*  Purpose:
*     Plot serving for spectral fits.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WAAL( PMASK, PMODEL, PERROR, ZONID, PLABEL,
*        RMIN, RMAX, NELM1, X1, Y1, NELM2, NUSE2, X2,
*        NELM3, X3, Y3, NELM4, X4, Y4, YL, YU, PMIN, PMAX, STATUS )

*  Description:
*     This routine controls the plotting of Specdre's fit routines. In
*     general two PGPLOT viewports on top of each other and with the
*     same abscissa are used. The bottom port is about three times the
*     size of the top port.
*
*     In total five curves may be drawn. Bad ordinate values will be
*     recognised.
*     -  The first set of plot positions results in a full-drawn
*        bin-style curve in the bottom viewport. This is intended for
*        the original data fed into the fit routine.
*     -  The second set is a series of abscissa values use to draw a
*        broken zero line in the top viewport. This is intended for
*        indicating the mask used by the fit routine, especially if the
*        mask consists of several intervals.
*     -  The third set of plot positions results in a dashed line-style
*        curve in the bottom viewport. This is intended for overlaying
*        the model with the data.
*     -  The fourth set of plot positions results in a series of cross
*        markers in the top viewport. This is intended for displaying
*        the residuals (data minus model). When several mask intervals
*        are used, this series of points should show gaps, which would
*        be impossible if they were drawn as a bin- or line-style curve.
*     -  The fifth set of pairs of ordinate values is drawn as error
*        bars at the same abscissa values as the fourth set. This is
*        intended to overlay the data errors with the residuals.
*
*     This routine needs four sets of range information.
*     -  The first set is the abscissa range to be displayed.
*     -  The second set is the ordinate range for the bottom viewport.
*        This routine will allow for an extra 5 % at the top and bottom
*        of the viewport.
*     -  The third and fourth set should be the extrema of the errors
*        (error-bar half-length) and the residuals. This routine works
*        out the ordinate range for the top viewport from these extrema
*        as follows:
*
*           y_{max} = 1.05 ( max{ |res_{min}|, |res_{max}| }
*                            + |err_{max}| )
*
*           y_{min} = -y_{max}

*  Arguments:
*     PMASK = LOGICAL (Given)
*        True if the second curve (mask-indicating zero line in the top
*        vieport) is to be plotted.
*     PMODEL = LOGICAL (Given)
*        True if the third curve (dashed model in bottom viewport) and
*        the fourth curve (cross-marked residuals in top viewport) are
*        to be plotted.
*     PERROR = LOGICAL (Given)
*        True if the fifth curve (error bars to residuals) is to be
*        plotted.
*     ZONID = INTEGER (Given)
*        The SGS zone identifier. This is needed to clear the correct
*        zone and to reopen PGPLOT in the correct zone. This is passed
*        on to SPAEC and thus should be the value returned by SPAEB.
*     PLABEL( 3 ) = CHARACTER * ( * ) (Given)
*        Plot labels for abscissa, ordinate and top.
*     RMIN( 4 ) = REAL (Given)
*        Minima determining abscissa and ordinate ranges:
*        -  1: abscissa range
*        -  2: ordinate range for bottom viewport, this routine will
*              allow for an extra 5 % on the range given
*        -  3: range of error-bar half-lengths
*        -  4: range of residuals
*     RMAX( 4 ) = REAL (Given)
*        Maxima determining abscissa and ordinate ranges, see RMIN.
*     NELM1 = INTEGER (Given)
*        The number of points in the first curve (data).
*     X1( NELM1 ) = REAL (Given)
*        The first curve's (data) abscissa values.
*     Y1( NELM1 ) = REAL (Given)
*        The first curve's (data) ordinate values.
*     NELM2 = INTEGER (Given)
*        The length of the X2 vector. Must be an even number.
*     NUSE2 = INTEGER (Given)
*        The number of pairs from X2 to be used. Must be less than or
*        equal to half the value of NELM2.
*     X2( NELM2 ) = REAL (Given)
*        The second curve's (mask) abscissa values. Straight lines are
*        drawn with ordinate value zero:
*        from     X2(1) to X2(NELM2/2+1),
*        from     X2(2) to X2(NELM2/2+2),
*                      ...,
*        from X2(NUSE2) to X2(NELM2/2+NUSE2).
*     NELM3 = INTEGER (Given)
*        The number of points in the third curve (model).
*     X3( NELM3 ) = REAL (Given)
*        The third curve's (model) abscissa values.
*     Y3( NELM3 ) = REAL (Given)
*        The third curve's (model) ordinate values.
*     NELM4 = INTEGER (Given)
*        The number of points in the fourth curve (residuals).
*     X4( NELM4 ) = REAL (Given)
*        The fourth and fifth curves' (residuals and error bars)
*        abscissa values.
*     Y4( NELM4 ) = REAL (Given)
*        The fourth curve's (residuals) ordinate values.
*     YL( NELM4 ) = REAL (Given)
*        The fifth curve's (error bars) lower bounds of bars.
*     YU( NELM4 ) = REAL (Given)
*        The fifth curve's (error bars) upper bounds of bars.
*     PMIN( 4 ) = REAL (Returned)
*        The actual minimal values of the PGPLOT viewports:
*        -  1: minimum of abscissa of bottom viewport
*        -  2: minimum of ordinate of bottom viewport
*        -  3: minimum of abscissa of top viewport
*        -  4: minimum of ordinate of top viewport
*     PMAX( 4 ) = REAL (Returned)
*        The actual maximal values of the PGPLOT viewports. See PMIN.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 Apr 1992 (hme):
*        Original version.
*     24 Jun 1993 (hme):
*        Use SPAEC instead of SGS_CLRZ and PGBEG. Replace CWKID argument
*        with ZONID. Change PGPLOT calls to standard (short routine
*        names). If a given range has zero extent, use 1 in plot and
*        returned range.
*     27 Jan 1995 (hme):
*        Renamed from SPABP.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL PMASK
      LOGICAL PMODEL
      LOGICAL PERROR
      INTEGER ZONID
      CHARACTER * ( * ) PLABEL( 3 )
      REAL RMIN( 4 )
      REAL RMAX( 4 )
      INTEGER NELM1
      REAL X1( NELM1 )
      REAL Y1( NELM1 )
      INTEGER NELM2
      INTEGER NUSE2
      REAL X2( NELM2 )
      INTEGER NELM3
      REAL X3( NELM3 )
      REAL Y3( NELM3 )
      INTEGER NELM4
      REAL X4( NELM4 )
      REAL Y4( NELM4 )
      REAL YL( NELM4 )
      REAL YU( NELM4 )

*  Arguments Returned:
      REAL PMIN( 4 )
      REAL PMAX( 4 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Work out plot ranges for bottom viewport.
*  Abscissa range as given, ordinate range as given plus 5 % on either
*  side.
      PMIN(1) = RMIN(1)
      PMAX(1) = RMAX(1)
      IF ( PMIN(1) .EQ. PMAX(1) ) THEN
         PMIN(1) = PMIN(1) - 0.5
         PMAX(1) = PMAX(1) + 0.5
      END IF
      PMIN(2) = RMIN(2) - 0.05 * ( RMAX(2) - RMIN(2) )
      PMAX(2) = RMAX(2) + 0.05 * ( RMAX(2) - RMIN(2) )
      IF ( PMIN(2) .EQ. PMAX(2) ) THEN
         PMIN(2) = PMIN(2) - 0.5
         PMAX(2) = PMAX(2) + 0.5
      END IF

*  Work out plot ranges for top viewport.
*  Abscissa range as given and equal to bottom viewport.
*  The ordinate range has zero in centre and allows to plot the biggest
*  (by absolute value) residual plus the longest error bar, and it still
*  leaves 5 % space on either side.
      PMIN(3) = PMIN(1)
      PMAX(3) = PMAX(1)
      IF ( PMASK .OR. PMODEL .OR. PERROR ) THEN
         PMAX(4) = 1.05 * MAX( ABS(RMIN(4)), ABS(RMAX(4)) )
         IF ( PERROR ) PMAX(4) = PMAX(4) + 1.05 * ABS(RMAX(3))
         IF ( PMAX(4) .LE. 0. ) PMAX(4) = 1.
      ELSE
         PMAX(4) = 0.
      END IF
      PMIN(4) = -PMAX(4)

*  Clear the view surface.
      CALL SPD_UGAC( ZONID, STATUS )

*  Top viewport.
      IF ( PMASK .OR. PMODEL .OR. PERROR ) THEN
         CALL PGSVP( 0.10, 0.95, 0.70, 0.90 )
         CALL PGSWIN( PMIN(3), PMAX(3), PMIN(4), PMAX(4) )

*     Axes and label.
         CALL PGBOX( 'BCTS', 0., 0, 'BCTSM', 0., 0 )
         CALL PGLAB( ' ', ' ', PLABEL(3) )

*     Second curve.
         IF ( PMASK ) THEN
            DO 1 I = 1, NUSE2
               CALL PGMOVE(         X2(I), 0. )
               CALL PGDRAW( X2(NELM2/2+I), 0. )
 1          CONTINUE
         END IF

*     Fourth curve and fifth curve.
         IF ( PMODEL ) THEN
            IF ( PERROR ) THEN
               CALL SPD_WAAD( .TRUE., .FALSE., .FALSE., .FALSE., .TRUE.,
     :            1, 5, NELM4, X4, Y4, YL, YU, 0., 0., STATUS )
            ELSE
               CALL SPD_WAAD(.FALSE., .FALSE., .FALSE., .FALSE., .TRUE.,
     :            1, 5, NELM4, X4, Y4, 0., 0., 0., 0., STATUS )
            END IF
         END IF
      END IF

*  Bottom viewport.
      CALL PGSVP( 0.10, 0.95, 0.10, 0.70 )
      CALL PGSWIN( PMIN(1), PMAX(1), PMIN(2), PMAX(2) )

*  Bottom viewport: Axes and labels.
      CALL PGBOX( 'BCTSN', 0., 0, 'BCTSN', 0., 0 )
      IF ( .NOT. ( PMASK .OR. PMODEL .OR. PERROR ) ) THEN
         CALL PGLAB( PLABEL(1), PLABEL(2), PLABEL(3) )
      ELSE
         CALL PGLAB( PLABEL(1), PLABEL(2), ' ' )
      END IF

*  Bottom viewport: First curve.
      CALL SPD_WAAD( .FALSE., .FALSE., .FALSE., .TRUE., .FALSE.,
     :   1, 0, NELM1, X1, Y1, 0., 0., 0., 0., STATUS )

*  Bottom viewport: Third curve.
      IF ( PMODEL )
     :   CALL SPD_WAAD( .FALSE., .FALSE., .TRUE., .FALSE., .FALSE.,
     :      2, 0, NELM3, X3, Y3, 0., 0., 0., 0., STATUS )

*  Return.
      END
