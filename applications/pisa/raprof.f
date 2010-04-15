      SUBROUTINE RAPROF( XBUF, SIDE, FITSIZ, XPEAK, YPEAK, XPL, RPL,
     :                   IPL, MEANSQ, DEVIA )
*+
*  Name:
*     RAPROF

*  Purpose:
*     Forms normalised radial profile of about a given position.

*  Language:
*     Starlink_Fortran

*  Invocation:
*     CALL RAPROF( XBUF, SIDE, FITSIZ, XPEAK, YPEAK, XPL, RPL, IPL,
*                  MEANSQ, DEVIA )

*  Description:
*     The routine forms the radial profile about a given position.
*     First the routine clears all buffers to contain the radial
*     profile, radius estimators and number counts. The routine then
*     scans an area of size SIDE by SIDE about the position. For each
*     pixel within the area the intensity is placed within a bin in the
*     range 1 to FITSIZ, dependent on its distance from the given
*     position. The number count for this bin is incremented and the
*     radius is added to the sum of all pixels whose radii contribute
*     to this bin. Finally if the pixel lies close to the given
*     position its intensity is compared to see if it has the maximum
*     value of all the pixels 'close' to the position. This is
*     therefore assuming that the profile is of a peaked object whose
*     peak intensity is pointed at by the given position. Finally the
*     profile is scaled so that its peak intensity is one and the
*     radii are averaged to give a good estimator of the radius of the
*     bins. The routine also derives an estimator of the standard
*     deviation of the values contributing to each bin. This is
*     performed by forming the mean square of the contributing values
*     and differencing this with the squared mean. The deviations are
*     scaled by the normalising factor.

*  Arguments:
*     XBUF( SIDE, SIDE )= REAL (Given)
*        Array containing the data to be profiled.
*     SIDE = INTEGER (Given)
*        The dimensions of XBUF.
*     FITSIZ = INTEGER (Given)
*        The dimension of the profiling buffers.
*     XPEAK = REAL (Given)
*        The x-axis position about which the radial profile is to be
*        performed.
*     YPEAK = REAL (Given)
*        The y-axis position about which the radial profile is to be
*        performed.
*     XPL( FITSIZ ) = REAL (Returned)
*        Buffer to contain the normalised radial profile.
*     RPL( FITSIZ ) = REAL (Returned)
*        Buffer to contain the radii estimates for each bin.
*     IPL( FITSIZ ) = INTEGER (Returned)
*        Buffer to contain the number counts for each bin.
*     MEANSQ( FITSIZ ) = DOUBLE PRECISION (Given)
*        Workspace to contain the mean squared values
*     DEVIA( FITSIZ ) = DOUBLE PRECISION (Returned)
*        The standard deviations for each bin.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-SEP-1990 (PDRAPER):
*        First version from APM::MIKE orginal.
*     10-SEP-1990 (PDRAPER):
*        changed to return average radii and to use 2-dimensional array
*        instead of 1.
*     16-NOV-1990 (PDRAPER):
*        Added ability to return standard deviations.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*
*  Arguments Given:
      INTEGER SIDE
      INTEGER FITSIZ
      REAL XBUF( SIDE, SIDE )
      REAL XPEAK
      REAL YPEAK

*  Arguments Returned:
      REAL XPL( FITSIZ )
      REAL RPL( FITSIZ )
      INTEGER  IPL( FITSIZ )
      DOUBLE PRECISION MEANSQ( FITSIZ )
      DOUBLE PRECISION DEVIA( FITSIZ)

*  Local Constants
      REAL CLOSE                 ! square of distance to object which is
                                 ! considered close.
      PARAMETER ( CLOSE = 9.0 )
*  Local Variables:
      INTEGER I, J, IDSQ
      REAL XMAX, SCALE, DSQ, RSCALE, DS
*.

*  Clear profile buffers.
      DO 1 I = 1, FITSIZ
         IPL( I ) = 0
         XPL( I ) = 0.0
         RPL( I ) = 0.0
         MEANSQ( I ) = 0.0D0
         DEVIA( I ) = 0.0D0
 1    CONTINUE

*  (Get radial profile from spatial domain).
      XMAX = 0.0
      SCALE = REAL( FITSIZ )/ REAL( INT( SQRT( 2.0 * SIDE * SIDE )))
      DO 2 I = 1, SIDE
         DO 3 J = 1, SIDE

*  Form distance estimate from object to current position.
            DSQ = ( REAL( I ) - XPEAK )**2 + ( REAL( J ) - YPEAK )**2
            DS = SQRT( DSQ )

*  Scale actual distance up from SIDE to FITSIZ; selects appropriate bin.
            IDSQ = MIN( FITSIZ, INT( SCALE * DS ) + 1 )

*  Sum values in this bin, increment number count.
            XPL( IDSQ ) = XPL( IDSQ ) + XBUF( J, I )
            IPL( IDSQ ) = IPL( IDSQ ) + 1

*  Next contribution to squared mean sum.
            MEANSQ( IDSQ ) = MEANSQ( IDSQ ) +
     :                       DBLE( XBUF( J ,I ) * XBUF( J ,I ))

*  Add this radius to radii sums.
            RPL( IDSQ ) = RPL( IDSQ ) + DS

*  If we're close to the object then record the intensity.
            IF( .NOT. ( DSQ .GT. CLOSE ) ) THEN
               XMAX = MAX( XMAX, XBUF( J, I ) )
            END IF
 3       CONTINUE
 2    CONTINUE

*  If for some reason a central value has not been found, supply one.
*  we've got normalization to do . Not strictly a good idea !
      IF ( IPL( 1 ) .EQ. 0 ) THEN
         IPL( 1 ) = 1
         XPL( 1 ) = XMAX
      END IF

*  Do the statistics first, calculate the mean intensity value
*  then the squared deviations for values in this bin.
      DO 4 I= 1, FITSIZ
         IF( IPL( I ) .GT. 0 ) THEN
            XPL( I ) = XPL( I ) / IPL( I )
            IF ( IPL( I ) .EQ. 1 ) THEN

*  If ipl(i) equals one then no standard deviation.
               DEVIA( I ) = 0.0D0
            ELSE

*  Ok, derive the standard deviation, abs to be very safe
               DEVIA( I ) =MEANSQ( I ) / IPL( I ) - XPL( I ) * XPL( I )
               DEVIA( I ) = SQRT( ABS( DEVIA( I ) ) )
            END IF
         ELSE

*  No values in bin set all to zero.
            XPL( I ) =0.0
            DEVIA( I ) = 0.0D0
         END IF
 4    CONTINUE

*  Normalise profiles to show one at maximum intensity, scale deviations
*  accordingly.
      RSCALE = XPL( 1 ) / REAL( IPL( 1 ) )
      RSCALE = 1.0 / MAX( 1.0E-20, RSCALE )
      DO 5 I = 1, FITSIZ
         IF( IPL( I ) .GT. 0 )THEN
            XPL( I ) = RSCALE * XPL( I )
            DEVIA( I ) = RSCALE * DEVIA( I )

*  Find average radius of values in a bin.
            RPL( I ) = RPL( I )/ REAL ( IPL( I ) )
         END IF
 5    CONTINUE

      END
* $Id$
