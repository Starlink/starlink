      SUBROUTINE CCD1_STAS( FULL, ERROR, MINMAT, XIN1, YIN1, NREC1,
     :                      XMODE, XHIST, XBIN, XWIDTH, XZERO, XIN2,
     :                      YIN2, NREC2, YMODE, YHIST, YBIN, YWIDTH,
     :                      YZERO, XRANK, YRANK, BEST, XOFF, YOFF,
     :                      OK, STATUS )
*+
*  Name:
*     CCD1_STAS

*  Purpose:
*     Performs the work for CCD1_STAO2

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL CCD1_STAS( FULL, ERROR, MINMAT, XIN1, YIN1, NREC1, XMODE,
*                      XHIST, XBIN, XWIDTH, XZERO, XIN2, YIN2, NREC2, 
*                      YMODE, YHIST, YBIN, YWIDTH, YZERO, XRANK,
*                      YRANK, BEST, XOFF, YOFF, OK, STATUS )

*  Description:
*     {routine_description}

*  [arguments]
*  [optional_subroutine_items]...
*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-APR-1993 (PDRAPER):
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
      LOGICAL FULL
      DOUBLE PRECISION ERROR
      INTEGER MINMAT
      INTEGER NREC1
      DOUBLE PRECISION XIN1( NREC1 )
      DOUBLE PRECISION YIN1( NREC1 )
      INTEGER XMODE
      INTEGER XBIN
      INTEGER XHIST( XBIN )
      DOUBLE PRECISION XWIDTH
      DOUBLE PRECISION XZERO
      INTEGER NREC2
      DOUBLE PRECISION XIN2( NREC2 )
      DOUBLE PRECISION YIN2( NREC2 )
      INTEGER YMODE
      INTEGER YBIN
      INTEGER YHIST( YBIN )
      DOUBLE PRECISION YWIDTH
      DOUBLE PRECISION YZERO

*  Arguments Given and Returned:
      INTEGER XRANK( XBIN )
      INTEGER YRANK( YBIN )
      INTEGER BEST( 2, * )

*  Arguments Returned:
      DOUBLE PRECISION XOFF
      DOUBLE PRECISION YOFF
      LOGICAL OK

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION RMSMIN    ! Differences sum minimum
      DOUBLE PRECISION RMS       ! Differences sum minimum
      DOUBLE PRECISION XOFFT     ! Temporary X offset 
      DOUBLE PRECISION YOFFT     ! Temporary X offset 
      DOUBLE PRECISION XDIFF     ! X position differences
      DOUBLE PRECISION YDIFF     ! Y position differences
      DOUBLE PRECISION XCUR      ! Current X difference
      DOUBLE PRECISION YCUR      ! Current Y difference
      INTEGER IFAIL              ! Nag failure flag
      INTEGER XLOOK              ! Number of X values to look at
      INTEGER YLOOK              ! Number of Y values to look at
      INTEGER NBEST              ! Number of best values (matches)
      INTEGER NMATCH             ! Number of matched position pairs
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      INTEGER K                  ! Loop variable
      INTEGER L                  ! Loop variable
      INTEGER NOUT               ! Number of positions matched in a list
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

C       DO I =1, XBIN
C          WRITE(1,*)I,XHIST(I),(I-1)*XWIDTH+XZERO
C       END DO
C       DO I =1, YBIN
C          WRITE(2,*)I,YHIST(I),(I-1)*YWIDTH+YZERO
C       END DO

*  If full is true then look at each of the histogram values in order of
*  the number of counts in the bins, until either the number of counts
*  falls below the threshold or a match has been sucessfull.
       XLOOK = 0
       IF ( FULL ) THEN

*  Rank the histograms.
         IFAIL = 1
         CALL M01DBF( XHIST, 1, XBIN, 'D', XRANK, IFAIL )
         IFAIL = 1
         CALL M01DBF( YHIST, 1, YBIN, 'D', YRANK, IFAIL )

*  Convert these into indices for direct look-up.
         IFAIL = 1
         CALL M01ZAF( XRANK, 1, XBIN, IFAIL )
         IFAIL = 1
         CALL M01ZAF( YRANK, 1, YBIN, IFAIL )

*  Set range of values to look at.
         XLOOK = XLOOK + 1
         DO 5 I = 1, XBIN
            IF ( XHIST( I ) .GT. MINMAT ) THEN
               XLOOK = XLOOK + 1
            END IF
 5       CONTINUE
         YLOOK = YLOOK + 1
         DO 6 I = 1, YBIN
            IF ( YHIST( I ) .GT. MINMAT ) THEN
               YLOOK = YLOOK + 1
            END IF
 6       CONTINUE
      ELSE

*  Just look for match using the modes which have been found.
         XRANK( 1 ) = XMODE
         XLOOK = 1
         YRANK( 1 ) = YMODE
         YLOOK = 1
      END IF

*  Perform matching process for all offsets.
      NBEST = 0
      NMATCH = 0  
      DO 1 I = 1, XLOOK

*  Set the Y offset.
         XOFF = DBLE( XHIST( XRANK( I ) ) - 1 ) * XWIDTH + XZERO
         DO 2 J = 1, YLOOK

*  Set the X offset.
            YOFF = DBLE( YHIST( YRANK( J ) ) - 1 ) * YWIDTH + YZERO

*  Now generate the matched lists. Use the offsets from to see
*  which position are the same (within the limits of the bin).
*  Loop over possible intercomparisons of the two sets of positions.
            NOUT = 0
            DO 3 K = 1, NREC1
               XCUR = XIN1( K ) - XOFF
               YCUR = YIN1( K ) - YOFF
               DO 4 L = 1, NREC2

*  Form the difference in X and Y positions. Offsetting the second
*  positions by XOFF and YOFF
                  XDIFF = ABS( XCUR - XIN2( L ) )
                  IF ( XDIFF .LE. ERROR ) THEN
                     YDIFF = ABS( YCUR - YIN2( L ) )
                     IF ( YDIFF .LE. ERROR ) THEN

*  Yes they are.
                        NOUT = NOUT + 1

*  Skip as no more matches are allowed.
                        GO TO 3
                     END IF
                  END IF
 4             CONTINUE
 3          CONTINUE

*  If enough matches have been made...
            IF ( NOUT .GT. MINMAT ) THEN 
               IF ( NOUT .GT. NMATCH ) THEN
                  NBEST = 1
                  BEST( 1, NBEST ) = I
                  BEST( 1, NBEST ) = J
                  NMATCH = NOUT
               ELSE IF ( NOUT .EQ. NMATCH ) THEN

*  A match which is just as good.
                  NBEST = NBEST + 1
                  BEST( 1, NBEST ) = I
                  BEST( 1, NBEST ) = J
               END IF
            END IF
 2       CONTINUE
 1    CONTINUE

*  Have we a match?
      IF ( NMATCH .GT. 0 ) THEN
         OK = .TRUE.

*  If more than one position has been returned then pick the best fit.
         IF ( NBEST .GT. 1 ) THEN
            RMSMIN = 0.0D0
            DO 9 I = 1, NBEST

*  Set the offsets for this match.
               XOFFT = DBLE( XHIST( BEST( 1, I ) - 1 ) ) * XWIDTH +
     :                 XZERO
               YOFFT = DBLE( YHIST( BEST( 2, I ) - 1 ) ) * YWIDTH +
     :                 YZERO

*  Number of matches and difference sum.
               RMS = 0.0D0
               NOUT = 0
               DO 7 K = 1, NREC1
                  XCUR = XIN1( K ) - XOFFT
                  YCUR = YIN1( K ) - YOFFT
                  DO 8 L = 1, NREC2

*  Form the difference in X and Y positions. Offsetting the second
*  positions by XOFF and YOFF
                     XDIFF = ABS( XCUR - XIN2( L ) )
                     IF ( XDIFF .LE. ERROR ) THEN
                        YDIFF = ABS( YCUR - YIN2( L ) )
                        IF ( YDIFF .LE. ERROR ) THEN

*  Yes they are.
                           RMS = RMS + XDIFF * XDIFF + YDIFF * YDIFF
                           NOUT = NOUT + 1

*  Skip as no more matches are allowed.
                           GO TO 3
                        END IF
                     END IF
 8                CONTINUE
 7             CONTINUE

*  Pick best.
               RMS = RMS / DBLE( NOUT )
               IF ( RMS .LT. RMSMIN ) THEN
                  XOFF = XOFFT
                  YOFF = YOFFT
               END IF
 9          CONTINUE
         END IF
      ELSE

*  No match.
         OK = .FALSE.
      END IF

      END
* $Id$
