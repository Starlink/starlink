      SUBROUTINE SPD_CZBD( REASON, INFO, MODE, OMAX, NDF,
     :                     T_AXIS, T_FRAME, DELAY, IMIN, IMAX, STATUS )
*+
*  Name:
*     SPD_CZBD

*  Purpose:
*     Parameter-free MOVIE call back.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZBD( REASON, INFO, MODE, OMAX,
*        NDF, AXIS, FRAME, DELAY, IMIN, IMAX, STATUS )

*  Description:
*     This is the call back for the MOVIE application common to the
*     Motif- and ADAM-based interfaces. All parameter handling takes
*     place outwith this routine, all data access and work takes place
*     under the control of this routine. Since NDF_BEGIN/END must be
*     outside NDF_ASSOC, they too are handled outwith this routine.

*  Arguments:
*     REASON = INTEGER (Given)
*        The call back reason:
*         0 Clean up,
*         1 start up,
*         2 access input cube and get work spaces,
*         3 display forward sequence,
*         4 display backward sequence,
*         5 display specified frame,
*         6 display previous frame,
*         7 display next frame.
*     INFO = INTEGER (Given)
*        Non-zero if the frame number is to be reported after display.
*     MODE = INTEGER (Given)
*        1, 2, or 3 for fast, square and fill modes respectively.
*     OMAX = INTEGER (Given)
*        The maximum pen number to be used in display. This should be
*        the number of colours available on the device minus 1 (they are
*        counted from zero).
*     NDF = INTEGER (Given)
*        The identifier of the NDF. It must have three axes with more
*        than 1 pixel.
*     AXIS = INTEGER (Given)
*        The number of the axis along which slices are counted. This can
*        be 1, 2, 3.
*     FRAME = INTEGER (Given)
*        The number of the frame or slice. This should be between the
*        relevant lower and upper bound of the NDF pixel index.
*     DELAY = REAL (Given)
*        The extra delay in seconds between frame display.
*     IMIN = REAL (Given)
*        The lower threshold for input values.
*     IMAX = REAL (Given)
*        The upper threshold for input values. IMAX must not equal IMIN.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     19 May 1994 (hme):
*        Original version.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     2006 Oct 19 (TIMJ):
*        Fix CNF_PVAL pointer offsetting
*     2008 April 10 (PWD):
*        Map IAXIS axes component, not first (we now have cubes
*        longer in spectral dimension than spatial).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER REASON
      INTEGER INFO
      INTEGER MODE
      INTEGER OMAX
      INTEGER NDF
      INTEGER T_AXIS
      INTEGER T_FRAME
      REAL DELAY
      REAL IMIN, IMAX

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER INTESZ             ! Size of an INTEGER
      PARAMETER ( INTESZ = 4 )
      REAL LEFT, RIGHT, BOTTOM, TOP
      PARAMETER ( LEFT = 0.05, RIGHT = 0.95, BOTTOM = 0.05, TOP = 0.95 )
      INTEGER BADVAL             ! PGPLOT pen for bad values
      PARAMETER ( BADVAL = 0 )
      INTEGER OMIN               ! PGPLOT pen for minimum
      PARAMETER ( OMIN = 16 )

*  Local Static Variables:
      INTEGER WPTR               ! Display workspace pointer
      INTEGER FPTR               ! Frame-converted flags
      INTEGER FRAME              ! Actually displayed frame
      INTEGER AXIS               ! T_AXIS, but counting degenerate axes
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      INTEGER SNELM              ! Section size
      INTEGER IMDIM( 2 )         ! 2-D size of displayed frame
      REAL IMWIN( 4 )            ! Image range, usually PGPLOT window

      SAVE WPTR, FPTR, FRAME,
     :   AXIS, LBND, UBND, SNELM, IMDIM, IMWIN

*  Local Volatile Variables:
      INTEGER I, J, K            ! Temporary integers
      INTEGER NDIM               ! NDF dimensionality
      INTEGER NELM               ! NDF size
      INTEGER PLACE              ! NDF place holder
      INTEGER WNDF               ! Work space NDF identifier
      REAL PL, PR, PB, PT        ! Viewport size in pixels
      REAL RIGHT2, TOP2          ! Adjustments to RIGHT, TOP
      REAL DISWIN( 4 )           ! PGPLOT window

*  Internal References:
      INTEGER SPD_UAAGI          ! Get an array element.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN


*  Clean up.
*  =========

      IF ( REASON .EQ. 0 ) THEN
         CONTINUE


*  Start up.
*  =========

      ELSE IF ( REASON .EQ. 1 ) THEN
         CONTINUE


*  Access.
*  =======

      ELSE IF ( REASON .EQ. 2 ) THEN

*     Check AXIS parameter
         IF ( T_AXIS .LT. 1 .OR. T_AXIS .GT. 3 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_CZBD_E02', 'MOVIE: Frame-counting' //
     :         'axis is none of 1, 2, or 3.', STATUS )
            GO TO 500
         END IF

*     Find out NDF size and bounds.
         CALL NDF_SIZE(  NDF, NELM, STATUS )
         CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Check NDF is 3-D, work out which is frame axis (consider
*     degenerate axes here). Frame axis must not be degenerate.
         J = 0
         DO 1 I = 1, NDIM
            IF ( UBND(I) .GT. LBND(I) ) THEN
               J = J + 1
               IF ( J .EQ. T_AXIS ) AXIS = I
            END IF
 1       CONTINUE
         IF ( J .NE. 3 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_CZBD_E01', 'MOVIE: Cube is not 3-D.',
     :         STATUS )
            GO TO 500
         END IF

*     Establish the frame coordinates and 2-D size.
         J = 0
         K = 0
         DO 2 I = 1, NDIM
            IF ( UBND(I) .GT. LBND(I) .AND. I .NE. AXIS ) THEN
               J = J + 1
               K = K + 1
               IMWIN(K) = FLOAT(LBND(I)) - 0.5
               K = K + 1
               IMWIN(K) = FLOAT(UBND(I)) + 0.5
               IMDIM(J) = UBND(I) - LBND(I) + 1
            END IF
 2       CONTINUE

*     Work out slice size.
         SNELM = NELM / ( UBND(AXIS) - LBND(AXIS) + 1 )

*     Get an _INTEGER work space of same shape as cube, map it.
*     Also get a work space to flag each frame as converted or not.
*     Reset flags to indicate that none of the frames has been
*     converted.
         CALL NDF_TEMP( PLACE, STATUS )
         CALL NDF_NEW( '_INTEGER', NDF__MXDIM, LBND, UBND, PLACE,
     :                 WNDF, STATUS )
         CALL NDF_MAP( WNDF, 'DATA', '_INTEGER', 'WRITE',
     :                 WPTR, I, STATUS )
         CALL NDF_AMAP( WNDF, 'CENTRE', AXIS, '_INTEGER', 'WRITE',
     :                  FPTR, I, STATUS )
         CALL SPD_UAAFI( 1, I, %VAL( CNF_PVAL( FPTR ) ), 0, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500


*     Set PGPLOT viewport and window.
*     ===============================

*     If square mode.
         IF ( MODE .EQ. 2 ) THEN

*        Set maximum viewport, then adjust down for equal scales.
            CALL PGSVP( LEFT, RIGHT, BOTTOM, TOP )
            CALL PGWNAD( IMWIN(1), IMWIN(2), IMWIN(3), IMWIN(4) )

*     Else if fill mode.
         ELSE IF ( MODE .EQ. 3 ) THEN

*        Just set maximum viewport, set window without adjusting
*        viewport.
            CALL PGSVP( LEFT, RIGHT, BOTTOM, TOP )
            CALL PGSWIN( IMWIN(1), IMWIN(2), IMWIN(3), IMWIN(4) )

*     Else (fast mode).
         ELSE

*        First set maximum viewport within AGI picture.
*        Also set the window without adjustment.
            CALL PGSVP( LEFT, RIGHT, BOTTOM, TOP )
            CALL PGSWIN( IMWIN(1), IMWIN(2), IMWIN(3), IMWIN(4) )

*        Find out how many pixels the viewport now has.
            CALL PGQVP( 3, PL, PR, PB, PT )

*        Adjust viewport position to make image pixels equal to display
*        pixels.
            RIGHT2 = LEFT + (RIGHT-LEFT) * (IMWIN(2)-IMWIN(1)) / (PR-PL)
            TOP2 = BOTTOM + (TOP-BOTTOM) * (IMWIN(4)-IMWIN(3)) / (PT-PB)

*        If this viewport is not too big, set it and set the window.
            IF ( RIGHT2 .LE. RIGHT .AND. TOP2 .LE. TOP ) THEN
               CALL PGSVP( LEFT, RIGHT2, BOTTOM, TOP2 )
               CALL PGSWIN( IMWIN(1), IMWIN(2), IMWIN(3), IMWIN(4) )

*        Else (viewport cannot show whole image), use maximum viewport
*        and adjust window.
            ELSE
               DISWIN(1) = IMWIN(1)
               DISWIN(2) = IMWIN(2)
               DISWIN(3) = IMWIN(3)
               DISWIN(4) = IMWIN(4)
               IF ( RIGHT2 .GT. RIGHT ) THEN
                  DISWIN(2) = DISWIN(1)
     :               + (DISWIN(2)-DISWIN(1))*(RIGHT-LEFT)/(RIGHT2-LEFT)
                  RIGHT2 = RIGHT
               END IF
               IF ( TOP2 .GT. TOP ) THEN
                  DISWIN(4) = DISWIN(3)
     :               + (DISWIN(4)-DISWIN(3))*(TOP-BOTTOM)/(TOP2-BOTTOM)
                  TOP2 = TOP
               END IF
               CALL PGSVP( LEFT, RIGHT2, BOTTOM, TOP2 )
               CALL PGSWIN( DISWIN(1), DISWIN(2), DISWIN(3), DISWIN(4) )
            END IF

         END IF

*     Draw box.
*     This draws automatic outward ticks and NDF pixel
*     index labels. No text labels and no box lines are drawn.
         CALL PGBOX( 'BCINTS', 0., 0, 'BCINTS', 0., 0 )

*     Reset displayed-frame number.
         FRAME = LBND(AXIS) - 1

*     Report frame number range.
         IF ( INFO .NE. 0 ) THEN
            CALL MSG_SETI( 'SPD_CZBD_T02', LBND(AXIS) )
            CALL MSG_SETI( 'SPD_CZBD_T03', UBND(AXIS) )
            CALL MSG_OUT( 'SPD_CZBD_M03', 'Frame number ranges from ' //
     :         '^SPD_CZBD_T02 to ^SPD_CZBD_T03.', STATUS )
         END IF


*  Display forward sequence.
*  =========================

      ELSE IF ( REASON .EQ. 3 ) THEN

*     Display each frame in sequence.
         J = 0
         DO 3 I = LBND(AXIS), UBND(AXIS)
            J = J + 1

*        If it has not been converted before, convert it.
            IF ( SPD_UAAGI( %VAL( CNF_PVAL( FPTR ) ), J, STATUS )
     :           .EQ. 0 ) THEN
               CALL SPD_CZBE( NDF, AXIS, I, BADVAL, IMIN, IMAX,
     :                        OMIN, OMAX, SNELM, %VAL( CNF_PVAL(
     :                        WPTR)+(J-1)*SNELM*INTESZ ), STATUS )
            END IF

*        Flag it as converted.
            CALL SPD_UAAFI( J, J, %VAL( CNF_PVAL( FPTR ) ), 1, STATUS )

*        Display it.
            CALL PGPIXL( %VAL( CNF_PVAL( WPTR ) +(J-1)*SNELM*INTESZ ),
     :                   IMDIM(1), IMDIM(2), 1, IMDIM(1), 1, IMDIM(2),
     :                   IMWIN(1), IMWIN(2), IMWIN(3), IMWIN(4) )

*        Report frame number.
            IF ( INFO .NE. 0 ) THEN
               CALL MSG_SETI( 'SPD_CZBD_T01', I )
               CALL MSG_OUT( 'SPD_CZBD_M01',
     :            'Displayed frame # ^SPD_CZBD_T01.', STATUS )
            END IF

*        Wait for DELAY seconds.

 3       CONTINUE

*     At the end set the displayed-frame number.
         FRAME = UBND(AXIS)

*     Report sequence is done.
         IF ( INFO .NE. 0 )
     :      CALL MSG_OUT( 'SPD_CZBD_M02', 'Sequence finished.', STATUS )


*  Display backward sequence.
*  ==========================

      ELSE IF ( REASON .EQ. 4 ) THEN

*     Display each frame in sequence.
         J = UBND(AXIS) - LBND(AXIS) + 2
         DO 4 I = UBND(AXIS), LBND(AXIS), -1
            J = J - 1

*        If it has not been converted before, convert it.
            IF ( SPD_UAAGI( %VAL( CNF_PVAL( FPTR ) ), J, STATUS )
     :           .EQ. 0 ) THEN
               CALL SPD_CZBE( NDF, AXIS, I, BADVAL, IMIN, IMAX,
     :                       OMIN, OMAX, SNELM, %VAL( CNF_PVAL(
     :                       WPTR ) +(J-1)*SNELM*INTESZ ), STATUS )
            END IF

*        Flag it as converted.
            CALL SPD_UAAFI( J, J, %VAL( CNF_PVAL( FPTR ) ), 1, STATUS )

*        Display it.
            CALL PGPIXL( %VAL( CNF_PVAL( WPTR )+(J-1)*SNELM*INTESZ ),
     :                   IMDIM(1), IMDIM(2), 1, IMDIM(1), 1, IMDIM(2),
     :                   IMWIN(1), IMWIN(2), IMWIN(3), IMWIN(4) )

*        Report frame number.
            IF ( INFO .NE. 0 ) THEN
               CALL MSG_SETI( 'SPD_CZBD_T01', I )
               CALL MSG_OUT( 'SPD_CZBD_M01',
     :            'Displayed frame # ^SPD_CZBD_T01.', STATUS )
            END IF

*        Wait for DELAY seconds.

 4       CONTINUE

*     At the end set the displayed-frame number.
         FRAME = UBND(AXIS)

*     Report sequence is done.
         IF ( INFO .NE. 0 )
     :      CALL MSG_OUT( 'SPD_CZBD_M02', 'Sequence finished.', STATUS )


*  Display specified frame.
*  ========================

      ELSE IF ( REASON .EQ. 5 ) THEN

*     Check frame number within range.
         IF ( T_FRAME.LT.LBND(AXIS) .OR. T_FRAME.GT.UBND(AXIS) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_CZBD_E03',
     :         'MOVIE: Frame is outside cube.', STATUS )
            GO TO 500
         END IF

*     Copy given frame number to local static one.
         FRAME = T_FRAME
         J = FRAME - LBND(AXIS) + 1

*     If it has not been converted before, convert it.
         IF ( SPD_UAAGI( %VAL( CNF_PVAL( FPTR ) ), J, STATUS )
     :        .EQ. 0 ) THEN
            CALL SPD_CZBE( NDF, AXIS, FRAME, BADVAL, IMIN, IMAX,
     :                     OMIN, OMAX, SNELM, %VAL( CNF_PVAL(
     :                     WPTR ) +(J-1)*SNELM*INTESZ ), STATUS )
         END IF

*     Flag it as converted.
         CALL SPD_UAAFI( J, J, %VAL( CNF_PVAL( FPTR ) ), 1, STATUS )

*     Display it.
         CALL PGPIXL( %VAL( CNF_PVAL( WPTR )+(J-1)*SNELM*INTESZ ),
     :                IMDIM(1), IMDIM(2), 1, IMDIM(1), 1, IMDIM(2),
     :                IMWIN(1), IMWIN(2), IMWIN(3), IMWIN(4) )

*     Report frame number.
         IF ( INFO .NE. 0 ) THEN
            CALL MSG_SETI( 'SPD_CZBD_T01', FRAME )
            CALL MSG_OUT( 'SPD_CZBD_M01',
     :         'Displayed frame # ^SPD_CZBD_T01.', STATUS )
         END IF


*  Display previous frame.
*  ========================

      ELSE IF ( REASON .EQ. 6 ) THEN

*     Update frame number.
         FRAME = FRAME - 1
         J = FRAME - LBND(AXIS) + 1

*     Check frame number within range.
         IF ( FRAME .LT. LBND(AXIS) .OR. FRAME .GT. UBND(AXIS) ) THEN
            STATUS = SAI__ERROR
            FRAME = FRAME + 1
            CALL ERR_REP( 'SPD_CZBD_E03',
     :         'MOVIE: Frame is outside cube.', STATUS )
            GO TO 500
         END IF

*     If it has not been converted before, convert it.
         IF ( SPD_UAAGI( %VAL( CNF_PVAL( FPTR ) ), J, STATUS )
     :        .EQ. 0 ) THEN
            CALL SPD_CZBE( NDF, AXIS, FRAME, BADVAL, IMIN, IMAX,
     :                     OMIN, OMAX, SNELM, %VAL( CNF_PVAL(
     :                     WPTR ) +(J-1)*SNELM*INTESZ ), STATUS )
         END IF

*     Flag it as converted.
         CALL SPD_UAAFI( J, J, %VAL( CNF_PVAL( FPTR ) ), 1, STATUS )

*     Display it.
         CALL PGPIXL( %VAL( CNF_PVAL( WPTR )+(J-1)*SNELM*INTESZ ),
     :               IMDIM(1), IMDIM(2), 1, IMDIM(1), 1, IMDIM(2),
     :               IMWIN(1), IMWIN(2), IMWIN(3), IMWIN(4) )

*     Report frame number.
         IF ( INFO .NE. 0 ) THEN
            CALL MSG_SETI( 'SPD_CZBD_T01', FRAME )
            CALL MSG_OUT( 'SPD_CZBD_M01',
     :         'Displayed frame # ^SPD_CZBD_T01.', STATUS )
         END IF

*     If previous has not been converted before, convert it while the
*     user enjoys this display.
         IF ( FRAME .GT. LBND(AXIS) ) THEN
            J = J - 1
            IF ( SPD_UAAGI( %VAL( CNF_PVAL( FPTR ) ), J, STATUS )
     :           .EQ. 0 ) THEN
               CALL SPD_CZBE( NDF, AXIS, FRAME-1, BADVAL, IMIN, IMAX,
     :                        OMIN, OMAX, SNELM, %VAL( CNF_PVAL(
     :                        WPTR)+(J-1)*SNELM*INTESZ ), STATUS )
            END IF
            CALL SPD_UAAFI( J, J, %VAL( CNF_PVAL( FPTR ) ), 1, STATUS )
         END IF


*  Display next frame.
*  ===================

      ELSE IF ( REASON .EQ. 7 ) THEN

*     Update frame number.
         FRAME = FRAME + 1
         J = FRAME - LBND(AXIS) + 1

*     Check frame number within range.
         IF ( FRAME .LT. LBND(AXIS) .OR. FRAME .GT. UBND(AXIS) ) THEN
            FRAME = FRAME - 1
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_CZBD_E03',
     :         'MOVIE: Frame is outside cube.', STATUS )
            GO TO 500
         END IF

*     If it has not been converted before, convert it.
         IF ( SPD_UAAGI( %VAL( CNF_PVAL( FPTR ) ), J, STATUS )
     :        .EQ. 0 ) THEN
            CALL SPD_CZBE( NDF, AXIS, FRAME, BADVAL, IMIN, IMAX,
     :                     OMIN, OMAX, SNELM, %VAL( CNF_PVAL(
     :                     WPTR)+(J-1)*SNELM*INTESZ ), STATUS )
         END IF

*     Flag it as converted.
         CALL SPD_UAAFI( J, J, %VAL( CNF_PVAL( FPTR ) ), 1, STATUS )

*     Display it.
         CALL PGPIXL( %VAL( CNF_PVAL( WPTR)+(J-1)*SNELM*INTESZ ),
     :                IMDIM(1), IMDIM(2), 1, IMDIM(1), 1, IMDIM(2),
     :                IMWIN(1), IMWIN(2), IMWIN(3), IMWIN(4) )

*     Report frame number.
         IF ( INFO .NE. 0 ) THEN
            CALL MSG_SETI( 'SPD_CZBD_T01', FRAME )
            CALL MSG_OUT( 'SPD_CZBD_M01',
     :         'Displayed frame # ^SPD_CZBD_T01.', STATUS )
         END IF

*     If next has not been converted before, convert it while the
*     user enjoys this display.
         IF ( FRAME .LT. UBND(AXIS) ) THEN
            J = J + 1
            IF ( SPD_UAAGI( %VAL( CNF_PVAL( FPTR ) ), J, STATUS )
     :           .EQ. 0 ) THEN
               CALL SPD_CZBE( NDF, AXIS, FRAME+1, BADVAL, IMIN, IMAX,
     :                        OMIN, OMAX, SNELM, %VAL( CNF_PVAL(
     :                        WPTR)+(J-1)*SNELM*INTESZ ), STATUS )
            END IF
            CALL SPD_UAAFI( J, J, %VAL( CNF_PVAL( FPTR ) ), 1, STATUS )
         END IF

      END IF


*  Return.
*  =======

 500  CONTINUE

      END
