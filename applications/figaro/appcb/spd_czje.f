      SUBROUTINE SPD_CZJE( MODE, NPIX, OPIX, NCOMP, NROWS, FWHM,
     :   XVAL, DATA, MXDWC, RESULT, RESVAR, STATUS )
*+
*  Name:
*     SPD_CZJE

*  Purpose:
*     Graphical dialogue for ARCLOCAT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZJE( MODE, NPIX, OPIX, NCOMP, NROWS, FWHM,
*        XVAL, DATA, MXDWC, RESULT, RESVAR, STATUS )

*  Description:
*     This routine performs the graphical dialogue interaction for the
*     feature-location application ARCLOCAT. This includes opening and
*     closing the graphics device.
*
*     The main structure of this routine is an infinite loop to wait for
*     a PGCURSE event and act upon it. The event must occur within the
*     x range of the plot box. The key strokes are case-insensitive and
*     can be:
*        R - Choose different row to work on
*        X - X-zoom 2x on cursor
*        Y - Y-zoom 2x on cursor
*        W - Unzoom to show whole row
*        N - Pan left/right by 75% of current x range
*        A - Add the feature under cursor to list (subject to line fit)
*        S - Add the cursor position as feature to list
*        D - Delete the feature nearest cursor from list
*        Q - Quit, preserving the updated list of located features
*        ? - Help
*
*     In the S option, the current cursor is taken to denote the peak
*     of a feature. The x coordinate is stored as the centre and is given
*     a variance of 0.25. The y coordinate is stored as the peak and
*     given the bad value as variance.

*  Arguments:
*     MODE = CHARACTER * ( * ) (Given)
*        'G' for Gauss line fits, 'T' for triangles. Case-sensitive.
*     NPIX = INTEGER (Given)
*        The length of XVAL, the length of rows in DATA.
*     OPIX = INTEGER (Given)
*        1.5 times FWHM, used for size of MXDWC.
*     NCOMP = INTEGER (Given)
*        Maximum number of locations, half the size of a row in RESULT
*        and RESVAR.
*     NROWS = INTEGER (Given)
*        Second dimension of DATA, RESULT, RESVAR. The number of rows
*        available.
*     FWHM = REAL (Given)
*        The guess full width at half maximum for line fits.
*     XVAL( NPIX ) = REAL (Given)
*        The x values for the profile, common to all rows.
*     DATA( NPIX, NROWS ) = REAL (Given)
*        The data values for all rows.
*     MXDWC( 4 * ( 2 * OPIX + 9 ) ) = REAL (Given and Returned)
*        This is a workspace needed for the packed local profile that is
*        passed to the line fit routine.
*     RESULT( 2 * NCOMP, NROWS ) = REAL (Given and Returned)
*        This array holds for each row pairs of numbers (centre,peak).
*        Each pair is an entry in the list of located features. Valid
*        entries are at the beginning of each row, bad-valued entries
*        are at the end of each row (if any).
*     RESVAR( 2 * NCOMP, NROWS ) = REAL (Given and Returned)
*        This array is the variance array corresponding to RESULT.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     11 Jun 1993 (hme):
*        Original version.
*     18 Jun 1993 (hme):
*        Don't let "D" choose the first bad to delete.
*     29 Jun 1993 (hme):
*        Review graphics access and paging. Use standard PGPLOT calls
*        (shorter names). Save view port as AGI picture.
*     25 Nov 1994 (hme):
*        Renamed from SPADW.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants

*  Arguments Given:
      CHARACTER * ( * ) MODE
      INTEGER NPIX
      INTEGER OPIX
      INTEGER NCOMP
      INTEGER NROWS
      REAL FWHM
      REAL XVAL( NPIX )
      REAL DATA( NPIX, NROWS )

*  Arguments Given and Returned:
      REAL MXDWC( 4 * ( 2 * OPIX + 9 ) )
      REAL RESULT( 2 * NCOMP, NROWS )
      REAL RESVAR( 2 * NCOMP, NROWS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL PLOT               ! True if plot necessary
      LOGICAL QUIT               ! True if quit chosen
      LOGICAL FITTED             ! True if line fit succeeded
      INTEGER ZONID              ! SGS zone identifier
      INTEGER I, J               ! Temporary integers
      INTEGER ROWNUM             ! Current row number
      REAL DLOW, DHIGH           ! Min/max data in row
      REAL LEFT, RIGHT, BOTTOM, TOP ! PGPLOT window
      REAL XKEY, YKEY            ! PGCURSE event position
      REAL DELTA                 ! Plotted range
      REAL LOCAT, LOCATV         ! Fitted location
      REAL STREN, STRENV         ! Fitted strength
      CHARACTER * ( 1 ) CKEY     ! PGCURSE event key

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access graphics device.
      CALL SPD_UGAA( 'DEVICE', 'WRITE', ' ', I, ZONID, STATUS )

*  Find out which row to work on.
      CALL PAR_GET0I( 'ROWNUM', ROWNUM, STATUS )
      CALL PAR_CANCL( 'ROWNUM', STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( ROWNUM .LT. 1 ) THEN
         CALL MSG_OUT( 'ARCLOCAT_M01', 'Warning: Minimum row number ' //
     :      'is 1. Working on row #1 now.', STATUS )
         ROWNUM = 1
      ELSE IF ( ROWNUM .GT. NROWS ) THEN
         CALL MSG_SETI( 'ARCLOCAT_T03', NROWS )
         CALL MSG_OUT( 'ARCLOCAT_M02', 'Warning: Maximum row number ' //
     :      'is ^ARCLOCAT_T03. Working on row #^ARCLOCAT_T03 now.',
     :      STATUS )
         ROWNUM = NROWS
      END IF

*  Set up window.
*  After finding the exact y range, add 5 % on either side.
      LEFT  = XVAL(1)
      RIGHT = XVAL(NPIX)
      CALL SPD_UAAAR( .TRUE., NPIX, DATA(1,ROWNUM),
     :   DLOW, DHIGH, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      DELTA  = DHIGH - DLOW
      BOTTOM = DLOW  - DELTA / 20.
      TOP    = DHIGH + DELTA / 20.

*  Instruct user.
      CALL MSG_OUT( 'ARCLOCAT_M03', ' ', STATUS )
      CALL MSG_OUT( 'ARCLOCAT_M04', 'When plot complete, use cursor ' //
     :   'and keyboard to choose option.', STATUS )
      CALL MSG_OUT( 'ARCLOCAT_M03', ' ', STATUS )

*  Turn plot switch on, quit switch off.
      PLOT = .TRUE.
      QUIT = .FALSE.

*  Set cursor start position to centre.
      XKEY = ( LEFT + RIGHT ) / 2.
      YKEY = ( TOP + BOTTOM ) / 2.

*  While quit item not chosen.
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( .NOT. QUIT ) THEN

*     If plot switch on, plot and turn it off.
         IF ( PLOT ) THEN
            CALL SPD_UGAC( ZONID, STATUS )
            CALL PGSWIN( LEFT, RIGHT, BOTTOM, TOP )

*        Plot (row in bin-style, centre from feature list as
*        green dashed lines).
            CALL PGBOX( 'BCTSN', 0., 0, 'BCTSN', 0., 0 )
            CALL SPD_WAAD( .FALSE., .FALSE., .FALSE., .TRUE., .FALSE.,
     :         1, 0, NPIX, XVAL, DATA(1,ROWNUM), 0., 0., 0., 0.,
     :         STATUS )
            CALL PGSLS( 2 )
            CALL PGSCI( 3 )
            I = 1
 2          CONTINUE             ! Start of 'DO WHILE' loop
            IF ( I .LT. NCOMP ) THEN
               IF ( RESULT(2*I-1,ROWNUM) .NE. VAL__BADR ) THEN
                  CALL PGMOVE( RESULT(2*I-1,ROWNUM), BOTTOM )
                  CALL PGDRAW( RESULT(2*I-1,ROWNUM), TOP )
                  I = I + 1
                  GO TO 2
               END IF
            END IF
            CALL PGSLS( 1 )
            CALL PGSCI( 1 )
            PLOT = .FALSE.
         END IF

*     Wait for PGCURSE event.
         CALL PGCURS( XKEY, YKEY, CKEY )
         CALL CHR_UCASE( CKEY )

*     If the cursor is outside the x range, ignore the event.
         IF ( XKEY .LT. LEFT .OR. XKEY .GT. RIGHT ) THEN
            CONTINUE

*     If "R" switch row chosen.
         ELSE IF ( CKEY .EQ. 'R' ) THEN

*        Find out new row number.
            CALL PAR_GET0I( 'ROWNUM', ROWNUM, STATUS )
            CALL PAR_CANCL( 'ROWNUM', STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 500
            IF ( ROWNUM .LT. 1 ) THEN
               CALL MSG_OUT( 'ARCLOCAT_M01', 'Warning: Minimum row ' //
     :            'number is 1. Working on row #1 now.', STATUS )
               ROWNUM = 1
            ELSE IF ( ROWNUM .GT. NROWS ) THEN
               CALL MSG_SETI( 'ARCLOCAT_T03', NROWS )
               CALL MSG_OUT( 'ARCLOCAT_M02', 'Warning: Maximum row ' //
     :            'number is ^ARCLOCAT_T03. Working on row ' //
     :            '#^ARCLOCAT_T03 now.', STATUS )
               ROWNUM = NROWS
            END IF

*       Set up PGPLOT window and switch plot on.
            LEFT  = XVAL(1)
            RIGHT = XVAL(NPIX)
            CALL SPD_UAAAR( .TRUE., NPIX, DATA(1,ROWNUM),
     :         DLOW, DHIGH, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 500
            DELTA  = DHIGH - DLOW
            BOTTOM = DLOW  - DELTA / 20.
            TOP    = DHIGH + DELTA / 20.
            PLOT = .TRUE.

*     Else if "X" zoom chosen.
*     The window does never exceed the actual range of XVAL.
         ELSE IF ( CKEY .EQ. 'X' ) THEN
            DELTA = ( RIGHT - LEFT ) / 2.
            LEFT  = MAX( XKEY - DELTA / 2., XVAL(1)    )
            RIGHT = MIN( LEFT + DELTA,      XVAL(NPIX) )
            PLOT = .TRUE.

*     Else if "Y" zoom chosen.
*     The window is not restricted to the actual range of DATA.
         ELSE IF ( CKEY .EQ. 'Y' ) THEN
            DELTA  = ( TOP - BOTTOM ) / 2.
            BOTTOM = YKEY   - DELTA / 2.
            TOP    = BOTTOM + DELTA
            PLOT = .TRUE.

*     Else if "W" whole row chosen, reset window and switch plot on.
         ELSE IF ( CKEY .EQ. 'W' ) THEN
            LEFT  = XVAL(1)
            RIGHT = XVAL(NPIX)
            DELTA  = DHIGH - DLOW
            BOTTOM = DLOW  - DELTA / 20.
            TOP    = DHIGH + DELTA / 20.
            PLOT = .TRUE.

*     Else if "N" pan left/right chosen, change window and switch plot on.
         ELSE IF ( CKEY .EQ. 'N' ) THEN
            DELTA = RIGHT - LEFT
            IF ( XKEY .LT. LEFT + DELTA / 2. ) THEN
               LEFT  = MAX( LEFT - 0.75 * DELTA, XVAL(1) )
               RIGHT = LEFT + DELTA
               XKEY  = LEFT + 0.25 * DELTA
            ELSE
               RIGHT = MIN( RIGHT + 0.75 * DELTA, XVAL(NPIX) )
               LEFT  = RIGHT - DELTA
               XKEY  = RIGHT - 0.25 * DELTA
            END IF
            PLOT = .TRUE.

*     Else if "A" add feature chosen.
         ELSE IF ( CKEY .EQ. 'A' ) THEN

*        If there is storage space.
            IF ( RESULT(2*NCOMP-1,ROWNUM) .EQ. VAL__BADR ) THEN

*           Find pixel nearest to cursor position.
               I = 1
 3             CONTINUE          ! Start of 'DO WHILE' loop
               IF ( I .LT. NPIX ) THEN
                  IF ( XVAL(I) .LT. XKEY ) THEN
                     I = I + 1
                     GO TO 3
                  END IF
               END IF
               IF ( I .GT. 1 ) THEN
                  IF ( ABS( XVAL(I-1) - XKEY ) .LT.
     :                 ABS( XVAL(I)   - XKEY )      ) I = I - 1
               END IF

*           Try a line fit.
               CALL SPD_WZJC( MODE, NPIX, OPIX, I, FWHM, XVAL,
     :            DATA(1,ROWNUM), MXDWC, FITTED, LOCAT, LOCATV,
     :            STREN, STRENV, STATUS )

*           If line fit succeeded.
               IF ( FITTED ) THEN

*              Insert centre and peak so that centres remain monotonic.
                  I = NCOMP
 4                CONTINUE       ! Start of 'DO WHILE' loop
                  IF ( I .GE. 1 ) THEN
                     IF ( RESULT(2*I-1,ROWNUM) .EQ. VAL__BADR ) THEN
                        I = I - 1
                        GO TO 4
                     END IF
                  END IF
 5                CONTINUE       ! Start of 'DO WHILE' loop
                  IF ( I .GE. 1 ) THEN
                     IF ( RESULT(2*I-1,ROWNUM) .GT. LOCAT ) THEN
                        RESULT(2*(I+1)-1,ROWNUM) = RESULT(2*I-1,ROWNUM)
                        RESULT(2*(I+1),  ROWNUM) = RESULT(2*I,  ROWNUM)
                        RESVAR(2*(I+1)-1,ROWNUM) = RESVAR(2*I-1,ROWNUM)
                        RESVAR(2*(I+1),  ROWNUM) = RESVAR(2*I,  ROWNUM)
                        I = I - 1
                        GO TO 5
                     END IF
                  END IF
                  RESULT(2*(I+1)-1,ROWNUM) = LOCAT
                  RESULT(2*(I+1),  ROWNUM) = STREN
                  RESVAR(2*(I+1)-1,ROWNUM) = LOCATV
                  RESVAR(2*(I+1),  ROWNUM) = STRENV

*              Plot the one new location.
                  CALL PGSLS( 2 )
                  CALL PGSCI( 3 )
                  CALL PGMOVE( LOCAT, BOTTOM )
                  CALL PGDRAW( LOCAT, TOP )
                  CALL PGSLS( 1 )
                  CALL PGSCI( 1 )

*           Else, inform user about failure.
               ELSE
                  CALL MSG_OUT( 'ARCLOCAT_M04', 'Warning: Feature ' //
     :               'location not stored. Line fit failed.', STATUS )
               END IF

*        Else, inform user about failure.
            ELSE
               CALL MSG_OUT( 'ARCLOCAT_M05', 'Warning: Feature ' //
     :            'location not stored. No more storage space.',STATUS )
            END IF

*     Else if "S" add cursor position as feature chosen.
         ELSE IF ( CKEY .EQ. 'S' ) THEN

*        If there is storage space.
            IF ( RESULT(2*NCOMP-1,ROWNUM) .EQ. VAL__BADR ) THEN

*           Insert centre and peak so that centres remain monotonic.
               I = NCOMP
 6             CONTINUE       ! Start of 'DO WHILE' loop
               IF ( I .GE. 1 ) THEN
                  IF ( RESULT(2*I-1,ROWNUM) .EQ. VAL__BADR ) THEN
                     I = I - 1
                     GO TO 6
                  END IF
               END IF
 7             CONTINUE       ! Start of 'DO WHILE' loop
               IF ( I .GE. 1 ) THEN
                  IF ( RESULT(2*I-1,ROWNUM) .GT. XKEY ) THEN
                     RESULT(2*(I+1)-1,ROWNUM) = RESULT(2*I-1,ROWNUM)
                     RESULT(2*(I+1),  ROWNUM) = RESULT(2*I,  ROWNUM)
                     RESVAR(2*(I+1)-1,ROWNUM) = RESVAR(2*I-1,ROWNUM)
                     RESVAR(2*(I+1),  ROWNUM) = RESVAR(2*I,  ROWNUM)
                     I = I - 1
                     GO TO 7
                  END IF
               END IF
               RESULT(2*(I+1)-1,ROWNUM) = XKEY
               RESULT(2*(I+1),  ROWNUM) = YKEY
               RESVAR(2*(I+1)-1,ROWNUM) = 0.25
               RESVAR(2*(I+1),  ROWNUM) = VAL__BADR

*           Plot the one new location.
               CALL PGSLS( 2 )
               CALL PGSCI( 3 )
               CALL PGMOVE( XKEY, BOTTOM )
               CALL PGDRAW( XKEY, TOP )
               CALL PGSLS( 1 )
               CALL PGSCI( 1 )

*        Else, inform user about failure.
            ELSE
               CALL MSG_OUT( 'ARCLOCAT_M05', 'Warning: Feature ' //
     :            'location not stored. No more storage space.',STATUS )
            END IF

*     Else if "D" delete feature chosen.
         ELSE IF ( CKEY .EQ. 'D' ) THEN

*        If there are any located features.
            IF ( RESULT(1,ROWNUM) .NE. VAL__BADR ) THEN

*           Find feature nearest to cursor position.
               I = 1
 8             CONTINUE          ! Start of 'DO WHILE' loop
               IF ( I .LT. NCOMP ) THEN
                  IF ( RESULT(2*I+1,ROWNUM) .NE. VAL__BADR .AND.
     :                 RESULT(2*I-1,ROWNUM) .LT. XKEY ) THEN
                     I = I + 1
                     GO TO 8
                  END IF
               END IF
               IF ( I .GT. 1 ) THEN
                  IF ( ABS( RESULT(2*I-3,ROWNUM) - XKEY ) .LT.
     :                 ABS( RESULT(2*I-1,ROWNUM) - XKEY ) ) I = I - 1
               END IF

*           If the feature is in the plot.
               IF ( RESULT(2*I-1,ROWNUM) .GE. LEFT .AND.
     :              RESULT(2*I-1,ROWNUM) .LE. RIGHT      ) THEN

*              Over-plot the one location.
                  CALL PGSCI( 0 )
                  CALL PGMOVE( RESULT(2*I-1,ROWNUM), BOTTOM )
                  CALL PGDRAW( RESULT(2*I-1,ROWNUM), TOP )
                  CALL PGSCI( 1 )

*              Remove feature from list.
                  DO 9 J = I, NCOMP-1
                     RESULT(2*J-1,ROWNUM) = RESULT(2*(J+1)-1,ROWNUM)
                     RESULT(2*J,  ROWNUM) = RESULT(2*(J+1),  ROWNUM)
                     RESVAR(2*J-1,ROWNUM) = RESVAR(2*(J+1)-1,ROWNUM)
                     RESVAR(2*J,  ROWNUM) = RESVAR(2*(J+1),  ROWNUM)
 9                CONTINUE
                  RESULT(2*NCOMP-1,ROWNUM) = VAL__BADR
                  RESULT(2*NCOMP,  ROWNUM) = VAL__BADR
                  RESVAR(2*NCOMP-1,ROWNUM) = VAL__BADR
                  RESVAR(2*NCOMP,  ROWNUM) = VAL__BADR

*           Else, warn user.
               ELSE
                  CALL MSG_OUT( 'ARCLOCAT_M06', 'Warning: Nearest ' //
     :               'feature is beyond plot. Not deleted.', STATUS )
               END IF

*        Else, warn user.
            ELSE
               CALL MSG_OUT( 'ARCLOCAT_M07', 'Warning: No feature ' //
     :            'locations known. None can be deleted.', STATUS )
            END IF

*     Else if "Q" quit chosen.
         ELSE IF ( CKEY .EQ. 'Q' ) THEN
            QUIT = .TRUE.

*     Else (unrecognised key or h or ?).
         ELSE

*        Issue help messages.
            CALL MSG_OUT( 'ARCLOCAT_M03', ' ', STATUS )
            CALL MSG_OUT( 'ARCLOCAT_M08', ' R - Choose different row',
     :         STATUS )
            CALL MSG_OUT( 'ARCLOCAT_M09', ' X - X-zoom 2x on cursor',
     :         STATUS )
            CALL MSG_OUT( 'ARCLOCAT_M10', ' Y - Y-zoom 2x on cursor',
     :         STATUS )
            CALL MSG_OUT( 'ARCLOCAT_M11', ' W - Unzoom to show ' //
     :         'whole row', STATUS )
            CALL MSG_OUT( 'ARCLOCAT_M12', ' N - Pan left/right by ' //
     :         '75 % of current x range', STATUS )
            CALL MSG_OUT( 'ARCLOCAT_M13', ' A - Add the feature ' //
     :         'under cursor to list', STATUS )
            CALL MSG_OUT( 'ARCLOCAT_M14', ' S - Add the cursor ' //
     :         'position as feature to list', STATUS )
            CALL MSG_OUT( 'ARCLOCAT_M10', ' D - Delete the feature ' //
     :         'nearest cursor from list', STATUS )
            CALL MSG_OUT( 'ARCLOCAT_M15', ' Q - Quit', STATUS )
            CALL MSG_OUT( 'ARCLOCAT_M16', ' ? - Help', STATUS )
            CALL MSG_OUT( 'ARCLOCAT_M03', ' ', STATUS )
         END IF

*     This action is complete.
         IF ( STATUS .NE. SAI__OK ) GO TO 500
         GO TO 1
      END IF

*  Before leaving, save the current view port as an AGI picture.
      CALL SPD_UGAD( 'DATA', 'SPECDRE_ARCLOCAT', I, STATUS )

*  Return.
 500  CONTINUE
      CALL SPD_UGAB( 'DEVICE', .FALSE., STATUS )
      END
