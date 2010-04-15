      SUBROUTINE SPD_CZMD( ORDER, NFDB, NCOMP, NPIX, NROWS, FDB,
     :   LOCATS, IDENTS, WEIGHT, RETVAL, A3, XVAL, RESULT, RESVAR,
     :   FINSHD, STATUS )
*+
*  Name:
*     SPD_CZMD

*  Purpose:
*     Graphical dialogue for ARCDISP.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZMD( ORDER, NFDB, NCOMP, NPIX, NROWS, FDB, LOCATS, IDENTS,
*        WEIGHT, WORK1, WORK2, XVAL, RESULT, RESVAR, FINSHD, STATUS )

*  Description:
*     This routine performs the graphical dialogue interaction for the
*     dispersion-curve application ARCDISP. This includes opening and
*     closing the graphics device.
*
*     The main structure of this routine is an infinite loop to wait for
*     a PGCURSE event and act upon it. The event must occur within the
*     x range of the plot box. The key strokes are case-insensitive and
*     can be:
*        R - Switch to next row, accepting the current fit for this row
*        X - X-zoom 2x on cursor
*        Y - Y-zoom 2x on cursor
*        W - Unzoom to show whole row
*        N - Pan by 75% of current plot range
*        A - Add ID for location nearest to cursor (from FDB)
*        S - Set ID for location nearest to cursor (from cursor y pos.)
*        D - Delete ID for location nearest to cursor
*        Q - Quit (preserves updated IDs, discards applied fits for all
*            rows)
*        ? - Help
*
*     "D" will delete the ID for the nearest located feature, not the
*     nearest identified feature. So it may do nothing at all, namely if
*     the nearest located feature is not identified anyway.

*  Arguments:
*     ORDER = INTEGER (Given)
*        The polynomial order to be used for the fitted dispersion
*        curves. A lower order is used if too few identifications are
*        available for a row. The order must be at least 1.
*     NFDB = INTEGER (Given)
*        The number of features listed in the feature data base. The
*        size of FDB.
*     NCOMP = INTEGER (Given)
*        Maximum number of locations, half the size of a row in RESULT
*        and RESVAR.
*     NPIX = INTEGER (Given)
*        The length of rows in XVAL.
*     NROWS = INTEGER (Given)
*        Second dimension of XVAL, RESULT, RESVAR. The number of rows
*        available.
*     FDB( NFDB ) = REAL (Given)
*        The list of possible identifications from a feature data base.
*        Only elements that lie within the WRANGE plot range are used.
*     LOCATS( NCOMP ) = DOUBLE PRECISION (Given and Returned)
*        A workspace used for locations of identified features.
*     IDENTS( NCOMP ) = DOUBLE PRECISION (Given and Returned)
*        A workspace used for identifications.
*     WEIGHT( NCOMP ) = DOUBLE PRECISION (Given and Returned)
*        A workspace used for reciprocal location
*        variances. These are used as statistical weights in the
*        polynomial fit.
*     WORK1( NCOMP ) = DOUBLE PRECISION (Given and Returned)
*        A workspace used by polynomial fit
*     WORK2( 3*NCOMP + 3*(ORDER+1) ) = DOUBLE PRECISION (Given and Returned)
*        A workspace used by polynomial fit and evalutation.
*     XVAL( NPIX, NROWS ) = REAL (Given and Returned)
*        The x values for each row. On entry these must be NDF pixel
*        coordinates. On regular exit they are laboratory values, that
*        is, in each row the appropriate dispersion curve is applied.
*     RESULT( 2 * NCOMP, NROWS ) = REAL (Given and Returned)
*        This array holds for each row pairs of numbers (centre,
*        laboratory value). Each pair is an entry in the list of located
*        features. Located features are at the beginning of each row,
*        bad-valued locations may be at the end of each row. Each
*        located feature may or may not be identified (laboratory value
*        good or bad).
*     RESVAR( 2 * NCOMP, NROWS ) = REAL (Given and Returned)
*        This array is the variance array corresponding to RESULT.
*        Location variances must be good and positive, identification
*        variances should be zero.
*     FINSHD = LOGICAL (Returned)
*        If true, the routine exited normally after all rows had been
*        worked on. If false, the routine quit before all rows had been
*        worked on; either an error occured or the user chose the Quit
*        option.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set if
*        -  the given ORDER is less than 1,
*        -  a call to the polynomial fit routine returned a bad status,
*        -  a call to the polynomial evaluation routine returned a bad
*           status,
*        -  the user moves to next row while a fit of the specified
*           order could not be achieved for the old row.

*  Notes:
*     This routine may behave in a strange way if there are good
*     locations with bad or zero variance.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     18 Jun 1993 (hme):
*        Original version.
*     21 Jun 1993 (hme):
*        Change to show full lab value instead of only non-linear
*        component. Draw lines for IDs as well as locations.
*     22 Jun 1993 (hme):
*        Plot two panels, one for full fit, one for non-linear part.
*     29 Jun 1993 (hme):
*        Review graphics access and paging. Use standard PGPLOT calls
*        (shorter names). Save view ports as AGI pictures.
*        Add FNSHD returned argument to signal abort or not, Quit no
*        longer causes bad status.
*     09 Jul 1993 (hme):
*        Fix bug whereby FDB(0) might be looked at and IDMIN might
*        remain 0.
*     25 Jan 1995 (hme):
*        Renamed from SPADZ.
*     20 Nov 1995 (hme):
*        Use PDA instead of NAG.
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
      INTEGER ORDER
      INTEGER NFDB
      INTEGER NCOMP
      INTEGER NPIX
      INTEGER NROWS
      REAL FDB( NFDB )

*  Arguments Given and Returned:
      DOUBLE PRECISION LOCATS( NCOMP )
      DOUBLE PRECISION IDENTS( NCOMP )
      DOUBLE PRECISION WEIGHT( NCOMP )
      DOUBLE PRECISION RETVAL( NCOMP )
      DOUBLE PRECISION A3( 3 * NCOMP + 3 * ( ORDER + 1 ) )
      REAL XVAL( NPIX, NROWS )
      REAL RESULT( 2 * NCOMP, NROWS )
      REAL RESVAR( 2 * NCOMP, NROWS )

*  Arguments Returned:
      LOGICAL FINSHD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXPAR             ! Maximum polynomial order plus 1
      PARAMETER ( MAXPAR = 8 )
      INTEGER PLTRES             ! Number of points for curve plot
      PARAMETER ( PLTRES = 501 )
      REAL VPL, VPR              ! Horizontal extent of both view ports
      PARAMETER ( VPL  = 0.1,  VPR  = 0.9  )
      REAL VPB1, VPT1            ! Vertical extent of lower view port
      PARAMETER ( VPB1 = 0.05, VPT1 = 0.75 )
      REAL VPB2, VPT2            ! Vertical extent of upper view port
      PARAMETER ( VPB2 = 0.75, VPT2 = 0.95 )

*  Local Variables:
      REAL WRANGE( 2 )
      LOGICAL PLOT               ! True if plot necessary
      LOGICAL QUIT               ! True if quit chosen
      LOGICAL FIT                ! True if fit necessary
      LOGICAL FITOK              ! True if acceptable fit was achieved
      INTEGER PICID              ! Identifier for current AGI picture
      INTEGER ZONID              ! SGS zone identifier
      INTEGER I, J, K            ! Temporary integers
      INTEGER IFAIL1, IFAIL2     ! PDA_DPOLFT stati
      INTEGER ROWNUM             ! Current row number
      INTEGER NIDENT             ! Number of valid IDs
      INTEGER IDMIN, IDMAX       ! Indices into FDB corresp. to WRANGE
      INTEGER NDEG               ! Highest degree fitted by PDA_DPOLFT
      REAL TEMP                  ! Temporary number
      REAL DELTA                 ! Temporary range
      REAL XKEY, YKEY            ! PGCURSE event position
      REAL LEFT, RIGHT           ! Plot window extent
      REAL BOTTOM, TOP           ! Plot window extent
      REAL DLOW, DHIGH           ! Plot window y extent 2nd panel
      REAL FITL, FITR            ! Left and right fit curve values
      REAL FITX( PLTRES )        ! Curve plot x array
      REAL FITY( PLTRES )        ! Curve plot y array
      DOUBLE PRECISION EPS       ! Control / r.m.s. for PDA_DPOLFT
      DOUBLE PRECISION DTEMP1, DTEMP2 ! Temporary numbers
      CHARACTER * ( 1 ) CKEY     ! PGCURSE event key

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default return value.
      FINSHD = .FALSE.

*  Check order.
      IF ( ORDER .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCDISP_E11', 'ARCDISP: Error obtaining ' //
     :      'polynomial order. Must be at least 1.', STATUS )
         RETURN
      END IF

*  Access graphics device.
      CALL SPD_UGAA( 'DEVICE', 'WRITE', ' ', PICID, ZONID, STATUS )

*  Initialise for first row.
      ROWNUM = 1
      FIT   = .TRUE.
      PLOT  = .TRUE.
      QUIT  = .FALSE.
      FITOK = .FALSE.

*  The horizontal extent of the plot is determined by the range of old
*  axis data. For the purpose of drawing the fit, an array of x values
*  covering the plot (only) is created.
      LEFT  = XVAL(1,   ROWNUM)
      RIGHT = XVAL(NPIX,ROWNUM)
      XKEY  = ( LEFT + RIGHT ) / 2.
      CALL SPD_UAAJR( LEFT, RIGHT, PLTRES, FITX, STATUS )

*  Instruct user.
      CALL MSG_OUT( 'ARCDISP_M01', ' ', STATUS )
      CALL MSG_OUT( 'ARCDISP_M02', 'When plot complete, use cursor ' //
     :   'and keyboard to choose option.', STATUS )
      CALL MSG_OUT( 'ARCDISP_M01', ' ', STATUS )

*  Get WRANGE for current row.
      CALL MSG_SETI( 'ARCDISP_T03', ROWNUM )
      CALL MSG_OUT( 'ARCDISP_M03', 'Starting row #^ARCDISP_T03...',
     :   STATUS )
      CALL PAR_GET1R( 'WRANGE', 2, WRANGE, I, STATUS )
      CALL PAR_CANCL( 'WRANGE', STATUS )
      IF ( I .NE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCDISP_E12', 'ARCDISP: Error reading plot ' //
     :      'range. Need two numbers.', STATUS )
         GO TO 500
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      DELTA = WRANGE(1)
      WRANGE(1) = MIN( WRANGE(1), WRANGE(2) )
      WRANGE(2) = MAX( WRANGE(2), DELTA )

*  The vertical extent of the plot.
      BOTTOM = WRANGE(1)
      TOP    = WRANGE(2)
      YKEY   = ( BOTTOM + TOP ) / 2.

*  Find index range into FDB corresponding to plot range.
      IDMIN = 1
 1    CONTINUE                     ! Start of 'DO WHILE' loop
      IF ( IDMIN .LE. NFDB ) THEN
         IF ( FDB(IDMIN) .LT. WRANGE(1) ) THEN
            IDMIN = IDMIN + 1
            GO TO 1
         END IF
      END IF
      IDMAX = IDMIN
 2    CONTINUE                     ! Start of 'DO WHILE' loop
      IF ( IDMAX .LT. NFDB .AND. FDB(IDMAX+1) .LT. WRANGE(2) ) THEN
         IDMAX = IDMAX + 1
         GO TO 2
      END IF
      IF ( IDMAX .GT. NFDB ) THEN
         IDMIN = 1
         IDMAX = 0
      END IF

*  While not quit or last row.
 3    CONTINUE                     ! Start of 'DO WHILE' loop
      IF ( .NOT. QUIT .AND. ROWNUM .LE. NROWS ) THEN

*     If fit necessary.
         IF ( FIT ) THEN
            FITOK = .FALSE.

*        Sort and convert to double the locations, IDs, weights.
            I = 1
            NIDENT = 0
 4          CONTINUE               ! Start of 'DO WHILE' loop
            IF ( I .LE. NCOMP ) THEN
               IF ( RESULT(2*I-1,ROWNUM) .NE. VAL__BADR ) THEN
                  IF ( RESULT(2*I,  ROWNUM) .NE. VAL__BADR .AND.
     :                 RESVAR(2*I-1,ROWNUM) .NE. VAL__BADR .AND.
     :                 RESVAR(2*I-1,ROWNUM) .GT. 0. ) THEN
                     NIDENT = NIDENT + 1
                     LOCATS(NIDENT) = DBLE( RESULT(2*I-1,ROWNUM) )
                     IDENTS(NIDENT) = DBLE( RESULT(2*I  ,ROWNUM) )
                     WEIGHT(NIDENT) = 1D0 / DBLE( RESVAR(2*I-1,ROWNUM) )
                  END IF
                  I = I + 1
                  GO TO 4
               END IF
            END IF

*        If there are sufficient points to fit chosen order.
            IF ( NIDENT .GE. ORDER+1 ) THEN

*           Perform the fit.
               IFAIL2 = 0
               EPS = 0D0
               CALL PDA_DPOLFT( NIDENT, LOCATS, IDENTS, WEIGHT, ORDER,
     :            NDEG, EPS, RETVAL, IFAIL1, A3, IFAIL2 )
               IF (  NDEG .NE. ORDER .OR.
     :               IFAIL1 .NE. 1 .OR. IFAIL2 .NE. 0 ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'ARCDISP_E13', 'ARCDISP: Error ' //
     :               'fitting dispersion curve.', STATUS )
                  GO TO 500
               ELSE
                  FITOK = .TRUE.
               END IF

*        Else if there are at least two points.
            ELSE IF ( NIDENT .GE. 2 ) THEN

*           Perform the fit with highest order possible.
               IFAIL2 = 0
               EPS = 0D0
               CALL PDA_DPOLFT( NIDENT, LOCATS, IDENTS, WEIGHT,
     :            NIDENT-1, NDEG, EPS, RETVAL, IFAIL1, A3, IFAIL2 )
               IF (  NDEG .NE. NIDENT-1 .OR.
     :               IFAIL1 .NE. 1 .OR. IFAIL2 .NE. 0 ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'ARCDISP_E14', 'ARCDISP: Error ' //
     :               'fitting simplified dispersion curve.', STATUS )
                  GO TO 500
               END IF

            END IF

*        Turn plot switch on, fit switch off.
            PLOT = .TRUE.
            FIT  = .FALSE.
         END IF

*     If plot necessary.
         IF ( PLOT ) THEN
            CALL SPD_UGAC( ZONID, STATUS )

*        Set up plot window.
            CALL PGSVP( VPL, VPR, VPB1, VPT1 )
            CALL PGSWIN( LEFT, RIGHT, BOTTOM, TOP )

*        Plot the box.
            CALL PGBOX( 'BCTSN', 0., 0, 'BCTSN', 0., 0 )

*        Plot the identifications.
            CALL PGSCI( 3 )
            DO 5 I = IDMIN, IDMAX
               CALL PGMOVE( LEFT,  FDB(I) )
               CALL PGDRAW( RIGHT, FDB(I) )
 5          CONTINUE

*        Plot the locations.
            CALL PGSCH( 2. )
            I = 1
 6          CONTINUE            ! Start of 'DO WHILE' loop
            IF ( I .LE. NCOMP ) THEN
               IF ( RESULT(2*I-1,ROWNUM) .NE. VAL__BADR ) THEN
                  IF ( RESULT(2*I,ROWNUM) .EQ. VAL__BADR ) THEN
                     CALL PGMOVE( RESULT(2*I-1,ROWNUM), BOTTOM )
                     CALL PGDRAW( RESULT(2*I-1,ROWNUM), TOP )
                  ELSE
                     CALL PGPT( 1,
     :                  RESULT(2*I-1,ROWNUM), RESULT(2*I,ROWNUM), 5 )
                  END IF
               END IF
               I = I + 1
               GO TO 6
            END IF
            CALL PGSCH( 1. )
            CALL PGSCI( 1  )

*        Plot the fit curve in the main panel and it non-linear part in
*        an upper panel. The upper panel will also display identified
*        locations.
            IF ( NIDENT .GE. 2 ) THEN

*           Given the coefficients and the FITX array, generate the FITY
*           array and plot the fit curve.
               IFAIL2 = 0
               DO 7 K = 1, PLTRES
                  CALL PDA_DP1VLU( NDEG, 0, DBLE(FITX(K)),
     :               DTEMP1, DTEMP2, A3, IFAIL2 )
                  IF ( IFAIL2 .NE. 0 ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP( 'ARCDISP_E19', 'ARCDISP: Error ' //
     :                  'evaluating dispersion curve.', STATUS )
                     GO TO 500
                  END IF
                  FITY(K) = SNGL(DTEMP1)
 7             CONTINUE
               CALL SPD_WAAD(
     :            .FALSE., .FALSE., .TRUE., .FALSE., .FALSE.,
     :            1, 0, PLTRES, FITX, FITY, 0., 0., 0., 0., STATUS )

*           The first and last FITX and FITY define a linear function,
*           subtract that from FITY, in situ.
               FITL  = FITY(1)
               FITR  = FITY(PLTRES)
               DELTA = ( FITR - FITL ) / ( RIGHT - LEFT )
               DO 8 I = 1, PLTRES
                  FITY(I) = FITY(I) - FITL - ( FITX(I) - LEFT ) * DELTA
 8             CONTINUE

*           Given FITY, find the plot y range.
               CALL SPD_UAAAR( .FALSE., PLTRES, FITY,
     :            DLOW, DHIGH, STATUS )

*           Given LEFT and RIGHT work out the y range so that FITY can be
*           plotted and all IDs with location inside [LEFT,RIGHT] can be
*           plotted. We know already that FITY would need [DLOW,DHIGH]. We
*           must now go through RESULT; for each identified location that
*           is within [LEFT,RIGHT] we subtract from the identification the
*           linear function.
               DO 9 I = 1, NIDENT
                  TEMP = SNGL(IDENTS(I)) - FITL
     :               - ( SNGL(LOCATS(I)) - LEFT ) * DELTA
                  DLOW  = MIN( DLOW,  TEMP )
                  DHIGH = MAX( DHIGH, TEMP )
 9             CONTINUE

*           Add 5% on top and bottom.
               DELTA = DHIGH - DLOW
               DLOW  = DLOW  - DELTA / 20.
               DHIGH = DHIGH + DELTA / 20.

*           Switch to upper panel. Set up plot window.
               CALL PGSVP( VPL, VPR, VPB2, VPT2 )
               CALL PGSWIN( LEFT, RIGHT, DLOW, DHIGH )

*           Plot the box.
               CALL PGBOX( 'BCTS', 0., 0, 'BCTSM', 0., 0 )

*           Plot the fit curve.
               CALL SPD_WAAD(
     :            .FALSE., .FALSE., .TRUE., .FALSE., .FALSE.,
     :            1, 0, PLTRES, FITX, FITY, 0., 0., 0., 0., STATUS )

*           Plot the identified locations.
               DELTA = ( FITR - FITL ) / ( RIGHT - LEFT )
               CALL PGSCH( 2. )
               DO 10 I = 1, NIDENT
                  TEMP = SNGL(IDENTS(I)) - FITL
     :               - ( SNGL(LOCATS(I)) - LEFT ) * DELTA
                  CALL PGPT( 1, SNGL(LOCATS(I)), TEMP, 5 )
 10            CONTINUE
               CALL PGSCH( 1. )

*           Restore view port to lower panel, restore window before
*           PGCURSE.
               CALL PGSVP( VPL, VPR, VPB1, VPT1 )
               CALL PGSWIN( LEFT, RIGHT, BOTTOM, TOP )
            END IF

*        Turn plot off.
            PLOT = .FALSE.
         END IF

*     Wait for PGCURSE event.
         CALL PGCURS( XKEY, YKEY, CKEY )
         CALL CHR_UCASE( CKEY )

*     If the cursor is outside the x range, ignore the event.
         IF ( XKEY .LT. LEFT .OR. XKEY .GT. RIGHT ) THEN
            CONTINUE

*     Else if "R" switch to next row.
         ELSE IF ( CKEY .EQ. 'R' ) THEN

*        If the fit succeeded with the correct order.
            IF ( FITOK ) THEN

*           Apply the current fit (in situ) to the x values for the
*           current row.
               IFAIL2 = 0
               DO 11 K = 1, NPIX
                  CALL PDA_DP1VLU( NDEG, 0, DBLE(XVAL(K,ROWNUM)),
     :               DTEMP1, DTEMP2, A3, IFAIL2 )
                  IF ( IFAIL2 .NE. 0 ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP( 'ARCDISP_E19', 'ARCDISP: Error ' //
     :                  'evaluating dispersion curve.', STATUS )
                     GO TO 500
                  END IF
                  XVAL(K,ROWNUM) = SNGL(DTEMP1)
 11            CONTINUE

*           Increment row number.
               ROWNUM = ROWNUM + 1

*           If last row complete, exit without status and signal that
*           normal end.
               IF ( ROWNUM .GT. NROWS ) THEN
                  QUIT   = .TRUE.
                  FINSHD = .TRUE.

*           Else (still a row to work on), re-initialise for row.
               ELSE
                  FIT   = .TRUE.
                  PLOT  = .TRUE.
                  FITOK = .FALSE.
                  LEFT  = XVAL(1,   ROWNUM)
                  RIGHT = XVAL(NPIX,ROWNUM)
                  XKEY  = ( LEFT + RIGHT ) / 2.
                  CALL SPD_UAAJR( LEFT, RIGHT, PLTRES, FITX, STATUS )

*              Get WRANGE for current row.
                  CALL MSG_SETI( 'ARCDISP_T03', ROWNUM )
                  CALL MSG_OUT( 'ARCDISP_M03', 'Starting row ' //
     :               '#^ARCDISP_T03...', STATUS )
                  CALL PAR_GET1R( 'WRANGE', 2, WRANGE, I, STATUS )
                  CALL PAR_CANCL( 'WRANGE', STATUS )
                  IF ( I .NE. 2 ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP( 'ARCDISP_E15', 'ARCDISP: Error ' //
     :                 'reading plot range. Need two numbers.', STATUS )
                     GO TO 500
                  END IF
                  IF ( STATUS .NE. SAI__OK ) GO TO 500
                  DELTA = WRANGE(1)
                  WRANGE(1) = MIN( WRANGE(1), WRANGE(2) )
                  WRANGE(2) = MAX( WRANGE(2), DELTA )

*              The vertical extent of the plot.
                  BOTTOM = WRANGE(1)
                  TOP    = WRANGE(2)
                  YKEY   = ( BOTTOM + TOP ) / 2.


*              Find index range into FDB corresponding to plot range.
                  IDMIN = 1
 12               CONTINUE      ! Start of 'DO WHILE' loop
                  IF ( IDMIN .LE. NFDB ) THEN
                     IF ( FDB(IDMIN) .LT. WRANGE(1) ) THEN
                        IDMIN = IDMIN + 1
                        GO TO 12
                     END IF
                  END IF
                  IDMAX = IDMIN
 13               CONTINUE      ! Start of 'DO WHILE' loop
                  IF ( IDMAX .LT. NFDB .AND.
     :                  FDB(IDMAX+1) .LT. WRANGE(2) ) THEN
                     IDMAX = IDMAX + 1
                     GO TO 13
                  END IF
                  IF ( IDMAX .GT. NFDB ) THEN
                     IDMIN = 1
                     IDMAX = 0
                  END IF
               END IF

*        Else (too few identifications in this row).
            ELSE
               STATUS = SAI__ERROR
               CALL ERR_REP( 'ARCDISP_E16', 'ARCDISP: Error: Could ' //
     :            'not fit dispersion curve. Too few ' //
     :            'identified features in a row.', STATUS )
               GO TO 500
            END IF

*     Else if "X" zoom chosen.
*     The window does never exceed the actual range of XVAL.
         ELSE IF ( CKEY .EQ. 'X' ) THEN
            DELTA = ( RIGHT - LEFT ) / 2.
            LEFT  = MAX( XKEY - DELTA / 2., XVAL(1,   ROWNUM) )
            RIGHT = MIN( LEFT + DELTA,      XVAL(NPIX,ROWNUM) )
            CALL SPD_UAAJR( LEFT, RIGHT, PLTRES, FITX, STATUS )
            PLOT = .TRUE.

*     Else if "Y" zoom chosen.
*     The window does never exceed the actual range.
         ELSE IF ( CKEY .EQ. 'Y' ) THEN
            DELTA  = ( TOP - BOTTOM ) / 2.
            BOTTOM = MAX( YKEY   - DELTA / 2., WRANGE(1) )
            TOP    = MIN( BOTTOM + DELTA,      WRANGE(2) )
            PLOT = .TRUE.

*     Else if "W" whole row chosen, reset window and switch plot on.
         ELSE IF ( CKEY .EQ. 'W' ) THEN
            LEFT  = XVAL(1,   ROWNUM)
            RIGHT = XVAL(NPIX,ROWNUM)
            CALL SPD_UAAJR( LEFT, RIGHT, PLTRES, FITX, STATUS )
            BOTTOM = WRANGE(1)
            TOP    = WRANGE(2)
            PLOT   = .TRUE.

*     Else if "N" pan chosen, change window and switch plot on.
         ELSE IF ( CKEY .EQ. 'N' ) THEN
            DELTA = RIGHT - LEFT
            IF ( XKEY .LE. LEFT + DELTA / 4. ) THEN
               LEFT  = MAX( LEFT - 0.75 * DELTA, XVAL(1,ROWNUM) )
               RIGHT = LEFT + DELTA
            ELSE IF ( XKEY .GE. RIGHT - DELTA / 4. ) THEN
               RIGHT = MIN( RIGHT + 0.75 * DELTA, XVAL(NPIX,ROWNUM) )
               LEFT  = RIGHT - DELTA
            END IF
            CALL SPD_UAAJR( LEFT, RIGHT, PLTRES, FITX, STATUS )
            DELTA = TOP - BOTTOM
            IF ( YKEY .LE. BOTTOM + DELTA / 4. ) THEN
               BOTTOM = MAX( BOTTOM - 0.75 * DELTA, WRANGE(1) )
               TOP    = BOTTOM + DELTA
            ELSE IF ( YKEY .GE. TOP - DELTA / 4. ) THEN
               TOP    = MIN( TOP + 0.75 * DELTA, WRANGE(2) )
               BOTTOM = TOP - DELTA
            END IF
            PLOT = .TRUE.

*     Else if "A" add ID from FDB.
         ELSE IF ( CKEY .EQ. 'A' ) THEN

*        Find the location nearest to cursor.
            I = 1
 14         CONTINUE            ! Start of 'DO WHILE' loop
            IF ( I .LT. NCOMP ) THEN
               IF ( RESULT(2*I+1,ROWNUM) .NE. VAL__BADR .AND.
     :              RESULT(2*I-1,ROWNUM) .LT. XKEY ) THEN
                  I = I + 1
                  GO TO 14
               END IF
            END IF
            IF ( I .GT. 1 ) THEN
               IF ( ABS( RESULT(2*I-3,ROWNUM) - XKEY ) .LT.
     :              ABS( RESULT(2*I-1,ROWNUM) - XKEY )      ) I = I - 1
            END IF

*        If the location is already identified, warn user.
            IF ( RESULT(2*I,ROWNUM) .NE. VAL__BADR ) THEN
               CALL MSG_OUT( 'ARCDISP_M04', 'Warning: This feature ' //
     :            'is already identified. No change made.', STATUS )

*        Else (location unidentified so far).
            ELSE

*           Find FDB entry nearest to cursor.
               J = IDMIN
 15            CONTINUE         ! Start of 'DO WHILE' loop
               IF ( J .LT. IDMAX ) THEN
                  IF ( FDB(J) .LT. YKEY ) THEN
                     J = J + 1
                     GO TO 15
                  END IF
               END IF
               IF ( J .GT. 1 ) THEN
                  IF ( ABS( FDB(J-1) - YKEY ) .LT.
     :                 ABS( FDB(J)   - YKEY )      ) J = J - 1
               END IF

*           Insert identification.
               RESULT(2*I,ROWNUM) = FDB(J)

*           Turn fit on.
               FIT = .TRUE.
            END IF

*     Else if "S" add ID from keyboard.
         ELSE IF ( CKEY .EQ. 'S' ) THEN

*        Find the location nearest to cursor.
            I = 1
 16         CONTINUE            ! Start of 'DO WHILE' loop
            IF ( I .LT. NCOMP ) THEN
               IF ( RESULT(2*I+1,ROWNUM) .NE. VAL__BADR .AND.
     :              RESULT(2*I-1,ROWNUM) .LT. XKEY ) THEN
                  I = I + 1
                  GO TO 16
               END IF
            END IF
            IF ( I .GT. 1 ) THEN
               IF ( ABS( RESULT(2*I-3,ROWNUM) - XKEY ) .LT.
     :              ABS( RESULT(2*I-1,ROWNUM) - XKEY )      ) I = I - 1
            END IF

*        If the location is already identified, warn user.
            IF ( RESULT(2*I,ROWNUM) .NE. VAL__BADR ) THEN
               CALL MSG_OUT( 'ARCDISP_M04', 'Warning: This feature ' //
     :            'is already identified. No change made.', STATUS )

*        Else (location unidentified so far).
            ELSE

*           Insert identification.
               RESULT(2*I,ROWNUM) = YKEY

*           Turn fit on.
               FIT = .TRUE.
            END IF

*     Else if "D" delete ID.
         ELSE IF ( CKEY .EQ. 'D' ) THEN

*        Find the feature nearest to cursor.
            I = 1
 17         CONTINUE            ! Start of 'DO WHILE' loop
            IF ( I .LT. NCOMP ) THEN
               IF ( RESULT(2*I+1,ROWNUM) .NE. VAL__BADR .AND.
     :              RESULT(2*I-1,ROWNUM) .LT. XKEY ) THEN
                  I = I + 1
                  GO TO 17
               END IF
            END IF
            IF ( I .GT. 1 ) THEN
               IF ( ABS( RESULT(2*I-3,ROWNUM) - XKEY ) .LT.
     :              ABS( RESULT(2*I-1,ROWNUM) - XKEY )      ) I = I - 1
            END IF

*        If the feature is not identified, warn user.
            IF ( RESULT(2*I,ROWNUM) .EQ. VAL__BADR ) THEN
               CALL MSG_OUT( 'ARCDISP_M05', 'Warning: Nearest ' //
     :            'feature is not identified. Nothing to delete.',
     :            STATUS )

*        Else if the feature is in the plot set ID bad and turn fit on.
            ELSE IF ( RESULT(2*I-1,ROWNUM) .GE. LEFT  .AND.
     :                RESULT(2*I-1,ROWNUM) .LE. RIGHT       ) THEN
               RESULT(2*I,ROWNUM) = VAL__BADR
               FIT = .TRUE.

*        Else (feature is beyond plot), warn user.
            ELSE
               CALL MSG_OUT( 'ARCDISP_M06', 'Warning: Nearest ' //
     :            'feature is beyond plot. ID not deleted.', STATUS )
            END IF

*     Else if "Q" quit chosen, return without signalling normal end.
         ELSE IF ( CKEY .EQ. 'Q' ) THEN
            QUIT = .TRUE.

*     Else (unrecognised key or h or ?).
         ELSE

*        Issue help messages.
            CALL MSG_OUT( 'ARCDISP_M01', ' ', STATUS )
            CALL MSG_OUT( 'ARCDISP_M07', ' R - Go to next row',
     :         STATUS )
            CALL MSG_OUT( 'ARCDISP_M08', ' X - X-zoom 2x on cursor',
     :         STATUS )
            CALL MSG_OUT( 'ARCDISP_M09', ' Y - Y-zoom 2x on cursor',
     :         STATUS )
            CALL MSG_OUT( 'ARCDISP_M10', ' W - Unzoom to show ' //
     :         'whole row', STATUS )
            CALL MSG_OUT( 'ARCDISP_M11', ' N - Pan by ' //
     :         '75 % of current plot range', STATUS )
            CALL MSG_OUT( 'ARCDISP_M12', ' A - Add ID for feature ' //
     :         'nearest to cursor from FDB', STATUS )
            CALL MSG_OUT( 'ARCDISP_M13', ' S - Set ID for feature ' //
     :         'nearest to cursor using cursor y position', STATUS )
            CALL MSG_OUT( 'ARCDISP_M14', ' D - Delete ID for ' //
     :         'feature nearest to cursor', STATUS )
            CALL MSG_OUT( 'ARCDISP_M15', ' Q - Quit (retains ' //
     :         'updated IDs, discards applied fits for all rows)',
     :         STATUS )
            CALL MSG_OUT( 'ARCDISP_M16', ' ? - Help', STATUS )
            CALL MSG_OUT( 'ARCDISP_M01', ' ', STATUS )
         END IF

*     This action is complete.
         IF ( STATUS .NE. SAI__OK ) GO TO 500
         GO TO 3
      END IF

*  Before leaving, save the two view ports as AGI pictures. First the
*  upper panel, which gives the lower panel higher priority in AGI.
*  In between we must make sure that the current picture is again the
*  one that was current when the application started.
      CALL PGSVP( VPL, VPR, VPB2, VPT2 )
      CALL PGSWIN( LEFT, RIGHT, DLOW, DHIGH )
      CALL SPD_UGAD( 'DATA', 'SPECDRE_ARCDISP', I, STATUS )
      CALL AGI_SELP( PICID, STATUS )
      CALL PGSVP( VPL, VPR, VPB1, VPT1 )
      CALL PGSWIN( LEFT, RIGHT, BOTTOM, TOP )
      CALL SPD_UGAD( 'DATA', 'SPECDRE_ARCDISP', I, STATUS )

*  Return.
 500  CONTINUE
      CALL SPD_UGAB( 'DEVICE', .FALSE., STATUS )
      END
