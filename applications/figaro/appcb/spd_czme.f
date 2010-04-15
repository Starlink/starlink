      SUBROUTINE SPD_CZME( ORDER, NFDB, NCOMP, NPIX, ROWNUM, FDB,
     :   LOCATS, IDENTS, WEIGHT, RETVAL, A3, XVAL, RESULT, RESVAR,
     :   STATUS )
*+
*  Name:
*     SPD_CZME

*  Purpose:
*     Apply dispersion curve to a row for ARCDISP.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZME( ORDER, NFDB, NCOMP, NPIX, ROWNUM, FDB, LOCATS, IDENTS,
*        WEIGHT, WORK1, XVAL, RESULT, RESVAR, STATUS )

*  Description:
*     This routine performs the non-dialogue action for the
*     dispersion-curve application ARCDISP. This includes opening and
*     closing the graphics device.

*  Arguments:
*     ORDER = INTEGER (Given)
*        The polynomial order to be used for the fitted dispersion
*        curves.
*     NFDB = INTEGER (Given)
*        The number of features listed in the feature data base. The
*        size of FDB.
*     NCOMP = INTEGER (Given)
*        Maximum number of locations, half the size of a row in RESULT
*        and RESVAR.
*     NPIX = INTEGER (Given)
*        The length of rows in XVAL.
*     ROWNUM = INTEGER (Given)
*        Second dimension of XVAL, RESULT, RESVAR. The number of rows
*        available. This is equal to the row currently worked on.
*     FDB( NFDB ) = REAL (Given)
*        The list of possible identifications from a feature data base.
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
*     XVAL( NPIX, ROWNUM ) = REAL (Given and Returned)
*        The x values for each row. On entry these must be NDF pixel
*        coordinates. On regular exit they are laboratory values, that
*        is, in each row the appropriate dispersion curve is applied.
*     RESULT( 2 * NCOMP, ROWNUM ) = REAL (Given and Returned)
*        This array holds for each row pairs of numbers (centre,
*        laboratory value). Each pair is an entry in the list of located
*        features. Located features are at the beginning of each row,
*        bad-valued locations may be at the end of each row. Each
*        located feature may or may not be identified (laboratory value
*        good or bad).
*     RESVAR( 2 * NCOMP, ROWNUM ) = REAL (Given and Returned)
*        This array is the variance array corresponding to RESULT.
*        Location variances must be good and positive, identification
*        variances should be zero.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set if
*        -  the given ORDER is less than 1,
*        -  a fit of the specified order could not be achieved,
*        -  a call to the polynomial fit routine E02ADF returned a bad
*           status.
*        -  a call to the polynomial evaluation routine returned a bad
*           status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     18 Jun 1993 (hme):
*        Original version.
*     16 Mar 1994 (hme):
*        Re-adapt from SPADZ to allow logging graphics. The graphics
*        is inefficient since the plot device is opened and closed for
*        each row.
*     25 Jan 1995 (hme):
*        Renamed from SPAEA.
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
      INCLUDE 'PAR_ERR'          ! Standard PAR status values

*  Arguments Given:
      INTEGER ORDER
      INTEGER NFDB
      INTEGER NCOMP
      INTEGER NPIX
      INTEGER ROWNUM
      REAL FDB( NFDB )

*  Arguments Given and Returned:
      DOUBLE PRECISION LOCATS( NCOMP )
      DOUBLE PRECISION IDENTS( NCOMP )
      DOUBLE PRECISION WEIGHT( NCOMP )
      DOUBLE PRECISION RETVAL( NCOMP )
      DOUBLE PRECISION A3( 3 * NCOMP + 3 * ( ORDER + 1 ) )
      REAL XVAL( NPIX, ROWNUM )
      REAL RESULT( 2 * NCOMP, ROWNUM )
      REAL RESVAR( 2 * NCOMP, ROWNUM )

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
      INTEGER PICID              ! Identifier for current AGI picture
      INTEGER ZONID              ! SGS zone identifier
      INTEGER I, K               ! Temporary integers
      INTEGER IFAIL1, IFAIL2     ! PDA_DPOLFT stati
      INTEGER NIDENT             ! Number of valid IDs
      INTEGER IDMIN, IDMAX       ! Indices into FDB corresp. to plot
                                 ! range
      INTEGER NDEG               ! Highest degree fitted by PDA_DPOLFT
      REAL TEMP                  ! Temporary number
      REAL DELTA                 ! Temporary range
      REAL LEFT, RIGHT           ! Plot window extent
      REAL BOTTOM, TOP           ! Plot window extent
      REAL DLOW, DHIGH           ! Plot window y extent 2nd panel
      REAL FITL, FITR            ! Left and right fit curve values
      REAL FITX( PLTRES )        ! Curve plot x array
      REAL FITY( PLTRES )        ! Curve plot y array
      DOUBLE PRECISION EPS       ! Control / r.m.s. for PDA_DPOLFT
      DOUBLE PRECISION DTEMP1, DTEMP2 ! Temporary numbers

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check order.
      IF ( ORDER .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCDISP_E11', 'ARCDISP: Error obtaining ' //
     :      'polynomial order. Must be at least 1.', STATUS )
         RETURN
      END IF

*  Sort and convert to double the locations, IDs, weights.
      I = 1
      NIDENT = 0
 1    CONTINUE               ! Start of 'DO WHILE' loop
      IF ( I .LE. NCOMP ) THEN
         IF ( RESULT(2*I-1,ROWNUM) .NE. VAL__BADR ) THEN
            IF ( RESULT(2*I,  ROWNUM) .NE. VAL__BADR .AND.
     :           RESVAR(2*I-1,ROWNUM) .NE. VAL__BADR .AND.
     :           RESVAR(2*I-1,ROWNUM) .GT. 0. ) THEN
               NIDENT = NIDENT + 1
               LOCATS(NIDENT) = DBLE( RESULT(2*I-1,ROWNUM) )
               IDENTS(NIDENT) = DBLE( RESULT(2*I  ,ROWNUM) )
               WEIGHT(NIDENT) = 1D0 / DBLE( RESVAR(2*I-1,ROWNUM) )
            END IF
            I = I + 1
            GO TO 1
         END IF
      END IF

*  If there are insufficient points to fit chosen order.
      IF ( NIDENT .LT. ORDER+1 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'ARCDISP_T03', ROWNUM )
         CALL ERR_REP( 'ARCDISP_E17', 'ARCDISP: Error: Could ' //
     :      'not fit dispersion curve. Too few identified ' //
     :      'features in row #^ARCDISP_T03.', STATUS )
         GO TO 500

*  Else (fit can be attempted).
      ELSE

*     Perform the fit.
         IFAIL2 = 0
         EPS = 0D0
         CALL PDA_DPOLFT( NIDENT, LOCATS, IDENTS, WEIGHT, ORDER,
     :      NDEG, EPS, RETVAL, IFAIL1, A3, IFAIL2 )
         IF (  NDEG .NE. ORDER .OR.
     :         IFAIL1 .NE. 1 .OR. IFAIL2 .NE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'ARCDISP_T03', ROWNUM )
            CALL ERR_REP( 'ARCDISP_E18', 'ARCDISP: Error fitting ' //
     :         'dispersion curve in row #^ARCDISP_T03.', STATUS )
            GO TO 500
         END IF

*     Apply the current fit (in situ) to the x values for the
*     current row.
*     Just before we do that, we must safe the original range for the
*     purpose of plotting the dispersion curve etc.
         LEFT  = XVAL(1,   ROWNUM)
         RIGHT = XVAL(NPIX,ROWNUM)
         IFAIL2 = 0
         DO 2 K = 1, NPIX
            CALL PDA_DP1VLU( NDEG, 0, DBLE(XVAL(K,ROWNUM)),
     :         DTEMP1, DTEMP2, A3, IFAIL2 )
            IF ( IFAIL2 .NE. 0 ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'ARCDISP_T03', ROWNUM )
               CALL ERR_REP( 'ARCDISP_E20', 'ARCDISP: Error ' //
     :            'evaluating dispersion curve ' //
     :            'in row #^ARCDISP_T03.', STATUS )
               GO TO 500
            END IF
            XVAL(K,ROWNUM) = SNGL(DTEMP1)
 2       CONTINUE

*     Access graphics device.
         IF ( STATUS .NE. SAI__OK ) GO TO 500
         CALL SPD_UGAA( 'DEVICE', 'WRITE', ' ', PICID, ZONID, STATUS )
         IF ( STATUS .EQ. PAR__NULL )
     :       CALL ERR_ANNUL( STATUS )
         IF ( STATUS .NE. SAI__OK )
     :      GO TO 500

*     The horizontal extent of the plot is determined by the range of old
*     axis data. For the purpose of drawing the fit, an array of x values
*     covering the plot (only) is created.
         CALL SPD_UAAJR( LEFT, RIGHT, PLTRES, FITX, STATUS )
         IFAIL2 = 0
         DO 3 K = 1, PLTRES
            CALL PDA_DP1VLU( NDEG, 0, DBLE(FITX(K)),
     :         DTEMP1, DTEMP2, A3, IFAIL2 )
            IF ( IFAIL2 .NE. 0 ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'ARCDISP_T03', ROWNUM )
               CALL ERR_REP( 'ARCDISP_E20', 'ARCDISP: Error ' //
     :            'evaluating dispersion curve ' //
     :            'in row #^ARCDISP_T03.', STATUS )
               GO TO 500
            END IF
            FITY(K) = SNGL(DTEMP1)
 3       CONTINUE

*     The vertical extent of the plot is given by the range of fit y
*     values, plus 5 percent.
         CALL SPD_UAAAR( .FALSE., PLTRES, FITY, BOTTOM, TOP, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 400
         DELTA  = TOP - BOTTOM
         BOTTOM = BOTTOM - DELTA / 20.
         TOP    = TOP    + DELTA / 20.

*     Find index range into FDB corresponding to plot range.
         IDMIN = 1
 4       CONTINUE                     ! Start of 'DO WHILE' loop
         IF ( IDMIN .LE. NFDB ) THEN
            IF ( FDB(IDMIN) .LT. BOTTOM ) THEN
               IDMIN = IDMIN + 1
               GO TO 4
            END IF
         END IF
         IDMAX = IDMIN
 5       CONTINUE                     ! Start of 'DO WHILE' loop
         IF ( IDMAX .LT. NFDB .AND. FDB(IDMAX+1) .LT. TOP ) THEN
            IDMAX = IDMAX + 1
            GO TO 5
         END IF
         IF ( IDMAX .GT. NFDB ) THEN
            IDMIN = 1
            IDMAX = 0
         END IF

*     Set up plot window.
         CALL SPD_UGAC( ZONID, STATUS )
         CALL PGSVP( VPL, VPR, VPB1, VPT1 )
         CALL PGSWIN( LEFT, RIGHT, BOTTOM, TOP )

*     Plot the box.
         CALL PGBOX( 'BCTSN', 0., 0, 'BCTSN', 0., 0 )

*     Plot the identifications.
         CALL PGSCI( 3 )
         DO 6 I = IDMIN, IDMAX
            CALL PGMOVE( LEFT,  FDB(I) )
            CALL PGDRAW( RIGHT, FDB(I) )
 6       CONTINUE

*     Plot the locations.
         CALL PGSCH( 2. )
         I = 1
 7       CONTINUE               ! Start of 'DO WHILE' loop
         IF ( I .LE. NCOMP ) THEN
            IF ( RESULT(2*I-1,ROWNUM) .NE. VAL__BADR ) THEN
               IF ( RESULT(2*I,ROWNUM) .EQ. VAL__BADR ) THEN
                  CALL PGMOVE( RESULT(2*I-1,ROWNUM), BOTTOM )
                  CALL PGDRAW( RESULT(2*I-1,ROWNUM), TOP )
               ELSE
                  CALL PGPT( 1,
     :               RESULT(2*I-1,ROWNUM), RESULT(2*I,ROWNUM), 5 )
               END IF
            END IF
            I = I + 1
            GO TO 7
         END IF
         CALL PGSCH( 1. )
         CALL PGSCI( 1  )

*     Plot the fit curve in the main panel and its non-linear part in
*     an upper panel. The upper panel will also display identified
*     locations.
         IF ( NIDENT .GE. 2 ) THEN

*        Plot the fit curve.
            CALL SPD_WAAD( .FALSE., .FALSE., .TRUE., .FALSE., .FALSE.,
     :         1, 0, PLTRES, FITX, FITY, 0., 0., 0., 0., STATUS )

*        The first and last FITX and FITY define a linear function,
*        subtract that from FITY, in situ.
            FITL  = FITY(1)
            FITR  = FITY(PLTRES)
            DELTA = ( FITR - FITL ) / ( RIGHT - LEFT )
            DO 8 I = 1, PLTRES
               FITY(I) = FITY(I) - FITL - ( FITX(I) - LEFT ) * DELTA
 8          CONTINUE

*        Given FITY, find the plot y range.
            CALL SPD_UAAAR( .FALSE., PLTRES, FITY, DLOW, DHIGH, STATUS )

*        Given LEFT and RIGHT work out the y range so that FITY can be
*        plotted and all IDs with location inside [LEFT,RIGHT] can be
*        plotted. We know already that FITY would need [DLOW,DHIGH]. We
*        must now go through RESULT; for each identified location that
*        is within [LEFT,RIGHT] we subtract from the identification the
*        linear function.
            DO 9 I = 1, NIDENT
               TEMP = SNGL(IDENTS(I)) - FITL
     :            - ( SNGL(LOCATS(I)) - LEFT ) * DELTA
               DLOW  = MIN( DLOW,  TEMP )
               DHIGH = MAX( DHIGH, TEMP )
 9          CONTINUE

*        Add 5% on top and bottom.
            DELTA = DHIGH - DLOW
            DLOW  = DLOW  - DELTA / 20.
            DHIGH = DHIGH + DELTA / 20.

*        Switch to upper panel. Set up plot window.
            CALL PGSVP( VPL, VPR, VPB2, VPT2 )
            CALL PGSWIN( LEFT, RIGHT, DLOW, DHIGH )

*        Plot the box.
            CALL PGBOX( 'BCTS', 0., 0, 'BCTSM', 0., 0 )

*        Plot the fit curve.
            CALL SPD_WAAD( .FALSE., .FALSE., .TRUE., .FALSE., .FALSE.,
     :         1, 0, PLTRES, FITX, FITY, 0., 0., 0., 0., STATUS )

*        Plot the identified locations.
            DELTA = ( FITR - FITL ) / ( RIGHT - LEFT )
            CALL PGSCH( 2. )
            DO 10 I = 1, NIDENT
               TEMP = SNGL(IDENTS(I)) - FITL
     :            - ( SNGL(LOCATS(I)) - LEFT ) * DELTA
               CALL PGPT( 1, SNGL(LOCATS(I)), TEMP, 5 )
 10         CONTINUE
            CALL PGSCH( 1. )
         END IF

*     Before leaving, save the two view ports as AGI pictures. First the
*     upper panel, which gives the lower panel higher priority in AGI.
*     In between we must make sure that the current picture is again the
*     one that was current when the application started.
         CALL SPD_UGAD( 'DATA', 'SPECDRE_ARCDISP', I, STATUS )
         CALL AGI_SELP( PICID, STATUS )
         CALL PGSVP( VPL, VPR, VPB1, VPT1 )
         CALL PGSWIN( LEFT, RIGHT, BOTTOM, TOP )
         CALL SPD_UGAD( 'DATA', 'SPECDRE_ARCDISP', I, STATUS )
      END IF

*  Return.
 400  CONTINUE
      CALL SPD_UGAB( 'DEVICE', .FALSE., STATUS )
 500  CONTINUE
      END
