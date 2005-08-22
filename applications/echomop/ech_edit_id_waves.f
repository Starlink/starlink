      SUBROUTINE ECH_EDIT_ID_WAVES(
     :           NX,
     :           MAX_FEATURES,
     :           FTR_LIST,
     :           EXTRACTED_REF,
     :           ORDER,
     :           ORDER_IDNUM,
     :           N_POS,
     :           INTERACTIVE,
     :           WAVE_NPOLY,
     :           MAX_PERM_FTRS,
     :           IPF_POS,
     :           STRENGTH_FACTOR,
     :           MAXIMUM_POLY,
     :           FITTER,
     :           RMS,
     :           RMS_LIMIT,
     :           TEMP_WAVE_COEFFS,
     :           IDF_COUNT,
     :           IDF_POS,
     :           IDF_STATUS,
     :           IDF_WAVE,
     :           WWEIGHTS,
     :           RMSES,
     :           FITS,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_EDIT_ID_WAVES

*  Purpose:
*     Interactive wavelength editor for arc calibration.

*  Description:
*     This routine provides a set of interactive options to assist in
*     identifying arc lines and fitting the wavelength polyomial to
*     describe the variation along an order. The options provided are
*     as follows and are all selected using a single character. Note that
*     the RETURN key is not necessary for option selection, care is
*     therefore needed to ensure that the cursor is correctly positioned
*     BEFORE a cursor dependent option is selected.
*
*     A(uto), initiates an automatic search and match of the reference
*     feature database. Any preset limits on the wavelength and dispersion
*     search range are taken into account. Each possible solution is
*     reported upon with an indication of its probable accuracy. The
*     user may then choose to reject it, or examine it in detail for
*     verification or modification.
*
*     D(elete), removes an identified line from the set of identified
*     lines. This will usually be used to remove a suspect line
*     which has been incorrectly automatically identified. The identified
*     line nearest to the cursors x-position when the D is pressed, will
*     be the one which is deleted.
*
*     C(lear), Clear all non manually identified features from the order.
*     Will normally be used to remove the results of a useless fit.
*     If only some lines need to the removed then the T(hreshold) or
*     D(elete) options may be more appropriate.
*
*     E(xit), leaves the interactive identification/fitting routine
*     and returns to the main line id menu. The current set of fitted
*     lines, and the polynomial are saved in the reduction database.
*
*     F(it), performs a polynomial fit to the positions/wavelengths of
*     all currently identified lines. Reports on the deviation of each
*     line from the fit, and the improvement possible by deleting
*     each line and re-fitting. No check is made against the database
*     to see if further lines may now be identified, use the R option
*     if this is required.
*
*     I(nfo), reports information known about the nearest identified line
*     (relative to x-position of cursor when I is pressed). Details
*     include position, wavelength etc.
*
*     K(eep), examines the feature database to find the nearest features
*     to the current x-position of the cursor, if the feature is identified
*     then it is flagged as not-to-be-removed. This makes the feature
*     almost permanent and it can only be removed by either D(eleting)
*     it, or by doing a complete A(utomatic) re-identify of the order.
*     The C(lear) option does not remove manually identified/kept features.
*
*     L(ist), examines the feature database to find the nearest features
*     to the current x-position of the cursor, assuming the current
*     polynomial fits' predictions about the corresponding wavelengths.
*
*     M(ove), when viewing a zoomed graph of lines,allows the central
*     point of the plot to be moved to any point along the x axis. A prompt
*     is made for the exact co-ordinate required.
*
*     N(ew), used to specify a completely NEW feature. Should be used when
*     you wish to identify a line which has not been located at all by
*     the arc line location algorithm. All located lines are indicated by
*     a | appearing above their peak. Identified lines additionally have
*     the wavelength shown.
*
*     O(out-zoom), Reverses any currently selected zoom factor on the
*     displayed graph of lines.
*
*     Q(uit), leaves the manual identification/fitting routine WIHTOUT
*     updating the polynomial in the reduction database.  NOTE that any
*     newly identified lines will be saved. Only the polynomial (which
*     represents the outcome of an identification) is left unchanged.
*
*     R(e-interpolate), a polynomial fit is made to the currently
*     identified lines. This polynomial is then used to search the
*     database for any new line candidates. All new candidates are then
*     added into the fit and the polynomial iteratively re-fitted and
*     clipped until a stable solution is obtained.
*
*     S(et), is used to set the wavelength of a line. The line whose
*     wavelength is to be set should be the one nearest to the x-position
*     of the cursor and may be an identified or unidentified line.
*     Use the N(ew) option to create a new line where none had been
*     auto-located.
*
*     T(hreshold), is used to adjust the Strength factor above which
*     lines are allowed to be included in a fit. The Strength factor
*     is expressed as a reciprocal fractional intensity related to
*     the intensity of the brightest line in the order. IE. Strength
*     factor of 10 allows only lines at least 1/10 as bright as the
*     brightest line. Move the cursor to the intensity you wish to
*     set as the new threshold, the calculated strength factor will
*     be set up and reported.
*
*     Z(oom), increases the magnification of the graph used to plot
*     the line positions. To reverse the effect use the O(ut) option.
*
*     (Plus key), increments the degree of polynomial used for the
*     wavelength fitting. May be increased up to the maximum specified
*     by the tunable parameter TUNE_MAXPOLY.
*
*     (Minus key), decrements the degree of polynomial used for the
*     wavelength fitting.
*
*     (Greater-than key), when a zoomed graph is plotted, will shift the
*     viewpoint along to the right (plus x) by an amount sufficient to
*     show the adjacent section of the plot.
*
*     (Less-than key), when a zoomed graph is plotted, will shift the
*     viewpoint along to the left (minus x) by an amount sufficient to
*     show the adjacent section of the plot.
*
*     (any other key), provides available inofrmation on the line
*     nearest to the current x-position of the cursor.

*  Invocation:
*     CALL ECH_EDIT_ID_WAVES(
*     :    NX,
*     :    MAX_FEATURES,
*     :    FTR_LIST,
*     :    EXTRACTED_REF,
*     :    ORDER,
*     :    ORDER_IDNUM,
*     :    N_POS,
*     :    INTERACTIVE,
*     :    WAVE_NPOLY,
*     :    MAX_PERM_FTRS,
*     :    IPF_POS,
*     :    STRENGTH_FACTOR,
*     :    MAXIMUM_POLY,
*     :    FITTER,
*     :    RMS,
*     :    RMS_LIMIT,
*     :    TEMP_WAVE_COEFFS,
*     :    IDF_COUNT,
*     :    IDF_POS,
*     :    IDF_STATUS,
*     :    IDF_WAVE,
*     :    WWEIGHTS,
*     :    RMSES,
*     :    FITS,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     EXTRACTED_REF = REAL (Given)
*        Extracted reference spectrum.
*     ORDER = INTEGER (Returned)
*        Order being processed.
*     MAX_PERM_FTRS = INTEGER (Given)
*        Maximum number of features to use in a fit.
*     IPF_POS = REAL (Given)
*        Observed central positions of features.
*     STRENGTH_FACTOR = REAL (Given)
*        Minimum strength for fitted features.
*     N_POS = INTEGER (Given)
*        Number of features observed.
*     WAVE_NPOLY = INTEGER (Given)
*        Default order of wavelength polynomial fit.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum order of wavelength polynomial fit.
*     TEMP_WAVE_COEFFS = DOUBLE (Returned)
*        Wavelength polynomial coefficients.
*     IDF_COUNT = INTEGER (Returned)
*        Count of identified features.
*     IDF_POS = REAL (Returned)
*        Positions of identified features.
*     IDF_STATUS = INTEGER (Returned)
*        Statuses of identified features.
*     IDF_WAVE = REAL (Returned)
*        Wavelengths of identified features.
*     MAX_FEATURES = INTEGER (Given)
*        Maximum number of observed features per order.
*     FTR_LIST = REAL (Given)
*        List of known arc line wavelengths.
*     ORDER_IDNUM = INTEGER (Given)
*        Echelle order number.
*     INTERACTIVE = LOGICAL (Given)
*        TRUE if interactive mode is used.
*     FITTER = CHAR (Given)
*        Type of wavelength fitting function in use (POLY/SPLINE).
*     RMS = REAL (Given and Returned)
*        Average RMS deviation from fit.
*     RMS_LIMIT = REAL (Given)
*        Limiting rms deviation.
*     WWEIGHTS = REAL (Given)
*        Weights.
*     RMSES = REAL (Given)
*        RMS deviations for fits excluding each line in turn.
*     FITS = REAL (Given)
*
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     08-MAY-1996 (MJC):
*       Various bug fixes, use of CHR calls to beautify output text.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_CONTEXT.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_FEATURE.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER MAX_FEATURES
      REAL FTR_LIST( MAX_FEATURES )
      REAL RMS_LIMIT
      REAL EXTRACTED_REF(NX )
      INTEGER ORDER
      INTEGER ORDER_IDNUM
      INTEGER N_POS
      INTEGER MAX_PERM_FTRS
      REAL IPF_POS( MAX_PERM_FTRS )  ! Observed central positions of features.
      REAL STRENGTH_FACTOR
      INTEGER WAVE_NPOLY
      INTEGER MAXIMUM_POLY
      CHARACTER*( * ) FITTER
      INTEGER IDF_COUNT
      LOGICAL INTERACTIVE
      REAL WWEIGHTS( MAX_FIT_FTRS )
      REAL RMSES( MAX_FIT_FTRS )
      REAL FITS( MAX_FIT_FTRS )

*  Arguments Returned:
      REAL RMS
      DOUBLE PRECISION TEMP_WAVE_COEFFS( MAX_FIT_COEFFS ) ! Wavelength polynomials.
      REAL IDF_POS( MAX_PERM_FTRS )  ! Positions of identified features.
      REAL IDF_WAVE( MAX_PERM_FTRS ) ! Wavelengths of identified features.
      INTEGER IDF_STATUS( MAX_PERM_FTRS ) ! Statuses of identified features.

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL FTR_WAVES( MAX_FIT_FTRS )
      REAL FTR_WAVES2( MAX_FIT_FTRS )
      REAL XVA( MAX_FIT_FTRS )
      REAL TICK_INTERVAL
      REAL NEAR1, ORIGIN, PER_PIXEL
      REAL HIGH, LOW
      REAL XCV, XV, XVMAX, XVMIN, YOFF
      REAL YP, YP2, XV2, DIFF, WORST
      REAL MIN_DIST
      REAL XPOS
      REAL MAX_STRENGTH
      REAL POSS_WAVELEN
      REAL WAVE_FIT_DELTA
      REAL WAVE_DELTA
      REAL VALUE
      REAL VALUE1
      REAL VALUE2
      REAL ARC_ARFIND
      REAL TOP
      REAL DELTA_RMS
      REAL LAST_RMS
      REAL H1,H2,H3,HEIGHT

      INTEGER FTR_STATI( MAX_FIT_FTRS )
      INTEGER LOG( 6, 2, 4, 9, 6 )
      INTEGER INVOKE, IX, NEXT, IX2, NEXT2
      INTEGER IPOS, IERR
      INTEGER IXST, IXEN
      INTEGER I, II, III
      INTEGER IP
      INTEGER WINDOW
      INTEGER KEEP
      INTEGER LUN
      INTEGER INEAR1
      INTEGER INEAR2
      INTEGER SUBTICKS
      INTEGER CLEAN
      INTEGER FEATURE
      INTEGER ID_FEATURE
      INTEGER LAST
      INTEGER LOGIT
      INTEGER NP
      INTEGER NL
      INTEGER OPS
      INTEGER ND
      INTEGER BLENDS
      INTEGER NCHAR1
      INTEGER NCHAR2

      LOGICAL YESNO
      LOGICAL WAVEDEF
      LOGICAL HIGHER
      LOGICAL LOGGING
      LOGICAL HARDCOPY
      LOGICAL REFRESH
      LOGICAL ATLAS
      LOGICAL EDITING
      LOGICAL AUTOCLIP
      LOGICAL REVERSED
      LOGICAL CHECK

      CHARACTER*80 LOGNAME
      CHARACTER*55 TITLE
      CHARACTER*20 CHARS
      CHARACTER*20 CHARS2
      CHARACTER*16 REF_STR1
      CHARACTER*16 REF_STR2

*  Functions Called:
      EXTERNAL ARC_ARFIND
      INTEGER PGBEG
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      INTEGER ICH_ENCODE
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) )  RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Set status return by default.
      STATUS = ECH__RETRY_WFIT
      HARDCOPY = .FALSE.
      TOP = 0.0
      MAX_STRENGTH = 1E-20
      DO I = 1, N_POS
         IF ( EXTRACTED_REF( INT( IPF_POS( I ) ) ) .GT. MAX_STRENGTH )
     :      MAX_STRENGTH = EXTRACTED_REF( INT( IPF_POS( I ) ) )
      END DO
      LAST = 2
      OPS = 0
      WAVE_FIT_DELTA = 0.0

*  Report on rms.
      IF ( IDF_COUNT .GT. 0 .AND. RMS .GT. 0.0 ) THEN
         CALL CHR_RTOC( FLOAT( INT( RMS_LIMIT * 2500.0 ) ) / 10000.0,
     :        REF_STR1, NCHAR1 )
         CALL CHR_RTOC( FLOAT( INT( RMS * 10000.0 ) ) / 10000.0,
     :        REF_STR2, NCHAR2 )
         REPORT_STRING = ' RMS deviations (target:actual): ' //
     :        REF_STR1( :NCHAR1 ) // ':' //
     :        REF_STR2( :NCHAR2 ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )

         IF ( RMS .LT. RMS_LIMIT / 2.0 ) THEN
             CALL ECH_REPORT( 0, ' Probably a good identification.' )

         ELSE IF ( RMS .LT. RMS_LIMIT ) THEN
             REPORT_STRING = ' Possibly a good identification, ' //
     :            'check the plot.'
             CALL ECH_REPORT( 0, REPORT_STRING )

         ELSE IF ( RMS .GT. RMS_LIMIT * 10.0 ) THEN
             CALL ECH_REPORT( 0, '! Probably a bad identification.' )

         ELSE
             CALL ECH_REPORT( 0, '! Probably a bad identification.' )
         END IF
      END IF

*  Plot up the identified features.
      DO IP = 1, NX
         X_COORD( IP ) = FLOAT( IP )
      END DO

      IF ( .NOT. GRAPHICS_SETUP ) CALL ECH_SETUP_GRAPHICS( STATUS )
      IXST = 1
      IXEN = NX
      EDITING = .TRUE.
      IF ( STATUS .LT. 0 ) THEN
         REFRESH = .FALSE.

      ELSE
         REFRESH = .TRUE.
      END IF
      ATLAS = .FALSE.
      DO WHILE ( EDITING )
         IF ( REFRESH ) THEN

*        Autoscale data, allowing 10% extra at the top. (And allow for
*        the pathological case where there is no data at all!)
*        ECHARC ... ASSUMES LOW=0.0, ALLOWS 55% SINCE LABELS VERTICAL
            IF ( TEMP_WAVE_COEFFS( 1 ) .NE. 0.0 ) THEN
               DO IP = 1, NX
                  CALL ECH_FEVAL( FITTER, WAVE_NPOLY, TEMP_WAVE_COEFFS,
     :                 1, FLOAT( IP ), X_COORD( IP ), STATUS)
               END DO
               REVERSED = .FALSE.
               IF ( X_COORD( NX ) .LT. X_COORD( 1 ) ) REVERSED = .TRUE.
            END IF
            CALL GEN_RANGEF( EXTRACTED_REF, IXST, IXEN, HIGH, LOW )
            LOW = 0.0
            IF ( HIGH .EQ. LOW ) THEN
               HIGH = 10.0 + LOW
               LOW = -10.0 + LOW
            END IF
            HIGH = HIGH + 0.55 * ( HIGH - LOW )
            XVMIN = X_COORD( IXST )
            XVMAX = X_COORD( IXEN )
            IF ( XVMIN .GT. XVMAX ) THEN
               XVMAX = X_COORD( IXST )
               XVMIN = X_COORD( IXEN )
            END IF
            YOFF = ( HIGH - LOW ) * 0.05
            HIGHER = .FALSE.

            IF ( atlas ) THEN
               tick_interval = 10.0
               subticks = 5
               h1 = 0.0
               h2 = 0.0
               h3 = 0.0
               DO i = 1, IDF_COUNT
                 height = extracted_ref( int(idf_pos( i )) )
                 IF ( height .GT. h1 ) THEN
                   IF ( h1 .GT. h2 ) THEN
                     h2 = h1
                   ELSE IF ( h1 .GT. h3 ) THEN
                     h3 = h1
                   END IF
                   h1 = height
                 ELSE IF ( height .GT. h2 ) THEN
                   IF ( h2 .GT. h3 ) h3 = h2
                   h2 = height
                 ELSE IF ( height .GT. h3 ) THEN
                   h3 = height
                 END IF
               END DO
              IF ( h1 .GT. 15.*h3 ) THEN
                 IF ( h2 .GT. 15.*h3 ) THEN
                    high = 1.5 * h3
                 ELSE
                    high = 1.5 * h2
                 END IF
              ELSE
                 high = 1.5 * h1
              END IF
            ELSE
              tick_interval = 0.0
              subticks = 0
            END IF
            IF ( high .LE. 0.0 ) high = h1
            IF ( high .LE. 0.0 ) high = yoff
            IF ( top .GT. 0.0 ) high = top

*        Setup plot environment - note, not done using PGENV
*        because the wiewport is non-standard.
            CALL PGASK( .FALSE. )
            CALL PGPAGE
            CALL PGSVP( 0.1, 0.95, 0.1, 0.85 )
            CALL PGSWIN( XVMIN, XVMAX, LOW, HIGH )
            CALL PGBOX( 'ABCNST', TICK_INTERVAL, SUBTICKS, 'ABCNST',
     :           0.0, 0 )
            IF ( ATLAS ) THEN
               CALL CHR_ITOC( ORDER_IDNUM, REF_STR1, NCHAR1 )
               TITLE = 'Echelle order ' // REF_STR1( :NCHAR1 )

            ELSE
               CALL CHR_ITOC( ORDER, REF_STR1, NCHAR1 )
               TITLE = 'Identified features in order ' //
     :               REF_STR1( :NCHAR1 ) // ' [*-Imports, K-Keep]'
            END IF
            CALL PGLAB( 'Wavelength', 'Flux', TITLE )

*        Plot data.
            CALL PGBIN( IXEN - IXST + 1, X_COORD( IXST ),
     :           EXTRACTED_REF( IXST ), .TRUE. )

*        See if the x_coord represent pixels or wavelengths.
            WAVEDEF = X_COORD( 1 ) .NE. 1.0

*        Indicate any lines identified in this range.
            ii = 1
            IF ( N_POS .GT. 0 ) THEN
             IF ( .NOT. atlas ) THEN
              IF ( WAVEDEF ) THEN
                 CALL ECH_FEVAL( FITTER, WAVE_NPOLY,
     :                TEMP_WAVE_COEFFS, N_POS, IPF_POS, XVA, STATUS )

              ELSE
                 DO I = 1, N_POS
                    XVA( I ) = IPF_POS( I )
                 END DO
              END IF
              DO I = 1, N_POS
                IX = IPF_POS( I )
                XV = XVA( I )
                DO WHILE ( IDF_POS( II ) .GT. 0.0 .AND.
     :                     IDF_POS( II ) .LT. IPF_POS( I ) )
                   II = II + 1
                END DO
                IF ( XVMIN .LT. XV .AND. XV .LE. XVMAX ) THEN
                   YP = MAX( extracted_ref( IX - 1 ),
     :                       extracted_ref( IX ),
     :                       extracted_ref( IX + 1 ) ) + YOFF
                   CHARS = '___ '
                   CHARS2 = '__* '
                   NEXT = 5
                   NEXT2 = 5
                   IF ( IDF_POS( II ) .EQ. IPF_POS( I ) ) THEN
                      IF ( IAND( IDF_STATUS( II ),
     :                           FTR_POSIB_BLEND ) .NE. 0 ) THEN
                         CHARS = '_B_'
                      END IF
                      IF ( IAND( IDF_STATUS( II ),
     :                           FTR_MANU_IDENT ) .NE. 0 ) THEN
                         CHARS = '_K_'
                      END IF
                      INVOKE = ICH_ENCODE( CHARS,
     :                      IDF_WAVE( II ), 5, 6, NEXT )
                      NEXT = NEXT - 1
                      IF ( HIGHER ) THEN
                         CHARS = '______' // CHARS( :NEXT )
                         NEXT = NEXT + 6
                      END IF
                      HIGHER = .NOT. HIGHER
                      CALL PGPTXT( XV, YP, 90.0, 0.0, CHARS( :NEXT ) )

                   ELSE IF ( idf_pos( ii ) .GT. IPF_POS( i ) .AND.
     :                  idf_pos( ii ) .LT. IPF_POS( i + 1 ) ) THEN
                      IF ( IAND( idf_status( ii ),
     :                     ftr_posib_blend ) .NE. 0 ) THEN
                         chars2 = '_B*'
                      END IF
                      IF ( IAND( idf_status( ii ),
     :                     ftr_manu_ident ) .NE. 0 ) THEN
                         chars2 = '_K*'
                      END IF
                      xv2 = idf_pos( ii )
                      ix2 = INT( xv2 )
                      IF (WAVEDEF) THEN
                         CALL ECH_FEVAL( fitter, wave_npoly,
     :                        temp_wave_coeffs, 1,
     :                        idf_pos(ii), xv2, status)
                      END IF
                      IF ((XV2.GE.XVMIN).AND.(XV2.LE.XVMAX)) THEN
                         YP2 = MAX ( extracted_ref(IX2-1),
     :                        extracted_ref(IX2),
     :                        extracted_ref(IX2+1) ) + YOFF
                         INVOKE=ICH_ENCODE(CHARS2,
     :                               IDF_WAVE(ii),5,6,NEXT2)
                         NEXT2=NEXT2-1
                         IF ( higher ) THEN
                            chars2 = '______' // CHARS2( :NEXT2 )
                            next2 = next2 + 6
                         END IF
                         higher = .NOT. higher
                         CALL PGPTXT(XV2,YP2,90.0,0.0,CHARS2(:NEXT2))
                      END IF

                   ELSE IF ( yp-yoff .GE.
     :                       max_strength / strength_factor ) THEN
                     IF ( .NOT. atlas )
     :                 CALL PGPTXT(XV,YP,90.0,0.0,CHARS(:NEXT))
                   END IF
                END IF
              END DO
             END IF
            END IF

            IF ( .NOT. atlas ) THEN
               CALL ECH_GR_SET_LINESTYLE( 3 )
               CALL PGMOVE( xvmin, max_strength / strength_factor )
               CALL PGDRAW( xvmax, max_strength / strength_factor )
            END IF
            IF ( .NOT. atlas ) THEN
             IF ( WAVEDEF ) THEN
                I = 1
                DO WHILE ( I .LE. MAX_FEATURES )
                   IF ( FTR_LIST( I ) .GE. XVMIN ) GO TO 200
                   I = I + 1
                END DO
  200           DO WHILE ( I .LE. MAX_FEATURES )
                   IF ( FTR_LIST( I ) .GT. XVMAX ) GO TO 210
                   CALL PGPTXT( FTR_LIST( I ), HIGH - YOFF,
     :                  90.0, 0.0, '___' )
                   I = I + 1
                END DO
  210           CONTINUE
             END IF

            ELSE
               HIGHER = .FALSE.
               DO i = 1, max_features
                  IF ( ftr_list( i ) .GE. xvmin .AND.
     :                 ftr_list( i ) .LE. xvmax ) THEN
                     chars2 = '___'
                     next=5
                     INVOKE = ICH_ENCODE( CHARS2,
     :                        ftr_list( i ), 5, 6, NEXT2 )
                     higher = .NOT. higher
                     IF ( higher ) THEN
                        CALL PGPTXT( ftr_list( i ), 0.75 * high,
     :                       90.0, 0.0, chars2 )
                     ELSE
                        CALL PGPTXT( ftr_list( i ), 0.55 * high,
     :                       90.0, 0.0, chars2 )
                     END IF

                  ELSE IF ( FTR_LIST( I ) .GT. XVMAX ) THEN
                     GO TO 230
                  END IF
               END DO
  230          CONTINUE
            END IF

            CALL ECH_GR_SET_LINESTYLE( 1 )

           END IF

           IF ( HARDCOPY ) THEN
              HARDCOPY = .FALSE.
              CALL PGEND
              STATUS = PGBEG( 0, GRAPHICS_DEVICE_NAME, 1, 1 )
           END IF

*          Display choices avaliable
           IF ( interactive ) THEN
           CALL ECH_REPORT( 0, ' ' )

           CALL CHR_ITOC( WAVE_NPOLY, REF_STR1, NCHAR1 )
           REPORT_STRING = ' Selected degree for fits: ' //
     :                      REF_STR1( :NCHAR1 ) // '.'
           CALL ECH_REPORT( 0, REPORT_STRING )

           CALL CHR_ITOC( IDF_COUNT, REF_STR1, NCHAR1 )
           REPORT_STRING = ' Number of features identified: ' //
     :           REF_STR1( :NCHAR1 ) // '.'
           CALL ECH_REPORT( 0, REPORT_STRING )

           CALL ECH_REPORT( 0, ' ' )
           REPORT_STRING = ' Option [Info,Del,Set,Thresh,Auto,New,' //
     :           'Plot,Re-interp,Worst,BClip,Fit,+-=,'
           CALL ECH_REPORT( 0, REPORT_STRING )
           REPORT_STRING = '         XClip,Clear,Keep,List,Move,' //
     :           'Zoom,Ozoom,>,<,Exit,Quit,Help,?] '
           CALL ECH_REPORT( 0, REPORT_STRING )
           USER_INPUT_CHAR = ' '
           CALL ECH_READ_GRPH_CURSOR( STATUS )

           IF ( logging ) THEN
              logit = 0
              IF ( user_input_char .EQ. 'E' .AND. ops .GT. 3 )
     :           logit = 1
              IF ( user_input_char .EQ. 'R' ) logit = 2
              IF ( user_input_char .EQ. 'B' ) logit = 3
              IF ( user_input_char .EQ. 'W' ) logit = 4
              IF ( user_input_char .EQ. '+' ) logit = 5
              IF ( user_input_char .EQ. '-' ) logit = 6
              IF ( logit .GT. 0 .AND. rms .GT. 0. ) THEN
                 np = 1
                 IF ( wave_npoly .GT. 15 ) np = wave_npoly / 2 - 7
                 IF ( wave_npoly .GT. 5 ) np = 2
                 nl = 1
                 IF ( IDF_COUNT .GT. wave_npoly ) nl = 2
                 IF ( IDF_COUNT .GT. 2 * wave_npoly ) nl = 3
                 IF ( IDF_COUNT .GT. 3 * wave_npoly ) nl = 4
                 IF ( LAST_RMS .NE. 0.0 ) THEN
                    DELTA_RMS = 1.0 - RMS / LAST_RMS

                 ELSE
                    DELTA_RMS = 0.0
                 END IF
                 IF ( DELTA_RMS .GT. 0.5 ) THEN
                    ND = 5

                 ELSE IF ( DELTA_RMS .GT. 0.1 ) THEN
                    ND = 4

                 ELSE IF ( DELTA_RMS .GT. 0.01 ) THEN
                    ND = 3

                 ELSE IF ( DELTA_RMS .GT. 0.0 ) THEN
                    ND = 2

                 ELSE IF ( DELTA_RMS .LT. -0.5 ) THEN
                    ND = 9

                 ELSE IF ( DELTA_RMS .LT. -0.1 ) THEN
                    ND = 8

                 ELSE IF ( DELTA_RMS .LT. -0.01 ) THEN
                    ND = 7

                 ELSE IF ( DELTA_RMS .LT. 0.0 ) THEN
                    ND = 6

                 ELSE
                    ND = 1
                 END IF
                 OPS = OPS + 1
                 LAST_RMS = RMS
                 LOG( LAST, NP, NL, ND, LOGIT ) =
     :                LOG( LAST, NP, NL, ND, LOGIT ) + 1
                 LAST = LOGIT
              END IF
           END IF

*       Determine nearest feature to cursor.
           IF ( WAVEDEF ) THEN
              I = 2
              DO WHILE ( X_COORD( I ) .LE. X_CURSOR .AND. I .LT. NX )
                 I = I + 1
              END DO
              X_CURSOR = FLOAT( I ) + ( X_CURSOR - X_COORD( I ) ) /
     :              ( X_COORD( I ) - X_COORD( I - 1 ) )
           END IF
           X_CURSOR = MAX( 1.0, MIN( X_CURSOR, FLOAT( NX ) ) )
           min_dist = 1.0e20
           DO I = 1, N_POS
              IF ( ABS( IPF_POS( I ) - X_CURSOR ) .LT. MIN_DIST ) THEN
                 MIN_DIST = ABS( IPF_POS( I ) - X_CURSOR )
                 FEATURE = I
              END IF
           END DO
           IDF_COUNT = 0
           DO WHILE ( IDF_POS( IDF_COUNT + 1 ) .NE. 0.0 .AND.
     :                IDF_COUNT .LT. MAX_PERM_FTRS .AND.
     :                ( ( IDF_POS( IDF_COUNT + 1 ) .LT.
     :                IDF_POS( IDF_COUNT + 2 ) ) .OR.
     :                IDF_POS( IDF_COUNT + 2 ) .EQ. 0. ) )
              IDF_COUNT = IDF_COUNT + 1
           END DO
           DO I = IDF_COUNT + 1, MAX_PERM_FTRS
              IDF_POS( I ) = 0.0
              IDF_WAVE( I ) = 0.0
              IDF_STATUS( I ) = 0
           END DO
           ID_FEATURE = 0
           DO I = 1, IDF_COUNT
              IF ( IPF_POS( FEATURE ) .EQ. IDF_POS( I ) ) THEN
                 ID_FEATURE = I
              END IF
           END DO
           IF ( TEMP_WAVE_COEFFS( 1 ) .NE. 0.0 .AND.
     :          WAVE_NPOLY .GT. 0 )
     :        CALL ECH_FEVAL( FITTER, WAVE_NPOLY, TEMP_WAVE_COEFFS, 1,
     :             X_CURSOR, POSS_WAVELEN, STATUS )
           IF ( IDF_COUNT .GT. 0 )
     :       CALL ECH_CHECK_BLENDS( MAX_FEATURES, FTR_LIST,
     :            MAX_PERM_FTRS, IDF_COUNT, IDF_WAVE, IDF_STATUS,
     :            5.0 * RMS_LIMIT, BLENDS, STATUS )

*          If information requested then
           IF ( USER_INPUT_CHAR .EQ. 'I' ) THEN
              REFRESH = .FALSE.
              CALL ECH_REPORT( 0, ' ' )
              IF ( ID_FEATURE .GT. 0 ) THEN
                 CALL CHR_ITOC( FEATURE, REF_STR1, NCHAR1 )
                 CALL CHR_RTOC( IDF_POS( ID_FEATURE ),
     :                REF_STR2, NCHAR2 )
                 REPORT_STRING = ' Information for feature ' //
     :                REF_STR1( :NCHAR1 ) // ' at X=' //
     :                REF_STR2( :NCHAR2 ) // ':'
                 CALL ECH_REPORT( 0, REPORT_STRING )
     :
                 CALL CHR_RTOC( IDF_WAVE( ID_FEATURE ),
     :                          REF_STR1, NCHAR1 )
                 CALL CHR_RTOC( FITS( ID_FEATURE ), REF_STR2, NCHAR2 )
                 REPORT_STRING = '    Wavelength (reference/fitted): '//
     :                REF_STR1( :NCHAR1 ) // '/' //
     :                REF_STR2( :NCHAR2 ) // '.'
                 CALL ECH_REPORT( 0, REPORT_STRING )
     :
                 CALL CHR_RTOC(
     :                ABS( IDF_WAVE( ID_FEATURE ) -
     :                FITS( ID_FEATURE ) ), REF_STR1, NCHAR1 )
                 REPORT_STRING = '    Deviation from fit: ' //
     :                REF_STR1( :NCHAR1 ) // '.'
                 CALL ECH_REPORT( 0, REPORT_STRING )
                 CALL CHR_RTOC( RMSES( ID_FEATURE ), REF_STR1, NCHAR1 )
                 REPORT_STRING = '    RMS deviation if feature' //
     :                ' omitted: ' //
     :                REF_STR1( :NCHAR1 ) // '.'
                 CALL ECH_REPORT( 0, REPORT_STRING )
                 POSS_WAVELEN = IDF_WAVE( ID_FEATURE )

              ELSE
                 CALL CHR_RTOC( IPF_POS( FEATURE ), REF_STR1, NCHAR1 )
                 REPORT_STRING = ' Unidentified feature at' //
     :                ' position X=' //
     :                REF_STR1( :NCHAR1 ) // '.'
                 CALL ECH_REPORT( 0, REPORT_STRING )
                 IF ( TEMP_WAVE_COEFFS( 1 ) .NE. 0.0 .AND.
     :                WAVE_NPOLY .GT. 0 )
     :              CALL ECH_FEVAL( FITTER, WAVE_NPOLY,
     :                   TEMP_WAVE_COEFFS, 1, IPF_POS( FEATURE ),
     :                   POSS_WAVELEN, STATUS )
                 IF ( POSS_WAVELEN .GT. ABS_MAX_WAVELENGTH .OR.
     :                POSS_WAVELEN .LT. ABS_MIN_WAVELENGTH )
     :              POSS_WAVELEN = 0.0
               CALL CHR_RTOC( POSS_WAVELEN, REF_STR1, NCHAR1 )
               REPORT_STRING = ' Predicted wavelength of feature is ' //
     :              REF_STR1( :NCHAR1 ) // '.'
               CALL ECH_REPORT( 0, REPORT_STRING )
              END IF

              IF ( POSS_WAVELEN .GE. FTR_LIST( 1 ) ) THEN
                 I = 1
                 DO WHILE ( FTR_LIST( I ) .LE. POSS_WAVELEN )
                    I = I + 1
                 END DO

*             Make sure we don't go off the ends of the database.
                 I = MIN( MAX( 3, I - 1 ), MAX_FEATURES - 2 )

*             Display nearby reference features.
                 CALL ECH_REPORT( 0,
     :                '    Nearby reference features are:' )
                 IPOS = 1
                 IF ( REVERSED ) IPOS = NX
                 IF ( TEMP_WAVE_COEFFS( 1 ) .NE. 0.0 .AND.
     :                WAVE_NPOLY .GT. 0 )
     :              CALL ECH_FEVAL( FITTER, WAVE_NPOLY,
     :                   TEMP_WAVE_COEFFS, 1, FLOAT( IPOS ),
     :                   POSS_WAVELEN, STATUS )
                 DO II = -2, 2
                    IF ( REVERSED ) THEN
                       CHECK = ( IPOS .GE. 1 )

                    ELSE
                       CHECK = ( IPOS .LE. NX )
                    END IF
                    DO WHILE ( POSS_WAVELEN .LT. FTR_LIST( I + II )
     :                         .AND. CHECK )
                       IF ( REVERSED ) THEN
                          IPOS = IPOS - 1

                       ELSE
                          IPOS = IPOS + 1
                       END IF
                       IF ( TEMP_WAVE_COEFFS( 1 ) .NE. 0.0 .AND.
     :                      WAVE_NPOLY .GT. 0 )
     :                    CALL ECH_FEVAL( FITTER, WAVE_NPOLY,
     :                         TEMP_WAVE_COEFFS, 1, FLOAT( IPOS ),
     :                         POSS_WAVELEN, STATUS )
                    END DO
                    CALL CHR_RTOC( FTR_LIST( I + II ), REF_STR1, NCHAR1)
                    CALL CHR_ITOC( IPOS, REF_STR2, NCHAR2 )
                    REPORT_STRING = '       wavelength: ' //
     :                   REF_STR1( :NCHAR1 ) //
     :                   ' predicted X=' //
     :                   REF_STR2( :NCHAR2 ) // '.'
                    CALL ECH_REPORT( 0, REPORT_STRING )
                 END DO
              END IF

*          Else If zoom requested then
           ELSE IF ( user_input_char .EQ. 'Z' ) THEN
              refresh = .TRUE.
              window = MAX( 10, ixen - ixst + 1 )
              ixst = MAX( INT( x_cursor ) - window / 6, 1 )
              ixen = MIN( INT( x_cursor ) + window / 6, nx )
              window = MAX( 10, ixen - ixst + 1 )

*          Else If left-shift requested then
           ELSE IF ( user_input_char .EQ. '<' ) THEN

*          Only shift if we are zoomed.
              IF ( IXST .GT. 1 .OR. IXEN .LT. NX ) THEN
                 REFRESH = .TRUE.
                 IXST = MAX( INT( IXST ) - WINDOW * 4 / 5, 1 )
                 IXEN = MIN( INT( IXST ) + WINDOW, NX )
                 IXST = MIN( IXST, IXEN - WINDOW )

              ELSE
                 REFRESH = .FALSE.
              END IF

*          Else If right-shift requested then
           ELSE IF ( user_input_char .EQ. '>' ) THEN

*          Only shift if we are zoomed.
              IF ( IXST .GT. 1 .OR. IXEN .LT. NX ) THEN
                 REFRESH = .TRUE.
                 IXST = MAX( INT( IXST ) + WINDOW * 4 / 5, 1 )
                 IXEN = MIN( INT( IXST ) + WINDOW, NX )
                 IXST = MIN( IXST, IXEN - WINDOW )

              ELSE
                 REFRESH = .FALSE.
              END IF

*          Else If de-zoom requested then
           ELSE IF ( user_input_char .EQ. 'O' ) THEN
              refresh = .TRUE.
              window = MAX( 10, ( ixen - ixst + 1 ) * 12 )
              ixst = INT( x_cursor ) - window / 2
              ixen = INT( x_cursor ) + window / 2
              IF ( ixst .LT. 1 .OR. ixen .GT. nx ) THEN
                 ixst = 1
                 ixen = nx
              END IF
              window = MAX( 10, ixen - ixst + 1 )

*          Else If clear requested then
           ELSE IF ( user_input_char .EQ. 'C' ) THEN
              refresh = .TRUE.
              keep = 0
              clean = 0
              DO i = 1, max_perm_ftrs
                IF ( IAND( idf_status( i ),
     :               ftr_manu_ident ) .NE. 0 ) THEN
                 keep = keep + 1
                 idf_pos( keep ) = idf_pos( i )
                 IDF_WAVE( keep ) = IDF_WAVE( i )
                 idf_status( keep ) = idf_status( i )
                 rmses( keep ) = rmses( i )
                 fits( keep ) = fits( i )
                 wweights( keep ) = wweights( i )

                ELSE IF ( idf_pos ( i ) .GT. 0.0 ) THEN
                   clean = clean + 1
                END IF
              END DO
              DO i = keep + 1, max_perm_ftrs
                 idf_pos( i ) = 0.0
                 IDF_WAVE( i ) = 0.0
                 idf_status( i ) = 0
                 rmses( i ) = 0.0
                 fits( i ) = 0.0
                 wweights( i ) = 1.0
              END DO
              WRITE ( report_string, 1015 ) clean
              IDF_COUNT = KEEP
              CALL ECH_REPORT( 0, ' ' )
              CALL ECH_REPORT( 0, report_string )
              WRITE ( report_string, 1022 ) IDF_COUNT
              CALL ECH_REPORT( 0, report_string )
              CALL ECH_REPORT( 0, ' ' )

*          Else If quit requested then
           ELSE IF ( user_input_char .EQ. 'Q' ) THEN
              editing = .FALSE.
              status = ECH__QUIT_WFIT

*          Else If new feature specification requested then
           ELSE IF ( user_input_char .EQ. 'N' ) THEN
              editing = .TRUE.
              refresh = .FALSE.
              CALL ECH_REPORT( 0, ' ' )
              CALL CHR_RTOC( X_CURSOR, REF_STR1, NCHAR1 )
              REPORT_STRING = ' New feature specification at X=' //
     :              REF_STR1( :NCHAR1 ) // '.'
              CALL ECH_REPORT( 0, REPORT_STRING )
              CALL ECH_REPORT( 0, ' ' )
              value = ARC_ARFIND( ftr_list, max_features, poss_wavelen )
              CALL CHR_RTOC( POSS_WAVELEN, REF_STR1, NCHAR1 )
              REPORT_STRING = ' Fitted wavelength is ' //
     :              REF_STR1( :NCHAR1 ) // '.'
              CALL ECH_REPORT( 0, REPORT_STRING )
              WRITE ( report_string, 1025 ) value
              CALL ECH_REPORT( 0, report_string )
              WRITE ( report_string, 1028 ) value
              CALL ECH_REPORT( 0, report_string )
              CALL ECH_GET_PARAMETER(
     :             'INSTANT-PROMPT=Wavelength for feature',
     :             'FLOAT', value, .FALSE., ' ', 0, status )
              IF ( value2 .NE. value )
     :           CALL ECH_REPORT( 0,
     :           ' Warning: no listed line at specified wavelength.' )
              i = 1
              DO WHILE ( idf_pos ( i ) .GT. 0.0 .AND.
     :                   idf_pos ( i ) .LT. x_cursor )
                 i = i + 1
              END DO
              IF ( idf_pos ( i ) .GE. x_cursor ) THEN
               DO ii = MIN( max_perm_ftrs, IDF_COUNT + 1 ),
     :                MAX( i, 2 ), -1
                  idf_pos( ii ) = idf_pos( ii-1 )
                  IDF_WAVE( ii ) =
     :                  IDF_WAVE( ii-1 )
                  idf_status( ii ) = idf_status( ii-1 )
                  rmses( ii ) = rmses( ii-1 )
                  fits( ii ) = fits( ii-1 )
                  wweights( ii ) = wweights( ii-1 )
               END DO
              END IF
              idf_pos( i ) = x_cursor
              IDF_WAVE( i ) = value
              wweights( i ) = MIN( 1.0,
     :              extracted_ref( INT ( x_cursor+0.5 ) ) )
              idf_status( i ) = IOR( idf_status( i ),
     :              ftr_manu_ident )
              IDF_COUNT = MIN( max_perm_ftrs, IDF_COUNT + 1 )

              IX=idf_pos(i)
              IF (WAVEDEF) THEN
                   XV= IDF_WAVE(i)
                   xcv = poss_wavelen
              ELSE
                   XV=idf_pos(i)
                   xcv = x_cursor
              END IF
              IF ((XV.GE.XVMIN).AND.(XV.LE.XVMAX)) THEN
                   YP = MAX ( extracted_ref(IX-1),
     :                        extracted_ref(IX),
     :                        extracted_ref(IX+1) ) + YOFF
                   CHARS='_K_ '
                   next = 5
                   next2 = 5
                   INVOKE=ICH_ENCODE(CHARS,
     :                               IDF_WAVE(i),5,6,NEXT)
                   NEXT=NEXT-1
                   CALL PGPTXT(xv,YP,90.0,0.0,CHARS(:NEXT))
                   IF ( ABS ( xv-xcv ) .GT. (xvmax-xvmin)/100. )
     :                refresh = .TRUE.
              ELSE
                   refresh = .TRUE.
              END IF

*          Else If new x coord specification requested then
           ELSE IF ( user_input_char .EQ. 'M' ) THEN
              editing = .TRUE.
              refresh = .TRUE.
              IF ( WAVEDEF ) THEN
                 value = poss_wavelen

              ELSE
                 value = x_cursor
              END IF
              CALL ECH_GET_PARAMETER(
     :             'INSTANT-PROMPT=New central X-coord',
     :             'FLOAT', VALUE, .FALSE., ' ', 0, STATUS )
              X_CURSOR = VALUE
              IF ( WAVEDEF ) THEN
                 I = 1
                 DO WHILE ( VALUE .GE. X_COORD( I ) .AND. I .LT. NX )
                    I = I + 1
                 END DO
                 VALUE = FLOAT( I )
              END IF
              IF ( WINDOW .EQ. 0 ) THEN
                 window = MAX( 10, ixen - ixst + 1 )
              END IF
              IXST = MAX( INT( VALUE ) - WINDOW / 2, 1 )
              IXEN = MIN( INT( VALUE ) + WINDOW / 2, NX )

*          Else If feature to-be-kept specification requested then
           ELSE IF ( user_input_char .EQ. 'K' ) THEN
              editing = .TRUE.
              refresh = .FALSE.
              IF ( ID_FEATURE .GT. 0 ) THEN
                 I = ID_FEATURE
                 IDF_STATUS( I ) = IOR( IDF_STATUS( I ),
     :                 FTR_MANU_IDENT )
                 CALL CHR_RTOC( IDF_WAVE( ID_FEATURE ),
     :                REF_STR1, NCHAR1 )
                 REPORT_STRING = ' Feature at wavelength ' //
     :                 REF_STR1( :NCHAR1 ) //
     :                 ' flagged as manually confirmed.'

              ELSE
                 REPORT_STRING =  ' No feature identified here.'
              END IF
              CALL ECH_REPORT( 0, ' ' )
              CALL ECH_REPORT( 0, report_string )

*          Else If new feature wavelength specification requested then
           ELSE IF ( user_input_char .EQ. 'S' ) THEN
              editing = .TRUE.
              refresh = .FALSE.
              WRITE ( report_string, 1012 ) feature,x_cursor
              CALL ECH_REPORT( 0, ' ' )
              CALL ECH_REPORT( 0, report_string )
              CALL ECH_REPORT( 0, ' ' )
              value = ARC_ARFIND( ftr_list, max_features, poss_wavelen )
              CALL CHR_RTOC( POSS_WAVELEN, REF_STR1, NCHAR1 )
              REPORT_STRING = ' Fitted wavelength is ' //
     :              REF_STR1( :NCHAR1 ) // '.'
              CALL ECH_REPORT( 0, REPORT_STRING )
              WRITE ( report_string, 1025 ) value
              CALL ECH_REPORT( 0, report_string )
              IF ( id_feature .NE. 0 ) THEN
                 value = IDF_WAVE ( id_feature )
                 CALL CHR_RTOC( VALUE, REF_STR1, NCHAR1 )
                 REPORT_STRING = ' Currently identified as ' //
     :                REF_STR1( :NCHAR1 ) // '.'
                 CALL ECH_REPORT( 0, REPORT_STRING )
              END IF
              WRITE ( report_string, 1028 ) value
              CALL ECH_REPORT( 0, report_string )

              CALL ECH_GET_PARAMETER(
     :             'INSTANT-PROMPT=Wavelength for feature',
     :             'FLOAT', value, .FALSE., ' ', 0, status )
              value2 = ARC_ARFIND( ftr_list, max_features, value )
              IF ( value2 .NE. value )
     :           CALL ECH_REPORT( 0,
     :           ' Warning: no listed line at specified wavelength.' )
              IF ( id_feature .GT. 0 ) THEN
                 IDF_WAVE( id_feature ) = value
                 i = id_feature
                 idf_status( i ) = IOR( idf_status( i ), ftr_manu_ident)
              ELSE
                 i = 1
                 DO WHILE ( idf_pos ( i ) .GT. 0.0 .AND.
     :                      idf_pos ( i ) .LT. x_cursor )
                    i = i + 1
                 END DO
                 IF ( IDF_POS( I ) .GE. X_CURSOR ) THEN
                  DO II = MIN( MAX_PERM_FTRS, IDF_COUNT + 1 ),
     :                    MAX( I, 2 ), -1
                     IDF_POS( II ) = IDF_POS( II - 1 )
                     IDF_WAVE( II ) = IDF_WAVE( II - 1 )
                     IDF_STATUS( II ) = IDF_STATUS( II - 1 )
                     RMSES( II ) = RMSES( II - 1 )
                     FITS( II ) = FITS( II - 1 )
                     WWEIGHTS( II ) = WWEIGHTS( II - 1 )
                  END DO
                 END IF
                 IDF_POS( I ) = IPF_POS( FEATURE )
                 IDF_WAVE( I ) = VALUE
                 WWEIGHTS( I ) = MIN( 1.0,
     :                 EXTRACTED_REF( INT( X_CURSOR + 0.5 ) ) )
                 IDF_STATUS( I ) = IOR( IDF_STATUS( I ),
     :                 FTR_MANU_IDENT )
                 IDF_COUNT = MIN( MAX_PERM_FTRS, IDF_COUNT + 1 )
              END IF

              IX = IPF_POS( feature )
              IF ( WAVEDEF ) THEN
                   XV = IDF_WAVE( i )
                   xcv = poss_wavelen

              ELSE
                   XV = IPF_POS( feature )
                   xcv = x_cursor
              END IF
              IF ((XV.GE.XVMIN).AND.(XV.LE.XVMAX)) THEN
                   YP = MAX ( extracted_ref(IX-1),
     :                        extracted_ref(IX),
     :                        extracted_ref(IX+1) ) + YOFF
                   CHARS='_K_ '
                   next = 5
                   next2 = 5
                   INVOKE=ICH_ENCODE(CHARS,
     :                               IDF_WAVE(i),5,6,NEXT)
                   NEXT=NEXT-1
                   CALL PGPTXT(xv,YP,90.0,0.0,CHARS(:NEXT))
                   IF ( ABS ( xv-xcv) .GT. (xvmax-xvmin)/100. )
     :                  refresh = .TRUE.
              ELSE
                    refresh= .TRUE.
              END IF

*          Else If threshold modification requested then
           ELSE IF ( user_input_char .EQ. 'T' ) THEN
              refresh = .TRUE.
              CALL ECH_REPORT( 0, ' ' )
              IF ( atlas ) THEN
                 top = y_cursor
                 CALL CHR_RTOC( TOP, REF_STR1, NCHAR1 )
                 REPORT_STRING = ' Atlas maximum flux threshold' //
     :                ' set to ' // REF_STR1( :NCHAR1 ) // '.'
                 CALL ECH_REPORT( 0, REPORT_STRING )

              ELSE
                 strength_factor = max_strength / MAX ( 1.,y_cursor )
                 WRITE ( report_string, 1019 ) strength_factor
                 CALL ECH_REPORT( 0, report_string )
              END IF
              CALL ECH_REPORT( 0, ' ' )

*          Else If list reference requested then
           ELSE IF ( user_input_char .EQ. 'L' ) THEN
              refresh = .FALSE.
              editing = .TRUE.
              IF ( poss_wavelen .LT. ftr_list(1) ) THEN
                IF ( IDF_COUNT .GT. 0 ) THEN
                  DO iii = 1, max_fit_coeffs
                     temp_wave_coeffs( iii ) = 0.0
                  END DO
                  IF ( temp_wave_coeffs ( 1 ) .NE. 0.0 .AND.
     :                 wave_npoly .GT. 0 ) THEN
                    CALL ECH_WAVE_POLYFIT( idf_pos,
     :                   IDF_WAVE, idf_status,
     :                   wweights, rmses, fits, .FALSE.,
     :                   IDF_COUNT, nx,
     :                   max_fit_coeffs, .TRUE., wave_npoly,
     :                   temp_wave_coeffs, rms )
                    CALL ECH_FEVAL( fitter, wave_npoly,
     :                   temp_wave_coeffs, 1, x_cursor,
     :                   poss_wavelen, status)
                  END IF
                ELSE
                   CALL ECH_REPORT( 0,
     :                  ' No identified features to use in a fit.' )
                END IF
              END IF

              IF ( poss_wavelen .GE. ftr_list(1) ) THEN
               i = 1
               DO WHILE ( ftr_list ( i ) .LT. poss_wavelen )
                 i = i + 1
               END DO
               CALL ECH_REPORT( 0, ' ' )
               ipos = 0
               IF ( temp_wave_coeffs ( 1 ) .NE. 0.0 .AND.
     :              wave_npoly .GT. 0 )
     :            CALL ECH_FEVAL( fitter, wave_npoly, temp_wave_coeffs,
     :                 1, FLOAT( ipos ), poss_wavelen, status )
               DO ii = -10, 10
                 DO WHILE ( poss_wavelen .LT. ftr_list( i + ii ) .AND.
     :                      ipos .LE. nx )
                    ipos = MAX( ipos + 1, 1 )
                    IF ( temp_wave_coeffs ( 1 ) .NE. 0.0 .AND.
     :                   wave_npoly .GT. 0 )
     :                CALL ECH_FEVAL( fitter, wave_npoly,
     :                     temp_wave_coeffs, 1, FLOAT( ipos ),
     :                     poss_wavelen, status )

                 END DO
                 IF ( IPOS .GE. 1 .AND. IPOS .LE. NX ) THEN
                    WRITE ( report_string, 1018 ) ftr_list(i+ii),ipos

                 ELSE
                    WRITE ( report_string, 1018 ) ftr_list(i+ii),0
                 END IF
                 CALL ECH_REPORT( 0, report_string )
               END DO
               CALL ECH_REPORT( 0, ' ' )
              END IF

*          Else If auto re-identify requested then
           ELSE IF ( user_input_char .EQ. 'R' ) THEN
              REFRESH = .TRUE.
              EDITING = .TRUE.
              IF ( autoclip ) THEN
                CALL ECH_REPORT( 0,
     :               ' Automatic poor-line clipping is enabled.' )
              END IF

              IF ( RMS .EQ. 0.0 ) THEN
                 IF ( IDF_COUNT .GT. 0 .AND. WAVE_NPOLY .GT. 0 ) THEN
                    DO III = 1, MAX_FIT_COEFFS
                       TEMP_WAVE_COEFFS ( III ) = 0.0
                    END DO
                    CALL ECH_WAVE_POLYFIT( IDF_POS, IDF_WAVE,
     :                   IDF_STATUS, WWEIGHTS, RMSES, FITS, AUTOCLIP,
     :                   IDF_COUNT, NX, MAX_FIT_COEFFS, .TRUE.,
     :                   WAVE_NPOLY, TEMP_WAVE_COEFFS, RMS )

                 ELSE
                    CALL ECH_REPORT( 0,
     :                   ' No identified features to use in a fit.' )
                 END IF
              END IF

              IF ( IDF_COUNT .GT. 1 )
     :           WAVE_FIT_DELTA = 1.0 / ABS( (
     :                 IDF_POS( IDF_COUNT ) - IDF_POS( 1 ) ) /
     :                 ( IDF_WAVE( IDF_COUNT ) - IDF_WAVE( 1 ) ) )

*          Check for previously identified features and remember their
*          wavelengths and statuses.
              DO I = 1, N_POS
                 IF ( TEMP_WAVE_COEFFS( 1 ) .NE. 0.0 .AND.
     :                WAVE_NPOLY .GT. 0 )
     :              CALL ECH_FEVAL( FITTER, WAVE_NPOLY,
     :                   TEMP_WAVE_COEFFS, 1, IPF_POS( I ),
     :                   FTR_WAVES( I ), STATUS )
                 FTR_STATI( I ) = 0

                 DO II = 1, IDF_COUNT
                    IF ( INT( IPF_POS( I ) * 10000.0 ) .EQ.
     :                   INT( IDF_POS( II ) * 10000.0 ) ) THEN
                       FTR_WAVES( I ) = IDF_WAVE( II )
                       FTR_STATI( I ) = IDF_STATUS( II )
                    END IF
                 END DO
              END DO

              DO i = 1, N_POS
                 FTR_WAVES2( i ) = 0.0
                 inear1 = 0
                 inear2 = 0
                 near1 = 1.0e20
                 IF ( IDF_COUNT .GT. 1 ) THEN
                    DO id_feature = 1, IDF_COUNT
                      IF ( ABS( IPF_POS( i ) - idf_pos( id_feature ) )
     :                     .LT. near1 ) THEN
                         near1 = ABS( IPF_POS( i ) -
     :                         idf_pos( id_feature ) )
                         inear1 = id_feature
                      END IF
                    END DO
                    IF ( inear1 .EQ. 1 ) THEN
                       inear2 = 2

                    ELSE IF ( inear1 .EQ. IDF_COUNT ) THEN
                       inear2 = IDF_COUNT - 1

                    ELSE IF ( idf_pos( inear1 ) .GT. IPF_POS( i ) ) THEN
                       inear2 = inear1 - 1

                    ELSE
                       inear2 = inear1 + 1
                    END IF
                    IF ( inear2 .LT. inear1 ) THEN
                       id_feature = inear1
                       inear1 = inear2
                       inear2 = id_feature
                    END IF
                    origin = IDF_WAVE ( inear1 ) -
     :                        ( IDF_WAVE ( inear2 ) -
     :                          IDF_WAVE ( inear1 )   ) /
     :                        ( idf_pos ( inear2 ) -
     :                          idf_pos ( inear1 )   ) *
     :                       idf_pos ( inear1 )
                    per_pixel =
     :                        ( IDF_WAVE ( inear2 ) -
     :                          IDF_WAVE ( inear1 )   ) /
     :                        ( idf_pos ( inear2 ) -
     :                          idf_pos ( inear1 )   )
                    FTR_WAVES2( i ) = origin + per_pixel * IPF_POS( i )
                    FTR_WAVES2( i ) = ( FTR_WAVES2( i ) +
     :                    FTR_WAVES( i ) ) / 2.0
                 END IF
              END DO

*             Loop thru all observed features
              IDF_COUNT = 0
              DO i = 1, N_POS

*               Search for nearest matching wavelength in database
                poss_wavelen = ARC_ARFIND( ftr_list,
     :                max_features, FTR_WAVES( i ) )

*               If match is close enough then
                wave_delta = ABS( poss_wavelen - FTR_WAVES( i ) )
                IF ( ( ABS( poss_wavelen - FTR_WAVES( i ) ) .LE.
     :               wave_fit_delta .AND.
     :         ( extracted_ref ( INT (
     :                 IPF_POS( i ))) / max_strength .GE.
     :                                1.0 / strength_factor ) ) .OR.
     :            IAND(ftr_stati(i),ftr_manu_ident) .EQ.
     :                              ftr_manu_ident     ) THEN
                 IF ( IDF_COUNT .LT. max_fit_ftrs ) THEN

*                   Flag feature as identified, use feature strength to as weight
                    IF ( reversed ) THEN
                      check = poss_wavelen .LT. IDF_WAVE( IDF_COUNT )
                    ELSE
                      check = poss_wavelen .GT. IDF_WAVE( IDF_COUNT )
                    END IF
                    IF ( IDF_COUNT .EQ. 0 ) THEN
                       IDF_COUNT = IDF_COUNT + 1
                    ELSE IF ( check .OR.
     :                        wave_delta .GT. rmses( IDF_COUNT ) ) THEN
                       IDF_COUNT = IDF_COUNT + 1
                    END IF
                    idf_pos( IDF_COUNT ) = IPF_POS( i )
                    IDF_WAVE( IDF_COUNT ) = poss_wavelen
                    idf_status( IDF_COUNT ) = ftr_stati( i )
                    wweights( IDF_COUNT ) = MIN( 0.01,
     :                    extracted_ref( INT( IPF_POS( i ) ) ) )
                    rmses( IDF_COUNT ) = wave_delta
                 END IF

                ELSE IF ( FTR_WAVES2( i ) .GT. 0.0 ) THEN
                  poss_wavelen = ARC_ARFIND( ftr_list,
     :                  max_features, FTR_WAVES2( i ) )

                  wave_delta = ABS( poss_wavelen - FTR_WAVES2( i ) )
                  IF ( ( ABS( poss_wavelen - FTR_WAVES2( i ) ) .LE.
     :                 wave_fit_delta .AND.
     :         ( extracted_ref ( INT (
     :                 IPF_POS( i )))  / max_strength .GE.
     :                                1.0 / strength_factor  ) ) .OR.
     :            IAND(ftr_stati(i),ftr_manu_ident) .EQ.
     :                              ftr_manu_ident     ) THEN
                   IF ( IDF_COUNT .LT. max_fit_ftrs ) THEN

*                   Flag feature as identified, use feature strength to as weight
                    IF ( reversed ) THEN
                      check = poss_wavelen .LT. IDF_WAVE( IDF_COUNT )
                    ELSE
                      check = poss_wavelen .GT. IDF_WAVE( IDF_COUNT )
                    END IF
                    IF ( IDF_COUNT .EQ. 0 ) THEN
                       IDF_COUNT = IDF_COUNT + 1
                    ELSE IF (  check .OR.
     :                  ( wave_delta .GT.
     :                    rmses ( IDF_COUNT ) )   ) THEN
                       IDF_COUNT = IDF_COUNT + 1
                    END IF
                    idf_pos( IDF_COUNT ) = IPF_POS( i )
                    IDF_WAVE( IDF_COUNT ) = poss_wavelen
                    idf_status( IDF_COUNT ) = ftr_stati( i )
                    wweights( IDF_COUNT ) =
     :                 MIN( 0.01, extracted_ref( INT( IPF_POS( i ))))
                    rmses( IDF_COUNT ) = wave_delta
                   END IF
                  END IF
                END IF
              END DO

              DO i = IDF_COUNT+1, max_perm_ftrs
                 idf_pos ( i ) = 0.0
                 IDF_WAVE ( i ) = 0.0
                 idf_status ( i ) = 0
              END DO
              IF ( IDF_COUNT .GT. 0 )
     :             CALL ECH_CHECK_BLENDS(
     :                  MAX_FEATURES,
     :                  FTR_LIST,
     :                  MAX_PERM_FTRS,
     :                  IDF_COUNT,
     :                  IDF_WAVE,
     :                  IDF_STATUS,
     :                  5.0 * RMS_LIMIT,
     :                  BLENDS,
     :                  STATUS
     :                 )

              IF ( IDF_COUNT .GT. 0 .AND.
     :             wave_npoly .GT. 0 ) THEN
                DO iii = 1, max_fit_coeffs
                   temp_wave_coeffs( iii ) = 0.0
                END DO
                CALL ECH_WAVE_POLYFIT ( idf_pos,
     :               IDF_WAVE,
     :               idf_status,
     :               wweights, rmses, fits,
     :               autoclip,
     :               IDF_COUNT, nx,
     :               max_fit_coeffs, .TRUE.,
     :               wave_npoly,
     :               temp_wave_coeffs, rms )

              ELSE
                 CALL ECH_REPORT( 0,
     :                ' No identified features to use in a fit.' )
              END IF

*          Else If exit requested then
           ELSE IF ( user_input_char .EQ. 'E' ) THEN
              editing = .FALSE.
              status = ECH__SAVE_WFIT

*          Else If autosearch requested then
           ELSE IF ( user_input_char .EQ. 'A' ) THEN
              yesno = .TRUE.
              IF ( IDF_COUNT .GT. 0 ) THEN
                REPORT_STRING = ' Warning: all current'//
     :           ' identifications for this order will be cleared.'
                CALL ECH_REPORT(0, REPORT_STRING )
                CALL ECH_GET_PARAMETER(
     :              'INSTANT-PROMPT=Proceed with automatic search',
     :              'LOGICAL', value, yesno, ' ', 0, status )
              END IF
              IF ( yesno ) THEN
                DO i = 1, max_perm_ftrs
                   idf_pos ( i ) = 0.0
                   IDF_WAVE ( i ) = 0.0
                   idf_status ( i ) = 0
                   rmses ( i ) = 0.0
                   fits ( i ) = 0.0
                   wweights ( i ) = 1.0
                END DO
                editing = .FALSE.
                status = ECH__AUTO_IDENTIFY
              END IF

*          Else If plot refresh requested then
           ELSE IF ( USER_INPUT_CHAR .EQ. 'P' ) THEN
              REFRESH = .TRUE.

           ELSE IF ( USER_INPUT_CHAR .EQ. '^' ) THEN
              REFRESH = .TRUE.
              HARDCOPY = .TRUE.
              CALL PGEND
              STATUS = PGBEG( 0, HARDCOPY_DEVICE_NAME, 1, 1 )

           ELSE IF ( USER_INPUT_CHAR .EQ. '!' ) THEN
              REFRESH = .TRUE.
              ATLAS = .NOT. ATLAS

           ELSE IF ( user_input_char .EQ. 'W' ) THEN
              worst = 0.0
              IF ( IDF_COUNT .GT. 0 ) THEN
                 IF ( temp_wave_coeffs( 1 ) .EQ. 0.0D0 ) THEN
                   CALL ECH_WAVE_POLYFIT( idf_pos,
     :                  IDF_WAVE,
     :                  idf_status,
     :                  wweights, rmses, fits,
     :                  autoclip,
     :                  IDF_COUNT, nx,
     :                  max_fit_coeffs, .TRUE.,
     :                  wave_npoly,
     :                  temp_wave_coeffs, rms )
                 END IF
                 id_feature = 0
                 DO i = 1, IDF_COUNT
                    CALL ECH_FEVAL( fitter, wave_npoly,
     :                   temp_wave_coeffs, 1,
     :                   idf_pos( i ),
     :                   value1, status )
                    diff = ABS( IDF_WAVE( i ) -
     :                           value1 )
                    IF ( diff .GT. worst ) THEN
                       worst = diff
                       id_feature = i
                    END IF
                 END DO
                IF ( ID_FEATURE .GT. 0 ) THEN
                 XPOS = IDF_POS ( ID_FEATURE )
                 REFRESH  = .FALSE.
                 DO I = ID_FEATURE, IDF_COUNT
                    IDF_POS( I ) = IDF_POS( I + 1 )
                    IDF_WAVE( I ) = IDF_WAVE( I + 1 )
                    IDF_STATUS( I ) = IDF_STATUS( I + 1 )
                    RMSES( I ) = RMSES( I + 1 )
                    FITS( I ) = FITS( I + 1 )
                    WWEIGHTS( I ) = WWEIGHTS( I + 1 )
                 END DO
                 IDF_POS( IDF_COUNT ) = 0.0
                 IDF_WAVE( IDF_COUNT ) = 0.0
                 IDF_STATUS( IDF_COUNT ) = 0
                 RMSES( IDF_COUNT ) = 0.0
                 FITS( IDF_COUNT ) = 0.0
                 WWEIGHTS( IDF_COUNT ) = 1.0
                 IDF_COUNT = IDF_COUNT - 1
                 CALL ECH_REPORT( 0, ' ' )
                 CALL ECH_REPORT( 0, ' ' )
                 WRITE ( report_string, 1024 ) id_feature,xpos
                 CALL ECH_REPORT( 0, report_string )
                 CALL ECH_WAVE_POLYFIT( idf_pos, IDF_WAVE,
     :                idf_status, wweights, rmses, fits,
     :                .FALSE., IDF_COUNT, nx, max_fit_coeffs, .TRUE.,
     :                wave_npoly, temp_wave_coeffs, rms )

                ELSE
                   CALL ECH_REPORT( 0,' No feature identified here.' )
                END IF
              END IF

           ELSE IF ( user_input_char .EQ. 'B' ) THEN
              IF ( IDF_COUNT .GT. 0 ) THEN
                 IF ( temp_wave_coeffs ( 1 ) .EQ. 0.0D0 ) THEN
                   CALL ECH_WAVE_POLYFIT( idf_pos, IDF_WAVE,
     :                  idf_status, wweights, rmses, fits,
     :                  autoclip, IDF_COUNT, nx,
     :                  max_fit_coeffs, .TRUE., wave_npoly,
     :                  temp_wave_coeffs, rms )
                 END IF
                IF ( IDF_COUNT .GT. 0 ) THEN
                 DO id_feature = 1, IDF_COUNT
                   IF ( IAND(idf_status(id_feature),
     :                       ftr_posib_blend) .NE. 0 ) THEN
                     xpos = idf_pos ( id_feature )
                     DO i = id_feature, IDF_COUNT
                        idf_pos( i ) = idf_pos( i + 1 )
                        IDF_WAVE( i ) = IDF_WAVE( i + 1 )
                        idf_status( i ) = idf_status( i + 1 )
                        rmses( i ) = rmses( i + 1 )
                        fits( i ) = fits( i + 1 )
                        wweights( i ) = wweights( i + 1 )
                     END DO
                     idf_pos( IDF_COUNT ) = 0.0
                     IDF_WAVE( IDF_COUNT ) = 0.0
                     idf_status( IDF_COUNT ) = 0
                     rmses( IDF_COUNT ) = 0.0
                     fits( IDF_COUNT ) = 0.0
                     wweights( IDF_COUNT ) = 1.0
                     IDF_COUNT = IDF_COUNT - 1
                     WRITE ( report_string, 1029 ) id_feature, xpos
                     CALL ECH_REPORT( 0, report_string )
                   END IF
                 END DO
                END IF
                IF ( IDF_COUNT .GT. 0 ) THEN
                 CALL ECH_WAVE_POLYFIT( idf_pos, IDF_WAVE, idf_status,
     :                wweights, rmses, fits, .FALSE., IDF_COUNT, nx,
     :                max_fit_coeffs, .TRUE., wave_npoly,
     :                temp_wave_coeffs, rms )
                END IF
              END IF

*          Else If feature deletion requested then
           ELSE IF ( user_input_char .EQ. 'D' ) THEN
              IF ( id_feature .GT. 0 ) THEN
                 xpos = idf_pos( id_feature )
                 refresh  = .TRUE.
                 DO I = ID_FEATURE, IDF_COUNT
                    IDF_POS( I ) = IDF_POS( I + 1 )
                    IDF_WAVE( I ) = IDF_WAVE( I + 1 )
                    IDF_STATUS( I ) = IDF_STATUS( I + 1 )
                    RMSES( I ) = RMSES( I + 1 )
                    FITS( I ) = FITS( I + 1 )
                    WWEIGHTS( I ) = WWEIGHTS( I + 1 )
                 END DO
                 IDF_POS( IDF_COUNT ) = 0.0
                 IDF_WAVE( IDF_COUNT ) = 0.0
                 IDF_STATUS( IDF_COUNT ) = 0
                 RMSES( IDF_COUNT ) = 0.0
                 FITS( IDF_COUNT ) = 0.0
                 WWEIGHTS( IDF_COUNT ) = 1.0
                 IDF_COUNT = IDF_COUNT - 1
                 CALL ECH_REPORT( 0, ' ' )
                 WRITE ( report_string, 1010 ) feature,xpos
                 CALL ECH_REPORT( 0, report_string )
                 CALL ECH_REPORT( 0, ' ' )
              ELSE
                 CALL ECH_REPORT( 0, ' No feature identified here.' )
              END IF

*          Else If toggle clipping mode requested then
           ELSE IF ( user_input_char .EQ. 'X' ) THEN
              refresh = .FALSE.
              autoclip = .NOT. autoclip
              IF ( autoclip ) THEN
                 CALL ECH_REPORT( 0,
     :                ' Automatic poor line clipping is now enabled.' )
              ELSE
                 CALL ECH_REPORT( 0,
     :                ' Automatic poor line clipping is now disabled.' )
              END IF

*          Else If fit requested then
           ELSE IF ( user_input_char .EQ. 'F' .OR.
     :               user_input_char .EQ. '+' .OR.
     :               user_input_char .EQ. '=' .OR.
     :               user_input_char .EQ. '-'  ) THEN
             IF ( wave_npoly .EQ. 0 ) wave_npoly = 2
             refresh = .FALSE.
             IF ( autoclip ) THEN
                CALL ECH_REPORT( 0,
     :               ' Automatic poor line clipping is enabled.' )
             END IF
              IF ( user_input_char .EQ. '+' ) THEN
                 CALL ECH_FEVAL( fitter, wave_npoly,
     :                temp_wave_coeffs, maximum_poly, 0., 0.,
     :                ECH__INC_NUMCOEFF )
              END IF
              IF ( user_input_char .EQ. '-' ) THEN
                 CALL ECH_FEVAL( fitter, wave_npoly,
     :                temp_wave_coeffs, maximum_poly, 0., 0.,
     :                ECH__DEC_NUMCOEFF )
              END IF
              IF ( user_input_char .EQ. '=' ) THEN
                 CALL ECH_FEVAL( fitter, wave_npoly,
     :                0.0D0, 1, 0., 0., ECH__NEXT_FITTER )
              END IF
              IF ( IDF_COUNT .GT. 0 .AND. wave_npoly .GT. 0 ) THEN
                DO iii = 1, max_fit_coeffs
                   temp_wave_coeffs( iii ) = 0.0
                END DO
                CALL ECH_WAVE_POLYFIT( idf_pos, IDF_WAVE, idf_status,
     :               wweights, rmses, fits, autoclip, IDF_COUNT, nx,
     :               max_fit_coeffs, .TRUE., wave_npoly,
     :               temp_wave_coeffs, rms )
              ELSE
                 CALL ECH_REPORT( 0,
     :                ' No identified features to use in a fit.' )
              END IF

*          Else give a hint
           ELSE
              CALL ECH_REPORT( 0, ' ' )
              CALL ECH_REPORT( 0, ' ' )
              WRITE ( report_string, 1009 ) x_cursor, poss_wavelen
              CALL ECH_REPORT( 0, report_string )
              CALL ECH_REPORT( 0,
     :        ' Move the graphics cursor to the feature of interest,' )
              CALL ECH_REPORT( 0,
     :        ' then press the key for the option required.' )
              CALL ECH_REPORT( 0, ' ' )
              CALL ECH_REPORT( 0,
     :             ' Feature identification options:' )
              CALL ECH_REPORT( 0, ' ' )
              CALL ECH_REPORT( 0,
     :             '   A - Automatic search.' )
              CALL ECH_REPORT( 0,
     :             '   B - Clip possible blends.' )
              CALL ECH_REPORT( 0,
     :             '   C - Clear all identified features.' )
              CALL ECH_REPORT( 0,
     :             '   D - Delete feature.' )
              CALL ECH_REPORT( 0,
     :             '   E - Exit keeping fit.' )
              CALL ECH_REPORT( 0,
     :             '   F - Fit using identified features.' )
              CALL ECH_REPORT( 0,
     :             '   I - Information on nearest feature.' )
              CALL ECH_REPORT( 0,
     :             '   K - Keep identified feature.')
              CALL ECH_REPORT( 0,
     :             '   L - List reference database.' )
              CALL ECH_REPORT( 0,
     :             '   M - Move to specified X-coordinate.' )
              CALL ECH_REPORT( 0,
     :             '   N - New feature specification.' )
              CALL ECH_REPORT( 0,
     :             '   O - Zoom out.' )
              CALL ECH_REPORT( 0,
     :             '   P - Plot/replot graph.' )
              CALL ECH_REPORT( 0,
     :             '   Q - Quit, fit ignored.' )
              CALL ECH_REPORT( 0,
     :             '   R - Re-interpolate all features & fit.' )
              CALL ECH_REPORT( 0,
     :             '   S - Set feature wavelength.')
              CALL ECH_REPORT( 0,
     :             '   T - Set fittable threshold.')
              CALL ECH_REPORT( 0,
     :             '   W - Clip worst line from fit.' )
              CALL ECH_REPORT( 0,
     :             '   X - Toggle auto-clip during fits.' )
              CALL ECH_REPORT( 0,
     :             '   + - Increment degree of fit.' )
              CALL ECH_REPORT( 0,
     :             '   - - Decrement degree of fit.' )
              CALL ECH_REPORT( 0,
     :             '   = - Change type of fit used.' )
              CALL ECH_REPORT( 0,
     :             '   Z - Zoom in on display.' )
              CALL ECH_REPORT( 0,
     :             '   > - Shift window to right.' )
              CALL ECH_REPORT( 0,
     :             '   < - Shift window to left.' )
              CALL ECH_REPORT( 0,
     :             '   ^ - Copy plot to hardcopy.' )
              CALL ECH_REPORT( 0, ' ' )
              refresh = .FALSE.
           END IF

*          Otherwise automatic mode, so accept fit and exit
         ELSE
            STATUS = ECH__SAVE_WFIT
            EDITING = .FALSE.
         END IF
      END DO

      IF ( LOGGING .AND. OPS .GT. 3 ) THEN
         LOGGING = .FALSE.
         CALL ECH_OPEN_FILE( 'ech_id.log', 'UNFORMATTED', 'UNKNOWN',
     :        .TRUE., LUN, LOGNAME, IERR )
         WRITE ( LUN ) LOG
         CALL ECH_OPEN_FILE( ' ', 'CLOSE', ' ', .TRUE., LUN, LOGNAME,
     :        IERR )
      END IF

 1009 FORMAT ( 1X, 'Cursor positioned at X= ',F10.3,
     :               ', and wavelength= ',F10.3 )
 1010 FORMAT ( 1X, 'Deleted feature ',I3,' at X= ',F10.3 )
 1012 FORMAT ( 1X, 'Specify wavelength for feature ',
     :               I3,' at X= ',F10.3 )
 1015 FORMAT ( 1X, 'Deleted ',I3,' feature identifications.' )
 1018 FORMAT ( 1X, 'Reference at wavelength ',F10.3,
     :             ' predicted X=', I5, '.' )
 1019 FORMAT ( 1X, 'Strength factor for thresholding set to ',F10.1 )
 1021 FORMAT ( 1X,
     :        'Cannot alter degree as requested - Limits are 2 to ',I2)
 1022 FORMAT ( 1X, 'Kept ',I3,' manually identified features' )
 1023 FORMAT ( 1X, 'Feature at wavelength ',F10.3,
     :               ' flagged as manually confirmed.' )
 1024 FORMAT ( 1X, 'Clipped feature ',I3,' at X= ',F10.3 )
 1025 FORMAT ( 1X, 'Nearest line in list is at ', F16.5 )
 1028 FORMAT ( 1X, 'Default (hit return to accept) is ',F16.5 )
 1029 FORMAT ( 1X, 'Clipped possible blended feature ',
     :               I3,' at X= ',F10.3 )

      END
