      SUBROUTINE ECH_CHECK_ID_FTRS(
     :           NX,
     :           INTERACTIVE,
     :           MAX_PERM_FTRS,
     :           IPF_POS,
     :           OBS_STRENGTH,
     :           WAVE_NPOLY,
     :           MAXIMUM_POLY,
     :           FITTER,
     :           TEMP_WAVE_COEFFS,
     :           IDF_COUNT,
     :           IDF_POS,
     :           IDF_STAT,
     :           IDF_WAVE,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_CHECK_ID_FTRS

*  Purpose:
*     Check consistency of located/identified line sets.

*  Description:
*     This routine takes the current set of located lines, and the current
*     set of identified lines and checks that they are consistent. Ie, that
*     the identified lines positions are present in the list of located
*     lines. If this is not the case then the presumption is that the
*     identifications are correct and that a new set of positions has been
*     supplied (for a new arc frame). Each identified line is then matched
*     with the nearest located line position, and a re-fit performed.

*  Invocation:
*     CALL ECH_CHECK_ID_FTRS(
*     :    NX,
*     :    INTERACTIVE,
*     :    MAX_PERM_FTRS,
*     :    IPF_POS,
*     :    OBS_STRENGTH,
*     :    WAVE_NPOLY,
*     :    MAXIMUM_POLY,
*     :    FITTER,
*     :    TEMP_WAVE_COEFFS,
*     :    IDF_COUNT,
*     :    IDF_POS,
*     :    IDF_STAT,
*     :    IDF_WAVE,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     EXTRACTED_REF = REAL (Given)
*        Extracted reference spectrum.
*     INTERACTIVE = LOGICAL (Given)
*        TRUE if interactive mode is used.
*     ORDER = INTEGER (Given)
*        Number of order being processed.
*     MAX_PERM_FTRS = INTEGER (Given)
*        Maximum number of features to use in a fit.
*     IPF_POS = REAL (Given)
*        Observed central positions of features.
*     OBS_STRENGTH = REAL (Given)
*        Observed strengths (usually intensity) of features.
*     WAVE_NPOLY = INTEGER (Given)
*        Default order of wavelength polynomial fit.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum order of wavelength polynomial fit.
*     TEMP_WAVE_COEFFS = DOUBLE (Returned)
*        Wavelength polynomial coefficients.
*     IDF_COUNT = INTEGER (Returned)
*        Count of identified features.
*     IDF_POS = REAL (Returned)
*        Positions identified features.
*     IDF_STAT = INTEGER (Returned)
*        Statuses of identified features.
*     IDF_WAVE = REAL (Returned)
*        Wavelengths of identified features.
*     FITTER = CHAR (Given)
*        Type of wavelength fitting function in use (POLY/SPLINE).
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     01-JUN-1997 (MJC):
*       Tidy up, add prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_FEATURE.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'

*  Arguments Returned:
      INTEGER NX
      LOGICAL INTERACTIVE
      INTEGER MAX_PERM_FTRS
      REAL IPF_POS( MAX_PERM_FTRS ) ! Observed central positions of features.
      REAL OBS_STRENGTH( MAX_PERM_FTRS )
*          ! Observed strengths (usually intensity) of features.
      INTEGER WAVE_NPOLY
      INTEGER MAXIMUM_POLY
      CHARACTER*( * ) FITTER
      INTEGER IDF_COUNT

*  Arguments Given:
      DOUBLE PRECISION TEMP_WAVE_COEFFS( MAXIMUM_POLY )
*           ! Wavelength polynomial coefficients.
      REAL IDF_POS( MAX_PERM_FTRS ) ! Positions of identified features.
      REAL IDF_WAVE( MAX_PERM_FTRS ) ! Wavelengths of identified features.
      INTEGER IDF_STAT( MAX_PERM_FTRS ) ! Statuses of identified features.

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL WWEIGHTS( MAX_FIT_FTRS )
      REAL RMSES( MAX_FIT_FTRS )
      REAL FITS( MAX_FIT_FTRS )
      REAL FTR_POSITIONS( MAX_ALLOWED_RF_FEAT )
*          ! Temporary storage for feature positions.
      REAL RMS
      REAL DIFF
      REAL ADIFF
      REAL MEAN_DIFF
      REAL MEANSQ_DIFF
      REAL SIGMA_DIFF
      REAL BEST_MEAN
      REAL AVG

      INTEGER I
      INTEGER II
      INTEGER NO_OF_POSITIONS
      INTEGER LOW_NCOEFF
      INTEGER NPOLY
      INTEGER BEST_OFFSET
      INTEGER OFFSET
      INTEGER IMODIFY
      INTEGER MATCHES

      LOGICAL AUTOCLIP


*  Functions Called:
      EXTERNAL ECH_FATAL_ERROR
      LOGICAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  If any identified features present.
      IF ( IDF_COUNT .GT. 0 ) THEN

*     Count number of observed features for which positions could be obtained.
         NO_OF_POSITIONS = 0
         DO WHILE ( IPF_POS( NO_OF_POSITIONS + 1 ) .GT. 0.0
     :              .AND. NO_OF_POSITIONS + 1 .LT. MAX_PERM_FTRS )
            NO_OF_POSITIONS = NO_OF_POSITIONS + 1
         END DO

*     Count those whose positions are in the original 'located' list.
         MATCHES = 0
         DO I = 1, IDF_COUNT
            DO II = NO_OF_POSITIONS, 1, -1
               IF ( IPF_POS( II ) .GT. 0 .AND.
     :              IPF_POS( II ) .EQ. IDF_POS( I ) ) THEN
                  MATCHES = MATCHES + 1
               END IF
            END DO
         END DO

*     If less than half the identified lines have matches.
         IF ( MATCHES .LT. IDF_COUNT / 2 ) THEN
            CALL ECH_REPORT( 0, ' ' )
            CALL ECH_REPORT( 0, ' Positions of located lines do not' //
     :           ' match currently identified lines.' )
            CALL ECH_REPORT( 0, ' Refitting using lines nearest to' //
     :           ' identified positions.' )

*        Find nearest located lines to each identified line position
*        Calculate mean and sigma of differences between the two sets.
            BEST_OFFSET = 0
            BEST_MEAN = 1.0E20
            DO OFFSET = -25, 25
               MEAN_DIFF = 0.0
               MEANSQ_DIFF = 0.0
               DO I = 1, IDF_COUNT
                  DIFF = 1.0E20
                  DO II = 1, NO_OF_POSITIONS
                     IF ( IPF_POS( II ) .GT. 0 .AND.
     :                    ABS( IPF_POS( II ) - IDF_POS( I ) +
     :                    FLOAT( OFFSET ) ) .LT. DIFF ) THEN
                        DIFF = ABS( IPF_POS( II ) -
     :                        IDF_POS( I ) + FLOAT( OFFSET ) )
                        FTR_POSITIONS( I ) = IPF_POS( II )
                     END IF
                  END DO
                  MEAN_DIFF = MEAN_DIFF + DIFF
                  MEANSQ_DIFF = MEANSQ_DIFF + DIFF * DIFF
               END DO
               MEAN_DIFF = MEAN_DIFF / FLOAT( IDF_COUNT )
               MEANSQ_DIFF = MEANSQ_DIFF / FLOAT( IDF_COUNT )
               SIGMA_DIFF = SQRT( ABS( MEANSQ_DIFF -
     :               MEAN_DIFF * MEAN_DIFF ) )
               IF ( MEAN_DIFF .LT. BEST_MEAN ) THEN
                  BEST_MEAN = MEAN_DIFF
                  BEST_OFFSET = OFFSET
               END IF
            END DO

            OFFSET = BEST_OFFSET
            MEAN_DIFF = 0.0
            AVG = 0.0
            MEANSQ_DIFF = 0.0
            DO I = 1, IDF_COUNT
               ADIFF = 1.0E20
               DO II = 1, NO_OF_POSITIONS
                  IF ( IPF_POS( II ) .GT. 0 .AND.
     :                 ABS( IPF_POS( II ) - IDF_POS( I ) +
     :                 FLOAT( OFFSET ) ) .LT. ADIFF ) THEN
                     DIFF = IPF_POS( II ) - IDF_POS( I ) +
     :                     FLOAT( OFFSET )
                     ADIFF = ABS( DIFF )
                     FTR_POSITIONS( I ) = IPF_POS( II )
                  END IF
               END DO
               AVG = AVG + DIFF
               MEAN_DIFF = MEAN_DIFF + ADIFF
               MEANSQ_DIFF = MEANSQ_DIFF + ADIFF * ADIFF
            END DO
            AVG = AVG / FLOAT( IDF_COUNT )
            MEAN_DIFF = MEAN_DIFF / FLOAT( IDF_COUNT )
            MEANSQ_DIFF = MEANSQ_DIFF / FLOAT( IDF_COUNT )
            SIGMA_DIFF = MAX( SQRT( ABS( MEANSQ_DIFF -
     :            MEAN_DIFF * MEAN_DIFF ) ), 1.0E-5 )
            WRITE ( REPORT_STRING, 1002 ) AVG - FLOAT( OFFSET )
            CALL ECH_REPORT( 0, REPORT_STRING )

*        Replace identified lines positions using those from the nearest
*        matching located lines, if match is close enough (3 sigma).
            IMODIFY = 0
            DO I = 1, IDF_COUNT
               IF ( ABS( FTR_POSITIONS( I ) -
     :              IDF_POS( I ) + FLOAT( OFFSET ) )
     :              .LT. MEAN_DIFF + 3 * SIGMA_DIFF ) THEN
                  IMODIFY = IMODIFY + 1
                  WRITE ( REPORT_STRING, 1000 )
     :                  IDF_POS( I ), FTR_POSITIONS( I )
                  CALL ECH_REPORT( 0, REPORT_STRING )
                  IDF_POS( IMODIFY ) = FTR_POSITIONS( I )
                  IDF_WAVE( IMODIFY ) = IDF_WAVE( I )
                  IDF_STAT( IMODIFY ) = IDF_STAT( I )
                  WWEIGHTS( IMODIFY ) = 1.0

               ELSE
                  WRITE ( REPORT_STRING, 1001 ) IDF_POS( I )
                  CALL ECH_REPORT( 0, REPORT_STRING )
               END IF
            END DO

            IDF_COUNT = IMODIFY
            DO I = IDF_COUNT + 1, MAX_ID_FTRS
               IDF_POS( I ) = 0.0
               IDF_WAVE( I ) = 0.0
               IDF_STAT( I ) = 0
            END DO

*        Refit polynomial to identified lines.
            NPOLY = WAVE_NPOLY
            LOW_NCOEFF = 0
            DO I = MAXIMUM_POLY, 1, -1
               IF ( TEMP_WAVE_COEFFS( I ) .NE. 0.0 .AND.
     :              LOW_NCOEFF .EQ. 0 ) LOW_NCOEFF = I
            END DO
            IF ( FITTER .EQ. 'SPLINE' ) LOW_NCOEFF = LOW_NCOEFF + 4
            IF ( LOW_NCOEFF .GT. 0 ) NPOLY = LOW_NCOEFF
            DO I = MAXIMUM_POLY, 1, -1
               TEMP_WAVE_COEFFS ( I ) = 0.0
            END DO
            RMS = 1.0E6
            IF ( IDF_COUNT .GT. 0 )
     :         CALL ECH_WAVE_POLYFIT( IDF_POS, IDF_WAVE, IDF_STAT,
     :              WWEIGHTS, RMSES, FITS, AUTOCLIP, IDF_COUNT,
     :              NX, MAXIMUM_POLY, .TRUE., NPOLY, TEMP_WAVE_COEFFS,
     :              RMS )
            IF ( IDF_COUNT .GT. 0 ) STATUS = ECH__ID_DONE
         END IF
      END IF

 1000 FORMAT ( 1X, 'Line position modified : was ',F12.3,
     :         ', new position = ',F12.3 )
 1001 FORMAT ( 1X, 'Line position has no clear match : was ',F12.3,
     :         ', Line has been clipped' )
 1002 FORMAT ( 1X, 'Mean position offset from previous fit is ',
     :         F7.2,' pixels' )

      END
