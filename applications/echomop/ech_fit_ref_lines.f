      SUBROUTINE ECH_FIT_REF_LINES(
     :           EXTRACTED_REF,
     :           NX,
     :           MAX_PERM_LINES,
     :           LINE_WIDTH,
     :           LINE_POSITION,
     :           LINE_INTENSITY,
     :           CONTINUUM,
     :           IORD,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_FIT_REF_LINES

*  Purpose:
*     Fit gaussians to find arc-line central positions.

*  Description:
*     This routine fits gaussians to the roughly located reference
*     lines to be used for wavelength calibration purposes.
*     Generally the reference spectrum will be an ARC lamp spectrum,
*     but in principle this routine could be used with a sky-line
*     rich spectrum, or even an object with strong emission lines
*     (subject to user provision of an applicable line list).
*     In the normal (ARC-lamp) case, this routine will be called
*     once for each data frame.  Usually there will be two ARC-lamp
*     frames (bracketing the object exposure).

*  Invocation:
*     CALL ECH_FIT_REF_LINES(
*     :    EXTRACTED_REF,
*     :    NX,
*     :    MAX_PERM_LINES,
*     :    LINE_WIDTH,
*     :    LINE_POSITION,
*     :    LINE_INTENSITY,
*     :    CONTINUUM,
*     :    STATUS
*     :   )

*  Arguments:
*     EXTRACTED_REF = REAL (Given)
*        Reference spectrum estimate columns and ny rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     MAX_PERM_LINES = INTEGER (Given)
*        Maximum number of lines per order to allow.
*     LINE_WIDTH = REAL (Given)
*        Approximate line width in pixels.
*     LINE_POSITION = REAL (Given and Returned)
*        X position of lines (peak value).
*     LINE_INTENSITY = REAL (Given and Returned)
*        Average Peak intensity of lines.
*     CONTINUUM = REAL (Given and Returned)
*        Estimated continuum intensity.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     05-JUN-1997 (MJC):
*       Introduced reverse fit test.
*       I tried flipping a dataset in X and found that some lines
*       were accepted with the reversed spectrum.  As a fudge I have
*       changed the criterion for rejection to be that 'a fit cannot
*       be found to either the data, or the reflected data'.  This
*       accepts results in more lines being accepted - this is
*       important as some of the lines being rejected were valid and
*       their rejection made wavelength calibration difficult.  I
*       suspect a problem in the gaussian fitter but do not have time
*       to investigate.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER MAX_PERM_LINES
      REAL LINE_WIDTH
      REAL EXTRACTED_REF( NX )

*  Arguments Returned:
      REAL LINE_POSITION( MAX_PERM_LINES )
      REAL LINE_INTENSITY( MAX_PERM_LINES )

*  Workspace:
      REAL CONTINUUM( NX )
      INTEGER IORD

*  Status:
      INTEGER STATUS

*  Local variables:
      REAL FIT_DATA( 100 )
      REAL FIT_ERROR( 100 )
      REAL FIT_PARS( 3 )
      REAL FIT_VAR( 3 )
      REAL LOCAL_MIN
      REAL PEAK_VALUE
      REAL SWAP

      INTEGER ILINE_WIDTH
      INTEGER LINE_COUNT
      INTEGER ICOPY
      INTEGER X_AT_PEAK
      INTEGER X_DELTA
      INTEGER XBOX
      INTEGER IX
      INTEGER DCOUNT
      INTEGER CCOUNT
      INTEGER DUCOUNT

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) )  RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Loop through all line candidates found.
      LINE_COUNT = 1
      DO WHILE ( LINE_POSITION( LINE_COUNT ) .GT. 0.0 .AND.
     :           LINE_COUNT .LT. MAX_PERM_LINES  )

*     Resample line locality of line-width+5 pixels (in x).
*     Determine line pixel-coords.
         IX = INT( LINE_POSITION( LINE_COUNT ) )

*     Loop Sampling line-width+5 values around estimated line position.
         X_AT_PEAK = 0
         ILINE_WIDTH = MAX( INT( LINE_WIDTH ), 1 )
         XBOX = ILINE_WIDTH + 5
         LOCAL_MIN = 1.0E20
         DO X_DELTA = IX - XBOX, IX + XBOX
            IF ( 0 .LT. X_DELTA .AND. X_DELTA .LE. NX ) THEN

*           If lowest value so far, remember it.
               IF ( EXTRACTED_REF( X_DELTA ) .LT. LOCAL_MIN )
     :            LOCAL_MIN = EXTRACTED_REF( X_DELTA )
               IF ( CONTINUUM( X_DELTA ) .LT. LOCAL_MIN )
     :            LOCAL_MIN = CONTINUUM( X_DELTA )
            END IF
         END DO

*     Copy local values into data array for fitting.
         DCOUNT = 0
         DUCOUNT = 0
         PEAK_VALUE = -1.0E20
         X_AT_PEAK = 0
         DO X_DELTA = -XBOX, XBOX
            DCOUNT = DCOUNT + 1
            FIT_DATA( DCOUNT ) = 0.0
            FIT_ERROR( DCOUNT ) = 1.0
            IF ( IX + X_DELTA .GT. 0 .AND. IX + X_DELTA .LE. NX ) THEN
               IF ( ABS( X_DELTA ) .LE. MAX( 3, ILINE_WIDTH ) ) THEN
                  FIT_DATA( DCOUNT ) = EXTRACTED_REF( IX + X_DELTA ) -
     :                  LOCAL_MIN
                  FIT_ERROR( DCOUNT ) = 1.0 /
     :                  MAX( 1.0, FIT_DATA( DCOUNT ) )
                  DUCOUNT = DUCOUNT + 1
                  IF ( FIT_DATA( DCOUNT ) .GT. PEAK_VALUE ) THEN
                     PEAK_VALUE = FIT_DATA( DCOUNT )
                     X_AT_PEAK = DCOUNT
                  END IF
               END IF
            END IF
         END DO
         CCOUNT = ( DCOUNT + 1 ) / 2
         PEAK_VALUE = MAX( FIT_DATA( CCOUNT - 1 ),
     :         FIT_DATA( CCOUNT ), FIT_DATA( CCOUNT + 1 ) )
         DO X_DELTA = 2, XBOX
            IF ( FIT_DATA( CCOUNT - X_DELTA ) .GT. PEAK_VALUE )
     :         FIT_DATA( CCOUNT - X_DELTA ) = LOCAL_MIN
            IF ( FIT_DATA( CCOUNT + X_DELTA ) .GT. PEAK_VALUE )
     :         FIT_DATA( CCOUNT + X_DELTA ) = LOCAL_MIN
         END DO

*     Try to fit a gaussian of width line_width to the data.
         IF ( DUCOUNT .GT. 1 ) THEN
            FIT_PARS( 1 ) = FIT_DATA( INT( ( DCOUNT + 1 ) / 2 ) )
            FIT_PARS( 2 ) = FLOAT( DCOUNT + 1 ) / 2.0
            FIT_PARS( 3 ) = 2.0 * LINE_WIDTH / 2.35
            STATUS = 0
            CALL ECH_FIT_GAUSSIAN( FIT_DATA, FIT_ERROR, DCOUNT,
     :           FIT_PARS, FIT_VAR, 3, STATUS )

*        If too far from first guess, reject it.
            IF ( ABS( FLOAT( DCOUNT + 1 ) / 2.0 - FIT_PARS( 2 ) ) .GT.
     :           2.0 )
     :         STATUS = ECH__NO_DATA

*        If fit failed first time, try reversing data and refit.
            IF ( STATUS .NE. 0 .OR.
     :           FIT_PARS( 3 ) .GT. LINE_WIDTH * 3.0 ) THEN
               DO X_DELTA = 1, DCOUNT / 2
                  SWAP = FIT_DATA( X_DELTA )
                  FIT_DATA( X_DELTA ) = FIT_DATA( DCOUNT - X_DELTA + 1 )
                  FIT_DATA( DCOUNT - X_DELTA + 1 ) = SWAP
               END DO
               FIT_PARS( 1 ) = FIT_DATA( INT( ( DCOUNT + 1 ) / 2 ) )
               FIT_PARS( 2 ) = FLOAT( DCOUNT + 1 ) / 2.0
               FIT_PARS( 3 ) = 2.0 * LINE_WIDTH / 2.35
               STATUS = 0
               CALL ECH_FIT_GAUSSIAN( FIT_DATA, FIT_ERROR, DCOUNT,
     :              FIT_PARS, FIT_VAR, 3, STATUS )
               FIT_PARS( 2 ) = FLOAT( DCOUNT + 1 ) - FIT_PARS( 2 )
            END IF

         ELSE
            STATUS = ECH__NO_DATA
         END IF

*     If we have obtained at least a marginal fit.
         IF ( STATUS .EQ. 0 .AND.
     :        FIT_PARS( 3 ) .LE. LINE_WIDTH * 3.0 ) THEN
            IF ( FIT_PARS( 3 ) .GT. 0.0 .AND.
     :           FIT_PARS( 1 ) .GT. 0.0 ) THEN

*           Replace observed peak intensity with fitted one.
               LINE_INTENSITY( LINE_COUNT ) = FIT_PARS( 1 )
            END IF

*        Replace line x-position with fitted centre.
            X_AT_PEAK = X_AT_PEAK - XBOX + 1 + IX
            LINE_POSITION( LINE_COUNT ) = FIT_PARS( 2 ) -
     :            FLOAT( XBOX + 1 ) + FLOAT( IX )

         ELSE

*        Report rejection and remove from candidate list.
            WRITE ( REPORT_STRING, 1000 ) IX
            CALL ECH_REPORT ( 0, REPORT_STRING )
            ICOPY = LINE_COUNT + 1
            DO WHILE ( LINE_POSITION( ICOPY - 1 ) .GT. 0.0 .AND.
     :                 ICOPY .LE. MAX_PERM_LINES )
               LINE_POSITION( ICOPY - 1 ) = LINE_POSITION( ICOPY )
               LINE_INTENSITY( ICOPY - 1 ) = LINE_INTENSITY( ICOPY )
               ICOPY = ICOPY + 1
            END DO
            LINE_COUNT = LINE_COUNT - 1
         END IF

*     Increment lines located counter.
         LINE_COUNT = LINE_COUNT + 1
      END DO

      LINE_COUNT = LINE_COUNT - 1

 1000 FORMAT ( 1X, 'Rejected line candidate at X= ', I4, '.' )

      END
