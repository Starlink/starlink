      SUBROUTINE ECH_GET_WAVE_WINDOW(
     :           NX,
     :           NO_OF_FEATURES,
     :           MIN_DISPERSION,
     :           MAX_DISPERSION,
     :           START_WAVELENGTH,
     :           END_WAVELENGTH,
     :           MAX_FEATURES,
     :           FTR_LIST,
     :           MAX_PERM_FTRS,
     :           IDEN_FTR_POSITION,
     :           IDEN_FTR_WAVELENGTH,
     :           START_WAVELENGTH_INDEX,
     :           END_WAVELENGTH_INDEX,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_GET_WAVE_WINDOW

*  Purpose:
*     Set search limits in wavelength.

*  Description:
*     This routine sets the search limits for a single-order search in
*     terms of indices in the feature database, and min/max dispersion.

*  Invocation:
*     CALL ECH_GET_WAVE_WINDOW(
*     :    NX,
*     :    NO_OF_FEATURES,
*     :    MIN_DISPERSION,
*     :    MAX_DISPERSION,
*     :    START_WAVELENGTH,
*     :    END_WAVELENGTH,
*     :    MAX_FEATURES,
*     :    FTR_LIST,
*     :    MAX_PERM_FTRS,
*     :    IDEN_FTR_POSITION,
*     :    IDEN_FTR_WAVELENGTH,
*     :    START_WAVELENGTH_INDEX,
*     :    END_WAVELENGTH_INDEX,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NO_OF_FEATURES = INTEGER (Given)
*        Number of identified features.
*     MIN_DISPERSION = INTEGER (Given and Returned)
*        Minimum dispersion.
*     MAX_DISPERSION = INTEGER (Given and Returned)
*        Maximum dispersion.
*     START_WAVELENGTH = REAL (Given)
*        Start search window wavelength.
*     END_WAVELENGTH = REAL (Given)
*        End search window wavelength.
*     MAX_FEATURES = INTEGER (Given)
*        Maximum number of observed features per order.
*     FTR_LIST = REAL (Given)
*        List of known arc line wavelengths.
*     MAX_PERM_FTRS = INTEGER (Given)
*        Maximum number of features per order.
*     IDEN_FTR_POSITION = REAL (Given and Returned)
*        Positions identified features.
*     IDEN_FTR_WAVELENGTH = REAL (Given and Returned)
*        Wavelengths of identified features.
*     START_WAVELENGTH_IND = INTEGER (Given and Returned)
*        Index into arc line list.
*     END_WAVELENGTH_INDEX = INTEGER (Given)
*        Index into arc line list.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     23-MAY-1997 (MJC):
*       Removed a divide-by-zero bug, fixed error in dispersion
*       constraints.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NO_OF_FEATURES
      INTEGER END_WAVELENGTH_INDEX ! Highest entry in ftr_list to check.
      INTEGER MAX_PERM_FTRS
      REAL START_WAVELENGTH
      REAL END_WAVELENGTH
      INTEGER MAX_FEATURES
      REAL FTR_LIST( MAX_FEATURES )

*  Arguments Returned:
      INTEGER START_WAVELENGTH_INDEX      ! First entry in ftr_list to check.
      REAL IDEN_FTR_POSITION( MAX_PERM_FTRS ) ! Identified feature positions.
      REAL IDEN_FTR_WAVELENGTH( MAX_PERM_FTRS ) ! Feature wavelengths.
      REAL MIN_DISPERSION
      REAL MAX_DISPERSION

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL EST_MIN_DISPERSION
      REAL EST_MAX_DISPERSION
      REAL SCALE
      REAL TEMP
      REAL MIN_ID_POS
      REAL MAX_ID_POS
      REAL MIN_ID_WAVE
      REAL MAX_ID_WAVE

      INTEGER I
      INTEGER NCHAR1
      INTEGER NCHAR2

      CHARACTER*16 REF_STR1
      CHARACTER*16 REF_STR2

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Look for features already identified.
      MIN_ID_POS = 1.0E20
      MAX_ID_POS = -1.0E20
      MIN_ID_WAVE = 0.0
      MAX_ID_WAVE = 0.0
      DO I = MAX_PERM_FTRS, 1, -1
         IF ( IDEN_FTR_POSITION( I ) .GT. 0.0 ) THEN
            IF ( MIN_ID_POS .GT. IDEN_FTR_POSITION( I ) ) THEN
               MIN_ID_WAVE = IDEN_FTR_WAVELENGTH( I )
               MIN_ID_POS = IDEN_FTR_POSITION( I )
            END IF
            IF ( MAX_ID_POS .LT. IDEN_FTR_POSITION( I ) ) THEN
               MAX_ID_WAVE = IDEN_FTR_WAVELENGTH( I )
               MAX_ID_POS = IDEN_FTR_POSITION( I )
            END IF
         END IF
      END DO
      IF ( MIN_ID_WAVE .GT. 0.0 .AND.
     :     MAX_ID_POS .NE. MIN_ID_POS ) THEN
         SCALE = ( MAX_ID_WAVE - MIN_ID_WAVE ) /
     :           ( MAX_ID_POS - MIN_ID_POS )
         START_WAVELENGTH = MIN_ID_WAVE - SCALE * MIN_ID_POS
         END_WAVELENGTH = START_WAVELENGTH  + SCALE * FLOAT( NX )
         IF ( START_WAVELENGTH .GT. END_WAVELENGTH ) THEN
            TEMP = START_WAVELENGTH
            START_WAVELENGTH = END_WAVELENGTH
            END_WAVELENGTH = TEMP
         END IF
      END IF

*  If the wavelength-window start-point has not yet been set then
*  set it to minimum allowed, and zero start index.
      IF ( INT( START_WAVELENGTH ) .EQ. 0 ) THEN
         START_WAVELENGTH_INDEX = 0
         START_WAVELENGTH = ABS_MIN_WAVELENGTH
      END IF

*  If the wavelength-window end-point has not yet been set then
*  set it the to maximum allowed, and zero end index.
      IF ( INT( END_WAVELENGTH ) .EQ. 0 ) THEN
         END_WAVELENGTH_INDEX = 0
         END_WAVELENGTH = ABS_MAX_WAVELENGTH
      ELSE

*  Estimate min/max dispersions using the wavelength window
*  and adopt these if more stringent than what we have already.
         IF ( START_WAVELENGTH .NE. END_WAVELENGTH ) THEN
            EST_MIN_DISPERSION = 0.5 * FLOAT( NX ) /
     :            ( END_WAVELENGTH - START_WAVELENGTH )
            EST_MAX_DISPERSION = 4.0 * FLOAT( NX ) /
     :            ( END_WAVELENGTH - START_WAVELENGTH )
            MIN_DISPERSION = MAX( EST_MIN_DISPERSION, MIN_DISPERSION )
            MAX_DISPERSION = MIN( EST_MAX_DISPERSION, MAX_DISPERSION )
         END IF
      END IF

*  Widen window a bit.
      START_WAVELENGTH = MAX( 0.0, START_WAVELENGTH - 10.0 )
      END_WAVELENGTH = END_WAVELENGTH + 10.0
      START_WAVELENGTH_INDEX = 1

*  Loop though list of features finding window limits.
      DO I = 1, NO_OF_FEATURES
         IF ( FTR_LIST( I ) .LE. START_WAVELENGTH ) THEN
            START_WAVELENGTH_INDEX = I

         ELSE IF ( END_WAVELENGTH .GE. FTR_LIST( I ) ) THEN
            END_WAVELENGTH_INDEX = I
         END IF
      END DO

*  Report feature list limits being used.
      CALL CHR_RTOC( FTR_LIST( START_WAVELENGTH_INDEX ), REF_STR1,
     :     NCHAR1 )
      CALL CHR_RTOC( FTR_LIST( END_WAVELENGTH_INDEX ), REF_STR2,
     :     NCHAR2 )
      REPORT_STRING = ' Search ref. feature list between ' //
     :      REF_STR1( :NCHAR1 ) // ' and ' //
     :      REF_STR2( :NCHAR2 ) // '.'
      CALL ECH_REPORT( 0, REPORT_STRING )

      END
