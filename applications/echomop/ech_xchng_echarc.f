      SUBROUTINE ECH_XCHNG_ECHARC (
     :           OPTION,
     :           NX,
     :           LUN_ECHARC,
     :           N_ORDERS,
     :           MAX_PERM_FTRS,
     :           IDENTIFIED_FTRS,
     :           IDEN_FTR_POSITION,
     :           IDEN_FTR_WAVELENGTH,
     :           IDEN_FTR_STATUS,
     :           MAX_NPOLY,
     :           WAVE_FIT_DELTA,
     :           EXTRACTED_REF,
     :           ECHARC_X_LABEL,
     :           ECHARC_Y_LABEL,
     :           ECHARC_Z_LABEL,
     :           ECHARC_X_DATA,
     :           ECHARC_Y_DATA,
     :           ECHARC_Z_DATA,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_XCHNG_ECHARC

*  Purpose:
*     Manages data import/export to ECHARC.

*  Description:
*     This routine handles data export/import between ECHOMOP
*     and the FIGARO based ARC/ECHARC arc-line identification
*     programs.

*  Invocation:
*     CALL ECH_XCHNG_ECHARC(
*     :    OPTION,
*     :    NX,
*     :    LUN_ECHARC,
*     :    N_ORDERS,
*     :    MAX_PERM_FTRS,
*     :    IDENTIFIED_FTRS,
*     :    IDEN_FTR_POSITION,
*     :    IDEN_FTR_WAVELENGTH,
*     :    IDEN_FTR_STATUS,
*     :    MAX_NPOLY,
*     :    WAVE_FIT_DELTA,
*     :    EXTRACTED_REF,
*     :    ECHARC_X_LABEL,
*     :    ECHARC_Y_LABEL,
*     :    ECHARC_Z_LABEL,
*     :    ECHARC_X_DATA,
*     :    ECHARC_Y_DATA,
*     :    ECHARC_Z_DATA,
*     :    STATUS
*     :   )

*  Arguments:
*     OPTION = CHAR (Given)
*        LOAD or SAVE option.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     MAX_NPOLY = INTEGER (Given)
*        Maximum degree of wavelength polynomials.
*     MAX_PERM_FTRS = INTEGER (Given)
*        Maximum permitted features per order.
*     LUN_ECHARC = INTEGER (Given)
*        Logical unit number of exchange ASCII file.
*     WAVE_FIT_DELTA = REAL (Given)
*        Fit deviation level.
*     EXTRACTED_REF = REAL (Given)
*        Extracted reference spectrum.
*     IDENTIFIED_FTRS = INTEGER (Returned)
*        Count of identified features per order.
*     IDEN_FTR_POSITION = REAL (Returned)
*        X-coordinates of identified features.
*     IDEN_FTR_STATUS = INTEGER (Returned)
*        Status flags for identified features.
*     IDEN_FTR_WAVELENGTH = REAL (Returned)
*        Wavelengths of identified features.
*     ECHARC_X_LABEL = CHAR (Given)
*        Label for X axis data object.
*     ECHARC_Y_LABEL = CHAR (Given)
*        Label for Y axis data object.
*     ECHARC_Z_LABEL = CHAR (Given)
*        Label for Z axis data object.
*     ECHARC_X_DATA = REAL (Given)
*        Data for X object (wavelengths).
*     ECHARC_Y_DATA = INTEGER (Given)
*        Data for Y object (order numbers).
*     ECHARC_Z_DATA = REAL (Given)
*        Data for Z object (arc spectrum).
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     16-MAY-1997 (MJC):
*       Added prologue.
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
      INTEGER MAX_PERM_FTRS
      INTEGER N_ORDERS
      INTEGER IDENTIFIED_FTRS( N_ORDERS )
*          ! Count of identified features.
      REAL EXTRACTED_REF ( NX, N_ORDERS )
      INTEGER MAX_NPOLY
      INTEGER LUN_ECHARC
      REAL WAVE_FIT_DELTA
      CHARACTER*( * ) ECHARC_X_LABEL
      CHARACTER*( * ) ECHARC_Y_LABEL
      CHARACTER*( * ) ECHARC_Z_LABEL
      REAL ECHARC_X_DATA( NX, N_ORDERS )
*          ! Data for X  object (wavelengths)
      INTEGER ECHARC_Y_DATA( N_ORDERS )
*          ! Data for Y  object (order numbers)
      REAL ECHARC_Z_DATA( NX, N_ORDERS )
*          ! Data for Z  object (arc spectrum)

*  Arguments Returned:
      CHARACTER*( * ) OPTION
      REAL IDEN_FTR_POSITION( MAX_PERM_FTRS, N_ORDERS )
*          ! X-coordinates of identified features.
      INTEGER IDEN_FTR_STATUS( MAX_PERM_FTRS, N_ORDERS )
*          ! Status flags for identified features.
      REAL IDEN_FTR_WAVELENGTH( MAX_PERM_FTRS,N_ORDERS )
*          ! Wavelengths of identified features.

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I
      INTEGER IX

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  If LOAD option specified.
      IF ( OPTION .EQ. 'LOAD' ) THEN
         CALL ECH_IMPORT_ECHARC( LUN_ECHARC, 'ECHARC.LIS',
     :        MAX_PERM_FTRS, .TRUE., N_ORDERS, IDEN_FTR_POSITION,
     :        IDEN_FTR_WAVELENGTH, IDENTIFIED_FTRS, IDEN_FTR_STATUS,
     :        MAX_NPOLY, WAVE_FIT_DELTA, STATUS )

*  If SAVE option specified.
      ELSE IF ( OPTION .EQ. 'SAVE' ) THEN
         ECHARC_X_LABEL = 'WAVELENGTH'
         ECHARC_Y_LABEL = 'ORDER NUMBER'
         ECHARC_Z_LABEL = 'ARC SPECTRUM'
         DO I = 1, N_ORDERS
            ECHARC_Y_DATA( I ) = I
            DO IX = 1, NX
               ECHARC_Z_DATA( IX, I ) = EXTRACTED_REF( IX, I )
            END DO
         END DO

         CALL ECH_IMPORT_ECHARC( LUN_ECHARC, 'ECHARC.LIS',
     :        MAX_PERM_FTRS, .FALSE., N_ORDERS, IDEN_FTR_POSITION,
     :        IDEN_FTR_WAVELENGTH, IDENTIFIED_FTRS, IDEN_FTR_STATUS,
     :        MAX_NPOLY, WAVE_FIT_DELTA, STATUS )
         CALL ECH_EXPORT_ECHARC( LUN_ECHARC, 'ECHARC.LIS', N_ORDERS,
     :        IDENTIFIED_FTRS, MAX_PERM_FTRS, IDEN_FTR_POSITION,
     :        IDEN_FTR_WAVELENGTH, IDEN_FTR_STATUS, MAX_NPOLY,
     :        WAVE_FIT_DELTA, STATUS )
      END IF

      END
