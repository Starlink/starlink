      SUBROUTINE ECH_TUNE_BATCH( STATUS )
*+
*  Name:
*     ECHOMOP - ECH_TUNE_BATCH

*  Purpose:
*     Adjust parameters for batch operation

*  Description:
*     This routine automatically adjusts tuning parameters for a comprehensive
*     batch extraction run.

*  Invocation:
*     CALL ECH_TUNE_BATCH( STATUS )

*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'

*  Status:
      INTEGER Status

*  Functions Called:
      EXTERNAL ECH_FATAL_ERROR
      LOGICAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      CALL ECH_REPORT( 0, ' Setting parameters for BATCH operation.' )
      CALL ECH_SET_PARAMETER( '2D_INTERACT', 'KEY',
     :     0.0, .FALSE., ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'DISPLAY', 'KEY',
     :     0.0, .FALSE., ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'AUTO_ID', 'KEY',
     :     0.0, .TRUE., ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'PFL_INTERACT', 'KEY',
     :     0.0, .FALSE., ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'TRC_INTERACT', 'KEY',
     :     0.0, .FALSE., ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'TRC_VETO', 'KEY',
     :     0.0, .FALSE., ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'TUNE_AUTLOC', 'KEY',
     :     0.0, .TRUE., ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'TUNE_CRCLEAN', 'KEY',
     :     0.0, .FALSE., ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'TUNE_FCHECK', 'KEY',
     :     0.0, .FALSE., ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'TUNE_FFINTER', 'KEY',
     :     0.0, .FALSE., ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'TUNE_REVCHK', 'KEY',
     :     0.0, .FALSE., ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'TUNE_CRTRC', 'KEY',
     :     0.0, .FALSE., ' ', STATUS )

      END
