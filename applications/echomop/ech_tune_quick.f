      SUBROUTINE ECH_TUNE_QUICK( STATUS )
*+
*  Name:
*     ECHOMOP - ECH_TUNE_QUICK

*  Purpose:
*     Adjust parameters for quick-look mode operation.

*  Description:
*     This routine automatically adjusts tuning parameters for a quick and
*     dirty extraction.

*  Invocation:
*     CALL ECH_TUNE_QUICK( STATUS )

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
      INTEGER STATUS

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      CALL ECH_REPORT( 0, ' Setting parameters for QUICK extraction.' )
      CALL ECH_SET_PARAMETER( 'TUNE_MXSMP', 'INT',
     :     100.0, .FALSE., ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'TRACE_MODE', 'CHAR',
     :     0.0, .FALSE., 'TB', STATUS )
      CALL ECH_SET_PARAMETER( 'DISPLAY', 'KEY',
     :     0.0, .FALSE., ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'TRC_INTERACT', 'KEY',
     :     0.0, .FALSE., ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'USE_MEDIAN', 'KEY',
     :     0.0, .FALSE., ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'TUNE_CLPMXDEV', 'FLOAT',
     :     1.0, .FALSE., ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'TUNE_CLPBY', 'INT',
     :     5.0, .FALSE., ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'TUNE_XBOX', 'INT',
     :     5.0, .FALSE., ' ', STATUS )

      END
