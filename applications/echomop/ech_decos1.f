      SUBROUTINE ECH_DECOS1
*+
*  Name:
*     ECHOMOP - ECH_DECOS1

*  Purpose:
*     Does user assisted cosmic ray pixel location.
*

*  Description.
*     This routine is a top-level task-control routine which performs
*     primary-level cosmic-ray pixel identification.

*  Invocation :
*     CALL ECH_DECOS1

*  Arguments:
*     None.

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
      INCLUDE 'ECH_INIT_RDCTN.INC'
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_MAPPING.INC'
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_CONTEXT.INC'

*  Status:
      INTEGER STATUS

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  Report routine entry if enabled.
      IF ( IAND ( report_mode, rpm_full + rpm_calls ) .GT. 0 )
     :   CALL ECH_REPORT( report_mode, ECH__MOD_ENTRY )

      CALL ECH_INITIALISE( STATUS )
      CALL ECH_SET_PARAMETER( 'TUNE_CRTRC', 'LOGICAL',
     :                        0., .TRUE., ' ', STATUS )
      CALL ECH_MODULE_INIT( 'ECH_DECOSMIC_1', STATUS )
      IF ( .NOT. ECH_FATAL_ERROR( STATUS ) ) THEN
         CALL ECH_DECOSMIC_1(
     :        %VAL( IADDR_TRACIM ),
     :        %VAL( IADDR_NX_PIXELS ),
     :        %VAL( IADDR_NY_PIXELS ),
     :        USR_TUNE_CRXBOX,
     :        USR_TUNE_CRYBOX,
     :        USR_TUNE_MINCR,
     :        USR_TUNE_CRTRC,
     :        USR_TUNE_CRMAX,
     :        USR_TUNE_CRINTER,
     :        %VAL( IADDR_XMEDTMP ),
     :        %VAL( IADDR_YMEDTMP ),
     :        %VAL( IADDR_TRACIMQ ),
     :        STATUS )
         CALL ECH_MODULE_TIDYUP( 'ECH_DECOSMIC_1', STATUS )
         CALL ECH_MODULE_INIT( 'ECH_IMAGE_COSMIC', STATUS )
         CALL ECH_IMAGE_COSMIC(
     :        %VAL( IADDR_NX_PIXELS ),
     :        %VAL( IADDR_NY_PIXELS ),
     :        %VAL( IADDR_TRACIM ),
     :        %VAL( IADDR_TRACIMQ ),
     :        %VAL( IADDR_OUTPUT_IMAGE ),
     :        STATUS )
         CALL ECH_MODULE_TIDYUP( 'ECH_IMAGE_COSMIC', STATUS )
      END IF
      CALL ECH_CLOSEDOWN( STATUS )

      END
