      SUBROUTINE ECH_DECOS2
*+
*  Name:
*     ECHOMOP - ECH_DECOS2

*  Purpose:
*     Does automatic cosmic-ray pixel location.

*  Description:
*     This routine is a top-level task control routine which performs 2nd-
*     level cosmic-ray pixel identification.  It is used after order tracing
*     has been performed.

*  Invocation:
*     CALL ECH_DECOS2

*  Arguments:
*     None:

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

*  Local Variables:
      INTEGER STATUS

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

         CALL ECH_INITIALISE( STATUS )
         CALL ECH_SET_PARAMETER( 'IDX_NUM_ORDERS', 'INT',
     :        0.0, 0, ' ', STATUS )
         CALL ECH_MODULE_INIT( 'ECH_DECOSMIC_2', STATUS )
         IF ( .NOT. ECH_FATAL_ERROR( STATUS )  ) THEN
            CALL ECH_DECOSMIC_2(
     :          %VAL( IADDR_INPTIM ),
     :          %VAL( IADDR_INPTIMQ ),
     :          %VAL( IADDR_NX_PIXELS ),
     :          %VAL( IADDR_NY_PIXELS ),
     :          %VAL( IADDR_NO_OF_ORDERS ),
     :          USR_TUNE_MAXPOLY,
     :          %VAL( IADDR_TRC_POLY ),
     :          USR_TUNE_MXSKYPIX,
     :          %VAL( IADDR_OBJ_MASK ),
     :          %VAL( IADDR_SKY_MASK ),
     :          %VAL( IADDR_DEK_BELOW ),
     :          %VAL( IADDR_DEK_ABOVE ),
     :          USR_TUNE_FINCPLY,
     :          %VAL( IADDR_PLY_ORDERINCS ),
     :          USR_TUNE_DSGMTHR,
     :          USR_TUNE_DPRBTHR,
     :          %VAL( IADDR_SKY_SPECTRUM ),
     :          %VAL( IADDR_X_TRACE_COORD ),
     :          %VAL( IADDR_Y_TRACE_COORD ),
     :          %VAL( IADDR_DECOS_DATA ),
     :          %VAL( IADDR_DECOS_SDATA ),
     :          %VAL( IADDR_DECOS_XAXIS ),
     :          %VAL( IADDR_DECOS_YAXIS ),
     :          %VAL( IADDR_DECOS_GAUSS ),
     :          %VAL( IADDR_DECOS_INDEX_X  ),
     :          %VAL( IADDR_DECOS_INDEX_Y ),
     :          %VAL( IADDR_FIT_WORK_XREAL ),
     :          %VAL( IADDR_FIT_WORK_XREAL2 ),
     :          %VAL( IADDR_FIT_WORK_XREAL3 ),
     :          %VAL( IADDR_FIT_WORK_XREAL4 ),
     :          %VAL( IADDR_FIT_WORK_XINT ),
     :          STATUS
     :         )
         CALL ECH_MODULE_TIDYUP( 'ECH_DECOSMIC_2', STATUS )
         CALL ECH_MODULE_INIT( 'ECH_IMAGE_COSMIC', STATUS )
         CALL ECH_IMAGE_COSMIC(
     :        %VAL( IADDR_NX_PIXELS ),
     :        %VAL( IADDR_NY_PIXELS ),
     :        %VAL( IADDR_INPTIM ),
     :        %VAL( IADDR_INPTIMQ ),
     :        %VAL( IADDR_OUTPUT_IMAGE ),
     :        STATUS )
         CALL ECH_MODULE_TIDYUP( 'ECH_IMAGE_COSMIC', STATUS )
      ENDIF

      CALL ECH_CLOSEDOWN( STATUS )

      END
