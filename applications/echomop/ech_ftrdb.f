      SUBROUTINE ECH_FTRDB
*+
*  Name:
*     ECHOMOP - ECH_FTRDB

*  Purpose:
*     Build arc line feature database.

*  Description:
*     This routine is a top-level task-control routine.

*  Invocation:
*     CALL ECH_FTRDB

*  Arguments:
*     None used.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_MAPPING.INC'

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAX_FTRS
      PARAMETER ( MAX_FTRS = 5000 )

*  Local Variables:
      REAL FEATURES( MAX_FTRS )
      REAL STRENGTHS( MAX_FTRS )

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  Update context.
      CALL ECH_SET_CONTEXT( 'ROUTINE', 'ECH_FTRDB' )

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      CALL ECH_INITIALISE( STATUS )
      CALL ECH_MODULE_INIT( 'ECH_GENERATE_FDB', STATUS )
      CALL NINI( %VAL( IADDR_WINDEX_SIZE ) )
      IF ( .NOT. ECH_FATAL_ERROR( STATUS ) ) THEN
         CALL ECH_READ_FTR_SRC(
     :        USR_ARC_TYPE,
     :        MAX_FTRS,
     :        FEATURES,
     :        STRENGTHS,
     :        %VAL( IADDR_WINDEX_SIZE ),
     :        CSTR_EFTRDB_SOURCE,
     :        STATUS
     :       )
      END IF
      CALL ECH_MODULE_TIDYUP( 'ECH_GENERATE_FDB', STATUS )
      CALL ECH_MODULE_INIT( 'ECH_GENERATE_FDB', STATUS )
      IF ( .NOT. ECH_FATAL_ERROR( STATUS ) ) THEN
         CALL ECH_GENERATE_FDB(
     :        USR_ARC_TYPE,
     :        %VAL( IADDR_WINDEX_SIZE ),
     :        USR_TUNE_DB_SCOPE,
     :        %VAL( IADDR_QINDEX_SIZE ),
     :        MAX_FTRS,
     :        FEATURES,
     :        STRENGTHS,
     :        %VAL( IADDR_FDB_WAVELENGTH ),
     :        %VAL( IADDR_FDB_INTENSITY ),
     :        %VAL( IADDR_FDB_DATABASE ),
     :        %VAL( IADDR_FDB_LEFT ),
     :        %VAL( IADDR_FDB_RIGHT ),
     :        %VAL( IADDR_FDB_WAVE_INDEX ),
     :        %VAL( IADDR_FDB_QUICK_INDEX ),
     :        %VAL( IADDR_FDB_QUICK_VALUE ),
     :        %VAL( IADDR_WFDB_LEFT ),
     :        %VAL( IADDR_WFDB_RIGHT ),
     :        %VAL( IADDR_WFDB_DATABASE ),
     :        STATUS
     :       )
      END IF
      CALL ECH_MODULE_TIDYUP( 'ECH_GENERATE_FDB', STATUS )

      CALL ECH_CLOSEDOWN( STATUS )

      END

      SUBROUTINE NINI( VAL )
      INTEGER VAL
      WRITE ( 6, * ) ' Size:', VAL
      END
