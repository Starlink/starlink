      SUBROUTINE ECH_SCRN2D
*+
*  Name:
*     ECHOMOP - ECH_SCRN2D

*  Purpose:
*     Top-level 2-D scrunch.

*  Description:
*     This routine is a top-level task-control routine which performs the
*     scrunching of a set of 2D extracted orders, into a rebinned image.

*  Invocation:
*     CALL ECH_SCRN2D

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
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_MAPPING.INC'

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER IORD
      INTEGER IFRM
      INTEGER START_ORD
      INTEGER END_ORD
      LOGICAL GOT_ERRORS

*  Functions called:
      INTEGER ECH_DEREF_INT
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      CALL ECH_INITIALISE( STATUS )
      CALL ECH_SET_PARAMETER( 'IDX_NUM_ORDERS', 'INT', 0.0, 0, ' ',
     :     STATUS )
      IFRM = 1
      CALL ECH_SET_PARAMETER( 'IDX_NREF_FRAME', 'INT', FLOAT( IFRM ), 0,
     :     ' ', STATUS )
      CALL ECH_SET_PARAMETER( 'NREF_FRAME', 'INT', FLOAT( IFRM ), 0,
     :     ' ', STATUS )
      CALL ECH_MODULE_INIT( 'ECH_DUMMY', STATUS )

      START_ORD = 1
      END_ORD = ECH_DEREF_INT( %VAL( IADDR_NO_OF_ORDERS ) )
      DO IORD = START_ORD, END_ORD
         CALL ECH_SET_PARAMETER( 'IDX_NUM_ORDERS', 'INT',
     :        FLOAT( IORD ), 0, ' ', STATUS )
         CALL ECH_MODULE_INIT( 'ECH_SCRUNCH_2D_ORDER', STATUS )
         IF ( .NOT. ECH_FATAL_ERROR( STATUS ) ) THEN
            GOT_ERRORS = .FALSE.
            IF ( IADDR_INPTIME .NE. 0 ) GOT_ERRORS = .TRUE.
            IF ( USR_TUNE_CLONE .EQ. 'NULL' ) THEN
               CALL ECH_SCRUNCH_2D_ORDER(
     :              %VAL( IADDR_NX_PIXELS ),
     :              %VAL( IADDR_NY_PIXELS ),
     :              %VAL( IADDR_INPTIM ),
     :              %VAL( IADDR_INPTIME ),
     :              GOT_ERRORS,
     :              %VAL( IADDR_NO_OF_BINS ),
     :              USR_TUNE_MXSKYPIX,
     :              %VAL( IADDR_READOUT_NOISE),
     :              %VAL( IADDR_NX_REBIN ),
     :              USR_W2_NX_POLY,
     :              USR_W2_NY_POLY,
     :              %VAL( IADDR_W_POLY_2D ),
     :              USR_TUNE_MAXPOLY,
     :              %VAL( IADDR_W_POLY ),
     :              %VAL( IADDR_SCRNCHD_WAVES),
     :              %VAL( IADDR_DEK_BELOW ),
     :              %VAL( IADDR_DEK_ABOVE ),
     :              %VAL( IADDR_TRC_POLY ),
     :              %VAL( IADDR_FITTED_FLAT ),
     :              %VAL( IADDR_FLAT_ERRORS ),
     :              %VAL( IADDR_REBIN_OBJECT ),
     :              %VAL( IADDR_REBIN_OBJECTE ),
     :              %VAL( IADDR_X_TRACE_COORD ),
     :              %VAL( IADDR_Y_TRACE_COORD ),
     :              %VAL( IADDR_FIT_WORK_XDOUBLE ),
     :              %VAL( IADDR_FIT_WORK_XDOUBLE2 ),
     :              %VAL( IADDR_TWO_D_DLAMBDA ),
     :              STATUS
     :             )
            END IF
            CALL ECH_MODULE_TIDYUP( 'ECH_SCRUNCH_2D_ORDER', STATUS )
         END IF
      END DO
      CALL ECH_CLOSEDOWN( STATUS )

      END
