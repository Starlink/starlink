      SUBROUTINE ECH_DMP_MEM_USE( STATUS )
*+
*  Name:
*     ECHOMOP - ECH_DMP_MEM_USE

*  Purpose:
*     Dump details of memory mapped arrays.

*  Description:
*     This routine dumps the status of all mapped/allocated memory objects.

*  Invocation:
*     CALL ECH_DMP_MEM_USE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*  Bugs:
*     None known.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_USE_RDCTN.INC'

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I
      INTEGER LUN
      INTEGER ISTAT

      CHARACTER*255 OPENED_NAME
*.

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      ISTAT = 0
      CALL ECH_OPEN_FILE( 'ech_mem_usage.dmp', 'TEXT', 'NEW', .TRUE.,
     :     LUN, OPENED_NAME, ISTAT )
      IF ( ISTAT .EQ. 0 ) THEN
         DO I = 1, ACCESS_COUNT
            WRITE ( LUN, 1000 ) OBJECT_NAME( I )
            WRITE ( LUN, 1001 )
     :           OBJECT_SIZE( I ), OBJECT_ADDRESS( I ), OBJECT_TYPE( I )
         END DO
         WRITE ( LUN, '( // 1X, A // )' ) 'Workspace objects:'
         DO I = 1, WS_ACCESS_COUNT
            WRITE ( LUN, 1000 ) WS_OBJECT_NAME( I )
            WRITE ( LUN, 1001 ) WS_OBJECT_SIZE( I ),
     :            WS_OBJECT_ADDRESS( I ), WS_OBJECT_TYPE( I )
         END DO
      END IF
      CALL ECH_OPEN_FILE( 'ech_mem_usage.dmp', 'CLOSE', ' ', .TRUE.,
     :     LUN, OPENED_NAME, ISTAT )
      ISTAT =  STATUS

 1000 FORMAT ( 1X, 'Object ', A72 )
 1001 FORMAT ( 1X, I8, ' bytes used @', I10, ' for type ', A12 )

      END
