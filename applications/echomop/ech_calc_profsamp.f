      SUBROUTINE ECH_CALC_PROFSAMP( NY, N_ORDERS, PFL_SUBSAMPLES,
     :           DEK_BELOW, DEK_ABOVE, ORDER_SIZE,
     :           SUBSTEPS, STATUS )
*+
*  Name:
*     ECHOMOP - ECH_CALC_PROFSAMP

*  Description:
*     Calculates profile subsampling factor.
*
*  Invocation:
*     CALL ECH_CALC_PROFSAMP( NY, N_ORDERS, PFL_SUBSAMPLES,
*    :     DEK_BELOW, DEK_ABOVE, ORDER_SIZE,
*    :     SUBSTEPS, STATUS )

*  Arguments:
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     DEK_BELOW = INTEGER (Given and Returned)
*        Lower dekker limit in pixels from trace.
*     DEK_ABOVE = INTEGER (Given and Returned)
*        Upper dekker limit in pixels from trace.
*     PFL_SUBSAMPLES = INTEGER (Given and Returned)
*        Number of subsamples to use for profiles.
*     ORDER_SIZE = INTEGER (Returned)
*        Approximate order spatial extent in pixels.
*     SUBSTEPS = INTEGER (Returned)
*        Subsamples per pixel to use for profiles.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     If more than one order present then
*        Estimate the spatial extent in pixels (= ny*2/3)
*        If dekker limits known the re-estimate using them
*     Else
*        If dekker limits known then estimate using them
*        Else
*           Estimate extent as ny/3
*        Endif
*     Endif
*     Calculate subsampling rate for profiling

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

*  Arguments Given:
      INTEGER NY
      INTEGER N_ORDERS
      INTEGER DEK_BELOW( N_ORDERS )
      INTEGER DEK_ABOVE( N_ORDERS )
      INTEGER PFL_SUBSAMPLES

*  Arguments Returned:
      INTEGER ORDER_SIZE
      INTEGER SUBSTEPS

*  Status:
      INTEGER STATUS

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  If more than one order present then
*  Estimate the spatial extent in pixels ( = ny * 2 / 3 )
*  If dekker limits known the re-estimate using them.
      IF ( N_ORDERS .EQ. 1 ) THEN
         IF ( DEK_BELOW( 1 ) .NE. 0 .OR. DEK_ABOVE( 1 ) .NE. 0 ) THEN
            ORDER_SIZE = DEK_ABOVE( 1 ) - DEK_BELOW( 1 ) + 1

         ELSE
            ORDER_SIZE = NY * 2 / 3
         END IF

*  If dekker limits known then estimate using them.
      ELSE
         IF ( DEK_BELOW( 1 ) .NE. 0 .OR. DEK_ABOVE( 1 ) .NE. 0 ) THEN
            ORDER_SIZE = DEK_ABOVE( 1 ) - DEK_BELOW( 1 ) + 1

*     Estimate extent as ny/3.

         ELSE
            ORDER_SIZE = NY / N_ORDERS
         END IF
      END IF

*  Calculate subsampling rate for profiling.
      SUBSTEPS = MAX( 2, ( PFL_SUBSAMPLES / ORDER_SIZE / 2 ) / 2 * 2 )
      SUBSTEPS = MIN( SUBSTEPS, 10 )

      END
