      SUBROUTINE ECH_DISABLE_ORDER(
     :           BAD_ORDER,
     :           MAXIMUM_POLY,
     :           N_ORDERS,
     :           TRACE_POLYNOMIALS,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_DISABLE_ORDER

*  Purpose:
*     Disable order from further processing.

*  Description:
*     This routine disables a single order from further processing by
*     writing a BAD value into the first element of its trace polynomial
*     description.

*  Invocation:
*     CALL ECH_DISABLE_ORDER(
*     :    BAD_ORDER,
*     :    MAXIMUM_POLY,
*     :    N_ORDERS,
*     :    TRACE_POLYNOMIALS,
*     :    STATUS
*     :   )

*  Arguments:
*     BAD_ORDER = INTEGER (Given)
*        Order to be disabled.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum degree of polynomials.
*     TRACE_POLYNOMIALS = DOUBLE (Given and Returned)
*        Coefficients for the trace polynomials.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     28-MAY-1997 (MJC):
*       Tidy up.
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
      INTEGER N_ORDERS
      INTEGER BAD_ORDER
      INTEGER MAXIMUM_POLY

*  Arguments Returned:
      DOUBLE PRECISION TRACE_POLYNOMIALS( MAXIMUM_POLY, N_ORDERS )
*           ! Coefficients for trace polynomials.

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER NCHAR

      CHARACTER*8 REF_STR

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      CALL CHR_ITOC( BAD_ORDER, REF_STR, NCHAR )

*  If a good order number at all.
      IF ( BAD_ORDER .GT. 0 .AND. BAD_ORDER .LE. N_ORDERS ) THEN

*     If not disabled already.
         IF ( TRACE_POLYNOMIALS( 1, BAD_ORDER ) .NE.
     :        ECH__BAD_DOUBLE ) THEN
            TRACE_POLYNOMIALS( 1, BAD_ORDER ) = ECH__BAD_DOUBLE
            REPORT_STRING = ' Order ' // REF_STR( :NCHAR ) //
     :            ' is now disabled.'
            CALL ECH_REPORT( 0, REPORT_STRING )

         ELSE
            REPORT_STRING = ' Order ' // REF_STR( :NCHAR ) //
     :            ' is already disabled.'
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF

      ELSE
         REPORT_STRING= ' Cannot disable illegal order number ' //
     :         REF_STR( :NCHAR ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )
      END IF

      END
