      SUBROUTINE ECH_FLATTEN_YBLAZE(
     :           NX,
     :           N_ORDERS,
     :           MEDIAN_BLAZE,
     :           SPECTRUM,
     :           VARIANCE,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_FLATTEN_YBLAZE

*  Purpose:
*     Apply Y-blaze order slopes.

*  Description:
*     This routine applies the Y-blaze per-order slope coefficients.

*  Invocation:
*     CALL ECH_FLATTEN_YBLAZE(
*     :    NX,
*     :    N_ORDERS,
*     :    MEDIAN_BLAZE,
*     :    SPECTRUM,
*     :    VARIANCE,
*     :    STATUS
*     :   )

*  Arguments:
*     N_ORDERS = INTEGER (Given)
*        Number of orders.
*     MEDIAN_BLAZE = REAL (Given and Returned)
*        Medians of X blazes.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     SPECTRUM = REAL (Given and Returned)
*        Input order spectrum.
*     VARIANCE = REAL (Given and Returned)
*
*     STATUS = INTEGER(Given, WRITE )
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     17-JUN-1996 (MJC):
*       New prologue, new output formats.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}


*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'

*  Arguments Given:
      INTEGER N_ORDERS
      INTEGER NX

*  Arguments Returned:
      REAL MEDIAN_BLAZE( N_ORDERS )
      REAL SPECTRUM( NX, N_ORDERS )
      REAL VARIANCE( NX, N_ORDERS )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I
      INTEGER II
      INTEGER NCHAR1
      INTEGER NCHAR2

      CHARACTER*32 REF_STR1
      CHARACTER*32 REF_STR2

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) )  RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      IF ( N_ORDERS .GT. 1 ) THEN
         DO I = 1, N_ORDERS
            IF ( MEDIAN_BLAZE( I ) .NE. 0.0 ) THEN
               DO II = 1, NX / 2 - 1
                  SPECTRUM( II, I ) = SPECTRUM( II, I ) *
     :                  ( 1.0 + FLOAT( NX / 2 - II ) / FLOAT( NX / 2 ) *
     :                  ( 1.0 / MEDIAN_BLAZE( I ) - 1.0 ) )
                  VARIANCE( II, I ) = VARIANCE( II, I ) *
     :                  ( 1.0 + FLOAT( NX / 2 - II ) / FLOAT( NX / 2 ) *
     :                  ( 1.0 / MEDIAN_BLAZE( I ) - 1.0 ) )
               END DO
               DO II = NX / 2 + 1, NX
                  SPECTRUM( II, I ) = SPECTRUM( II, I ) *
     :                  ( 1.0 + FLOAT( II - NX / 2 ) / FLOAT( NX / 2 ) *
     :                  ( MEDIAN_BLAZE( I ) - 1.0 ) )
                  VARIANCE( II, I ) = VARIANCE( II, I ) *
     :                  ( 1.0 + FLOAT( II - NX / 2 ) / FLOAT( NX / 2 ) *
     :                  ( MEDIAN_BLAZE( I ) - 1.0 ) )
               END DO
            CALL CHR_ITOC( I, REF_STR1, NCHAR1 )
            CALL CHR_RTOC( MEDIAN_BLAZE( I ), REF_STR2, NCHAR2 )
            REPORT_STRING = ' Order ' // REF_STR1( :NCHAR1 ) //
     :                      ', slope of ' // REF_STR2( :NCHAR2 ) //
     :                      ' flattened.'
            CALL ECH_REPORT( 0, REPORT_STRING )
            END IF
         END DO
         CALL ECH_REPORT( 0,
     :        ' Y-blaze normalisation performed' //
     :        ' on extracted object spectrum.')

      ELSE
         CALL ECH_REPORT( 0, ' Not enough orders to fit Y blaze.' )
      END IF

      END
