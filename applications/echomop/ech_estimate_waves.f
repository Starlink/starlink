      SUBROUTINE ECH_ESTIMATE_WAVES(
     :           LOW_WAVE_LIMIT,
     :           HI_WAVE_LIMIT,
     :           N_ORDERS,
     :           CENTRAL_WAVELENGTH,
     :           CENTRAL_ORDERNUM,
     :           START_SEARCH_WAVE,
     :           END_SEARCH_WAVE,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_ESTIMATE_WAVES

*  Purpose:
*     Estimate wavelength coverage of an order using central order number.

*  Description:
*     This routine makes a broad estimate of the wavelength coverage present
*     in a calibration frame. To do this it uses the central wavelength
*     and central order number. The estimate is of the coverage of the entire
*     set of orders as we do not yet know if the order increase or decrease
*     in wavelength as we move up (increasing y) the frame. The estimated
*     range will be used to limit the wavelength range searched when no
*     more precise information is available. If the limits provided by the
*     user parameters are more limiting, then they will be used in preference.

*  Invocation:
*     CALL ECH_ESTIMATE_WAVES(
*     :    LOW_WAVE_LIMIT,
*     :    HI_WAVE_LIMIT,
*     :    N_ORDERS,
*     :    CENTRAL_WAVELENGTH,
*     :    CENTRAL_ORDERNUM,
*     :    START_SEARCH_WAVE,
*     :    END_SEARCH_WAVE,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     LOW_WAVE_LIMIT = REAL (Given)
*        Lower wavelength limit for searches.
*     HI_WAVE_LIMIT = REAL (Given)
*        Upper wavelength limit for searches.
*     CENTRAL_WAVELENGTH = REAL (Given)
*        Approximate central wavelength in frame.
*     CENTRAL_ORDERNUM = INTEGER (Given)
*        Central order number in frame.
*     START_SEARCH_WAVE = REAL (Given and Returned)
*        Start wavelength for searches per order.
*     END_SEARCH_WAVE = REAL (Given and Returned)
*        End wavelength for searches per order.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     26-MAY-1997 (MJC):
*       Tidy up.
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
      REAL LOW_WAVE_LIMIT
      REAL HI_WAVE_LIMIT
      REAL CENTRAL_WAVELENGTH
      INTEGER CENTRAL_ORDERNUM

*  Arguments Returned:
      REAL START_SEARCH_WAVE( N_ORDERS ) ! Start wavelength for searches.
      REAL END_SEARCH_WAVE( N_ORDERS )   ! End wavelength for searches.

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL WAVE_LOWEST
      REAL WAVE_HIGHEST

      INTEGER I
      INTEGER MAX_ORDERNUM
      INTEGER MIN_ORDERNUM
      INTEGER NCHAR1
      INTEGER NCHAR2

      CHARACTER*16 REF_STR1
      CHARACTER*16 REF_STR2

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Set user-supplied limits in case we cannot do any better.
      WAVE_LOWEST = LOW_WAVE_LIMIT
      WAVE_HIGHEST = HI_WAVE_LIMIT

*  If we have a valid central wavelength AND a valid central order number.
      IF ( CENTRAL_WAVELENGTH .GT. 0.0 .AND.
     :     CENTRAL_ORDERNUM .GT. 0 ) THEN

*     Calculate broad search limits allowing for error of +-2 in central
*     order number.
         MIN_ORDERNUM = MAX( 1, CENTRAL_ORDERNUM - N_ORDERS / 2 - 3 )
         MAX_ORDERNUM = CENTRAL_ORDERNUM + N_ORDERS / 2 + 3
         WAVE_LOWEST = CENTRAL_WAVELENGTH * FLOAT( MIN_ORDERNUM ) /
     :         FLOAT( CENTRAL_ORDERNUM )
         WAVE_HIGHEST = CENTRAL_WAVELENGTH * FLOAT( MAX_ORDERNUM ) /
     :         FLOAT( CENTRAL_ORDERNUM )
         CALL CHR_RTOC( WAVE_LOWEST, REF_STR1, NCHAR1 )
         CALL CHR_RTOC( WAVE_HIGHEST, REF_STR2, NCHAR2 )
         REPORT_STRING = ' Ref. frame wavelength-range estimate: ' //
     :         REF_STR1( :NCHAR1 ) // ' to ' // REF_STR2( :NCHAR2 ) //
     :         '.'
         CALL ECH_REPORT( 0, REPORT_STRING )
      END IF

*  Default to user-supplied limits if they are more constraining.
      WAVE_LOWEST = MAX( WAVE_LOWEST, LOW_WAVE_LIMIT )
      IF ( HI_WAVE_LIMIT .NE. 0.0 ) THEN
         WAVE_HIGHEST = MIN( WAVE_HIGHEST, HI_WAVE_LIMIT )

      ELSE
         WAVE_HIGHEST = WAVE_HIGHEST
      END IF
      CALL CHR_RTOC( WAVE_LOWEST, REF_STR1, NCHAR1 )
      CALL CHR_RTOC( WAVE_HIGHEST, REF_STR2, NCHAR2 )
      REPORT_STRING = ' Wavelength search window limits are: ' //
     :      REF_STR1( :NCHAR1 ) // ' to ' // REF_STR2( :NCHAR2 ) // '.'
      CALL ECH_REPORT( 0, REPORT_STRING )

*  Set wavelength search range for each a order.
      DO I = 1, N_ORDERS
         START_SEARCH_WAVE( I ) = WAVE_LOWEST
         END_SEARCH_WAVE( I ) = WAVE_HIGHEST
      END DO

      END
