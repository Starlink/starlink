*+  HSTDSP - Displays an histogram

      SUBROUTINE HSTDSP( HIST, HRMIN, HRMAX, NUMBIN, STATUS )
*
*    Description :
*
*     This routine reports an histogram to the user.
*
*    Invocation :
*
*     CALL HSTDSP( HIST, HRMIN, HRMAX, NUMBIN, STATUS )
*
*    Arguments :
*
*     HIST( NUMBIN ) = INTEGER( READ )
*           The array holding the histogram.
*     HRMIN = REAL( READ )
*           The minimum value of the histogram.
*     HRMAX = REAL( READ )
*           The maximum value of the histogram.
*     NUMBIN = INTEGER( READ )
*           The number of bins in the histogram.
*     STATUS = INTEGER( READ, WRITE )
*           The status value on entry to this subroutine.
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     If there are insufficient bins then
*        Report error and set bad status
*     Else
*        Calculate binsize
*        Report title
*        For each histogram bin
*           Report range and number in bin
*        End do
*     Endif
*     Return
*
*    Authors :
*
*     Malcolm Currie RAL ( UK.AC.RL.STAR::CUR )
*
*    History :
*
*     1988 Jul 7  : Original (RL.STAR::CUR).
*
*    Type Definitions :

      IMPLICIT NONE

*    Global constants:

      INCLUDE 'SAE_PAR'

*    Import :

      INTEGER
     :    NUMBIN,
     :    HIST( NUMBIN )

      REAL
     :    HRMIN,
     :    HRMAX

*    Status :

      INTEGER STATUS

*    Local variables :

      REAL
     :    NBINWD,               ! Width of the report bins
     :    RANGE                 ! The difference between the maximum
                                ! and the minimum values

      INTEGER
     :    K                     ! loop counter

      CHARACTER*50 TEXT         ! summary of the histogram

*-

*    If the status is bad, then return

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check that there can be a compression

      IF ( NUMBIN .LT. 1 ) THEN

*       Report error and set a bad status

         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NUMBIN', NUMBIN )
         CALL ERR_REP( 'ERR_HSTDSP_ISFBIN',
     :     'HSTDSP: Insufficient bins (^NUMBIN) in the histogram.',
     :     STATUS )

      ELSE

*       Calculate the size of the bins for the report.

         RANGE = HRMAX - HRMIN
         NBINWD = RANGE / REAL( NUMBIN )

*       Report title

         CALL MSG_OUT( 'LINE', ' ', STATUS )
         CALL MSG_OUT( 'TITLE', '                Histogram',
     :                 STATUS )
         CALL MSG_OUT( 'LINE', ' ', STATUS )

*       Report histogram

         DO  K = 1, NUMBIN, 1

            WRITE ( TEXT, 10 ) HRMIN + ( K-1 )*NBINWD,
     :              HRMIN + K*NBINWD, HIST( K )
  10        FORMAT( ' ', 1PG15.7, ' to', 1PG15.7, ' ', I8, ' pixels' )
            CALL MSG_OUT( 'OUTLINE', TEXT, STATUS )

         END DO

         CALL MSG_OUT( 'LINE', ' ', STATUS )
      END IF

      END
