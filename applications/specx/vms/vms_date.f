      SUBROUTINE VMS_DATE( BUF )
*+
*  Name:
*     VMS_DATE

*  Purpose:
*     Emulate VMS DATE routine.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL VMS_DATE( BUF )

*  Description:
*     This routine returns the date of the current system time in the
*     format familiar from VAX/VMS system, i.e. "dd-mmm-yy".

*  Arguments:
*     BUF = CHARACTER * ( * ) (Given)
*        Should be at least 9 characters.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     timj: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     27 Jan 1994 (hme):
*        Original version.
*     30 Dec 1999 (timj):
*        Check status. Use SAE_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Returned:
      CHARACTER * ( * ) BUF

*  Local Variables:
      INTEGER NTICKS
      INTEGER STATUS            ! Starlink status
      INTEGER SECS, MINS, HOURS
      INTEGER DAY,  MONTH, YEAR
      INTEGER WDAY, YDAY, ISDST
      INTEGER TSTRCT
      CHARACTER * ( 9 ) LBUF
      CHARACTER * ( 3 ) CMONTH( 12 )

*  Local Data:
      DATA CMONTH / 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     :              'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC' /

*.

*     Initialise status
      STATUS = SAI__OK

*     Get time
      CALL PSX_TIME( NTICKS, STATUS )

      IF (STATUS .NE. SAI__OK) THEN
         PRINT *, '-- VMS_DATE --'
         PRINT *, '   bad status in PSX_TIME = ', STATUS

         RETURN
      END IF

      CALL PSX_LOCALTIME( NTICKS, SECS, MINS, HOURS,
     :   DAY, MONTH, YEAR, WDAY, YDAY, ISDST, TSTRCT, STATUS )

      WRITE( LBUF, 101 ) DAY, CMONTH(MONTH+1), YEAR
 101  FORMAT( I2.2, '-', A3, '-', I2.2 )

      BUF = LBUF

      END
