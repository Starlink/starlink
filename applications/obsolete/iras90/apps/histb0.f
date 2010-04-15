      SUBROUTINE HISTB0( LOC, HLOC, RLOC, EXTSIZ, NEXT, NRECS, STATUS )
*+
*  Name:
*     HISTB0

*  Purpose:
*     Create an empty HISTORY structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HISTB0( LOC, HLOC, RLOC, EXTSIZ, NEXT, NRECS, STATUS )

*  Description:
*     This routine creates an empty HISTORY structure and returns
*     various items of information describing it, together with
*     locators to the structure and the RECORDS array.

*  Arguments:
*     LOC = CHARACTER (Given)
*        A locator to an HDS object in which to create the structure.
*     HLOC = CHARACTER (Returned)
*        A locator to the HISTORY structure.
*     RLOC = CHARACTER (Returned)
*        A locator to the RECORDS array.
*     EXTSIZ = INTEGER (Returned)
*        The value of the EXTEND_SIZE component. Set to 5.
*     NEXT = INTEGER (Returned)
*        The index of the next HISTORY record to be written. One more
*        than the value of the CURRENT_RECORD component (interpreted
*        as the most recently completed HISTORY record). Set to 1.
*     NRECS = INTEGER (Returned)
*        The current size of the RECORDS array. Set to 10.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-JAN-1992 (DSB):
*        Original version.
*     10-FEB-1992 (DSB):
*        Changed interpretation of CURRENT_RECORD.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER LOC*(*)

*  Arguments Returned:
      CHARACTER HLOC*(*)
      CHARACTER RLOC*(*)
      INTEGER EXTSIZ
      INTEGER NEXT
      INTEGER NRECS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DATTIM*24        ! Date and time string.
      INTEGER   DAY              ! Day within month.
      INTEGER   HOURS            ! The hours field of the time.
      INTEGER   ISDST            ! Daylight saving flag.
      INTEGER   MINS             ! The minutes field within the time.
      CHARACTER MON( 12 )*3      ! Month abreviations.
      INTEGER   MONTH            ! The month within the year, starting
                                 ! at zero.
      INTEGER   NTICKS           ! Current time in binary form.
      INTEGER   SECS             ! The seconds field within the time.
      INTEGER   TSTRCT           ! A pointer to a C time structure.
      INTEGER   WDAY             ! The day number within the week.
      INTEGER   YDAY             ! The day number within the year.
      INTEGER   YEAR             ! The year number within the century.

      DATA MON/ 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG',
     :          'SEP', 'OCT', 'NOV', 'DEC'/

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the date and time in the format described in SGP/38 (HISTORY
*  section), e.g. 1992-JAN-13 10:20:21.000.
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_LOCALTIME( NTICKS, SECS, MINS, HOURS, DAY, MONTH,
     :                    YEAR, WDAY, YDAY, ISDST, TSTRCT, STATUS )
      WRITE( DATTIM, 10 ) YEAR + 1900, MON( MONTH + 1 ), DAY, HOURS,
     :                    MINS, SECS
 10   FORMAT( I4, '-', A3, '-', I2.2, ' ', I2.2, ':', I2.2, ':',
     :        I2.2, '.000')

*  Create the HISTORY structure and obtain a locator to it.
      CALL DAT_NEW( LOC, 'HISTORY', 'HISTORY', 0, 0, STATUS )
      CALL DAT_FIND( LOC, 'HISTORY', HLOC, STATUS )

*  Create the VARIANT component, and assign the value "SIMPLE" to it.
      CALL DAT_NEW0C( HLOC, 'VARIANT', 6, STATUS )
      CALL CMP_PUT0C( HLOC, 'VARIANT', 'SIMPLE', STATUS )

*  Create the CREATED component, and assign the date and time string
*  to it.
      CALL DAT_NEW0C( HLOC, 'CREATED', 24, STATUS )
      CALL CMP_PUT0C( HLOC, 'CREATED', DATTIM, STATUS )

*  Create the EXTEND_SIZE component, and assign the value 5 to it.
      EXTSIZ = 5
      CALL DAT_NEW0I( HLOC, 'EXTEND_SIZE', STATUS )
      CALL CMP_PUT0I( HLOC, 'EXTEND_SIZE', EXTSIZ, STATUS )

*  Create the CURRENT_RECORD component, and assign the value 0 to it.
*  NOTE, SGP/38 is not clear what is meant by a "current record". This
*  routine assumes it means the index of the most recently completed
*  record.
      CALL DAT_NEW0I( HLOC, 'CURRENT_RECORD', STATUS )
      CALL CMP_PUT0I( HLOC, 'CURRENT_RECORD', 0, STATUS )
      NEXT = 1

*  Create the RECORDS component, and get a locator to it. This component
*  is an array of HIST_REC structures, with initial size of 10.
      NRECS = 10
      CALL DAT_NEW( HLOC, 'RECORDS', 'HIST_REC', 1, NRECS, STATUS )
      CALL DAT_FIND( HLOC, 'RECORDS', RLOC, STATUS )

      END
