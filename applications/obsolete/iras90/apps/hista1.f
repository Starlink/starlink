      SUBROUTINE HISTA1( IGRP, EXTSIZ, HLOC, RLOC, NRECS, NEXT, STATUS )
*+
*  Name:
*     HISTA1

*  Purpose:
*     Add a history record.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HISTA1( IGRP, EXTSIZ, HLOC, RLOC, NRECS, NEXT, STATUS )

*  Description:
*     The required components within the element of the HIST_REC array
*     pointed to by the supplied value of NEXT are created. The TEXT
*     component is filled with lines of text taken from the GRP group
*     identified by IGRP.  The supplied value of NEXT is stored in the
*     CURRENT_RECORD component of the HISTORY structure, and the
*     returned value is incremented by one. If necessary the HIST_REC
*     array is extended, and the new size is returned in NRECS.

*  Arguments:
*     IGRP = INTEGER (Given)
*        An identifier for a GRP group containing the text to be used to
*        create the history record.
*     EXTSIZ = INTEGER (Given)
*        The no. of elements by which to extend the HIST_REC array
*        if it is full.
*     HLOC = CHARACTER (Given)
*        A locator to the HISTORY structure.
*     RLOC = CHARACTER (Given)
*        A locator to the HIST_REC array.
*     NRECS = INTEGER (Given and Returned)
*        The size of the HIST_REC array. If necessary this is extended
*        to make room for the new record, in which case the new size is
*        returned.
*     NEXT = INTEGER (Given and Returned)
*        The index of the next element of HIST_REC to be written to.
*        This is incremented by one before returning. The supplied value
*        is written to the CURRENT_RECORD component of the HISTORY
*        structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-JAN-1992 (DSB):
*        Original version.
*     10-FEB-1992 (DSB):
*        Interpretation of CURRENT_RECORD changed.
*     7-JUL-1992 (DSB):
*        Changed to use text stored in a GRP group.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Given:
      INTEGER IGRP
      INTEGER EXTSIZ
      CHARACTER HLOC*(*)
      CHARACTER RLOC*(*)

*  Arguments Given and Returned:
      INTEGER NRECS
      INTEGER NEXT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CLOC*(DAT__SZLOC)! Locator to a cell of HIST_REC
      CHARACTER DATTIM*24        ! Date and time string.
      INTEGER   DAY              ! Day within month.
      INTEGER   HOURS            ! The hours field of the time.
      INTEGER   ISDST            ! Daylight saving flag.
      INTEGER   MINS             ! The minutes field within the time.
      CHARACTER MON( 12 )*3      ! Month abreviations.
      INTEGER   MONTH            ! The month within the year, starting
                                 ! at zero.
      INTEGER   NTICKS           ! Current time in binary form.
      INTEGER   PNTR             ! Pointer to the mapped TEXT component.
      INTEGER   SECS             ! The seconds field within the time.
      INTEGER   SIZE             ! Current size of the TEXT array.
      CHARACTER TLOC*(DAT__SZLOC)! Locator to the TEXT array.
      INTEGER   TSTRCT           ! A pointer to a C time structure.
      INTEGER   WDAY             ! The day number within the week.
      INTEGER   YDAY             ! The day number within the year.
      INTEGER   YEAR             ! The year number within the century.

      DATA MON/ 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG',
     :          'SEP', 'OCT', 'NOV', 'DEC'/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the RECORDS component is full, extend it.
      IF( NEXT .GT. NRECS ) THEN
         NRECS = NRECS + EXTSIZ
         CALL DAT_ALTER( RLOC, 1, NRECS, STATUS )
      END IF

*  Get a locator to the "current" cell of RECORDS.
      CALL DAT_CELL( RLOC, 1, NEXT, CLOC, STATUS )

*  Create the VARIANT component within the RECORD cell, and assign the
*  value SIMPLE to it.
      CALL DAT_NEW0C( CLOC, 'VARIANT', 6, STATUS )
      CALL CMP_PUT0C( CLOC, 'VARIANT', 'SIMPLE', STATUS )

*  Get the date and time in the format described in SGP/38 (HISTORY
*  section), e.g. 1992-JAN-13 10:20:21.000.
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_LOCALTIME( NTICKS, SECS, MINS, HOURS, DAY, MONTH,
     :                    YEAR, WDAY, YDAY, ISDST, TSTRCT, STATUS )
      WRITE( DATTIM, 10 ) YEAR + 1900, MON( MONTH + 1 ), DAY, HOURS,
     :                    MINS, SECS
 10   FORMAT( I4, '-', A3, '-', I2.2, ' ', I2.2, ':', I2.2, ':',
     :        I2.2, '.000')

*  Create the DATE component within the RECORD cell, and assign the
*  date and time string to it.
      CALL DAT_NEW0C( CLOC, 'DATE', 24, STATUS )
      CALL CMP_PUT0C( CLOC, 'DATE', DATTIM, STATUS )

*  Create the COMMAND component within the RECORD cell, and assign the
*  value "IRAS90:HISTORY" to it.
      CALL DAT_NEW0C( CLOC, 'COMMAND', 14, STATUS )
      CALL CMP_PUT0C( CLOC, 'COMMAND', 'IRAS90:HISTORY', STATUS )

*  Get the size of the group containing the text.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )

*  Create the TEXT component of the RECORD cell with size equal to the
*  size of the group, and get a locator to it.
      CALL DAT_NEW1C( CLOC, 'TEXT', 80, SIZE, STATUS )
      CALL DAT_FIND( CLOC, 'TEXT', TLOC, STATUS )

*  Map the TEXT component.
      CALL DAT_MAPV( TLOC, '_CHAR*80', 'WRITE', PNTR, SIZE, STATUS )

*  Store the text in the mapped array.
      CALL GRP_GET( IGRP, 1, SIZE, %VAL( PNTR ), STATUS, %VAL( 80 ) )

*  Update the value of CURRENT_RECORD, and increment NEXT.
*  CURRENT_RECORD holds the index of the most recently completed HISTORY
*  record.
      CALL CMP_PUT0I( HLOC, 'CURRENT_RECORD', NEXT, STATUS )
      NEXT = NEXT + 1

*  Annul the locators (thus unmapping the mapped TEXT component).
 999  CONTINUE
      CALL DAT_ANNUL( TLOC, STATUS )
      CALL DAT_ANNUL( CLOC, STATUS )

      END
