      SUBROUTINE IRM_HIST( PARAM, INDF, COMMND, SIZE, TEXT, STATUS )
*+
*  Name:
*     IRM_HIST

*  Purpose:
*     Add history information to an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_HIST( PARAM, INDF, COMMND, SIZE, TEXT, STATUS )

*  Description:
*     A value is obtained for the given ADAM parameter (which should be
*     of type LOGICAL). If a false value is obtained then the routine
*     deletes any existing history information from the supplied NDF.
*     Otherwise, the supplied history information is added to the
*     supplied NDF. If the NDF does not have a defined HISTORY
*     component, then an empty HISTORY component is created and the
*     supplied information is added to it. The HISTORY structure created
*     conforms with the description of HISTORY contained in SGP/38.2.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which is used to allow the user
*        to enable or disable HISTORY recording. This parameter must
*        be of type LOGICAL.
*     INDF = INTEGER (Given)
*        The identifier for the NDF to which history information is to
*        be added.
*     COMMND = CHARACTER * ( * ) (Given)
*        The name of the application currently running.
*     SIZE = INTEGER (Given)
*        The number of lines of text to be stored in the new history
*        record.
*     TEXT( SIZE ) = CHARACTER * ( * ) (Given)
*        The lines of history text to be stored.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JAN-1992 (DSB):
*        Original version.
*     10-FEB-1992 (DSB):
*        Changed to delete existing history if PARAM value is obtained
*        false.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER INDF
      CHARACTER COMMND*(*)
      INTEGER SIZE
      CHARACTER TEXT( SIZE )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CLOC*(DAT__SZLOC)! Locator to a cell of RECORDS.
      CHARACTER DATTIM*24        ! Date and time string.
      INTEGER   DAY              ! Day within month.
      LOGICAL   ENABLE           ! True if history recording is enabled.
      INTEGER   END              ! Position of last non-blank character.
      INTEGER   EXTSIZ           ! Amount by which to extend the RECORDS
                                 ! array when it becomes full.
      CHARACTER HLOC*(DAT__SZLOC)! Locator to the HISTORY structure.
      INTEGER   HOURS            ! The hours field of the time.
      INTEGER   ISDST            ! Daylight saving flag.
      CHARACTER LOC*(DAT__SZLOC) ! Locator to the NDF.
      INTEGER   MINS             ! The minutes field within the time.
      CHARACTER MON( 12 )*3      ! Month abbreviations.
      INTEGER   MONTH            ! The month within the year, starting
                                 ! at zero.
      INTEGER   PREV             ! The index of the previous RECORDS
                                 ! element written to the NDF.
      INTEGER   NRECS            ! The size of the RECORDS array.
      INTEGER   NTICKS           ! Current time in binary form.
      CHARACTER RLOC*(DAT__SZLOC)! Locator to the whole RECORDS array.
      INTEGER   SECS             ! The seconds field within the time.
      INTEGER   START            ! The first non-blank character.
      LOGICAL   THERE            ! True if a component has been found.
      INTEGER   TSTRCT           ! A pointer to a C time structure.
      INTEGER   WDAY             ! The day number within the week.
      INTEGER   YDAY             ! The day number within the year.
      INTEGER   YEAR             ! The year number within the century.

      DATA MON/ 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG',
     :          'SEP', 'OCT', 'NOV', 'DEC'/

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a locator to the NDF.
      CALL NDF_LOC( INDF, 'UPDATE', LOC, STATUS )

*  See if the NDF already has a HISTORY component.
      CALL DAT_THERE( LOC, 'HISTORY', THERE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get a value for the supplied ADAM parameter.
      CALL PAR_GET0L( PARAM, ENABLE, STATUS )

*  If HISTORY recording is enabled...
      IF( ENABLE .AND. STATUS .EQ. SAI__OK ) THEN

*  ...get the date and time in the format described in SGP/38 (HISTORY
*  section), e.g. 1992-JAN-13 10:20:21.000.
         CALL PSX_TIME( NTICKS, STATUS )
         CALL PSX_LOCALTIME( NTICKS, SECS, MINS, HOURS, DAY, MONTH,
     :                       YEAR, WDAY, YDAY, ISDST, TSTRCT, STATUS )
         WRITE( DATTIM, 10 ) YEAR + 1900, MON( MONTH + 1 ), DAY, HOURS,
     :                       MINS, SECS
 10      FORMAT( I4, '-', A3, '-', I2.2, ' ', I2.2, ':', I2.2, ':',
     :           I2.2, '.000')


*  If the NDF already has a HISTORY component, get a locator to it.
         IF( THERE ) THEN
            CALL DAT_FIND( LOC, 'HISTORY', HLOC, STATUS )

*  Get values for the EXTEND_SIZE and CURRENT_RECORD components.
            CALL CMP_GET0I( HLOC, 'EXTEND_SIZE', EXTSIZ, STATUS )
            CALL CMP_GET0I( HLOC, 'CURRENT_RECORD', PREV, STATUS )

*  Get a locator to the RECORDS component, and get its current size.
            CALL DAT_FIND( HLOC, 'RECORDS', RLOC, STATUS )
            CALL DAT_SIZE( RLOC, NRECS, STATUS )

*  If an error has occured, give a contextual error report.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'IRM_HIST_ERR1',
     :        'IRM_HIST: Unable to access existing HISTORY information',
     :                       STATUS )
               GO TO 998
            END IF

*  If the NDF does not contain a HISTORY component, create it and obtain
*  a locator to the new structure.
         ELSE
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

*  Create the CURRENT_RECORD component, and assign the value 1 to it.
*  NOTE, SGP/38 is not clear what is meant by a "current record". This
*  routine assumes it means the index of the most recently completed
*  record.
            PREV = 0
            CALL DAT_NEW0I( HLOC, 'CURRENT_RECORD', STATUS )
            CALL CMP_PUT0I( HLOC, 'CURRENT_RECORD', PREV, STATUS )

*  Create the RECORDS component, and get a locator to it. This component
*  is an array of HIST_REC structures, with initial size of 10.
            NRECS = 10
            CALL DAT_NEW( HLOC, 'RECORDS', 'HIST_REC', 1, NRECS,
     :                    STATUS )
            CALL DAT_FIND( HLOC, 'RECORDS', RLOC, STATUS )

*  If an error has occured, give a contextual error report.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'IRM_HIST_ERR2',
     :        'IRM_HIST: Unable to create NDF HISTORY component',
     :                       STATUS )
               GO TO 998
            END IF

         END IF

*  If the RECORDS component is full, extend it.
         IF( PREV .EQ. NRECS ) THEN
            NRECS = NRECS + EXTSIZ
            CALL DAT_ALTER( RLOC, 1, NRECS, STATUS )
         END IF

*  Get a locator to the next cell of RECORDS to be written to.
         CALL DAT_CELL( RLOC, 1, PREV + 1, CLOC, STATUS )

*  Create the VARIANT component within the RECORD cell, and assign the
*  value SIMPLE to it.
         CALL DAT_NEW0C( CLOC, 'VARIANT', 6, STATUS )
         CALL CMP_PUT0C( CLOC, 'VARIANT', 'SIMPLE', STATUS )

*  Create the DATE component within the RECORD cell, and assign the
*  date and time string to it.
         CALL DAT_NEW0C( CLOC, 'DATE', 24, STATUS )
         CALL CMP_PUT0C( CLOC, 'DATE', DATTIM, STATUS )

*  Create the COMMAND component within the RECORD cell, and assign the
*  value of argument COMMND to it.
         CALL CHR_FANDL( COMMND, START, END )
         CALL DAT_NEW0C( CLOC, 'COMMAND', END - START + 1, STATUS )
         CALL CMP_PUT0C( CLOC, 'COMMAND', COMMND( START : END ),
     :                   STATUS )

*  Create the TEXT component of the RECORD cell, and store the supplied
*  text.
         CALL DAT_NEW1C( CLOC, 'TEXT', LEN( TEXT( 1 ) ), SIZE, STATUS )
         CALL CMP_PUT1C( CLOC, 'TEXT', SIZE, TEXT, STATUS )

*  Update the value of CURRENT_RECORD.
         CALL CMP_PUT0I( HLOC, 'CURRENT_RECORD', PREV + 1, STATUS )

*  If an error occured, give a contextual message.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'IRM_HIST_ERR3',
     :       'IRM_HIST: Unable to add history information to ^NDF',
     :                    STATUS )
         END IF

*  Annul the locators to the HISTORY structure and its components.
         CALL DAT_ANNUL( CLOC, STATUS )

 998     CONTINUE

         CALL DAT_ANNUL( RLOC, STATUS )
         CALL DAT_ANNUL( HLOC, STATUS )

*  If the output is to include no history, delete any existing HISTORY
*  component.
      ELSE
         IF( THERE ) CALL DAT_ERASE( LOC, 'HISTORY', STATUS )

      END IF

*  Annul the locator to the NDF.
 999  CALL DAT_ANNUL( LOC, STATUS )

      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRM_HIST_ERR',
     : 'IRM_HIST: Unable to add history information to an NDF',
     :                 STATUS )
      END IF

      END
