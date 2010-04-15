      SUBROUTINE HISTB1( MJDSIN, MJDBEF, CSUBST, REC, RLOC, OK, STATUS )
*+
*  Name:
*     HISTB1

*  Purpose:
*     Check if a record passes the selection criteria.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HISTB1( MJDSIN, MJDBEF, CSUBST, REC, RLOC, OK, STATUS )

*  Description:
*     The command which created the history record is checked to see if
*     it contains a sub-string equal to CSUBST. If it doesn't, OK is
*     returned .FALSE. If it does (or if CSUBST is blank), the creation
*     date of the requested record of HISTORY information is compared
*     against the two supplied modified Julian dates. If it was created
*     before MJDBEF but after MJDSIN, then OK is returned true,
*     otherwise it is returned false.

*  Arguments:
*     MJDSIN = DOUBLE PRECISION ( Given )
*        The starting time of the acceptable window.
*     MJDBEF = DOUBLE PRECISION ( Given )
*        The ending time of the acceptable window.
*     CSUBST = CHARACTER * ( * )
*        The command sub-string to be searched for. Ignored if blank.
*     REC = INTEGER ( Given )
*        The index of the record to be checked.
*     RLOC = CHARACTER (Given)
*        A locator to the HIST_REC array.
*     OK = LOGICAL ( Returned )
*        True if the history record is acceptable.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JUL-1992 (DSB):
*        Original version.
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
      DOUBLE PRECISION MJDSIN
      DOUBLE PRECISION MJDBEF
      CHARACTER CSUBST*(*)
      INTEGER REC
      CHARACTER RLOC*(*)

*  Arguments Returned:
      LOGICAL OK

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CLOC*(DAT__SZLOC)! Locator to a cell of HIST_REC
      CHARACTER COMMND*80        ! Value of the COMMAND component.
      CHARACTER DATTIM*24        ! Date and time string.
      INTEGER ID                 ! Day of the month.
      INTEGER IHOUR              ! Hour of the day.
      INTEGER IM                 ! Month of the year.
      INTEGER IMIN               ! Minutes into the hour.
      INTEGER IY                 ! Year.
      DOUBLE PRECISION MJD       ! Modified Julian Date at the time the
                                 ! history record was created.
      REAL SEC                   ! Seconds into the minute.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a locator to the required cell of RECORDS.
      CALL DAT_CELL( RLOC, 1, REC, CLOC, STATUS )

*  Get the name of the application which created the record from the
*  COMMAND component.
      CALL CMP_GET0C( CLOC, 'COMMAND', COMMND, STATUS )

*  If the history record was not created by the right command, return
*  with OK set false.
      IF( CSUBST .NE. ' ' .AND. INDEX( COMMND, CSUBST ) .EQ. 0 ) THEN
         OK = .FALSE.
         GO TO 999
      END IF

*  Get the string holding the date and time at which the record was
*  created from component DATE.
      CALL CMP_GET0C( CLOC, 'DATE', DATTIM, STATUS )

*  Convert the string to a Modified Julian Date.
      CALL IRM_TD( DATTIM, IY, IM, ID, IHOUR, IMIN, SEC, MJD, STATUS )

*  Check that the creation date is within the specified time window.
      IF( MJD .LT. MJDSIN .OR. MJD .GT. MJDBEF ) THEN
         OK = .FALSE.
      ELSE
         OK = .TRUE.
      ENDIF

*  Annul the locator.
 999  CONTINUE
      CALL DAT_ANNUL( CLOC, STATUS )

      END
