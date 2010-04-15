      SUBROUTINE HISTA2( NDFNAM, LOGPOS, FD, REC, RLOC, STATUS )
*+
*  Name:
*     HISTA2

*  Purpose:
*     Display a history record.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HISTA2( NDFNAM, LOGPOS, FD, REC, RLOC, STATUS )

*  Description:
*     The requested record of HISTORY information is displayed on the
*     terminal screen (subject to the conditional message filter level),
*     and optionally logged to a text file.

*  Arguments:
*     NDFNAM = CHARACTER * ( * ) ( Given )
*        The name of the NDF.
*     LOGPOS = LOGICAL ( Given )
*        True if display is to be written to a log file.
*     FD = INTEGER ( Given )
*        The file descriptor by which to access the log file.
*     REC = INTEGER ( Given )
*        The index of the record to be displayed.
*     RLOC = CHARACTER (Given)
*        A locator to the HIST_REC array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-JAN-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants.

*  Arguments Given:
      CHARACTER NDFNAM*(*)
      LOGICAL LOGPOS
      INTEGER FD
      INTEGER REC
      CHARACTER RLOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER   CLEN             ! Length of each line of text.
      CHARACTER CLOC*(DAT__SZLOC)! Locator to a cell of HIST_REC
      CHARACTER COMMND*80        ! Value of HIST_REC COMMAND component.
      CHARACTER DATTIM*24        ! Date and time string.
      CHARACTER LINE*78          ! A line of underscores.
      INTEGER   PNTR             ! Pointer to the mapped TEXT array.
      INTEGER   SIZE             ! Current size of the TEXT array.
      CHARACTER TLOC*(DAT__SZLOC)! Locator to the TEXT array.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a locator to the required cell of RECORDS.
      CALL DAT_CELL( RLOC, 1, REC, CLOC, STATUS )

*  Get the date and time at which the record was created from component
*  DATE.
      CALL CMP_GET0C( CLOC, 'DATE', DATTIM, STATUS )

*  Get the name of the application which created the record from the
*  COMMAND component.
      CALL CMP_GET0C( CLOC, 'COMMAND', COMMND, STATUS )

*  Get a locator to the TEXT array, and map it.
      CALL DAT_FIND( CLOC, 'TEXT', TLOC, STATUS )
      CALL DAT_MAPV( TLOC, '_CHAR', 'READ', PNTR, SIZE, STATUS )

*  Determine the length of each string in the array.
      CALL DAT_CLEN( TLOC, CLEN, STATUS )

*  Set up the line of underscores used to seperate history records.
      CALL CHR_FILL( '_', LINE )

*  Display header information.
      CALL HISTC0( ' ', LOGPOS, FD, STATUS )
      CALL HISTC0( LINE, LOGPOS, FD, STATUS )

      CALL MSG_SETC( 'NDF', NDFNAM )
      CALL MSG_SETI( 'REC', REC )
      CALL HISTC0( ' History record number ^REC from ^NDF', LOGPOS, FD,
     :               STATUS )

      CALL MSG_SETC( 'DATTIM', DATTIM )
      CALL MSG_SETC( 'COM', COMMND )
      CALL HISTC0( ' Created at ^DATTIM by ^COM', LOGPOS, FD, STATUS )

      CALL HISTC0( ' ', LOGPOS, FD, STATUS )

*  Call a lower level routine to display the text. Pass the length of
*  each character string as an "invisible" argument at the end. This
*  extra argument is ignored on VMS because there is no corresponding
*  argument defined in HISTA3, but is used on UNIX to get access to the
*  mapped character array.
      CALL HISTA3( SIZE, %VAL( PNTR ), LOGPOS, FD, STATUS,
     :             %VAL( CLEN ) )

*  Annul the locators (thus unmapping the TEXT array).
 999  CALL DAT_ANNUL( TLOC, STATUS )
      CALL DAT_ANNUL( CLOC, STATUS )

      END
