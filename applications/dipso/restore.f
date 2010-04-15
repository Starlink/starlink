      SUBROUTINE RESTORE( COMM, PARAMS, STATUS )
*+
*  Name:
*     RESTORE

*  Purpose:
*     Implements the DIPSO command RESTORE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RESTORE( COMM, PARAMS, STATUS )

*  Description:
*     The RESTORE command reads the contents of an NDF created by the
*     SAVE or EXIT commands, and puts the read data onto the stack.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The name of the command which invoked this routine. This will
*        usually be "RESTORE".
*     PARAMS = CHARACTER * ( * ) (Given)
*        Any text supplied by the user on the command line following the
*        command name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-AUG-1994 (DSB):
*        Original version.
*     13-DEC-1995 (DSB):
*        Remove LOC argument from GETNDF call. This argument returned a
*        primary locator to keep the file open, but the NDF library now
*        looks after it all.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Global Variables:
      INCLUDE 'DECLARE_STKS'     ! Common blocks holding the DIPSO stack
*        STKSZE = INTEGER (Read)
*           The size of the XSTACK and YSTACK arrays.
*        BSTSZE = INTEGER (Read)
*           The size of the BSTACK array.
*        MAXSTK = INTEGER (Read)
*           The maximum number of stack entries.
*        NONSTK = INTEGER (Read)
*           The current number of stack entries.
*        XSTACK( STKSZE ) = REAL (Read)
*           An array holding the X values for all the stack entries.
*        YSTACK( STKSZE ) = REAL (Read)
*           An array holding the Y values for all the stack entries.
*        BSTACK( BSTSZE ) = REAL (Read)
*           An array holding the break points for all the stack entries.
*        BSTNPT( MAXSTK ) = INTEGER (Read)
*           The number of breaks in each stack entry.
*        POINTR( MAXSTK ) = INTEGER (Read)
*           The XSTACK indices corresponding to the start of each stack
*           entry.
*        STKNPT( MAXSTK ) = INTEGER (Read)
*           The numbers of points in each stack entry.
*        BPOINT( MAXSTK ) = INTEGER (Read)
*           The BSTACK indices corresponding to the first break for each
*           stack entry.
*        STITLE( MAXSTK ) = CHARACTER* ( * ) (Read)
*           The title associated with each stack entry.
*        WORVST( MAXSTK ) = REAL (Read)
*           The WORV value for each stack entry.
*        STKLST = INTEGER (Read)
*           The index of the last used element in XSTACK.
*        BSTLST = INTEGER (Read)
*           The index of the last used element in BSTACK.

*  Arguments Given:
      CHARACTER COMM*(*)
      CHARACTER PARAMS*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :        INDF,              ! NDF identifier
     :        NSTK0              ! Stack size on entry

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Give a warning if two or more parameters were supplied.
      CALL EXPAR( COMM, PARAMS, 1, STATUS )

*  Save the current number of stack entries. If an error occurs in this
*  routine, then this value will be restored at the end.
      NSTK0 = NONSTK

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get an identifier for the NDF. The string "_STK" is appended to the
*  NDF name unless it already ends with "_STK". The default NDF name is
*  "SAVE_STK".
      CALL GETNDF( PARAMS, 1, .TRUE., COMM, 'NDF holding saved '//
     :             'stack data', 'SAVE', 'READ', '_STK', INDF,
     :             STATUS )

*  Tell the user what is about to happen.
      CALL NDF_MSG( 'NDFNAM', INDF )
      CALL MSGOUT( COMM, 'Restoring stack data from NDF ''^NDFNAM''.',
     :             .FALSE., STATUS )

*  Read the data from the NDF into the common arrays.
      CALL RESSTK( COMM, INDF, STKSZE, BSTSZE, MAXSTK, NONSTK, XSTACK,
     :             YSTACK, BSTACK, BSTNPT, POINTR, STKNPT, BPOINT,
     :             STITLE, WORVST, STKLST, BSTLST, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error has occurred, re0report it with less information unless
*  the current message filtering level is verbose.  Also, ensure that
*  the stack size is left unchanged.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL REREP( COMM, 'An error occurred while restoring '//
     :               'stack data.', STATUS )
         NONSTK = NSTK0
      END IF

      END
