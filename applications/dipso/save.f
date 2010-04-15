      SUBROUTINE SAVE( COMM, PARAMS, STATUS )
*+
*  Name:
*     SAVE

*  Purpose:
*     Implements the DIPSO command SAVE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SAVE( COMM, PARAMS, STATUS )

*  Description:
*     The SAVE command writes the contents of the stack (or part of the
*     stack) to an NDF. See routine SAVSTK for information about the
*     structure of the stack and how they are saved.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The name of the command which invoked this routine. This will
*        usually be "SAVE", but could conceivably be something else ("EXIT"
*        for instance).
*     PARAMS = CHARACTER * ( * ) (Given)
*        Any text supplied by the user on the command line following the
*        command name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-AUG-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
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
      CHARACTER
     :        NDFNM*255,         ! Full name of NDF structure.
     :        ROOT*255           ! Root name of NDF structure.

      INTEGER
     :        BOT,               ! Lowest stack entry to be saved
     :        TOP                ! Highest stack entry to be saved
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the stack is empty, issue a warning and return.
      IF( NONSTK .EQ. 0 ) THEN
         CALL MSGOUT( COMM, 'Stack is empty - not saved.', .TRUE.,
     :                STATUS )
         GO TO 999
      END IF

*  Give a warning if three or more parameters were supplied.
      CALL EXPAR( COMM, PARAMS, 2, STATUS )

*  Obtain a range of stack entries to be saved. The default is to save
*  the entire stack, but this is over-ridden by the presence of a
*  second parameter on the command line. ANy second parameter should be
*  a range specification giving the bottom and top indices of the stack
*  entries to be saved. If only one parameter is supplied, the entire
*  stack is saved.
      BOT = 1
      TOP = NONSTK
      CALL GETRNG( PARAMS, 2, .TRUE., COMM, 'Stack limits', 1, NONSTK,
     :             TOP, BOT, STATUS )

*  Get the root name of the NDF (to which will be appended "_STK"). The
*  default is the command name but this is over-ridden by the presence
*  of the first command line parameter.
      CALL GET0C( PARAMS, 1, .TRUE., COMM, 'Name of the NDF ' //
     :            'structure', COMM, ROOT, STATUS )

*  Append the _STK suffix (so long as it doesn't already end with _STK).
      CALL NDFNAM( ROOT, '_STK', NDFNM, STATUS )

*  Tell the user what is about to happen.
      CALL MSG_SETC( 'NDFNAM', NDFNM )
      CALL MSG_SETI( 'TOP', TOP )
      CALL MSG_SETI( 'BOT', BOT )

      IF( TOP .NE. BOT ) THEN
         CALL MSGOUT( COMM, 'Saving stack entries ^BOT to ^TOP in '//
     :                'NDF ''^NDFNAM''.', .FALSE., STATUS )
      ELSE
         CALL MSGOUT( COMM, 'Saving stack entry ^BOT in NDF '//
     :                '''^NDFNAM''.', .FALSE., STATUS )
      END IF

*  Save the stack contents.
      CALL SAVSTK( NDFNM, BOT, TOP, STKSZE, BSTSZE, MAXSTK, NONSTK,
     :             XSTACK, YSTACK, BSTACK, BSTNPT, POINTR, STKNPT,
     :             BPOINT, STITLE, WORVST, STKLST, BSTLST, STATUS  )

*  Jump to here if an error occurs.
 999  CONTINUE

*  If an error has occurred, re-report it with less information if the
*  current MSG message filtering level is not verbose.
      CALL REREP( COMM, 'An error occurred while saving the '//
     :            'stack contents.', STATUS )

      END
