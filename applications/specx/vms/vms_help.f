      SUBROUTINE VMS_HELP( TOPIC, IFAIL )
*+
*  Name:
*     VMS_HELP

*  Purpose:
*     Provide Specx on-line help.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPE_HELP( STATUS )

*  Arguments:
*     TOPIC = CHARACTER * ( * ) (Given)
*        A initial topic to be looked for in the library. If this is not
*        given, the top level of the library will be presented.
*     IFAIL = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine interfaces the portable help library for Specx
*     package with a terminal. The argument TOPIC is used only for
*     the initial entry into the help library. The user can then
*     navigate through the library with the following responses to the
*     prompt:
*     -  A blank response gets you one level up in the topic hierarchy.
*     -  A question mark (?) re-displays the current topic.
*     -  An end-of-file character exits spe_help. Note that this is
*        Ctrl-z under VMS but usually Ctrl-d under Unix.
*     -  Any normal text specifies (sub-) topics to look for.
*     -  Each blank-separated word stands for one topic in the
*        hierarchy. E.g. three blank-separated words go down three
*        levels in the hierarchy.
*     -  Each underscore-separated word stands for an
*        underscore-separated word in a single topic
*     -  Words (whether separated by blanks or underscores) that are not
*        unique topics or include wild card characters are expanded and
*        help is given on all matching topics. Wild card characters are
*        % for a single character and * for any number of characters
*        including none. In the word expansion A_G_N would match
*        active_galactic_nuclei, which is one topic. The same is true
*        for A*_G*_N* or active or active*.
*
*     When the help text to be printed is longer than the terminal page,
*     then the user is asked to press the Return key before proceeding
*     with output. At this point, too, can an end-of-file character be
*     given to exit spe_help immediately.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     timj: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     09 Dec 1993 (hme):
*        Original version, adapted from Specdre's SPE_HELP.
*     28 Jul 2004 (timj):
*        Now uses SHL library (simplifies this a lot)
*     19 Oct 2004 (timj):
*        SHL_GETHLP interface change
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) TOPIC

*  Status:
      INTEGER IFAIL              ! Global status

*  External References:

*  Local Variables:
      INTEGER STATUS             ! Global status
      CHARACTER * ( 132 ) LIBRA  ! Help library name

*  Internal References:

*.

*  Check inherited global status.
      IF ( IFAIL .NE. 0 ) RETURN

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Call SHL routine to determine help library
      CALL SHL_TRNVAR( 'SPECX', .TRUE., LIBRA, STATUS )

*  Call SHL routine for help browser
      CALL SHL_GETHLP( LIBRA, TOPIC, .TRUE., STATUS )

*  Tidy up error context
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE

*  Return.
      END
