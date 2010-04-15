      SUBROUTINE I90HELP( STATUS )
*+
*  Name:
*     I90HELP

*  Purpose:
*     Gives help about IRAS90.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL I90HELP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine displays help information, containing classified and
*     alphabetical lists of commands, general information and related
*     material; it describes individual commands in detail.
*
*     Here are some of the main options:
*
*        I90HELP
*
*           No parameter is given so the introduction and the top-level
*           help index is displayed.
*
*        I90HELP application/topic
*
*           This gives help about the specified application or topic.
*
*        I90HELP application/topic subtopic
*
*           This lists help about a subtopic of the specified
*           application or topic. The hierarchy of topics has a maximum
*           of four levels.
*
*     Once in the help library, it can be navigated in the same way
*     as VMS help libraries.  CTRL/Z (in VMS) or CTRL/D (in UNIX) to
*     exit from any level, and <RET> to move up a level in the
*     hierarchy.

*  Usage:
*     I90HELP [TOPIC] [SUBTOPIC] [SUBSUBTOPIC] [SUBSUBSUBTOPIC]

*  ADAM Parameters:
*     SUBSUBSUBTOPIC = LITERAL (Read)
*        Subsubsubtopic for which help is to be given.             [" "]
*     SUBSUBTOPIC = LITERAL (Read)
*        Subsubtopic for which help is to be given.                [" "]
*     SUBTOPIC = LITERAL (Read)
*        Subtopic for which help is to be given.                   [" "]
*     TOPIC = LITERAL (Read)
*        Topic for which help is to be given.                      [" "]

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1992 (DSB):
*        Original version, modified from KAPPA equivalent written by
*        MJC.
*      8-AUG-2004 (TIMJ):
*        Use SHL
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SSE global definitions


*  Status:
      INTEGER STATUS

*  External References:

*  Local Constants:
      CHARACTER*11 LIBNAM      ! Name of the IRAS90 help library
      PARAMETER ( LIBNAM = 'IRAS90_HELP' )

*  Local Variables:

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the help library application layer
      CALL SHL_ADAM( LIBNAM, .TRUE., STATUS)

      END
