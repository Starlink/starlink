      SUBROUTINE DATACUBE_HELP( STATUS )
*+
*  Name:
*     DATACUBE_HELP

*  Purpose:
*     Gives help about DATACUBE_.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DATACUBE_HELP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Displays help about DATACUBE_ using the DATACUBE_HELP.SHL library.
*
*     Here are some of the main options:
*        DATACUBE_HELP
*           No parameter is given so the introduction and the top-level
*           help index is displayed.
*        DATACUBE_HELP application
*           Gives help on the named application.
*
*     Once in the help library, it can be navigated in the normal
*     way.  CTRL/Z to exit from any level, and <CR> to move up a
*     level in the hierarchy.

*  Usage:
*     DATACUBE_HELP [TOPIC] [SUBTOPIC] [SUBSUBTOPIC] [SUBSUBSUBTOPIC]

*  ADAM Parameters:
*     TOPIC = LITERAL (Read)
*        Topic for which help is to be given. [" "]
*     SUBTOPIC = LITERAL (Read)
*        Subtopic for which help is to be given. [" "]
*     SUBSUBTOPIC = LITERAL (Read)
*        Subsubtopic for which help is to be given. [" "]
*     SUBSUBSUBTOPIC = LITERAL (Read)
*        Subsubsubtopic for which help is to be given. [" "]

*  Algorithm:
*     -  Check for error on entry; return if not o.k.
*     -  Obtain topic and subtopics required.  Concatenate them
*        separated by spaces.
*     -  If an error has occurred set all topics to be null.
*     -  Get help on required topic.

*  Implementation Status:
*     -  Uses the portable help system.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     AALLAN: Alasdair Allan (STARLINK, Keele University)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1986 November 14 (MJC):
*        Original.
*     1988 October 11 (MJC):
*        Fixed bug to enable subtopics to be accessed from the command
*        line.
*     1991 September 25 (MJC):
*        Corrected the description for the new layout in the new help
*        library, KAPPA.
*     1992 June 17 (MJC):
*        UNIX version using portable HELP.
*     1992 August 19 (MJC):
*        Rewrote the description, added usage and implementation status.
*     1993 January 5 (PDRAPER):
*        Extract kaphelp system from KAPPA and converted for
*        CCDPACK use. As few changes as possible made.
*     05-NOV-1996 (PDRAPER):
*        Converted to PHOTOMHELP.
*     04-SEP-2000 (AALLAN):
*        Converted to DATACUBE_HELP
*     02-SEP-2004 (TIMJ):
*        Use SHL
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! SSE global definitions

*  Status:
      INTEGER STATUS

*  External References:

*  Local Constants:
      CHARACTER * ( 12 ) LIBNAM ! Name of the DATACUBE_ help library
      PARAMETER ( LIBNAM = 'DATACUBE' )

*  Local Variables:

*  .

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the help library application layer
      CALL SHL_ADAM( LIBNAM, .TRUE., STATUS)

      END
