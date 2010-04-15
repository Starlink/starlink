      SUBROUTINE CATHELP( STATUS )
*+
*  Name:
*     CATHELP

*  Purpose:
*     Gives help about CATPAC.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CATHELP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Displays help about CATPAC using the CATAHELP.SHL library.
*
*     Here are some of the main options:
*        CATHELP
*           No parameter is given so the introduction and the top-level
*           help index is displayed.
*        CATHELP application
*           Gives help on the named application.
*
*     Once in the help library, it can be navigated in the normal
*     way.  CTRL/Z to exit from any level, and <CR> to move up a
*     level in the hierarchy.

*  Usage:
*     CATHELP [TOPIC] [SUBTOPIC] [SUBSUBTOPIC] [SUBSUBSUBTOPIC]

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
*     ARW: Alan Wood (STPDF)
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
*     19-MAR-1993 (PDRAPER):
*        Converted to PISAHELP.
*     8-APR-1994 (ARW):
*        Converted to CATHELP.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_PAR'
      INCLUDE 'PAR_ERR'        ! Parameter-system errors

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER
     :  CHR_LEN                ! Length of character strings ignoring
                               ! trailing blanks

*  Local Constants:
      CHARACTER*11 LIBNAM         ! Name of the CATPAC help library
      PARAMETER ( LIBNAM = 'CATPAC_HELP' )
      INTEGER MAXLEV             ! Maximum number of help levels
      PARAMETER ( MAXLEV = 4 )

*  Local Variables:
      CHARACTER*19
     :  HLPTXT*80,               ! Composite command
     :  LIBRAY*132,              ! Library name and path
     :  PATH*122,                ! Library path
     :  TOPIC( MAXLEV )          ! Name of the topic required

      INTEGER
     :  I,                       ! Loop counter
     :  NC                       ! Number of characters in the help string
                                 ! less trailing blanks
*  .

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Translate the environment variable/logical name.
      CALL PSX_GETENV( LIBNAM, PATH, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Form the full library name.
         NC = CHR_LEN( PATH )
         LIBRAY = PATH( :NC )//'.shl'

*  Get topic and subtopics.
         CALL PAR_GET0C( 'TOPIC', TOPIC(1), STATUS )
         CALL PAR_CANCL( 'TOPIC', STATUS )

         CALL PAR_GET0C( 'SUBTOPIC', TOPIC(2), STATUS )
         CALL PAR_CANCL( 'SUBTOPIC', STATUS )

         CALL PAR_GET0C( 'SUBSUBTOPIC', TOPIC(3), STATUS )
         CALL PAR_CANCL( 'SUBSUBTOPIC', STATUS )

         CALL PAR_GET0C( 'SUBSUBSUBTOPIC', TOPIC(4), STATUS )
         CALL PAR_CANCL( 'SUBSUBSUBTOPIC', STATUS )

*  Concatenate the help topics into a single string
         HLPTXT = TOPIC( 1 )
         NC = CHR_LEN( TOPIC( 1 ) ) + 1
         DO I = 2, MAXLEV
            CALL CHR_APPND( ' '//TOPIC( I ), HLPTXT, NC )
         END DO

*  Use a null string when something has gone wrong obtaining the
*  topics and sub-topics.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            HLPTXT = '         '
         END IF

*  Get help text.
         CALL GETHLP( LIBRAY, HLPTXT, STATUS )
      END IF

      END
