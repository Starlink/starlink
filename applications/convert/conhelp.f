      SUBROUTINE KAPHELP( STATUS )
*+
*  Name:
*     KAPHELP

*  Purpose:
*     Gives help about CONVERT.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL KAPHELP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Displays help about CONVERT.  The help information has classified
*     and alphabetical lists of commands, general information about
*     CONVERT and related material; it describes individual commands in
*     detail.
*
*     Here are some of the main options:
*        CONHELP
*           No parameter is given so the introduction and the top-level
*           help index is displayed.
*        CONHELP application/topic
*           This gives help about the specified application or topic.
*        CONHELP application/topic subtopic
*           This lists help about a subtopic of the specified
*           application or topic. The hierarchy of topics has a maximum
*           of four levels.
*        CONHELP SUMMARY
*           This shows a one-line summary of each application.

*     Once in the help library, it can be navigated in the normal
*     way.  CTRL/Z (on VMS) and CTRL/D (on UNIX) to exit from any level,
*     and <CR> to move up a level in the hierarchy.

*  Usage:
*     CONHELP [TOPIC] [SUBTOPIC] [SUBSUBTOPIC] [SUBSUBSUBTOPIC]

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
*     -  The help libraries are slightly different for VMS and UNIX.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 July 30 (MJC):
*        Original based on KAPHELP.
*     {enter_any_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'PAR_ERR'          ! Parameter-system errors

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Length of character strings ignoring
                                 ! trailing blanks

*  Local Constants:
      CHARACTER * ( 12 ) LIBNAM  ! Name of the CONVERT help library
      PARAMETER ( LIBNAM = 'CONVERT_HELP' )
      INTEGER MAXLEV             ! Maximum number of help levels
      PARAMETER ( MAXLEV = 4 )

*  Local Variables:
      CHARACTER HLPTXT * ( 80 )  ! Composite command
      CHARACTER * ( 132 ) LIBRAY ! Library name and path
      CHARACTER * ( 122 ) PATH   ! Library path
      CHARACTER * ( 19 ) TOPIC( MAXLEV ) ! Name of the topic required

      INTEGER I                  ! Loop counter
      INTEGER NC                 ! Number of characters in the help
                                 ! string less trailing blanks
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Translate the environment variable/logical name.
      CALL PSX_GETENV( LIBNAM, PATH, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Form the full library name.
         NC = CHR_LEN( PATH )
         LIBRAY = PATH( :NC )//'.shl'

*  Get topic and subtopics.
         CALL PAR_GET0C( 'TOPIC', TOPIC( 1 ), STATUS )
         CALL PAR_CANCL( 'TOPIC', STATUS )

         CALL PAR_GET0C( 'SUBTOPIC', TOPIC( 2 ), STATUS )
         CALL PAR_CANCL( 'SUBTOPIC', STATUS )

         CALL PAR_GET0C( 'SUBSUBTOPIC', TOPIC( 3 ), STATUS )
         CALL PAR_CANCL( 'SUBSUBTOPIC', STATUS )

         CALL PAR_GET0C( 'SUBSUBSUBTOPIC', TOPIC( 4 ), STATUS )
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
         CALL CON_GHELP( LIBRAY, HLPTXT, STATUS )
      END IF

      END
