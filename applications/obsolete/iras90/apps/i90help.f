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
*     LIBRARY = LITERAL (Read)
*        The Starlink help library from which help text is to be 
*        obtained.                              [IRAS90_DIR:IRAS90_HELP]
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
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'PAR_ERR'        ! Parameter-system errors

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER
     :  CHR_LEN                ! Length of character strings ignoring
                               ! trailing blanks

*  Local Constants:
      INTEGER MAXLEV           ! Maximum number of help levels
      PARAMETER ( MAXLEV = 4 )

*  Local Variables:
      CHARACTER*19
     :  HLPTXT*80,             ! Composite command
     :  LIBRAY*132,            ! Library name 
     :  TOPIC( MAXLEV )        ! Name of the topic required

      INTEGER
     :  I,                     ! Loop counter
     :  NC                     ! Number of characters in the help string
                               ! less trailing blanks
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the name of the help library to use.
      CALL PAR_GET0C( 'LIBRARY', LIBRAY, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Form the full library name.
         NC = CHR_LEN( LIBRAY )
         IF( LIBRAY( NC - 3: ) .NE. '.SHL' .AND.
     :       LIBRAY( NC - 3: ) .NE. '.shl' ) THEN
            CALL CHR_APPND( '.shl', LIBRAY, NC )
         END IF

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
         CALL IRM_GTHLP( LIBRAY, HLPTXT, STATUS )
      END IF

*  If a parameter null or abort value was given, annul the error.
      IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'I90HELP_ERR1',
     :                 'I90HELP: Error displaying help.',
     :                  STATUS )
      END IF
      
      END
