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
*     {enter_new_authors_here}

*  History:
*     09 Dec 1993 (hme):
*        Original version, adapted from Specdre's SPE_HELP.
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
      EXTERNAL SPACK             ! Input routine for help system
      INTEGER SPACK
      EXTERNAL SPACL             ! Output routine for help system
      INTEGER SPACL
      EXTERNAL HLP_NAMETR        ! Library name translation routine
      INTEGER HLP_HELP           ! Help system retrieval routine

*  Local Variables:
      INTEGER STATUS             ! Global status
      INTEGER PAGE, WIDTH        ! Terminal size
      CHARACTER * ( 132 ) LIBRA  ! Help library name
      INTEGER I                  ! Temporary integer
      INTEGER HSTAT              ! Help status
      INTEGER FU                 ! I/O unit
      CHARACTER * ( 64 ) MESSAG  ! An error message

*  Internal References:
      INTEGER CHR_LEN            ! Used length of a string

*.

*  Check inherited global status.
      IF ( IFAIL .NE. 0 ) RETURN

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Get page length and width.
*  If spaef finds no information at all, it will return page and
*  with unchanged, but status will be bad.  That status can be simply
*  set good again, since no ERR calls are made by spaef.
*  If spaef finds only partial information (say a non-zero page but
*  a zero width), we must bring it into the range acceptable.
*  Finally, if the width is allegedly more than 132, we restrict it to
*  132 anyway.
      PAGE  = 24
      WIDTH = 80
      CALL SPAEF( PAGE, WIDTH, IFAIL )
      IF ( IFAIL .NE. 0 ) IFAIL = 0
      IF ( PAGE  .LE. 0 ) PAGE  = 24
      IF ( WIDTH .LE. 0 ) WIDTH = 80
      WIDTH = MIN( WIDTH, 132 )

*  Get library name from environment variable.
      CALL PSX_GETENV( 'SPECX_HELP', LIBRA, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      I = CHR_LEN( LIBRA ) + 1
      LIBRA(I:) = '.shl'

*  Get a free Fortran unit for the library.
      CALL FIO_GUNIT( FU, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Store the page length/width and reset the line counter.
      CALL SPAEG( 3, PAGE, WIDTH, 0, STATUS )

*  Call the help system. This uses a good status of 1.
      HSTAT = HLP_HELP( SPACL, WIDTH, TOPIC, FU, LIBRA, 1, SPACK,
     :   HLP_NAMETR )
      CLOSE( FU )

*  If there was an error detected by the help system, make sure it is
*  reported. The exception are errors -12 and -13 line output and line
*  input failure. These are caused by our I/O routines, and a failure is
*  equivalent to requesting an immediate exit from the application. It
*  should not constitute an error condition and not cause an error
*  report.
      IF ( HSTAT .NE. 1 .AND. HSTAT .NE. -12 .AND. HSTAT .NE. -13 ) THEN
         CALL HLP_ERRMES( HSTAT, MESSAG )
         WRITE( *, * ) 'VMS_HELP: ' // MESSAG
         GO TO 500
      END IF

*  Tidy up.
 500  CONTINUE
      CALL FIO_PUNIT( FU, STATUS )
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE

*  Return.
      END
