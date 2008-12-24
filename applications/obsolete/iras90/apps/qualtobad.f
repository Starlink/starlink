      SUBROUTINE QUALTOBAD( STATUS )
*+
*  Name:
*     QUALTOBAD

*  Purpose:
*     Set selected NDF pixels bad on the basis of quality.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL QUALTOBAD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine copies a group of input NDFs to a group of
*     corresponding output NDFs, setting selected pixels bad in the
*     process. Such pixels are selected on the basis of quality; any
*     pixel which holds a set of qualities which satisfies the quality
*     expression given for parameter QEXP is set bad in the DATA and
*     (if it is defined) VARIANCE components of the output NDF.  See
*     help on "Quality_in_IRAS90" for further information about the use
*     of quality within IRAS90.

*  Usage:
*     QUALTOBAD IN OUT QEXP

*  ADAM Parameters:
*     HISTORY = _LOGICAL (Read)
*        Determines if history information is to be stored within the
*        output NDF. See help on "History_in_IRAS90" for more
*        information on history.               [current history setting]
*     IN = NDF (Read)
*        A group of input NDFs. This should be in the form of a group
*        expression (see help on "Group_expressions"). If an input NDF
*        does not contain definitions of all the quality names
*        referenced within the quality expression given for parameter
*        QEXP, then no corresponding output NDF is created, but
*        processing continues with the other input NDFs.
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").
*                                       [current message filter setting]
*     OUT = NDF (Write)
*        A group of output NDFs corresponding one-for-one with the list
*        of input NDFs given for parameter IN.  This should be in the
*        form of a group expression (see help on "Group_expressions").
*        Expressions such as "*_NEW" are expanded by replacing the "*"
*        character with each input NDF in turn.
*     QEXP = LITERAL (Read)
*        The quality expression. 

*  Examples:
*     QUALTOBAD M51* *_CLEAN SATURATED.OR.GLITCH
*        This example copies all NDFs starting with the string "M51" to
*        a set of corresponding output NDFs. The name of each output
*        NDF is formed by extending the name of the input NDF with the
*        string "_CLEAN". Any pixels which hold either of the qualities
*        "SATURATED" or "GLITCH" are set to the bad value in the output
*        NDFs.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-OCT-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PAR_ERR'          ! PAR status constants
      INCLUDE 'MSG_PAR'          ! MSG constants.
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error constants.
      INCLUDE 'GRP_PAR'          ! GRP constants.

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns used length of a character string.

*  Local Variables:
      LOGICAL ALLBAD             ! True if all output data pixels are
                                 ! bad.
      CHARACTER COMC*1           ! Output group comment character.
      INTEGER ERRPNT             ! Offset to error within quality
                                 ! expression.
      CHARACTER HSTORY(3)*(MSG__SZMSG)! Lines of history to be added to
                                 ! each output NDF.
      INTEGER I                  ! Index into input and output groups.
      INTEGER IDQ                ! An identifier for the compiled
                                 ! quality expression.
      INTEGER IGRP1              ! Identifier for group holding input
                                 ! NDFs.
      INTEGER IGRP2              ! Identifier for group holding all
                                 ! candidate output NDFs.
      INTEGER IPNT               ! Pointer to a mapped array in the
                                 ! output NDF.
      INTEGER J                  ! Index into list of undefined quality names.
      CHARACTER LOCS(5)*(DAT__SZLOC)! Locators to quality information.
      INTEGER NDFIN              ! Identifier for the input NDF.
      INTEGER NDFOUT             ! Identifier for the output NDF.
      INTEGER NEL                ! No. of elements mapped from the
                                 ! output DATA array.
      LOGICAL NOBAD              ! True if no output data pixels are
                                 ! bad.
      INTEGER NOUT               ! No. of good output NDFs.
      INTEGER NUNDEF             ! Number of undefined quality names
                                 ! referenced in the quality
                                 ! expression.
      CHARACTER OUTNDF*(GRP__SZNAM)! The name of the current output NDF.
      CHARACTER QEXP*(IRQ__SZQEX)! Quality expression.
      INTEGER SIZE               ! Total size of the input group.
      INTEGER SIZEO              ! Total size of the output group.
      CHARACTER TEXT*(GRP__SZNAM)! Text for output group list file.
      LOGICAL THERE              ! True if VARIANCE is in a defined
                                 ! state.
      INTEGER TLEN               ! Used length of text string.
      CHARACTER UNDEF(IRQ__QNREF)*(IRQ__SZQNM)! List of undefined
                                 ! quality names referenced in the
                                 ! quality expression.
      CHARACTER XNAME*(DAT__SZNAM)! Name of NDF extension containing
                                 ! quality name information.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      NOUT = 0

*  Establish the conditional message filter level using parameter
*  MSG_LEVEL.
      CALL MSG_IFGET( 'MSG_FILTER', STATUS )

*  Get a syntactically correct quality expression from the environment.
      CALL IRM_GETQX( 'QEXP', QEXP, STATUS )

*  Get a group containing the names of the NDFs to be processed.
      CALL IRM_RDNDF( 'IN', 0, 1, '  Give more NDF names...', 
     :                IGRP1, SIZE, STATUS )

*  Similarly, get a group containing the names of the output NDFs.
*  Base modification elements on the group containing the input NDFs.
      CALL IRM_WRNDF( 'OUT', IGRP1, SIZE, SIZE,
     :                '  Give more NDF names...',
     :                 IGRP2, SIZEO, STATUS )

*  Get the character used to introduce comments into group expressions
*  relating to the output group.
      CALL GRP_GETCC( IGRP2, 'COMMENT', COMC, STATUS )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Loop round each NDF to be processed.
      NOUT = 0
      DO I = 1, SIZE
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRP1, I, 'READ', NDFIN, STATUS )

*  Tell the user which input NDF is currently being procesed.
         CALL NDF_MSG( 'NDF', NDFIN )
         CALL MSG_OUTIF( MSG__NORM, 'QUALTOBAD_MSG1',
     :                   '  Processing ^NDF...', STATUS )

*  Obtain an identifier the output NDF, propagating all components from
*  the input to the output (the HISTORY, LABEL, TITLE and all
*  extensions are propagated by default).
         CALL NDG_NDFPR( NDFIN, 'UNITS,DATA,VARIANCE,QUALITY,AXIS',
     :                   IGRP2, I, NDFOUT, STATUS )

*  Attempt to locate any existing quality name information in the input
*  NDF. If such information is found, LOCS is returned holding a set of
*  5 HDS locators which identify the NDF and the various components of
*  the quality information. XNAME is returned holding the name of the
*  NDF extension in which the information was found. If no quality name
*  information is found, then an error is reported.
         CALL IRQ_FIND( NDFIN, LOCS, XNAME, STATUS )

*  Attempt to compile the quality expression. An IRQ identifier is
*  returned for the compiled expression if it compiles succesfully.
         CALL IRQ_COMP( LOCS, IRQ__QNREF, .FALSE., QEXP, UNDEF, NUNDEF,
     :                  ERRPNT, IDQ, STATUS )

*  Map the DATA component in the output NDF.
         CALL NDF_MAP( NDFOUT, 'DATA', '_REAL', 'UPDATE', IPNT, NEL,
     :                 STATUS )

*  Set bad all output DATA pixels which satisfy the given quality
*  expression.
         CALL IRQ_SBAD( IDQ, .TRUE., NEL, %VAL( IPNT ), ALLBAD, NOBAD,
     :                  STATUS )

*  If the output contains no valid data, give a warning message.
         IF( ALLBAD ) THEN
            CALL NDF_MSG( 'NDF', NDFOUT )
            CALL MSG_OUTIF( MSG__QUIET, 'QUALTOBAD_MSG2',
     :               'WARNING: ^NDFOUT contains no valid data', STATUS )
         END IF

*  Set the bad-pixel flag for the DATA array of the output NDF.
         CALL NDF_SBAD( .NOT.NOBAD, NDFOUT, 'DATA', STATUS )

*  Unmap the DATA component.
         CALL NDF_UNMAP( NDFOUT, 'DATA', STATUS )

*  See if the VARIANCE component of the output NDF is in a defined
*  state.
         CALL NDF_STATE( NDFOUT, 'VARIANCE', THERE, STATUS )

*  If it is, process the VARIANCE array in the same way that the DATA
*  array was processed.
         IF( THERE ) THEN
            CALL NDF_MAP( NDFOUT, 'VARIANCE', '_REAL', 'UPDATE', IPNT,
     :                    NEL, STATUS )
            CALL IRQ_SBAD( IDQ, .TRUE., NEL, %VAL( IPNT ), ALLBAD,
     :                     NOBAD, STATUS )

            IF( ALLBAD ) THEN
               CALL NDF_MSG( 'NDF', NDFOUT )
               CALL MSG_OUTIF( MSG__QUIET, 'QUALTOBAD_MSG3',
     :                 'WARNING: ^NDFOUT contains no valid variances',
     :                         STATUS )
            END IF

            CALL NDF_SBAD( .NOT.NOBAD, NDFOUT, 'VARIANCE', STATUS )
            CALL NDF_UNMAP( NDFOUT, 'VARIANCE', STATUS )

         END IF

*  Add a history record to the output NDF.
         CALL NDF_MSG( 'OUT', NDFOUT )
         CALL MSG_LOAD( ' ', 'Output NDF: ^OUT',
     :                  HSTORY( 1 ), TLEN, STATUS )

         CALL NDF_MSG( 'IN', NDFIN )
         CALL MSG_LOAD( ' ', 'Input NDF: ^IN',
     :                  HSTORY( 2 ), TLEN, STATUS )

         CALL MSG_SETC( 'QEXP', QEXP )
         CALL MSG_LOAD( ' ', 'Quality expression: ^QEXP',
     :                  HSTORY( 3 ), TLEN, STATUS )

         CALL IRM_HIST( 'HISTORY', NDFOUT, 'IRAS90:QUALTOBAD', 3,
     :                  HSTORY, STATUS )

*  Release the quality name information in the input NDF.
         CALL IRQ_RLSE( LOCS, STATUS )

*  Annul the identifier for the compiled quality expression.
         CALL IRQ_ANNUL( IDQ, STATUS )

*  Annul the input NDF identifier.
         CALL NDF_ANNUL( NDFIN, STATUS )

*  If an error has occurred, delete the output NDF, otherwise just 
*  annul its identifier.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL NDF_DELET( NDFOUT, STATUS )
         ELSE
            CALL NDF_ANNUL( NDFOUT, STATUS )
         END IF

*  If an error occured processing the current input NDF...
         IF( STATUS .NE. SAI__OK ) THEN

*  Annul the error and give a more friendly report if the problem was 
*  that some quality names were not defined.
            IF( STATUS .EQ. IRQ__NOQNM ) THEN
               CALL ERR_ANNUL( STATUS )

               CALL MSG_SETC( 'C', 
     :    '    The following quality names are undefined in this NDF:')
               DO J = 1, NUNDEF 
                  CALL MSG_SETC( 'C', ' ' )
                  CALL MSG_SETC( 'C', UNDEF( J ) )
               END DO
               CALL MSG_OUTIF( MSG__NORM, 'QUALTOBAD_MSG4', '^C', 
     :                         STATUS )

*  Annul the error and give a more friendly report if the problem was 
*  that no quality names information was found.
            ELSE IF( STATUS .EQ. IRQ__NOQNI ) THEN
               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUTIF( MSG__NORM, 'QUALTOBAD_MSG5',
     :             '    This NDF contains no quality name definitions', 
     :                        STATUS )

*  If the problem was neither of these two things, flush the error.
            ELSE
               CALL ERR_FLUSH( STATUS )

            END IF

*  Give a warning telling the user that no output NDF will be created 
*  for the current input NDF.
            CALL GRP_GET( IGRP2, I, 1, OUTNDF, STATUS )
            CALL MSG_SETC( 'NDF', OUTNDF )
            CALL MSG_OUTIF( MSG__QUIET, 'QUALTOBAD_MSG6',
     :                      'WARNING: ^NDF cannot be produced',
     :                      STATUS )

*  Overwrite the current output NDF name with a string which will be
*  interpreted as a comment by later applications.
            TEXT = COMC//'          '//OUTNDF( : CHR_LEN( OUTNDF ) )//
     :             ' could not be produced'
            CALL GRP_PUT( IGRP2, 1, TEXT, I, STATUS )

*  If no error occurred, increment the number of good output NDFs.
         ELSE
            NOUT = NOUT + 1
         END IF

*  Process the next input NDF.
      END DO

*  Display a blank line.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Close down the IRQ identifier system.
 999  CONTINUE
      CALL IRQ_CLOSE( STATUS )

*  Assign a group expression to the output parameter NDFLIST which
*  specifies all the output NDFs. NDFLIST should normally be associated 
*  with a suitable global parameter to cause its value to be passed on 
*  to the next application.  The output parameter NDFLIST is not 
*  advertised as a user parameter since users will normally not be 
*  aware of the existence of global parameter, and so will not know 
*  how to assign a value to it.
      IF( NOUT .GT. 0 ) CALL IRM_LISTN( 'NDFLIST', IGRP2, 'QUALTOBAD', 
     :                                   STATUS )

*  Delete all groups.
      CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN

*  If a null parameter was given or a parameter abort was requested, 
*  annul the error.
         IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )

*  If any other error occurred, then report a contextual message.
         ELSE
            CALL ERR_REP( 'QUALTOBAD_ERR1',
     :    'QUALTOBAD: Unable to convert quality information into bad '//
     :                    'pixels.', STATUS )
         END IF

      END IF

      END
