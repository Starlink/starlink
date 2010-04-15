      SUBROUTINE TRACC2( PARAM, FLAGS, STATUS )
*+
*  Name:
*     TRACC2

*  Purpose:
*     Get the sections to include in the display from the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACC2( PARAM, FLAGS, STATUS )

*  Description:
*     This routine uses the supplied parameter to obtain a group of
*     objects to be omitted from the display produced by routine
*     TRACA7. A set of flags are returned indicating which of these
*     objects are to be plotted, and which are not. A true value for the
*     flag means the corresponding object is to be plotted, and a false
*     value means that it is not to be plotted. The sections are refered
*     to by thefollowing names:
*
*     IN-LINE:  the in-line detector number labels = FLAGS(1).
*
*     LABEL_X:  the X axis label = FLAGS(2).
*
*     LABEL_Y:  the Y axis label = FLAGS(3).
*
*     SCANDIR:  the scan direction indicator = FLAGS(4).
*
*     MARKS_RHS:  the right hand detector offset markers = FLAGS(5).
*
*     MARKS_LHS:  the left hand detector offset markers (including
*                 detector numbers) = FLAGS(6).
*
*     TABLE:  the table of detector offsets = FLAGS(7).
*
*     REF_POS:  the description of the reference position = FLAGS(8).
*
*     In addition the string NOTHING can be given to indicate that no
*     sections should be omitted from the display.
*
*     Section names can be abbreviated. If an ambiguous abbreviation
*     is supplied, than all sections matching the abbreviation are
*     omitted from the display. Thus, for instance, the string "M" would
*     cause both left and right hand markers to be omitted.
*
*     The section specifiers are obtained in the form of a GRP group
*     expression in the normal way.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter to use.
*     FLAGS( 8 ) = LOGICAL (Given and Returned)
*        Flags indicating which parts of the display are required:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-NOV-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Given and Returned:
      LOGICAL FLAGS( 8 )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string.

*  Local Constants:
      INTEGER NFLAG              ! No. of section flags.
      PARAMETER ( NFLAG = 8 )

*  Local Variables:
      CHARACTER GRPEXP*(GRP__SZGEX)! Group expression.
      CHARACTER OPT(NFLAG)*9     ! Section names.
      CHARACTER SPEC*(GRP__SZNAM)! Display section specification.

      INTEGER ADDED              ! No. of items added to the group.
      INTEGER I                  ! Loop count.
      INTEGER IGRP1              ! Group identifier.
      INTEGER J                  ! Loop count.
      INTEGER LSPEC              ! Used length of SPEC.
      INTEGER NBAD               ! No. of re-prompts so far.
      INTEGER NMATCH             ! No. of matches between specification
                                 ! and list of options.
      INTEGER NSPEC              ! No. of items in group.

      LOGICAL FLAG               ! True if more values to be given.
      LOGICAL MORE               ! True if more re-prompts are needed.

*  Local Data:
      DATA OPT/ 'IN-LINE', 'LABEL_X', 'LABEL_Y', 'SCAN_DIR',
     :          'MARKS_RHS', 'MARKS_LHS', 'TABLE', 'REF_POS' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a group to hold a list of the display sections to omit.
      CALL GRP_NEW( 'Display sections', IGRP1, STATUS )

*  Ensure that the group is case insensitive.
      CALL GRP_SETCS( IGRP1, .FALSE., STATUS )

*  Loop until a good group of display sections is obtained.
      NBAD = 0
      MORE = .TRUE.
      DO WHILE( MORE )

*  Initialise all the flags to cause all parts of the display
*  to be included.
         DO I = 1, NFLAG
            FLAGS( I ) = .TRUE.
         END DO

*  Obtain a group of display sections. Loop until the group
*  expression obtained from the environment does not terminate with a
*  minus sign. The parameter is cancelled each time.
         FLAG = .TRUE.
         NSPEC = 0
         DO WHILE( FLAG .AND. STATUS .EQ. SAI__OK )

            CALL PAR_GET0C( PARAM, GRPEXP, STATUS )
            CALL PAR_CANCL( PARAM, STATUS )
            CALL GRP_GRPEX( GRPEXP, GRP__NOID, IGRP1, NSPEC, ADDED,
     :                      FLAG, STATUS )
            IF( FLAG ) CALL MSG_OUTIF( MSG__NORM, 'TRACC2_MSG1',
     :                      '  Please specify more display sections...',
     :                                 STATUS )

         END DO

*  The user may have indicated the end of the group by giving a null
*  value for the parameter. This would normally cause the application to
*  abort, so annul the error in order to prevent this from happening.
         IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Loop round each element in the group.
         DO I = 1, NSPEC

*  Get the next element.
            CALL GRP_GET( IGRP1, I, 1, SPEC, STATUS )

*  Remove leading blanks and get the remaining length.
            CALL CHR_LDBLK( SPEC )
            LSPEC = CHR_LEN( SPEC )

*  If this element is NOTHING (or an abbreviation), then it should be
*  the only element in the group. Report an error if this is not so.
            IF( INDEX( 'NOTHING', SPEC( : LSPEC ) ) .EQ. 1 ) THEN
               IF( NSPEC .NE. 1 .AND. STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'TRACC2_ERR1',
     :   'TRACC2: Option "NOTHING" cannot be mixed with other options.',
     :                          STATUS )
               END IF

*  If this option specifies a sections to omit...
            ELSE

*  ...loop round each of the options, setting the flags for any which
*  match the current element.
               NMATCH = 0

               DO J = 1, NFLAG
                  IF( INDEX( OPT( J ), SPEC( : LSPEC ) ) .EQ. 1 ) THEN
                     FLAGS( J ) = .FALSE.
                     NMATCH = NMATCH + 1
                  END IF
               END DO

*  If no match was found, report an error.
               IF( NMATCH .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'C', SPEC )
                  CALL ERR_REP( 'TRACC2_ERR2',
     :        'TRACC2: No display sections start with the string "^C".',
     :                           STATUS )
               END IF

            END IF

         END DO

*  If an error has occurred (other than PAR__ABORT), add a report
*  listing the available options, and another asking the user to try
*  again. Flush the errors, and reset the group size to zero.
         IF( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT .AND.
     :       NBAD .LE. 4 ) THEN

            DO J = 1, NFLAG
               CALL MSG_SETC( 'L', OPT( J ) )
               CALL MSG_SETC( 'L', ',' )
            END DO

            CALL ERR_REP( 'TRACC2_ERR3',
     :                    '  Available options are: ^L or NOTHING.',
     :                    STATUS )
            CALL ERR_REP( 'TRACC2_ERR4',
     :                  '  Please give a new group of options',
     :                    STATUS )

            CALL ERR_FLUSH( STATUS )
            CALL GRP_SETSZ( IGRP1, 0, STATUS )
            NBAD = NBAD + 1

*  Otherwise, indicate that no more prompting is needed.
         ELSE
            MORE = .FALSE.
         END IF

      END DO

*  Delete the group.
 999  CONTINUE
      CALL GRP_DELET( IGRP1, STATUS )

      END
