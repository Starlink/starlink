      SUBROUTINE ATL1_RDGRP( IGRP, IAST, STATUS )
*+
*  Name:
*     ATL1_RDGRP

*  Purpose:
*     Read an AST Object from a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL1_RDGRP( IGRP, IAST, STATUS )

*  Description:

*  Arguments:
*     IGRP = INTEGER (Given)
*        An identifier for the group holding the text.
*     IAST = INTEGER (Returned)
*        The AST Object, or AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JAN-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      INTEGER IGRP

*  Arguments Returned:
      INTEGER IAST

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables.
      INTEGER IGRPC
      INTEGER NEXT
      INTEGER SIZE
      COMMON /ATL1SRC/ IGRPC, NEXT, SIZE

*  External References:
      EXTERNAL ATL1_SRC1
      EXTERNAL ATL1_SRC2

*  Local Constants:
      INTEGER NCHAN
      PARAMETER ( NCHAN = 2 )

*  Local Variables:
      CHARACTER TEXT*(GRP__SZNAM)
      INTEGER CH( NCHAN )
      INTEGER CHAN
      INTEGER FCHAN
      INTEGER I
      LOGICAL DUMP
*.

*  Initialise.
      IAST = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Determine the most likely format of the text in the group. If the
*  group contains a line beginning with the word "Begin" it is probably a
*  dump of an AST object produced by AST_SHOW.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )
      DUMP = .FALSE.
      DO I = 1, SIZE
         CALL GRP_GET( IGRP, I, 1, TEXT, STATUS ) 
         CALL CHR_LDBLK( TEXT )
         IF( TEXT( : 6 ) .EQ. 'Begin ' ) THEN
            DUMP = .TRUE.
            GO TO 10
         END IF
      END DO
 10   CONTINUE

*  We now create channels through which to read the Objects stored in the
*  group. 
       CHAN = AST_CHANNEL( ATL1_SRC1, AST_NULL, ' ', STATUS ) 
       FCHAN = AST_FITSCHAN( ATL1_SRC2, AST_NULL, ' ', STATUS ) 

*  We use the least likely one first, so that he errors from the
*  most likely one are reported to the user at the end.
      IF( DUMP ) THEN
         CH( 1 ) = FCHAN
         CH( 2 ) = CHAN
      ELSE
         CH( 1 ) = CHAN
         CH( 2 ) = FCHAN
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each channel.
      DO I = 1, NCHAN

*  Reset the group.
         NEXT = 1

*  Attempt to read an object from the current channel.
         IAST = AST_READ( CH( I ), STATUS )

*  If no error occurred, leave the loop.
         IF( STATUS .EQ. SAI__OK ) THEN 
            GO TO 999

*  Otherwise, if any channels remain to be tried, annul the error.
         ELSE IF( I .NE. NCHAN ) THEN
            CALL AST_ANNUL( IAST, STATUS )
            CALL ERR_ANNUL( STATUS )
         END IF

      END DO

*  Arrive here when finished.
 999  CONTINUE

*  Export the returned Object from the current AST context so that it is
*  not annulled by the following call to AST_END. If an error has occurred,
*  the Object will not be exported, and so will be annulled by AST_END.
      CALL AST_EXPORT( IAST, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END


      SUBROUTINE ATL1_SRC1( STATUS )
*+
*  A source function for use with a standard AST Channel.
*-
      INCLUDE 'SAE_PAR'
      INCLUDE 'GRP_PAR'
      
*  Arguments:
      INTEGER STATUS

*  Global Variables.
      INTEGER IGRPC
      INTEGER NEXT
      INTEGER SIZE
      COMMON /ATL1SRC/ IGRPC, NEXT, SIZE

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER BUF*(GRP__SZNAM)

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If there are no more lements in the group, return a length of -1.
      IF( NEXT .GT. SIZE ) THEN
         CALL AST_PUTLINE( ' ', -1, STATUS )

*  Otherwise, get the element from the group, store it in the channel,
*  and increment the index of the next element to be read from the group.
      ELSE
         CALL GRP_GET( IGRPC, NEXT, 1, BUF, STATUS ) 
         CALL AST_PUTLINE( BUF, CHR_LEN( BUF ), STATUS )
         NEXT = NEXT + 1
      END IF

      END


      INTEGER FUNCTION ATL1_SRC2( BUF, STATUS )
*+
*  A source function for use with an AST FitsChan.
*-
      INCLUDE 'SAE_PAR'
      
*  Arguments:
      CHARACTER BUF*(*)
      INTEGER STATUS

*  Global Variables.
      INTEGER IGRPC
      INTEGER NEXT
      INTEGER SIZE
      COMMON /ATL1SRC/ IGRPC, NEXT, SIZE

*  Initialise things to indicate "no more headers".
      BUF = ' '
      ATL1_SRC2 = 0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If any elements remain to be read, get the next element from the group,
*  and increment the index of the next element to be read from the group. 
*  If this is not the last element in the group, return a function value of 
*  one.
      IF( NEXT .LE. SIZE ) THEN 
         CALL GRP_GET( IGRPC, NEXT, 1, BUF, STATUS ) 

         IF( NEXT .LT. SIZE ) THEN
            NEXT = NEXT + 1
            ATL1_SRC2 = 1
         END IF

      END IF

      END
