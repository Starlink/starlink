      SUBROUTINE ATL1_RDCH( IGRP, IAST, STATUS )
*+
*  Name:
*     ATL1_RDCH

*  Purpose:
*     Read an AST Object from a GRP group using a Channel.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL1_RDCH( IGRP, IAST, STATUS )

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

*  Local Variables:
      INTEGER CHAN
*.

*  Initialise.
      IAST = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Store the group identifer in common so that the source function can
*  get at it. 
      IGRPC = IGRP

*  Initialise the next group element to be read.
      NEXT = 1

*  Store the size of the group.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )

*  Create a Channel through which to read the Objects stored in the
*  group. 
      CHAN = AST_CHANNEL( ATL1_SRC1, AST_NULL, ' ', STATUS ) 

*  Attempt to read an object from the current channel.
      IAST = AST_READ( CHAN, STATUS )

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
