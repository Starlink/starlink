      SUBROUTINE ATL1_RDFCH( IGRP, IAST, STATUS )
*+
*  Name:
*     ATL1_RDFCH

*  Purpose:
*     Read an AST Object from a GRP group using a FitsChan.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL1_RDFCH( IGRP, IAST, STATUS )

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
      EXTERNAL ATL1_SRC2

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

*  Create a FitsChan through which to read the Objects stored in the
*  group. 
      CHAN = AST_FITSCHAN( ATL1_SRC2, AST_NULL, ' ', STATUS ) 

*  Attempt to read an object from the current channel.
      IAST = AST_READ( CHAN, STATUS )

*  Export the returned Object from the current AST context so that it is
*  not annulled by the following call to AST_END. If an error has occurred,
*  the Object will not be exported, and so will be annulled by AST_END.
      CALL AST_EXPORT( IAST, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END




      INTEGER FUNCTION ATL1_SRC2( BUF, STATUS )
*+
*  A source function for use with an AST FitsChan.
*-
      INCLUDE 'SAE_PAR'
      
*  Arguments:
      CHARACTER BUF*80
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
