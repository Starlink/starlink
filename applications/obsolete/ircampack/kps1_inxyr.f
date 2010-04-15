      SUBROUTINE KPS1_INXYR( PNAME, NVERT, IPX, IPY, STATUS )
*+
*  Name:
*     KPS1_INXYR

*  Purpose:
*     Obtain a list of vertex co-ordinates from the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_INXYR( PNAME, NVERT, IPX, IPY, STATUS )

*  Description:
*     The supplied parameter is used to get a list of X,Y coordinates
*     from the environment. These coordinates are stored in dynamic
*     workspace which is expanded as necessary so that any number of
*     vertices can be given. The user indicates the end of the list of
*     vertices by a null value. Pointers to the workspace arrays holding
*     the vertex coordinates are returned, and should be annulled when
*     no longer needed by calling PSX_FREE.

*  Arguments:
*     PNAME = CHARACTER * ( * ) (Given)
*        Name of the parameter with which to associate the co-ordinates.
*     NVERT = INTEGER (Returned)
*        The number of vertices specified.
*     IPX = INTEGER (Returned)
*        A pointer to workspace of type _REAL and size NVERT holding the
*        X co-ordinate of each vertex.
*     IPY = INTEGER (Returned)
*        A pointer to workspace of type _REAL and size NVERT holding the
*        Y co-ordinate of each vertex.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants

*  Arguments Given:
      CHARACTER PNAME*(*)

*  Arguments Returned:
      INTEGER NVERT
      INTEGER IPX
      INTEGER IPY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER INITSZ             ! Initial workspace size in _REALs
      PARAMETER ( INITSZ = 20 )

*  Local Variables:
      REAL XY( 2 )               ! (X,Y) coordinate pair
      INTEGER WSIZE              ! Current size of workspace in bytes

*.

*  Initialise the number of vertices in the polygon to indicate that no
*  polygon has yet been supplied.
      NVERT = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the first pair of coordinates using the supplied parameter.
      CALL PAR_EXACR( PNAME, 2, XY, STATUS )

*  Allocate workspace to hold a reasonable number of vertices. This will
*  be expanded if necessary.
      WSIZE = INITSZ*VAL__NBR
      CALL PSX_MALLOC( WSIZE, IPX, STATUS )
      CALL PSX_MALLOC( WSIZE, IPY, STATUS )

*  Loop round obtaining new vertices until an error occurs.
      DO WHILE( STATUS .EQ. SAI__OK )

*  Increment the number of verticies supplied so far.
         NVERT = NVERT + 1

*  If there is no room for any more vertices in the workspace, double
*  the size of the workspace.
         IF( NVERT .GT. WSIZE ) THEN
            WSIZE = 2*WSIZE
            CALL PSX_REALLOC( WSIZE, IPX, STATUS )
            CALL PSX_REALLOC( WSIZE, IPY, STATUS )
         END IF

*  Store the supplied coordinates in the workspace.
         CALL KPS1_STOR( WSIZE, NVERT, XY( 1 ), %VAL( IPX ), STATUS )
         CALL KPS1_STOR( WSIZE, NVERT, XY( 2 ), %VAL( IPY ), STATUS )

*  Cancel the current value of the parameter.
         CALL PAR_CANCL( PNAME, STATUS )

*  Get the next pair of coordinates using the supplied parameter.
         CALL PAR_EXACR( PNAME, 2, XY, STATUS )

      END DO

*  If a null parameter value has been supplied, annul the error.
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Cancel the current value of the parameter.
      CALL PAR_CANCL( PNAME, STATUS )

*  If any other error has occurred, set the number of vertices in the
*  polygon to zero, and release the workspace. This is done within a
*  new error reporting environment because PSX_FREE does nothing if an
*  error status exists on entry (in contrast to most other "tidying up"
*  routines).
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_BEGIN( STATUS )
         NVERT = 0
         CALL PSX_FREE( IPX, STATUS )
         CALL PSX_FREE( IPY, STATUS )
         CALL ERR_END( STATUS )
      END IF

      END
