      SUBROUTINE CCD1_GROFF( GRAPH, NEDGES, XOFF, YOFF, NCOMP, BEEN ,
     :                       QUEUE, XOFFN, YOFFN, STATUS )
*+
*  Name:
*     CCD1_GROFF

*  Purpose:
*     Determines absolute offsets given a spanning graph

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GROFF( GRAPH, NEDGES, XOFF, YOFF, NCOMP,
*                      BEEN, QUEUE, XOFFN, YOFFN, STATUS )

*  Description:
*     This routine determines the absolute offsets of the nodes of a
*     spanning graph given the relative offsets and a graph of edges.
*     The reference position is the first node in the graph.  The
*     offsets are assumed to be product of a intercomparison process
*     between NCOMP datasets whose original indexes are stored in (4,*)
*     of the graph On output the offsets (XOFFN and YOFFN) are indexed
*     by the node number as they are now absolute not relative (edge
*     lengths).

*  Arguments:
*     GRAPH( 4, NEDGES ) = INTEGER ) (Given)
*        The spanning graph of edges, related to the offsets XOFF and
*        YOFF. This graph is assumed undirected.
*     NEDGES = INTEGER (Given)
*        The number of edges in the spanning graph.
*     XOFF( * ) = DOUBLE PRECISION (Given)
*        X offsets of the nodes whose index is given as described in the
*        Description section.
*     YOFF( * ) = DOUBLE PRECISION (Given)
*        Y offsets of the nodes whose index is given as described in the
*        Description section.
*     NCOMP = INTEGER (Given)
*        Number of nodes whose data were initially intercompared.
*     BEEN( * ) = LOGICAL (Given and Returned)
*        Workspace used to indicate which nodes of the spanning tree
*        have been visited. Must be able to be referenced by the largest
*        node number as an index.
*     QUEUE( * ) = INTEGER (Given and Returned)
*        Workspace used to queue nodes yet to be visited on the
*        spanning tree. Needs to be larger than the number of nodes.
*     XOFFN( * ) = DOUBLE PRECISION (Returned)
*        X offsets of nodes to reference node.
*     YOFFN( * ) = DOUBLE PRECISION (Returned)
*        Y offsets of nodes to reference node.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - this routine writes information using the CCDPACK logfile
*     system, so this must be opened first.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1992 (PDRAPER):
*        Original version.
*     17-MAR-1995 (PDRAPER):
*        Removed unused argument NNODE.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'MSG_PAR'          ! Message system constants

*  Arguments Given:
      INTEGER NEDGES
      INTEGER GRAPH( 4, NEDGES )
      DOUBLE PRECISION XOFF( * )
      DOUBLE PRECISION YOFF( * )
      INTEGER NCOMP

*  Arguments Given and Returned:
      LOGICAL BEEN( * )
      INTEGER QUEUE( * )

*  Arguments Returned:
      DOUBLE PRECISION XOFFN( * )
      DOUBLE PRECISION YOFFN( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( MSG__SZMSG ) BUFFER ! Output message string
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Position in string
      INTEGER J                  ! Loop variable
      INTEGER QCNT               ! Number on queue
      INTEGER NODE               ! Current node number
      INTEGER XS                 ! X-offset position in o/p string
      INTEGER YS                 ! Y-offset position in o/p string
      LOGICAL ADD                ! Add node to queue ?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the output offsets and visited node flags.
      DO 1 I = 1, NCOMP
         XOFFN( I ) = VAL__BADD
         YOFFN( I ) = VAL__BADD
         BEEN( I ) = .FALSE.
 1    CONTINUE

*  Put first node on the queue of nodes to visit.
      QCNT = 1
      QUEUE( 1 ) = GRAPH( 1, 1 )

*  First node in edges list is the starting (reference) node.
      NODE = GRAPH( 1, 1 )

*  Its offsets are zero.
      XOFFN( NODE ) = 0.0D0
      YOFFN( NODE ) = 0.0D0

*  Loop while there are still nodes to visit on the queue.
 2    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( QCNT .GT. 0 ) THEN

*  Current node is the next on the queue.
         NODE = QUEUE( 1 )

*  Flag this node as visited. We can reference offsets to this.
         BEEN( NODE ) = .TRUE.

*  Re-shuffle the queue to remove the first entry.
         QCNT = QCNT - 1
         DO 3 I = 1, QCNT
            QUEUE( I ) = QUEUE( I + 1 )
 3       CONTINUE

*  Look at all the edges of the graph locating all the occurences of
*  this node. If any edges are located put the end nodes which have not
*  already been visited on the queue and visit them sometime in the
*  future. 
         DO 4 I = 1, NEDGES
            IF ( GRAPH( 1, I ) .EQ. NODE ) THEN
               IF ( .NOT. BEEN( GRAPH( 2, I ) ) ) THEN 

*  See if this node is already on the queue.
                  ADD = .TRUE.
                  DO 5 J = 1, QCNT
                     IF ( QUEUE( J ) .EQ. GRAPH( 2, I ) ) THEN
                        ADD = .FALSE.
                        GO TO 6
                     END IF
 5                CONTINUE
 6                CONTINUE    

*  Add to queue if not present already
                  IF ( ADD ) THEN             
                     QCNT = QCNT + 1
                     QUEUE( QCNT ) = GRAPH( 2, I )
                  END IF

*  Determine the offset to this noed by adding its relative offset to
*  the offset of the visited node.
                  XOFFN( GRAPH( 2, I ) ) = XOFFN( NODE ) +
     :                                     XOFF( GRAPH( 4, I ) )
                  YOFFN( GRAPH( 2, I ) ) = YOFFN( NODE ) +
     :                                     YOFF( GRAPH( 4, I ) )
               END IF
            ELSE IF ( GRAPH( 2, I ) .EQ. NODE ) THEN
               IF ( .NOT. BEEN( GRAPH( 1, I ) ) ) THEN 

*  See if this node is already on the queue.
                  ADD = .TRUE.
                  DO 7 J = 1, QCNT
                     IF ( QUEUE( J ) .EQ. GRAPH( 1, I ) ) THEN
                        ADD = .FALSE.
                        GO TO 8
                     END IF
 7                CONTINUE
 8                CONTINUE    

*  Add to queue if not present already
                  IF ( ADD ) THEN             
                     QCNT = QCNT + 1
                     QUEUE( QCNT ) = GRAPH( 1, I )
                  END IF

*  Direction of this edge is negative so subtract the offset from the
*  offset of the visited node.
                  XOFFN( GRAPH( 1, I ) ) = XOFFN( NODE ) -
     :                                     XOFF( GRAPH( 4, I ) )
                  YOFFN( GRAPH( 1, I ) ) = YOFFN( NODE ) -
     :                                     YOFF( GRAPH( 4, I ) )
               END IF
            END IF
 4       CONTINUE

*  Next while.
         GO TO 2
      END IF     

*  Write out the offsets to the log file.
*  First construct a header string.
      BUFFER = '  List'
      IAT = VAL__SZI + 4
      BUFFER( IAT: ) = '     X-offset '
      XS = IAT + 2
      IAT = IAT + VAL__SZD + 4
      BUFFER( IAT: ) = '     Y-offset '
      YS = IAT + 2
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', BUFFER, STATUS )

*  Now write out the offsets for those nodes along spanning graph.
       DO I = 1, NCOMP
         IF ( BEEN( I ) )  THEN
            BUFFER = ' '
            CALL CHR_ITOC( I, BUFFER( 5: ), IAT )
            IAT = 5 + IAT
            BUFFER( IAT : IAT ) = ')'
            CALL CHR_DTOC( XOFFN( I ), BUFFER( XS: ), IAT )
            CALL CHR_DTOC( YOFFN( I ), BUFFER( YS: ), IAT )
            CALL CCD1_MSG( ' ', BUFFER, STATUS )
         END IF
      END DO 

      END
* $Id$
