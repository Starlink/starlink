      SUBROUTINE CCD1_GRAPC( GRAPH, NEDGES, START, QUEUE, BEEN, COMPL,
     :                       CYCLIC, SUBGRP, NEWED, NNODE, STATUS )
*+
*  Name:
*     CCD1_GRAPC

*  Purpose:
*     Tests the properties of a graph, represented as a series of
*     edges.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GRAPC( GRAPH, NEDGES, START, BEEN, QUEUE, COMPL,
*                      CYCLIC, SUBGRP, NEWED, NNODE, STATUS )

*  Description:
*     This routine performs a search of a graph which is represented by
*     its edges. The search of the graph is performed starting at an
*     edge which selected by the caller. The returns are BEEN which
*     represents which nodes the search visited (positions within BEEN
*     are selected by the node numbers in GRAPH, so BEEN should be large
*     enough to contain the largest node number as an offset). COMPL
*     which is true if all the nodes are visited during the search, the
*     graph is then connected and complete. If the number of edges of
*     the graph (or sub-graph if not complete) exceed the number of
*     edges used for the SPAN then the graph is cyclic and CYCLIC is
*     returned true.

*  Arguments:
*     GRAPH( 4, NEDGES ) = INTEGER (Given)
*        The edges of the graph. The node numbers are held in elements
*        1 and 2.
*     NEDGES = INTEGER (Given)
*        The number of edges in the graph.
*     START = INTEGER (Given)
*        The offset into GRAPH which is the edge to start the span at.
*     QUEUE( * ) = INTEGER (Given and Returned)
*        Workspace to hold the queue of nodes still be visited.
*     BEEN( * ) = LOGICAL (Returned)
*        Array of flags indicating which nodes have been visited. The
*        index into this array are those of the node numbers, so this
*        array should be large enough to be indexed by the largest node
*        number.
*     COMPL = LOGICAL (Returned)
*        If all the nodes of the input graph are visited then the graph
*        is complete and this flag is set true. Otherwise this flag is
*        set false.
*     CYCLIC = LOGICAL (Returned)
*        If the number of edges connected to the nodes which are part of
*        the graph which is spanned exceeds the number of the span
*        itself then part of the graph must be cyclic. If this is so
*        then this flag is set true. Otherwise it is set false.
*     SUBGRP( 4, * ) = INTEGER (Returned)
*        The edge information of the spanning sub-graph which is
*        produced from the search starting at edge START.
*     NEWED = INTEGER (Returned)
*        The number of edges in the output sub-graph (second dimension).
*     NNODE = INTEGER (Returned)
*        The number of nodes in the output sub-graph
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-DEC-1992 (PDRAPER):
*        Original version.
*     12-SEP-2000 (MBT):
*        Added check that NEDGES is not zero.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      INTEGER NEDGES
      INTEGER GRAPH( 4, NEDGES )
      INTEGER START

*  Arguments Given and Returned:
      INTEGER QUEUE( * )

*  Arguments Returned:
      LOGICAL BEEN( * )
      LOGICAL COMPL
      LOGICAL CYCLIC
      INTEGER SUBGRP( 4, * )
      INTEGER NEWED
      INTEGER NNODE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER QCNT
      INTEGER NOW
      INTEGER I, J
      INTEGER FNODE
      INTEGER LNODE
      LOGICAL ADD

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that we have at least one edge.
      IF ( NEDGES .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'CCD1_GRAPC: Graph has no edges', STATUS )
         GO TO 99
      END IF

*  Find the span of node numbers.
      FNODE = VAL__MAXI
      LNODE = VAL__MINI
      DO 1 I = 1, NEDGES
         IF ( GRAPH( 1, I ) .GT. LNODE ) LNODE = GRAPH( 1, I )
         IF ( GRAPH( 2, I ) .GT. LNODE ) LNODE = GRAPH( 2, I )
         IF ( GRAPH( 1, I ) .LT. FNODE ) FNODE = GRAPH( 1, I )
         IF ( GRAPH( 2, I ) .LT. FNODE ) FNODE = GRAPH( 2, I )
 1    CONTINUE

*  Number of EDGES known. Set flags for visited node.
      DO 2 I = FNODE, LNODE
         BEEN( I ) = .FALSE.
 2    CONTINUE

*  Span the graph. Locate the starting node position first.
      NOW = GRAPH( 1, START )
      QUEUE( 1 ) = NOW
      QCNT = 1

*  Set the edge count for the spanning (sub)graph.
      NEWED = 0

*  Loop while there are still nodes to visit on the queue.
 3    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( QCNT .GT. 0  ) THEN

*  Pick the next node on queue.
         NOW = QUEUE( 1 )
C         write(*,*)'Visting node:',NOW

*  Flag this node as visited and remove it from the queue.
         BEEN( NOW ) = .TRUE.

*  Re-shuffle the queue to remove the first entry.
         QCNT = QCNT - 1
         DO 4 I = 1, QCNT
            QUEUE( I ) = QUEUE( I + 1 )
 4       CONTINUE

*  Look for neighbours of node we're currently visiting. Neighbours
*  are nodes which share an edge.
C         write(*,*)'Neighbours:'
         DO 5 I = 1, NEDGES
            IF ( GRAPH( 1, I ) .EQ. NOW ) THEN

*  Node neighbour. If it has not been visited, put on the queue.
C               write(*,*)graph(2,i)
               IF ( .NOT. BEEN( GRAPH( 2, I ) ) ) THEN

*  See if this node is already on the queue.
                  ADD = .TRUE.
                  DO 9 J = 1, QCNT
                     IF ( QUEUE( J ) .EQ. GRAPH( 2, I ) ) THEN
                        ADD = .FALSE.
                        GO TO 10
                     END IF
 9                CONTINUE
 10               CONTINUE

*  Add to queue if not present already
                  IF ( ADD ) THEN
                     QCNT = QCNT + 1
                     QUEUE( QCNT ) = GRAPH( 2, I )
                  END IF

*  Record the edge this node is associated with -- other node has not
*  been visited.
                  NEWED = NEWED + 1
                  SUBGRP( 1, NEWED ) = NOW
                  SUBGRP( 2, NEWED ) = GRAPH( 2, I )
                  SUBGRP( 3, NEWED ) = GRAPH( 3, I )
                  SUBGRP( 4, NEWED ) = GRAPH( 4, I )
               END IF
            ELSE IF ( GRAPH( 2, I ) .EQ. NOW ) THEN

*  Neighbour.
C               write(*,*)graph(1,i)
               IF ( .NOT. BEEN( GRAPH( 1, I ) ) ) THEN

*  See if this node is already on the queue.
                  ADD = .TRUE.
                  DO 11 J = 1, QCNT
                     IF ( QUEUE( J ) .EQ. GRAPH( 1, I ) ) THEN
                        ADD = .FALSE.
                        GO TO 12
                     END IF
 11               CONTINUE
 12               CONTINUE

*  Add to queue if not present already
                  IF ( ADD ) THEN
                     QCNT = QCNT + 1
                     QUEUE( QCNT ) = GRAPH( 1, I )
                  END IF

*  Record the edge this node is associated with -- other node has not
*  been visited.
                  NEWED = NEWED + 1
                  SUBGRP( 1, NEWED ) = GRAPH( 1, I )
                  SUBGRP( 2, NEWED ) = NOW
                  SUBGRP( 3, NEWED ) = GRAPH( 3, I )
                  SUBGRP( 4, NEWED ) = GRAPH( 4, I )
               END IF
            END IF
 5       CONTINUE

*  Next while.
         GO TO 3
      END IF

*  Write out which nodes have been visited.
C      write(*,*)( been( i ), i = fnode, lnode )

*  Confirm that graph is complete. This is true if all node numbers are
*  flagged as been visited.
      QCNT = 0
      DO 6 I = 1, NEDGES
         IF ( BEEN( GRAPH( 1, I ) ) .AND. BEEN( GRAPH( 2, I ) ) ) THEN
            COMPL = .TRUE.
         ELSE
            COMPL = .FALSE.
            GO TO 7
         END IF
 6    CONTINUE
 7    CONTINUE

*  Count the number of nodes in the sub-graph. If the graph is cyclic
*  then the number of edges should equal or exceed this value.
      NNODE = 0
      DO 8 I = FNODE, LNODE
         IF ( BEEN( I ) ) NNODE = NNODE + 1
 8    CONTINUE

*  Compare the number of valid edges with the number in sub-graph.
      IF ( NEWED .GT. NNODE - 1 ) THEN
         CYCLIC = .TRUE.
      ELSE
         CYCLIC = .FALSE.
      END IF

 99   CONTINUE
      END
* $Id$
