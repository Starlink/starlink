      SUBROUTINE CCD1_MLSPG( GRAPH, WEIGHT, NEDGES, TOTNOD, QUEUE, BEEN,
     :                       SPAN, SUBGRP, NEWED, NNODE, STATUS )
*+
*  Name:
*     CCD1_MLSPG

*  Purpose:
*     Determines the most likely spanning graph of a graph of weighted
*     edges.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MLSPG( GRAPH, WEIGHT, NEDGES, TOTNOD, QUEUE, BEEN, SPAN,
*                      SUBGRP, NEWED, NNODE, STATUS )

*  Description:
*     This routine determines the most likely spanning graph of a
*     graph of edges. The most likely in this case is the spanning graph
*     which has the highest weight. The return from this routine are
*     the spanning graph represented in the input form (i.e. a graph of
*     edges and weights) the numbers of edges in the graph, the
*     number of nodes and an array of logical values indicating
*     which nodes have been visited (i.e. are part of the extracted
*     sub-graph).

*  Arguments:
*     GRAPH( 4, NEDGES ) = INTEGER (Given and Returned)
*        The edges of the graph. The node numbers are held in elements
*        1 and 2 and the original intercomparison index (unique for each
*        node) in position 4.  This graph should be complete (i.e. fully
*        connected) on entry. On exit the graph will be sorted into
*        decreasing weight order.
*     WEIGHT( * ) = DOUBLE PRECISION (Given and Returned)
*        The weights associated with each edge.  It is indexed by
*        GRAPH( 4, * ).  On exit this will be sorted to match the
*        positions in the graph.
*     NEDGES = INTEGER (Given)
*        The number of edges in the graph.
*     TOTNOD = INTEGER (Given)
*        The number of nodes in the input graph.
*     QUEUE( * ) = INTEGER (Given and Returned)
*        Workspace to hold the queue of nodes still be visited. This
*        should
*     BEEN( * ) = LOGICAL (Returned)
*        Array of flags indicating which nodes have been visited. The
*        index into this array are those of the node numbers, so this
*        array should be large enough to be indexed by the largest node
*        number.
*     SPAN( 4, * ) = INTEGER (Given and Returned)
*        Workspace graph, needs to be size of SUBGRP
*     SUBGRP( 4, * ) = INTEGER (Returned)
*        The maxiumum likelyhood spanning sub-graph.
*     NEWED = INTEGER (Returned)
*        The number of edges in the output sub-graph (second dimension).
*     NNODE = INTEGER (Returned)
*        The number of nodes in the output sub-graph
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - this routine writes CCDPACK logfile information so must
*     be used as part of a CCDPACK application.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     11-DEC-1992 (PDRAPER):
*        Original version.
*     26-JAN-2001 (MBT):
*        Modified to allow use of an external weights array.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NEDGES
      INTEGER TOTNOD

*  Arguments Given and Returned:
      INTEGER GRAPH( 4, NEDGES )
      INTEGER QUEUE( * )
      INTEGER SPAN( 4, * )
      DOUBLE PRECISION WEIGHT( * )

*  Arguments Returned:
      INTEGER BEEN( * )
      INTEGER SUBGRP( 4, * )
      INTEGER NEWED
      INTEGER NNODE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      INTEGER NAT                ! Offset into graph
      INTEGER NSPAN              ! Offset into new graph
      INTEGER START              ! Starting point for graph tests
      INTEGER VAL( 4 )           ! Dummy storage
      DOUBLE PRECISION SUM       ! Sum of weights
      DOUBLE PRECISION WT        ! Weight of current edge
      LOGICAL COMPL              ! Graph is Complete flag
      LOGICAL CYCLIC             ! Graph is cyclic flag

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Sort edges into decreasing weight order.
      DO 1 I = 2, NEDGES
         VAL( 1 ) = GRAPH( 1, I )
         VAL( 2 ) = GRAPH( 2, I )
         VAL( 3 ) = GRAPH( 3, I )
         VAL( 4 ) = GRAPH( 4, I )
         WT = WEIGHT( GRAPH( 4, I ) )
         DO 2 J = I - 1, 1, -1
            IF ( WT .LT. WEIGHT( GRAPH( 4, J ) ) ) GO TO 3
            GRAPH( 1, J + 1 ) = GRAPH( 1, J )
            GRAPH( 2, J + 1 ) = GRAPH( 2, J )
            GRAPH( 3, J + 1 ) = GRAPH( 3, J )
            GRAPH( 4, J + 1 ) = GRAPH( 4, J )
            WEIGHT( GRAPH( 4, J + 1 ) ) = WEIGHT( GRAPH( 4, J ) )
 2       CONTINUE
         J = 0
 3       CONTINUE
         GRAPH( 1, J + 1 ) = VAL( 1 )
         GRAPH( 2, J + 1 ) = VAL( 2 )
         GRAPH( 3, J + 1 ) = VAL( 3 )
         GRAPH( 4, J + 1 ) = VAL( 4 )
         WEIGHT( GRAPH( 4, J + 1 ) ) = WT
 1    CONTINUE

*  Pick first edge as starting point.
      NSPAN = 1
      NAT = 1
      SPAN( 1, 1 ) = GRAPH( 1, 1 )
      SPAN( 2, 1 ) = GRAPH( 2, 1 )
      SPAN( 3, 1 ) = GRAPH( 3, 1 )
      SPAN( 4, 1 ) = GRAPH( 4, 1 )

*  Loop while the number of edges in the subgraph is less than the
*  number of nodes in the graph minus one and the whole graph has not
*  been looked at. The basic algorithm for this part is to choose the
*  edges with highest weight and try to put it into the new sub-graph,
*  this is ok if the new edge does not form a cycle if it does form a
*  cycle then the edge is rejected and the next highest valued one is
*  choosen etc. This always results in a maximum spanning tree.
 4    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( NSPAN .LT. TOTNOD - 1 .AND. NSPAN .LE. NEDGES ) THEN

*  Add new edge.
         NSPAN = NSPAN + 1
         NAT = NAT + 1
         SPAN( 1, NSPAN ) = GRAPH( 1, NAT )
         SPAN( 2, NSPAN ) = GRAPH( 2, NAT )
         SPAN( 3, NSPAN ) = GRAPH( 3, NAT )
         SPAN( 4, NSPAN ) = GRAPH( 4, NAT )

*  Check that this isn't a closed loop. Start the graph search at
*  the edge which has just been entered.
         START = NSPAN
         CALL CCD1_GRAPC( SPAN, NSPAN, START, QUEUE, BEEN, COMPL,
     :                    CYCLIC, SUBGRP, NEWED, NNODE, STATUS )
         IF ( CYCLIC ) THEN

*  Remove this edge.
            NSPAN = NSPAN - 1
         END IF
         GO TO 4
      END IF

*  Found maximum spanning tree, temporary report section.
      CALL CCD1_MSG( ' ',
     :'  Spanning graph connections:', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )
      SUM = 0
      DO 5 I = 1, NSPAN

*  Find the weight associated with this edge by matching unique
*  intercomparison index with the input graph.
         WT = -1D0
         DO 6 J = 1, NEDGES
            IF ( SPAN( 4, I ) .EQ. GRAPH( 4, J ) ) THEN
               WT = WEIGHT( GRAPH( 4, J ) )
               GO TO 7
            END IF
 6       CONTINUE
 7       CONTINUE
         CALL MSG_SETI( 'S1', SPAN( 1, I ) )
         CALL MSG_SETI( 'S2', SPAN( 2, I ) )
         CALL MSG_SETR( 'WT', REAL( WT ) )
         CALL MSG_SETI( 'I', I )
         CALL CCD1_MSG( ' ',
     :'  Lists ^S1) and ^S2) with weight ^WT', STATUS )
         SUM = SUM + WT
 5    CONTINUE
      CALL MSG_SETR( 'SUM', REAL( SUM ) )
      CALL CCD1_MSG( ' ',
     :'    Sum of weights: ^SUM', STATUS )
      END
* $Id$
