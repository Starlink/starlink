      SUBROUTINE CCD1_GRREP( GRAPH, NEDGE, NNODE, REF, SGRAPH, WORK,
     :                       BEEN, EVERBN, COMPL, STATUS )
*+
*  Name:
*     CCD1_GRREP

*  Purpose:
*     Report on completeness of a graph.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_GGREP( GRAPH, NEDGE, NNODE, REF, SGRAPH, WORK, BEEN,
*                      EVERBN, COMPL, STATUS )

*  Description:
*     This routine assesses the completeness of a graph, returning a
*     logical value if all nodes can be visited starting from any other.
*     If it is complete, no further action is taken.  If it is not
*     complete however, more detailed information about the connected
*     subgraphs which comprise it is output to the user via the CCDPACK
*     messaging system.

*  Arguments:
*     GRAPH( 4, NEDGE ) = INTEGER (Given)
*        The edges of the graph.  The node numbers are held in elements
*        1 and 2.
*     NEDGE = INTEGER (Given)
*        The number of edges in the graph.
*     NNODE = INTEGER (Given)
*        The number of nodes in the graph.  This is the largest node
*        index which appears in any edge, not the number of distinct
*        nodes which appear in all the edges, which may be less than
*        this value if some nodes are not linked at all.
*     REF = INTEGER (Given)
*        The index of the reference node.  The value of this will not
*        influence the completeness of the graph, but the subgraph of
*        which this node forms part may be given additional prominence
*        in the report to the user.
*     SGRAPH( 4, NEDGE ) = INTEGER (Given and Returned)
*        Workspace.  Used for storing subgraphs.
*     WORK( NNODE ) = INTEGER (Given and Returned)
*        Workspace.
*     BEEN( NNODE ) = LOGICAL (Given and Returned)
*        Workspace.  Array of flags indicating which nodes have been
*        visited per subgraph.
*     EVERBN( NNODE ) = LOGICAL (Given and Returned)
*        Workspace.  Array of flags indicating which nodes have been
*        visited to date.
*     COMPL = LOGICAL (Returned)
*        Whether the graph is complete or not.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-APR-1999 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK internal constants

*  Arguments Given:
      INTEGER NEDGE
      INTEGER NNODE
      INTEGER GRAPH( 4, NEDGE )
      INTEGER REF

*  Arguments Given and Returned:
      INTEGER SGRAPH( 4, NEDGE )
      INTEGER WORK( NNODE )
      LOGICAL BEEN( NNODE )
      LOGICAL EVERBN( NNODE )

*  Arguments Returned:
      LOGICAL COMPL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CCD1__BLEN ) BUFFER ! Output buffer
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Position in buffer
      INTEGER IED                ! Index of edge to root subgraph at
      INTEGER IGR                ! Index of current subgraph
      INTEGER NEXT               ! Next node to root subgraph at
      INTEGER SNEDGE             ! Number of edges in the subgraph
      INTEGER SNNODE             ! Number of nodes in the subgraph
      LOGICAL CYCLIC             ! Is subgraph cyclic
      LOGICAL DONE               ! Have all nodes been visited
      LOGICAL PCOMPL             ! Is graph complete w.r.t all represented nodes

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate REF.  Supplying it outside these bounds would really
*  constitute a programming error.
      REF = MIN( MAX( 1, REF ), NNODE )

*  Initialise array representing nodes visited to date.
      DO 1 I = 1, NNODE
         EVERBN( I ) = .FALSE.
         BEEN( I ) = .FALSE.
 1    CONTINUE

*  The first node to look at is the reference node.
      NEXT = REF

*  No subgraphs have been considered yet.
      IGR = 0

*  This is the start of the subgraph discovery loop.
 10   CONTINUE

*  Identify an edge which contains the next node.
      IED = 0
      DO 2 I = 1, NEDGE
         IF ( GRAPH( 1, I ) .EQ. NEXT .OR. GRAPH( 2, I ) .EQ. NEXT )
     :      THEN
            IED = I
            GO TO 3
         END IF
 2    CONTINUE
 3    CONTINUE

*  Get a subgraph starting at the given edge.  Treat the case in which no
*  edge contains the next node as a special case.  The only return value
*  we are interested in is BEEN.
      IF ( IED .GT. 0 ) THEN
         CALL CCD1_GRAPC( GRAPH, NEDGE, IED, WORK, BEEN, PCOMPL, CYCLIC,
     :                    SGRAPH, SNEDGE, SNNODE, STATUS )
      ELSE
         DO 4 I = 1, NNODE
            BEEN( I ) = I .EQ. NEXT
 4       CONTINUE
      END IF

*  Loop to determine which nodes have been visited to date and which
*  is next to start at.
      DONE = .TRUE.
      NEXT = 0
      DO 5 I = 1, NNODE

*  Update the list of visited nodes.
         EVERBN( I ) = EVERBN( I ) .OR. BEEN( I )

*  Keep a record of whether all nodes have been visited.
         DONE = DONE .AND. EVERBN( I )

*  Set the next unvisited node.
         IF ( NEXT .EQ. 0 .AND. .NOT. EVERBN( I ) ) NEXT = I
 5    CONTINUE

*  At the first iteration, if all nodes have been visited then the
*  graph is complete and we can exit right away.  Otherwise tell the
*  user that the graph is not complete.
      IF ( IGR .EQ. 0 ) THEN
         COMPL = DONE
         IF ( COMPL ) THEN
            CALL CCD1_MSG( ' ', '  The graph is fully connected.',
     :                     STATUS )
            CALL CCD1_MSG( ' ' , ' ', STATUS )
            GO TO 99
         ELSE
            CALL CCD1_MSG( ' ', '  The graph is not fully connected.',
     :                     STATUS )
            CALL CCD1_MSG( ' ', '  Identifying connected subgraphs, '//
     :                     'starting with the reference node.', STATUS )
            CALL CCD1_MSG( ' ' , ' ', STATUS )
         END IF
      END IF

*  Increment the number of subgraphs considered.
      IGR = IGR + 1

*  Output the subgraph.
      CALL MSG_SETI( 'IGR', IGR )
      CALL CCD1_MSG( ' ', '  Subgraph ^IGR contains nodes:', STATUS )
      BUFFER = ' '
      IAT = 4
      DO 6 I = 1, NNODE
         IF ( BEEN( I ) ) THEN
            IAT = IAT + 4
            IF ( IAT .GT. 75 ) THEN
               CALL CCD1_MSG( ' ', BUFFER, STATUS )
               BUFFER = ' '
               IAT = 4
            END IF
            WRITE( BUFFER( IAT: ), '(I4)' ) I
         END IF
 6    CONTINUE
      CALL CCD1_MSG( ' ', BUFFER, STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  If there are still unvisited nodes, go find the next subgraph.
      IF ( .NOT. DONE ) GO TO 10

*  Exit label.
 99   CONTINUE

      END
* $Id$
