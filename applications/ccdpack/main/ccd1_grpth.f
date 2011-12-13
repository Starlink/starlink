      SUBROUTINE CCD1_GRPTH( GRAPH, NEDGE, NODE1, NODE2, WORK, WORK2,
     :                       PATH, NSTEP, STATUS )
*+
*  Name:
*     CCD1_GRPTH

*  Purpose:
*     Find the best path through a graph.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_GRPTH( GRAPH, NEDGE, NODE1, NODE2, WORK, WORK2, PATH,
*                      NSTEP, STATUS )

*  Description:
*     This routine operates on a graph expressed as a set of edges,
*     and finds the best path (subgraph) from one node to another.
*     As currently implemented, "best" means the one with the smallest
*     number of edges, and if there is a tie, the one with the smallest
*     sum of edge weights is chosen, where the weight is an integer
*     associated with each edge, is chosen.

*  Arguments:
*     GRAPH( 4, NEDGE ) = INTEGER (Given)
*        A set of edges, with node numbers at positions (1,*) and (2,*)
*        and weights at positions (3,*).  Note that for the purpose of
*        this routine, a small weight is preferable. The fourth
*        position is unused and present for compatibility with other
*        CCD1_ graph handling routines.  The graph must contain a
*        maximum of one edge between any pair of nodes.
*     NEDGE = INTEGER (Given)
*        The number of edges in GRAPH.
*     NODE1 = INTEGER (Given)
*        The number of the node at which the path must start.
*     NODE2 = INTEGER (Given)
*        The number of the node at which the path must terminate.
*     WORK( 4, * ) = INTEGER (Given and Returned)
*        Workspace.  The second dimension should be at least the number
*        of distinct nodes represented in the graph.
*     WORK2( * ) = INTEGER (Given and Returned)
*        Workspace.  Its dimension should be at least the number of
*        distinct nodes represented in the graph.
*     PATH( 4, * ) = INTEGER (Returned)
*        A set of NSTEP edges from GRAPH for which PATH(1,1) = NODE1,
*        PATH(2,NSTEP) = NODE2, and PATH(2,N-1) = PATH(1,N) for
*        N = 2..NSTEP.  The second dimension should be at least the
*        number of distinct nodes represented in the graph.
*     NSTEP = INTEGER (Returned)
*        The number of edges in PATH.
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
*     14-APR-1999 (MBT):
*        Original version.
*     20-MAR-2001 (MBT):
*        Added a check which prevented it taking far too long for large
*        and highly connected graphs (don't recurse unless current path
*        is shorter than shortest yet).
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants

*  Arguments Given:
      INTEGER NEDGE
      INTEGER GRAPH( 4, NEDGE )
      INTEGER NODE1
      INTEGER NODE2

*  Arguments Given and Returned.
      INTEGER WORK( 4, * )
      INTEGER WORK2( * )

*  Arguments Returned:
      INTEGER PATH( 4, * )
      INTEGER NSTEP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BESTWT             ! Weight of best candidate path so far
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      INTEGER K                  ! Loop variable
      INTEGER NODEC              ! Node currently being searched for
      INTEGER STEP               ! Current level of recursion
      INTEGER WEIGHT             ! Weight of current candidate path
      LOGICAL BEST               ! Best path so far
      LOGICAL CYCLIC             ! Current path contains a cycle
      LOGICAL DONE               ! At least one candidate path found
      LOGICAL FAIL               ! Current path cannot be part of a candidate
      LOGICAL MATCH              ! The current node is of interest
      LOGICAL SUCCES             ! Current path is a candidate
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No success yet.
      DONE = .FALSE.

*  Initialise best weight so far to pessimal value.
      BESTWT = NUM__MAXI

*  Initialise path to failed value.
      NSTEP = 0

*  As a special case, if the initial and terminal nodes are the same,
*  fabricate a one-step path and exit successfully.
      IF ( NODE1 .EQ. NODE2 ) THEN
         DONE = .TRUE.
         NSTEP = 1
         PATH( 1, NSTEP ) = NODE1
         PATH( 2, NSTEP ) = NODE2
         PATH( 3, NSTEP ) = 1
         PATH( 4, NSTEP ) = 0
         GO TO 99
      END IF

*  Recursion level begins at unity.
      STEP = 1

*  To start with we will be looking for an edge containing the starting
*  node, NODE1.
      NODEC = NODE1

*  Begin a recursive loop through all the edges in the graph.
      I = 1
 1    CONTINUE

*  Look for an edge with the current node at one end.
         IF ( GRAPH( 1, I ) .EQ. NODEC ) THEN
            MATCH = .TRUE.
            WORK( 1, STEP ) = GRAPH( 1, I )
            WORK( 2, STEP ) = GRAPH( 2, I )
            WORK( 3, STEP ) = GRAPH( 3, I )
            WORK( 4, STEP ) = GRAPH( 4, I )
         ELSE IF ( GRAPH( 2, I ) .EQ. NODEC ) THEN
            MATCH = .TRUE.
            WORK( 1, STEP ) = GRAPH( 2, I )
            WORK( 2, STEP ) = GRAPH( 1, I )
            WORK( 3, STEP ) = GRAPH( 3, I )
            WORK( 4, STEP ) = GRAPH( 4, I )
         ELSE
            MATCH = .FALSE.
         END IF

*  If we have found a node of interest, check whether it forms a
*  successful path, or whether it is bound to fail.
         IF ( MATCH ) THEN
            SUCCES = .FALSE.
            FAIL = .FALSE.

*  If the destination node is at the other end of this edge, we have
*  found a successful candidate path.
            IF ( WORK( 2, STEP ) .EQ. NODE2 ) THEN
               SUCCES = .TRUE.

*  Work out the weight of this path.
               WEIGHT = 0
               DO 11 J = 1, STEP
                  WEIGHT = WEIGHT + WORK( 3, J )
 11            CONTINUE

*  If this is the best path so far, save it.
               BEST =     ( .NOT. DONE )
     :               .OR. ( STEP .LT. NSTEP )
     :               .OR. ( STEP .EQ. NSTEP .AND. WEIGHT .LT. BESTWT )
               IF ( BEST ) THEN
                  DONE = .TRUE.
                  BESTWT = WEIGHT
                  NSTEP = STEP
                  DO 12 J = 1, STEP
                     DO 13 K = 1, 4
                        PATH( K, J ) = WORK( K, J )
 13                  CONTINUE
 12               CONTINUE
               END IF
            ELSE

*  Check whether we have encountered the node at the other end of this
*  edge so far in this path.
               CYCLIC = .FALSE.
               DO 14 J = 1, STEP - 1
                  CYCLIC = ( WORK( 1, J ) .EQ. WORK( 2, STEP ) )
     :                     .OR. CYCLIC
 14            CONTINUE
               FAIL = CYCLIC
            END IF

*  If investigation of this edge is not conclusive, and the path
*  is not longer than the best candidate yet, recurse further
*  into it.
            IF ( .NOT. ( SUCCES .OR. FAIL ) .AND.
     :         ( .NOT. DONE .OR. STEP .LE. NSTEP ) ) THEN
               NODEC = WORK( 2, STEP )
               WORK2( STEP ) = I
               STEP = STEP + 1
               I = 0
            END IF

         END IF

*  If we have reached the end of the list of edges, drop back a level
*  of recursion.
         IF ( I .GE. NEDGE ) THEN
 3          CONTINUE
            STEP = STEP - 1
            IF ( STEP .EQ. 0 ) THEN
               GO TO 2
            ELSE IF ( STEP .EQ. 1 ) THEN
               NODEC = NODE1
            ELSE
               NODEC = WORK( 2, STEP - 1 )
            END IF
            I = WORK2( STEP )
            IF ( I .GE. NEDGE ) GO TO 3
         END IF

*  Increment edge index and return to start of loop
         I = I + 1
         GO TO 1
 2    CONTINUE

*  Exit label.
 99   CONTINUE

      END
* $Id$
