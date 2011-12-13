      SUBROUTINE CCD1_GMID( GRAPH, NEDGES, HINODE, IP, IL, X1, Y1, X2,
     :                      Y2, N, TOLS, OFFS, ID1, ID2, STATUS )
*+
*  Name:
*     CCD1_GMID

*  Purpose:
*     Generates the identifiers for graph of position lists.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GMID( GRAPH, NEDGES, HINODE, IP, IL, X1, Y1,
*                     X2, Y2, N, TOLS, OFFS, ID1, ID2, STATUS )

*  Description:
*     This routine generates identifiers for positions within lists
*     associated with a (probably) spanning graph. The graph should
*     have pairs of X and Y positions (X1,Y1 and X2,Y2) associated with
*     each edge which are identified as the same, the job of this
*     routine is then to generate integer identifiers for every
*     position pair, making sure that positions at each node which are
*     the same have the same identifier. This task is performed by
*     setting ID's for all positions and then identifying multiple
*     assignments of the same position for each node and removing them.
*     This process is repeated as long as ids are being reassigned.
*     This allows the graph identifiers to relax to all positions which
*     are really identical everywhere.

*  Arguments:
*     GRAPH( 4, NEDGES ) = INTEGER ) (Given)
*        The spanning graph of edges which are associated with the
*        positions X1(*), Y1(*) and X2(*), Y2(*).
*        The indices of an edges positions list are (4,*) in this array.
*     NEDGES = INTEGER (Given)
*        The number of edges in the spanning graph.
*     HINODE = INTEGER (Given)
*        The index of the highest-indexed node which may be in the
*        input graph.  The lowest is assumed to be 1.
*     IP( N ) = INTEGER (Given and Returned)
*        Workspace
*     IL( N ) = INTEGER (Given and Returned)
*        Workspace
*     X1( N ) = DOUBLE PRECISION (Given)
*        The X1 position list merged into one. The offsets into this
*        list are OFFS.
*     Y1( N ) = DOUBLE PRECISION (Given)
*        The Y1 position list merged into one. The offsets into this
*        list are OFFS.
*     X2( N ) = DOUBLE PRECISION (Given)
*        The X2 position list merged into one. The offsets into this
*        list are OFFS.
*     Y2( N ) = DOUBLE PRECISION (Given)
*        The X1 position list merged into one. The offsets into this
*        list are OFFS.
*     N = INTEGER (Given)
*        The total number of entries in input merged lists.
*     TOLS( * ) = DOUBLE PRECISION (Given)
*        Tolerances for deduplicating centroided points indexed by node.
*        If two points in an image points are within this distance of
*        each other in both X and Y directions they should be considered
*        to refer to the same feature.  This might normally be a value
*        equivalent to about a pixel, but may be larger to accomodate
*        objects which are multi-peaked (for instance if they are only
*        faintly apparent above the background noise).  If equivalent
*        points are known to have the same value every time the
*        elements may be set to zero.
*     OFFS( NEDGES + 1 ) = INTEGER (Given)
*        The offsets of the start of the input lists which are
*        associated with an edge. Thus range (OFFS(1), OFFS(2) -1 )
*        of the arrays X1,Y1 and X2,Y2 contains the first edges position
*        lists etc.
*     ID1( N ) = INTEGER (Returned)
*        The identifiers of the corresponding positions X1 and Y1.
*     ID1( N ) = INTEGER (Returned)
*        The identifiers of the corresponding positions X2 and Y2.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The identifiers ID1 and ID2 should be equal on exit.
*     -  Multiple (identical) identifications of the same positions
*     will exist on exit, these should be removed.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
*     Copyright (C) 2001-2002 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     14-DEC-1992 (PDRAPER):
*        Original version.
*     7-MAR-1993 (PDRAPER):
*        Complete rewrite.
*     11-JUL-2001 (MBT):
*        Added TOLS argument.
*     11-FEB-2002 (MBT):
*        Now receives rather than calculating the range of referenced nodes.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NEDGES
      INTEGER GRAPH( 4, NEDGES )
      INTEGER HINODE
      INTEGER N
      DOUBLE PRECISION X1( N )
      DOUBLE PRECISION Y1( N )
      DOUBLE PRECISION X2( N )
      DOUBLE PRECISION Y2( N )
      DOUBLE PRECISION TOLS( * )
      INTEGER OFFS( NEDGES + 1 )

*  Arguments Given and Returned:
      INTEGER IP( N )
      INTEGER IL( N )

*  Arguments Returned:
      INTEGER ID1( N )
      INTEGER ID2( N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXIT              ! Maximum number of iterations
      PARAMETER ( MAXIT = 10000) ! used to stop cyclic runaways

*  Local Variables:
      DOUBLE PRECISION XN        ! Current X position
      DOUBLE PRECISION YN        ! Current X position
      INTEGER CHANGE             ! Numbers of identifiers changed this time
      INTEGER EDGE               ! Current edge
      INTEGER I                  ! Loop variable
      INTEGER ID                 ! Identifier count
      INTEGER IDN                ! Current identifier
      INTEGER J                  ! Loop variable
      INTEGER NIT                ! Number of iterations
      INTEGER NODE               ! Current node
      INTEGER NPOS               ! Number of positions associated with a node

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the id counter.
      ID = 0

*  Visit each edge and assign identfiers.
      DO 1 EDGE = 1, NEDGES
         DO 2 I = OFFS( EDGE ), OFFS( EDGE + 1 ) - 1
            ID = ID + 1
            ID1( I ) = ID
 2       CONTINUE
 1    CONTINUE

*  Look at the nodes in turn. While IDs change using the method of
*  identifying positions with the same positions and different IDs
*  keep iterating. Convergence should ensure a reasonable number of
*  common positions are identified.
      NIT = 0
 100  CONTINUE

*  Initialise the identifier aliases.
      DO 3 I = 1, N
         ID2( I ) = 0
 3    CONTINUE

*  For each node...
      DO 5 NODE = 1, HINODE

*  Identify all the edges which have this node.
         NPOS = 0
         DO 6 EDGE = 1, NEDGES
            IF ( GRAPH( 1, EDGE ) .EQ. NODE ) THEN

*  Extend the list of pointers for positions associated with this node.
               DO 7 I = OFFS( EDGE ), OFFS( EDGE + 1 ) - 1
                  NPOS = NPOS + 1
                  IP( NPOS ) = I

*  And record which position list to look in.
                  IL( NPOS ) = 1
 7             CONTINUE
            ELSE IF ( GRAPH( 2, EDGE ) .EQ. NODE ) THEN

*  Extend the list of pointers for positions associated with this node.
               DO 8 I = OFFS( EDGE ), OFFS( EDGE + 1 ) - 1
                  NPOS = NPOS + 1
                  IP( NPOS ) = I

*  And record which position list to look in.
                  IL( NPOS ) = 2
 8             CONTINUE
            END IF
 6       CONTINUE

*  For each position associated with this node look for multiple
*  occurrences.
         DO 9 I = 1, NPOS - 1

*  Extract the values of this position.
            IF ( IL( I ) .EQ. 1 ) THEN
               XN = X1( IP( I ) )
               YN = Y1( IP( I ) )
            ELSE
               XN = X2( IP( I ) )
               YN = Y2( IP( I ) )
            END IF
            IDN = ID1( IP( I ) )

*  Loop over all the other positions.
            DO 10 J = I + 1, NPOS

*  Only look at those with no current alias.
                IF ( ID2( IP( J ) ) .EQ. 0 ) THEN

*  If this the same same position?
                  IF ( IL( J ) .EQ. 1 ) THEN             ! Use X1 and Y1
                     IF ( ABS( XN - X1( IP( J ) ) )
     :                    .LE. TOLS( NODE ) ) THEN
                        IF ( ABS( YN - Y1( IP( J ) ) )
     :                       .LE. TOLS( NODE ) ) THEN

*  Same position set alias to reference ID. If the identifier not the
*  same. If the identifier is the same then that's fine.
                           IF ( ID1( IP( J ) ) .NE. IDN ) THEN
                              ID2( IP( J ) ) = IDN
                           END IF
                        END IF
                     END IF
                  ELSE                                   ! Use X2 and Y2
                     IF ( ABS( XN - X2( IP( J ) ) )
     :                    .LE. TOLS( NODE ) ) THEN
                        IF ( ABS( YN - Y2( IP( J ) ) )
     :                       .LE. TOLS( NODE ) ) THEN

*  Same position set alias to reference ID. If the identifier not the
*  same.
                           IF ( ID1( IP( J ) ) .NE. IDN ) THEN
                              ID2( IP( J ) ) = IDN
                           END IF
                        END IF
                     END IF
                  END IF
               END IF
 10          CONTINUE
 9       CONTINUE
 5    CONTINUE

*  Finally set all aliased positions.
      CHANGE = 0
      DO 11 I = 1, N
         IF ( ID2( I ) .NE. 0 ) THEN
            CHANGE = CHANGE + 1
            ID1( I ) = ID2( I )
         END IF
 11   CONTINUE

*  See if ids are still being changed.
      IF ( CHANGE .NE. 0 .AND. NIT .LT. MAXIT ) THEN
         NIT = NIT + 1
         GO TO 100
      END IF

*  Iteration finished.

      END
* $Id$
