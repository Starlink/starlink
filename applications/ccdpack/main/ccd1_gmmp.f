      SUBROUTINE CCD1_GMMP( GRAPH, NEDGES, NNODE, IPX1, IPY1, IPX2,
     :                      IPY2, NIN, OFFS, IPX, IPY, IPIDS, NOUT,
     :                      STATUS )
*+
*  Name:
*     CCD1_GMMP

*  Purpose:
*     Matches and merges position lists associated with a graph of
*     offsets.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GMMP( GRAPH, NEDGES, NNODE, IPX1, IPY1, IPX2, IPY2,
*                     NIN, OFFS, IPX, IPY, IPIDS, NOUT, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     GRAPH( 4, NEDGES ) = INTEGER ) (Given)
*        The spanning graph of edges which are associated with the
*        positions pointed to by IPX1(*), IPY1(*) and IPX2(*), IPY2(*).
*        The indices of an edges positions list are (4,*) in this array.
*     NEDGES = INTEGER (Given)
*        The number of edges in the spanning graph.
*     NNODE = INTEGER (Given)
*        The number of nodes in the input graph (NEDGES+1 for a spanning
*        graph).
*     IPX1( * ) = INTEGER (Given)
*        Pointer to X positions related to edge. These are indexed
*        by IPX1( GRAPH( 4, * ) ).
*     IPY1( * ) = INTEGER (Given)
*        Pointer to Y positions related to edge. These are indexed
*        by IPY1( GRAPH( 4, * ) ).
*     IPX2( * ) = INTEGER (Given)
*        Pointer to X positions related to edge. These are indexed
*        by IPX2( GRAPH( 4, * ) ).
*     IPY2( * ) = INTEGER (Given)
*        Pointer to X positions related to edge. These are indexed
*        by IPY2( GRAPH( 4, * ) ).
*     NIN( * ) = INTEGER (Given)
*        Number of X and Y positions in IPX1, IPY1, IPX2, IPY2.
*     OFFS( NEDGES + 1 ) = INTEGER (Given and Returned)
*        Workspace for offsets into vectorised lists.
*     IPX( * ) = INTEGER (Returned)
*        Pointers to all X positions matched at this node. The pointers
*        are generated in this routine. The index to this array and IPY,
*        IPIDS and NOUT are the node numbers.
*     IPY( * ) = INTEGER (Returned)
*        Pointers to all Y positions matched at this node. The pointers
*        are generated in this routine. The index to this array and IPX,
*        IPIDS and NOUT are the node numbers.
*     IPIDS( * ) = INTEGER (Returned)
*        Pointers to the identifiers of the positions for each node. The
*        pointers are generated within this routine. The index to this
*        array and IPX, IPY and NOUT are the node numbers.
*     NOUT( * ) = INTEGER (Returned)
*        The numbers of positions matched at each node. The index to
*        this array and IPX, IPY and IPIDS are the node numbers.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1992 (PDRAPER):
*        Original version.
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
      INTEGER NNODE
      INTEGER IPX1( * )
      INTEGER IPY1( * )
      INTEGER IPX2( * )
      INTEGER IPY2( * )
      INTEGER NIN( * )

*  Arguments Given and Returned:
      INTEGER OFFS( NEDGES + 1 )

*  Arguments Returned:
      INTEGER IPX( * )
      INTEGER IPY( * )
      INTEGER IPIDS( * )
      INTEGER NOUT( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FNODE              ! First node index
      INTEGER I                  ! Loop variable
      INTEGER IIN                ! Offset into lists
      INTEGER IPAX1              ! Pointer to workspace
      INTEGER IPAX2              ! Pointer to workspace
      INTEGER IPAY1              ! Pointer to workspace
      INTEGER IPAY2              ! Pointer to workspace
      INTEGER IPI1               ! Pointer to workspace
      INTEGER IPI2               ! Pointer to workspace
      INTEGER IP                 ! Pointer to workspace
      INTEGER IL                 ! Pointer to workspace
      INTEGER J                  ! Loop variable
      INTEGER LNODE              ! Last node index
      INTEGER NEED               ! Total number of positions
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Need to reorganise the input data so that it is all addressable
*  at once. The method used (which is probably the only way in f77)
*  is to vectorise all the input lists. Addressing is then performed
*  by offsetting.
      NEED = 0
      DO 1 I = 1, NEDGES
         NEED = NEED + GRAPH( 3, I )
 1    CONTINUE

*  Allocate the required amount of memory to hold information.
      CALL CCD1_MALL( NEED, '_DOUBLE', IPAX1, STATUS )
      CALL CCD1_MALL( NEED, '_DOUBLE', IPAY1, STATUS )
      CALL CCD1_MALL( NEED, '_DOUBLE', IPAX2, STATUS )
      CALL CCD1_MALL( NEED, '_DOUBLE', IPAY2, STATUS )
      CALL CCD1_MALL( NEED, '_INTEGER', IPI1, STATUS )
      CALL CCD1_MALL( NEED, '_INTEGER', IPI2, STATUS )
      CALL CCD1_MALL( NEED, '_INTEGER', IP, STATUS )
      CALL CCD1_MALL( NEED, '_INTEGER', IL, STATUS )

*  Transfer the data.
      IIN = 1
      DO 2 I = 1, NEDGES
         J = GRAPH( 4, I )
         CALL CCG1_LAPND( %VAL( IPX1( J ) ), NIN( J ), IIN,
     :                    %VAL( IPAX1 ), STATUS )
         CALL CCG1_LAPND( %VAL( IPY1( J ) ), NIN( J ), IIN,
     :                    %VAL( IPAY1 ), STATUS )
         CALL CCG1_LAPND( %VAL( IPX2( J ) ), NIN( J ), IIN,
     :                    %VAL( IPAX2 ), STATUS )
         CALL CCG1_LAPND( %VAL( IPY2( J ) ), NIN( J ), IIN,
     :                    %VAL( IPAY2 ), STATUS )
         OFFS( I ) = IIN
         IIN = IIN + NIN( J ) 
 2    CONTINUE

*  Add final bound for offsets.
      OFFS( NEDGES + 1 ) = IIN
      IIN = IIN - 1

*  Now do the work of generating the matched identifiers.
      CALL CCD1_GMID( GRAPH, NEDGES, NNODE, %VAL( IP ), %VAL( IL ),
     :                %VAL( IPAX1 ), %VAL( IPAY1 ), %VAL( IPAX2 ),
     :                %VAL( IPAY2 ), IIN, OFFS, %VAL( IPI1 ),
     :                %VAL( IPI2 ), STATUS )

*  Get workspace for the output lists.
*  First find the span of node numbers.
      FNODE = VAL__MAXI
      LNODE = VAL__MINI
      DO 3 I = 1, NEDGES
         IF ( GRAPH( 1, I ) .GT. LNODE ) LNODE = GRAPH( 1, I )
         IF ( GRAPH( 2, I ) .GT. LNODE ) LNODE = GRAPH( 2, I )
         IF ( GRAPH( 1, I ) .LT. FNODE ) FNODE = GRAPH( 1, I )
         IF ( GRAPH( 2, I ) .LT. FNODE ) FNODE = GRAPH( 2, I )
 3    CONTINUE

*  Reset number of output entries per-node.
      DO 4 I = FNODE, LNODE
         NOUT( I ) = 0
 4    CONTINUE

*  Estimate the maximum number of entries which are possible per node.
      DO 5 I = 1, NEDGES
         J = GRAPH( 4, I )
         NOUT( GRAPH( 1, I ) ) = NOUT( GRAPH( 1, I ) ) + NIN( J )
         NOUT( GRAPH( 2, I ) ) = NOUT( GRAPH( 2, I ) ) + NIN( J )
 5    CONTINUE

*  Loop over all node numbers, extracting values for those nodes which
*  have been used.
      DO 6 I = FNODE, LNODE
         IF ( NOUT( I ) .GT. 0 ) THEN
            CALL CCD1_MALL( NOUT( I ), '_DOUBLE', IPX( I ), STATUS )
            CALL CCD1_MALL( NOUT( I ), '_DOUBLE', IPY( I ), STATUS )
            CALL CCD1_MALL( NOUT( I ), '_INTEGER', IPIDS( I ), STATUS )

*  Now generate the final positions and identifiers removing multiple
*  identifications of the same positions.
            CALL CCD1_GEFP( GRAPH, NEDGES, %VAL( IPAX1 ), %VAL( IPAY1 ),
     :                      %VAL( IPAX2 ), %VAL( IPAY2 ), %VAL( IPI1 ),
     :                      IIN, OFFS, I,
     :                      %VAL( IPX( I ) ), %VAL( IPY( I ) ),
     :                      %VAL( IPIDS( I ) ), NOUT( I ), STATUS )

         END IF
 6    CONTINUE

*  Release unused workspace.
      CALL CCD1_MFREE( IPAX1, STATUS )
      CALL CCD1_MFREE( IPAY1, STATUS )
      CALL CCD1_MFREE( IPAX2, STATUS )
      CALL CCD1_MFREE( IPAY2, STATUS )
      CALL CCD1_MFREE( IPI1, STATUS )
      CALL CCD1_MFREE( IPI2, STATUS )
      CALL CCD1_MFREE( IP, STATUS )
      CALL CCD1_MFREE( IL, STATUS )

      END
* $Id$
