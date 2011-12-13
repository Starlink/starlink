      SUBROUTINE CCD1_GMMP( GRAPH, NEDGES, HINODE, IPX1, IPY1, IPRAN1,
     :                      IPX2, IPY2, IPRAN2, NIN, TOLS, OFFS, IPX,
     :                      IPY, IPRANK, IPIDS, NOUT, STATUS )
*+
*  Name:
*     CCD1_GMMP

*  Purpose:
*     Matches and merges position lists associated with a graph of
*     offsets.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GMMP( GRAPH, NEDGES, HINODE, IPX1, IPY1, IPRAN1, IPX2,
*                     IPY2, IPRAN, NIN, TOLS, OFFS, IPX, IPY, IPRANK,
*                     IPIDS, NOUT, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     GRAPH( 4, NEDGES ) = INTEGER ) (Given)
*        The spanning graph of edges which are associated with the
*        positions pointed to by IPX1(*), IPY1(*) and IPX2(*), IPY2(*).
*        The indices of an edges positions list are (4,*) in this array.
*     NEDGES = INTEGER (Given)
*        The number of edges in the spanning graph.
*     HINODE = INTEGER (Given)
*        The index of the highest-indexed node which may be in the
*        input graph.  The lowest is assumed to be 1.
*     IPX1( * ) = INTEGER (Given)
*        Pointer to X positions related to edge. These are indexed
*        by IPX1( GRAPH( 4, * ) ).
*     IPY1( * ) = INTEGER (Given)
*        Pointer to Y positions related to edge. These are indexed
*        by IPY1( GRAPH( 4, * ) ).
*     IPRAN1( * ) = INTEGER (Given)
*        Pointer to rank identifiers related to edge.  These are indexed
*        by IPRAN1( GRAPH( 4, * ) ).  The values are not used by this
*        routine, but the returned IPRANK preserves them in the
*        corresponding order.
*     IPX2( * ) = INTEGER (Given)
*        Pointer to X positions related to edge. These are indexed
*        by IPX2( GRAPH( 4, * ) ).
*     IPY2( * ) = INTEGER (Given)
*        Pointer to X positions related to edge. These are indexed
*        by IPY2( GRAPH( 4, * ) ).
*     IPRAN2( * ) = INTEGER (Given)
*        Pointer to rank identifiers related to edge.  These are indexed
*        by IPRAN2( GRAPH( 4, * ) ).  The values are not used by this
*        routine, but the returned IPRANK preserves them in the
*        corresponding order.
*     NIN( * ) = INTEGER (Given)
*        Number of X and Y positions in IPX1, IPY1, IPX2, IPY2.
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
*     OFFS( NEDGES + 1 ) = INTEGER (Given and Returned)
*        Workspace for offsets into vectorised lists.
*     IPX( * ) = INTEGER (Returned)
*        Pointers to all X positions matched at this node. The pointers
*        are generated in this routine. The index to this array and IPY,
*        IPRANK, IPIDS and NOUT are the node numbers.
*     IPY( * ) = INTEGER (Returned)
*        Pointers to all Y positions matched at this node. The pointers
*        are generated in this routine. The index to this array and IPX,
*        IPRANK, IPIDS and NOUT are the node numbers.
*     IPRANK( * ) = INTEGER (Returned)
*        Pointers to the rank identifiers for each node.  The mapping
*        from the points in the input arrays to the ones in the output
*        arrays can be kept track of using these values.  The index
*        to this array and IPX, IPY, IPIDS and NOUT are the node numbers.
*     IPIDS( * ) = INTEGER (Returned)
*        Pointers to the identifiers of the positions for each node. The
*        pointers are generated within this routine. The index to this
*        array and IPX, IPY, IPRANK and NOUT are the node numbers.
*     NOUT( * ) = INTEGER (Returned)
*        The numbers of positions matched at each node. The index to
*        this array and IPX, IPY, IPIDS and IPRANK are the node numbers.
*        The first HINODE elements of this array will be returned
*        correctly.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     29-JAN-2001 (MBT):
*        Added the IPRAN1, IPRAN2 and IPRANK parameters.
*     11-JUL-2001 (MBT):
*        Added TOLS parameter.
*     11-FEB-2002 (MBT):
*        Fixed a bug in which elements of NOUT were uninitialised for a
*        non-spanning graph.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NEDGES
      INTEGER GRAPH( 4, NEDGES )
      INTEGER HINODE
      INTEGER IPX1( * )
      INTEGER IPY1( * )
      INTEGER IPRAN1( * )
      INTEGER IPX2( * )
      INTEGER IPY2( * )
      INTEGER IPRAN2( * )
      INTEGER NIN( * )
      DOUBLE PRECISION TOLS( * )

*  Arguments Given and Returned:
      INTEGER OFFS( NEDGES + 1 )

*  Arguments Returned:
      INTEGER IPX( * )
      INTEGER IPY( * )
      INTEGER IPRANK( * )
      INTEGER IPIDS( * )
      INTEGER NOUT( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER IIN                ! Offset into lists
      INTEGER IPAR1              ! Pointer to workspace
      INTEGER IPAR2              ! Pointer to workspace
      INTEGER IPAX1              ! Pointer to workspace
      INTEGER IPAX2              ! Pointer to workspace
      INTEGER IPAY1              ! Pointer to workspace
      INTEGER IPAY2              ! Pointer to workspace
      INTEGER IPI1               ! Pointer to workspace
      INTEGER IPI2               ! Pointer to workspace
      INTEGER IP                 ! Pointer to workspace
      INTEGER IL                 ! Pointer to workspace
      INTEGER J                  ! Loop variable
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
      CALL CCD1_MALL( NEED, '_INTEGER', IPAR1, STATUS )
      CALL CCD1_MALL( NEED, '_INTEGER', IPAR2, STATUS )
      CALL CCD1_MALL( NEED, '_INTEGER', IPI1, STATUS )
      CALL CCD1_MALL( NEED, '_INTEGER', IPI2, STATUS )
      CALL CCD1_MALL( NEED, '_INTEGER', IP, STATUS )
      CALL CCD1_MALL( NEED, '_INTEGER', IL, STATUS )

*  Transfer the data.
      IIN = 1
      DO 2 I = 1, NEDGES
         J = GRAPH( 4, I )
         CALL CCG1_LAPND( %VAL( CNF_PVAL( IPX1( J ) ) ), NIN( J ), IIN,
     :                    %VAL( CNF_PVAL( IPAX1 ) ), STATUS )
         CALL CCG1_LAPND( %VAL( CNF_PVAL( IPY1( J ) ) ), NIN( J ), IIN,
     :                    %VAL( CNF_PVAL( IPAY1 ) ), STATUS )
         CALL CCG1_LAPND( %VAL( CNF_PVAL( IPX2( J ) ) ), NIN( J ), IIN,
     :                    %VAL( CNF_PVAL( IPAX2 ) ), STATUS )
         CALL CCG1_LAPND( %VAL( CNF_PVAL( IPY2( J ) ) ), NIN( J ), IIN,
     :                    %VAL( CNF_PVAL( IPAY2 ) ), STATUS )
         CALL CCG1_LAPNI( %VAL( CNF_PVAL( IPRAN1( J ) ) ),
     :                    NIN( J ), IIN,
     :                    %VAL( CNF_PVAL( IPAR1 ) ), STATUS )
         CALL CCG1_LAPNI( %VAL( CNF_PVAL( IPRAN2( J ) ) ),
     :                    NIN( J ), IIN,
     :                    %VAL( CNF_PVAL( IPAR2 ) ), STATUS )
         OFFS( I ) = IIN
         IIN = IIN + NIN( J )
 2    CONTINUE

*  Add final bound for offsets.
      OFFS( NEDGES + 1 ) = IIN
      IIN = IIN - 1

*  Now do the work of generating the matched identifiers.
      CALL CCD1_GMID( GRAPH, NEDGES, HINODE, %VAL( CNF_PVAL( IP ) ),
     :                %VAL( CNF_PVAL( IL ) ),
     :                %VAL( CNF_PVAL( IPAX1 ) ),
     :                %VAL( CNF_PVAL( IPAY1 ) ),
     :                %VAL( CNF_PVAL( IPAX2 ) ),
     :                %VAL( CNF_PVAL( IPAY2 ) ), IIN, TOLS, OFFS,
     :                %VAL( CNF_PVAL( IPI1 ) ),
     :                %VAL( CNF_PVAL( IPI2 ) ), STATUS )

*  Reset number of output entries per-node.
      DO 4 I = 1, HINODE
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
      DO 6 I = 1, HINODE
         IF ( NOUT( I ) .GT. 0 ) THEN
            CALL CCD1_MALL( NOUT( I ), '_DOUBLE', IPX( I ), STATUS )
            CALL CCD1_MALL( NOUT( I ), '_DOUBLE', IPY( I ), STATUS )
            CALL CCD1_MALL( NOUT( I ), '_INTEGER', IPRANK( I ), STATUS )
            CALL CCD1_MALL( NOUT( I ), '_INTEGER', IPIDS( I ), STATUS )

*  Now generate the final positions and identifiers removing multiple
*  identifications of the same positions.
            CALL CCD1_GEFP( GRAPH, NEDGES, %VAL( CNF_PVAL( IPAX1 ) ),
     :                      %VAL( CNF_PVAL( IPAY1 ) ),
     :                      %VAL( CNF_PVAL( IPAR1 ) ),
     :                      %VAL( CNF_PVAL( IPAX2 ) ),
     :                      %VAL( CNF_PVAL( IPAY2 ) ),
     :                      %VAL( CNF_PVAL( IPAR2 ) ),
     :                      %VAL( CNF_PVAL( IPI1 ) ), IIN, OFFS, I,
     :                      %VAL( CNF_PVAL( IPX( I ) ) ),
     :                      %VAL( CNF_PVAL( IPY( I ) ) ),
     :                      %VAL( CNF_PVAL( IPRANK( I ) ) ),
     :                      %VAL( CNF_PVAL( IPIDS( I ) ) ),
     :                      NOUT( I ), STATUS )
         END IF
 6    CONTINUE

*  Release unused workspace.
      CALL CCD1_MFREE( IPAX1, STATUS )
      CALL CCD1_MFREE( IPAY1, STATUS )
      CALL CCD1_MFREE( IPAX2, STATUS )
      CALL CCD1_MFREE( IPAY2, STATUS )
      CALL CCD1_MFREE( IPAR1, STATUS )
      CALL CCD1_MFREE( IPAR2, STATUS )
      CALL CCD1_MFREE( IPI1, STATUS )
      CALL CCD1_MFREE( IPI2, STATUS )
      CALL CCD1_MFREE( IP, STATUS )
      CALL CCD1_MFREE( IL, STATUS )

      END
* $Id$
