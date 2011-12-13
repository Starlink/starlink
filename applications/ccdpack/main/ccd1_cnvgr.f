      SUBROUTINE CCD1_CNVGR( IFSETS, NFSET, DMNS, NDMN, USESET, SNAME,
     :                       GRAPH, NEDGE, STATUS )
*+
*  Name:
*     CCD1_CNVGR

*  Purpose:
*     Create a graph of conversions between frameset pairs.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_CNVGR( IFSETS, NFSET, DMNS, NDMN, USESET, SNAME, GRAPH,
*                      NEDGE, STATUS )

*  Description:
*     This routine creates a representation of a graph in which the
*     nodes are AST framesets and the edges are conversions between
*     them.  A maximum of one edge between two nodes is allowed, and
*     represents the conversion using one of the intermediate domains
*     given in the DMNS array; preferentially the first, if that fails
*     then the second, and so on.  The graph is returned as a series
*     of edges, each containing the node numbers, the index of the
*     domain used for matching, and an index into any associated data
*     created.  If USESET is true, then the first domain in DMNS
*     is only used if the corresponding SNAME values match; in this
*     way Set-based alignment can be taken account of.

*  Arguments:
*     IFSETS( NFSET ) = INTEGER (Given)
*        Array of AST pointers to framesets.
*     NFSET = INTEGER (Given)
*        Dimension of IFSETS.
*     DMNS( NDMN ) = CHARACTER * ( * ) (Given)
*        Ordered list of domains in which frame matching should occur.
*     NDMN = INTEGER (Given)
*        Dimension of DMNS.
*     USESET = LOGICAL (Given)
*        If true, then the first domain in DMNS will be ignored unless
*        the elements of the SNAME array for the two framesets being
*        matched are the same.
*     SNAME( NFSET ) = CHARACTER * ( * ) (Given)
*        If USESET is true, then this array is used to store the Set
*        Name attribute corresponding to each frameset.
*     GRAPH( 4, * ) = INTEGER (Returned)
*        The graph as a set of edges, with node numbers (1,*) and (2,*),
*        match domain indices (3,*), and index into associated data
*        (4,*) (this last is not really used, but exists for
*        compatibility with other CCD1_ graph handling routines).
*        The second dimension should be large enough to hold all
*        possible matches, NFSET * ( NFSET - 1 ) / 2.
*     NEDGE = INTEGER (Returned)
*        Second dimension of GRAPH, which is the number of successful
*        conversions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The algorithm used makes use of high-level AST routines
*     including AST_CONVERT.  If necessary it could probably be recoded
*     more efficiently by examining the Domain attributes of each frame
*     in turn directly.

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
*     16-FEB-2001 (MBT):
*        Upgraded for Set alignment.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants

*  Arguments Given:
      INTEGER NFSET
      INTEGER IFSETS( NFSET )
      INTEGER NDMN
      CHARACTER * ( * ) DMNS( NDMN )
      LOGICAL USESET
      CHARACTER * ( * ) SNAME( NFSET )

*  Arguments Returned:
      INTEGER GRAPH( 4, * )
      INTEGER NEDGE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( AST__SZCHR ) DLIST ! Domain list for conversion
      CHARACTER * ( AST__SZCHR ) DMCNV ! Domain in which conversion took place
      CHARACTER * ( AST__SZCHR ) NDLIST ! Domain list omitting first element
      CHARACTER * ( AST__SZCHR ) ODLIST ! Domain list including first element
      INTEGER COUNT              ! Position in original list of edges
      INTEGER CNV                ! AST pointer to conversion frameset
      INTEGER FRBAS              ! Base frame of frameset
      INTEGER I                  ! Loop variable
      INTEGER IDMN               ! Index in DLIST of conversion domain
      INTEGER J                  ! Loop variable
      INTEGER K                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise variables.
      NEDGE = 0
      COUNT = 0

*  Set up the domain list parameter to AST_CONVERT, which needs to be an
*  ordered comma-separated list of the domains for conversion.  If
*  we are doing Set-based alignment, construct a list omitting the
*  first element too.
      CALL CCD1_DLCAT( DMNS, NDMN, ',', ODLIST, STATUS )
      IF ( USESET ) THEN
         CALL CCD1_DLCAT( DMNS( 2 ), NDMN - 1, ',', NDLIST, STATUS )
      END IF

*  Begin new AST context.
      CALL AST_BEGIN( STATUS )

*  Loop over all pairs of framesets.
      DO 1 I = 1, NFSET - 1
         DO 2 J = I + 1, NFSET
            COUNT = COUNT + 1

*  Attempt conversion between framesets.
            IF ( USESET ) THEN
               IF ( SNAME( I ) .EQ. SNAME( J ) ) THEN
                  DLIST = ODLIST
               ELSE
                  DLIST = NDLIST
               END IF
            ELSE
               DLIST = ODLIST
            END IF
            CNV = AST_CONVERT( IFSETS( I ), IFSETS( J ), DLIST, STATUS )

*  If conversion is successful, write a new edge to the graph.
            IF ( CNV .NE. AST__NULL ) THEN

*  Annul the pointer to conserve resources.
               CALL AST_ANNUL( CNV, STATUS )

*  Get the domain via which the conversion took place.
               FRBAS = AST_GETFRAME( IFSETS( I ), AST__BASE, STATUS )
               DMCNV = AST_GETC( FRBAS, 'Domain', STATUS )
               CALL AST_ANNUL( FRBAS, STATUS )

*  Find the index of this domain in the domain list.
               DO 3 K = 1, NDMN
                  IF ( DMNS( K ) .EQ. DMCNV ) THEN
                     IDMN = K
                     GO TO 4
                  END IF
 3             CONTINUE

*  This shouldn't really happen, but just in case an empty domain has
*  slipped in or something.
               IDMN = NDMN + 1
 4             CONTINUE

*  Write the new edge.
               NEDGE = NEDGE + 1
               GRAPH( 1, NEDGE ) = I
               GRAPH( 2, NEDGE ) = J
               GRAPH( 3, NEDGE ) = IDMN
               GRAPH( 4, NEDGE ) = COUNT
            END IF
 2       CONTINUE
 1    CONTINUE

*  End AST context.
      CALL AST_END( STATUS )

      END
* $Id$
