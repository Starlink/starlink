      SUBROUTINE KPG1_ASSPL( IWCS, MXAX, MAP, STATUS )
*+
*  Name:
*     KPG1_ASSPL

*  Purpose:
*     Gets a set of one-dimensional Mappings for each axis in a
*     FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASSPL( IWCS, MXAX, MAP, STATUS )

*  Description:
*     This routine returns a set of AST Mapping pointers. Each Mapping
*     has one input and one output. The I'th Mapping goes from Axis I in
*     the Base Frame of the supplied FrameSet, to Axis I in the Current
*     Frame of the FrameSet.
*
*     There should usually be a one-to-one correspondance between the
*     axes in the Base and Current Frames in the FrameSet.

*  Arguments:
*     IWCS = INTEGER (Given)
*        The AST pointer to the FrameSet.
*     MXAX = INTEGER (Given)
*        The maximum number of mappings to be returned. Un-used elements
*        are returned holding AST__NULL.
*     MAP( MXAX ) = INTEGER (Returned)
*        The Mapping pointers.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER IWCS
      INTEGER MXAX

*  Arguments Returned:
      INTEGER MAP( MXAX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                    ! Axis index
      INTEGER MAP0                 ! Base->Current Mapping from FrameSet
      INTEGER NBAX                 ! No. of axes in base Frame
      INTEGER NCAX                 ! No. of axes in current Frame
      INTEGER PERM( NDF__MXDIM )   ! Axis permutation array
      INTEGER PMAP                 ! PermMap pointer

*  Local Data:
      DATA PERM/NDF__MXDIM*0/

*.

*  Initialise.
      DO I = 1, MXAX
         MAP( I )  = AST__NULL
      END DO

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Mapping form Base to Current Frame.
      MAP0 = AST_SIMPLIFY( AST_GETMAPPING( IWCS, AST__BASE,
     :                                     AST__CURRENT, STATUS ),
     :                     STATUS )

*  Find the no. of axes in Base and Current Frame.
      NBAX = AST_GETI( MAP0, 'NIN', STATUS )
      NCAX = AST_GETI( MAP0, 'NOUT', STATUS )

*  Do each required axis.
      DO I = 1, MIN( MXAX, MIN( NBAX, NCAX ) )

*  Set the element of the permutation array. for this axis to 1.
         PERM( I ) = 1

*  Create a PermMap with 1 input and NBAX outputs which feeds the
*  Base Frame. The 1 input is fed from output axis I (by the inverse
*  PermMap), and output axis I is fed by input Axis 1 (by the forward
*  PermMap). All other outputs are assigned AST__BAD by the forward
*  PermMap.
         PMAP = AST_PERMMAP( 1, I, NBAX, PERM, 0.0D0, ' ', STATUS )

*  Concatenate this Mapping in series with the Base->Current Mapping.
         MAP( I ) = AST_CMPMAP( PMAP, MAP0, .TRUE., ' ', STATUS )

*  Create a second PermMap with NCAX inputs fed by the Current Frame, and
*  1 output. The 1 output is fed from input axis I (by the forward
*  PermMap), and input axis I is fed by output Axis 1 (by the inverse
*  PermMap). All other inputs are assigned AST__BAD by the inverse
*  PermMap.
         PMAP = AST_PERMMAP( NCAX, PERM, 1, I, 0.0D0, ' ', STATUS )

*  Concatenate the previous CmpMap in series with this Mapping.
         MAP( I ) = AST_CMPMAP( MAP( I ), PMAP, .TRUE., ' ', STATUS )

*  Reset the element of the permutation array for this axis to 0.
         PERM( I ) = 0

*  Export the Mapping.
         CALL AST_EXPORT( MAP( I ), STATUS )

      END DO

*  If an error occurred, annul the returned Mappings.
      IF( STATUS .NE. SAI__OK ) THEN
         DO I = 1, MXAX
            IF( MAP( I ) .NE. AST__NULL ) CALL AST_ANNUL( MAP( I ),
     :                                                    STATUS )
         END DO
      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
