      SUBROUTINE CCD1_PSIZE( IWCS, JFRM, PSIZE, STATUS )
*+
*  Name:
*     CCD1_PSIZE

*  Purpose:
*     Get the approximate size of a pixel in an NDF.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_PSIZE( IWCS, JFRM, PSIZE, STATUS )

*  Description:
*     This routine gets the approximate linear dimension of an NDF's
*     pixels in a given coordinate frame of its WCS component.
*     The actual dimension returned is the SQRT(1/2) times the
*     distance between the transformed points which have GRID-domain
*     coordinates (0,0) and (1,1).  For a grossly nonlinear or
*     anisotropic mapping between the GRID frame and the chosen
*     one this may not give you what you want, but for most purposes
*     it is likely to be OK.
*
*     Note this routine only examines the first two dimensions at
*     each end of the mapping.  If the frames involved have fewer
*     than two dimensions it will fail, and if they have more than
*     two it will ignore the higher ones.

*  Arguments:
*     IWCS = INTEGER (Given)
*        An AST pointer to the WCS frameset of an NDF.
*     JFRM = INTEGER (Given)
*        The frame index of the frame in the IWCS frameset in which
*        the size is to be returned.  The symbolic values AST__CURRENT
*        and AST__BASE may be used.
*     PSIZE = DOUBLE PRECISION (Returned)
*        Linear dimension of a GRID pixel.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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
*     15-MAR-2001 (MBT):
*        Original version.
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
      INTEGER IWCS
      INTEGER JFRM

*  Arguments Returned:
      DOUBLE PRECISION PSIZE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER JGRID              ! Frame index of the GRID-domain frame
      INTEGER MAP                ! AST pointer to mapping
      DOUBLE PRECISION XP( 2 )   ! X coordinates in GRID frame
      DOUBLE PRECISION XQ( 2 )   ! X coordinates in JFRM frame
      DOUBLE PRECISION YP( 2 )   ! Y coordinates in GRID frame
      DOUBLE PRECISION YQ( 2 )   ! Y coordinates in JFRM frame

*  Local Data:
      DATA XP / 0D0, 1D0 /
      DATA YP / 0D0, 1D0 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the frame index of the GRID-domain frame.
      CALL CCD1_FRDM( IWCS, 'GRID', JGRID, STATUS )

*  Get the mapping from GRID to output coordinates.
      MAP = AST_GETMAPPING( IWCS, JGRID, JFRM, STATUS )

*  Transform the point from grid coordinates into the chosen frame.
      CALL AST_TRAN2( MAP, 2, XP, YP, .TRUE., XQ, YQ, STATUS )

*  Get the pixel size.
      IF ( STATUS .EQ. SAI__OK ) THEN
         PSIZE = SQRT( ( XQ( 2 ) - XQ( 1 ) ) ** 2
     :               + ( YQ( 2 ) - YQ( 1 ) ) ** 2 )
     :         / SQRT( 2D0 )
      END IF

*  Tidy up.
      CALL AST_ANNUL( MAP, STATUS )

      END
* $Id$
