      SUBROUTINE KPG1_ASFIX( MAP, INDF1, INDF2, STATUS )
*+
*  Name:
*     KPG1_ASFIX

*  Purpose:
*     Modifies the WCS FrameSet of an NDF to take account of
*     re-gridding the pixel array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASFIX( MAP, INDF1, INDF2, STATUS )

*  Description:
*     This routine copies the WCS FrameSet from NDF1 to NDF2, re-mapping
*     the PIXEL Frame using the specified Mapping in the process. It should
*     be used to set up the WCS FrameSet of a newly created output NDF
*     which has been formed by applying a geometric transformation to an
*     input NDF.

*  Arguments:
*     MAP = INTEGER (Given)
*        An AST Mapping from pixel co-ordinates in the INDF1 to pixel
*        co-ordinates in INDF2.
*     INDF1 = INTEGER (Given)
*        The input NDF.
*     INDF2 = INTEGER (Given)
*        The output NDF. Any existing WCS FrameSet is discarded and
*        replaced by a re-mapped copy of the WCS FrameSet from the input
*        NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2002 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)

*  History:
*     11-JAN-2002 (DSB):
*        Original version.

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST declarations and constants

*  Arguments Given:
      INTEGER MAP
      INTEGER INDF1
      INTEGER INDF2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IPIX1              ! Index of the input PIXEL Frame
      INTEGER IPIX2              ! Index of the output PIXEL Frame
      INTEGER MAP1               ! Input GRID->PIXEL Mapping
      INTEGER MAP2               ! Output PIXEL->GRID Mapping
      INTEGER MAP3               ! Input GRID->Output GRID Mapping
      INTEGER IWCS1              ! WCS FrameSet from input NDF
      INTEGER IWCS2              ! WCS FrameSet from output NDF

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin a new AST context.
      CALL AST_BEGIN( STATUS )

*  The required output WCS is a copy of the input WCS in which the GRID
*  Frame has been re-mapped, using AST_REMAPFRAME. This requires the
*  Mapping from the input GRID Frame to the output GRID Frame. First get
*  the Mapping from the input GRID Frame to the input PIXEL Frame. To do
*  this, get the WCS FrameSet from the input NDF, find the PIXEL Frame,
*  and get the Mapping.
      CALL KPG1_GTWCS( INDF1, IWCS1, STATUS )
      CALL KPG1_ASFFR( IWCS1, 'PIXEL', IPIX1, STATUS )
      MAP1 = AST_GETMAPPING( IWCS1, AST__BASE, IPIX1, STATUS )

*  Also get the PIXEL->GRID Mapping from the output NDF. We use NDF_GTWCS
*  here instead of KPG1_GTWCS since we are only interested in the GRID and
*  PIXEL Frames and so do not need the facility for importing WCS from FITS
*  extensions, etc, which KPG1_GTWCS provides.
      CALL NDF_GTWCS( INDF2, IWCS2, STATUS )
      CALL KPG1_ASFFR( IWCS2, 'PIXEL', IPIX2, STATUS )
      MAP2 = AST_GETMAPPING( IWCS2, IPIX2, AST__BASE, STATUS )

*  Combine all three Mappings to get the required Mapping from input GRID
*  Frame to output GRID Frame. Simplify it.
      MAP3 = AST_SIMPLIFY( AST_CMPMAP( AST_CMPMAP( MAP1, MAP, .TRUE.,
     :                                             ' ', STATUS ),
     :                                 MAP2, .TRUE., ' ', STATUS ),
     :                     STATUS )

*  Use this Mapping to remap the GRID Frame in the input WCS FrameSet.
      CALL AST_REMAPFRAME( IWCS1, AST__BASE, MAP3, STATUS )

*  Store this modified WCS FrameSet in the output NDF.
      CALL NDF_PTWCS( IWCS1, INDF2, STATUS )

*  Exit the AST context.
      CALL AST_END( STATUS )

      END
