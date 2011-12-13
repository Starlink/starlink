      SUBROUTINE KPS1_LOOK1( INDF, MCOMP, IWCS, SDIM, IGRP, RLBND,
     :                       RUBND, IPDAT, STATUS )
*+
*  Name:
*     KPS1_LOOK1

*  Purpose:
*     Copies a 2D ARD-specified region of an NDF into a work array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LOOK1( INDF, MCOMP, IWCS, SDIM, IGRP, RLBND, RUBND,
*                      IPDAT, STATUS )

*  Description:
*     This routine returns a pointer to a dynamically allocated
*     two-dimensional array containing a copy of sections of a component
*     of the input NDF.  The sections to copy are specified by an ARD
*     description.  The returned array is just large enough to hold the
*     specified regions.  Any pixels within the returned array that are
*     not inside the ARD region are flagged by setting their value to
*     (VAL__MAXD - 1).  VAL__BADD is not used, so that bad pixels and
*     excluded pixels can be distinguished.

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF identifier.
*     MCOMP = CHARACTER * ( * ) (Given)
*        The name of the NDF component to copy.
*     IWCS = INTEGER (Given)
*        An AST pointer to the WCS FrameSet read from the NDF.
*     SDIM( 2 ) = INTEGER (Given)
*        The indices of the significant pixel axes in the NDF.
*     IGRP = INTEGER (Given)
*        A GRP group holding the ARD description.
*     RLBND( 2 ) = INTEGER (Returned)
*        The lower pixel-index bounds of the returned array.
*     RUBND( 2 ) = INTEGER (Returned)
*        The upper pixel-index bounds of the returned array.
*     IPDAT = INTEGER (Returned)
*        A pointer to the memory holding the returned array.  This
*        should be freed using PSX_FREE when no longer needed.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 2001, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-OCT-2001 (DSB):
*        Original version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     2006 April 12 (MJC):
*        Remove unused variable and wrapped long lines.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER INDF
      CHARACTER MCOMP*(*)
      INTEGER IWCS
      INTEGER SDIM( 2 )
      INTEGER IGRP

*  Arguments Returned:
      INTEGER RLBND( 2 )
      INTEGER RUBND( 2 )
      INTEGER IPDAT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER N                  ! Max size for initial mask array
      PARAMETER( N = 400 )

*  Local Variables:
      DOUBLE PRECISION INA( 2 )  ! Input co-ords of window corner A
      DOUBLE PRECISION INB( 2 )  ! Input co-ords of window corner B
      DOUBLE PRECISION OUTA( 2 ) ! Output co-ords of window corner A
      DOUBLE PRECISION OUTB( 2 ) ! Output co-ords of window corner B
      DOUBLE PRECISION XP( 2 )   ! X co-ords
      DOUBLE PRECISION YP( 2 )   ! Y co-ords
      INTEGER DIMS( 2 )          ! Dimensions of significant axes
      INTEGER EL                 ! Number of mapped elements
      INTEGER GFRM               ! AST pointer to mask grid co-ordinate
                                 ! Frame
      INTEGER ICURR              ! Index of original current Frame
      INTEGER INDF2              ! NDF section identifier
      INTEGER IPIN               ! Pointer to mapped NDF array component
      INTEGER IPMASK             ! Pointer to mask array
      INTEGER JWCS               ! AST pointer to modified FrameSet
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER LBNDE( 2 )         ! Lower bounds of exterior bounding box
      INTEGER LBNDI( 2 )         ! Lower bounds of interior bounding box
      INTEGER MAP                ! AST Mapping from ARDPIXCO to GRID
      INTEGER NDIM               ! Number of pixel axes in NDF
      INTEGER PFRM               ! AST pointer to mask pixel co-ordinate
                                 ! Frame
      INTEGER RDIM( 2 )          ! Dimensions of returned array
      INTEGER RV                 ! The returned value of REGVAL
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF
      INTEGER UBNDE( 2 )         ! Upper bounds of exterior bounding box
      INTEGER UBNDI( 2 )         ! Upper bounds of interior bounding box
      INTEGER WDIM( 2 )          ! Dimensions of mask array
      INTEGER WINMAP             ! AST pointer to a WinMap Mapping
      INTEGER WLBND( 2 )         ! Lower bounds for mask array
      INTEGER WUBND( 2 )         ! Lower bounds for mask array
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the bounds of the NDF.
      CALL NDF_BOUND( INDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Get the dimensions of the significant axes.
      DIMS( 1 ) = UBND( SDIM( 1 ) ) - LBND( SDIM( 1 ) ) + 1
      DIMS( 2 ) = UBND( SDIM( 2 ) ) - LBND( SDIM( 2 ) ) + 1

*  A mask array needs to be supplied to ARD_WORK which must be big
*  enough to include the entire ARD description. But how big is that?
*  For smallish NDFs we can just supply a mask array the same size as
*  the entire NDF.
      IF( DIMS( 1 )*DIMS( 2 ) .LE. N*N ) THEN
         WLBND( 1 ) = LBND( SDIM( 1 ) )
         WLBND( 2 ) = LBND( SDIM( 2 ) )
         WUBND( 1 ) = UBND( SDIM( 1 ) )
         WUBND( 2 ) = UBND( SDIM( 2 ) )

*  If the image is large, we need to avoid heavy memory usage.  To do
*  this, we find the...
      ELSE

*  Create a smallish work array over which the ARD description will be
*  evaluated.  This array will be mapped on to the entire NDF using a
*  WinMap.
         WLBND( 1 ) = 1
         WLBND( 2 ) = 1
         WUBND( 1 ) = N
         WUBND( 2 ) = N
         WDIM( 1 ) = WUBND( 1 ) - WLBND( 1 ) + 1
         WDIM( 2 ) = WUBND( 2 ) - WLBND( 2 ) + 1
         CALL PSX_CALLOC( WDIM( 1 )*WDIM( 2 ), '_INTEGER', IPMASK,
     :                    STATUS )

*  Take a copy of the supplied FrameSet so that the original is not
*  modified.
         JWCS = AST_COPY( IWCS, STATUS )

*  Note the index of the original current Frame in the FrameSet.
         ICURR = AST_GETI( JWCS, 'CURRENT', STATUS )

*  Create a new Frame representing grid co-ords within the above work
*  array.  Give it the Domain ARDGRIDCO so that it can be distinguished
*  from the real GRID Frame already in the FrameSet.
         GFRM = AST_FRAME( 2, 'DOMAIN=ARDGRIDCO', STATUS )

*  Add this Frame into the FrameSet, connecting it to the base (GRID)
*  Frame using a WinMap which results in the array covering the entire
*  NDF.  It becomes the current Frame.
         INA( 1 ) = 0.5D0
         INA( 2 ) = 0.5D0
         INB( 1 ) = DBLE( DIMS( 1 ) ) + 0.5D0
         INB( 2 ) = DBLE( DIMS( 2 ) ) + 0.5D0
         OUTA( 1 ) = 0.5D0
         OUTA( 2 ) = 0.5D0
         OUTB( 1 ) = DBLE( WDIM( 1 ) ) + 0.5D0
         OUTB( 2 ) = DBLE( WDIM( 2 ) ) + 0.5D0
         WINMAP = AST_WINMAP( 2, INA, INB, OUTA, OUTB, ' ', STATUS )
         CALL AST_ADDFRAME( JWCS, AST__BASE, WINMAP, GFRM, STATUS )

*  Create a new Frame representing pixel co-ordinates within the work
*  array.  Give it the Domain ARDPIXCO so that it can be distinguished
*  from  any PIXEL Frame already in the FrameSet.
         PFRM = AST_FRAME( 2, 'DOMAIN=ARDPIXCO', STATUS )

*  Add this Frame into the FrameSet, connecting it to the ARDGRIDCO
*  Frame using a WinMap which performs the required pixel shift.
         INA( 1 ) = 0.5D0
         INA( 2 ) = 0.5D0
         INB( 1 ) = 1.5D0
         INB( 2 ) = 1.5D0
         OUTA( 1 ) = DBLE( WLBND( 1 ) - 1 )
         OUTA( 2 ) = DBLE( WLBND( 2 ) - 1 )
         OUTB( 1 ) = OUTA( 1 ) + 1.0D0
         OUTB( 2 ) = OUTA( 2 ) + 1.0D0
         WINMAP = AST_WINMAP( 2, INA, INB, OUTA, OUTB, ' ', STATUS )
         CALL AST_ADDFRAME( JWCS, AST__CURRENT, WINMAP, PFRM, STATUS )

*  Save the Mapping from the ARDPIXCO Frame to the NDF GRID Frame.
         MAP = AST_GETMAPPING( JWCS, AST__CURRENT, AST__BASE, STATUS )

*  Reinstate the original current Frame.
         CALL AST_SETI( JWCS, 'CURRENT', ICURR, STATUS )

*  Use the modified FrameSet as the application FrameSet within
*  ARD_WORK.  Indicate the Domain associated with pixel co-ordinates
*  within the mask array.
         CALL ARD_WCS( JWCS, 'ARDPIXCO', STATUS )

*  Find the bounds of the box enclosing the entire ARD region.  These
*  bounds are given within the ARDPIXCO Frame.
         RV = 2
         CALL ARD_WORK( IGRP, 2, WLBND, WUBND, VAL__BADR, .FALSE.,
     :                  RV, %VAL( CNF_PVAL( IPMASK ) ),
     :                  LBNDI, UBNDI, LBNDE, UBNDE,
     :                  STATUS )

*  Report an error if the region contains no data.
         IF( ( LBNDI( 1 ) .GT. UBNDI( 1 ) .OR.
     :         LBNDI( 2 ) .GT. UBNDI( 2 ) ) .AND.
     :       STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'KPS1_LOOK1_ERR1', 'There is no overlap '//
     :                    'between ''^NDF'' and the supplied ARD '//
     :                    'description.', STATUS )
            GO TO 999
         END IF

*  Convert the bounds into the GRID Frame of the NDF.  Add a
*  single-pixel safety margin.
         XP( 1 ) = DBLE( LBNDI( 1 ) - 2 )
         XP( 2 ) = DBLE( UBNDI( 1 ) + 1 )
         YP( 1 ) = DBLE( LBNDI( 2 ) - 2 )
         YP( 2 ) = DBLE( UBNDI( 2 ) + 1 )
         CALL AST_TRAN2( MAP, 2, XP, YP, .TRUE., XP, YP, STATUS )

*  Convert these into pixel indices in the NDF.
         WLBND( 1 ) = NINT( XP( 1 ) ) + LBND( SDIM( 1 ) ) - 1
         WLBND( 2 ) = NINT( YP( 1 ) ) + LBND( SDIM( 2 ) ) - 1
         WUBND( 1 ) = NINT( XP( 2 ) ) + LBND( SDIM( 1 ) ) - 1
         WUBND( 2 ) = NINT( YP( 2 ) ) + LBND( SDIM( 2 ) ) - 1

*  Free the old mask.
         CALL PSX_FREE( IPMASK, STATUS )

      END IF

*  Allocate a mask.
      WDIM( 1 ) = WUBND( 1 ) - WLBND( 1 ) + 1
      WDIM( 2 ) = WUBND( 2 ) - WLBND( 2 ) + 1
      CALL PSX_CALLOC( WDIM( 1 )*WDIM( 2 ), '_INTEGER', IPMASK, STATUS )

*  Use the original FrameSet as the application FrameSet within
*  ARD_WORK.  Indicate the Domain associated with pixel co-ordinates
*  within the mask array.
      CALL ARD_WCS( IWCS, 'PIXEL', STATUS )

*  Find the bounds of the box enclosing the entire ARD region.  These
*  bounds are given within the PIXEL Frame.
      RV = 2
      CALL ARD_WORK( IGRP, 2, WLBND, WUBND, VAL__BADR, .FALSE.,
     :               RV, %VAL( CNF_PVAL( IPMASK ) ),
     :               RLBND, RUBND, LBNDE, UBNDE,
     :               STATUS )

*  Allocate the returned array.
      RDIM( 1 ) = RUBND( 1 ) - RLBND( 1 ) + 1
      RDIM( 2 ) = RUBND( 2 ) - RLBND( 2 ) + 1
      CALL PSX_CALLOC( RDIM( 1 )*RDIM( 2 ), '_DOUBLE', IPDAT, STATUS )

*  Obtain a section of the NDF with these bounds.
      LBND( SDIM( 1 ) ) = RLBND( 1 )
      LBND( SDIM( 2 ) ) = RLBND( 2 )
      UBND( SDIM( 1 ) ) = RUBND( 1 )
      UBND( SDIM( 2 ) ) = RUBND( 2 )
      CALL NDF_SECT( INDF, NDIM, LBND, UBND, INDF2, STATUS )

*  Map the required section component.
      CALL NDF_MAP( INDF2, MCOMP, '_DOUBLE', 'READ', IPIN, EL,
     :              STATUS )

*  Copy the masked NDF array into the returned array.
      CALL KPS1_LOOK2( WLBND( 1 ), WUBND( 1 ), WLBND( 2 ), WUBND( 2 ),
     :                 %VAL( CNF_PVAL( IPMASK ) ),
     :                 ( VAL__MAXD - 1.0D0 ),
     :                 RLBND( 1 ), RUBND( 1 ), RLBND( 2 ), RUBND( 2 ),
     :                 %VAL( CNF_PVAL( IPIN ) ),
     :                 %VAL( CNF_PVAL( IPDAT ) ), STATUS )

*  Tidy up.
 999  CONTINUE

*  Free resources.
      CALL NDF_ANNUL( INDF2, STATUS )
      CALL PSX_FREE( IPMASK, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
