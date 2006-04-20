      SUBROUTINE KPG1_WCFAX( INDF, FS, AXIS, EL, CENTRE, STATUS )
*+
*  Name:
*     KPG1_WCFAX

*  Purpose:
*     Obtains the co-ordinate of an axis in the current WCS Frame at
*     every pixel centre in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_WCFAX( INDF, FS, AXIS, EL, CENTRE, STATUS )

*  Description:
*     This routine returns the world co-ordinate along a nominated WCS
*     axis for each pixel centre of the supplied NDF.  The co-ordinates 
*     are defined within the current Frame in the supplied FrameSet. 

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF identifier.
*     FS = INTEGER (Given)
*        An AST pointer for a FrameSet.
*     AXIS = INTEGER (Given)
*        The number of the WCS axis whose co-ordinates are to be
*        returned.
*     EL = INTEGER (Given)
*        The number of elements in the co-ordinate array.
*     CENTRE( EL ) = DOUBLE PRECISION (Returned)
*        The current-Frame co-ordinate along the nominated WCS axis
*         at each pixel centre.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research
*                   Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*     
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2006 January 25 (MJC):
*        Original version based upon DSB's FTS1_WCSAX.
*     2006 February 2 (MJC):
*        Some tidying after switch from AXIS Frame to current Frame for
*        output co-ordinates.
*     2006 February 10 (MJC):
*        Further tidying.  Checked for a 1-1 mapping to avoid n-D to
*        1-D transformation, preferring to obain just one vector and
*        duplicating it.
*     2006 February 15 (MJC):
*        Set expansion factors to 1 for unused duimensions up to
*        NDF__MXDIM.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'AST_PAR'          ! AST_ public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER INDF
      INTEGER FS
      INTEGER AXIS
      INTEGER EL

*  Arguments Returned:
      DOUBLE PRECISION CENTRE( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIMS( NDF__MXDIM ) ! Dimensions of the NDF
      INTEGER EXPAND( NDF__MXDIM ) ! Expansion factors for 1-to-1
                                 ! mapping
      INTEGER IAX                ! Axes loop counter
      INTEGER ICURR              ! Index of Current Frame 
      INTEGER IERR               ! Position of first numerical error
      INTEGER IGRID              ! Index of the GRID Frame
      INTEGER IPCOIN             ! Pointer to mapped grid co-ordinates
      INTEGER IPCOUT             ! Pointer to mapped current-Frame
                                 ! co-ordinates
      INTEGER MAP1               ! n-D GRID to n-D Current mapping
      INTEGER MAP1T1             ! 1-D GRID to 1-D Current mapping
      INTEGER MAP2               ! n-D to 1-D mapping
      INTEGER MAP3               ! n-D GRID to 1-D Current mapping
      INTEGER MASK               ! Dummy
      INTEGER NAX                ! Number of WCS axes
      INTEGER NDIM               ! Number of dimensions
      INTEGER NERR               ! Number of numerical errors
      INTEGER OUT( NDF__MXDIM )  ! Index of output Mapping
      INTEGER PERM( NDF__MXDIM ) ! Indices to select 1-D
      INTEGER VDIMS( NDF__MXDIM ) ! Dimensions of the vector

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the axis.  The supplied axis index refers to the current
*  Frame of the supplied frameSet, and so must be in the range 1 to
*  the number of axes in the current frame.  Note that this may not be
*  the same as the number of pixel axes in the NDF.
      NAX = AST_GETI( FS, 'Naxes', STATUS )
      IF ( AXIS .LT. 1 .OR. AXIS .GT. NAX ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF )
         CALL MSG_SETI( 'AX', AXIS )
         CALL MSG_SETI( 'N', NAX )
         CALL ERR_REP( 'KPG1_WCFAX',
     :     'The chosen axis ^AX is not valid for ^NDF, which has ^N '/
     :     /'WCS axes (probable programming error).', STATUS )
         GOTO 999
      END IF

*  Obtain the NDF dimensions.
      CALL NDF_DIM( INDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  Save the index of the original current Frame in the FrameSet, so 
*  that it can be re-instated later.
      ICURR = AST_GETI( FS, 'CURRENT', STATUS )

*  Get the index of the GRID Frame.
      IGRID = AST_GETI( FS, 'BASE', STATUS )

*  Process the WCS axis in a separate AST context.
      CALL AST_BEGIN( STATUS )

*  We wish to map from the GRID Frame to the current Frame.  Thus
*  we can substitute the FrameSet for the mapping.

*  Get the mapping from the GRID Frame to the Current Frame.
      MAP1 = AST_GETMAPPING( FS, IGRID, ICURR, STATUS )

*  See if the mapping can be split, such that it only depends
*  one input, that along the chosen WCS axis.
      CALL AST_MAPSPLIT( MAP1, 1, AXIS, OUT, MAP1T1, STATUS )
      IF ( MAP1T1 .NE. AST__NULL ) THEN 

*  Obtain workspace to hold the input GRID co-ordinates.
         CALL PSX_CALLOC( EL, '_DOUBLE', IPCOIN, STATUS )

*  Obtain workspace to hold the output current-Frame co-ordinates.
         CALL PSX_CALLOC( EL, '_DOUBLE', IPCOUT, STATUS )

*  Check we can safely use %VAL on the pointer returned by PSX_CALLOC.
         IF ( STATUS .NE. SAI__OK ) GOTO 990

*  Fill the input array with GRID co-ordinates.
         CALL KPG1_ELNMD( 1, DIMS( AXIS ), DIMS( AXIS ), 
     :                    %VAL( CNF_PVAL( IPCOIN ) ), STATUS )

*  Transform the vector to current-Frame co-ordinates.
         CALL AST_TRAN1( MAP1T1, DIMS( AXIS ), 
     :                   %VAL( CNF_PVAL( IPCOIN ) ), .TRUE.,
     :                   %VAL( CNF_PVAL( IPCOUT ) ), STATUS )

*  Duplicate this for all elements along the other pixel axes
*  to fill the CENTRE array.  There is no expansion along the 
*  chosen pixel axis.
         DO IAX = 1, NDF__MXDIM
            EXPAND( IAX ) = DIMS( IAX )
         END DO
         EXPAND( AXIS ) = 1

*  Specify the dimension along which the vector resides, and
*  the output dimensions.
         DO IAX = 1, NDF__MXDIM
            VDIMS( IAX ) = 1
         END DO
         VDIMS( AXIS ) = DIMS( AXIS )
         CALL KPG1_PXDPD( VDIMS, %VAL( CNF_PVAL( IPCOUT ) ), EXPAND,
     :                    .FALSE., MASK, DIMS, CENTRE, STATUS )

*  This is a brute-force method, as it may demand considerable memory
*  and ignores insignificant dimensions.
      ELSE

*  Obtain workspace to hold the input GRID co-ordinates.
         CALL PSX_CALLOC( EL * NDIM, '_DOUBLE', IPCOIN, STATUS )

*  Check we can safely use %VAL on the pointer returned by PSX_CALLOC.
         IF ( STATUS .NE. SAI__OK ) GOTO 990

*  Fill the input array with GRID co-ordinates.
         CALL KPG1_FIGRD( NDIM, DIMS, EL, %VAL( CNF_PVAL( IPCOIN ) ),
     :                    STATUS )

*  Initialise the indices of the axes in the one-dimensional Frame
*  corresponding to each axis in the n-dimensional Frame.  Since the
*  axes are presumed to be independent of each other, it does not matter
*  what values we use so long as the axis being processed is assigned
*  the value 1.  We use AST PermMaps to pick the axis to process.
         DO IAX = 1, NAX
            PERM( IAX ) = 0
         END DO
         PERM( AXIS ) = 1

*  MAP1 goes from n-dimensional GRID co-ordinates to n-dimensional 
*  current co-ordinates.  We need a mapping that transforms
*  n-dimensional GRID co-ordinates to a one-dimensional co-ordinates
*  along the specified axis.  So we want to modify MAP1 such that it
*  maps only a single axis.  To do this we add a PermMap to the input
*  and output of MAP1, that selects only the required axis.  Create a 
*  PermMaps to extract axis AXIS from an n-dimensional Frame; MAP2 goes
*  from n-dimensional to one-dimensional.
         MAP2 = AST_PERMMAP( NDIM, PERM, 1, AXIS, 0.0D0, ' ', STATUS )

*  Concatenate the Mappings together, to get a mapping between axis
*  AXIS in the GRID Frame to the same axis in the current Frame.
         MAP3 = AST_CMPMAP( MAP1, MAP2, .TRUE., ' ', STATUS )

*  Transform the GRID co-ordinates to current-Frame co-ordinates at
*  each pixel of the NDF.
         CALL AST_TRANN( MAP3, EL, NDIM, EL, %VAL( CNF_PVAL( IPCOIN ) ),
     :                   .TRUE., 1, EL, CENTRE, STATUS )
      END IF

  990 CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Re-instate the original current Frame.
      CALL AST_SETI( FS, 'CURRENT', ICURR, STATUS )

  999 CONTINUE

      END
