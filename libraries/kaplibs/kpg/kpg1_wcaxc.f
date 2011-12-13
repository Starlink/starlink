      SUBROUTINE KPG1_WCAXC( INDF, FS, AXIS, EL, CENTRE, STATUS )
*+
*  Name:
*     KPG1_WCAXC

*  Purpose:
*     Obtains co-ordinates for an axis from a WCS component FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_WCAXC( INDF, FS, AXIS, EL, CENTRE, STATUS )

*  Description:
*     This routine returns an array of world co-ordinates along a
*     nominated axis, that are derived from the Current Frame in the
*     supplied FrameSet.

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF identifier.
*     FS = INTEGER (Given)
*        An AST pointer for a FrameSet.
*     AXIS = INTEGER (Given)
*        The number of the axes array to obtain.
*     EL = INTEGER (Returned)
*        The number of elements in the centre array.
*     CENTRE( EL ) = DOUBLE PRECISION (Returned)
*        The axis centres.
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
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2006 January 3 (MJC):
*        Original version based upon DSB's FTS1_WCSAX.
*     2006 January 6 (MJC):
*        Mappings between current and pixel co-ordinates, i.e.
*        ties to AXIS Frame removed.
*     {enter_changes_here}

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

*  Arguments Returned:
      INTEGER EL
      DOUBLE PRECISION CENTRE( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIMS( NDF__MXDIM ) ! Dimensions of the NDF
      INTEGER IAX                ! Axes loop counter
      INTEGER ICURR              ! Index of Current Frame
      INTEGER IP                 ! Pointer to mapped AXIS Centre array
      INTEGER IPIXEL             ! Index of the PIXEL Frame
      INTEGER MAP1               ! n-D PIXEL to n-D AXIS mapping
      INTEGER MAP2               ! 1-D PIXEL to n-D PIXEL mapping
      INTEGER MAP3               ! n-D AXIS to 1-D AXIS mapping
      INTEGER MAP4               ! 1-D PIXEL to n-D AXIS mapping
      INTEGER MAP5               ! 1-D PIXEL to 1-D AXIS mapping
      INTEGER NDIM               ! Number of dimensions
      INTEGER OUTPRM( NDF__MXDIM ) ! Indices of corresponding axes

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the axis.
      CALL NDF_DIM( INDF, NDF__MXDIM, DIMS, NDIM, STATUS )
      IF ( AXIS .LT. 1 .OR. AXIS .GT. NDIM ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF )
         CALL MSG_SETI( 'AX', AXIS )
         CALL MSG_SETI( 'N', NDIM )
         CALL ERR_REP( 'KPG1_WCAXC',
     :     'The chosen axis ^AX is not valid for ^NDF, which has ^N '/
     :     /'dimensions (probable programming error).', STATUS )
         GOTO 999
      END IF

*  Set default axis length.
      EL = DIMS( AXIS )

*  Save the index of the original current Frame in the FrameSet, so
*  that it can be re-instated later.
      ICURR = AST_GETI( FS, 'CURRENT', STATUS )

*  Search for a PIXEL Frame in the FrameSet.  If one is found, it
*  becomes the current Frame. If one is not found, do nothing.
      IF ( AST_FINDFRAME( FS, AST_FRAME( NDIM, ' ', STATUS ),
     :                        'PIXEL', STATUS ) .NE. AST__NULL ) THEN

*  If found, get the index of the PIXEL Frame.
         IPIXEL = AST_GETI( FS, 'CURRENT', STATUS )

*  Get the mapping from the PIXEL Frame to the Current Frame.
         MAP1 = AST_GETMAPPING( FS, IPIXEL, ICURR, STATUS )

*  The AXIS co-ordinates associated with each axis in an NDF are
*  independent of other axes.  Therefore, each axis can be treated
*  separately as one-dimensional co-ordinate system.  We use AST
*  PermMaps to pick the axis to process.

*  Initialise the indices of the axes in the one-dimensional Frame
*  corresponding to each axis in the n-dimensional Frame.  Since the
*  axes are presumed to be independent of each other, it does not matter
*  what values we use so long as the axis being processed is assigned
*  the value 1.
         DO IAX = 1, NDIM
            OUTPRM( IAX ) = 0
         END DO
         OUTPRM( AXIS ) = 1

*  Process the axis in a separate AST context.
         CALL AST_BEGIN( STATUS )

*  MAP1 goes from n-dimensional PIXEL co-ordimnates to n-dimensional
*  AXIS co-ordinates.  The NDF AXIS structures require each axis to be
*  independent of all others, so we can process the required axis
*  without worrying about the others.  So we want to modify MAP1 such
*  that it maps only a single axis.  To do this we add a PermMap to the
*  input and output of MAP1, that selects only the required axis.
*  Create two PermMaps to extract axis AXIS from an n-dimensional Frame;
*  MAP2 goes from one-dimensional to n-dimensional, MAP3 goes in the
*  opposite sense.
         MAP2 = AST_PERMMAP( 1, AXIS, NDIM, OUTPRM, 0.0D0, ' ',
     :                       STATUS )
         MAP3 = AST_PERMMAP( NDIM, OUTPRM, 1, AXIS, 0.0D0, ' ',
     :                       STATUS )

*  Concatenate the Mappings together, to get a mapping between axis
*  AXIS in the PIXEL Frame to the same axis in the AXIS Frame.
         MAP4 = AST_CMPMAP( MAP2, MAP1, .TRUE., ' ', STATUS )
         MAP5 = AST_CMPMAP( MAP4, MAP3, .TRUE., ' ', STATUS )

*  Map the NDF's Axis Centre array.  The NDF library will fill this
*  array with the pixel co-ordinates at the centre of each pixel (the
*  default Axis co-ordinate system).
         CALL NDF_AMAP( INDF, 'CENTRE', AXIS, '_DOUBLE',
     :                  'READ', IP, EL, STATUS )

*  Check we can safely use %VAL on the pointer returned by NDF_AMAP.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Map these pixel co-ordinates into AXIS co-ordinates.
             CALL AST_TRAN1( MAP5, EL, %VAL( CNF_PVAL( IP ) ), .TRUE.,
     :                      CENTRE, STATUS )
         END IF

*  Unmap the Axis Centre array.
         CALL NDF_AUNMP( INDF, 'CENTRE', AXIS, STATUS )

*  End the AST context and do the next axis.
         CALL AST_END( STATUS )

      END IF

*  Re-instate the original current Frame.
      CALL AST_SETI( FS, 'CURRENT', ICURR, STATUS )

  999 CONTINUE

      END
