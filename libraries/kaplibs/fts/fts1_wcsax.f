      SUBROUTINE FTS1_WCSAX( INDF, FS, NDIM, STATUS )
*+
*  Name:
*     FTS1_WCSAX

*  Purpose:
*     Re-creates AXIS structures from a WCS component FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_WCSAX( INDF, FS, NDIM, STATUS )

*  Description:
*     This routine creates NDF Axis structures from an AXIS Frame in the
*     supplied FrameSet. It looks for AXIS and PIXEL Frames in the
*     supplied FrameSet. If either of these Frames is not found, it does
*     nothing. Otherwise, it attempts to create AXIS structures in the NDF
*     from the AXIS Frame in the FrameSet. The AXIS Centre, Label and Unit
*     components are set.

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF identifier.
*     FS = INTEGER (Given)
*        An AST pointer for a FrameSet.
*     NDIM = INTEGER (Given)
*        The number of axes in the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1997, 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-DEC-1997 (DSB):
*        Original version.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL.
*     2008 June 18 (MJC):
*        Trim trailing blanks from output NDF character components.
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
      INTEGER NDIM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER AXIS*3           ! Axis index
      CHARACTER LAB*80           ! Axis label
      CHARACTER UNIT*80          ! Axis Units string
      INTEGER EL                 ! Number of elements in mapped array
      INTEGER IAX                ! Current axis index
      INTEGER IAXIS              ! Index of the AXIS Frame
      INTEGER ICURR              ! Index of Current Frame
      INTEGER IP                 ! Pointer to mapped AXIS Centre array
      INTEGER IPIXEL             ! Index of the PIXEL Frame
      INTEGER MAP1               ! n-D PIXEL to n-D AXIS mapping
      INTEGER MAP2               ! 1-D PIXEL to n-D PIXEL mapping
      INTEGER MAP3               ! n-D AXIS to 1-D AXIS mapping
      INTEGER MAP4               ! 1-D PIXEL to n-D AXIS mapping
      INTEGER MAP5               ! 1-D PIXEL to 1-D AXIS mapping
      INTEGER NC                 ! No. of characters in text
      INTEGER OUTPRM( NDF__MXDIM ) ! Indices of corresponding axes

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure there are no defined AXIS structures in the NDF.
      CALL NDF_RESET( INDF, 'AXIS', STATUS )

*  Save the index of the original current Frame in the FrameSet, so
*  that it can be re-instated later.
      ICURR = AST_GETI( FS, 'CURRENT', STATUS )

*  Search for a PIXEL Frame in the FrameSet. If one is found, it becomes
*  the current Frame. If one is not found, do nothing.
      IF( AST_FINDFRAME( FS, AST_FRAME( NDIM, ' ', STATUS ),
     :                      'PIXEL', STATUS ) .NE. AST__NULL ) THEN

*  If found, get the index of the PIXEL Frame.
         IPIXEL = AST_GETI( FS, 'CURRENT', STATUS )

*  Search for an AXIS Frame in the FrameSet. If one is found, it becomes
*  the current Frame. If one is not found, do nothing.
         IF( AST_FINDFRAME( FS, AST_FRAME( NDIM, ' ', STATUS ),
     :                         'AXIS', STATUS ) .NE. AST__NULL ) THEN

*  If found, get the index of the AXIS Frame.
            IAXIS = AST_GETI( FS, 'CURRENT', STATUS )

*  Get the mapping from the PIXEL Frame to the AXIS Frame.
            MAP1 = AST_GETMAPPING( FS, IPIXEL, IAXIS, STATUS )

*  The AXIS coordinates associated with each axis in an NDF are independant
*  of other axes. Therefore, each axis can be treated separately as a 1-D
*  coordinate system. We use AST PermMaps to pick the axis to process.
*  Initialise the indices of the axes in the 1-D Frame corresponding to
*  each axis in the n-D Frame. Since the axes are presumed to be
*  independant of each other, it does not matter what values we use
*  so long as the axis being processed is assigned the value 1. Therefore
*  it is easiest just to assign 1 to all axes.
            DO IAX = 1, NDIM
               OUTPRM( IAX ) = 1
            END DO

*  Loop round each axis. Process each axis in a separate AST context.
            DO IAX = 1, NDIM
               CALL AST_BEGIN( STATUS )

*  MAP1 goes from n-D PIXEL coords to n-D AXIS coords. The NDF AXIS structures
*  require each axis to be independant of all others, so we can process
*  each axis separately, making things much simpler. So we want to modify
*  MAP1 so that it maps only a single axis. To do this we add a PermMap
*  to the input and output of MAP1, which selects only the required axis.
*  Create two PermMaps to extract axis IAX from an n-D Frame; MAP2 goes
*  from 1-D to n-D, MAP3 goes from n-D to 1-D.
               MAP2 = AST_PERMMAP( 1, IAX, NDIM, OUTPRM, 0.0D0, ' ',
     :                                STATUS )
               MAP3 = AST_PERMMAP( NDIM, OUTPRM, 1, IAX, 0.0D0, ' ',
     :                                STATUS )

*  Concatenate the Mappings together, to get a mapping between axis
*  IAX in the PIXEL Frame to the same axis in the AXIS Frame.
               MAP4 = AST_CMPMAP( MAP2, MAP1, .TRUE., ' ', STATUS )
               MAP5 = AST_CMPMAP( MAP4, MAP3, .TRUE., ' ',
     :                               STATUS )

*  Map the NDF's Axis Centre array. The NDF library will fill this array
*  with the pixel coordinates at the centre of each pixel (the default
*  Axis coordinate system).
               CALL NDF_AMAP( INDF, 'CENTRE', IAX, '_DOUBLE',
     :                           'UPDATE', IP, EL, STATUS )

*  Check we can safely use %VAL on the pointer returned by NDF_AMAP.
               IF( STATUS .EQ. SAI__OK ) THEN

*  Map these pixel coordinates into AXIS coordinates.
                  CALL AST_TRAN1( MAP5, EL, %VAL( CNF_PVAL( IP ) ),
     :                            .TRUE.,
     :                            %VAL( CNF_PVAL( IP ) ), STATUS )
               END IF

*  Unmap the Axis Centre array.
               CALL NDF_AUNMP( INDF, 'CENTRE', IAX, STATUS )

*  If the Axis has a set value for the Label attribute, use it as
*  the NDF AXIS label.
               NC = 0
               CALL CHR_PUTI( IAX, AXIS, NC )

               IF( AST_TEST( FS, 'Label(' // AXIS( : NC ) // ')',
     :                       STATUS ) ) THEN
                  LAB = AST_GETC( FS, 'Label(' // AXIS( : NC ) // ')',
     :                            STATUS )
                  CALL NDF_ACPUT( LAB( : NC + 7 ), INDF, 'Lab', IAX,
     :                            STATUS )
               END IF

*  If the Axis has a set value for the Unit attribute, use it as
*  the NDF AXIS unit.  Note that NDF_ACPUT does not truncate
*  trailing blanks.
               IF( AST_TEST( FS, 'Unit(' // AXIS( : NC ) // ')',
     :                       STATUS ) ) THEN
                  UNIT = AST_GETC( FS, 'Unit(' // AXIS( : NC ) // ')',
     :                             STATUS )
                  CALL NDF_ACPUT( UNIT( : NC + 6 ), INDF, 'Unit', IAX,
     :                            STATUS )
               END IF

*  End the AST context and do the next axis.
               CALL AST_END( STATUS )
            END DO

         END IF

      END IF

*  Re-instate the original current Frame.
      CALL AST_SETI( FS, 'CURRENT', ICURR, STATUS )

      END
