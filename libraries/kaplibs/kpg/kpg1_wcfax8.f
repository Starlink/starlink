      SUBROUTINE KPG1_WCFAX8( LBND, UBND, MAP, JAXIS, IAXIS, CENTRE,
     :                        WORK, STATUS )
*+
*  Name:
*     KPG1_WCFAX8

*  Purpose:
*     Obtains the co-ordinate of an axis in the current WCS Frame at
*     every pixel centre in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_WCFAX8( LBND, UBND, MAP, JAXIS, IAXIS, CENTRE, WORK,
*                       STATUS )

*  Description:
*     This routine returns the world co-ordinate along a nominated WCS
*     axis for each pixel centre of an NDF.  The co-ordinates are defined
*     within the current Frame in the supplied FrameSet. It is assumed
*     that the nominated axis is independent of the other axes.

*  Arguments:
*     LBND( * ) = INTEGER*8 (Given)
*        The lower pixel index bounds of the NDF. This array should have
*        one element for each NDF pixel axis.
*     UBND( * ) = INTEGER*8 (Given)
*        The upper pixel index bounds of the NDF. This array should have
*        one element for each NDF pixel axis.
*     MAP = INTEGER (Given)
*        Mapping from PIXEL coords in the NDF to current WCS coords.
*     JAXIS = INTEGER (Given)
*        The number of the pixel axis which is varied.
*     IAXIS = INTEGER (Given)
*        The number of the WCS axis whose values are returned.
*     CENTRE( * ) = DOUBLE PRECISION (Returned)
*        The current-Frame co-ordinate along the nominated axis at each
*        pixel centre. The size and shape of this array should be the
*        same as the NDF.
*     WORK( * ) = DOUBLE PRECISION (Returned)
*        A work array with the same length as the requested pixel axis.
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
*     2006 January 25 (MJC):
*        Original version based upon DSB's FTS1_WCSAX.
*     2006 February 2 (MJC):
*        Some tidying after switch from AXIS Frame to current Frame for
*        output co-ordinates.
*     2006 February 10 (MJC):
*        Further tidying.  Checked for a 1-1 mapping to avoid n-D to
*        one-dimensional transformation, preferring to obain just one vector and
*        duplicating it.
*     2006 February 15 (MJC):
*        Set expansion factors to 1 for unused duimensions up to
*        NDF__MXDIM.
*     10-MAY-2007 (DSB):
*        Re-written to use a specified range of GRID coords, and to be
*        simpler.
*     15-JAN-2020 (DSB):
*        Support huge files.
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
      INTEGER*8 LBND( * )
      INTEGER*8 UBND( * )
      INTEGER MAP
      INTEGER JAXIS
      INTEGER IAXIS

*  Arguments Returned:
      DOUBLE PRECISION CENTRE( * )
      DOUBLE PRECISION WORK( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER*8 EL               ! Element index
      INTEGER*8 NEL              ! No. of elements in array
      INTEGER*8 I                ! Loop counter
      INTEGER NAXPIX             ! Number of pixel axes
      INTEGER NAXWCS             ! Number of WCS axes
      INTEGER*8 PIND( NDF__MXDIM ) ! Pixel indices
      DOUBLE PRECISION PPOS( NDF__MXDIM )! PIXEL position
      DOUBLE PRECISION WPOS( NDF__MXDIM )! WCS position
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the axis.  The supplied axis index refers to the current
*  Frame of the supplied frameSet, and so must be in the range 1 to
*  the number of axes in the current frame.  Note that this may not be
*  the same as the number of pixel axes in the NDF.
      NAXWCS = AST_GETI( MAP, 'Nout', STATUS )
      IF ( IAXIS .LT. 1 .OR. IAXIS .GT. NAXWCS ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'AX', IAXIS )
         CALL MSG_SETI( 'N', NAXWCS )
         CALL ERR_REP( 'KPG1_WCFAX8',
     :     'The chosen WCS axis ^AX is not valid for the NDF, which '/
     :     /'has ^N WCS axes (probable programming error).', STATUS )
         GOTO 999
      END IF

*  Validate the pixel axis.
      NAXPIX = AST_GETI( MAP, 'Nin', STATUS )
      IF ( JAXIS .LT. 1 .OR. JAXIS .GT. NAXPIX ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'AX', JAXIS )
         CALL MSG_SETI( 'N', NAXPIX )
         CALL ERR_REP( 'KPG1_WCFAX8',
     :     'The chosen pixel axis ^AX is not valid for the NDF, which'/
     :     /'has ^N pixel axes (probable programming error).', STATUS )
         GOTO 999
      END IF

*  Store the PIXEL coords at the centre of the NDF. Since each WCS axis is
*  assumed to depend only on the specified pixel axis, the other pixel
*  axes can be set to any arbitrary value. ALso find the total number of
*  elements in the array.
      NEL = 1
      DO I = 1, NAXPIX
         PPOS( I ) = 0.5*( LBND( I ) + UBND( I ) - 1 )
         NEL = NEL*( UBND( I ) - LBND( I ) + 1 )
      END DO

*  Loop round the range of PIXEL values on the pixel axis being used.
      DO I = LBND( JAXIS ), UBND( JAXIS )
         PPOS( JAXIS ) = DBLE( I ) - 0.5D0

*  Transform into current frame coords
         CALL AST_TRANN( MAP, 1, NAXPIX, 1, PPOS, .TRUE., NAXWCS,
     :                   1, WPOS, STATUS )

*  Store the required WCS axis value
         WORK( I - LBND( JAXIS ) + 1 ) = WPOS( IAXIS )
      END DO

*  Now copy the values into the one-dimensional work array into the three-dimensional CENTRE array.
*  First initialise the pixel indices of the first pixel in the N-d array.
      DO I = 1, NAXPIX
         PIND( I ) = LBND( I )
      END DO

*  Loop round all the pixels in the N-d array
      DO EL = 1, NEL

*  Store the co-ordinate value.
         CENTRE( EL ) = WORK( PIND( JAXIS ) - LBND( JAXIS ) + 1 )

*  Store the pixel indices of the next pixel in the N-d array.
         PIND( 1 ) = PIND( 1 ) + 1
         I = 1
         DO WHILE( PIND( I ) .GT. UBND( I ) .AND. I .LT. NAXPIX  )
            PIND( I ) = LBND( I )
            I = I + 1
            PIND( I ) = PIND( I ) + 1
         END DO

      END DO

 999  CONTINUE

      END
