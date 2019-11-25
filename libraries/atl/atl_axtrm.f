      SUBROUTINE ATL_AXTRM( IWCS, AXES, LBND, UBND, WORK, STATUS )
*+
*  Name:
*     ATL_AXTRM

*  Purpose:
*     Trim axes from the current Frame of a FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_AXTRM( IWCS, AXES, LBND, UBND, WORK, STATUS )

*  Description:
*     This routine ensures that the number of axes in the current
*     Frame of the supplied FrameSet is the same as the number in
*     the base Frame. If this is not the case on entry, one or more
*     new Frames with the required number of axes are created and
*     added into the FrameSet, one of which becomes the new current
*     Frame. The only case in which more than one new Frame is added
*     is if the current Frame has too many axes, and the FrameSet
*     contains more than one "ROI" Frame (that is, Frames which are
*     Regions and which have a Domain name beginning with "ROI"). If
*     the FrameSet contains zero or one ROI Frame, then only a single
*     new Frame is added into the FrameSet.
*
*     If the original current Frame has too few axes, the new Frame
*     is a copy of the original current Frame with extra simple axes
*     added to the end. These extra axes are supplied a value of
*     AST__BAD by the Mapping which connects the original current
*     Frame to the new current Frame.
*
*     If the original current Frame has too many axes, one or more
*     new Frames will be created by picking the specified axes from
*     the original current Frame. Each of these Frames is added into
*     the FrameSet using a Mapping which has a forward transformation
*     which simply drops the values for the unselected axes. The
*     inverse transformation (from new to old Frame) attempts to
*     assign usable values for the dropped axes if possible. If this
*     is not possible, then AST__BAD is assigned to the dropped axes.
*
*     Two methods are used for finding suitable values to assign to
*     dropped axes. The first is only possible if the value for a
*     dropped axis can be determined uniquely from the value of one
*     of the retained axes. This may be the case for instance in a
*     situation where (RA,wavelength) axes were selected from the
*     (RA,Dec,Wavelength) axes describing a 2D longslit spectrum. The
*     missing Dec value can probably be determined from the RA value
*     because the relationship between RA and Dec is determined by the
*     position and orientation of the slit on the sky.
*
*     If it is not possible to determine the value for a dropped axis
*     in this way, then a search is made for Frames that are Regions
*     having a Domain name beginning with "ROI". If any are found,
*     then a new Frame is added into the FrameSet for each ROI Region
*     found, connected to the original current Frame via a PermMap.
*     The values to be assigned to the dropped axes by the inverse
*     PermMap transformation are determined by transforming the
*     bounding box of the corresponding ROI Region into the original
*     current Frame. The assigned axis values are the mean values of
*     the transformed bounding box on each dropped axis. The Domain
*     name of the corresponding ROI Region is stored in the Ident
*     attribute of each new Frame so that later code can identify the
*     corresponding ROI Region, and is also appended to the end of the
*     Frame's Domain. The new Frame corresponding to the first ROI
*     Region found in the FrameSet is left as the current Frame on exit.

*  Arguments:
*     IWCS = INTEGER (Given)
*        The FrameSet to use. A new current Frame may be added to the
*        FrameSet by this routine.
*     AXES( * ) = INTEGER (Given)
*        The one-based indices of the axes to be retained in the event
*        of there being too many axes in the original current Frame
*        of IWCS. The number of values in the array should be equal to
*        the number of axes in the base Frame of IWCS (i.e the number
*        of pixel axes).
*     LBND( * ) = INTEGER (Given)
*        The lower pixel index bounds of the NDF from which the
*        FrameSet was obtained. The number of values in the array
*        should be equal to the number of axes in the base Frame of
*        IWCS (i.e the number of pixel axes).
*     UBND( * ) = INTEGER (Given)
*        The upper pixel index bounds of the NDF from which the
*        FrameSet was obtained. The number of values in the array
*        should be equal to the number of axes in the base Frame of
*        IWCS (i.e the number of pixel axes).
*     WORK( * ) = INTEGER (Given)
*        Work space. It's length should be at least twice as large as
*        the largest pixel dimension implied by LBND and UBND.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The routine ATL_AXTRM8 is equivalent to this function but uses
*     INTEGER*8 for the LBND and UBND arguments.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-MAY-2006 (DSB):
*        Original version, derived from KPG1_ASTRM.
*     26-JUN-2006 (DSB):
*        Shrink the ROI bounds before transforming it into current Frame.
*     29-NOV-2006 (DSB):
*        Attempt to split the existing Mapping first, then only use the
*        old method of appending a permmap in series with the original
*        Mapping if the mapping cannot be split.
*     29-OCT-2008 (DSB):
*        Do not supply a Frame to ATL_MKLUT so that the tabulated are
*        not normalised. Normalisation could prevent LutMaps being
*        created for longslit images if the ra value passes through
*        zero (normalisation introduces a discontinutity by adding 2.PI
*        to negative RAs).
*     4-OCT-2019 (DSB):
*        Changed to be a thin wrapper round ATL_AXTRM8.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ATL_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER IWCS
      INTEGER AXES( * )
      INTEGER LBND( * )
      INTEGER UBND( * )
      DOUBLE PRECISION WORK( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      INTEGER*8 LBND8( ATL__MXDIM )
      INTEGER*8 UBND8( ATL__MXDIM )
*.

*  Copy the supplied INTEGER values into local INTEGER*8 arrays.
      DO I = 1, ATL__MXDIM
         LBND8( I ) = LBND( I )
         UBND8( I ) = UBND( I )
      END DO

*  Call the 8-byte version of this routine.
      CALL ATL_AXTRM8( IWCS, AXES, LBND8, UBND8, WORK, STATUS )

      END
