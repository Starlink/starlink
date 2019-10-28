      SUBROUTINE KPG1_ASTRM( IWCS, DEFAX, LBND, UBND, WORK, STATUS )
*+
*  Name:
*     KPG1_ASTRM

*  Purpose:
*     Trims axes from the current Frame of a FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASTRM( IWCS, DEFAX, LBND, UBND, WORK, STATUS )

*  Description:
*     This routine ensures that the number of axes in the current Frame of
*     the supplied FrameSet is the same as the number in the base Frame.
*     If this is not the case on entry, a new Frame with the required number
*     of axes is created and added into the FrameSet and becomes the new
*     current Frame.
*
*     If the original current Frame has too few axes, the new Frame is a
*     copy of the original current frame with extra simple axes added to
*     the end. These extra axes are supplied a value of AST__BAD by the
*     Mapping which connects the original current Frame to the new current
*     Frame.
*
*     If the original current Frame has too many axes, the user is
*     allowed to select a subset of the original axes using the environment
*     parameter USEAXIS (see below). A new Frame is created by picking
*     these selected axes from the original current Frame. This Frame is
*     added into the FrameSet using a Mapping which has a forward
*     transformation which simply drops the values for the unselected axes.
*     The inverse transformation (from new to old Frame) attempts to
*     assign usable values for the dropped axes if possible. This is only
*     possible if the value for a dropped axis can be determined uniquely
*     from the value of one of the selected axes. This may be the case
*     for instance in a situation where (RA,wavelength) axes were
*     selected from the (RA,Dec,Wavelength) axes describing a 2D longslit
*     spectrum. The missing Dec value can probably be determined from the
*     RA value because the relationship between RA and Dec is determined
*     by the position and orientation of the slit on the sky. If it is
*     not possible to determine the value for a dropped axis in this way,
*     then what happens depends on whether or not any Frames can be found
*     in the FrameSet that are Regions having a Domain name of "ROI<n>",
*     where "<n>" is a positive integer. If no such Frame can be found,
*     AST__BAD is supplied for the dropped axis.
*
*     If the supplied current Frame has too many axes, and one or more ROI
*     Regions are found in the FrameSet, then a new Frame is added into
*     the FrameSet for each ROI Region found. These new Frames are all
*     equivalent, containing axes from the supplied current Frame as
*     specified by the USEAXIS parameter, and they are all connected to
*     the supplied current Frame via a PermMap. Each PermMap has a
*     forward transformation that simply drops the unwanted axis values.
*     The inverse transformation of each PermMap supplies suitable values
*     for the unwanted axes. These are determined by transforming the
*     bounding box of the corresponding ROI Region into the original current
*     Frame. The Domain name of the corresponding ROI Region is stored in
*     the Ident attribute of the new Frame, and is also appended to the
*     end of the Frame's Domain. The new Frame corresponding to the lowest
*     numbered ROI Region is left as the current Frame on exit.
*
*     Various environment parameters may be used to obtain options, etc. The
*     names of these parameters are hard-wired into this subroutine in
*     order to ensure conformity between applications.

*  Environment Parameters:
*     USEAXIS = LITERAL (Read)
*        A set of NDIM axes to be selected from the current Frame. Each
*        axis can be specified either by giving its index within the current
*        Frame in the range 1 to the number of axes in the Frame, or by
*        giving its symbol. This parameter is only accessed if the original
*        current Frame in the supplied FrameSet has too many axes. The value
*        should be given as a GRP group expression, with default control
*        characters.

*  Arguments:
*     IWCS = INTEGER (Given)
*        The FrameSet to use. A new current Frame may be added to the
*        FrameSet by this routine.
*     DEFAX( * ) = INTEGER (Given)
*        This array should have one element for each axis in the base
*        Frame of the supplied FrameSet. The i'th value is the index
*        within the original current Frame of the axis which is to be
*        associated with the i'th base Frame axis by default. Only used
*        if no better defaults can be found by splitting the FrameSet
*        Mapping.
*     LBND( * ) = INTEGER (Given)
*        The lower pixel bound on each pixel axis. Array length should be
*        at least equal to the number of base Frame axes in IWCS.
*     UBND( * ) = INTEGER (Given)
*        The upper pixel bound on each pixel axis. Array length should be
*        at least equal to the number of base Frame axes in IWCS.
*     WORK( * ) = INTEGER (Given)
*        Work space. It's length should be at least twice as large as the
*        largest pixel dimension implied by LBND and UBND.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The routine KPG1_ASTRM8 is equivalent to this function but uses
*     INTEGER*8 for the LBND and UBND arguments.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2019 East Asian Observatory
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
*     {enter_new_authors_here}

*  History:
*     7-JUL-2005 (DSB):
*        Original version.
*     2-DEC-2005 (DSB):
*        Added DEFAX to argument list.
*     25-MAY-2006 (DSB):
*        Added support for ROI Regions.
*     30-MAY-2006 (DSB):
*        Move the bulk of the work into ATL_AXTRM.
*     4-OCT-2019 (DSB):
*        Changed to be a thin wrapper round KPG1_ASTRM8.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER IWCS
      INTEGER DEFAX( * )
      INTEGER LBND( * )
      INTEGER UBND( * )
      DOUBLE PRECISION WORK( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      INTEGER*8 LBND8( NDF__MXDIM )
      INTEGER*8 UBND8( NDF__MXDIM )
*.

*  Copy the supplied INTEGER values into local INTEGER*8 arrays.
      DO I = 1, NDF__MXDIM
         LBND8( I ) = LBND( I )
         UBND8( I ) = UBND( I )
      END DO

*  Call the 8-byte version of this routine.
      CALL KPG1_ASTRM8( IWCS, DEFAX, LBND8, UBND8, WORK, STATUS )

      END
