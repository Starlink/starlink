      SUBROUTINE ASTBOX( STATUS )
*+
*  Name:
*     ASTBOX

*  Purpose:
*     Create a Box.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTBOX( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new Box and optionally initialises
*     its attributes.
*
*     The Box class implements a Region which represents a box with sides
*     parallel to the axes of a Frame (i.e. an area which encloses a given
*     range of values on each axis). A Box is similar to an Box, the
*     only real difference being that the Box class allows some axis
*     limits to be unspecified. Note, a Box will only look like a box if
*     the Frame geometry is approximately flat. For instance, a Box centred
*     close to a pole in a SkyFrame will look more like a fan than a box
*     (the Polygon class can be used to create a box-like region close to
*     a pole).

*  Usage:
*     astbox frame form point1 point2 unc options result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. Can be "AST", "XML",
*        "STCS", or any FitsChan encoding such as FITS-WCS. Only used
*        if the output object is written to a text file. An error is
*        reported if the output object cannot be written using the
*        requested format. ["AST"]
*     FRAME = LITERAL (Read)
*        An NDF or text file holding the Frame in which the region is defined.
*        If an NDF is supplied, the current Frame in its WCS FrameSet will be
*        used.
*     FORM = _INTEGER (Read)
*        Indicates how the box is described by the remaining parameters.
*        A value of zero indicates that the box is specified by a centre
*        position and a corner position. A value of one indicates that the
*        box is specified by a two opposite corner positions.
*     POINT1 = GROUP (Read)
*        A comma-separated list of floating point values with one element
*        for each Frame axis (Naxes attribute). If FORM is zero, this list
*        should contain the coordinates at the centre of the box. If FORM
*        is one, it should contain the coordinates at the corner of the box
*        which is diagonally opposite the corner specified by POINT2.
*     POINT2 = GROUP (Read)
*        A comma-separated list of floating point values with one element
*        for each Frame axis (Naxes attribute) containing the coordinates
*        at any corner of the box.
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute
*        assignments to be used for initialising the new Box.
*     RESULT = LITERAL (Read)
*        An text file to receive the new Box.
*     UNC = LITERAL (Read)
*        An optional text file containing an existing Region which
*        specifies the uncertainties associated with each point on the
*        boundary of the Box being created. The uncertainty at any
*        point on the Box is found by shifting the supplied
*        "uncertainty" Region so that it is centred at the point being
*        considered. The area covered by the shifted uncertainty Region
*        then represents the uncertainty in the position. The uncertainty
*        is assumed to be the same for all points.
*
*        If supplied, the uncertainty Region must be either a Box, a Circle
*        or an Ellipse. Alternatively, a null value (!) may be supplied, in
*        which case a default uncertainty is used equivalent to a box
*        1.0E-6 of the size of the bounding box of the Box being created.
*
*        The uncertainty Region has two uses: 1) when the astOverlap
*        function compares two Regions for equality the uncertainty
*        Region is used to determine the tolerance on the comparison, and 2)
*        when a Region is mapped into a different coordinate system and
*        subsequently simplified (using astSimplify), the uncertainties are
*        used to determine if the transformed boundary can be accurately
*        represented by a specific shape of Region.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-MAY-2007 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'CNF_PAR'          ! CNF constants

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL AST_ISAFRAME
      EXTERNAL AST_ISAREGION

*  Local Variables:
      INTEGER FORM
      INTEGER FRAME
      INTEGER IGRP, IPP1, IPP2
      INTEGER NP, NAX
      INTEGER RESULT
      INTEGER UNC

*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get a Frame.
      CALL KPG1_GTOBJ( 'FRAME', 'Frame', AST_ISAFRAME, FRAME,
     :                 STATUS )

*  Get the number of axes in the Frame.
      NAX = AST_GETI( FRAME, 'Naxes', STATUS )

*  Allocate memory for the bounds arrays.
      CALL PSX_CALLOC( NAX, '_DOUBLE', IPP1, STATUS )
      CALL PSX_CALLOC( NAX, '_DOUBLE', IPP2, STATUS )

*  Get the FORM value.
      CALL PAR_GET0I( 'FORM', FORM, STATUS )

*  Get a group holding point1.
      IGRP = GRP__NOID
      CALL KPG1_GTGRP( 'POINT1', IGRP, NP, STATUS )
      IF( NP .NE. NAX .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NP', NP )
         CALL MSG_SETI( 'NAX', NAX )
         CALL ERR_REP( 'ASTBOX_ERR1', ' Number of lower bounds '//
     :                 '(^NP) and Frame axes (^NAX) differ.', STATUS )
      END IF

*  Read the values from the group into the memory.
      CALL ATL1_GTOFL( IGRP, NAX, 1, %VAL( CNF_PVAL( IPP1 ) ),
     :                 STATUS )

*  Delete the group.
      CALL GRP_DELET( IGRP, STATUS )

*  Likewise get point2.
      CALL KPG1_GTGRP( 'POINT2', IGRP, NP, STATUS )
      IF( NP .NE. NAX .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NP', NP )
         CALL MSG_SETI( 'NAX', NAX )
         CALL ERR_REP( 'ASTBOX_ERR1', ' Number of upper bounds '//
     :                 '(^NP) and Frame axes (^NAX) differ.', STATUS )
      END IF
      CALL ATL1_GTOFL( IGRP, NAX, 1, %VAL( CNF_PVAL( IPP2 ) ),
     :                 STATUS )
      CALL GRP_DELET( IGRP, STATUS )

*  Get the uncertainty Region.
      IF( STATUS .NE. SAI__OK ) GO TO 999
      CALL KPG1_GTOBJ( 'UNC', 'Region', AST_ISAREGION, UNC, STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         UNC = AST__NULL
      END IF

*  Create the required Box.
      RESULT = AST_BOX( FRAME, FORM, %VAL( CNF_PVAL( IPP1 ) ),
     :                  %VAL( CNF_PVAL( IPP2 ) ), UNC, ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

*  Free resources.
      CALL PSX_FREE( IPP1, STATUS )
      CALL PSX_FREE( IPP2, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTBOX_ERR', 'Error creating a new '//
     :                 'Box.', STATUS )
      END IF

      END
