      SUBROUTINE ASTPOLYGON( STATUS )
*+
*  Name:
*     ASTPOLYGON

*  Purpose:
*     Create a Polygon.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTPOLYGON( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new Polygon and optionally initialises
*     its attributes. The Polygon class implements a polygonal area, defined
*     by a collection of vertices, within a 2-dimensional Frame. The vertices
*     are connected together by geodesic curves within the encapsulated Frame.
*     For instance, if the encapsulated Frame is a simple Frame then the
*     geodesics will be straight lines, but if the Frame is a SkyFrame then
*     the geodesics will be great circles.

*  Usage:
*     astpolygon frame xin yin unc options result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     FRAME = LITERAL (Read)
*        An NDF or text file holding the Frame in which the region is defined.
*        It must have exactly 2 axes. If an NDF is supplied, the current Frame
*        in its WCS FrameSet will be used.
*     XIN = GROUP (Read)
*        A comma-separated list of floating point values to be used as the X
*        axis value of the vertices. A text file may be specified by
*        preceeding the name of the file with an up arrow character "^". If
*        the supplied value ends with a minus sign, the user is re-prompted
*        for additional values.
*     YIN = GROUP (Read)
*        A comma-separated list of floating point values to be used as the Y
*        axis value of the vertices. A text file may be specified by
*        preceeding the name of the file with an up arrow character "^". If
*        the supplied value ends with a minus sign, the user is re-prompted
*        for additional values.
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute
*        assignments to be used for initialising the new Polygon.
*     RESULT = LITERAL (Read)
*        An text file to receive the new Polygon.
*     UNC = LITERAL (Read)
*        An optional text file containing an existing Region which
*        specifies the uncertainties associated with each point on the
*        boundary of the Polygon being created. The uncertainty at any
*        point on the Polygon is found by shifting the supplied
*        "uncertainty" Region so that it is centred at the point being
*        considered. The area covered by the shifted uncertainty Region
*        then represents the uncertainty in the position. The uncertainty
*        is assumed to be the same for all points.
*
*        If supplied, the uncertainty Region must be either a Box, a Circle
*        or an Ellipse. Alternatively, a null value (!) may be supplied, in
*        which case a default uncertainty is used equivalent to a box
*        1.0E-6 of the size of the bounding box of the Polygon being created.
*
*        The uncertainty Region has two uses: 1) when the astOverlap
*        function compares two Regions for equality the uncertainty
*        Region is used to determine the tolerance on the comparison, and 2)
*        when a Region is mapped into a different coordinate system and
*        subsequently simplified (using astSimplify), the uncertainties are
*        used to determine if the transformed boundary can be accurately
*        represented by a specific shape of Region.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     1-FEB-2005 (DSB):
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
      INTEGER RESULT
      INTEGER FRAME
      INTEGER UNC
      INTEGER IGRP1, IGRP2, IPIN
      INTEGER NP, NP2
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get a Frame.
      CALL KPG1_GTOBJ( 'FRAME', 'Frame', AST_ISAFRAME, FRAME,
     :                 STATUS )

*  Get two groups holding the input axis values.
      IGRP1 = GRP__NOID
      CALL KPG1_GTGRP( 'XIN', IGRP1, NP, STATUS )
      IGRP2 = GRP__NOID
      CALL KPG1_GTGRP( 'YIN', IGRP2, NP2, STATUS )

      IF( NP .NE. NP2 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NX', NP )
         CALL MSG_SETI( 'NY', NP2 )
         CALL ERR_REP( 'ASTPOLYGON_ERR1', ' Number of X positions'//
     :                 ' (^NX) and Y positions (^NY) differ.', STATUS )
      END IF

*  Allocate memory for the input positions.
      CALL PSX_CALLOC( 2*NP, '_DOUBLE', IPIN, STATUS )

*  Read the values from the group into the memory.
      CALL ATL1_GTOFL( IGRP1, 2*NP, 1, %VAL( CNF_PVAL( IPIN ) ),
     :                 STATUS )
      CALL ATL1_GTOFL( IGRP2, 2*NP, NP + 1, %VAL( CNF_PVAL( IPIN ) ),
     :                 STATUS )

*  Get the uncertainty Region.
      IF( STATUS .NE. SAI__OK ) GO TO 999
      CALL KPG1_GTOBJ( 'UNC', 'Region', AST_ISAREGION, UNC, STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         UNC = AST__NULL
      END IF

*  Create the required Polygon.
      RESULT = AST_POLYGON( FRAME, NP, NP, %VAL( CNF_PVAL( IPIN ) ),
     :                      UNC, ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

*  Free resources.
      CALL PSX_FREE( IPIN, STATUS )
      CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTPOLYGON_ERR', 'Error creating a new '//
     :                 'Polygon.', STATUS )
      END IF

      END
