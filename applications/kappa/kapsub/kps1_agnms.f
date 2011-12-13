      SUBROUTINE KPS1_AGNMS( ARDDEF, MXPOL, NPTS, STATUS )
*+
*  Name:
*     KPS1_AGNMS

*  Purpose:
*     Displays an informational message in ARDGEN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_AGNMS( ARDDEF, MXPOL, NPTS, STATUS )

*  Description:
*     This routine displays a message telling the user how to define a
*     region and sets the value of NPTS accordingly.

*  Arguments:
*     ARDDEF = CHARACTER * ( * ) (Given)
*        User-selected keyword.
*     MXPOL = INTEGER (Given)
*        Maximum number of polygon vertices.
*     NPTS = INTEGER (Returned)
*        Number of points required.  Set to a negative value if the
*        number of points is variable (for example, for a polygon
*        region).
*     STATUS = INTEGER (Given and Returned)
*        The status on entry to this subroutine.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     GJP: Grant Privett (STARLINK)
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-NOV-1994 (GJP)
*        Original version
*     11-NOV-1994 (GJP)
*        Added ARD further keywords.
*     5-DEC-1994 (DSB)
*        Tidied up comments/prologues/format.  Change name from
*        ARDG1_MESS to KPS1_AGNMS. Alter format of displayed text.  Add
*        MXPOL argument.  WHOLE added.  Return NPTS negative for
*        polygon, etc.  Compare ARDDEF with full region shape, rather
*        than the minimum unambiguous abbreviation.  Re-structured.
*        Error report added for unrecognised shapes.
*     1995 March 16 (MJC):
*        Corrected prologue identation and typo's.  Used modern style
*        variable declarations, and other stylistic changes for
*        consistency within KAPPA.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions

*  Arguments Given:
      CHARACTER * ( * ) ARDDEF   ! Input ARD keyword
      INTEGER MXPOL

*  Arguments Returned:
      INTEGER NPTS               ! Number of positions available

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Circle.
      IF ( ARDDEF .EQ. 'CIRCLE' ) THEN
         NPTS = 2
         CALL MSG_OUT( 'KPS1_AGNMS_MSG1', 'Region type is '/
     :                 /'"CIRCLE". Identify the centre then give a '/
     :                 /'point on the circumference.', STATUS )

*  Box.
      ELSE IF ( ARDDEF .EQ. 'BOX' ) THEN
         NPTS = 2
         CALL MSG_OUT( 'KPS1_AGNMS_MSG2', 'Region type is "BOX". '/
     :                 /'Identify the centre and then give a corner.',
     :                 STATUS )

*  Point.
      ELSE IF ( ARDDEF .EQ. 'POINT' ) THEN
         NPTS = 1
         CALL MSG_OUT( 'KPS1_AGNMS_MSG3', 'Region type is "POINT". '/
     :                 /'Identify the point.', STATUS )

*  Frame.
      ELSE IF ( ARDDEF .EQ. 'FRAME' ) THEN
         NPTS = 1
         CALL MSG_OUT( 'KPS1_AGNMS_MSG4', 'Region type is "FRAME". '/
     :                 /'Identify any point on the frame interior '/
     :                 /'border.', STATUS )

*  Rotbox.
      ELSE IF ( ARDDEF .EQ. 'ROTBOX' ) THEN
         NPTS = 3
         CALL MSG_OUT( 'KPS1_AGNMS_MSG5', 'Region type is "ROTBOX". '/
     :                 /'Identify the two end points of any edge and '/
     :                 /'then give a point on the opposite edge.',
     :                 STATUS )

*  Polygon.
      ELSE IF ( ARDDEF .EQ. 'POLYGON' ) THEN
         NPTS = -1
         CALL MSG_SETI( 'MX', MXPOL )
         CALL MSG_OUT( 'KPS1_AGNMS_MSG6', 'Region type is "POLYGON". '/
     :                 /'Identify up to ^MX vertices.', STATUS )

*  Row.
      ELSE IF ( ARDDEF .EQ. 'ROW' ) THEN
         NPTS = 1
         CALL MSG_OUT( 'KPS1_AGNMS_MSG7', 'Region type is "ROW". '/
     :                 /'Identify a point on the row.', STATUS )

*  Rectangle.
      ELSE IF ( ARDDEF .EQ. 'RECTANGLE' ) THEN
         NPTS = 2
         CALL MSG_OUT( 'KPS1_AGNMS_MSG8', 'Region type is '/
     :                 /'"RECTANGLE". Identify two opposite corners.',
     :                 STATUS )

*  Column.
      ELSE IF ( ARDDEF .EQ. 'COLUMN' ) THEN
         NPTS = 1
         CALL MSG_OUT( 'KPS1_AGNMS_MSG9', 'Region type is "COLUMN". '/
     :                 /'Identify a point on the column.', STATUS )

*  Ellipse.
      ELSE IF ( ARDDEF .EQ. 'ELLIPSE' ) THEN
         NPTS = 3
         CALL MSG_OUT( 'KPS1_AGNMS_MSG10', 'Region type is "ELLIPSE". '/
     :                 /'Identify the centre, then one end of the '/
     :                 /'semi-major axis, and finally one other point '/
     :                 /'on the ellipse.', STATUS )

*  Line.
      ELSE IF ( ARDDEF .EQ. 'LINE' ) THEN
         NPTS = 2
         CALL MSG_OUT( 'KPS1_AGNMS_MSG11', 'Region type is "LINE". '/
     :                 /'Identify the two ends of the line.', STATUS )

*  Whole
      ELSE IF ( ARDDEF .EQ. 'WHOLE' ) THEN
         NPTS = 0
         CALL MSG_OUT( 'KPS1_AGNMS_MSG11', 'Region type is "WHOLE". '/
     :                 /'No points need be identified.', STATUS )

*  Report an error for anything else.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'SH', ARDDEF )
         CALL ERR_REP( 'KPS1_AGNMS_ERR1', 'Subroutine KPS1_AGNMS does '/
     :                 /'not yet support shape "^SH" (programming '/
     :                 /'error).', STATUS )
      END IF

      END
