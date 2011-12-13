      SUBROUTINE RTD1_PRSH2( SHAPE, NEWGRP, STATUS )
*+
*  Name:
*     RTD1_PRSH2

*  Purpose:
*     Processes a shape into a 2D ARD description.

*  Language:
*     Starlink Fortran-77

*  Invocation:
*     CALL RTD1_PRSH2( SHAPE, NEWGRP, STATUS )

*  Description:
*     This routine acts on the value of SHAPE which should be a known
*     ARD keyword and prompts the user for any extra information
*     required to describe the shape (such as its centre, radius etc.)
*     The result is returned as an ARD description.

*  Arguments:
*     SHAPE = CHARACTER * ( * ) (Given)
*        The ARD shape to use in regions. One of
*           BOX, CIRCLE, ELLIPSE, POLYGON, RECT, ROTBOX
*     NEWGRP = INTEGER (Returned)
*        ARD description for region of image.
*     STATUS = INTEGER ({status_access_mode})
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  Authors:
*     PWD: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     19-MAR-1996 (PWD):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! SAE constants
      INCLUDE 'GRP_PAR'         ! GRP constants

*  Arguments Given:
      CHARACTER * ( * ) SHAPE

*  Arguments Returned:
      INTEGER NEWGRP

*  Status:
      INTEGER STATUS            ! Global status

*  Local constants:
      INTEGER MAXVER            ! Maximum number of polygon
      PARAMETER ( MAXVER = 200 ) ! positions (X and Y)

*  Local Variables:
      REAL XCEN                 ! X centre
      REAL YCEN                 ! X centre
      REAL SIDE1                ! Length of first side
      REAL SIDE2                ! Length of second side
      REAL RADIUS               ! Radius of circle
      REAL SMAJOR               ! Semimajor axes
      REAL SMINOR               ! Semimajor axes
      REAL ANGLE                ! Position angel
      REAL VERTEX( MAXVER )     ! Polygon points
      REAL XRANGE( 2 )          ! Range of object in X
      REAL YRANGE( 2 )          ! Range of object in X
      INTEGER I                 ! Loop variable
      INTEGER NVERT             ! Number of polygon positions
      INTEGER IAT               ! Insertion position in string
      CHARACTER * ( GRP__SZNAM ) BUFFER ! ARD description
      INTEGER BUFLEN            ! Used length of BUFFER
      LOGICAL FLAG              ! Unused

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise group to make sure of creation.
      NEWGRP = GRP__NOID

*  Check SHAPE and get extra information that is required.
*======================================================================
      IF ( SHAPE .EQ. 'BOX' ) THEN

*  Requires a center and two lengths.
         CALL PAR_GET0R( 'XCENTRE', XCEN, STATUS )
         CALL PAR_GET0R( 'YCENTRE', YCEN, STATUS )
         CALL PAR_GET0R( 'SIDE1', SIDE1, STATUS )
         CALL PAR_GET0R( 'SIDE2', SIDE2, STATUS )

*  Create the ARD description of this object.
         CALL MSG_SETR( 'XCEN', XCEN )
         CALL MSG_SETR( 'YCEN', YCEN )
         CALL MSG_SETR( 'SIDE1', SIDE1)
         CALL MSG_SETR( 'SIDE2', SIDE2)
         CALL MSG_LOAD( ' ', 'BOX( ^XCEN, ^YCEN, ^SIDE1, ^SIDE2 )',
     :                  BUFFER, BUFLEN, STATUS )
         BUFLEN = MAX( 1, BUFLEN )
         CALL ARD_GRPEX( BUFFER( :BUFLEN ), GRP__NOID, NEWGRP, FLAG,
     :                   STATUS )

*======================================================================
      ELSE IF ( SHAPE .EQ. 'CIRCLE' ) THEN

*  Requires a center and radius.
         CALL PAR_GET0R( 'XCENTRE', XCEN, STATUS )
         CALL PAR_GET0R( 'YCENTRE', YCEN, STATUS )
         CALL PAR_GET0R( 'RADIUS', RADIUS, STATUS )

*  Create the ARD description of this object.
         CALL MSG_SETR( 'XCEN', XCEN )
         CALL MSG_SETR( 'YCEN', YCEN )
         CALL MSG_SETR( 'RADIUS', RADIUS )
         CALL MSG_LOAD( ' ', 'CIRCLE( ^XCEN, ^YCEN, ^RADIUS )',
     :                  BUFFER, BUFLEN, STATUS )
         BUFLEN = MAX( 1, BUFLEN )
         CALL ARD_GRPEX( BUFFER( :BUFLEN ), GRP__NOID, NEWGRP, FLAG,
     :                   STATUS )

*======================================================================
      ELSE IF ( SHAPE .EQ. 'ELLIPSE' ) THEN

*  Requires a center and semimajor and minor axes and an angle.
         CALL PAR_GET0R( 'XCENTRE', XCEN, STATUS )
         CALL PAR_GET0R( 'YCENTRE', YCEN, STATUS )
         CALL PAR_GET0R( 'SEMIMAJOR', SMAJOR, STATUS )
         CALL PAR_GET0R( 'SEMIMINOR', SMINOR, STATUS )
         CALL PAR_GET0R( 'ANGLE', ANGLE, STATUS )

*  Create the ARD description of this object.
         CALL MSG_SETR( 'XCEN', XCEN )
         CALL MSG_SETR( 'YCEN', YCEN )
         CALL MSG_SETR( 'SMAJOR', SMAJOR )
         CALL MSG_SETR( 'SMINOR', SMINOR )
         CALL MSG_SETR( 'ANGLE', ANGLE )
         CALL MSG_LOAD( ' ',
     :  'ELLIPSE( ^XCEN, ^YCEN, ^SMAJOR, ^SMINOR, ^ANGLE )',
     :                  BUFFER, BUFLEN, STATUS )
         BUFLEN = MAX( 1, BUFLEN )
         CALL ARD_GRPEX( BUFFER( :BUFLEN ), GRP__NOID, NEWGRP, FLAG,
     :                   STATUS )

*======================================================================
      ELSE IF ( SHAPE .EQ. 'POLYGON' ) THEN

*  Requires a list of positions. Just get these as a vector.
         CALL PAR_GET1R( 'VERTICES', MAXVER, VERTEX, NVERT, STATUS )

*  Make sure we have even number of vertices.
         NVERT = ( NVERT / 2 ) * 2

*  Now create ARD description of unscaled object.
         CALL ARD_GRPEX( 'POLYGON(', GRP__NOID, NEWGRP, FLAG, STATUS )
         DO 2 I = 1, NVERT, 2
            IAT = 1
            BUFFER = ' '
            CALL CHR_PUTR( VERTEX( I ), BUFFER, IAT )
            IAT = IAT + 1
            BUFFER ( IAT : IAT ) = ','
            IAT = IAT + 1
            CALL CHR_PUTR( VERTEX( I + 1 ), BUFFER, IAT )
            CALL ARD_GRPEX( BUFFER( :IAT ), GRP__NOID, NEWGRP, FLAG,
     :                      STATUS )
 2       CONTINUE
         CALL ARD_GRPEX( ')', GRP__NOID, NEWGRP, FLAG, STATUS )

*======================================================================
      ELSE IF ( SHAPE .EQ. 'RECT' ) THEN

*  Requires a pair of diagonally opposite corners. Get these as
*  a range in X and Y.
         CALL PAR_EXACR( 'XRANGE', 2, XRANGE, STATUS )
         CALL PAR_EXACR( 'YRANGE', 2, YRANGE, STATUS )

*  Create ARD description of object.
         CALL MSG_SETR( 'XRANGE1', XRANGE( 1 ) )
         CALL MSG_SETR( 'XRANGE2', XRANGE( 2 ) )
         CALL MSG_SETR( 'YRANGE1', YRANGE( 1 ) )
         CALL MSG_SETR( 'YRANGE2', YRANGE( 2 ) )
         CALL MSG_LOAD( ' ',
     :  'RECT( ^XRANGE1, ^YRANGE1, ^XRANGE2, ^YRANGE2 )',
     :                  BUFFER, BUFLEN, STATUS )
         BUFLEN = MAX( 1, BUFLEN )
         CALL ARD_GRPEX( BUFFER( :BUFLEN ), GRP__NOID, NEWGRP, FLAG,
     :                   STATUS )

*======================================================================
      ELSE IF ( SHAPE .EQ. 'ROTBOX' ) THEN

*  Needs a centre, the lengths of the two sides and an angle.
         CALL PAR_GET0R( 'XCENTRE', XCEN, STATUS )
         CALL PAR_GET0R( 'YCENTRE', YCEN, STATUS )
         CALL PAR_GET0R( 'SIDE1', SIDE1, STATUS )
         CALL PAR_GET0R( 'SIDE2', SIDE2, STATUS )
         CALL PAR_GET0R( 'ANGLE', ANGLE, STATUS )

*  Create the ARD description of this object.
         CALL MSG_SETR( 'XCEN', XCEN )
         CALL MSG_SETR( 'YCEN', YCEN )
         CALL MSG_SETR( 'SIDE1', SIDE1 )
         CALL MSG_SETR( 'SIDE2', SIDE2 )
         CALL MSG_SETR( 'ANGLE', ANGLE )
         CALL MSG_LOAD( ' ',
     :        'ROTBOX( ^XCEN, ^YCEN, ^SIDE1, ^SIDE2, ^ANGLE )',
     :                  BUFFER, BUFLEN, STATUS )
         BUFLEN = MAX( 1, BUFLEN )
         CALL ARD_GRPEX( BUFFER( :BUFLEN ), GRP__NOID, NEWGRP, FLAG,
     :                   STATUS )

      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'RTD1_PRSH2', 'Unknown shape given: ^SHAPE'//
     :                 ' - possible programming error', STATUS )
      END IF
      END
