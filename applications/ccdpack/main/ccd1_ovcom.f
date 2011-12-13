      SUBROUTINE CCD1_OVCOM( X1, Y1, N1, X2, Y2, N2, NSELEC, XOFF, YOFF,
     :                       ERROR, COMFAC, STATUS )
*+
*  Name:
*     CCD1_OVCOM

*  Purpose:
*     Determines the completeness of points selected from an overlap
*     region.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_OVCOM( X1, Y1, N1, X2, Y2, N2, NSELEC, XOFF, YOFF,
*                      ERROR, COMFAC, STATUS )

*  Description:
*     This routine determines a completeness factor for a subset of
*     points selected (and paired) from two lists of positions. The
*     completeness is intended to test how many positions within the
*     overlap region have been ignored (i.e. failed any selection
*     tests) for whatever reason. The basic completeness function is
*     simply the ratio of the number of selected positions over the
*     number of positions which are in the overlap region and which
*     could be expected to be selected (i.e. the minimum of the count
*     from the other two lists in this position).

*  Arguments:
*     X1( N1 ) = DOUBLE PRECISION (Given)
*        First set of initial X positions.
*     Y1( N1 ) = DOUBLE PRECISION (Given)
*        First set of initial Y positions.
*     N1 = INTEGER (Given)
*        Number of positions in first set.
*     X2( N2 ) = DOUBLE PRECISION (Given)
*        Second set of initial X positions.
*     Y2( N2 ) = DOUBLE PRECISION (Given)
*        Second set of initial Y positions.
*     N2 = INTEGER (Given)
*        Number of positions in second set.
*     NSELEC = INTEGER (Given)
*        The number of positions selected from the overlap between the
*        first and second set of positions (using the offsets from the
*        first set coordinates XOFF and YOFF).
*     XOFF = DOUBLE PRECISION (Given)
*        The X offset in coordinates from positions in the first lists
*        to positions in the second list.
*     YOFF = DOUBLE PRECISION (Given)
*        The Y offset in coordinates from positions in the first lists
*        to positions in the second list.
*     ERROR = DOUBLE PRECISION (Given)
*        The error in position used when matching positions. The overlap
*        region is expanded to allow for this.
*     COMFAC = DOUBLE PRECISION (Returned)
*        The completeness factor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-APR-1993 (PDRAPER):
*        Original version.
*     12-MAY-1993 (PDRAPER):
*        Removed intensity stuff. Estimate is fair enough without this.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      INTEGER N1
      DOUBLE PRECISION X1( N1 )
      DOUBLE PRECISION Y1( N1 )
      INTEGER N2
      DOUBLE PRECISION X2( N2 )
      DOUBLE PRECISION Y2( N2 )
      INTEGER NSELEC
      DOUBLE PRECISION XOFF
      DOUBLE PRECISION YOFF
      DOUBLE PRECISION ERROR

*  Arguments Returned:
      DOUBLE PRECISION COMFAC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION MAXX1
      DOUBLE PRECISION MAXX2
      DOUBLE PRECISION MAXXO1
      DOUBLE PRECISION MAXXO2
      DOUBLE PRECISION MAXY1
      DOUBLE PRECISION MAXY2
      DOUBLE PRECISION MAXYO1
      DOUBLE PRECISION MAXYO2
      DOUBLE PRECISION MINX1
      DOUBLE PRECISION MINX2
      DOUBLE PRECISION MINXO1
      DOUBLE PRECISION MINXO2
      DOUBLE PRECISION MINY1
      DOUBLE PRECISION MINY2
      DOUBLE PRECISION MINYO1
      DOUBLE PRECISION MINYO2
      INTEGER I
      INTEGER NOVER
      INTEGER NUMOV1
      INTEGER NUMOV2

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the minimum and maximum extents of the initial positions.
*  First list of positions.
      MAXX1 = VAL__MIND
      MAXY1 = VAL__MIND
      MINX1 = VAL__MAXD
      MINY1 = VAL__MAXD
      DO 1 I = 1, N1
         MAXX1 = MAX( X1( I ), MAXX1 )
         MAXY1 = MAX( Y1( I ), MAXY1 )
         MINX1 = MIN( X1( I ), MINX1 )
         MINY1 = MIN( Y1( I ), MINY1 )
 1    CONTINUE

*  Second list of positions.
      MAXX2 = VAL__MIND
      MAXY2 = VAL__MIND
      MINX2 = VAL__MAXD
      MINY2 = VAL__MAXD
      DO 2 I = 1, N2
         MAXX2 = MAX( X2( I ), MAXX2 )
         MAXY2 = MAX( Y2( I ), MAXY2 )
         MINX2 = MIN( X2( I ), MINX2 )
         MINY2 = MIN( Y2( I ), MINY2 )
 2    CONTINUE

*  And find the overlap region (using the offsets)
*  First the the coordinates of the first list.
      MINXO1 = MAX( MINX1, MINX2 + XOFF ) - ERROR
      MINYO1 = MAX( MINY1, MINY2 + YOFF ) - ERROR
      MAXXO1 = MIN( MAXX1, MAXX2 + XOFF ) + ERROR
      MAXYO1 = MIN( MAXY1, MAXY2 + YOFF ) + ERROR

*  Now in the coordinates of the second list.
      MINXO2 = MAX( MINX1 - XOFF, MINX2 ) - ERROR
      MINYO2 = MAX( MINY1 - YOFF, MINY2 ) - ERROR
      MAXXO2 = MIN( MAXX1 - XOFF, MAXX2 ) + ERROR
      MAXYO2 = MIN( MAXY1 - YOFF, MAXY2 ) + ERROR

*  Just count the intensities in the overlap.
      NUMOV1 = 0
      DO 3 I = 1, N1
         IF ( X1( I ) .GE. MINXO1 .AND. X1( I ) .LE. MAXXO1 .AND.
     :        Y1( I ) .GE. MINYO1 .AND. Y1( I ) .LE. MAXYO1 ) THEN
            NUMOV1 = NUMOV1 + 1
         END IF
 3    CONTINUE

*  Same for second list
      NUMOV2 = 0
      DO 4 I = 1, N2
         IF ( X2( I ) .GE. MINXO2 .AND. X2( I ) .LE. MAXXO2 .AND.
     :        Y2( I ) .GE. MINYO2 .AND. Y2( I ) .LE. MAXYO2 ) THEN
            NUMOV2 = NUMOV2 + 1
         END IF
 4    CONTINUE

*  And select the appropriate one.
      NOVER = MIN( NUMOV2, NUMOV1 )

*  And get completeness factor.
      IF ( NOVER .GT. 0 ) THEN
         COMFAC = DBLE( NSELEC ) / DBLE ( NOVER )
      ELSE
         COMFAC = 0.0D0
      END IF

      END
* $Id$
