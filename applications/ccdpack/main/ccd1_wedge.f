      SUBROUTINE CCD1_WEDGE( IMETH, NEL, BOUNDS, NENT, MAXWEI, W,
     :                       STATUS )
*+
*  Name:
*     CCD1_WEDGE

*  Purpose:
*     To produce line of weights, using a set of column or row
*     pairs to define the extent of the regions to produce weights for.
*     The weights are such that they are maximum in the centre of a
*     line of data between the given columns or rows.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_WEDGE( IMETH, NEL, BOUNDS, NENT, MAXWEI, W, STATUS )

*  Description:
*     The range of the weights line is scanned once. Array elements
*     lying within the bounds specified in the BOUNDS array are given
*     a weight using the basis of the extent of the present section.
*     The weight can be linear, exponential or constant, so
*     weight = 1-2*abs(distance from centre of section)/range
*     or
*     weight = exp( -abs(distance from centre of section).
*     or
*     weight = const
*     The sum of weights for each section of a line has the same
*     total weight. Any number of bounds ranges may be specified. Note
*     that all of the output array values except those within the given
*     columns or rows are left at their input value.

*  Arguments:
*     IMETH = INTEGER (Given)
*        The weighting method, can have the values
*           0: constant
*           1: linear weighting
*           2: exponential weighting.
*     NEL = INTEGER (Given)
*        Number of elements in output weights array.
*     BOUNDS( NENT ) = INTEGER (Given)
*        The array specifying the ranges of columns or rows to be
*        weighted.  BOUNDS(2*I -1) and BOUNDS(2*I) contain the first
*        and last columns or rows to be precessed in range I.
*     NENT = INTEGER (Given)
*        Number of entries in BOUNDS.
*     MAXWEI = DOUBLE PRECISION (Given)
*        The maximum sum of weights in each line of data in a section.
*        (note that this is used to balance the weights between edge
*        areas, a larger central weight, or larger sumof weights
*        in one strip unfairly biases sloped fits).
*     W( NEL ) = DOUBLE PRECISION (Given)
*        The output array which contains the weights for the data
*        within the given columns.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine is intended for use in weighting bias regions in
*     CCD frames.
*     - The weights are produced in double precision.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
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
*     15-APR-1991 (PDRAPER):
*        Original Version.
*     28-OCT-1991 (PDRAPER):
*        Changed to produce a single line of weights to save workspace.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER IMETH
      INTEGER NEL
      INTEGER NENT
      INTEGER BOUNDS( NENT )
      DOUBLE PRECISION MAXWEI

*  Arguments Returned:
      DOUBLE PRECISION W( NEL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, K               ! Loop varibles
      DOUBLE PRECISION  SBOUND   ! Start of the bounds
      DOUBLE PRECISION  EBOUND   ! End of the bounds
      DOUBLE PRECISION  CENTRE   ! Centre of the bounds
      DOUBLE PRECISION  RANGE    ! Range of section
      DOUBLE PRECISION  SUMWEI   ! Sum of weights for each line section

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Scan through the column ranges.
      DO 2 K = 2, NENT, 2

*  Find the value of the central pixel, for this section.
         SBOUND = DBLE( BOUNDS( K - 1 ) )
         EBOUND = DBLE( BOUNDS( K ) )
         RANGE = EBOUND - SBOUND
         CENTRE = SBOUND + RANGE / 2.0D0
         IF ( RANGE .EQ. 0.0D0 ) THEN

*  Special case, one pixel width strip.
            SUMWEI = 1.0D0
            W( BOUNDS( K ) ) = 1.0D0
         ELSE IF ( RANGE .EQ. 1.0D0 ) THEN

*  Special case, just two values for strip. Given equal weight
*  to these.
            W( BOUNDS( K     ) ) = 0.5D0
            W( BOUNDS( K - 1 ) ) = 0.5D0
            SUMWEI = 1.0D0
         ELSE

*  Scan through the columns, setting the weights. Keep a sum of weights
*  and correct so that each column section has equal weight. Equal to
*  maxwei
            SUMWEI = 0.0D0
            DO 3 I = MAX(   1, BOUNDS( K - 1 ) ),
     :               MIN( NEL, BOUNDS( K     ) )
               W( I ) = ABS( DBLE( I ) - CENTRE )

*  If the weighting fall off is exponential then.
               IF ( IMETH .EQ. 2 ) THEN
                  W( I ) = EXP( - W( I ) )
               ELSE IF ( IMETH .EQ. 1 ) THEN

*  Else linear weighting.
                  W( I ) = 1.0D0 - 2.0D0 * W( I ) / RANGE
               ELSE

*  Constant weight.
                  W( I ) = 1.0D0
               END IF
               SUMWEI = SUMWEI + W( I )
 3          CONTINUE
         END IF

*  Weight correction factor.
         SUMWEI = MAXWEI / SUMWEI

*  Loop over columns again correcting weights.
         DO 4 I = MAX(   1, BOUNDS( K - 1 ) ),
     :            MIN( NEL, BOUNDS( K     ) )
            W( I ) = W( I ) * SUMWEI
 4       CONTINUE
 2    CONTINUE
      END
* $Id$
