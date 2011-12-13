      SUBROUTINE CCD1_WINAD( IN1, XDIM1, YDIM1, XOFF1, YOFF1, IN2,
     :                       XDIM2, YDIM2, XOFF2, YOFF2, XDIMT, YDIMT,
     :                       XOFFT, YOFFT, TRN, STATUS )

*+
*  Name:
*     CCD1_WINAD

*  Purpose:
*     Adds two integer images inside of a given window.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_WINAD( IN1, XDIM1, YDIM1, XOFF1, YOFF1, IN2, XDIM2,
*                      YDIM2, YOFF2, YOFF2, XDIMT, YDIMT, XOFFT,
*                      YOFFT, TRN, STATUS )

*  Description:
*     This routine adds the values within a window defined by an output
*     array. This is a specialised routine for the CCDPAIR application.

*  Arguments:
*     IN1( XDIM1, YDIM1 ) = INTEGER ( Given )
*        The first input array.
*     XDIM1 = INTEGER (Given)
*        First dimension of the first input array.
*     YDIM1 = INTEGER (Given)
*        Second dimension of the first input array.
*     XOFF1 = INTEGER (Given)
*        The X origin of the first input array in the global system to
*        which all three array are referenced.
*     YOFF1 = INTEGER (Given)
*        The Y origin of the first input array in the global system to
*        which all three array are referenced.
*     IN2( XDIM2, YDIM2 ) = INTEGER ( Given )
*        The second input array.
*     XDIM2 = INTEGER (Given)
*        First dimension of the second input array.
*     YDIM2 = INTEGER (Given)
*        Second dimension of the second input array.
*     XOFF2 = INTEGER (Given)
*        The X origin of the second input array in the global system to
*        which all three array are referenced.
*     YOFF2 = INTEGER (Given)
*        The Y origin of the second input array in the global system to
*        which all three array are referenced.
*     XDIMT = INTEGER (Given)
*        First dimension of the output array.
*     YDIM2 = INTEGER (Given)
*        Second dimension of the output array.
*     XOFFT = INTEGER (Given)
*        The X origin of the output array in the global system to
*        which all three array are referenced.
*     YOFFT = INTEGER (Given)
*        The Y origin of the output array in the global system to
*        which all three array are referenced.
*     TRN( XDIMT, YDIMT ) = INTEGER ( Returned )
*        The output array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  How the images are transfered.
*
*                |-----|
*                |     |
*                |     |
*                |     |
*             |- |-----|
*             |  |     |
*             |  |-----|  YOFF2, YOFFT
*             |     |
*             |     |
*             |-----|     YOFF1
*
*          XOFF1     XOFF2
*               XOFFT
*
*       1,1
*
*       The transfer window may have its lower corners not coincident
*       with another window. Any parts of the transfer window not part
*       of the other windows are will have zero value.

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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     2-FEB-1993 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER XDIM1
      INTEGER YDIM1
      INTEGER IN1( XDIM1, YDIM1 )
      INTEGER XOFF1
      INTEGER YOFF1

      INTEGER XDIM2
      INTEGER YDIM2
      INTEGER IN2( XDIM2, YDIM2 )
      INTEGER XOFF2
      INTEGER YOFF2

      INTEGER XDIMT
      INTEGER YDIMT
      INTEGER XOFFT
      INTEGER YOFFT

*  Arguments Returned:
      INTEGER TRN( XDIMT, YDIMT )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variables
      INTEGER J                  ! Loop variables
      INTEGER XGLOB              ! X global position
      INTEGER YGLOB              ! Y global position
      INTEGER XPOS1              ! X position in IN1 indices
      INTEGER YPOS1              ! Y position in IN1 indices
      INTEGER XPOS2              ! X position in IN2 indices
      INTEGER YPOS2              ! Y position in IN2 indices
      INTEGER NVAL               ! Number of values
      INTEGER VAL                ! Sum of values


*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over the output image.
      DO 1 J = 1, YDIMT

*  Determine the current global Y position.
         YGLOB = YOFFT + J - 1

*  From this get the positions in the input arrays.
         YPOS1 = YGLOB - YOFF1 + 1
         YPOS2 = YGLOB - YOFF2 + 1
         DO 2 I = 1, XDIMT

*  Determine the current global X position.
            XGLOB = XOFFT + I - 1

*  From this get the positions in the input arrays.
            XPOS1 = XGLOB - XOFF1 + 1
            XPOS2 = XGLOB - XOFF2 + 1

*  Add the values from these poitions.
            NVAL = 0
            VAL = 0
            IF ( XPOS1 .GE. 1 ) THEN
               IF ( XPOS1 .LE. XDIM1 ) THEN
                  IF ( YPOS1 .GE. 1 ) THEN
                     IF ( YPOS1 .LE. YDIM1 ) THEN
                        IF ( IN1( XPOS1, YPOS1 ) .GT. 0 ) THEN
                           VAL = IN1( XPOS1, YPOS1 )
                           NVAL = NVAL + 1
                        END IF
                     END IF
                  END IF
               END IF
            END IF
            IF ( XPOS2 .GE. 1 ) THEN
               IF ( XPOS2 .LE. XDIM2 ) THEN
                  IF ( YPOS2 .GE. 1 ) THEN
                     IF ( YPOS2 .LE. YDIM2 ) THEN
                        IF ( IN2( XPOS2, YPOS2 ) .GT. 0 ) THEN
                           VAL = VAL + IN2( XPOS2, YPOS2 )
                           NVAL = NVAL + 1
                        END IF
                     END IF
                  END IF
               END IF
            END IF

*  Form mean and put value into output array.
            IF ( NVAL .GT. 0 ) THEN
               TRN( I, J ) = VAL / NVAL
            ELSE
               TRN( I, J ) = 0
            END IF

*  Work out which
 2       CONTINUE
 1    CONTINUE

      END
* $Id$
