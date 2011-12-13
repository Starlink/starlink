      SUBROUTINE KPS1_VECPL( MAP, LBND, UBND, NVEC,
     :                       LBND1M, UBND1M, LBND2M, UBND2M, VECMAG,
     :                       LBND1O, UBND1O, LBND2O, UBND2O, VECORN,
     :                       STEP, ANGFAC, ANGROT, DSCALE, AHSIZE,
     :                       JUST, WORK1, WORK2, STATUS )
*+
*  Name:
*     KPS1_VECPL

*  Purpose:
*     Plots a vector map.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_VECPL( MAP, LBND, UBND, NVEC, LBND1M, UBND1M,
*                      LBND2M, UBND2M, VECMAG, LBND1O, UBND1O, LBND2O,
*                      UBND2O, VECORN, STEP, ANGFAC, ANGROT, DSCALE, AHSIZE,
*                      JUST, WORK1, WORK2, STATUS )

*  Description:
*     The supplied vectors are plotted within the current PGPLOT window.

*  Arguments:
*     MAP = INTEGER (Given)
*        Pointer to AST Mapping from PIXEL Frame to GRAPHICS Frame.
*     LBND( 2 ) = INTEGER (Given)
*        Lower pixel index bound of the overlap area.
*     UBND( 2 ) = INTEGER (Given)
*        Upper pixel index bound of the overlap area.
*     NVEC = INTEGER (Given)
*        Maximum number of vectors to be plotted.
*     LBND1M = INTEGER (Given)
*        Lower pixel index bound on axis 1 of vector magnitude array.
*     UBND1M = INTEGER (Given)
*        Upper pixel index bound on axis 1 of vector magnitude array.
*     LBND2M = INTEGER (Given)
*        Lower pixel index bound on axis 2 of vector magnitude array.
*     UBND2M = INTEGER (Given)
*        Upper pixel index bound on axis 2 of vector magnitude array.
*     VECMAG( LBNN1M : UBND1M, LBND2M : UBND2M ) = REAL (Given)
*        The vector magnitudes.
*     LBND1O = INTEGER (Given)
*        Lower pixel index bound on axis 1 of vector orientation array.
*     UBND1O = INTEGER (Given)
*        Upper pixel index bound on axis 1 of vector orientation array.
*     LBND2O = INTEGER (Given)
*        Lower pixel index bound on axis 2 of vector orientation array.
*     UBND2O = INTEGER (Given)
*        Upper pixel index bound on axis 2 of vector orientation array.
*     VECORN( LBNN1O : UBND1O, LBND2O : UBND2O ) = REAL (Given)
*        The vector orientations. A value of zero refers to the positive
*        Y pixel axis. Positive rotation is in the sense of pixel X to
*        pixel Y.
*     STEP = INTEGER (Given)
*        The increment in pixels between vectors.
*     ANGFAC = REAL (Given)
*        The factor which converts values from VECORN into units of
*        radians.
*     ANGROT = REAL (Given)
*        An ACW angle to add on to each orientation, in radians.
*     DSCALE = REAL (Given)
*        A factor which converts data values in VECMAG into
*        corresponding vector lengths in centimetres.
*     AHSIZE = REAL (Given)
*        The length of each stroke of the arrowhead placed at the end
*        of the vector, in pixels.  A value of zero causes no arrowhead
*        to be drawn and the vectors to be centred on the corresponding
*        pixel centres.  Otherwise, the vectors start at the centre of
*        the corresponding pixel.
*     JUST = CHARACTER * ( * ) (Given)
*        This can be 'CENTRE', 'START' or 'END'.  'CENTRE' causes
*        vectors to be drawn centred on the corresponding pixel.
*        'START' causes vectors to be drawn starting at the
*        corresponding pixel.  'END' causes vectors to be drawn ending
*        at the corresponding pixel.
*     WORK1( NVEC, 2 ) = DOUBLE PRECISION (Returned)
*        Work space to hold vector PIXEL positions.
*     WORK2( NVEC, 2 ) = DOUBLE PRECISION (Returned)
*        Work space to hold vector GRAPHICS positions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

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
*     4-OCT-1999 (DSB):
*        Original PGPLOT/AST version.
*     13-AUG-2002 (DSB):
*        Modified to include bounding box args and INK for KPS1_VECT.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'AST_PAR'          ! AST_ constants

*  Arguments Given:
      INTEGER MAP
      INTEGER LBND( 2 )
      INTEGER UBND( 2 )
      INTEGER NVEC
      INTEGER LBND1M
      INTEGER UBND1M
      INTEGER LBND2M
      INTEGER UBND2M
      REAL VECMAG( LBND1M : UBND1M, LBND2M : UBND2M )
      INTEGER LBND1O
      INTEGER UBND1O
      INTEGER LBND2O
      INTEGER UBND2O
      REAL VECORN( LBND1O : UBND1O, LBND2O : UBND2O )
      INTEGER STEP
      REAL ANGFAC
      REAL ANGROT
      REAL DSCALE
      REAL AHSIZE
      CHARACTER * ( * ) JUST

*  ARGUMENTS RETURNED
      DOUBLE PRECISION WORK1( NVEC, 2 )
      DOUBLE PRECISION WORK2( NVEC, 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL PIBY2                 ! 90 degrees in radians
      PARAMETER ( PIBY2 = 1.5707963268 )

*  Local Variables:
      DOUBLE PRECISION GX        ! GRAPHICS X at vector
      DOUBLE PRECISION GY        ! GRAPHICS Y at vector
      INTEGER I                  ! Column index
      INTEGER IVEC               ! No. of vectors in overlap area
      INTEGER J                  ! Row index
      INTEGER K                  ! Vector index
      INTEGER NPLOT              ! No. of vectors plotted
      REAL VECANG                ! Vector position angle in radians
      REAL VECLEN                ! Vector length in pixels
      REAL DX1, DX2, DY1, DY2    ! Bounding box (unused)
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the pixel co-ordinate at the centre of each pixel for which a
*  vector is to be plotted. Loop round the plot region, storing positions
*  at increments given by argument STEP.  The bottom-left vector is
*  placed in the middle of the bottom-left increment box.
      IVEC = 0
      DO J = LBND( 2 ) + STEP / 2, UBND( 2 ), STEP
         DO I = LBND( 1 ) + STEP / 2, UBND( 1 ), STEP

            IVEC = IVEC + 1
            WORK1( IVEC, 1 ) = REAL( I ) - 0.5
            WORK1( IVEC, 2 ) = REAL( J ) - 0.5
            IF( IVEC .EQ. NVEC ) GO TO 10

         END DO
      END DO

 10   CONTINUE

*  Transform these PIXEL positions into GRAPHICS positions using the
*  supplied Mapping.
      CALL AST_TRANN( MAP, IVEC, 2, NVEC, WORK1, .TRUE., 2, NVEC, WORK2,
     :                STATUS )

*  Indicate no vectors have been plotted.
      NPLOT = 0

*  Loop round each vector.
      DO K = 1, IVEC

*  Skip over bad GRAPHICS positions.
         GX = WORK2( K, 1 )
         GY = WORK2( K, 2 )
         IF( GX .NE. AST__BAD .AND. GY .NE. AST__BAD ) THEN

*  Find the corresponding pixel indices.
           I = NINT( WORK1( K, 1 ) + 0.5D0 )
           J = NINT( WORK1( K, 2 ) + 0.5D0 )

*  Skip over bad data or orientation values.
           IF( VECMAG( I, J ) .NE. VAL__BADR .AND.
     :         VECORN( I, J ) .NE. VAL__BADR ) THEN

*  Calculate the length of the vector in units of pixels.
               VECLEN = VECMAG( I, J ) / DSCALE

*  Calculate the vector orientation, in radians.
               VECANG = ANGFAC * VECORN( I, J ) + ANGROT

*  Plot the vector.
               CALL KPS1_VECT( .TRUE., REAL( GX ), REAL( GY ), JUST,
     :                         VECLEN, VECANG, AHSIZE, DX1, DX2, DY1,
     :                         DY2, STATUS )

*  Abort if an error has occurred.
               IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Increment the number of vectors plotted.
               NPLOT = NPLOT + 1

            END IF

         END IF

      END DO

*  If no vectors have been plotted, warn the user.
      CALL MSG_BLANK( STATUS )

      IF ( NPLOT .EQ. 0 ) THEN
         CALL MSG_OUT( 'KPS1_VECPL_MSG1', '  No vectors plotted.',
     :                 STATUS )
         CALL MSG_BLANK( STATUS )

*  Otherwise, tell the user how many vectors were plotted unless at
*  silent reporting.
      ELSE
         CALL MSG_SETI( 'NP', NPLOT )
         CALL MSG_OUTIF( MSG__NORM, 'KPS1_VECPL_MSG2', '  ^NP vectors'//
     :                   ' plotted.', STATUS )
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

      END IF

*  Arrive here if an error occurs.
 999  CONTINUE

      END
