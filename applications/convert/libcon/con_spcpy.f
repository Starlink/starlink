      SUBROUTINE CON_SPCPY( MAPX, MAPY, MAP, NSPEC, SPSIZE, SPECTR,
     :                      CUBE, STATUS )
*+
*  Name:
*     CON_SPCPY

*  Purpose:
*     Copies the arrays of a SPECX map into a simple data cube.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL CON_SPCPY( MAPX, MAPY, MAP, NSPEC, SPSIZE, SPECTR; CUBE;
*                     STATUS )

*  Description:
*     Copy the arrays of a SPECX map into a simple data cube.  Rows in
*     the cube corresponding to positions on sky where a spectrum was
*     not observed are filled with bad values.

*  Arguments:
*     MAPX = INTEGER (Given)
*        Number of points along the X-axis of the grid of observed
*        positions.
*     MAPY = INTEGER (Given)
*        Number of points along the X-axis of the grid of observed
*        positions.
*     MAP( MAPX, MAPY ) = INTEGER (Given)
*        Grid of observed positions.  The value of each grid point
*        either indicates the location of the spectrum at the point
*        or is a flag indicating that no observation was made.
*     NSPEC = INTEGER (Given)
*        The total number of spectra observed in the grid.
*     SPSIZE = INTEGER (Given)
*        The number of points in each spectrum.
*     SPECTR( SPSIZE, NSPEC ) = REAL (Given)
*        Array holding all the observed spectra.
*     CUBE( MAPX, MAPY, SPSIZE ) = INTEGER (Given)
*        The returned data cube containing the observation.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1997, 2003 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     ACD: A C Davenhall (Edinburgh)
*     DSB: David S Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-JUN-1997 (ACD):
*        Original version.
*     23-JUL-1997 (ACD):
*        First stable version.
*     27-FEB-2003 (DSB):
*        Re-format. Flip the output vertically. Change axis ordering to
*        make the spectral axis the third axis in the output cube.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'PRM_PAR'           ! Starlink Bad values.

*  Arguments Given:
      INTEGER MAPX
      INTEGER MAPY
      INTEGER MAP( MAPX, MAPY )
      INTEGER NSPEC
      INTEGER SPSIZE
      REAL SPECTR( SPSIZE, NSPEC )

*  Arguments Returned:
      REAL CUBE( MAPX, MAPY, SPSIZE )

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      INTEGER CSPEC             ! Number of the current spectrum
      INTEGER LOOPS             ! Loop index in the current spectrum
      INTEGER LOOPX             ! X loop index in the map grid
      INTEGER LOOPY             ! Y loop index in the map grid
      INTEGER OUTY              ! Y loop index in the output cube

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Examine every point in the map grid and check whether there is a
*  spectrum there or not.
      DO LOOPY = 1, MAPY

*  The SPECX map creation code has a bug which causes the rows to be
*  flipped vertically.  JACH prefer that SPECX2NDF should undo the
*  effect of this bug, rather than fixing the bug within SPECX
*  (presumably the SPECX map-reading code also undoes the effect of the
*  bug).  Store the row index within the output at which to store this
*  input row.
         OUTY = MAPY - LOOPY + 1

         DO LOOPX = 1, MAPX
            CSPEC = MAP( LOOPX, LOOPY )

            IF ( CSPEC .GE. 1  .AND.  CSPEC .LE. NSPEC ) THEN

*  There is a genuine spectrum; copy it to the cube.
               DO LOOPS = 1, SPSIZE
                  CUBE( LOOPX, OUTY, LOOPS ) = SPECTR( LOOPS, CSPEC )
               END DO

            ELSE

*  There is no spectrum; fill the row with null values.
               DO LOOPS = 1, SPSIZE
                  CUBE( LOOPX, OUTY, LOOPS ) = VAL__BADR
               END DO

            END IF

         END DO
      END DO

      END
