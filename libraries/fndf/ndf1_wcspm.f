      SUBROUTINE NDF1_WCSPM( MAP, LBND, UBND, PERM, STATUS )
*+
*  Name:
*     NDF1_WPLIM

*  Purpose:
*     Determine pixel limits for all NDF axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_WCSPM( MAP, LBND, UBND, PERM, STATUS )

*  Description:
*     This routine returns a permutation array that embodies a guess at
*     the correspondance between pixel and WCS axes. In some cases there
*     is no defined correspondance between WCS and pixel axes, and so we
*     need to just guess at what the user means when entering WCS axis
*     values in a certain order (e.g. in an NDF section specified in WCS).
*     For instance in a 2D image of the sky there no way of assigning one
*     pixel axis to the RA axis and the other pixel axis to the Dec axis.
*     To see that this is the case, consider an image in which the WCS
*     axes are linear and at 45 degrees to the pixel axes. Varying either
*     pixel axes independently of the other one will make equal changes to
*     both RA and Dec values.
*
*     Having said that, the 45 degree case is rare and we will usually be
*     able to have some confidence that the user will associate each WCS
*     axis with the "closest" pixel axis.
*
*     In the 45 degree case, the returned permuatation array is based on
*     the assumption that each WCS axis corresponds to the pixel axis with
*     the same index as the WCS axis.

*  Arguments:
*     MAP = INTEGER (Given)
*        AST pointer to the Mapping from pixel coordinates to WCS.
*     LBND( * ) = INTEGER (Given)
*        Lower pixel index bounds for the NDF.
*     UBND( * ) = INTEGER (Given)
*        Upper pixel index bounds for the NDF.
*     PERM( * ) = INTEGER (Returned)
*        The index into this array is pixel index, and the values stored
*        in the array are the corresponding WCS axis indices. A value of
*        0 is returned for any pixel axes that have no corresponding WCS
*        axis.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S Berry (JACH, UCLan)
*     {enter_new_authors_here}

*  History:
*     17-MAY-2012 (DSB):
*        Original version.
*     27-JUN-2012 (DSB):
*        Return 0 for any pixel axis that has no WCS axis rather than
*        reporting an error.
*     1-JUL-2015 (DSB):
*        Identify the 45 degree case, and return a PERM array that
*        assumes that pixel and wcs axes have the same index.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'AST_PAR'          ! AST_ constants and functions
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      INTEGER MAP
      INTEGER LBND( * )
      INTEGER UBND( * )

*  Arguments Returned:
      INTEGER PERM( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION CHANGE
      DOUBLE PRECISION DELTA( NDF__MXDIM )
      DOUBLE PRECISION DLBNDD( NDF__MXDIM )
      DOUBLE PRECISION DUBNDD( NDF__MXDIM )
      DOUBLE PRECISION MAXCHANGE
      DOUBLE PRECISION MAXCHANGE2
      DOUBLE PRECISION TESTPIX( NDF__MXDIM )
      DOUBLE PRECISION TESTWCS( NDF__MXDIM )
      DOUBLE PRECISION TESTWCS2( NDF__MXDIM )
      INTEGER DIFF
      INTEGER ICELL
      INTEGER IPIX
      INTEGER IWCS
      INTEGER JPIX
      INTEGER JWCS
      INTEGER MINDIFF
      INTEGER NCELL
      INTEGER NCELLTOT
      INTEGER NPIX
      INTEGER NTEST
      INTEGER NWCS
      INTEGER TMAP
      INTEGER WCSAX( NDF__MXDIM )
      LOGICAL GOOD
      LOGICAL MORE
      LOGICAL USED

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of pixel and WCS axes.
      NPIX = AST_GETI( MAP, 'Nin', STATUS )
      NWCS = AST_GETI( MAP, 'Nout', STATUS )

*  First look for any pixel axes that feed one and only one WCS axis.
*  For such pixel axes, store the index of the corresponding WCS axis
*  in the returned array. For other pixel axes, store zero and set a flag
*  to indicate that we have not yet associated each pixel axis with a WCS
*  axis.
      MORE = .FALSE.
      DO IPIX = 1, NPIX
         CALL AST_MAPSPLIT( MAP, 1, IPIX, WCSAX, TMAP, STATUS )
         IF( TMAP .NE. AST__NULL ) THEN
            IF( AST_GETI( TMAP, 'Nout', STATUS ) .EQ. 1 ) THEN
               PERM( IPIX ) = WCSAX( 1 )
            ELSE
               PERM( IPIX ) = 0
               MORE = .TRUE.
            END IF
            CALL AST_ANNUL( TMAP, STATUS )
         ELSE
            PERM( IPIX ) = 0
            MORE = .TRUE.
         END IF
      END DO

* If not all pixel axes have been assigned, we have more work to do.
      IF( MORE ) THEN

*  First need to find a pixel that has good world coords. Test an
*  increasingly fine square grid of points in pixel space. Initialise the
*  number of cells along a pixel axis (the same for all pixel axes), and
*  the size of a cell on each pixel axis. We start with one cell covering
*  the whole NDF.
         NCELL = 1
         DO IPIX = 1, NPIX
            DELTA( IPIX ) = UBND( IPIX ) - LBND( IPIX ) + 1.0
         END DO

*  Loop until we find a pixel with good WCS coords. Do at most 10000 tests.
         GOOD = .FALSE.
         NTEST = 0
         DO WHILE( .NOT. GOOD .AND. NTEST .LE. 10000 )

*  Find the total number of cells required to cover the NDF, and
*  initialise the pixel coords at the centre of the first cell.
            NCELLTOT = 1
            DO IPIX = 1, NPIX
               NCELLTOT = NCELLTOT*NCELL
               TESTPIX( IPIX ) = LBND( IPIX ) - 1.0 + 0.5*DELTA( IPIX )
            END DO

*  Loop round all cells, breaking early if a pixel with good WCS coords
*  is found.
            ICELL = 1
            DO WHILE( .NOT. GOOD .AND. ICELL .LE. NCELLTOT )
               ICELL = ICELL + 1

*  Increment the number of pixels tested.
               NTEST = NTEST + 1

*  Find the WCS coords of the current cell centre.
               CALL AST_TRANN( MAP, 1, NPIX, 1, TESTPIX, .TRUE., NWCS,
     :                         1, TESTWCS, STATUS )

*  Set the GOOD flag .TRUE. if all WCS axis values are good.
               IWCS = 1
               GOOD = .TRUE.
               DO WHILE( GOOD .AND. IWCS .LE. NWCS )
                  GOOD = ( TESTWCS( IWCS ) .NE. AST__BAD )
                  IWCS = IWCS + 1
               END DO

*  Set up the pixel coords at the centre of the next cell.
               IF( .NOT. GOOD ) THEN
                  IPIX = 1
                  TESTPIX( IPIX ) = TESTPIX( IPIX ) + DELTA( IPIX )
                  DO WHILE( IPIX .LT. NPIX .AND.
     :                      TESTPIX( IPIX ) .GT. UBND( IPIX ) )
                     TESTPIX( IPIX ) = LBND( IPIX ) - 1.0
     :                                 + 0.5*DELTA( IPIX )
                     IPIX = IPIX + 1
                     TESTPIX( IPIX ) = TESTPIX( IPIX ) + DELTA( IPIX )
                  END DO
               END IF

            END DO

*  Double the density of the cells covering the grid.
            IF( .NOT. GOOD ) THEN
               NCELL = 2*NCELL
               DO IPIX = 1, NPIX
                  DELTA( IPIX ) = 0.5*DELTA( IPIX )
               END DO
            END IF

         END DO

*  Report an error if no good pixel was found.
         IF( .NOT. GOOD .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = NDF__WCSIN
            CALL ERR_REP( ' ', 'Cannot find a pixel with valid world '//
     :                    'coordinates.', STATUS )
         ELSE

*  Loop round all pixel axes that have not yet been associated with a WCS
*  axis.
            DO IPIX = 1, NPIX
               IF( PERM( IPIX ) .EQ. 0 ) THEN


*  Move the test pixel position by a tenth of a pixel on the current
*  pixel axis.
                  TESTPIX( IPIX ) = TESTPIX( IPIX ) + 0.1

*  Find the corresponding WCS position.
                  CALL AST_TRANN( MAP, 1, NPIX, 1, TESTPIX, .TRUE.,
     :                            NWCS, 1, TESTWCS2, STATUS )

*  Find the WCS axis that has changed the most from the original TESTWCS
*  position. Ignore WCS axes that have already been assined to a pixel axis.
*  We keep a note of the second best axis.
                  JWCS = 0
                  MAXCHANGE = -1.0
                  MAXCHANGE2 = -1.0
                  DO IWCS = 1, NWCS

                     USED = .FALSE.
                     DO JPIX = 1, NPIX
                        IF( PERM( JPIX ) .EQ. IWCS ) USED = .TRUE.
                     END DO

                     IF( .NOT. USED ) THEN
                        CHANGE = ABS( TESTWCS( IWCS )
     :                                - TESTWCS2( IWCS ) )
                        IF( CHANGE .GT. MAXCHANGE ) THEN
                           MAXCHANGE2 = MAXCHANGE
                           MAXCHANGE = CHANGE
                           JWCS = IWCS
                        END IF
                     END IF

                  END DO

*  Only assign the axis if the best axis was significantly better than
*  the second best axis. This means that WCS axes at 45 degrees to the
*  pixel axes will not be assigned.
                  IF( JWCS .GE. 1 .AND. JWCS .LE. NWCS .AND.
     :                MAXCHANGE .GT. MAXCHANGE2*1.1 )
     :              PERM( IPIX ) = JWCS

*  Reset the test pixel position back to its original value.
                  TESTPIX( IPIX ) = TESTPIX( IPIX ) - 0.1

               END IF
            END DO

*  Loop round any pixel axes that have still not been associated with a WCS
*  axis. Each is associated with the remaining unassigned WCS axis that
*  has the closest index.
            DO IWCS = 1, NWCS
               WCSAX( IWCS ) = 0
            END DO

            DO IPIX = 1, NPIX
               IF( PERM( IPIX ) .GT. 0 ) THEN
                  WCSAX( PERM( IPIX ) ) = IPIX
               END IF
            END DO

            DO IPIX = 1, NPIX
               IF( PERM( IPIX ) .EQ. 0 ) THEN

                  MINDIFF = 2*NDF__MXDIM
                  DO IWCS = 1, NWCS
                     IF( WCSAX( IWCS ) .EQ. 0 ) THEN
                        DIFF = ABS( IWCS - IPIX )
                        IF( DIFF .LT. MINDIFF ) THEN
                           PERM( IPIX ) = IWCS
                           MINDIFF = DIFF
                        END IF
                     END IF
                  END DO

                  IF( PERM( IPIX ) .GT. 0 ) WCSAX( PERM( IPIX ) ) = IPIX

               END IF
            END DO
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_WCSPM', STATUS )

      END
