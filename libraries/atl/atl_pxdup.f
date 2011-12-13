      SUBROUTINE ATL_PXDUP( IWCS, POS, STATUS )
*+
*  Name:
*     ATL_PXDUP

*  Purpose:
*     Ensure the number of WCS axes is no less than the number of pixel
*     axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_PXDUP( IWCS, POS, STATUS )

*  Description:
*     This routine ensures that the number of axes in the current Frame
*     (WCS Frame) of a FrameSet is at least equal to the number of axes
*     in the base Frame (PIXEL or GRID Frame). If the initial number of
*     current Frame axes is too small, extra axes are added to the
*     current Frame by duplicating selected pixel axes.

*  Arguments:
*     IWCS = INTEGER (Given)
*        The supplied FrameSet.
*     POS( * ) = DOUBLE PRECISION (Given)
*        The base Frame coords of a position which has good current Frame
*        coords.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-MAR-2007 (DSB):
*        Original version.
*     2-MAR-2007 (DSB):
*        Initialise NDUP, correct argument list for AST_CMPMAP and remove
*        unused variables.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'ATL_PAR'          ! ATL constants

*  Arguments Given:
      INTEGER IWCS
      DOUBLE PRECISION POS(*)

*  Status:
      INTEGER STATUS           ! Global status

*  Local Variables:
      DOUBLE PRECISION RATE
      DOUBLE PRECISION MXRATE
      INTEGER CM1
      INTEGER DUPAX( ATL__MXDIM )
      INTEGER IAX
      INTEGER ICUR
      INTEGER IDUP
      INTEGER INAX( ATL__MXDIM )
      INTEGER INPRM( 2*ATL__MXDIM )
      INTEGER JAX
      INTEGER JDUP
      INTEGER JUNK
      INTEGER MAP
      INTEGER MAPAX
      INTEGER NDUP
      INTEGER NEWFRM
      INTEGER NEWMAP
      INTEGER NFEED
      INTEGER NPIX
      INTEGER NWCS
      INTEGER OUTAX( ATL__MXDIM )
      INTEGER OUTPRM( 2*ATL__MXDIM )
      INTEGER PM1
      INTEGER PM2
      LOGICAL ISDUP
      LOGICAL ISFEED
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of base (PIXEL) and current (WCS) axes.
      NPIX = AST_GETI( IWCS, 'Nin', STATUS )
      NWCS = AST_GETI( IWCS, 'Nout', STATUS )

*  Do nothing if the current Frame already has enough axes.
      IF( NWCS .LT. NPIX ) THEN

*  Begin an AST context.
         CALL AST_BEGIN( STATUS )

*  Note the number of pixel axes which will need to be duplicated in
*  order for the current Frame to have NPIX axes.
         NDUP = NPIX - NWCS

*  Find which pixel axes feed the wcs axes.
         MAP = AST_GETMAPPING( IWCS, AST__CURRENT, AST__BASE, STATUS )

         DO JAX = 1, NWCS
            INAX( JAX ) = JAX
         END DO

         CALL AST_MAPSPLIT( MAP, NWCS, INAX, OUTAX, MAPAX, STATUS )
         IF( MAPAX .NE. AST__NULL ) THEN
            NFEED = AST_GETI( MAPAX, 'Nout', STATUS )

*  Create an array holding the indices of the pixel axes which do not
*  feed any of the WCS axes. These are the pixel axes we will duplicate
*  as WCS axes. We only need NDUP of these.
            IDUP = 0
            DO IAX = 1, NPIX
               IF( IDUP .LT. NDUP ) THEN
                  ISFEED = .FALSE.
                  DO JAX = 1, NFEED
                     IF( OUTAX( JAX ) .EQ. IAX ) THEN
                        ISFEED = .TRUE.
                     END IF
                  END DO

                  IF( .NOT. ISFEED ) THEN
                     IDUP = IDUP + 1
                     DUPAX( IDUP ) = IAX
                  END IF
               END IF
            END DO

*  If the Mapping cannot be split, we do not as yet have any pixel axes
*  to be duplicated.
         ELSE
            IDUP = 0
         END IF

*  Invert the Mapping so that it goes from pixel to wcs coords.
         CALL AST_INVERT( MAP, STATUS )

*  Check each pixel axis
         DO IAX = 1, NPIX

*  If we do not yet have enough pixel axes available for duplication, see if
*  this pixel axis is independent of the existing WCS axes. If it is, we
*  can add it to the list of pixel axes to be duplicated.
            IF( IDUP .LT. NDUP ) THEN

*  If this pixel axis is already on the list of axes to be duplicated,
*  ignore it.
               ISDUP = .FALSE.
               DO JDUP = 1, IDUP
                  IF( DUPAX( JDUP ) .EQ. IAX ) ISDUP = .TRUE.
               END DO

               IF( .NOT. ISDUP ) THEN

*  Get the rate of change of each WCS axis with respect to the current pixel
*  axis, at the supplied pixel position. Note the maximum rate of change
*  with respect to any WCS axis.
                  MXRATE = 0.0
                  DO JAX = 1, NWCS
                     RATE = AST_RATE( MAP, POS, JAX, IAX, STATUS )
                     IF( RATE .NE. AST__BAD ) THEN
                        RATE = ABS( RATE )
                        IF( RATE .GT. MXRATE ) MXRATE = RATE
                     END IF
                  END DO

*  If this pixel axis is independent of all WCS axes, add it to the list
*  of pixel axes to be duplicated.
                  IF( MXRATE .LE. 1.0D-20 ) THEN
                     IDUP = IDUP + 1
                     DUPAX( IDUP ) = IAX
                  END IF

               END IF
            END IF
         END DO

*  If we do not yet have enough pixel axes available for duplication,
*  just pad the list arbitrarily with trailing pixel axes, excluding any
*  that are already on the list of pixel axes to be duplicated.
         DO IAX = 1, NPIX
            IF( IDUP .LT. NDUP ) THEN
               ISDUP = .FALSE.
               DO JDUP = 1, IDUP
                  IF( DUPAX( JDUP ) .EQ. IAX ) ISDUP = .TRUE.
               END DO
               IF( .NOT. ISDUP ) THEN
                  IDUP = IDUP + 1
                  DUPAX( IDUP ) = IAX
               END IF
            END IF
         END DO

*  Having decided which pixel axes to duplicate as WCS axes, we now go
*  ahead and duplicate them. This involves creating a new Mapping and a
*  new current Frame. First consider the Mapping. It is a series CmpMap
*  containing; 1) a PermMap that duplicates the required pixel axes, 2)
*  a parallel CmpMap that contains the original Mapping in parallel with
*  a UnitMap that copies the duplicated pixel axis values, and 3) a PermMap
*  that reorders the original WCS axes and the new duplicated WCS
*  (=pixel) axes into the required final order. Create the first PermMap.
*  It has NPIX inputs (the original pixel axes) and NPIX+NDUP outputs
*  (the original pixel axes that feed the original Mapping, plus the extra
*  NDUP duplicated pixel axes). The outputs corresponding to the original
*  pixel axes come first, followed by the duplicated axes.
         DO IAX = 1, NPIX
            OUTPRM( IAX ) = IAX
            INPRM( IAX ) = IAX
         END DO

         DO IDUP= 1, NDUP
            OUTPRM( NPIX + IDUP ) = DUPAX( IDUP )
            INPRM( DUPAX( IDUP ) ) = NPIX + IDUP
         END DO

         PM1 = AST_PERMMAP( NPIX, INPRM, NPIX + NDUP, OUTPRM, 0.0D0,
     :                      ' ', STATUS )

*  Now create the parallel CmpMap containing the original Mapping and a
*  UnitMap.
        CM1 = AST_CMPMAP( MAP, AST_UNITMAP( NDUP, ' ', STATUS ),
     :                    .FALSE., ' ', STATUS )

*  Now create the final PermMap.
         DO IAX = 1, NPIX
            OUTPRM( IAX ) = 0
         END DO

         DO IDUP = 1, NDUP
            INPRM( NWCS + IDUP ) = DUPAX( IDUP )
            OUTPRM( DUPAX( IDUP ) ) = NWCS + IDUP
         END DO

         JAX = 1
         DO IAX = 1, NWCS

            DO WHILE( OUTPRM( JAX ) .NE. 0 )
               JAX = JAX + 1
            END DO

            INPRM( IAX ) = JAX
            OUTPRM( JAX ) = IAX
         END DO

         PM2 = AST_PERMMAP( NPIX, INPRM, NPIX, OUTPRM, 0.0D0, ' ',
     :                      STATUS )

*  Combine all 3 into a single Mapping, and simplify it.
         NEWMAP = AST_SIMPLIFY( AST_CMPMAP( PM1, AST_CMPMAP( CM1, PM2,
     :                                                      .TRUE., ' ',
     :                                                      STATUS ),
     :                                      .TRUE., ' ', STATUS ),
     :                          STATUS )

*  Now we construct the new current Frame. First pick the duplicated pixel
*  axes from the base Frame, then combine the rsulting Fram ewith the
*  original current Frame.
         NEWFRM = AST_CMPFRAME( AST_GETFRAME( IWCS, AST__CURRENT,
     :                                        STATUS ),
     :                          AST_PICKAXES( AST_GETFRAME( IWCS,
     :                                                      AST__BASE,
     :                                                      STATUS ),
     :                                        NDUP, DUPAX, JUNK,
     :                                        STATUS ),
     :                          ' ', STATUS )

*  Now permute the order of the axes to put the duplicated pixel axes in
*  their original position.
         CALL AST_PERMAXES( NEWFRM, OUTPRM, STATUS )

*  Add the new Frame into the FrameSet, and then delete the original.
         ICUR = AST_GETI( IWCS, 'Current', STATUS )
         CALL AST_ADDFRAME( IWCS, AST__BASE, NEWMAP, NEWFRM, STATUS )
         CALL AST_REMOVEFRAME( IWCS, ICUR, STATUS )

*  End the AST context.
         CALL AST_END( STATUS )

      END IF

      END
