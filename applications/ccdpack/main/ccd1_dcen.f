      SUBROUTINE CCD1_DCEN( DIM1, X, Y, DINT, GROUP, MAXGRP, MINPIX,
     :                      SUM1, SUM2, NCON, XC, YC, MINT, NOUT,
     :                      STATUS )
*+
*  Name:
*     CCD1_DCEN

*  Purpose:
*     Determines the centroids of pixel groups

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_DCEN( DIM1, X, Y, DINT, GROUP, MAXGRP, MINPIX, SUM1,
*                     SUM2, NCON, XC, YC, MINT, NOUT, STATUS )

*  Description:
*     This routine forms the centroids and mean intensities of pixels
*     groups whose content is described in the lists X, Y and DINT. The
*     group membership is determined by the list of integer values in
*     GROUP. Pixel groups whose content is less than MINPIX are not
*     returned.

*  Arguments:
*     DIM1 = INTEGER (Given)
*        The first dimension of the input lists.
*     X( DIM1 ) = INTEGER (Given)
*        The X position of the intensity value.
*     Y( DIM1 ) = INTEGER (Given)
*        The Y position of the intensity value.
*     DINT( DIM1 ) = DOUBLE PRECISION (Given)
*        The intensity value of the pixel at this position.
*     GROUP( DIM1 ) = INTEGER (Given)
*        The group number of the pixel at this position. No group
*        membership (a null entry) is indicated by a 0.
*     MAXGRP = INTEGER (Given)
*        The maximum group number.
*     MINPIX = INTEGER (Given)
*        The minimum number of pixels which must be members of a group
*        for that group to be valid.
*     SUM1( MAXGRP ) = DOUBLE PRECISION (Given and Returned)
*        Workspace for holding group sums.
*     SUM2( MAXGRP ) = DOUBLE PRECISION (Given and Returned)
*        Workspace for holding group sums.
*     NCON( MAXGRP ) = INTEGER (Given and Returned)
*        Workspace. The number of pixels which contribute to each
*        group.
*     XC( MAXGRP ) = DOUBLE PRECISION (Returned)
*        The X centroid of the pixel groups.
*     YC( MAXGRP ) = DOUBLE PRECISION (Returned)
*        The Y centroid of the pixel groups.
*     MINT( MAXGRP ) = DOUBLE PRECISION (Returned)
*        The mean intensity values of pixels within the group.
*     NOUT = INTEGER (Returned)
*        The number of centroids returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-OCT-1992 (PDRAPER):
*        Original version.
*     8-JAN-1993 (PDRAPER):
*        Added mean intensity formation.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER DIM1
      INTEGER X( DIM1 )
      INTEGER Y( DIM1 )
      DOUBLE PRECISION DINT( DIM1 )
      INTEGER GROUP( DIM1 )
      INTEGER MAXGRP
      INTEGER MINPIX

*  Arguments Given and Returned:
      DOUBLE PRECISION SUM1( MAXGRP )
      DOUBLE PRECISION SUM2( MAXGRP )
      INTEGER NCON( MAXGRP )

*  Arguments Returned:
      DOUBLE PRECISION XC( MAXGRP )
      DOUBLE PRECISION YC( MAXGRP )
      DOUBLE PRECISION MINT( MAXGRP )
      INTEGER NOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER GRP                ! Current group
      INTEGER NGRP               ! Number of possible groups

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the buffers.
      DO 1 I = 1, MAXGRP
         NCON( I ) = 0
         SUM1( I ) = 0.0D0
         SUM2( I ) = 0.0D0
 1    CONTINUE

*  Form the X centroids.
      DO 2 I = 1, DIM1

*  Which group does this position belong to?
         GRP = GROUP( I )

*  Is this a valid group (i.e. non-zero).
         IF ( GRP .GT. 0 ) THEN

*  Form the centroid sums.
            SUM1( GRP ) = SUM1( GRP ) + DINT( I ) * DBLE( X( I ) )
            SUM2( GRP ) = SUM2( GRP ) + DINT( I )
            NCON( GRP ) = NCON( GRP ) + 1
         END IF
 2    CONTINUE

*  Form the X centroids. Only produce values for pixel groups whose
*  contribution count exceeds MINPIX. Set the number of potential
*  pixel groups.
      NGRP = 0
      NOUT = 0
      DO 3 I = 1, MAXGRP
         IF ( NCON( I ) .GT. 0 ) THEN
            NGRP = NGRP + 1
            IF ( NCON( I ) .GT. MINPIX ) THEN
               NOUT = NOUT + 1
               XC( NOUT ) = SUM1( I ) / SUM2 ( I )
            END IF
         END IF
 3    CONTINUE

*  Check that we have some output values.
      IF ( NOUT .GT. 0 ) THEN

*  Now do the same for the Y centroids.
*  Initialise the buffers.
         DO 4 I = 1, MAXGRP
            SUM1( I ) = 0.0D0
 4       CONTINUE

*  Form the Y centroids sums.
         DO 5 I = 1, DIM1

*  Which group does this position belong to?
            GRP = GROUP( I )

*  Is this a valid group (i.e. non-zero).
            IF ( GRP .GT. 0 ) THEN

*  Form the centroid sums.
               SUM1( GRP ) = SUM1( GRP ) + DINT( I ) * DBLE( Y( I ) )
            END IF
 5       CONTINUE

*  Form the Y centroids. Only produce values for pixel groups whose
*  contribution count exceeds MINPIX.
         NOUT = 0
         DO 6 I = 1, MAXGRP
            IF ( NCON( I ) .GT. MINPIX ) THEN
               NOUT = NOUT + 1
               YC( NOUT ) = SUM1( I ) / SUM2( I )
            END IF
 6       CONTINUE

*  Form sums for mean intensities.
*  Initialise the buffers.
         DO 7 I = 1, MAXGRP
            SUM1( I ) = 0.0D0
 7       CONTINUE
         DO 8 I = 1, DIM1

*  Which group does this position belong to?
            GRP = GROUP( I )

*  Is this a valid group (i.e. non-zero).
            IF ( GRP .GT. 0 ) THEN

*  Form the intensity sums.
               SUM1( GRP ) = SUM1( GRP ) + DINT( I )
            END IF
 8       CONTINUE

*  Now form the mean intensity. Only produce values for pixel groups
*  whose contribution count exceeds MINPIX (as has been done for the X
*  and Y sums)
         NOUT = 0
         DO 9 I = 1, MAXGRP
            IF ( NCON( I ) .GT. MINPIX ) THEN
               NOUT = NOUT + 1
               MINT( NOUT ) = SUM1( I ) ! change to sum / DBLE( NCON( I ) )
            END IF
 9       CONTINUE
      END IF
      IF ( NOUT .EQ. 0 ) THEN

*  No output centroids. Set STATUS and exit.
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NGRP', NGRP )
         CALL MSG_SETI( 'MINPIX', MINPIX )
         CALL ERR_REP( 'CCD1_DCEN1',
     :   '  All pixel groups rejected (from a possible ^NGRP pixel'//
     :   ' groups using a minimum of ^MINPIX pixels per group)',
     :   STATUS )
      END IF

      END
* $Id$
