      SUBROUTINE KPS1_SKYF2( IGRP, ORIENT, PSIZE, TILT, REFIMG, REFSKY,
     :                       SCS, NP, NPOS, P, PC, AA, BB, XX, YY,
     :                       STATUS )
*+
*  Name:
*     KPS1_SKYF2

*  Purpose:
*     Form an initial guess at the IRA projection parameters which
*     will describe the supplied co-ordinate data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_SKYF2( IGRP, ORIENT, PSIZE, TILT, REFIMG, REFSKY, SCS,
*                      NP, NPOS, P, PC, AA, BB, XX, YY, STATUS )

*  Description:
*     The formatted co-ordinate data supplied in the group identified by
*     IGRP is converted to floating-point format and stored in the
*     supplied arrays.  A guess at the IRA projection parameters which
*     relate the two sets of co-ordinates (sky and image) is then found.
*     The reference position is taken as the first supplied position.
*     The pixels size (pixels are initially assumed to be square) and
*     the image orientation are determined by the finding the
*     arc-distance and bearing of the second supplied position from the
*     first.  The celestial sphere is asummed not to have been tilted
*     before projection.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for a group holding the supplied co-ordinate
*        data.  Each set of 4 adjacent elements should contain formatted
*        versions of the sky longitude, sky latitude, image X and image
*        Y co-ordinates.  Blank elements should have been removed from
*        this group before-hand.
*     ORIENT = LOGICAL (Given)
*        Was a specific value supplied by the user for the position
*        angle of the image Y axis?
*     PSIZE = LOGICAL (Given)
*        Was a specific value supplied by the user for the pixel size?
*     TILT = LOGICAL (Given)
*        Was a specific value supplied by the user for the tilt of the
*        celestial sphere prior to projection?
*     REFIMG = LOGICAL (Given)
*        Was a specific value supplied by the user for the image
*        co-ordinates of the reference point.
*     REFSKY = LOGICAL (Given)
*        Was a specific value supplied by the user for the sky
*        co-ordinates of the reference point.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky co-ordinate system in which the longitude and latitude
*        values contained in the group identified by IRGP are given.
*     NP = INTEGER (Given)
*        The size of array P.
*     NPOS = INTEGER (Given)
*        The number of positions in the group identified by IGRP.
*     P( NP ) = DOUBLE PRECISION (Given)
*        Any projection parameters which were specified by the user.  If
*        the arguments TILT, ORIENT, PSIZE, REFIMG and REFSKY indicate
*        that a value has not been supplied for a particular parameter,
*        then the corresponding element of P is ignored.
*     PC( NP ) = DOUBLE PRECISION (Returned)
*        The initial guess projection parameters, combining those
*        supplied in P with initial guesses for those not supplied in P.
*     AA( NPOS ) = DOUBLE PRECISION (Returned)
*        The sky longitude values read from the group identified by IGRP.
*     BB( NPOS ) = DOUBLE PRECISION (Returned)
*        The sky latitude values read from the group identified by IGRP.
*     XX( NPOS ) = DOUBLE PRECISION (Returned)
*        The image X values read from the group identified by IGRP.
*     YY( NPOS ) = DOUBLE PRECISION (Returned)
*        The image Y values read from the group identified by IGRP.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council. All
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-OCT-1994 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*  Arguments Given:
      INTEGER IGRP
      LOGICAL ORIENT
      LOGICAL PSIZE
      LOGICAL TILT
      LOGICAL REFIMG
      LOGICAL REFSKY
      CHARACTER * ( * ) SCS
      INTEGER NP
      INTEGER NPOS
      DOUBLE PRECISION P( NP )

*  Arguments Given and Returned:
      DOUBLE PRECISION PC( NP )
      DOUBLE PRECISION AA( NPOS )
      DOUBLE PRECISION BB( NPOS )
      DOUBLE PRECISION XX( NPOS )
      DOUBLE PRECISION YY( NPOS )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION SLA_DBEAR ! Bearing of one point from another
      DOUBLE PRECISION SLA_DSEP  ! Arc-distance between two positions

*  Local Variables:
      DOUBLE PRECISION AD        ! Arc-distance between two positions
      DOUBLE PRECISION PD        ! Pixel distance between two positions
      INTEGER          POS       ! Current position index
      CHARACTER * ( GRP__SZNAM ) TEXT( 4 ) ! Text defining current
                                 ! position

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert the supplied co-ordinate data to double precision.
      DO POS = 1, NPOS
         CALL GRP_GET( IGRP, 4*POS - 3, 4, TEXT, STATUS )
         CALL IRA_CTOD( TEXT( 1 ), TEXT( 2 ), SCS, AA( POS ),
     :        BB( POS ), STATUS )
         CALL CHR_CTOD( TEXT( 3 ), XX( POS ), STATUS )
         CALL CHR_CTOD( TEXT( 4 ), YY( POS ), STATUS )
      END DO

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Form an initial guess at the projection parameters.  The reference
*  position is initially taken as the first supplied position unless
*  the user has specified it.
      IF ( .NOT. REFSKY ) THEN
         PC( 1 ) = AA( 1 )
         PC( 2 ) = BB( 1 )
      ELSE
         PC( 1 ) = P( 1 )
         PC( 2 ) = P( 2 )
      END IF

      IF ( .NOT. REFIMG ) THEN
         PC( 3 ) = XX( 1 )
         PC( 4 ) = YY( 1 )
      ELSE
         PC( 3 ) = P( 3 )
         PC( 4 ) = P( 4 )
      END IF

*  If pixel sizes were supplied by the user, they are used.  Otherwise,
*  the pixel size is guessed by dividing the arc-distance between the
*  first two positions by the distance in pixels.  An error is reported
*  if the first two positions are co-incident.
      IF ( .NOT. PSIZE ) THEN

         PD = SQRT( MAX( 0.0D0, ( XX( 1 ) - XX( 2 ) )**2 +
     :        ( YY( 1 ) - YY( 2 ) )**2 ) )
         AD = SLA_DSEP( AA( 1 ), BB( 1 ), AA( 2 ), BB( 2 ) )

         IF ( ( ABS( AD ) .LT. VAL__SMLD .OR. ABS( PD ) .LT. VAL__SMLD )
     :          .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPS1_SKYFT_ERR5', 'Co-incident sky '/
     :                    /'positions given.', STATUS )
            GO TO 999
         END IF

         PC( 5 ) = AD/PD
         PC( 6 ) = AD/PD

      ELSE
         PC( 5 ) = P( 5 )
         PC( 6 ) = P( 6 )
      END IF

*  If the image orientation was supplied by the user, it is
*  used. Otherwise, the image orientation is guessed by finding the
*  difference between the bearing of the second position from the first
*  in sky co-ordinates and image co-ordinates.  An error is reported
*  if the first two positions are co-incident.
      IF ( .NOT. ORIENT ) THEN

         PD = SQRT( MAX( 0.0D0, ( XX( 1 ) - XX( 2 ) )**2 +
     :        ( YY( 1 ) - YY( 2 ) )**2 ) )
         AD = SLA_DSEP( AA( 1 ), BB( 1 ), AA( 2 ), BB( 2 ) )

         IF ( ( ABS( AD ) .LT. VAL__SMLD .OR. ABS( PD ) .LT. VAL__SMLD )
     :          .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPS1_SKYFT_ERR6', 'Co-incident sky '/
     :                    /'positions given.', STATUS )
            GO TO 999
         END IF

         PC( 7 ) = SLA_DBEAR( AA( 1 ), BB( 1 ), AA( 2 ), BB( 2 ) ) -
     :             ATAN2( XX( 1 ) - XX( 2 ), YY( 2 ) - YY( 1 ) )
      ELSE
         PC( 7 ) = P( 7 )

      END IF

*  If the tilt of the celestial sphere was supplied by the user, it is
*  used, otherwise it is guessed as zero.
      IF ( .NOT. TILT ) THEN
         PC( 8 ) = 0.0D0
      ELSE
         PC( 8 ) = P( 8 )
      END IF

*  Shift the sky co-ordinates of the reference positions slightly (by a
*  tenth of a pixel) so that it is unlikely to correspond to any of the
*  supplied positions.  This needs to be done because some projections
*  (e.g. AITOFF) have a singularity at the reference position.
      PC( 1 ) = AA( 1 ) + 0.1*PC( 5 )
      PC( 2 ) = BB( 1 ) + 0.1*PC( 6 )

*  Jump to here if an error has occurred.
 999  CONTINUE

      END
