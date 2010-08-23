      SUBROUTINE KPG1_ASSMP( FRAME, ARRDIM, NAX, NPOS, POS, GEO, NSAMP,
     :                       DELTA, SAMP, STATUS )
*+
*  Name:
*     KPG1_ASSMP

*  Purpose:
*     Returns co-ordinates at evenly spaces positions along a given
*     poly-line.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASSMP( FRAME, ARRDIM, NAX, NPOS, POS, GEO, NSAMP, SAMP,
*                      DELTA, STATUS )

*  Description:
*     This routine returns an a pointer to an array holding the
*     co-ordinates at a set of NSAMP positions evenly spaced along a
*     poly-line defined by NPOS "profile positions".

*  Arguments:
*     FRAME = INTEGER (Given)
*        An AST pointer to the Frame in which the polyline is defined.
*        This is used to define geodesic curves joining the supplied
*        profile positions. Only accessed if GEO is .TRUE.
*     ARRDIM = INTEGER (Given)
*        The size of the first dimension of POS.
*     NAX = INTEGER (Given)
*        The number of axes in the Frame.
*     NPOS = INTEGER (Given)
*        The number of profile positions defining the poly-line. This
*        must be less than or equal to ARRDIM.
*     POS( ARRDIM, NAX ) = DOUBLE PRECISION (Given)
*        The profile positions. The first axis indexes the position
*        number, and the first NPOS elements should be used. The second
*        axis indexes the axis number. An error is reported if any
*        invalid positions are supplied.
*     GEO = LOGICAL (Given)
*        Should the poly-line be constructed from geodesic curves in the
*        supplied Frame? If not, the poly-line is made up of
*        straight-line segments in the supplied Frame.
*     NSAMP = INTEGER (Given)
*        The number of samples required along the poly-line.
*     DELTA = DOUBLE PRECISION (Given and Returned)
*        The increment between samples. If not known, this should be set
*        to zero. Returned holding the used increment.
*     SAMP( NSAMP, NAX ) = DOUBLE PRECISION (Returned)
*        The array holding the sample positions.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER FRAME
      INTEGER ARRDIM
      INTEGER NAX
      INTEGER NPOS
      DOUBLE PRECISION POS( ARRDIM, NAX )
      LOGICAL GEO
      INTEGER NSAMP

*  Arguments Given and Returned:
      DOUBLE PRECISION DELTA

*  Arguments Returned:
      DOUBLE PRECISION SAMP( NSAMP, NAX )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION KPG1_ASDIS! Distance between 2 positions
      CHARACTER CHR_NTH*2        ! "st", "nd", "rd", etc.

*  Local Variables:
      DOUBLE PRECISION CLEN      ! Curve length
      DOUBLE PRECISION DIS       ! Length from root position to current sample
      DOUBLE PRECISION MXDIS     ! Length from root position to next position
      DOUBLE PRECISION SPOS( NDF__MXDIM ) ! Current sample position
      INTEGER I                  ! Loop count
      INTEGER IROOT              ! Index of root position
      INTEGER ISAMP              ! Sample index
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the increment between samples was not supplied, find it now.
      IF( DELTA .EQ. 0.0D0 .OR. DELTA .EQ. AST__BAD ) THEN

*  Find the total length of the lines joining the profile positions.
         CLEN = 0.0D0
         DO I = 2, NPOS
            DELTA = KPG1_ASDIS( FRAME, NPOS, NAX, POS, I - 1, I,
     :                          GEO, STATUS )
            IF( DELTA .NE. AST__BAD ) CLEN = CLEN + DELTA

         END DO

*  Find the increment between each of the NSAMP points.
         DELTA = CLEN / DBLE( NSAMP - 1 )

      END IF

*  Initialise the "root" profile position to be the first supplied profile
*  position.
      IROOT = 1

*  Initialise the distance from the root profile position to the
*  "current" sample.
      DIS = -DELTA

*  Find the curve length to the next (second) profile position.
      MXDIS = KPG1_ASDIS( FRAME, ARRDIM, NAX, POS, 1, 2, GEO, STATUS )

*  Report an error if the distance between these two points is undefined.
      IF( MXDIS .EQ. AST__BAD .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'I', 1 )
         CALL MSG_SETC( 'I', CHR_NTH( 1 ) )
         CALL MSG_SETI( 'IP1', 2 )
         CALL MSG_SETC( 'IP1', CHR_NTH( 2 ) )
         CALL ERR_REP( 'KPG1_ASSMP_ERR', 'The distance between the '//
     :                 '^I and the ^IPI positions is undefined.',
     :                 STATUS )
         GO TO 999
      END IF

*  Set up the Current Frame co-ordinates of each sample position.
      DO ISAMP = 1, NSAMP

*  Find the distance from the current root profile position to this sample.
         DIS = DIS + DELTA

*  If we have gone beyond the next profile position, then change the root
*  profile position to be the next one (if there is a next one - otherwise
*  we extrapolate).
         IF( DIS .GE. MXDIS .AND. IROOT .LT. NPOS - 1 ) THEN
            IROOT = IROOT + 1
            DIS = DIS - MXDIS
            MXDIS = KPG1_ASDIS( FRAME, ARRDIM, NAX, POS, IROOT,
     :                         IROOT + 1, GEO, STATUS )

*  Report an error if the distance between these two points is undefined.
            IF( MXDIS .EQ. AST__BAD .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', IROOT )
               CALL MSG_SETC( 'I', CHR_NTH( IROOT ) )
               CALL MSG_SETI( 'IP1', IROOT + 1 )
               CALL MSG_SETC( 'IP1', CHR_NTH( IROOT + 1 ) )
               CALL ERR_REP( 'KPG1_ASSMP_ERR', 'The distance between '//
     :                    'the ^I and the ^IPI positions is undefined.',
     :                    STATUS )
               GO TO 999
            END IF

         END IF

*  Find the sample position by offsetting from the root profile position
*  towards the next profile position by the required distance.
         CALL KPG1_ASOFF( FRAME, ARRDIM, NAX, POS, IROOT, IROOT + 1,
     :                    GEO, DIS, SPOS, STATUS )

*  Store the sample position.
         DO I = 1, NAX
            SAMP( ISAMP, I ) = SPOS( I )
         END DO

      END DO

 999  CONTINUE

      END
