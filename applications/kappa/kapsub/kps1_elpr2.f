      SUBROUTINE KPS1_ELPR2( NX, NY, USEANN, PA, RATIO, RMIN, RMAX, XC,
     :                       YC, NSTEP, PA1, WID, PASTEP, REGVAL, IGRP,
     :                       REGIND, REGCEN, REGWID, STATUS )
*+
*  Name:
*     KPS1_ELPR2

*  Purpose:
*     Creates an ARD description for a set of elliptical sectors.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_ELPR2( NX, NY, USEANN, PA, RATIO, RMIN, RMAX, XC, YC,
*                      NSTEP, PA1, WID, PASTEP, REGVAL, IGRP, REGIND,
*                      REGCEN, REGWID, STATUS )

*  Description:
*     An identifier for a GRP group is returned which holds an ARD
*     description defining a set of wedge-shaped sectors described by
*     the supplied arguments.  The azimuthal angle at the centre, and
*     opening angle of each bin are also returned, together with the
*     bin number corresponding to each mask value (i.e. the integer
*     values in the mask which will be created by ARD_WORK).

*  Arguments:
*     NX = INTEGER (Given)
*        The first dimension of the image to which the ARD description
*        applies.
*     NY = INTEGER (Given)
*        The second dimension of the image to which the ARD description
*        applies.
*     USEANN = LOGICAL (Given)
*        It is .TRUE. if the sectors are to be restricted to the
*        intersection with the annulus defined by PA, RATIO, RMIN and
*        RMAX.  Otherwise, the ARD description describes the entirity
*        of each sector.
*     PA = REAL (Given)
*        The angle between the major axis of the ellipses and the first
*        axis, given in radians measured anti-clockwise.  It is only
*        used if USEANN is .TRUE.
*     RATIO = REAL (Given)
*        The ratio of the minor- to major-axis lengths.  In the range
*        zero to one.  It is only used if USEANN is .TRUE.
*     RMIN = REAL (Given)
*        The radius (measured on the major axis) of the inner edge of
*        the annulus.  It is only used if USEANN is .TRUE.
*     RMAX = REAL (Given)
*        The radius (measured on the major axis) of the outer edge of
*        the annulus.  It is only used if USEANN is .TRUE.
*     XC = REAL (Given)
*        The x pixel co-ordinate of the ellipse centre.
*     YC = REAL (Given)
*        The y pixel co-ordinate of the ellipse centre.
*     NSTEP = INTEGER (Given)
*        The number of sectors required.
*     PA1 = REAL (Given)
*        The angle between the x axis and the clockwise edge of the
*        first sector.  Given in radians, and measured positive
*        anti-clockwise.
*     WID = REAL (Given)
*        The width (opening angle) of each sector in radians.
*     PASTEP = REAL (Given)
*        The step between centres of adjacent sectors in radians.  This
*        should be no smaller than WID.
*     REGVAL = INTEGER (Given)
*        The value which will be passed to subroutine ARD_WORK as the
*        value to use to represent the first region included in the
*        ARD description.
*     IGRP = INTEGER (Returned)
*        A GRP identifier for a group containing the ARD description.
*        This group should be deleted using GRP_DELET when it is no
*        longer needed.
*     REGIND( * ) = INTEGER (Returned)
*        The index within REGCEN at which each regions azimuthal angle
*        is stored.  The supplied array should be at least 3+NSTEP
*        elements long.  This array is indexed using the positive
*        integer values which subroutine ARD_WORK uses to represent
*        each region.
*     REGCEN( NSTEP ) = REAL (Returned)
*        The azimuthal angle at the centre of each sector, in degrees.
*     REGWID( NSTEP ) = REAL (Returned)
*        The angular width of each sector, in degrees.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-JAN-1995 (DSB):
*        Original version.
*     1995 March 28 (MJC):
*        Minor stylistic and typographical changes, shortened long
*        lines, and used a modern-style variable declaration.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP__ constants

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      LOGICAL USEANN
      REAL PA
      REAL RATIO
      REAL RMIN
      REAL RMAX
      REAL XC
      REAL YC
      INTEGER NSTEP
      REAL PA1
      REAL WID
      REAL PASTEP
      INTEGER REGVAL

*  Arguments Returned:
      INTEGER IGRP
      INTEGER REGIND( * )
      REAL REGCEN( NSTEP )
      REAL REGWID( NSTEP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL RTOD                  ! Factor to convert radians to degrees
      PARAMETER ( RTOD = 57.29578 )

*  Local Variables:
      INTEGER CURVAL             ! ARD mask value for current region
      REAL HWID                  ! Half the annulus width
      REAL IMAJ                  ! Major axis radius of inner edge
      INTEGER I                  ! Loop count
      REAL IMIN                  ! Minor axis radius of inner edge
      REAL OMAJ                  ! Major axis radius of outer edge
      REAL OMIN                  ! Minor axis radius of outer edge
      CHARACTER * ( GRP__SZNAM ) POLY ! ARD description of first sector
      REAL R                     ! Radius of sector
      CHARACTER *( GRP__SZNAM ) TEXT ! Buffer for ARD keywords, etc

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create the GRP group to store the ARD description in.
      CALL GRP_NEW( 'ARD DESCRIPTION', IGRP, STATUS )

*  Initialise the value which will be used by ARD_WORK to represent
*  the `current' region.
      CURVAL = REGVAL - 1

*  Set the origin of the ARD user co-ordinate system to the centre of
*  the ellipse.  The centre of the ellipse is thus at (0,0) in the ARD
*  user co-ordinate system.
      WRITE( TEXT, * ) 'OFFSET(', XC, ',', YC, ')'
      CALL GRP_PUT( IGRP, 1, TEXT, 0, STATUS )

*  If the ARD description is to describe the intersection of the
*  sectors with the annulus specified by RMIN and RMAX...
      IF ( USEANN ) THEN

*  The annulus is represented by an ARD description of the form
*  "( ELLIPSE(...1...) .AND. .NOT. ELLIPSE(...2...) )" where the first
*  ellipse is the outer edge of the annulus, and the second ellipse is
*  the inner edge.  Now find the major and minor axes of the outer edge
*  of the annulus.
         OMAJ = RMAX
         OMIN = OMAJ * RATIO

*  Store the ARD description of this annulus so long as it is not
*  degenerate.
         IF ( OMAJ .GT. 0.0 ) THEN
            CURVAL = CURVAL + 1
            WRITE( TEXT, * ) '( ELLIPSE( 0.0, 0.0, ', OMAJ, ',', OMIN,
     :                       ',', RTOD * PA, ') '
            CALL GRP_PUT( IGRP, 1, TEXT, 0, STATUS )

*  Now find the major and minor axes of the inner edge of the annulus.
            IMAJ = RMIN
            IMIN = IMAJ * RATIO

*  Store the ARD description of this annulus so long as it is not
*  degenerate.
            IF ( IMAJ .GT. 0.0 ) THEN
               CURVAL = CURVAL + 1
               WRITE( TEXT, * ) '.AND. .NOT. ELLIPSE( 0.0, 0.0, ',
     :                          IMAJ, ',', IMIN, ',', RTOD * PA, ') '
               CALL GRP_PUT( IGRP, 1, TEXT, 0, STATUS )
            END IF

*  Store an .AND. operator to connect the annulus ARD description to
*  the list of sector descriptions.
            CALL GRP_PUT( IGRP, 1, ') .AND. ', 0, STATUS )

         END IF

      END IF

*  Start the ARD description with an opening parenthesis.
      CALL GRP_PUT( IGRP, 1, '(', 0, STATUS )

*  Set the length of the sector radii, such that there is no danger of
*  the open end of the sector intersecting the image.
      R = 2.0 * SQRT( REAL( NX**2 + NY**2 ) )

*  Create a text string holding the ARD description of the first sector
*  polygon.
      HWID = WID / 2.0
      WRITE( POLY, * ) 'POLY( 0.0, 0.0, ',
     :                 R * COS( PA1 ),          ',',
     :                 R * SIN( PA1 ),          ',',
     :                 R * COS( PA1 + HWID ),   ',',
     :                 R * SIN( PA1 + HWID ),   ',',
     :                 R * COS( PA1 + WID ),   ',',
     :                 R * SIN( PA1 + WID ),   ')'

*  Store this polygon in the group.
      CALL GRP_PUT( IGRP, 1, POLY, 0, STATUS )

*  Store the azimuthal angle at the centre of the first sector, and the
*  sector width, in degress.
      REGCEN( 1 ) = ( PA1 + HWID ) * RTOD
      REGWID( 1 ) = WID * RTOD

*  Find the ARD mask value used to represent the first sector, and
*  store the corresponding region index in the returned REGIND array.
      CURVAL = CURVAL + 1
      REGIND( CURVAL ) = 1

*  Do each of the remaining sectors. ARD will assume that implicit .OR.
*  operators exist between adjacent keywords (the statements such as
*  TWIST are skipped over by ARD when evaluating the logical
*  expression).
      DO I = 2, NSTEP

*  Put the origin of the ARD user co-ordinate system back to the origin
*  of the application co-ordinate system (in this case this is the same
*  as the pixel co-ordinate system).
         WRITE( TEXT, * ) 'OFFSET(', -XC, ',', -YC, ')'
         CALL GRP_PUT( IGRP, 1, TEXT, 0, STATUS )

*  Rotate the user co-ordinate system by the required sector step size
*  about the origin of application co-ordinates.
         WRITE( TEXT, * ) 'TWIST(',RTOD * PASTEP,')'
         CALL GRP_PUT( IGRP, 1, TEXT, 0, STATUS )

*  Set the origin of the ARD user co-ordinate system back to the centre of
*  the ellipse.
         WRITE( TEXT, * ) 'OFFSET(', XC, ',', YC, ')'
         CALL GRP_PUT( IGRP, 1, TEXT, 0, STATUS )

*  Store the polygon again, this time referred to the rotated co-ordinate
*  system.
         CALL GRP_PUT( IGRP, 1, POLY, 0, STATUS )

*  Store the azimuthal angle at the centre of this sector, and the
*  sector angular width.
         REGCEN( I ) = REGCEN( I - 1 ) + PASTEP * RTOD
         REGWID( I ) = REGWID( I - 1 )

*  Find the ARD mask value used to represent this sector, and store
*  the corresponding region index in the returned REGIND array.
         CURVAL = CURVAL + 1
         REGIND( CURVAL ) = I

      END DO

*  Finish the ARD description by closing the parentheses.
      CALL GRP_PUT( IGRP, 1, ')', 0, STATUS )

      END
