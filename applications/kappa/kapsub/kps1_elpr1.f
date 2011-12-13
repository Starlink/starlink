      SUBROUTINE KPS1_ELPR1( NX, NY, PA, RATIO, NSTEP, RSTEP, RMIN, WID,
     :                       XC, YC, PA1, PA2, USESEC, REGVAL, IGRP,
     :                       REGIND, REGCEN, REGWID, STATUS )
*+
*  Name:
*     KPS1_ELPR1

*  Purpose:
*     Creates an ARD description for a set of elliptical annuli.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_ELPR1( NX, NY, PA, RATIO, NSTEP, RSTEP, RMIN, WID, XC,
*                      YC, PA1, PA2, USESEC, REGVAL, IGRP, REGIND,
*                      REGCEN, REGWID, STATUS )

*  Description:
*     An identifier for a GRP group is returned which holds an ARD
*     description defining a set of elliptical annuli described by the
*     supplied arguments.  The central radius, and width of each bin are
*     also returned, together with the bin number corresponding to each
*     mask value (i.e. the integer values in the mask which will be
*     created by ARD_WORK).

*  Arguments:
*     NX = INTEGER (Given)
*        The first dimension of the image to which the ARD description
*        applies.
*     NY = INTEGER (Given)
*        The second dimension of the image to which the ARD description
*        applies.
*     PA = REAL (Given)
*        The angle between the major axis of the ellipses and the first
*        axis, given in radians measured anti-clockwise.
*     RATIO = REAL (Given)
*        The ratio of the minor- to major-axis lengths.  In the range
*        zero to one.
*     NSTEP = INTEGER (Given)
*        The number of annuli required.
*     RSTEP = REAL (Given)
*        The step between centres of adjacent annuli in pixels,
*        measured along the major axis.  This must be no smaller than
*        WID.
*     RMIN = REAL (Given)
*        The radius (measured on the major axis) of the inner edge of
*        the inner-most annulus.
*     WID = REAL (Given)
*        The width of each annulus in pixels, measured along the major
*        axis.
*     XC = REAL (Given)
*        The x pixel co-ordinate of the ellipse centre.
*     YC = REAL (Given)
*        The y pixel co-ordinate of the ellipse centre.
*     PA1 = REAL (Given)
*        The angle between the x axis and the first of the two radii
*        defining the sector in which the annuli are to be used.  Given
*        in radians in the range 0 - 2 pi, and measured positive anti-
*        clockwise.  The sector is that defined by moving
*        anti-clockwise from PA1 to PA2.  It is only used if USESEC is
*        .TRUE.
*     PA2 = REAL (Given)
*        The angle between the x axis and the second of the two radii
*        defining the sector in which the annuli are to be used.  Given
*        in radians in the range 0 - 2 pi, and measured positive anti-
*        clockwise.  It is only used if USESEC is .TRUE.
*     USESEC = LOGICAL (Given)
*        It is .TRUE. if the annuli are to be restricted to the
*        intersection with the sector defined by PA1 and PA2.
*        Otherwise, the ARD description describes the entirity of each
*        annulus.
*     REGVAL = INTEGER (Given)
*        The value which will be passed to subroutine ARD_WORK as the
*        value to use to represent the first region included in the
*        ARD description.
*     IGRP = INTEGER (Returned)
*        A GRP identifier for a group containing the ARD description.
*        This group should be deleted using GRP_DELET when it is no
*        longer needed.
*     REGIND( * ) = INTEGER (Returned)
*        The index within REGVAL at which each regions radial distance
*        is stored.  The supplied array should be at least 2+2*NSTEP
*        elements long.  This array is indexed using the positive
*        integer values which subroutine ARD_WORK uses to represent
*        each region.
*     REGCEN( NSTEP ) = REAL (Returned)
*        The radial distance to the centre of each annulus, in pixels.
*     REGWID( NSTEP ) = REAL (Returned)
*        The width of each annulus, in pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1995, 1998, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     3-AUG-1998 (DSB):
*        Corrected logic for creating sectors of larger than 180 degrees.
*     6-DEC-2004 (DSB):
*        Write first polgon vertex out on same line as the POLY keyword
*        (this is a work around for a bug in ARD).
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
      REAL PA
      REAL RATIO
      INTEGER NSTEP
      REAL RSTEP
      REAL RMIN
      REAL WID
      REAL XC
      REAL YC
      REAL PA1
      REAL PA2
      LOGICAL USESEC
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
      REAL PI                    ! Pi
      PARAMETER ( PI = 3.1415927 )
      REAL TWOPI                 ! Two Pi
      PARAMETER ( TWOPI = 2.0*PI )
      REAL PIBY2                 ! Pi by 2
      PARAMETER ( PIBY2 = 0.5*PI )

*  Local Variables:
      REAL CENTRE                ! Major axis radius of annulus centre
      INTEGER CURVAL             ! ARD mask value for current region
      REAL HWID                  ! Half the annulus width
      INTEGER I                  ! Loop count
      REAL IMAJ                  ! Major axis radius of inner edge
      REAL IMIN                  ! Minor axis radius of inner edge
      REAL LPA1                  ! Position angle of next polygon vertex
      REAL LPA2                  ! Local version of PA2 which is .GT. PA1
      REAL OMAJ                  ! Major axis radius of outer edge
      REAL OMIN                  ! Minor axis radius of outer edge
      REAL R                     ! Radius of sector
      CHARACTER * ( GRP__SZNAM ) TEXT ! Buffer for ARD expressions

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create the GRP group in which to store the ARD description.
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
*  annuli with the sector specified by PA1 and PA2...
      IF( USESEC ) THEN

*  Find the ARD mask value which will be used to represent the sector
*  polygon.
         CURVAL = CURVAL + 1

*  Set the length of the sector radii, such that there is no danger of
*  the open end of the sector intersecting the image.
         R = 2.0 * SQRT( REAL( NX**2 + NY**2 ) )

*  Add 2PI onto PA2 until it is larger than PA1.
         LPA2 = PA2
         DO WHILE( LPA2 .LE. PA1 )
            LPA2 = LPA2 + TWOPI
         END DO

*  Start a Polygon ARD region. The sector polygon starts at the profile
*   centre.
         CALL GRP_PUT( IGRP, 1, 'POLY( 0.0, 0.0', 0, STATUS )

*  Next point is the end of radius number 1 .
         WRITE( TEXT, * ) R*COS( PA1 ), ',', R*SIN( PA1 )
         CALL GRP_PUT( IGRP, 1, TEXT, 0, STATUS )

*  Add points separated by PI/2 to the polygon until the closing angle
*  is reached.
         LPA1 = PA1 + PIBY2
         DO WHILE( LPA1 .LT. LPA2 )
            WRITE( TEXT, * ) R*COS( LPA1 ), ',', R*SIN( LPA1 )
            CALL GRP_PUT( IGRP, 1, TEXT, 0, STATUS )
            LPA1 = LPA1 + PIBY2
         END DO

*  The last point is the end of radius no. 2.  Finished the polygon ARD
*  keyword, and .AND. it with the list of tangential sectors which is
*  to be added next.
         WRITE( TEXT, * ) R * COS( PA2 ), ',', R * SIN( PA2 ),
     :                    ' ) .AND. ('
         CALL GRP_PUT( IGRP, 1, TEXT, 0, STATUS )

*  If the ARD description is to describe all of each annulus, start the
*  ARD description with an opening parenthesis.
      ELSE
         CALL GRP_PUT( IGRP, 1, '(', 0, STATUS )
      END IF

*  Store half the annulus width.
      HWID = WID / 2.0

*  Find the major-axis radius of the centre of the outer-most annulus.
      CENTRE = RMIN + REAL( NSTEP - 1 ) * RSTEP + HWID

*  Loop round each annulus, from the outer most to the inner-most.  ARD
*  will assume that implicit .OR. operators exist between each pair of
*  keywords.
      DO I = NSTEP, 1, -1

*  Store the major-axis radius at the centre of the current annulus,
*  and the annulus width.
         REGCEN( I ) = CENTRE
         REGWID( I ) = WID

*  Find the ARD mask value used to represent this annulus, and store
*  the corresponding region index in the returned REGIND array.
         CURVAL = CURVAL + 1
         REGIND( CURVAL ) = I

*  Each annulus is represented by an ARD description of the form
*  "( ELLIPSE(...1...) .AND. .NOT. ELLIPSE(...2...) )" where the first
*  ellipse is the outer edge of the annulus, and the second ellipse is
*  the inner edge.  The exception to this is if the width and step are
*  equal, in which case a single ellipse keyword is used.  This is done
*  to prevent odd pixels being missed at the interface between two
*  adjacent annuli.  Now find the major and minor axes of the outer
*  edge of the annulus.
         OMAJ = CENTRE + HWID
         OMIN = OMAJ * RATIO

*  Now find the major and minor axes of the inner edge of the annulus.
         IMAJ = CENTRE - HWID
         IMIN = IMAJ * RATIO

*  If the width of each annulus is less than the step between pairs of
*  annuli...
         IF ( WID .LT. RSTEP ) THEN

*  Produe an ARD description for the annulus.
            WRITE( TEXT, * ) '( ELLIPSE( 0.0, 0.0, ', OMAJ, ',',
     :                       OMIN, ',', RTOD * PA, ') .AND. .NOT. ',
     :                       'ELLIPSE( 0.0, 0.0, ', IMAJ, ',',
     :                       IMIN, ',', RTOD * PA, ') )'

*  Increment the current mask value to take account of the second
*  ellipse region in the above ARD description.
            CURVAL = CURVAL + 1

*  If there are no gaps between the annuli, there is no need for a second
*  ellipse in the ARD description.
         ELSE
            WRITE( TEXT, * ) 'ELLIPSE( 0.0, 0.0, ', OMAJ, ',', OMIN,
     :                       ',', RTOD * PA, ')'

         END IF

*  Store the ARD description for this annulus.
         CALL GRP_PUT( IGRP, 1, TEXT, 0, STATUS )

*  Find the major axis radius of the centre of the next annulus.
         CENTRE = CENTRE - RSTEP

      END DO

*  Close the parentheses opened after the original POLY keyword).
      CALL GRP_PUT( IGRP, 1, ')', 0, STATUS )

*  If the width and step are equal, we now need to blank out the inside
*  of the inner-most ellipse.
      IF ( WID .GE. RSTEP ) THEN
         WRITE( TEXT, * ) ' .AND. .NOT. ELLIPSE( 0.0, 0.0, ', IMAJ,
     :                    ',', IMIN, ',', RTOD*PA, ')'
         CALL GRP_PUT( IGRP, 1, TEXT, 0, STATUS )
      END IF

      END
