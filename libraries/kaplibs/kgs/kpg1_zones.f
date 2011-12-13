      SUBROUTINE KPG1_ZONES( ZONED, ASPKEY, ZONEF, ZONEK, ZONEG, GOK,
     :                       KOK, STATUS )
*+
*  Name:
*     KPG1_ZONES

*  Purpose:
*     Creates zones for plotting with an existing DATA zone.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ZONES( ZONED, ASPKEY, ZONEF, ZONEK, ZONEG, GOK, KOK,
*                      STATUS )

*  Description:
*     Given a current zone, and an existing DATA zone contained within
*     the current zone, this routine creates three new zones, all
*     contained within the current zone:
*
*     1) Graph window zone - this is a zone corresponding to the NCAR
*     "graph window" (i.e. the area which contains the data plot and
*     any surrounding annotation, axes, etc).  It is created with the
*     default SGS uniform co-ordinates system.  It is positioned so that
*     the existing DATA zone is centred within it, leaving a margin for
*     axes, annotation etc.  If the graph window zone would extend
*     beyond the zone current on entry to this routine, then no graph
*     window zone is created, and GOK is returned .FALSE..
*
*     2) KEY zone - this is a zone in which a key may be produced.  It
*     is position to the right of the existing DATA window, and is
*     created with the given aspect ratio (see argument ASPKEY).  Its
*     width is limited to be no more than half the maximum dimension of
*     the DATA zone.  Its top-left corner is level with the top of the
*     DATA zone and just outside the graph-window zone.  It is created
*     with the default SGS uniform co-ordinates system.  If the zone
*     would be less than 2 millimetres in size along either dimension
*     then no zone is created and KOK is returned .FALSE..
*
*     3) FRAME zone - this is a zone which just encompasses the existing
*     DATA zone and all the zones succesSfully created by this routine.
*     It is created with the default SGS uniform co-ordinates system.
*
*     On exit, the current zone is the FRAME zone.

*  Arguments:
*     ZONED = INTEGER (Given)
*        An identifier for the existing DATA zone.
*     ASPKEY = REAL (Given)
*        The aspect ratio (width/height) required for the KEY zone (in
*        which width and height are expressed in metres).
*     ZONEF = INTEGER (Returned)
*        Identifier for the FRAME zone.
*     ZONEK = INTEGER (Returned)
*        Identifier for the KEY zone.  Returned equal to zero if KOK is
*        returned .FALSE..
*     ZONEG = INTEGER (Returned)
*        Identifier for the graph window zone.  Returned equal to zero
*        if GOK is returned .FALSE..
*     GOK = LOGICAL (Returned)
*        Returned .TRUE. if a there was room within the current zone to
*        create a graph-window zone.  Returned .FALSE. otherwise.
*     KOK = LOGICAL (Returned)
*        Returned .TRUE. if a there was room within the current zone to
*        create a KEY zone.  Returned .FALSE. otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1993 (DSB):
*        Original version.
*     1995 April 13 (MJC):
*        Renamed to KPG1_ZONES.  Minor stylistic changes, and typo's
*        corrected.  Used modern-style variable declaration.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER ZONED
      REAL ASPKEY

*  Arguments Returned:
      INTEGER ZONEF
      INTEGER ZONEK
      INTEGER ZONEG
      LOGICAL GOK
      LOGICAL KOK

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL  ANCLP1               ! Fraction of the frame zone in which the
      REAL  ANCLP2               ! image will appear when there are axes
      REAL  ANCLP3
      REAL  ANCLP4
      PARAMETER( ANCLP1 = 0.18 )
      PARAMETER( ANCLP2 = 0.96 )
      PARAMETER( ANCLP3 = 0.14 )
      PARAMETER( ANCLP4 = 0.92 )

*  Local Variables:
      REAL DX1                   ! X lower bound of DATA zone
      REAL DX1U                  ! X lower bound of uniform DATA zone
      REAL DX2                   ! X upper bound of DATA zone
      REAL DX2U                  ! X upper bound of uniform DATA zone
      REAL DXM                   ! X extent of DATA zone in metres
      REAL DY1                   ! Y lower bound of DATA zone
      REAL DY1U                  ! Y lower bound of uniform DATA zone
      REAL DY2                   ! Y upper bound of DATA zone
      REAL DY2U                  ! Y upper bound of uniform DATA zone
      REAL DYM                   ! Y extent of DATA zone in metres
      REAL FACT                  ! Axis scale factor
      REAL FX1                   ! X lower bound of FRAME zone
      REAL FX2                   ! X upper bound of FRAME zone
      REAL FY1                   ! Y lower bound of FRAME zone
      REAL FY2                   ! Y upper bound of FRAME zone
      REAL GX1                   ! X lower bound of graph-window zone
      REAL GX2                   ! X upper bound of graph-window zone
      REAL GY1                   ! Y lower bound of graph-window zone
      REAL GY2                   ! Y upper bound of graph-window zone
      REAL KH                    ! Height of the key zone
      REAL KW                    ! Width of the key zone
      REAL KX1                   ! X lower bound of KEY zone
      REAL KX2                   ! X upper bound of KEY zone
      REAL KY1                   ! Y lower bound of KEY zone
      REAL KY2                   ! Y upper bound of KEY zone
      REAL X1                    ! X lower bound of current zone w.c.
      REAL X2                    ! X upper bound of current zone w.c.
      REAL XM                    ! X extent of current zone in metres
      REAL Y1                    ! Y lower bound of current zone w.c.
      REAL Y2                    ! Y upper bound of current zone w.c.
      REAL YM                    ! Y extent of current zone in metres
      INTEGER ZONE1              ! Current zone with uniform coords

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error and abort if the supplied aspect ratio for the KEY
*  zone is zero.
      IF ( ASPKEY .EQ. 0.0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_ZONES_ERR1', 'KPG1_zones: Zero aspect '/
     :     /'ratio requested for KEY zone (programming error).',
     :     STATUS )
         GO TO 999
      END IF

*  Get the world co-ordinate bounds for the current SGS zone.
      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*  Create a new zone covering the same area as the current zone, but
*  with a uniform co-ordinate system (i.e. the same scale on each axis).
      CALL SGS_ZONE( X1, X2, Y1, Y2, ZONE1, STATUS )

*  Get the extent in world co-ordinates of the new zone.  (X1,Y1) will be
*  (0,0) and MIN(X2,Y2) will be 1.0.
      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*  Select the existing DATA zone.
      CALL SGS_SELZ( ZONED, STATUS )

*  Get the bounds of the DATA zone.
      CALL SGS_IZONE( DX1, DX2, DY1, DY2, DXM, DYM )

*  Transform these bounds into the uniform co-ordinate system covering
*  the original zone.
      CALL SGS_TPZ( ZONED, DX1, DY1, ZONE1, DX1U, DY1U, STATUS )
      CALL SGS_TPZ( ZONED, DX2, DY2, ZONE1, DX2U, DY2U, STATUS )

*  Re-select the uniform zone corresponding to the original picture.
      CALL SGS_SELZ( ZONE1, STATUS )

*  Calculate the bounds of a graph-window zone which result in the DATA
*  zone covering the fractional extent of the graph-window zone given
*  by constants ANCLP1 to ANCLP4.
      FACT = ( DX2U - DX1U ) / ( ANCLP2 - ANCLP1 )
      GX1 = DX1U - ANCLP1 * FACT
      GX2 = DX2U + ( 1.0 - ANCLP2 ) * FACT

      FACT = ( DY2U - DY1U ) / ( ANCLP4 - ANCLP3 )
      GY1 = DY1U - ANCLP3 * FACT
      GY2 = DY2U + ( 1.0 - ANCLP4 ) * FACT

*  If the graph-window zone extends beyond the current zone, then no
*  annotated axes can be created.  Set the graph-window limits to be
*  the limits of the DATA zone.
      IF ( GX1 .LT. X1 .OR. GX2 .GT. X2 .OR.
     :     GY1 .LT. Y1 .OR. GY2 .GT. Y2 ) THEN
         GOK = .FALSE.
         ZONEG = 0

         GX1 = DX1U
         GX2 = DX2U
         GY1 = DY1U
         GY2 = DY2U

*  Otherwise, create the graph-window zone and then reinstate the
*  original uniform zone.
      ELSE
         GOK = .TRUE.
         CALL SGS_ZONE( GX1, GX2, GY1, GY2, ZONEG, STATUS )
         CALL SGS_SELZ( ZONE1, STATUS )
      END IF

*  The KEY zone is created to the right of the graph-window zone, and
*  the top edge is at the same y position as the top of the DATA zone.
*  The KEY zone and the graph-window zone are separated horizontally by
*  a distance equal to 0.1 of the distance between the edge of the
*  graph window and the edge of the current zone.
      KY2 = DY2U
      KX1 = GX2 + 0.1*( X2 - GX2 )

*  The rest of the space to the right of the graph-window zone is
*  available for the KEY zone.  However, if this would result in a KEY
*  zone wider than half the maximum dimension of the DATA zone, then
*  limit the KEY zone width.
      KW = MIN( MAX( 0.0, X2 - KX1 ),
     :          0.5 * MAX( DX2U - DX1U, DY2U - DY1U ) )

*  The height of the KEY zone is then defined by the supplied aspect
*  ratio.
      KH = KW / ASPKEY

*  Set up the Y co-ordinate of the bottom of the KEY zone.
      KY1 = KY2 - KH

*  If the bottom edge of the KEY zone would be below the current
*  zone, limit the KEY zone height and find the corresponding width.
      IF ( KY1 .LT. Y1 ) THEN
         KY1 = Y1
         KH = KY2 - KY1
         KW = ASPKEY * KH
      END IF

*  Set up the x co-ordinate of the right edge of the KEY zone.
      KX2 = KX1 + KW

*  Find the height and width of the KEY zone in metres.
      KW = XM * KW / ( X2 - X1 )
      KH = YM * KH / ( Y2 - Y1 )

*  If the resulting KEY zone is less than 2mm in either dimension, or
*  extends beyond the current zone, no key can be produced.
      IF ( KX1 .LT. X1 .OR. KX2 .GT. X2 .OR.
     :     KY1 .LT. Y1 .OR. KY2 .GT. Y2 .OR.
     :     KW .LT. 2.0E-3 .OR. KH .LT. 2.0E-3 ) THEN
         KOK = .FALSE.
         ZONEK = 0

*  Otherwise, create the KEY zone and then reinstate the original
*  uniform zone.
      ELSE
         KOK = .TRUE.
         CALL SGS_ZONE( KX1, KX2, KY1, KY2, ZONEK, STATUS )
         CALL SGS_SELZ( ZONE1, STATUS )
      END IF

*  Create a FRAME zone covering all the successfully created zones.
      FX1 = GX1
      FX2 = GX2
      FY1 = GY1
      FY2 = GY2

      IF ( KOK ) THEN
         FX1 = MIN( FX1, KX1 )
         FX2 = MAX( FX2, KX2 )
         FY1 = MIN( FY1, KY1 )
         FY2 = MAX( FY2, KY2 )
      END IF

      CALL SGS_ZONE( FX1, FX2, FY1, FY2, ZONEF, STATUS )

*  Release the uniform zone.
      CALL SGS_RELZ( ZONE1 )

 999  CONTINUE

      END
