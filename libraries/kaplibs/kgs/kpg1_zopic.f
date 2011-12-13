      SUBROUTINE KPG1_ZOPIC( PNXSIZ, PNYSIZ, COMMNT, ASPKEY, XLO, XHI,
     :                       YLO, YHI, KEY, AXES, ZONEF, ZONED, ZONEK,
     :                       ZONEG, PICIDF, XM, YM, STATUS )
*+
*  Name:
*     KPG1_ZOPIC

*  Purpose:
*     Creates zones required to produce a plot with key and axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ZOPIC( PNXSIZ, PNYSIZ, COMMNT, ASPKEY, XLO, XHI, YLO,
*                      YHI, KEY, AXES, ZONEF, ZONED, ZONEK, ZONEG,
*                      PICIDF, XM, YM, STATUS )

*  Description:
*     This routine creates the zones required for the various parts of a plot
*     (FRAME, KEY, DATA and graph window).  If a DATA picture is
*     contained within the current picture then it is used to define
*     the zone in which to produce the data plot.  In this case, the
*     bounds of the displayed DATA picture (in pixel co-ordinates) are
*     returned in SXLO, etc.  Otherwise, the user is asked for a FRAME
*     picture size and the DATA picture is created within it using the
*     supplied values of SXLO, etc.  The current zone is unchanged on
*     exit.  A picture corresponding to the new FRAME zone is stored in
*     the AGI database.

*  Arguments:
*     PNXSIZ = CHARACTER * ( * ) (Given)
*        The name of the parameter to use when obtaining the x size of
*        a FRAME picture.  Only used if the current picture does not
*        contain a DATA picture.
*     PNXSIZ = CHARACTER * ( * ) (Given)
*        The name of the parameter to use when obtaining the y size of
*        a FRAME picture.  Only used if the current picture does not
*        contain a DATA picture.
*     COMMNT = CHARACTER * ( * ) (Given)
*        A comment to store with the new FRAME picture.
*     ASPKEY = REAL (Given)
*        The aspect ratio required for the KEY zone.
*     XLO = REAL (Given and Returned)
*        On entry: The lower x bound of the data array available for
*        plotting, in pixel co-ordinates.
*        On exit: The returned value depends on whether or not a DATA
*        picture was found within the current picture.  If a DATA
*        picture was found, the returned value is the lower x bound of
*        the DATA picture.  Otherwise the returned value is equal to
*        the supplied value.
*     XHI = REAL (Given and Returned)
*        The upper x bound of the DATA zone.  See XLO.
*     YLO = REAL (Given and Returned)
*        The lower y bound of the DATA zone.  See XLO.
*     YHI = REAL (Given and Returned)
*        The upper y bound of the DATA zone.  See XLO.
*     KEY = LOGICAL (Given and Returned)
*        On entry: whether or not a key has been requested.
*        On exit: set .FALSE. if a key was requested but there is
*        insufficient room within the current zone to produce one.  A
*        warning message is issued if this occurs.  Otherwise it
*        retains its value on entry.
*     AXES = LOGICAL (Given and Returned)
*        On entry: whether or not annotated axes have been requested.
*        On exit: set .FALSE. if annotated axes were requested but
*        there is insufficient room within the current zone to produce
*        them.  A warning message is issued if this occurs.  Otherwise
*        it retains its value on entry.
*     ZONEF = INTEGER (Returned)
*        Identifier for the FRAME zone.
*     ZONED = INTEGER (Returned)
*        Identifier for the DATA zone.
*     ZONEK = INTEGER (Returned)
*        Identifier for the KEY zone.  Returned set to zero if KEY is
*        returned .FALSE..
*     ZONEG = INTEGER (Returned)
*        Identifier for the graph window zone.  Returned set to zero if
*        AXES is returned .FALSE..
*     PICIDF = INTEGER (Returned)
*        AGI identifier for the FRAME picture.
*     XM = REAL (Returned)
*        The physical x dimension of the DATA zone in metres.
*     YM = REAL (Returned)
*        The physical y dimension of the DATA zone in metres.
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
*     21-SEP-1993 (DSB):
*        Original version.
*     1995 April 13 (MJC):
*        Renamed to KPG1_ZOPIC.  Minor stylistic changes, and typo's
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
      CHARACTER * ( * ) PNXSIZ
      CHARACTER * ( * ) PNYSIZ
      CHARACTER * ( * ) COMMNT
      REAL ASPKEY

*  Arguments Given and Returned:
      REAL XLO
      REAL XHI
      REAL YLO
      REAL YHI
      LOGICAL KEY
      LOGICAL AXES

*  Arguments Returned:
      INTEGER ZONEF
      INTEGER ZONED
      INTEGER ZONEK
      INTEGER ZONEG
      INTEGER PICIDF
      REAL XM
      REAL YM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL  ANCLP1               ! Fraction of the frame zone in which the
      REAL  ANCLP2               ! image will appear when there are axes
      REAL  ANCLP3
      REAL  ANCLP4
      PARAMETER( ANCLP1 = 0.19 )
      PARAMETER( ANCLP2 = 0.95 )
      PARAMETER( ANCLP3 = 0.15 )
      PARAMETER( ANCLP4 = 0.91 )

*  Local Variables:
      REAL ASPDAT                ! Aspect ratio required for DATA zone
      REAL DX1                   ! X lower bound of DATA zone
      REAL DX2                   ! X upper bound of DATA zone
      REAL DY1                   ! Y lower bound of DATA zone
      REAL DY2                   ! Y upper bound of DATA zone
      LOGICAL GOK                ! Was the graph window zone created OK?
      LOGICAL KOK                ! Was the KEY zone created OK?
      REAL KX                    ! X of point on top edge of DATA zone
      REAL KY                    ! Y of point on top edge of DATA zone
      INTEGER PICID0             ! Current picture on entry
      INTEGER PICIDD             ! Identifier for existing DATA picture
      REAL X1                    ! X lower bound of zone in w.c.
      REAL X2                    ! X upper bound of zone in w.c.
      REAL Y1                    ! Y lower bound of zone in w.c.
      REAL Y2                    ! Y upper bound of zone in w.c.
      INTEGER ZONE0              ! Current zone on entry
      INTEGER ZONEK2             ! New key zone identifier

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get identifiers for the current SGS zone and AGI picture.
      CALL SGS_ICURZ( ZONE0 )
      CALL AGI_ICURP( PICID0, STATUS )

*  Mark the error stack so that errors reported as a result of a DATA
*  picture not being found will be deferred.
      CALL ERR_MARK

*  See if the current picture is a DATA picture or contains a DATA
*  picture.
      CALL KPG1_AGFND( 'DATA', PICIDD, STATUS )

*  If a DATA picture was found...
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Release the error stack.
         CALL ERR_RLSE

*  Create an SGS zone from the DATA picture.  It is assumed that the
*  world co-ordinates of the DATA picture correspond to pixel
*  co-ordinates.
         CALL AGS_NZONE( ZONED, STATUS )

*  Reselect the original AGI picture, and zone.
         CALL AGI_SELP( PICID0, STATUS )
         CALL SGS_SELZ( ZONE0, STATUS )

*  Annul the identifier for the DATA picture.
         CALL AGI_ANNUL( PICIDD, STATUS )

*  Create the zones needed for the various parts of the plot.  These are
*  a `graph window' zone in which annotated axes are produced (and
*  within which the DATA zone lies), a KEY zone in which the key is
*  produced, and a FRAME zone containing all the other zones.  The new
*  zones are created with the default SGS world co-ordinate system (i.e.
*  (0,0) is the bottom left corner and (1,1) lies on either the top
*  or right hand edge), and the FRAME zone is made current.  Flags are
*  returned indicating if there was insufficient room to create the
*  graph window zone or key zone.
         CALL KPG1_ZONES( ZONED, ASPKEY, ZONEF, ZONEK, ZONEG, GOK, KOK,
     :                    STATUS )

*  Store the current zone (the FRAME zone) as a new AGI picture.
         CALL AGS_SZONE( 'FRAME', COMMNT, PICIDF, STATUS )

*  Issue a warning if a key was requested but there is insufficient
*  room to produce one.
         IF ( KEY .AND. (.NOT.KOK) ) THEN
            CALL MSG_OUT( 'KPG1_ZOPIC_MSG1', 'WARNING: There is '/
     :                    /'insufficient room within the current '/
     :                    /'picture to produce a key.', STATUS )
            KEY = .FALSE.
         END IF

*  If a KEY zone has been created but no key was requested, release the
*  key zone.
         IF ( ( .NOT. KEY ) .AND. KOK ) THEN
            CALL SGS_RELZ( ZONEK )
            ZONEK = 0
         END IF

*  Issue a warning if annotated axes were requested but there is
*  insufficient room to produce them.
         IF ( AXES .AND. (.NOT.GOK) ) THEN
            CALL MSG_OUT( 'KPG1_ZOPIC_MSG2', 'WARNING: There is '/
     :        /'insufficient room within the current picture to '/
     :        /'produce annotated axes.', STATUS )
            AXES = .FALSE.
         END IF

*  If a graph-window zone has been created but no annotated axes were
*  requested, release the graph-window zone.
         IF ( ( .NOT. AXES ) .AND. GOK ) THEN
            CALL SGS_RELZ( ZONEG )
            ZONEG = 0
         END IF

*  If no DATA picture was found within the current picture, annul the
*  error and release the error stack.
      ELSE
         CALL ERR_ANNUL( STATUS )
         CALL ERR_RLSE

*  Create and store a FRAME picture within the graphics database with a
*  size specified by the user.  The new picture and zone become current.
         CALL KPG1_FRPIC( PNXSIZ, PNYSIZ, COMMNT, .FALSE., ZONEF,
     :                    PICIDF, STATUS )

*  Calculate the aspect ratio required for the DATA zone.
         IF ( YHI .NE. YLO ) THEN
            ASPDAT = ( XHI - XLO ) / ( YHI - YLO )
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPG1_ZOPIC_ERR1', 'KPG1_ZOPIC: DATA zone '/
     :        /'has zero height (possible programming error).', STATUS )
            GO TO 999
         END IF

*  If a key is required, the current (FRAME) zone is divided into two
*  zones, one for the graph window (containing data plot and axes) and
*  one for the key, both with suitable aspect ratios.  If no key is
*  required, then a single zone is created within the current zone
*  which is the largest zone with the correct aspect ratio.  The current
*  zone is unchanged.  The new zones are created with the default SGS
*  world co-ordinate system.
         CALL KPG1_KEYZO( KEY, ASPKEY, ASPDAT, ZONEG, ZONEK, STATUS )

*  Select the graph-window zone, and get its world co-ordinate bounds.
         CALL SGS_SELZ( ZONEG, STATUS )
         CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*  Now create a DATA zone within the graph-window zone.  The extent of
*  this zone depends on whether or not space is to be left within the
*  graph window for annotated axes.  If no axes are required it occupies
*  the whole of the graph-window zone.
         IF ( .NOT. AXES ) THEN
            DX1 = X1
            DX2 = X2
            DY1 = Y1
            DY2 = Y2

*  Otherwise, it occupies 0.76 of each dimension of the graph window.
*  Create the zone and modify the physical extent of the DATA zone.
         ELSE
            DX1 = X1 + ( X2 - X1 ) * ANCLP1
            DX2 = X1 + ( X2 - X1 ) * ANCLP2
            DY1 = Y1 + ( Y2 - Y1 ) * ANCLP3
            DY2 = Y1 + ( Y2 - Y1 ) * ANCLP4

            XM = XM * ( ANCLP2 - ANCLP1 )
            YM = YM * ( ANCLP4 - ANCLP3 )

         END IF

* Create the DATA zone.
         CALL SGS_ZONE( DX1, DX2, DY1, DY2, ZONED, STATUS )

*  Set the world co-ordinates of the DATA zone to be pixel co-ordinates.
         CALL SGS_SW( XLO, XHI, YLO, YHI, STATUS )

*  If a KEY was produced, select the key zone.
         IF ( KEY ) THEN
            CALL SGS_SELZ( ZONEK, STATUS )

*  Get the extent of the KEY zone.
            CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*  Find the y co-ordinate within the key zone corresponding to the top of
*  the data zone.
            CALL SGS_TPZ( ZONED, XLO, YHI, ZONEK, KX, KY, STATUS )

*  If the KEY zone extends above the top of the DATA zone, truncate it
*  (preserving its aspect ratio).
            IF ( KY .LT. Y2 ) THEN
               X2 = X1 + ( X2 - X1 ) * ( KY - Y1 ) / ( Y2 - Y1 )
               CALL SGS_ZONE( X1, X2, Y1, KY, ZONEK2, STATUS )
               CALL SGS_RELZ( ZONEK )
               ZONEK = ZONEK2
            END IF

         END IF

*  If annotated axes have not been requested, release the graph-window
*  zone.
         IF ( .NOT. AXES ) THEN
            CALL SGS_RELZ( ZONEG )
            ZONEG = 0
         END IF

*  If no key has been requested, ensure a value of zero is returned for
*  the zone identifier.  (KPG1_KEYZO will not have created a key zone so
*  there is no need to release it).
         IF ( .NOT. KEY ) ZONEK = 0

      END IF

*  Obtain and return the world co-ordinate bounds of the DATA zone.
      CALL SGS_SELZ( ZONED, STATUS )
      CALL SGS_IZONE( XLO, XHI, YLO, YHI, XM, YM )

*  Re-select the original picture and zone.
      CALL AGI_SELP( PICID0, STATUS )
      CALL SGS_SELZ( ZONE0, STATUS )

*  Arrive here if an error occurs.
 999  CONTINUE

      END
