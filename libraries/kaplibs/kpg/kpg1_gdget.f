      SUBROUTINE KPG1_GDGET( IPIC, WCFRM, MKDATA, IPLOT, STATUS )
*+
*  Name:
*     KPG1_GDGET

*  Purpose:
*     Gets the AST Plot associated with a graphics-database picture.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GDGET( IPIC, WCFRM, MKDATA, IPLOT, STATUS )

*  Description:
*     This routine makes the specified graphics-database (AGI) picture
*     current, creates a corresponding PGPLOT viewport and window, and
*     returns a Plot associated with the picture.
*
*     On exit, the PGPLOT viewport corresponds to the area encompassed by
*     the specified picture. The world co-ordinate bounds within this
*     viewport are set so that the PGPLOT world co-ordinate system is
*     millimetres from the bottom-left corner of the view surface. This
*     corresponds to the Base (GRAPHICS) Frame in the returned Plot.
*
*     The returned Plot will normally be obtained from the MORE structure
*     in the graphics database (where it was stored by a previous AST-based
*     application). The Base Frame will be a GRAPHICS Frame, giving
*     millimetres from the bottom-left corner of the view surface.
*
*     If no Plot is available in the database, then an initial PLOT is
*     created containing a Base Frame with Domain GRAPHICS (giving
*     millimetres from the bottom-left corner of the view surface), and
*     a Current Frame corresponding to AGI world co-ordinates. The Frame
*     to represent AGI world co-ordinates in the Plot may be supplied by
*     the calling application (for instance, an application may supply a
*     PIXEL Frame on the assumption that AGI world co-ordinates are pixel
*     co-ordinates). If no such Frame is supplied then a simple Frame is
*     used, with Domain set to AGI_WORLD.
*
*     A third Frame may optionally be added to the Plot representing AGI
*     DATA co-ordinates. This Frame will have Domain AGI_DATA, and the
*     Mapping from world to data co-ordinates will be given by the
*     TRANSFORM structure stored with the picture in the database. If
*     present, it will be the Current Frame in the Plot on exit. See
*     "Usage" below for warnings about using this option.
*
*     Finally, some other Frames are added to the Plot representing
*     various normalised co-ordinates:
*
*     BASEPIC: The co-ordinates of the bottom-left corner of the BASE
*     picture are (0,0). The shorter dimension of the BASE picture has
*     length 1.0, and the other axis has a length greater than 1.0.
*
*     NDC: Normalized device co-ordinates. The bottom-left corner of the
*     screen is (0,0) and the top-right corner is (1,1).
*
*     CURPIC: The co-ordinates of the bottom-left corner of the current
*     picture are (0,0). The shorter dimension of the current picture has
*     length 1.0, and the other axis has a length greater than 1.0.
*
*     CURNDC: The co-ordinates of the bottom-left corner of the current
*     picture are (0,0), and the top-right corner is (1,1).
*
*     If the Plot read from the database already contains any of these
*     Frames then they are retained and no new Frame is added.

*  Usage:
*     -  To create a new AGI picture, an AST-based application should
*     call routine AGP_SVIEW to store the bounds of the current PGPLOT
*     viewport as a new picture in the AGI database. The bounds of the
*     viewport in PGPLOT world co-ordinates will be saved as AGI world
*     co-ordinates in the AGI database. Non-AST applications will use this
*     information and so PGPLOT world co-ordinates should be set
*     appropriately before calling AGP_SVIEW (for instance, DISPLAY sets
*     them to pixel co-ordinates in the displayed NDF). Note, AST-based
*     applications do not use the world co-ordinate information stored in
*     AGI database (see below).
*     -  After calling AGP_SVIEW, this routine (KPG1_GDGET) should be
*     called to obtain an AST Plot for the new picture. Since the picture
*     has just been created, it will not as yet have a Plot stored with it
*     in the database. An initial Plot is created in which the Base Frame
*     is a GRAPHICS Frame (giving millimetres from the bottom-left corner
*     of the view surface), and the Current Frame gives AGI world
*     co-ordinates (as stored with the picture by AGP_SVIEW). The Frame
*     specified by argument WCFRM is used for this purpose (a default
*     Frame with Domain AGI_WORLD is used if no Frame is supplied). For
*     instance, DISPLAY supplies a PIXEL Frame for WCFRM to identify AGI
*     world co-ordinates as pixel co-ordinates. As well as creating a Plot,
*     this routine also sets PGPLOT world co-ordinates so that they
*     correspond to GRAPHICS co-ordinates (millimetres from bottom left
*     corner of the view surface).
*     -  The application can then modify the returned Plot, for instance
*     by setting plotting style attributes, or adding extra Frames.
*     -  Finally, KPG1_GDPUT should be called to store the modified Plot
*     with the picture in the database. If a non-AST application
*     subsequently accesses the picture, it will ignore the Plot and use
*     the AGI world co-ordinates in the usual manner. AST-based
*     applications will ignore the AGI world co-ordinate bounds, and use
*     the information stored with the Plot instead.
*     -  If an AST-based application accesses an existing AGI picture, it
*     should first call this routine (KPG1_GDGET) to create a Plot defining
*     the co-ordinate systems associated with the picture. If a Plot is
*     stored with the picture in the database, then it will be returned.
*     Otherwise, an initial Plot will be created containing a GRAPHICS
*     Frame and a world co-ordinate Frame as described above. A DATA
*     co-ordinate Frame can optionally be included in the initial Plot
*     (see below).
*     -  If an AST-based application accesses an existing picture which has
*     an associated TRANSFORM structure, then the Plot created by this
*     routine can optionally include a Frame with Domain AGI_DATA. This
*     Frame will be connected to the world co-ordinate Frame using an AST
*     IntraMap which encapsulates the mapping implemented by the TRANSFORM
*     structure in the AGI database. If this option is selected, there are
*     some restrictions on what can and cannot be done with the returned Plot,
*     because of the potential presence of the IntraMap:
*        1) The Plot must not be combined with another Plot if the other
*        Plot may also contain an AGI_DATA Frame.
*        2) Before using the Plot (or a derived Mapping) to transform
*        positions (e.g. using AST_TRAN2), it should be ensured that the
*        picture from which the Plot was created is the current AGI
*        picture.
*     Because of these restrictions, the option to create an AGI_DATA
*     Frame should only be used when necessary.

*  Arguments:
*     IPIC = INTEGER (Given)
*        The AGI identifier for the picture. If a value of -1 is supplied,
*        the identifier for the current picture is used.
*     WCFRM = INTEGER (Given)
*        A pointer to an AST Frame which will be used to describe AGI
*        world co-ordinates if the picture has no associated Plot. This
*        argument is ignored if the picture already has a Plot stored with
*        it in the database. If a null pointer (AST__NULL) is supplied, then
*        a default Frame is used with Domain set to AGI_WORLD.
*     MKDATA = LOGICAL (Given)
*        Should a Frame with Domain AGI_DATA be included in the returned
*        Plot to represent AGI DATA co-ordinates? This is ignored if the
*        picture has a Plot already stored with it in the database.
*     IPLOT = INTEGER (Returned)
*        An AST pointer to the Plot. Returned equal to AST__NULL if an
*        error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The PGPLOT interface to the AGI library should be opened before
*     calling this routine.

*  Copyright:
*     Copyright (C) 1998, 1999, 2000, 2001, 2002 Central Laboratory of the Research Councils.
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
*     2-SEP-1998 (DSB):
*        Original version.
*     15-DEC-1998 (DSB):
*        Modified to include the BASEPIC Frame in the returned Plot.
*     14-OCT-1999 (DSB):
*        Added CURPIC Frame.
*     14-FEB-2000 (DSB):
*        Set BASEPIC and CURPIC Format to %.3f to avoid loads of
*        unnecessary digits being displayed by programs such as GDSTATE.
*     4-DEC-2001 (DSB):
*        Added NDC Frame.
*     13-AUG-2002 (DSB):
*        Added CURNDC Frame.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'AGI_PAR'          ! AGI constants

*  Arguments Given:
      INTEGER IPIC
      INTEGER WCFRM
      LOGICAL MKDATA

*  Arguments Returned:
      INTEGER IPLOT

*  Status:
      INTEGER STATUS               ! Global status

*  External References:
      EXTERNAL KPG1_ASAGD          ! Encapsulates a TRANSFORM strcuture

*  Local Variables:
      CHARACTER MORLOC*(DAT__SZLOC)! HDS locator for picture's MORE structure
      CHARACTER NAME*(AGI__SZNAM)  ! Name stored with required picture
      DOUBLE PRECISION DBX( 4 )    ! D.P. version of BX
      DOUBLE PRECISION INA( 2 )    ! Corner A in GRAPHICS co-ords
      DOUBLE PRECISION INB( 2 )    ! Corner B in GRAPHICS co-ords
      DOUBLE PRECISION OUTA( 2 )   ! Corner A in BASEPIC or CURPIC co-ords
      DOUBLE PRECISION OUTB( 2 )   ! Corner B in BASEPIC or CURPIC co-ords
      INTEGER WMAP                 ! GRAPHICS->BASEPIC/CURPIC WinMap
      INTEGER BPIC                 ! BASEPIC Frame
      INTEGER CPIC                 ! CURPIC Frame
      INTEGER CNDC                 ! CURNDC Frame
      INTEGER NPIC                 ! NDC Frame
      INTEGER IBPIC                ! Index of BASEPIC Frame
      INTEGER ICNDC                ! Index of CURNDC Frame
      INTEGER ICPIC                ! Index of CURPIC Frame
      INTEGER INPIC                ! Index of NDC Frame
      INTEGER ICURR                ! Index of original current Frame
      INTEGER IPICL                ! AGI identifier for picture to use
      INTEGER IWOCO                ! Index of AGI world co-ordinates Frame
      INTEGER MAP                  ! Pointer to WORLD -> DATA Mapping
      LOGICAL MORE                 ! Does picture have a MORE structure?
      REAL BX( 4 )                 ! Bounds of plotting area
*.

*  Initialise returned values.
      IPLOT = AST__NULL

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Ensure all KAPPA AST IntraMaps are registered.
      CALL KPG1_ASREG( STATUS )

*  If a picture was specified, make it current.
      IF( IPIC .NE. -1 ) THEN
         IPICL = IPIC
         CALL AGI_SELP( IPIC, STATUS )

*  Otherwise, store the current picture identifier.
      ELSE
         CALL AGI_ICURP( IPICL, STATUS )
      END IF

*  Get the name of the selected picture.
      CALL AGI_INAME( NAME, STATUS )

*  Create a PGPLOT viewport from the picture. The viewport covers the
*  entire picture (no border is left), and has world co-ordinates defined
*  by the AGI database.
      CALL AGP_NVIEW( .FALSE., STATUS )

*  See if there is a MORE structure associated with the picture.
      CALL AGI_IMORE( IPICL, MORE, STATUS )

*  If so we see if the MORE structure contains an AST Plot.
      IF( MORE ) THEN

*  Get an HDS locator to the MORE structure.
         CALL AGI_MORE( IPICL, 'READ', MORLOC, STATUS )

*  Attempt to read an AST Object from the AST_PLOT component of the HDS
*  structure. No error is reported if there is no AST_PLOT component.
         CALL KPG1_WREAD( MORLOC, 'AST_PLOT', IPLOT, STATUS )

*  Annul the locator to the MORE structure.
         CALL DAT_ANNUL( MORLOC, STATUS )

*  If the AST Object read from the database is not a Plot, report an error.
         IF( IPLOT .NE. AST__NULL ) THEN
            IF( .NOT. AST_ISAPLOT( IPLOT, STATUS ) ) THEN
               IF( STATUS .EQ. SAI__OK ) THEN
                  CALL AST_ANNUL( IPLOT, STATUS )

                  CALL MSG_SETC( 'NAME', NAME )
                  CALL MSG_SETC( 'CLASS', AST_GETC( IPLOT, 'CLASS',
     :                                                 STATUS ) )

                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'KPG1_GDGET_1', 'The required ^NAME '//
     :                       'picture has an AST ^CLASS associated '//
     :                       'with it in the AGI database. This '//
     :                       'application requires an AST Plot.',
     :                       STATUS )
               END IF

            END IF

         END IF

      END IF

*  If a Plot was obtained, ensure that the current PGPLOT window corresponds
*  to millimetres from the bottom-left corner of the view surface. This is
*  the co-ordinate system used by the Base (GRAPHICS) Frame in the Plot.
      IF( IPLOT .NE. AST__NULL ) THEN
         CALL PGQVP( 2, BX( 1 ), BX( 3 ), BX( 2 ), BX( 4 ) )
         CALL PGSWIN( BX( 1 ), BX( 3 ), BX( 2 ), BX( 4 ) )

*  If no Plot was obtained, return a Plot containing the GRAPHICS Frame,
*  an AGI world co-ordinates Frame and (optionally) an AGI Data
*  co-ordinates Frame.
      ELSE

*  Get the AGI world co-ordinate bounds of the specified picture,
*  and convert to double precision.
         CALL PGQWIN( BX( 1 ), BX( 3 ), BX( 2 ), BX( 4 ) )
         DBX( 1 ) = DBLE( BX( 1 ) )
         DBX( 2 ) = DBLE( BX( 2 ) )
         DBX( 3 ) = DBLE( BX( 3 ) )
         DBX( 4 ) = DBLE( BX( 4 ) )

*  Create the Plot. This sets the PGPLOT world co-ordinate system in the
*  current viewport to millimetres from the bottom-left corner.
         CALL KPG1_ASPLT( WCFRM, DBX, ' ', IPLOT, STATUS )

*  If no Frame was supplied, a default two-dimensional Frame will have been used. Set
*  its Domain to AGI_WORLD. Also set its Title and axis Symbols.
         IF( WCFRM .EQ. AST__NULL ) THEN
            CALL AST_SETC( IPLOT, 'Domain', 'AGI_WORLD', STATUS )
            CALL AST_SET( IPLOT, 'Title=World co-ordinates,'//
     :                    'symbol(1)=X,symbol(2)=Y', STATUS )
         END IF

*  If required, add an AGI_DATA Frame into the returned Plot.
         IF( MKDATA ) THEN

*  Get the Frame index of the AGI world co-ordinate Frame.
            IWOCO = AST_GETI( IPLOT, 'CURRENT', STATUS )

*  Create an "ASAGD" IntraMap to encapsulate the TRANSFORM structure stored in
*  the AGI database. It maps AGI WORLD co-ords into AGI data co-ords
*  using the TRANSFORM structure for the current AGI picture.
            MAP = AST_INTRAMAP( 'ASAGD', 2, 2, ' ', STATUS )

*  Create the Frame and add it into the Plot, using the above IntraMap to
*  connect it to the world co-ordinate Frame. The new Frame becomes the
*  Current Frame.
            CALL AST_ADDFRAME( IPLOT, IWOCO, MAP,
     :                         AST_FRAME( 2, ' ', STATUS ), STATUS )

*  Set its Domain to AGI_DATA, and set its Title and axis Symbols.
            CALL AST_SETC( IPLOT, 'Domain', 'AGI_DATA', STATUS )
            CALL AST_SET( IPLOT, 'Title=Data co-ordinates,'//
     :                    'symbol(1)=X,symbol(2)=Y', STATUS )

         END IF

      END IF

*  Add a Frame representing AGI world co-ordinates in the BASE picture.
*  This picture has equals scales on both axes, and the shorter axis
*  has a length of 1.0. This Frame is given the Domain BASEPIC.
*  ====================================================================

*  See if the Plot already contains a BASEPIC Frame.
      CALL KPG1_ASFFR( IPLOT, 'BASEPIC', IBPIC, STATUS )

*  If not, add one into the Plot now.
      IF( IBPIC .EQ. AST__NOFRAME ) THEN

*  Get the bounds of the entire view surface in millimetres.
         CALL PGQVSZ( 2, BX( 1 ), BX( 3 ), BX( 2 ), BX( 4 ) )

*  Store these as the GRAPHICS co-ordinates.
         INA( 1 ) = DBLE( BX( 1 ) )
         INA( 2 ) = DBLE( BX( 2 ) )
         INB( 1 ) = DBLE( BX( 3 ) )
         INB( 2 ) = DBLE( BX( 4 ) )

*  We now find the bounds of the view surface in BASEPIC co-ordinates (i.e.
*  co-ordinates normalised so that the shorter axis has length 1.0).
         IF( ABS( BX( 3 ) - BX( 1 ) ) .GT.
     :       ABS( BX( 4 ) - BX( 2 ) ) ) THEN
            OUTA( 1 ) = 0.0D0
            OUTA( 2 ) = 0.0D0
            OUTB( 1 ) = DBLE( ABS( BX( 3 ) - BX( 1 ) ) /
     :                        ABS( BX( 4 ) - BX( 2 ) ) )
            OUTB( 2 ) = 1.0D0
         ELSE
            OUTA( 1 ) = 0.0D0
            OUTA( 2 ) = 0.0D0
            OUTB( 1 ) = 1.0D0
            OUTB( 2 ) = DBLE( ABS( BX( 4 ) - BX( 2 ) ) /
     :                        ABS( BX( 3 ) - BX( 1 ) ) )
         END IF

*  Create a WinMap which scales millimetres into BASE_WORLD co-ordinates.
         WMAP = AST_WINMAP( 2, INA, INB, OUTA, OUTB, ' ', STATUS )

*  Create the BASEPIC Frame.
         BPIC = AST_FRAME( 2, 'DOMAIN=BASEPIC,TITLE=Normalised world '//
     :                     'co-ordinates in the AGI BASE picture.,'//
     :                     'Symbol(1)=X,Symbol(2)=Y,'//
     :                     'Label(1)=Horizontal offset,'//
     :                     'Label(2)=Vertical offset,'//
     :                     'Format(1)=%.3f,Format(2)=%.3f',
     :                     STATUS )

*  Save the original current Frame index.
         ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )

*  Add the BASEPIC Frame into the Plot.
         CALL AST_ADDFRAME( IPLOT, AST__BASE, WMAP, BPIC, STATUS )

*  If the picture is a DATA picture, re-instate the original current Frame
*  index. Otherwise, leave the BASEPIC Frame as the current Frame.
         IF( NAME .EQ. 'DATA' ) THEN
            CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )
         END IF

      END IF

*  Add a Frame representing normalized device co-ordinates. This picture
*  has (in general) un-equals scales on each axis. The bottom-left corner
*  is (0,0) and the top right is (1,1). This Frame is given the Domain NDC.
*  ====================================================================

*  See if the Plot already contains an NDC Frame.
      CALL KPG1_ASFFR( IPLOT, 'NDC', INPIC, STATUS )

*  If not, add one into the Plot now.
      IF( INPIC .EQ. AST__NOFRAME ) THEN

*  Get the bounds of the entire view surface in millimetres.
         CALL PGQVSZ( 2, BX( 1 ), BX( 3 ), BX( 2 ), BX( 4 ) )

*  Store these as the GRAPHICS co-ordinates.
         INA( 1 ) = DBLE( BX( 1 ) )
         INA( 2 ) = DBLE( BX( 2 ) )
         INB( 1 ) = DBLE( BX( 3 ) )
         INB( 2 ) = DBLE( BX( 4 ) )

*  Store the bounds of the view surface in NDC co-ordinates.
         OUTA( 1 ) = 0.0D0
         OUTA( 2 ) = 0.0D0
         OUTB( 1 ) = 1.0D0
         OUTB( 2 ) = 1.0D0

*  Create a WinMap which scales millimetres into NDC co-ordinates.
         WMAP = AST_WINMAP( 2, INA, INB, OUTA, OUTB, ' ', STATUS )

*  Create the NDC Frame.
         NPIC = AST_FRAME( 2, 'DOMAIN=NDC,TITLE=Normalised device '//
     :                     'co-ordinates.,'//
     :                     'Symbol(1)=X,Symbol(2)=Y,'//
     :                     'Label(1)=Horizontal offset,'//
     :                     'Label(2)=Vertical offset,'//
     :                     'Format(1)=%.3f,Format(2)=%.3f',
     :                     STATUS )

*  Save the original current Frame index.
         ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )

*  Add the NDC Frame into the Plot.
         CALL AST_ADDFRAME( IPLOT, AST__BASE, WMAP, NPIC, STATUS )

*  Re-instate the original current Frame index.
         CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

      END IF

*  Add a Frame representing a normalised co-ordinate system in the current
*  picture. This Frame has equals scales on both axes, and the shorter axis
*  has a length of 1.0. This Frame is given the Domain CURPIC.
*  ====================================================================

*  See if the Plot already contains a CURPIC Frame.
      CALL KPG1_ASFFR( IPLOT, 'CURPIC', ICPIC, STATUS )

*  If not, add one into the Plot now.
      IF( ICPIC .EQ. AST__NOFRAME ) THEN

*  Get the bounds of the current window (in millimetres).
         CALL PGQWIN( BX( 1 ), BX( 3 ), BX( 2 ), BX( 4 ) )

*  Store these as the GRAPHICS co-ordinates.
         INA( 1 ) = DBLE( BX( 1 ) )
         INA( 2 ) = DBLE( BX( 2 ) )
         INB( 1 ) = DBLE( BX( 3 ) )
         INB( 2 ) = DBLE( BX( 4 ) )

*  We now find the bounds of the current window in CURPIC co-ordinates (i.e.
*  co-ordinates normalised so that the shorter axis has length 1.0).
         IF( ABS( BX( 3 ) - BX( 1 ) ) .GT.
     :       ABS( BX( 4 ) - BX( 2 ) ) ) THEN
            OUTA( 1 ) = 0.0D0
            OUTA( 2 ) = 0.0D0
            OUTB( 1 ) = DBLE( ABS( BX( 3 ) - BX( 1 ) ) /
     :                        ABS( BX( 4 ) - BX( 2 ) ) )
            OUTB( 2 ) = 1.0D0
         ELSE
            OUTA( 1 ) = 0.0D0
            OUTA( 2 ) = 0.0D0
            OUTB( 1 ) = 1.0D0
            OUTB( 2 ) = DBLE( ABS( BX( 4 ) - BX( 2 ) ) /
     :                        ABS( BX( 3 ) - BX( 1 ) ) )
         END IF

*  Create a WinMap which scales millimetres into CURPIC co-ordinates.
         WMAP = AST_WINMAP( 2, INA, INB, OUTA, OUTB, ' ', STATUS )

*  Create the CURPIC Frame.
         CPIC = AST_FRAME( 2, 'DOMAIN=CURPIC,TITLE=Normalised world '//
     :                     'co-ordinates in the current AGI picture.,'//
     :                     'Symbol(1)=X,Symbol(2)=Y,'//
     :                     'Label(1)=Horizontal offset,'//
     :                     'Label(2)=Vertical offset,'//
     :                     'Format(1)=%.3f,Format(2)=%.3f',
     :                     STATUS )

*  Save the original current Frame index.
         ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )

*  Add the CURPIC Frame into the Plot.
         CALL AST_ADDFRAME( IPLOT, AST__BASE, WMAP, CPIC, STATUS )

*  Re-instate the original current Frame index.
         CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

      END IF

*  Add a Frame representing another normalised co-ordinate system in the
*  current picture. This Frame has (0,0) at the bottom-left corner of the
*  current picture, and (1,1) at the top-right corner. This Frame is given
*  the Domain CURNDC.
*  ====================================================================

*  See if the Plot already contains a CURNDC Frame.
      CALL KPG1_ASFFR( IPLOT, 'CURNDC', ICNDC, STATUS )

*  If not, add one into the Plot now.
      IF( ICNDC .EQ. AST__NOFRAME ) THEN

*  Get the bounds of the current window (in millimetres).
         CALL PGQWIN( BX( 1 ), BX( 3 ), BX( 2 ), BX( 4 ) )

*  Store these as the GRAPHICS co-ordinates.
         INA( 1 ) = DBLE( BX( 1 ) )
         INA( 2 ) = DBLE( BX( 2 ) )
         INB( 1 ) = DBLE( BX( 3 ) )
         INB( 2 ) = DBLE( BX( 4 ) )

*  We store the bounds of the current window in CURNDC co-ordinates.
         OUTA( 1 ) = 0.0D0
         OUTA( 2 ) = 0.0D0
         OUTA( 1 ) = 0.0D0
         OUTA( 2 ) = 0.0D0
         OUTB( 1 ) = 1.0D0
         OUTB( 2 ) = 1.0D0

*  Create a WinMap which scales millimetres into CURNDC co-ordinates.
         WMAP = AST_WINMAP( 2, INA, INB, OUTA, OUTB, ' ', STATUS )

*  Create the CURNDC Frame.
         CNDC = AST_FRAME( 2, 'DOMAIN=CURNDC,TITLE=Normalised world '//
     :                     'co-ordinates in the current AGI picture.,'//
     :                     'Symbol(1)=X,Symbol(2)=Y,'//
     :                     'Label(1)=Horizontal offset,'//
     :                     'Label(2)=Vertical offset,'//
     :                     'Format(1)=%.3f,Format(2)=%.3f',
     :                     STATUS )

*  Save the original current Frame index.
         ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )

*  Add the CURPIC Frame into the Plot.
         CALL AST_ADDFRAME( IPLOT, AST__BASE, WMAP, CNDC, STATUS )

*  Re-instate the original current Frame index.
         CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

      END IF

*  Export the Plot pointer from the current AST context. This will
*  prevent it being annulled by the following call to AST_END. If
*  an error has occurred, then the Plot will not be exported by this
*  call and so will be annulled by AST_END.
      CALL AST_EXPORT( IPLOT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
