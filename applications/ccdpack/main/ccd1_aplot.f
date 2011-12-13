      SUBROUTINE CCD1_APLOT( FSET, PICID, USEPIC, PLOT, STATUS )
*+
*  Name:
*     CCD1_APLOT

*  Purpose:
*     Create an AST Plot object.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_APLOT( FSET, PICID, USEPIC, PLOT, STATUS )

*  Description:
*     This routine creates an AST Plot object from an AST FrameSet,
*     optionally (according to the value of the USEPIC argument)
*     aligning it with an existing picture in the AGI database.
*     This will only work if a WCS component has been stored in
*     the MORE.AST_PLOT component of the AGI picture (i.e. TRANSFORM
*     components are not handled).
*
*     The Base (graphics) frame in the returned plot
*     corresponds to millimetres from the bottom left corner of the
*     plotting surface.  A new BASEPIC and CURPIC frame will also
*     be added unless they are already present.  The Current frame
*     of the returned Plot will be inherited from the supplied
*     Frameset.

*  Arguments:
*     FSET = INTEGER (Given)
*        An AST frameset on which to base the returned Plot.  It will
*        be returned in the same state in which it was received.
*     PICID = INTEGER (Given)
*        The AGI identifier of the picture to use.  The .MORE.AST_PLOT
*        component of this picture will be read to achieve alignment if
*        USEPIC is true.
*     USEPIC = LOGICAL (Given)
*        If true an attempt will be made to construct the returned Plot
*        using the AST information associated with the AGI picture
*        identified by PICID.
*     PLOT = INTEGER (Returned)
*        An AST Plot object which can be used to plot on the underlying
*        PGPLOT plotting surface.  If USEPIC is false, it will be a
*        copy of FSET turned into a plot by adding a suitable Base frame.
*        If USEPIC is true and it is possible to retrieve an AST
*        frameset from the .MORE.AST_PLOT, component of the AGI
*        picture identified by PICID it will be a copy of FSET joined
*        with that frameset.  In any case, the Current frame of PLOT
*        will be the same as the Current frame of FSET, and frames in
*        PLOT will be in the same order as they were in FSET, though
*        the frame indices may not be the same.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-JAN-2001 (MBT):
*        Original version.  Written with reference to KAPPA/CONTOUR;
*        see especially KPG1_GDGET.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants
      INCLUDE 'DAT_PAR'          ! Standard HDS constants

*  Arguments Given:
      INTEGER FSET
      INTEGER PICID
      LOGICAL USEPIC

*  Arguments Returned:
      INTEGER PLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL BX( 4 )               ! Bounds of plotting area
      REAL GBOX( 4 )             ! Plotting window bounding box
      REAL RBBOX( 4 )            ! Base frame bounding box (real)
      INTEGER BPIC               ! BASEPIC Frame
      INTEGER CPIC               ! CURPIC Frame
      INTEGER CHAN               ! AST identifier for Channel
      INTEGER CNV                ! AST identifier for alignment frameset
      INTEGER DIM( DAT__MXDIM )  ! Shape of array
      INTEGER IAT                ! Position in string
      INTEGER JBASF              ! Base frame index in FSET
      INTEGER JBASP              ! Base frame index in PLOT
      INTEGER JCURF              ! Current frame index in FSET
      INTEGER JCURP              ! Current frame index in PLOT
      INTEGER JFRM               ! Frame index
      INTEGER MAP                ! AST identifier for alignment mapping
      INTEGER MATFRM             ! AST identifier for alignment frame
      INTEGER NDIM               ! Number of dimensions
      INTEGER WMAP               ! GRAPHICS->BASEPIC/CURPIC WinMap
      LOGICAL THERE              ! Is HDS component present?
      CHARACTER * ( DAT__SZTYP ) TYPE ! HDS Data type
      CHARACTER * ( DAT__SZLOC ) APLOC ! .MORE.AST_PLOT component locator
      CHARACTER * ( DAT__SZLOC ) MORLOC ! .MORE component locator
      CHARACTER * ( DAT__SZLOC ) WCSLOC ! .MORE.AST_PLOT.DATA component locator
      CHARACTER * ( AST__SZCHR ) DMN ! Frame domain
      CHARACTER * ( AST__SZCHR ) DMNLST ! List of alignment domains
      CHARACTER * ( AST__SZCHR ) MATDMN ! Domain in which alignment occurred
      DOUBLE PRECISION BBOX( 4 ) ! Base frame bounding box (double precision)
      DOUBLE PRECISION INA( 2 )  ! Corner A in GRAPHICS co-ords
      DOUBLE PRECISION INB( 2 )  ! Corner B in GRAPHICS co-ords
      DOUBLE PRECISION OUTA( 2 ) ! Corner A in BASEPIC or CURPIC co-ords
      DOUBLE PRECISION OUTB( 2 ) ! Corner B in BASEPIC or CURPIC co-ords

*.

*  Initialise returned value.
      PLOT = AST__NULL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin a new AST context.
      CALL AST_BEGIN( STATUS )

*  Get the PGPLOT viewport in millimeters and ensure that the current
*  plotting window corresponds to this.  This is the coordinate system
*  which is used by the Base (GRAPHICS) frame in the plot.
      CALL PGQWIN( RBBOX( 1 ), RBBOX( 3 ), RBBOX( 2 ), RBBOX( 4 ) )
      BBOX( 1 ) = DBLE( RBBOX( 1 ) )
      BBOX( 2 ) = DBLE( RBBOX( 2 ) )
      BBOX( 3 ) = DBLE( RBBOX( 3 ) )
      BBOX( 4 ) = DBLE( RBBOX( 4 ) )
      CALL PGQVP( 2, GBOX( 1 ), GBOX( 3 ), GBOX( 2 ), GBOX( 4 ) )
      CALL PGSWIN( GBOX( 1 ), GBOX( 3 ), GBOX( 2 ), GBOX( 4 ) )

*  If requested, try to get a Plot object from the AGI database.
      IF ( USEPIC ) THEN

*  Attempt to get a locator to the MORE component of the AGI picture.
         CALL AGI_IMORE( PICID, THERE, STATUS )
         IF ( THERE .AND. STATUS .EQ. SAI__OK ) THEN
            CALL AGI_MORE( PICID, 'UPDATE', MORLOC, STATUS )

*  Attempt to get a locator to the MORE.AST_PLOT component.
            CALL DAT_THERE( MORLOC, 'AST_PLOT', THERE, STATUS )
            IF ( THERE .AND. STATUS .EQ. SAI__OK ) THEN
               CALL DAT_FIND( MORLOC, 'AST_PLOT', APLOC, STATUS )

*  Attempt to get a locator to the MORE.AST_PLOT.DATA component.
               CALL DAT_THERE( APLOC, 'DATA', THERE, STATUS )
               IF ( THERE .AND. STATUS .EQ. SAI__OK ) THEN
                  CALL DAT_FIND( APLOC, 'DATA', WCSLOC, STATUS )

*  Check that the MORE.AST_PLOT.DATA component is a one-dimensional
*  _CHAR array, and if so map it as such for READ access.
                  CALL DAT_TYPE( WCSLOC, TYPE, STATUS )
                  CALL DAT_SHAPE( WCSLOC, DAT__MXDIM, DIM, NDIM,
     :                            STATUS )
                  IF ( TYPE( 1: 6 ) .EQ. '_CHAR*' .AND. NDIM .EQ. 1
     :                 .AND. STATUS .EQ. SAI__OK ) THEN

*  Create an AST Channel to read from the DATA component.
                     CALL CCD1_HCHAN( WCSLOC, 'READ', CHAN, STATUS )

*  Read an object from the Channel, thus transferring the data.
                     PLOT = AST_READ( CHAN, STATUS )

*  If we have successfully retrieved a Plot object, we can now attempt
*  to attach the supplied Frameset to it.
                     IF ( AST_ISAPLOT( PLOT, STATUS )
     :                    .AND. STATUS .EQ. SAI__OK ) THEN

*  Get the Domain of the Current frame of the supplied frameset.
                        DMN = AST_GETC( FSET, 'Domain', STATUS )

*  Assemble a list of domains in which to seek alignment.
                        IAT = 0
                        IF ( DMN .NE. ' ' ) THEN
                           CALL CHR_APPND( DMN, DMNLST, IAT )
                           CALL CHR_APPND( ',', DMNLST, IAT )
                        END IF
                        CALL CHR_APPND( 'SKY,PIXEL,GRID,AGI_WORLD,',
     :                                  DMNLST, IAT )

*  Store information about the framesets which will be messed up by
*  subsequent calls.
                        JBASF = AST_GETI( FSET, 'Base', STATUS )
                        JCURF = AST_GETI( FSET, 'Current', STATUS )
                        JBASP = AST_GETI( PLOT, 'Base', STATUS )

*  Attempt to align the framesets.
                        CNV = AST_CONVERT( FSET, PLOT, DMNLST( : IAT ),
     :                                     STATUS )
                        MAP = AST_GETMAPPING( CNV, AST__CURRENT,
     :                                        AST__BASE, STATUS )
                        MAP = AST_SIMPLIFY( MAP, STATUS )

*  Get the name of the domain in which alignment occurred.
                        MATFRM = AST_GETFRAME( FSET, AST__BASE, STATUS )
                        MATDMN = AST_GETC( MATFRM, 'Domain', STATUS )

*  Reset the Base and Current frames of the framesets modified by
*  AST_CONVERT.
                        CALL AST_SETI( PLOT, 'Base', JBASP, STATUS )
                        CALL AST_SETI( FSET, 'Base', JBASF, STATUS )
                        CALL AST_SETI( FSET, 'Current', JCURF, STATUS )

*  Add the supplied frameset into the Plot.
                        CALL AST_ADDFRAME( PLOT, AST__CURRENT, MAP,
     :                                     FSET, STATUS )
                     END IF
                  END IF

*  Release the .MORE.AST_PLOT.DATA locator.
                  CALL DAT_ANNUL( WCSLOC, STATUS )
               END IF

*  Release the .MORE.AST_PLOT locator.
               CALL DAT_ANNUL( APLOC, STATUS )
            END IF

*  Release the .MORE locator.
            CALL DAT_ANNUL( MORLOC, STATUS )
         END IF

*  If all is well, inform the user in which domain the alignment was
*  achieved.
         IF ( STATUS .EQ. SAI__OK .AND. PLOT .NE. AST__NULL ) THEN
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL MSG_SETC( 'DMN', MATDMN )
            CALL CCD1_MSG( ' ', '   Alignment with picture occurred'
     :                     // ' in domain ^DMN.', STATUS )

*  Otherwise, ensure that the error status is set and write a context-
*  sensitive error message.
         ELSE
            IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_APLOT_NOALGN', 'CCD1_APLOT: ' //
     :         'Failed to register plot with existing picture.',
     :         STATUS )
         END IF

*  If no interaction with AGI was requested, construct a default Plot
*  instead using the supplied frameset as a basis.
      ELSE
         PLOT = AST_PLOT( FSET, GBOX, BBOX, ' ', STATUS )
      END IF

*  Store the Current frame since it may be disturbed by the subsequent
*  calls.
      JCURP = AST_GETI( PLOT, 'Current', STATUS )

*  Add a Frame representing AGI world co-ordinates in the BASE picture.
*  This picture has equal scales on both axes, and the shorter axis
*  has a length of 1.0. This Frame is given the Domain BASEPIC.
*  ====================================================================

*  See if the Plot already contains a BASEPIC Frame.
      CALL CCD1_FRDM( PLOT, 'BASEPIC', JFRM, STATUS )

*  If not, add one to the plot now.
      IF ( JFRM .EQ. AST__NOFRAME ) THEN

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

*  Add the BASEPIC Frame into the Plot.
         CALL AST_ADDFRAME( PLOT, AST__BASE, WMAP, BPIC, STATUS )
      END IF

*  Add a Frame representing a normalised co-ordinate system in the current
*  picture. This Frame has equals scales on both axes, and the shorter axis
*  has a length of 1.0. This Frame is given the Domain CURPIC.
*  ====================================================================

*  See if the Plot already contains a CURPIC Frame.
      CALL CCD1_FRDM( PLOT, 'CURPIC', JFRM, STATUS )

*  If not, add one into the Plot now.
      IF ( JFRM .EQ. AST__NOFRAME ) THEN

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

*  Add the CURPIC Frame into the Plot.
         CALL AST_ADDFRAME( PLOT, AST__BASE, WMAP, CPIC, STATUS )
      END IF

*  Restore the correct Current frame of the Plot.
      CALL AST_SETI( PLOT, 'Current', JCURP, STATUS )

*  Export the Plot so that it is not annulled by the looming AST_END
*  call.
      CALL AST_EXPORT( PLOT, STATUS )

*  Exit AST context.
      CALL AST_END( STATUS )

      END
* $Id$
