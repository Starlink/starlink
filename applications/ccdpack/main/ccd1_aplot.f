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
*     optionally aligning it with the existing current picture in the
*     AGI database.  This will only work if a WCS component has been
*     stored in the MORE component of the AGI picture (i.e. TRANSFORM
*     components are not handled).
*
*     The Base (graphics) frame in the returned plot
*     corresponds to millimetres from the bottom left corner of the
*     plotting surface.  The Current frame is inherited from the
*     supplied frameset.

*  Arguments:
*     FSET = INTEGER (Given)
*        An AST frameset on which to base the returned Plot.  It will
*        be returned in the same state in which it was received.
*     PICID = INTEGER (Given)
*        The AGI identifier of the picture to use.  Ignored if USEPIC
*        is false.
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
*        with that frameset.  If USEPIC is true and the attempt to 
*        retrieve AST information from AGI fails, a warning will be
*        emitted and PLOT will be returned as per USEPIC = false.
*        In any case, the Current frame of PLOT will be the same
*        as the Current frame of FSET, and frames in PLOT will be in
*        the same order as they were in FSET, though the frame indices
*        may not be the same.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-JAN-2001 (MBT):
*        Original version.
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
      REAL GBOX( 4 )             ! Plotting window bounding box
      REAL RBBOX( 4 )            ! Base frame bounding box (real)
      INTEGER DIM( DAT__MXDIM )  ! Shape of array
      INTEGER CHAN               ! AST identifier for Channel
      INTEGER CNV                ! AST identifier for alignment frameset
      INTEGER IAT                ! Position in string
      INTEGER JBASF              ! Base frame index in FSET
      INTEGER JBASP              ! Base frame index in PLOT
      INTEGER JCURF              ! Current frame index in FSET
      INTEGER MAP                ! AST identifier for alignment mapping
      INTEGER NDIM               ! Number of dimensions
      LOGICAL THERE              ! Is HDS component present?
      CHARACTER * ( DAT__SZTYP ) TYPE ! HDS Data type
      CHARACTER * ( DAT__SZLOC ) APLOC ! .MORE.AST_PLOT component locator
      CHARACTER * ( DAT__SZLOC ) MORLOC ! .MORE component locator
      CHARACTER * ( DAT__SZLOC ) WCSLOC ! .MORE.AST_PLOT.DATA component locator
      CHARACTER * ( AST__SZCHR ) DMN ! Frame domain
      CHARACTER * ( AST__SZCHR ) DMNLST ! List of alignment domains
      CHARACTER * ( AST__SZCHR ) MATDMN ! Domain in which alignment occurred
      DOUBLE PRECISION BBOX( 4 ) ! Base frame bounding box (double precision)
      
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

*  Initialise HDS locators.
      MORLOC = DAT__NOLOC
      APLOC = DAT__NOLOC
      WCSLOC = DAT__NOLOC

*  Attempt to get a locator to the MORE component of the AGI picture.
      CALL AGI_IMORE( PICID, THERE, STATUS )
      IF ( THERE .AND. STATUS .EQ. SAI__OK ) THEN
         CALL AGI_MORE( PICID, 'READ', MORLOC, STATUS )

*  Attempt to get a locator to the MORE.AST_PLOT component.
         CALL DAT_THERE( MORLOC, 'AST_PLOT', THERE, STATUS )
         IF ( THERE .AND. STATUS .EQ. SAI__OK ) THEN
            CALL DAT_FIND( MORLOC, 'AST_PLOT', APLOC, STATUS )
         END IF
      END IF

*  If requested, try to get a Plot object from the current picture of
*  the AGI database.
      IF ( USEPIC ) THEN

*  Attempt to get a locator to the MORE.AST_PLOT.DATA component.
         IF ( APLOC .NE. DAT__NOLOC ) THEN
            CALL DAT_THERE( APLOC, 'DATA', THERE, STATUS )
            IF ( THERE .AND. STATUS .EQ. SAI__OK ) THEN
               CALL DAT_FIND( APLOC, 'DATA', WCSLOC, STATUS )

*  Check that the MORE.AST_PLOT.DATA component is a one-dimensional
*  _CHAR array, and if so map it as such for READ access.
               CALL DAT_TYPE( WCSLOC, TYPE, STATUS )
               CALL DAT_SHAPE( WCSLOC, DAT__MXDIM, DIM, NDIM, STATUS )
               IF ( TYPE( 1: 6 ) .EQ. '_CHAR*' .AND. NDIM .EQ. 1 
     :              .AND. STATUS .EQ. SAI__OK ) THEN

*  Create an AST Channel to read from the DATA component.
                  CALL CCD1_HCHAN( WCSLOC, ' ', CHAN, STATUS )

*  Read an object from the Channel, thus transferring the data.
                  PLOT = AST_READ( CHAN, STATUS )
               END IF

*  Annul the locator for the AST_PLOT.DATA component, which also unmaps
*  the character data.
               CALL DAT_ANNUL( WCSLOC, STATUS )
            END IF
         END IF

*  If we have successfully retrieved a Plot object, we can now attempt 
*  to attach the supplied Frameset to it.
         IF ( AST_ISAPLOT( PLOT, STATUS ) ) THEN

*  Get the Domain of the Current frame of the supplied frameset.
            DMN = AST_GETC( FSET, 'Domain', STATUS )

*  Assemble a list of domains in which to seek alignment.
            IAT = 0 
            IF ( DMN .NE. ' ' ) THEN
               CALL CHR_APPND( DMN, DMNLST, IAT )
               CALL CHR_APPND( ',', DMNLST, IAT )
            END IF
            CALL CHR_APPND( 'SKY,PIXEL,GRID,AGI_WORLD,', DMNLST, IAT )

*  Store information about the framesets which will be messed up by 
*  subsequent calls.
            JBASF = AST_GETI( FSET, 'Base', STATUS )
            JCURF = AST_GETI( FSET, 'Current', STATUS )
            JBASP = AST_GETI( PLOT, 'Base', STATUS )

*  Attempt to align the framesets.
            CNV = AST_CONVERT( FSET, PLOT, DMNLST( : IAT ), STATUS )
            MAP = AST_SIMPLIFY( AST_GETMAPPING( CNV, AST__CURRENT, 
     :                                          AST__BASE, STATUS ),
     :                          STATUS )

*  Get the name of the domain in which alignment occurred.
            MATDMN = AST_GETC( AST_GETFRAME( FSET, AST__BASE, STATUS ),
     :                         'Domain', STATUS )

*  Reset the Base and Current frames of the framesets modified by 
*  AST_CONVERT.
            CALL AST_SETI( PLOT, 'Base', JBASP, STATUS )
            CALL AST_SETI( FSET, 'Base', JBASF, STATUS )
            CALL AST_SETI( FSET, 'Current', JCURF, STATUS )

*  Add the supplied frameset into the Plot.
            CALL AST_ADDFRAME( PLOT, AST__CURRENT, MAP, FSET, STATUS )
         END IF
            
*  If we do not now have a valid Plot object, ensure that the error 
*  status is set and return.
         IF ( STATUS .EQ. SAI__ERROR .OR. PLOT .EQ. AST__NULL ) THEN
            IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
            GO TO 99
         END IF

*  Inform the user which domain the alignment was achieved in.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL MSG_SETC( 'DMN', MATDMN )
         CALL CCD1_MSG( ' ', '   Alignment with picture occurred in '
     :                  // 'domain ^DMN.', STATUS )


*  If no interaction with AGI was requested, construct a default Plot 
*  instead using the supplied frameset as a basis.
      ELSE
         PLOT = AST_PLOT( FSET, GBOX, BBOX, ' ', STATUS )
      END IF

*  Export the Plot from the current context so that it is not annulled
*  by the looming AST_END call.
      CALL AST_EXPORT( PLOT, STATUS )

c*  Save the Plot in the AGI database.
cC  Not complete - see kpg1_wrast for hints.
c
c*  If there is already a .MORE.AST_PLOT component, delete it.
c      IF ( APLOC .NE. DAT__NOLOC ) THEN
c         CALL DAT_ANNUL( APLOC )
c         CALL DAT_ERASE( MORLOC, 'AST_PLOT', STATUS )
c      END IF
c
c*  Create a new .MORE.AST_PLOT component, a scalar of type WCS.
c      DIM( 1 ) = 0
c      CALL DAT_NEW( MORLOC, 'AST_PLOT', 'WCS', 0, DIM, STATUS )
c
c*  Obtain a locator to this structure and create within it a DATA 
c*  component, a 1-dimensional scalar array.
c      CALL DAT_FIND( MORLOC, 'AST_PLOT', APLOC, STATUS )
c      CALL DAT_NEW1C( APLOC, 'DATA', '_CHAR',



*  Error exit label.
 99   CONTINUE

*  Exit AST context.
      CALL AST_END( STATUS )

*  Write a context-sensitive error message if required.
      IF ( STATUS .NE. SAI__OK ) THEN
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL CCD1_MSG( ' ', '  Failed to register plot with '
     :                  // 'existing picture.', STATUS )
      END IF

      END
* $Id$
