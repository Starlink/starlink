      SUBROUTINE KPS1_LPLFS( INDF, IWCS, DIST, IAXIS, DIM, YLOG, MCOMP,
     :                       DUNIT, NOINV, FSET, STATUS )
*+
*  Name:
*     KPS1_LPLFS

*  Purpose:
*     Create a FrameSet describing Frames required by LINPLOT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LPLFS( INDF, IWCS, DIST, IAXIS, DIM, YLOG, MCOMP, DUNIT, 
*                      NOINV, FSET, STATUS )

*  Description:
*     This routine is supplied with FrameSet describing a 1-d data array as
*     read from an NDF DATA array for instance. The Base Frame should be
*     1-d GRID co-ordinate within the data array. The Current Frame can
*     have any number of axes. The axis within the Current Frame which is
*     to be displayed on the horizontal axis is supplied in IAXIS. If DIST
*     is TRUE, the horizontal axis is annotated with distance from the profile 
*     starting point (measured along the profile), and the axis given by 
*     IAXIS is used only to determine the format for the distance values.
*
*     A FrameSet is returned containing 3 Frames. Each Frame has 2 axes:
*
*     Frame 1: Corresponds to "what we've got"
*        Axis 1 - GRID co-ordinate in the supplied 1-d data array (i.e.
*                 an array component in the supplied NDF).
*        Axis 2 - Supplied data value.
*
*     Frame 2: This is the Frame which is to be mapped uniformly onto the
*              graphics screen. 
*        Axis 1 - The distance from the first grid element, measured
*                 along the profile.
*        Axis 2 - This will correspond to raw data value if YLOG is
*                 .FALSE., and the logaritthm of the data value otherwise.
*
*     Frame 3: Corresponds to "what we want to see" (i.e. the Frame describing 
*              the quantities which are to be annotated on the displayed axes).
*              It is given Domain DATAPLOT.
*        Axis 1 - The quantity to be plotted on the horizontal axis of the
*                 graph (i.e. distance from the starting point of the profile 
*                 if DIST is .TRUE., or the supplied Current Frame axis 
*                 otherwise).
*        Axis 2 - The raw data value, or logged data value as required.
*
*     Frame 2 is the Base Frame on exit, and Frame 3 is the Current Frame.

*  Arguments:
*     INDF = INTEGER (Given)
*        An idenfifier for the NDF being displayed.
*     IWCS = INTEGER (Given)
*        A pointer to the FrameSet associated with the array being plotted.
*     DIST = LOGICAL (Given)
*        Should the graph be annotated with offset from the starting
*        position along the profile path, instead of axis value?
*     IAXIS = INTEGER (Given)
*        The index of the Current Frame axis which is to be used to
*        annotated the horizontal axis (if DIST is .FALSE.) or to format 
*        distance values (if DIST is .TRUE.).
*     DIM = INTEGER (Given)
*        The number of pixels in the 1-d array being plotted.
*     YLOG = LOGICAL (Given)
*        Should the logarithm of the Y axis data be displayed?
*     MCOMP = CHARACTER * ( * ) (Given)
*        NDF component being displayed.
*     DUNIT = CHARACTER * ( * ) (Given)
*        Data units in the array being plotted. Ignored if blank.
*     NONV = LOGICAL (Returned)
*        Returned .TRUE. if any of the inter-Frame Mappings do not have
*        inverse transformations.
*     FSET = INTEGER (Returned)
*        A pointer to the returned FrameSet. Returned equal to AST__NULL
*        if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1998 (DSB):
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

*  Arguments Given:
      INTEGER INDF
      INTEGER IWCS
      LOGICAL DIST
      INTEGER IAXIS
      INTEGER DIM
      LOGICAL YLOG
      CHARACTER DUNIT*(*)
      CHARACTER MCOMP*(*)

*  Arguments Returned:
      LOGICAL NOINV
      INTEGER FSET

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER ATTR*20          ! Attribute name
      CHARACTER TEXT*30          ! General text string
      DOUBLE PRECISION POS( 2 )  ! Start and end of samples in GRID Frame
      INTEGER AXES( 2 )          ! Axes to pick from an existing Frame 
      INTEGER CFRM               ! Current Frame in supplied FrameSet
      INTEGER IAT                ! No. of characters in a string
      INTEGER IPD                ! Pointer to array of returned axis 1 values
      INTEGER IPG                ! Pointer to array of GRID values
      INTEGER IPW                ! Pointer to array of Current Frame values
      INTEGER MAP1               ! Map pointer
      INTEGER MAP2               ! Map pointer
      INTEGER NAX                ! No. of axes in supplied Current Frame
      INTEGER SMAP               ! Base->Current Mapping in supplied FrameSet
      INTEGER TMAP               ! Unused Mapping
      INTEGER UNIFRM             ! Uniform Frame in returned FrameSet
      INTEGER WWGOT              ! Base Frame in returned FrameSet
      INTEGER WWWANT             ! Current Frame in returned FrameSet
      INTEGER XMAP               ! X axis 1-D Mapping
      INTEGER YMAP               ! Y axis 1-D Mapping
      LOGICAL BAD                ! Any bad values found?
*.

*  Initialise.
      FSET = AST__NULL
      NOINV = .FALSE.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  First create the Mapping from the "what we've got" Frame to the uniform
*  Frame. This is a parallel CmpMap. The Mapping for axis 1 is a LutMap
*  giving distance from the starting point for any GRID position. The
*  Mapping for axis 2 is a UnitMap if raw data value is being displayed, or
*  an IntraMap implementing a LOG10 function if the log of the data value
*  is being displayed. An IntraMap is used because AST as yet has no LOG10
*  Mapping.
*  =======================================================================

*  Get a simplified Mapping from Base to Current Frame in the supplied
*  FrameSet.
      SMAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, AST__BASE, 
     :                                     AST__CURRENT, STATUS ), 
     :                     STATUS )

*  Save a pointer to the Current Frame in the supplied FrameSet.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  See how many axes the Current Frame has.
      NAX = AST_GETI( CFRM, 'NAXES', STATUS )         

*  Allocate memory to hold an array of DIM 1-d GRID values.
      CALL PSX_CALLOC( DIM, '_DOUBLE', IPG, STATUS )

*  Allocate memory to hold the corresponding n-d Current Frame values.
      CALL PSX_CALLOC( DIM*NAX, '_DOUBLE', IPW, STATUS )

*  Allocate memory to hold the corresponding distance values.
      CALL PSX_CALLOC( DIM, '_DOUBLE', IPD, STATUS )

*  Abort if an error occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find DIM positions evenly spaced in the GRID Frame along the profile.
*  The positions are at the centre of each grid cell.
      POS( 1 ) = 1.0D0
      POS( 2 ) = DBLE( DIM )
      CALL KPG1_ASSMP( AST__NULL, 2, 1, 2, POS, .FALSE., DIM, 
     :                 1.0D0, %VAL( IPG ), STATUS )

*  Transform these GRID positions into the Current Frame in the supplied
*  FrameSet.
      CALL AST_TRANN( SMAP, DIM, 1, DIM, %VAL( IPG ), .TRUE., NAX, DIM,
     :                %VAL( IPW ), STATUS ) 

*  Find the distance from the first GRID position to each subsequent GRID
*  position, measured along the profile.
      CALL KPG1_ASDSV( CFRM, DIM, NAX, %VAL( IPW ), %VAL( IPD ), BAD,
     :                 STATUS )

*  Report an error if any bad distance values were found.
      IF( BAD .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( 'KPS1_LPLFS_ERR', 'Some points along the '//
     :                 'profile have undefined positions within '//
     :                 'the current co-ordinate Frame of ''^NDF''.',
     :                 STATUS )
      END IF

*  Create the LutMap for axis 1.
      XMAP = AST_LUTMAP( DIM, %VAL( IPD ), 1.0D0, 1.0D0, ' ', STATUS )

*  Set the NOINV flag if the inverse transformation is not defined.
      NOINV = ( .NOT. AST_GETL( XMAP, 'TranInverse', STATUS ) )

*  Now do axis 2 (the vertical axis). If the Y axis is to display data as 
*  supplied, just use a UnitMap.
      IF( .NOT. YLOG ) THEN
         YMAP = AST_UNITMAP( 1, ' ', STATUS )         

*  If the Y axis is to display log base 10 of the data, create s suitable
*  Mapping.
      ELSE

*  At the moment AST does not have a Log Mapping, so we need to implement
*  a private log mapping by using an IntraMap. Ensure KAPPA intramaps are
*  registered.
         CALL KPG1_ASREG( STATUS )

*  Create a "Log10" Mapping.
         YMAP = AST_INTRAMAP( 'Log10', 1, 1, ' ', STATUS )         

      END IF

*  Combine the X and the Y Mappings in parallel.
      MAP1 = AST_CMPMAP( XMAP, YMAP, .FALSE., ' ', STATUS )

*  Now create the Mapping from the "what we've got" Frame to the "what we
*  want" Frame. This is also a parallel CmpMap. The only difference between
*  this Mapping and the one just created (MAP1) is that if DIST is .FALSE.
*  the axis 1 Mapping maps GRID position onto the selected axis in
*  the current Frame, instead of distance from the starting point.
*  =======================================================================

*  If the horizontal axis is being annotated with distance, then the
*  required Mapping is identical to the "what we've got" -> "uniform"
*  Mapping. So just clone MAP1.
      IF( DIST ) THEN
         MAP2 = AST_CLONE( MAP1, STATUS )

*  Otherwise, create a new Mapping for axis 1.
      ELSE

*  Normalise the axis values and copy the values on the required axis to
*  a new 1-d array. Note if there are any bad values in this array.
         CALL KPS1_LPLNM( CFRM, IAXIS, DIM, NAX, %VAL( IPW ),
     :                    %VAL( IPD ), BAD, STATUS )

*  Report an error if any bad axis values were found.
         IF( BAD .AND. STATUS .EQ. SAI__OK ) THEN
            ATTR = 'LABEL('
            IAT = 7
            CALL CHR_PUTI( IAXIS, ATTR, IAT )
            CALL CHR_APPND( ')', ATTR, IAT )
            TEXT = AST_GETC( CFRM, ATTR( : IAT ), STATUS )
            CALL KPG1_PGESC( TEXT, STATUS )
            CALL MSG_SETC( 'LBL', TEXT )

            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'KPS1_LPLFS_ERR', 'Some points along the '//
     :                    'profile have undefined ^LBL values.', 
     :                    STATUS )
         END IF

*  Now create the LutMap for axis 1.
         XMAP = AST_LUTMAP( DIM, %VAL( IPD ), 1.0D0, 1.0D0, ' ', 
     :                      STATUS )

*  Set the NOINV flag if the inverse transformation is not defined.
         NOINV = ( NOINV .OR. 
     :             .NOT. AST_GETL( XMAP, 'TranInverse', STATUS ) )

*  Create a CmpMap giving the required "what we've got" -> "what we want"
*  Mapping (the axis 2 Mapping is the same as for the "what we've got" ->
*  "uniform" Mapping).
         MAP2 = AST_CMPMAP( XMAP, YMAP, .FALSE., ' ', STATUS )

      END IF

*  Create the required Frames
*  ==========================

*  "What we've got":
*  -----------------
*  Axis 1 is copied from the GRID axis of the Base Frame of the supplied 
*  FrameSet. Axis 2 is a default Axis.
      AXES( 1 ) = 1
      AXES( 2 ) = 0
      WWGOT = AST_PICKAXES( AST_GETFRAME( IWCS, AST__BASE, STATUS ), 
     :                      2, AXES, TMAP, STATUS ) 

*  Set the label, symbol and units for the data axis (axis 2).
      TEXT = ' '
      IAT = 0
      CALL CHR_APPND( MCOMP, TEXT, IAT )
      CALL CHR_UCASE( TEXT )
      CALL AST_SETC( WWGOT, 'SYMBOL(2)', TEXT( : IAT ), STATUS )

      IAT = IAT + 1
      CALL CHR_APPND( 'value', TEXT, IAT )
      CALL AST_SETC( WWGOT, 'LABEL(2)', TEXT( : IAT ), STATUS )
      IF( DUNIT .NE. ' ' ) CALL AST_SETC( WWGOT, 'UNIT(2)', DUNIT,
     :                                    STATUS )

*  Clear the Domain and Title values which will have been inherited from the
*  supplied FrameSet.
      CALL AST_CLEAR( WWGOT, 'DOMAIN', STATUS )
      CALL AST_CLEAR( WWGOT, 'TITLE', STATUS )

*  "What we want":
*  ---------------
*  The first axis is copied from the specified axis in the Current Frame of 
*  the supplied FrameSet. The second (data) axis is a default Axis.
      AXES( 1 ) = IAXIS
      AXES( 2 ) = 0
      WWWANT = AST_PICKAXES( CFRM, 2, AXES, TMAP, STATUS ) 

*  When a SkyAxis is extracted from a SkyFrame, its Format and Digits 
*  attributes are set, even if they were not set in the SkyFrame. This means 
*  that Plot does not remove trailing zeros from the formatted axis values.
*  To avoid this, explicitly clear the Format and Digits attributes for the
*  first axis of the "what we want" Frame, unless values have been set
*  for them in the original Current Frame.
      ATTR = 'FORMAT('
      IAT = 7
      CALL CHR_PUTI( IAXIS, ATTR, IAT )
      CALL CHR_APPND( ')', ATTR, IAT )

      IF( .NOT. AST_TEST( CFRM, ATTR( : IAT ), STATUS ) ) THEN
         CALL AST_CLEAR( WWWANT, 'FORMAT(1)', STATUS )
      END IF

      ATTR( : 6 ) = 'DIGITS'
      IF( .NOT. AST_TEST( CFRM, ATTR( : IAT ), STATUS ) ) THEN
         CALL AST_CLEAR( WWWANT, 'DIGITS(1)', STATUS )
      END IF

*  If distance is being used to annotate the axis, set appropriate
*  attributes for axis 1.
      IF( DIST ) THEN
         CALL AST_SETC( WWWANT, 'LABEL(1)', 'Offset', STATUS )
         CALL AST_SETC( WWWANT, 'SYMBOL(1)', 'OFFSET ', STATUS )
      END IF

*  Set the label, symbol and units for the data axis (axis 2).
      IF( YLOG ) THEN
         TEXT = ' '
         IAT = 0
         CALL CHR_APPND( 'Log\\d10\\u(', TEXT, IAT )
         CALL CHR_APPND( MCOMP, TEXT, IAT )
         CALL CHR_APPND( ')', TEXT, IAT )
         CALL AST_SETC( WWWANT, 'SYMBOL(2)', TEXT( : IAT ), STATUS )

         TEXT( IAT : IAT ) = ' '   
         CALL CHR_APPND( 'value)', TEXT, IAT )
         CALL AST_SETC( WWWANT, 'LABEL(2)', TEXT( : IAT ), STATUS )
         IF( DUNIT .NE. ' ' ) CALL AST_SETC( WWWANT, 'UNIT(2)', DUNIT,
     :                                       STATUS )
      ELSE
         TEXT = ' '
         IAT = 0
         CALL CHR_APPND( MCOMP, TEXT, IAT )
         CALL AST_SETC( WWWANT, 'SYMBOL(2)', TEXT( : IAT ), STATUS )
   
         IAT = IAT + 1
         CALL CHR_APPND( 'value', TEXT, IAT )
         CALL AST_SETC( WWWANT, 'LABEL(2)', TEXT( : IAT ), STATUS )
         IF( DUNIT .NE. ' ' ) CALL AST_SETC( WWWANT, 'UNIT(2)', DUNIT,
     :                                       STATUS )
      END IF

*  Set the Domain of the "what we want" Frame to DATAPLOT (a special
*  Domain used to indicate a 2D Frame with one dependant axis and one
*  independant axis).
      CALL AST_SETC( WWWANT, 'DOMAIN', 'DATAPLOT', STATUS )

*  "Uniform": 
*  ----------
*  The uniform Frame is identical to the "what we want" Frame if the
*  horizontal axis is being annotated with distance.
      UNIFRM = AST_COPY( WWWANT, STATUS )

*  If the "what we want" Frame was not annotated with distance, we need to
*  change the attributes for axis 1.
      IF( DIST ) THEN
         CALL AST_SETC( UNIFRM, 'LABEL(1)', 'Offset', STATUS )
         CALL AST_SETC( UNIFRM, 'SYMBOL(1)', 'OFFSET ', STATUS )
      END IF

*  Clear the Domain (set to DATAPLOT in the "what we want" Frame).
      CALL AST_CLEAR( UNIFRM, 'DOMAIN', STATUS )

*  Now create the returned FrameSet.
*  =================================

*  Create a FrameSet holding the "what we've got" Frame. This is Frame 1
*  and is initially both the Base and Current Frame.
      FSET = AST_FRAMESET( WWGOT, ' ', STATUS )

*  Add the "uniform" Frame. This is Frame 2, and becomes the new Current
*  Frame.
      CALL AST_ADDFRAME( FSET, AST__BASE, MAP1, UNIFRM, STATUS ) 

*  Add the "what we want" Frame. This is Frame 3 and becomes the new 
*  Current Frame.
      CALL AST_ADDFRAME( FSET, AST__BASE, MAP2, WWWANT, STATUS ) 

*  Set the Base Frame to correspond to the "uniform" Frame.
      CALL AST_SETI( FSET, 'BASE', 2, STATUS )

*  Tidy up.
*  ========
 999  CONTINUE

*  Free the workspace.
      CALL PSX_FREE( IPG, STATUS )
      CALL PSX_FREE( IPW, STATUS )
      CALL PSX_FREE( IPD, STATUS )

*  Export the returned FrameSet pointer.
      CALL AST_EXPORT( FSET, STATUS )

*  If an error has occurred, annul the returned FrameSet pointer.
      IF( STATUS .NE. SAI__OK ) CALL AST_ANNUL( FSET, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
