      SUBROUTINE KPS1_MLPFS( LUTMAP, INDF, CFRM, IAXIS, YLOG, MCOMP,
     :                       DUNIT, FSET, STATUS )
*+
*  Name:
*     KPS1_MLPFS

*  Purpose:
*     Create a FrameSet describing Frames required by MLINPLOT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MLPFS( INDF, IWCS, DIST, IAXIS, DIM, YLOG, MCOMP, DUNIT, 
*                      FSET, STATUS )

*  Description:
*     This routine returns a FrameSet containing 2 Frames. Each Frame has 
*     2 axes:
*
*     Frame 1: Corresponds to "what we've got"
*        Axis 1 - nominal GRID value (see KPS1_MLPNG).
*        Axis 2 - nominal data value (see KPS1_MLPND).
*
*     Frame 2: Corresponds to "what we want to see" (i.e. the Frame describing 
*              the quantities which are to be annotated on the displayed axes).
*              It is given Domain DATAPLOT.
*        Axis 1 - The quantity to be plotted on the horizontal axis of the
*                 graph (i.e. the specified Current Frame axis).
*        Axis 2 - nominal data value (see KPS1_MLPND).
*
*     Frame 1 is the Base Frame on exit, and Frame 2 is the Current Frame.

*  Arguments:
*     LUTMAP = INTEGER (Given)
*        The 1D Mapping from nominal GRID value, to the value used to
*        annotate the horizontal axis. This Mapping will have a defined
*        and usable inverse transformation.
*     INDF = INTEGER (Given)
*        An idenfifier for the NDF being displayed.
*     CFRM = INTEGER (Given)
*        A pointer to the Current Frame in the NDFs WCS FrameSet.
*     IAXIS = INTEGER (Given)
*        The index of the Current Frame axis which is to be used to
*        annotated the horizontal axis.
*     YLOG = LOGICAL (Given)
*        Is the logarithm of the Y axis data being displayed?
*     MCOMP = CHARACTER * ( * ) (Given)
*        NDF component being displayed.
*     DUNIT = CHARACTER * ( * ) (Given)
*        Data units in the array being plotted. Ignored if blank.
*     FSET = INTEGER (Returned)
*        A pointer to the returned FrameSet. Returned equal to AST__NULL
*        if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-AUG-1999 (DSB):
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
      INTEGER LUTMAP
      INTEGER INDF
      INTEGER CFRM
      INTEGER IAXIS
      LOGICAL YLOG
      CHARACTER DUNIT*(*)
      CHARACTER MCOMP*(*)

*  Arguments Returned:
      INTEGER FSET

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER ATTR*20          ! Attribute name
      CHARACTER LAB*80           ! Label text string
      CHARACTER TEXT*30          ! General text string
      INTEGER AXES( 2 )          ! Axes to pick from an existing Frame 
      INTEGER IAT                ! No. of characters in a string
      INTEGER MAP1               ! Map pointer
      INTEGER TMAP               ! Unused Mapping
      INTEGER WWGOT              ! Base Frame in returned FrameSet
      INTEGER WWWANT             ! Current Frame in returned FrameSet

*.

*  Initialise.
      FSET = AST__NULL

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  First create the Mapping from the "what we've got" Frame to the "what
*  we want" Frame. This is a parallel CmpMap. The Mapping for axis 1 is 
*  the supplied LutMap which transforms nominal GRID value into the
*  annotated axis value. The Mapping for axis 2 is a UnitMap.
      MAP1 = AST_CMPMAP( LUTMAP,  AST_UNITMAP( 1, ' ', STATUS ), 
     :                   .FALSE., ' ', STATUS )
     
*  Now create the "What we've got" Frame. It is a simple 2D Frame.
      WWGOT = AST_FRAME( 2, ' ', STATUS )

*  Now create the "What we want" Frame. The first axis is copied from 
*  the specified axis in the Current Frame of the supplied FrameSet. 
*  The second (data) axis is a default Axis.
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

*  Get the Label component from the NDF, use a default equal to 
*  "<MCOMP> value" where MCOMP is the name of the NDF component.
      LAB = MCOMP
      CALL NDF_CGET( INDF, 'LABEL', LAB, STATUS )

*  Set the label, symbol and units for the data axis (axis 2).
      IF( YLOG ) THEN
         TEXT = ' '
         IAT = 0
         CALL CHR_APPND( 'Log\\d10\\u(', TEXT, IAT )
         CALL CHR_APPND( LAB, TEXT, IAT )
         CALL CHR_APPND( ')', TEXT, IAT )
         CALL AST_SETC( WWWANT, 'SYMBOL(2)', TEXT( : IAT ), STATUS )

         IF( LAB .EQ. MCOMP ) THEN
            TEXT( IAT : IAT ) = ' '   
            CALL CHR_APPND( 'value)', TEXT, IAT )
         END IF

         CALL AST_SETC( WWWANT, 'LABEL(2)', TEXT( : IAT ), STATUS )
         IF( DUNIT .NE. ' ' ) CALL AST_SETC( WWWANT, 'UNIT(2)', 
     :                                      DUNIT( : CHR_LEN( DUNIT ) ), 
     :                                      STATUS )

      ELSE
         TEXT = ' '
         IAT = 0
         CALL CHR_APPND( LAB, TEXT, IAT )
         CALL AST_SETC( WWWANT, 'SYMBOL(2)', TEXT( : IAT ), STATUS )
   
         IF( LAB .EQ. MCOMP ) THEN
            IAT = IAT + 1
            CALL CHR_APPND( 'value', TEXT, IAT )
         END IF

         CALL AST_SETC( WWWANT, 'LABEL(2)', TEXT( : IAT ), STATUS )
         IF( DUNIT .NE. ' ' ) CALL AST_SETC( WWWANT, 'UNIT(2)', 
     :                                      DUNIT( : CHR_LEN( DUNIT ) ), 
     :                                      STATUS )

      END IF

*  Set the Domain of the "what we want" Frame to DATAPLOT (a special
*  Domain used to indicate a 2D Frame with one dependant axis and one
*  independant axis).
      CALL AST_SETC( WWWANT, 'DOMAIN', 'DATAPLOT', STATUS )

*  Now create the returned FrameSet, initially holding the "what we've got" 
*  Frame. This is Frame 1 and is initially both the Base and Current Frame.
      FSET = AST_FRAMESET( WWGOT, ' ', STATUS )

*  Add the "what we want" Frame. This is Frame 2, and becomes the new Current
*  Frame.
      CALL AST_ADDFRAME( FSET, AST__BASE, MAP1, WWWANT, STATUS ) 

*  Export the returned FrameSet pointer.
      CALL AST_EXPORT( FSET, STATUS )

*  If an error has occurred, annul the returned FrameSet pointer.
      IF( STATUS .NE. SAI__OK ) CALL AST_ANNUL( FSET, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
