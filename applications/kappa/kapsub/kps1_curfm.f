      SUBROUTINE KPS1_CURFM( FRM, MAP, XC, YC, NAX, EXTRA, NEWPIC, 
     :                       IAT, LINE, ICOL, GOOD, CXY, STATUS )
*+
*  Name:
*     KPS1_CURFM

*  Purpose:
*     Format a position for CURSOR.
*     contoured.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CURFM( FRM, MAP, XC, YC, NAX, EXTRA, NEWPIC, IAT, LINE, 
*                      ICOL, GOOD, CXY, STATUS )

*  Description:
*     This routine formats a position for application CURSOR. The
*     position is supplied in GRAPHICS co-ordinates. It is mapped into the 
*     required Frame before being formatted.

*  Arguments:
*     FRM = INTEGER (Given)
*        A pointer to the Frame to which the position refers.
*     MAP = INTEGER (Given)
*        A pointer to the Mapping from the supplied graphics position 
*        (XC, YC) to the Frame given by FRM.
*     XC = REAL (Given)
*        The X GRAPHICS co-ordinate.
*     YC = REAL (Given)
*        The Y GRAPHICS co-ordinate.
*     NAX = INTEGER (Given)
*        The number of axes in FRM.
*     EXTRA = LOGICAL (Given)
*        If .TRUE., then axis symbols (and units if NEWPIC is .TRUE.) are 
*        included in the formatted text returned in LINE.
*     NEWPIC = LOGICAL (Given)
*        If .TRUE., then axis units are included in the formatted text 
*        returned in LINE (if EXTRA is .TRUE.).
*     IAT = INTEGER (Given and Returned)
*        The number of characters in LINE.
*     LINE = CHARACTER * ( * ) (Given and Returned)
*        The text.
*     ICOL( NAX ) = INTEGER (Given and Returned)
*        The tab positions for each column of axis values. Initialised if
*        NEWPIC is .TRUE. (should be left unchanged between calls).
*     GOOD = LOGICAL (Returned)
*        Were all axis value good in Frame FRM?
*     CXY( NAX ) = DOUBLE PRECISION (Returned)
*        The corresponding co-ordinates in FRM.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-OCT-1998 (DSB):
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
      INTEGER FRM
      INTEGER MAP
      REAL XC
      REAL YC
      INTEGER NAX
      LOGICAL EXTRA
      LOGICAL NEWPIC

*  Arguments Given and Returned:
      INTEGER IAT
      CHARACTER LINE*(*)
      INTEGER ICOL( NAX )

*  Arguments Returned:
      LOGICAL GOOD
      DOUBLE PRECISION CXY( NAX )
 
*  Status:
      INTEGER STATUS             ! Global status
      
*  Local Variables:
      CHARACTER ATTRIB*20        ! AST attribute name
      CHARACTER FMT*30           ! Formatted axis value
      CHARACTER SYM*30           ! Axis symbol
      CHARACTER UNIT*30          ! Axis Units string
      DOUBLE PRECISION GXY( 2 )  ! Graphics position
      INTEGER I                  ! Loop count
      INTEGER INDENT             ! Indentation for each axis value
      INTEGER JAT                ! No. of characters in the string
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Transform the cursor position from GRAPHICS co-ordinates into the
*  required Frame.
      GXY( 1 ) = DBLE( XC )
      GXY( 2 ) = DBLE( YC )
      CALL AST_TRANN( MAP, 1, 2, 1, GXY, .TRUE., NAX, 1, CXY, STATUS )

*  Assume all axis values are good in the required Frame.
      GOOD = .TRUE.

*  Note the indentation, and then reset it.
      INDENT = IAT
      IAT = 0

*  Loop round each axis.
      DO I = 1, NAX

*  Indent each axis value.
         IAT = IAT + INDENT

*  Are all axis values good?
         IF( CXY( I ) .EQ. AST__BAD ) GOOD = .FALSE.

*  Append the axis symbol and an equals sign to thr returned text if
*  required.
         IF( EXTRA ) THEN

*  Form the name of the Symbol attribute for this axis.
            ATTRIB = 'Symbol('
            JAT = 7
            CALL CHR_PUTI( I, ATTRIB, JAT )
            CALL CHR_APPND( ')', ATTRIB, JAT )

*  Get the symbol string.
            SYM = AST_GETC( FRM, ATTRIB( : JAT ), STATUS) 

*  Remove any PGPLOT escape sequences.
            CALL KPG1_PGESC( SYM, STATUS )

*  Create a "symbol=value" string.
            IF( SYM .NE. ' ' ) THEN
               CALL CHR_APPND( SYM, LINE, IAT )
               CALL CHR_APPND( ' =', LINE, IAT )
               IAT = IAT + 1
            END IF

         END IF

*  Format and append the axis value.
         FMT = AST_FORMAT( FRM, I, CXY( I ), STATUS )
         CALL CHR_APPND( FMT, LINE, IAT )

*  Append the axis units to the returned text if required, and if this
*  is the first position in a new picture.
         IF( EXTRA .AND. NEWPIC ) THEN

*  Form the name of the Unit attribute for this axis.
            ATTRIB = 'Unit('
            JAT = 5
            CALL CHR_PUTI( I, ATTRIB, JAT )
            CALL CHR_APPND( ')', ATTRIB, JAT )

*  Get the unit string.
            UNIT = AST_GETC( FRM, ATTRIB( : JAT ), STATUS) 

*  Remove any PGPLOT escape sequences.
            CALL KPG1_PGESC( UNIT, STATUS )

*  Create a "symbol=value" string.
            IF( UNIT .NE. ' ' ) THEN
               IAT = IAT + 1
               CALL CHR_APPND( '(', LINE, IAT )
               CALL CHR_APPND( UNIT, LINE, IAT )
               CALL CHR_APPND( ')', LINE, IAT )
            END IF

         END IF

*  Tab to the start of the next column. Subsequent positions use the 
*  same tabs even though the field widths will be smaller (due to the 
*  abscence of the units strings).
         IF( NEWPIC ) THEN
            IAT = 6*( 1 + IAT/6 )
            ICOL( I ) = IAT
         ELSE
            IAT = MAX( ICOL( I ), 6*( 1 + IAT/6 ) )
         END IF

      END DO

      END
