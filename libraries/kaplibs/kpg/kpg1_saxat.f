      SUBROUTINE KPG1_SAXAT( INDF, COMP, AXIS, LOG, FRAME, STATUS )
*+
*  Name:
*     KPG1_SAXAT

*  Purpose:
*     Sets attributes of a Frame axis to describe an NDF array
*     component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_SAXAT( INDF, COMP, AXIS, LOG, FRAME, STATUS )

*  Description:
*     This routine sets the Symbol, Units and Label attributes of a
*     specified axis in an AST Frame so that they describe the values
*     in a specified array component of an NDF. The Symbol is set to
*     the name of the NDF array component (note, this is short and has
*     no spaces so that it can be used as a column name in a catalogue),
*     the Label is set to the NDF Label component, and Units is set to
*     the NDF Units component.

*  Arguments:
*     INDF = INTEGER (Given)
*        An identifier for the NDF being described.
*     COMP = CHARACTER * ( * ) (Given)
*        The name of the NDF array component being described.
*     AXIS = INTEGER (Given)
*        The index of the Frame axis to be modified.
*     LOG = LOGICAL (Given)
*        If .TRUE., then the attribute values are modified so that they
*        are appropriate for a logarithmic axis. The modified attribute
*        values are of the form "Log10(...)" where "..." is the
*        attribute value used for non-logarithmic axes. PGPLOT escape
*        characters are included in the attribute values to produce a
*        sub-scripted "10".
*     FRAME = INTEGER (Given)
*        The identifier for the AST Frame to be modified.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-OCT-2009 (DSB):
*        Original version, extracted from KPS1_LPLFS.
*     16-OCT-2009 (DSB):
*        Recognise "Error" as a component name.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Local Constants:
      CHARACTER BCKSLH*1         ! A single backslash
*  Some compilers need '\\' to get '\', which isn't a problem as Fortran
*  will truncate the string '\\' to '\' on the occasions when that isn't
*  needed.
      PARAMETER( BCKSLH = '\\' )

*  Arguments Given:
      INTEGER INDF
      CHARACTER COMP*(*)
      INTEGER AXIS
      LOGICAL LOG
      INTEGER FRAME

*  Status:
      INTEGER STATUS              ! Global status

*  External references:
      INTEGER CHR_LEN             ! Used length of a string

*  Local Variables:
      CHARACTER LAB*200           ! NDF Label string
      CHARACTER LATT*9            ! Full name of axis Label attribute
      CHARACTER SATT*10           ! Full name of axis Symbol attribute
      CHARACTER TEXT*300          ! New attribute value.
      CHARACTER UATT*8            ! Full name of axis Unit attribute
      CHARACTER UNITS*50          ! NDF Units string
      INTEGER IAT                 ! No. of characters in string
      INTEGER NCU                 ! No. of characters in UNITS

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct the full axis attribute names.
      LATT = 'Label('
      IAT = 6
      CALL CHR_PUTI( AXIS, LATT, IAT )
      CALL CHR_APPND( ')', LATT, IAT )

      UATT = 'Unit('
      IAT = 5
      CALL CHR_PUTI( AXIS, UATT, IAT )
      CALL CHR_APPND( ')', UATT, IAT )

      SATT = 'Symbol('
      IAT = 7
      CALL CHR_PUTI( AXIS, SATT, IAT )
      CALL CHR_APPND( ')', SATT, IAT )

*  Get the Units component from the NDF (and square them if dealing with
*  variance).
      UNITS = ' '
      CALL KPG1_DAUNI( INDF, COMP, UNITS, NCU, STATUS )

*  Get the Label component from the NDF.
      LAB = ' '
      CALL NDF_CGET( INDF, 'LABEL', LAB, STATUS )

*  If the label is blank, use the name of the NDF array component
*  followed by " value" instead.
      IF( LAB .EQ. ' ' ) THEN
         LAB = COMP
         IAT = CHR_LEN( LAB )
         CALL CHR_APPND( ' value', LAB, IAT )

*  Otherwise, if the array component is Variance or Quality, append the
*  array component name to the end of the label.
      ELSE
         IAT = CHR_LEN( LAB )

         IF( COMP( 1 : 1 ) .EQ. 'v' .OR.
     :       COMP( 1 : 1 ) .EQ. 'V' ) THEN
            CALL CHR_APPND( ' variance', LAB, IAT )

         ELSE IF( COMP( 1 : 1 ) .EQ. 'e' .OR.
     :       COMP( 1 : 1 ) .EQ. 'E' ) THEN
            CALL CHR_APPND( ' error', LAB, IAT )

         ELSE IF( COMP( 1 : 1 ) .EQ. 'q' .OR.
     :            COMP( 1 : 1 ) .EQ. 'Q' ) THEN
            CALL CHR_APPND( ' quality', LAB, IAT )

         END IF
      END IF

*  First handle cases where we are setting attributes for a logarithmic
*  axis.
      IF( LOG ) THEN

*  The Symbol is "Log10(array component name)". Include PGPLOT escape
*  characters to produce a sub-script "10".
         TEXT = ' '
         IAT = 0
         CALL CHR_APPND( 'Log'//BCKSLH//'d10'//BCKSLH//'u(', TEXT, IAT )
         CALL CHR_APPND( COMP, TEXT, IAT )
         CALL CHR_APPND( ')', TEXT, IAT )
         CALL AST_SETC( FRAME, SATT, TEXT( : IAT ), STATUS )

*  The Label is "Log10(label)".
         TEXT = ' '
         IAT = 0
         CALL CHR_APPND( 'Log'//BCKSLH//'d10'//BCKSLH//'u(', TEXT, IAT )
         CALL CHR_APPND( LAB, TEXT, IAT )
         CALL CHR_APPND( ')', TEXT, IAT )
         CALL AST_SETC( FRAME, LATT, TEXT( : IAT ), STATUS )

*  The Units string (if not blank) is "Log10(units)".
         IF( UNITS .NE. ' ' ) THEN
            TEXT = ' '
            IAT = 0
            CALL CHR_APPND( 'Log'//BCKSLH//'d10'//BCKSLH//'u(', TEXT,
     :                      IAT )
            CALL CHR_APPND( UNITS, TEXT, IAT )
            CALL CHR_APPND( ')', TEXT, IAT )
            CALL AST_SETC( FRAME, UATT, TEXT( : IAT ), STATUS )
         END IF

*  Now set attributes for a linear axis.
      ELSE

*  Symbol...
         CALL AST_SETC( FRAME, SATT, COMP, STATUS )

*  Label...
         CALL AST_SETC( FRAME, LATT, LAB, STATUS )

*  Units...
         IF( UNITS .NE. ' ' ) THEN
            CALL AST_SETC( FRAME, UATT, UNITS( : NCU ), STATUS )
         END IF

      END IF

      END
