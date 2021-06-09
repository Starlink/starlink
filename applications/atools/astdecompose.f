      SUBROUTINE ASTDECOMPOSE( STATUS )
*+
*  Name:
*     ASTDECOMPOSE

*  Purpose:
*     Decompose a Mapping into two component Mappings.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTDECOMPOSE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application returns pointers to two Mappings which, when applied
*     either in series or parallel, are equivalent to the supplied Mapping.
*
*     Since Frame and Region classes inherits from the Mapping class, Frames
*     and Regions can be considered as special types of Mappings and so this
*     method can be used to decompose either CmpMaps, CmpFrames, CmpRegions
*     or Prisms.

*  Usage:
*     astdecompose this map1 map2

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     INVERT1 = _LOGICAL (Write)
*        An output parameter to which is written the value of the Invert
*        attribute to be used with MAP1.
*     INVERT2 = _LOGICAL (Write)
*        An output parameter to which is written the value of the Invert
*        attribute to be used with MAP2.
*     MAP1 = LITERAL (Read)
*        A text file to receive the first component Mapping.
*     MAP2 = LITERAL (Read)
*        A text file to receive the second component Mapping.
*     SERIES = _LOGICAL (Write)
*        An output parameter to which is written a flag indicating if the
*        component Mappings are applied in series or parallel. A TRUE value
*        means that the supplied Mapping is equivalent to applying MAP1
*        followed by MAP2 in series. A FALSE value means that the supplied
*        Mapping is equivalent to applying MAP1 to the lower numbered axes
*        and MAP2 to the higher numbered axes, in parallel.
*     THIS = LITERAL (Read)
*        An NDF or text file holding the Mapping. If an NDF is supplied,
*        the Mapping from the base Frame (always GRID coordinates) to
*        the current Frame will be used.

*  Copyright:
*     Copyright (C) 2021 East Asian Observatory
*     All Rights Reserved.

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
*     {enter_new_authors_here}

*  History:
*     8-JUN-2021 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  External References:
      EXTERNAL AST_ISAMAPPING

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER MAP1
      INTEGER MAP2
      INTEGER THIS
      LOGICAL INVERT2
      LOGICAL INVERT1
      LOGICAL SERIES
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get an AST Mapping.
      CALL KPG1_GTOBJ( 'THIS', 'Mapping', AST_ISAMAPPING, THIS,
     :                 STATUS )

*  Decompose the Mapping.
      CALL AST_DECOMPOSE( THIS, MAP1, MAP2, SERIES, INVERT1, INVERT2,
     :                    STATUS )

*  Report scalar results.
      IF( MAP2 .NE. AST__NULL ) then

         IF( AST_ISAREGION( THIS ) ) THEN
            CALL MSG_SETC( 'C', AST_GETC( MAP1, 'Class', STATUS ) )
            CALL MSG_OUT( ' ', 'First component Region is a ^C',
     :                    STATUS )
            CALL MSG_SETC( 'C', AST_GETC( MAP2, 'Class', STATUS ) )
            CALL MSG_OUT( ' ', 'Second component Region is a ^C',
     :                    STATUS )

*  This is needed to get the FrameSets in each component to be included
*  in the dump. See the protected RegionFS attribute in the Region class.
            MAP1 = AST_COPY( MAP1, STATUS )
            MAP2 = AST_COPY( MAP2, STATUS )

         ELSE IF( AST_ISAFRAME( THIS ) ) THEN
            CALL MSG_SETC( 'C', AST_GETC( MAP1, 'Class', STATUS ) )
            CALL MSG_OUT( ' ', 'First component Frame is a ^C',
     :                    STATUS )
            CALL MSG_SETC( 'C', AST_GETC( MAP2, 'Class', STATUS ) )
            CALL MSG_OUT( ' ', 'Second component Frame is a ^C',
     :                    STATUS )

         ELSE
            IF( SERIES ) THEN
               CALL MSG_OUT( ' ', 'Mappings are used in series',
     :                       STATUS )
            ELSE
               CALL MSG_OUT( ' ', 'Mappings are used in parallel',
     :                       STATUS )
            END IF

            CALL MSG_SETL( 'I', INVERT1 )
            CALL MSG_SETC( 'C', AST_GETC( MAP1, 'Class', STATUS ) )
            CALL MSG_OUT( ' ', 'First Mapping (a ^C) has INVERT=^I',
     :                    STATUS )
            CALL MSG_SETL( 'I', INVERT2 )
            CALL MSG_SETC( 'C', AST_GETC( MAP2, 'Class', STATUS ) )
            CALL MSG_OUT( ' ', 'Second Mapping (a ^C) has INVERT=^I',
     :                    STATUS )
         END IF

*  Write the Mappings out to text files.
         CALL ATL1_PTOBJ( 'MAP1', ' ', MAP1, STATUS )
         CALL ATL1_PTOBJ( 'MAP2', ' ', MAP2, STATUS )

*  Store the output flags.
         CALL PAR_PUT0L( 'INVERT1', INVERT1, STATUS )
         CALL PAR_PUT0L( 'INVERT2', INVERT2, STATUS )
         CALL PAR_PUT0L( 'SERIES', SERIES, STATUS )

*  Report an error if the supplied mapping is atomic and so cannot be
*  decomposed.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'C', AST_GETC( THIS, 'Class', STATUS ) )
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'The supplied ^C cannot be decomposed',
     :                 STATUS )
      END IF

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTDECOMPOSE_ERR', 'Error decomposing a '//
     :                 'Mapping into two component Mappings.', STATUS )
      END IF

      END
