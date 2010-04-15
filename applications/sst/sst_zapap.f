      SUBROUTINE SST_ZAPAP( LINE, STATUS )
*+
*  Name:
*     SST_ZAPAP

*  Purpose:
*     Remove "old style" ADAM placeholders from a prologue line.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_ZAPAP( LINE, STATUS )

*  Description:
*     The routine searches a source code line to identify those "old
*     style" ADAM placeholders which are often left in place within
*     prologues. It replaces them with blanks.

*  Arguments:
*     LINE = CHARACTER * ( * ) (Given and Returned)
*        The source code line to be processed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-FEB-1990 (RFWS):
*        Original, derived from the SST_ZAPPL routine.
*     22-MAY-1990 (RFWS):
*        Increased maximum length of placeholder strings to 63.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given and Returned:
      CHARACTER * ( * ) LINE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXPL               ! Number of recognised placeholders
      PARAMETER ( MXPL = 34 )
      INTEGER SZPL               ! Maximum length of a placeholder
      PARAMETER ( SZPL = 63 )

*  Local Variables:
      CHARACTER * ( SZPL ) PLACE( MXPL ) ! Placeholder strings
      INTEGER I                  ! Loop counter for placeholders
      INTEGER II                 ! Placeholder position in line
      INTEGER LPLACE( MXPL )     ! Length of each placeholder
      LOGICAL FOUND              ! Whether a placeholder was found

*  Local Data:
      DATA ( PLACE( I ), LPLACE( I ), I = 1, 10 )
     :  / '<name of task>', 14,
     :    '<brief title for subroutine>', 28,
     :    '<brief title for function>', 26,
     :    '<description of what the task does>', 35,
     :    '<description of what the subroutine does>', 41,
     :    '<description of what the function does>', 39,
     :    'Invoked by the a-task environment', 33,
     :    'CALL name[(argument_list)]', 26,
     :    'result = name[(argument list)]', 30,
     :    'name=type', 9 /

      DATA ( PLACE( I ), LPLACE( I ), I = 11, 20 )
     :  / '<description of what the function returns>', 42,
     :    'Defined in interface module (when it exists)', 44,
     :    'parameter[(dimensions)]=type(access)', 36,
     :    '<description of parameter>', 26,
     :    '<description of how the task works>', 35,
     :    '<description of how the subroutine works>', 41,
     :    '<description of how the function works>', 39,
     :    '<description of any deficiencies>', 33,
     :    '<description of any "bugs" which have not been fixed>', 53,
     :    'author (institution::username)', 30 /

      DATA ( PLACE( I ), LPLACE( I ), I = 21, 30 )
     :  / 'date:  changes (institution::username)', 38,
     :    '<any INCLUDE files containing global constant definitions>',
     :                                                               58,
     :    '<declarations and descriptions for imported arguments>', 54,
     :'<declarations and descriptions for imported/exported arguments>',
     :                                                               63,
     :    '<declarations and descriptions for exported arguments>', 54,
     :    '<declaration for status argument>', 33,
     :    '<declarations for external function references>', 47,
     :    '<local constants defined by PARAMETER>', 38,
     :    '<declarations for local variables>', 34,
     :  '<any INCLUDE files for global variables held in named COMMON>',
     :                                                             61 /

      DATA ( PLACE( I ), LPLACE( I ), I = 31, MXPL )
     :  / '<declarations for internal functions>', 37,
     :    '<any DATA initialisations for local variables>', 46,
     :    '<subroutine code>', 17,
     :    '<function code>', 15 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop to zap placeholders until no more can be found.
      FOUND = .TRUE.
1     CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( FOUND ) THEN

*  Search the line for each placeholder in turn.
         FOUND = .FALSE.
         DO 2 I = 1, MXPL

*  See if the line contains the placeholder.
            II = INDEX( LINE, PLACE( I )( : LPLACE( I ) ) )

*  If so, then replace it with blanks and start agin.
            IF ( II .NE. 0 ) THEN
               LINE( II : II + LPLACE( I ) - 1 ) = ' '
               FOUND = .TRUE.
               GO TO 1
            END IF
2        CONTINUE

*  Return to search for another placeholder.
         GO TO 1
      END IF

      END
* @(#)sst_zapap.f   1.1   94/12/05 11:31:37   96/07/05 10:27:29
